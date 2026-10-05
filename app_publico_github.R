# =========================================================
# PLATAFORMA BARBEARIA V1 - VERSÃO PÚBLICA / PORTFÓLIO
# =========================================================
# Preparada para publicação em GitHub e apresentação no LinkedIn.
#
# SEGURANÇA:
# - não grave senhas, tokens ou e-mails pessoais neste arquivo;
# - configure banco, SMTP e URL pública por variáveis de ambiente;
# - mantenha o arquivo .Renviron real fora do Git;
# - não publique backups .bak com dados reais.
# =========================================================

# =========================================================
# BARBEARIA - PLATAFORMA MULTIUSUARIO
# =========================================================

pacotes <- c("shiny","shinydashboard","shinyWidgets","DBI","odbc","DT","sodium","blastula")
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly=TRUE)]
if(length(faltantes)>0) install.packages(faltantes, repos="https://cloud.r-project.org")
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DBI)
library(odbc)
library(DT)
library(sodium)
library(blastula)

try(Sys.setlocale("LC_TIME","Portuguese_Brazil.1252"), silent=TRUE)

# ---------------------------------------------------------
# CONFIGURAÇÃO DE E-MAIL
# ---------------------------------------------------------
# As credenciais podem ser informadas em um arquivo ".Renviron" na mesma
# pasta deste app.R, uma por linha. A senha NÃO fica gravada neste arquivo.
# Exemplo:
#   SMTP_FROM=conta@gmail.com
#   SMTP_USER=conta@gmail.com
#   SMTP_PASSWORD=senhadeapp16caracteres
#   SMTP_PROVIDER=gmail
if(file.exists(".Renviron")) try(readRenviron(".Renviron"),silent=TRUE)

# URL pública oficial do ambiente de testes.
# O endereço abaixo corresponde ao Dev Tunnel persistente criado para a V1.
# Se futuramente APP_URL for definida no .Renviron, ela terá prioridade,
# permitindo trocar o domínio sem alterar novamente o código.
# URL pública definida fora do código.
# Ex.: APP_URL=https://seu-dominio.example
APP_URL <- trimws(Sys.getenv("APP_URL",""))
APP_URL <- sub("/+$", "", APP_URL)

# Porta fixa para que o Dev Tunnel continue apontando sempre para o mesmo
# endereço local, inclusive após reiniciar o RStudio/aplicativo.
APP_PORT <- suppressWarnings(as.integer(Sys.getenv("APP_PORT","3728")))
if(is.na(APP_PORT) || APP_PORT < 1L || APP_PORT > 65535L) APP_PORT <- 3728L
options(shiny.port=APP_PORT)

SMTP_KEY_ID <- trimws(Sys.getenv("SMTP_KEY_ID","barbearia_email"))
SMTP_FROM <- trimws(Sys.getenv("SMTP_FROM",""))
SMTP_USER <- trimws(Sys.getenv("SMTP_USER",""))

# Senha de app do Gmail: o Google pode exibi-la em grupos de 4 caracteres.
# Removemos espaços e aspas acidentais somente na memória do aplicativo.
SMTP_PASSWORD <- gsub("[[:space:]\"']","",Sys.getenv("SMTP_PASSWORD",""))

SMTP_PROVIDER <- trimws(Sys.getenv("SMTP_PROVIDER","gmail"))
SMTP_HOST <- trimws(Sys.getenv("SMTP_HOST","smtp.gmail.com"))
SMTP_PORT <- trimws(Sys.getenv("SMTP_PORT","587"))
SMTP_USE_SSL <- trimws(Sys.getenv("SMTP_USE_SSL","true"))

# Diagnóstico no console ao iniciar (a senha nunca é exibida).
if(nzchar(SMTP_USER) && nzchar(SMTP_PASSWORD)) {
  message("E-mail configurado: ",SMTP_USER,
          " | provedor: ",if(nzchar(SMTP_PROVIDER)) SMTP_PROVIDER else SMTP_HOST,
          " | tamanho da senha: ",nchar(SMTP_PASSWORD))
  if(identical(tolower(SMTP_PROVIDER),"gmail") && nchar(SMTP_PASSWORD)!=16)
    message("ATENÇÃO: senha de app do Gmail deve ter 16 caracteres, mas esta tem ",
            nchar(SMTP_PASSWORD),". Gere uma nova em myaccount.google.com/apppasswords.")
} else {
  message("E-mail NÃO configurado: o envio do link de senha não vai funcionar. ",
          "Defina SMTP_FROM, SMTP_USER, SMTP_PASSWORD e SMTP_PROVIDER antes do runApp().")
}

# ---------------------------------------------------------
# BANCO SQL SERVER
# ---------------------------------------------------------
# Configuração do SQL Server via variáveis de ambiente.
# Valores locais e credenciais não ficam gravados no repositório.
DB_DRIVER <- trimws(Sys.getenv("BARB_DB_DRIVER","ODBC Driver 18 for SQL Server"))
DB_SERVER <- trimws(Sys.getenv("BARB_DB_SERVER","localhost"))
DB_DATABASE <- trimws(Sys.getenv("BARB_DB_DATABASE","Barbearia"))
DB_UID <- trimws(Sys.getenv("BARB_DB_UID",""))
DB_PASSWORD <- Sys.getenv("BARB_DB_PASSWORD","")
DB_TRUSTED <- tolower(trimws(Sys.getenv("BARB_DB_TRUSTED","yes"))) %in%
  c("1","true","yes","sim")

conectar_banco <- function() tryCatch({
  args <- list(
    driver = DB_DRIVER,
    server = DB_SERVER,
    database = DB_DATABASE,
    TrustServerCertificate = "Yes"
  )
  if(DB_TRUSTED || !nzchar(DB_UID)) {
    args$trusted_connection <- "Yes"
  } else {
    args$uid <- DB_UID
    args$pwd <- DB_PASSWORD
  }
  do.call(DBI::dbConnect, c(list(drv = odbc::odbc()), args))
}, error=function(e) {
  stop(paste0(
    "Não foi possível conectar ao SQL Server. Confira BARB_DB_SERVER, ",
    "BARB_DB_DATABASE, BARB_DB_DRIVER e as variáveis de autenticação. Detalhes: ",
    conditionMessage(e)
  ))
})

con <- conectar_banco()

# Indica se há uma transação aberta (ver dbWithTransaction abaixo).
em_transacao <- FALSE

# Fecha a conexão antiga (se ainda existir) e abre uma nova.
reconectar_banco <- function() {
  try(DBI::dbDisconnect(con),silent=TRUE)
  con <<- conectar_banco()
  invisible(TRUE)
}

# Reconhece erros de conexão perdida (IMC01, 08S01, "vínculo de comunicação"...).
# A mensagem pode vir em Latin-1, por isso é convertida antes de comparar.
conexao_perdida <- function(e) {
  msg <- iconv(conditionMessage(e), from="", to="UTF-8", sub="?")
  grepl(paste0("IMC01|08S01|08001|08003|communication link|v.nculo de comunica|",
               "connection is closed|not connected|invalid connection|",
               "conex.o foi desfeita|TCP Provider|Named Pipes Provider"),
        msg,ignore.case=TRUE)
}

# Executa a operação; se a conexão tiver caído, reconecta e tenta UMA vez de
# novo. Dentro de uma transação só reconecta (sem repetir), pois a transação
# foi perdida junto com a conexão e repetir só parte dela gravaria dados
# incompletos.
executar_com_retry <- function(f,statement,params) {
  tryCatch(f(),error=function(e) {
    if(conexao_perdida(e)) {
      ok <- tryCatch({reconectar_banco();TRUE},error=function(e2) FALSE)
      if(ok && !em_transacao)
        return(tryCatch(f(),error=function(e2) erro_sql(e2,statement,params)))
    }
    erro_sql(e,statement,params)
  })
}

# Versão do dbWithTransaction que marca quando há transação aberta.
dbWithTransaction <- function(conn,code) {
  em_transacao <<- TRUE
  on.exit(em_transacao <<- FALSE)
  DBI::dbWithTransaction(conn,code)
}

# ---------------------------------------------------------
# COMPATIBILIDADE DE CONSULTAS
# ---------------------------------------------------------
# O sistema foi desenvolvido inicialmente com SQLite. Estes helpers
# preservam a lógica do aplicativo e traduzem apenas os poucos comandos
# específicos do SQLite que permanecem no código.
translate_limit1_sqlserver <- function(statement) {
  # Converte cada LIMIT 1 no SELECT correspondente para TOP 1,
  # inclusive quando houver subconsultas.
  chars <- strsplit(statement, "", fixed=TRUE)[[1]]
  n <- length(chars)
  if(n==0) return(statement)

  is_word_char <- function(x) grepl("[A-Za-z0-9_]", x)
  keyword_at <- function(pos,word) {
    k <- pos + nchar(word) - 1L
    if(k > n) return(FALSE)
    tolower(paste0(chars[pos:k],collapse="")) == tolower(word)
  }

  depth <- 0L
  in_string <- FALSE
  selects <- data.frame(pos=integer(),depth=integer(),stringsAsFactors=FALSE)
  limits <- data.frame(pos=integer(),depth=integer(),end=integer(),stringsAsFactors=FALSE)

  i <- 1L
  while(i <= n) {
    ch <- chars[i]

    if(ch == "'") {
      if(in_string && i < n && chars[i+1L] == "'") {
        i <- i + 2L
        next
      }
      in_string <- !in_string
      i <- i + 1L
      next
    }

    if(!in_string) {
      if(ch == "(") {
        depth <- depth + 1L
        i <- i + 1L
        next
      }
      if(ch == ")") {
        depth <- max(0L,depth-1L)
        i <- i + 1L
        next
      }

      if(keyword_at(i,"select")) {
        before_ok <- i==1L || !is_word_char(chars[i-1L])
        after_pos <- i + 6L
        after_ok <- after_pos>n || !is_word_char(chars[after_pos])
        if(before_ok && after_ok)
          selects <- rbind(selects,data.frame(pos=i,depth=depth,stringsAsFactors=FALSE))
        i <- i + 6L
        next
      }

      if(keyword_at(i,"limit")) {
        before_ok <- i==1L || !is_word_char(chars[i-1L])
        after_pos <- i + 5L
        after_ok <- after_pos>n || !is_word_char(chars[after_pos])
        if(before_ok && after_ok) {
          k <- after_pos
          while(k <= n && grepl("[[:space:]]",chars[k])) k <- k + 1L
          if(k <= n && chars[k] == "1") {
            limits <- rbind(limits,data.frame(pos=i,depth=depth,end=k,stringsAsFactors=FALSE))
          }
        }
      }
    }

    i <- i + 1L
  }

  if(nrow(limits)==0 || nrow(selects)==0) return(statement)

  insert_after <- character(n)
  remove_pos <- logical(n)

  for(j in seq_len(nrow(limits))) {
    lp <- limits$pos[j]
    ld <- limits$depth[j]
    cand <- selects[selects$depth==ld & selects$pos < lp,,drop=FALSE]
    if(nrow(cand)==0) next
    sp <- cand$pos[nrow(cand)]

    # Adiciona TOP 1 logo APÓS a palavra SELECT (sp+5 é o último "T").
    insert_after[sp+5L] <- paste0(insert_after[sp+5L]," TOP 1")

    # Remove LIMIT 1, deixando eventual ORDER BY/fechamento de subconsulta intacto.
    remove_pos[lp:limits$end[j]] <- TRUE
  }

  # CORREÇÃO: primeiro escreve o caractere, depois o texto inserido.
  out <- character(0)
  for(i in seq_len(n)) {
    if(!remove_pos[i]) out <- c(out,chars[i])
    if(nzchar(insert_after[i])) out <- c(out,insert_after[i])
  }
  paste0(out,collapse="")
}

translate_sqlserver <- function(statement) {
  statement <- gsub("datetime\\('now','\\+30 minutes'\\)", "DATEADD(MINUTE,30,GETDATE())", statement, fixed=FALSE)
  statement <- gsub("datetime\\('now','\\+24 hours'\\)", "DATEADD(HOUR,24,GETDATE())", statement, fixed=FALSE)
  statement <- gsub("datetime\\('now'\\)", "GETDATE()", statement, fixed=FALSE)
  statement <- gsub("substr\\([[:space:]]*data[[:space:]]*,[[:space:]]*1[[:space:]]*,[[:space:]]*7[[:space:]]*\\)",
                     "LEFT(CONVERT(varchar(10),data,23),7)", statement, ignore.case=TRUE)
  # Literal com acento como Unicode (evita problemas de code page).
  statement <- gsub("'CONCLU\u00cdDO'", "N'CONCLU\u00cdDO'", statement, fixed=TRUE)
  translate_limit1_sqlserver(statement)
}

# Normaliza os parâmetros para tipos que o driver ODBC sempre aceita:
# vazio/NULL -> NA, Date/POSIXct/hms/factor -> texto, sempre 1 valor por parâmetro.
sanitize_params <- function(params) {
  lapply(params, function(p) {
    if(is.null(p) || length(p)==0) return(NA_character_)
    p <- p[1]
    if(inherits(p,"Date")) return(as.character(p))
    if(inherits(p,c("POSIXct","POSIXlt"))) return(format(p,"%Y-%m-%d %H:%M:%S"))
    if(inherits(p,"hms")) return(substr(as.character(p),1,8))
    if(is.factor(p)) return(as.character(p))
    if(is.logical(p) && is.na(p)) return(NA_character_)
    # O driver ODBC só converte texto para TIME no formato HH:MM:SS.
    if(is.character(p) && !is.na(p) && grepl("^[0-9]{1,2}:[0-9]{2}$",p))
      return(paste0(sprintf("%02d",as.integer(sub(":.*","",p))),
                    ":",sub(".*:","",p),":00"))
    p
  })
}

# Em caso de erro, mostra o SQL e os parâmetros (a mensagem original do
# driver pode vir em Latin-1 e quebrar o R, por isso é convertida).
erro_sql <- function(e, statement, params) {
  msg <- iconv(conditionMessage(e), from="", to="UTF-8", sub="?")
  ps <- paste(vapply(params, function(p)
    paste0(class(p)[1], ":", as.character(p)), character(1)), collapse=" | ")
  stop(sprintf("%s\n--- SQL ---\n%s\n--- PARAMETROS ---\n%s", msg, statement, ps),
       call.=FALSE)
}

dbq <- function(statement, params=list()) {
  statement <- translate_sqlserver(statement)
  params <- sanitize_params(params)
  executar_com_retry(function() DBI::dbGetQuery(con,statement,params=params),
                     statement,params)
}

# ---------------------------------------------------------
# CONTROLE DE ALTERAÇÕES PARA SINCRONIZAÇÃO ENTRE SESSÕES
# ---------------------------------------------------------
# A tabela app_sync_version funciona como um "relógio" do banco.
# Ela só muda quando ocorre INSERT/UPDATE/DELETE pela aplicação.
# As telas consultam apenas esse número periodicamente; se ele não
# mudou, nenhuma UI é reconstruída e a página não fica piscando.
DBI::dbExecute(con, "
IF OBJECT_ID(N'dbo.app_sync_version',N'U') IS NULL
BEGIN
  CREATE TABLE dbo.app_sync_version(
    id INT NOT NULL CONSTRAINT PK_app_sync_version PRIMARY KEY,
    versao BIGINT NOT NULL,
    atualizado_em DATETIME2(0) NOT NULL
      CONSTRAINT DF_app_sync_version_atualizado DEFAULT GETDATE()
  );
  INSERT INTO dbo.app_sync_version(id,versao,atualizado_em)
  VALUES(1,0,GETDATE());
END
ELSE IF NOT EXISTS(SELECT 1 FROM dbo.app_sync_version WHERE id=1)
BEGIN
  INSERT INTO dbo.app_sync_version(id,versao,atualizado_em)
  VALUES(1,0,GETDATE());
END
")

dbe <- function(statement, params=list()) {
  statement <- translate_sqlserver(statement)
  params <- sanitize_params(params)

  resultado <- executar_com_retry(
    function() DBI::dbExecute(con,statement,params=params),
    statement,params
  )

  # Só alterações de dados disparam a versão global.
  # DDL (CREATE/ALTER), SELECT e a própria tabela de sincronização
  # não provocam atualização visual.
  sql_inicio <- toupper(trimws(statement))
  eh_dml <- grepl("^(INSERT|UPDATE|DELETE|MERGE)\\b", sql_inicio)
  eh_sync <- grepl("APP_SYNC_VERSION", sql_inicio, fixed=TRUE)

  if(eh_dml && !eh_sync) {
    try(
      DBI::dbExecute(
        con,
        "UPDATE dbo.app_sync_version
            SET versao=versao+1, atualizado_em=GETDATE()
          WHERE id=1"
      ),
      silent=TRUE
    )
  }

  resultado
}

# CORREÇÃO: SCOPE_IDENTITY() retorna NULL quando chamado em outro lote
# (cada chamada do driver ODBC é um lote separado). @@IDENTITY vale para
# a sessão inteira. As tabelas não possuem triggers, então é seguro.
db_last_id <- function() {
  x <- DBI::dbGetQuery(con, "SELECT CAST(@@IDENTITY AS INT) AS id")
  if(nrow(x)==0 || is.na(x$id[1])) stop("Não foi possível obter o ID gerado pelo SQL Server.")
  as.integer(x$id[1])
}

index_exists <- function(index_name, table_name) {
  nrow(DBI::dbGetQuery(con,
    "SELECT 1 AS existe
       FROM sys.indexes i
       JOIN sys.tables t ON t.object_id=i.object_id
       JOIN sys.schemas s ON s.schema_id=t.schema_id
      WHERE s.name='dbo' AND t.name=? AND i.name=?",
    params=list(table_name,index_name))) > 0
}

ensure_index <- function(index_name, table_name, columns_sql, unique=FALSE, where_sql=NULL) {
  if(index_exists(index_name,table_name)) return(invisible(NULL))
  uq <- if(isTRUE(unique)) "UNIQUE " else ""
  wh <- if(is.null(where_sql)) "" else paste0(" WHERE ",where_sql)
  dbe(sprintf("CREATE %sINDEX %s ON dbo.%s(%s)%s", uq,index_name,table_name,columns_sql,wh))
  invisible(NULL)
}

drop_index_if_exists <- function(index_name, table_name) {
  if(index_exists(index_name,table_name))
    dbe(sprintf("DROP INDEX %s ON dbo.%s", index_name, table_name))
  invisible(NULL)
}

# ---------------------------------------------------------
# ESTRUTURA DO SQL SERVER
# ---------------------------------------------------------
dbe("IF OBJECT_ID(N'dbo.barbearias',N'U') IS NULL
CREATE TABLE dbo.barbearias(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_barbearias PRIMARY KEY,
 nome NVARCHAR(200) NOT NULL,
 telefone NVARCHAR(30) NULL,
 status_acesso NVARCHAR(20) NOT NULL DEFAULT 'BLOQUEADA',
 data_cadastro DATE NOT NULL,
 nome_responsavel NVARCHAR(200) NULL,
 cpf_cnpj NVARCHAR(20) NULL,
 slug_publico NVARCHAR(100) COLLATE Latin1_General_100_CI_AS NULL
)")

dbe("IF OBJECT_ID(N'dbo.dados_barbearia',N'U') IS NULL
CREATE TABLE dbo.dados_barbearia(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_dados_barbearia PRIMARY KEY,
 barbearia_id INT NOT NULL UNIQUE,
 nome_responsavel NVARCHAR(200) NULL,
 cpf_cnpj NVARCHAR(20) NULL,
 cep NVARCHAR(20) NULL,
 endereco NVARCHAR(250) NULL,
 numero NVARCHAR(30) NULL,
 bairro NVARCHAR(150) NULL,
 cidade NVARCHAR(150) NULL,
 complemento NVARCHAR(200) NULL
)")

dbe("IF OBJECT_ID(N'dbo.assinaturas',N'U') IS NULL
CREATE TABLE dbo.assinaturas(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_assinaturas PRIMARY KEY,
 barbearia_id INT NOT NULL,
 plano NVARCHAR(30) NOT NULL DEFAULT 'MENSAL',
 valor_mensal DECIMAL(18,2) NOT NULL DEFAULT 0,
 data_inicio DATE NOT NULL,
 data_vencimento DATE NOT NULL,
 status NVARCHAR(20) NOT NULL DEFAULT 'ATIVA',
 data_cancelamento DATE NULL
)")

dbe("IF OBJECT_ID(N'dbo.clientes',N'U') IS NULL
CREATE TABLE dbo.clientes(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_clientes PRIMARY KEY,
 nome NVARCHAR(200) NOT NULL,
 telefone NVARCHAR(30) NULL,
 email NVARCHAR(320) COLLATE Latin1_General_100_CI_AS NULL,
 data_cadastro DATE NULL,
 hora_cadastro TIME(0) NULL,
 status NVARCHAR(20) NOT NULL DEFAULT 'ATIVO',
 barbearia_id INT NULL,
 cadastro_cliente_id INT NULL
)")

dbe("IF OBJECT_ID(N'dbo.cadastros_clientes',N'U') IS NULL
CREATE TABLE dbo.cadastros_clientes(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_cadastros_clientes PRIMARY KEY,
 barbearia_id INT NOT NULL,
 usuario_id INT NULL,
 cliente_id INT NULL,
 nome NVARCHAR(200) NOT NULL,
 telefone NVARCHAR(30) NOT NULL,
 email NVARCHAR(320) COLLATE Latin1_General_100_CI_AS NOT NULL,
 data_cadastro DATE NOT NULL,
 hora_cadastro TIME(0) NULL,
 status NVARCHAR(30) NOT NULL DEFAULT 'CADASTRADO',
 data_conversao DATE NULL
)")

# Colunas acrescentadas em bancos que já existiam antes desta versão.
# Registros antigos mantêm hora_cadastro NULL, pois o horário original não pode ser reconstruído.
dbe("IF COL_LENGTH(N'dbo.barbearias',N'nome_responsavel') IS NULL
     ALTER TABLE dbo.barbearias ADD nome_responsavel NVARCHAR(200) NULL")
dbe("IF COL_LENGTH(N'dbo.barbearias',N'cpf_cnpj') IS NULL
     ALTER TABLE dbo.barbearias ADD cpf_cnpj NVARCHAR(20) NULL")
dbe("IF COL_LENGTH(N'dbo.cadastros_clientes',N'hora_cadastro') IS NULL
     ALTER TABLE dbo.cadastros_clientes ADD hora_cadastro TIME(0) NULL")
dbe("IF COL_LENGTH(N'dbo.clientes',N'hora_cadastro') IS NULL
     ALTER TABLE dbo.clientes ADD hora_cadastro TIME(0) NULL")

dbe("IF OBJECT_ID(N'dbo.servicos',N'U') IS NULL
CREATE TABLE dbo.servicos(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_servicos PRIMARY KEY,
 nome NVARCHAR(150) NOT NULL,
 preco DECIMAL(18,2) NOT NULL DEFAULT 0,
 duracao_minutos INT NOT NULL DEFAULT 30,
 status NVARCHAR(20) NOT NULL DEFAULT 'ATIVO',
 barbearia_id INT NULL
)")

dbe("IF OBJECT_ID(N'dbo.atendimentos',N'U') IS NULL
CREATE TABLE dbo.atendimentos(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_atendimentos PRIMARY KEY,
 cliente_id INT NOT NULL,
 servico_id INT NOT NULL,
 data DATE NOT NULL,
 hora TIME(0) NOT NULL,
 preco DECIMAL(18,2) NOT NULL DEFAULT 0,
 duracao_minutos INT NOT NULL DEFAULT 30,
 status NVARCHAR(20) NOT NULL DEFAULT 'AGENDADO',
 observacao NVARCHAR(MAX) NULL,
 data_criacao DATETIME2(0) NOT NULL,
 barbearia_id INT NULL,
 barbeiro_id INT NULL
)")

# Múltiplos barbeiros: cada atendimento pertence a um barbeiro da própria barbearia.
# Registros antigos permanecem com barbeiro_id NULL, pois não é possível descobrir
# com segurança quem realizou o atendimento histórico.
dbe("IF COL_LENGTH(N'dbo.atendimentos',N'barbeiro_id') IS NULL
     ALTER TABLE dbo.atendimentos ADD barbeiro_id INT NULL")

# Confirmação do agendamento e auditoria de quem criou/confirmou.
# Registros históricos ficam NULL quando a informação original não pode ser reconstruída.
dbe("IF COL_LENGTH(N'dbo.atendimentos',N'status_confirmacao') IS NULL
     ALTER TABLE dbo.atendimentos ADD status_confirmacao NVARCHAR(20) NULL")
dbe("IF COL_LENGTH(N'dbo.atendimentos',N'criado_por_perfil') IS NULL
     ALTER TABLE dbo.atendimentos ADD criado_por_perfil NVARCHAR(20) NULL")
dbe("IF COL_LENGTH(N'dbo.atendimentos',N'criado_por_usuario_id') IS NULL
     ALTER TABLE dbo.atendimentos ADD criado_por_usuario_id INT NULL")
dbe("IF COL_LENGTH(N'dbo.atendimentos',N'confirmado_em') IS NULL
     ALTER TABLE dbo.atendimentos ADD confirmado_em DATETIME2(0) NULL")
dbe("IF COL_LENGTH(N'dbo.atendimentos',N'confirmado_por_usuario_id') IS NULL
     ALTER TABLE dbo.atendimentos ADD confirmado_por_usuario_id INT NULL")

# Central multicanal. EMAIL já é funcional; WHATSAPP fica reservado para integração futura.
dbe("IF OBJECT_ID(N'dbo.notificacoes',N'U') IS NULL
CREATE TABLE dbo.notificacoes(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_notificacoes PRIMARY KEY,
 barbearia_id INT NOT NULL,
 atendimento_id INT NULL,
 cliente_id INT NULL,
 canal NVARCHAR(20) NOT NULL,
 tipo NVARCHAR(50) NOT NULL,
 destinatario NVARCHAR(320) NOT NULL,
 assunto NVARCHAR(300) NULL,
 mensagem NVARCHAR(MAX) NOT NULL,
 status NVARCHAR(20) NOT NULL DEFAULT 'PENDENTE',
 programado_em DATETIME2(0) NOT NULL,
 processando_em DATETIME2(0) NULL,
 enviado_em DATETIME2(0) NULL,
 erro NVARCHAR(MAX) NULL,
 tentativas INT NOT NULL DEFAULT 0,
 criado_em DATETIME2(0) NOT NULL,
 criado_por_usuario_id INT NULL,
 criado_por_perfil NVARCHAR(20) NULL
)")
ensure_index("ix_notificacoes_status_programado","notificacoes","status,programado_em")
ensure_index("ix_notificacoes_barbearia","notificacoes","barbearia_id,criado_em")

dbe("IF OBJECT_ID(N'dbo.bloqueios_agenda',N'U') IS NULL
CREATE TABLE dbo.bloqueios_agenda(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_bloqueios_agenda PRIMARY KEY,
 barbearia_id INT NOT NULL,
 nome_barbearia NVARCHAR(200) NOT NULL,
 barbeiro_id INT NULL,
 nome_barbeiro NVARCHAR(150) NULL,
 data DATE NOT NULL,
 hora_inicio TIME(0) NOT NULL,
 hora_fim TIME(0) NOT NULL,
 motivo NVARCHAR(300) NULL,
 criado_por_usuario_id INT NULL,
 criado_por_usuario NVARCHAR(150) NULL,
 criado_por_perfil NVARCHAR(20) NULL,
 criado_em DATETIME2(0) NOT NULL
)")

dbe("IF COL_LENGTH(N'dbo.bloqueios_agenda',N'criado_por_usuario') IS NULL
     ALTER TABLE dbo.bloqueios_agenda ADD criado_por_usuario NVARCHAR(150) NULL")

# Auditoria de remoção de bloqueios: o registro nunca é apagado fisicamente.
dbe("IF COL_LENGTH(N'dbo.bloqueios_agenda',N'status') IS NULL
     ALTER TABLE dbo.bloqueios_agenda ADD status NVARCHAR(20) NULL")
dbe("UPDATE dbo.bloqueios_agenda SET status='ATIVO'
     WHERE status IS NULL OR LTRIM(RTRIM(status))=''")
dbe("IF NOT EXISTS (
       SELECT 1 FROM sys.default_constraints dc
       INNER JOIN sys.columns c ON c.default_object_id=dc.object_id
       WHERE dc.parent_object_id=OBJECT_ID(N'dbo.bloqueios_agenda')
         AND c.name=N'status'
     )
     ALTER TABLE dbo.bloqueios_agenda
     ADD CONSTRAINT DF_bloqueios_agenda_status DEFAULT 'ATIVO' FOR status")

dbe("IF COL_LENGTH(N'dbo.bloqueios_agenda',N'excluido_por_usuario_id') IS NULL
     ALTER TABLE dbo.bloqueios_agenda ADD excluido_por_usuario_id INT NULL")
dbe("IF COL_LENGTH(N'dbo.bloqueios_agenda',N'excluido_por_usuario') IS NULL
     ALTER TABLE dbo.bloqueios_agenda ADD excluido_por_usuario NVARCHAR(150) NULL")
dbe("IF COL_LENGTH(N'dbo.bloqueios_agenda',N'excluido_por_perfil') IS NULL
     ALTER TABLE dbo.bloqueios_agenda ADD excluido_por_perfil NVARCHAR(20) NULL")
dbe("IF COL_LENGTH(N'dbo.bloqueios_agenda',N'excluido_em') IS NULL
     ALTER TABLE dbo.bloqueios_agenda ADD excluido_em DATETIME2(0) NULL")

ensure_index("ix_bloqueios_agenda_barbearia_data","bloqueios_agenda","barbearia_id,data,hora_inicio,hora_fim")
ensure_index("ix_bloqueios_agenda_barbeiro_data","bloqueios_agenda","barbearia_id,barbeiro_id,data,hora_inicio,hora_fim")

dbe("IF OBJECT_ID(N'dbo.horarios_funcionamento',N'U') IS NULL
CREATE TABLE dbo.horarios_funcionamento(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_horarios_funcionamento PRIMARY KEY,
 barbearia_id INT NOT NULL,
 nome_barbearia NVARCHAR(200) NOT NULL,
 dia_semana TINYINT NOT NULL,
 nome_dia NVARCHAR(20) NOT NULL,
 aberto BIT NOT NULL DEFAULT 1,
 hora_abertura TIME(0) NULL,
 hora_fechamento TIME(0) NULL,
 atualizado_por_usuario_id INT NULL,
 atualizado_por_usuario NVARCHAR(150) NULL,
 atualizado_por_perfil NVARCHAR(20) NULL,
 atualizado_em DATETIME2(0) NOT NULL
)")

dbe("IF COL_LENGTH(N'dbo.horarios_funcionamento',N'atualizado_por_usuario') IS NULL
     ALTER TABLE dbo.horarios_funcionamento ADD atualizado_por_usuario NVARCHAR(150) NULL")

ensure_index("ux_horarios_funcionamento_barbearia_dia","horarios_funcionamento","barbearia_id,dia_semana",TRUE)

dbe("UPDATE h
     SET h.atualizado_por_usuario=u.nome_usuario
     FROM horarios_funcionamento h
     INNER JOIN usuarios u ON u.id=h.atualizado_por_usuario_id
     WHERE h.atualizado_por_usuario IS NULL")

dbe("UPDATE b
     SET b.criado_por_usuario=u.nome_usuario
     FROM bloqueios_agenda b
     INNER JOIN usuarios u ON u.id=b.criado_por_usuario_id
     WHERE b.criado_por_usuario IS NULL")

# Preserva o comportamento das barbearias já existentes até que o horário seja configurado.
# 1=segunda ... 7=domingo. 00:00-23:59 significa dia inteiro disponível.
dbe("INSERT INTO horarios_funcionamento(
       barbearia_id,nome_barbearia,dia_semana,nome_dia,aberto,
       hora_abertura,hora_fechamento,atualizado_por_usuario_id,atualizado_por_usuario,
       atualizado_por_perfil,atualizado_em)
     SELECT b.id,b.nome,d.dia,d.nome_dia,1,CAST('00:00' AS TIME),CAST('23:59' AS TIME),
            NULL,NULL,'MIGRACAO',SYSDATETIME()
     FROM barbearias b
     CROSS JOIN (VALUES
       (1,N'Segunda'),(2,N'Terça'),(3,N'Quarta'),(4,N'Quinta'),
       (5,N'Sexta'),(6,N'Sábado'),(7,N'Domingo')
     ) d(dia,nome_dia)
     WHERE NOT EXISTS(
       SELECT 1 FROM horarios_funcionamento h
       WHERE h.barbearia_id=b.id AND h.dia_semana=d.dia
     )")

dbe("IF OBJECT_ID(N'dbo.horarios_barbeiros',N'U') IS NULL
CREATE TABLE dbo.horarios_barbeiros(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_horarios_barbeiros PRIMARY KEY,
 barbearia_id INT NOT NULL,
 nome_barbearia NVARCHAR(200) NOT NULL,
 barbeiro_id INT NOT NULL,
 nome_barbeiro NVARCHAR(150) NOT NULL,
 dia_semana TINYINT NOT NULL,
 nome_dia NVARCHAR(20) NOT NULL,
 trabalha BIT NOT NULL DEFAULT 1,
 hora_inicio TIME(0) NULL,
 hora_fim TIME(0) NULL,
 atualizado_por_usuario_id INT NULL,
 atualizado_por_usuario NVARCHAR(150) NULL,
 atualizado_por_perfil NVARCHAR(20) NULL,
 atualizado_em DATETIME2(0) NOT NULL
)")

ensure_index("ux_horarios_barbeiros_barb_barbeiro_dia",
             "horarios_barbeiros","barbearia_id,barbeiro_id,dia_semana",TRUE)
ensure_index("ix_horarios_barbeiros_consulta",
             "horarios_barbeiros","barbearia_id,barbeiro_id,dia_semana,trabalha")

dbe("UPDATE h
     SET h.nome_barbearia=b.nome,
         h.nome_barbeiro=u.nome_usuario
     FROM horarios_barbeiros h
     INNER JOIN barbearias b ON b.id=h.barbearia_id
     INNER JOIN usuarios u ON u.id=h.barbeiro_id")

dbe("IF OBJECT_ID(N'dbo.gastos',N'U') IS NULL
CREATE TABLE dbo.gastos(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_gastos PRIMARY KEY,
 data DATE NOT NULL,
 descricao NVARCHAR(250) NOT NULL,
 categoria NVARCHAR(100) NOT NULL,
 quantidade DECIMAL(18,4) NOT NULL DEFAULT 1,
 valor_unitario DECIMAL(18,2) NOT NULL DEFAULT 0,
 valor_total DECIMAL(18,2) NOT NULL DEFAULT 0,
 barbearia_id INT NULL
)")

dbe("IF OBJECT_ID(N'dbo.usuarios',N'U') IS NULL
CREATE TABLE dbo.usuarios(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_usuarios PRIMARY KEY,
 barbearia_id INT NULL,
 cliente_id INT NULL,
 nome_usuario NVARCHAR(150) COLLATE Latin1_General_100_CI_AS NOT NULL,
 email NVARCHAR(320) COLLATE Latin1_General_100_CI_AS NOT NULL,
 senha_hash NVARCHAR(255) NULL,
 perfil NVARCHAR(20) NOT NULL,
 ativo INT NOT NULL DEFAULT 1,
 troca_senha_obrigatoria INT NOT NULL DEFAULT 0,
 data_cadastro DATE NOT NULL
)")

dbe("IF OBJECT_ID(N'dbo.recuperacao_senha',N'U') IS NULL
CREATE TABLE dbo.recuperacao_senha(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_recuperacao_senha PRIMARY KEY,
 usuario_id INT NOT NULL,
 token_hash NVARCHAR(255) NOT NULL,
 tipo NVARCHAR(30) NOT NULL,
 expira_em DATETIME2(0) NOT NULL,
 usado INT NOT NULL DEFAULT 0,
 criado_em DATETIME2(0) NOT NULL
)")

dbe("IF OBJECT_ID(N'dbo.historico_alteracoes',N'U') IS NULL
CREATE TABLE dbo.historico_alteracoes(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_historico_alteracoes PRIMARY KEY,
 barbearia_id INT NULL,
 usuario_id INT NULL,
 perfil_usuario NVARCHAR(20) NULL,
 acao NVARCHAR(50) NOT NULL,
 entidade NVARCHAR(100) NOT NULL,
 campo NVARCHAR(100) NULL,
 valor_anterior NVARCHAR(MAX) NULL,
 valor_novo NVARCHAR(MAX) NULL,
 data_alteracao DATETIME2(0) NOT NULL
)")

# Coluna calculada "nome_barbearia" em clientes e usuarios.
# Sempre mostra o nome atual da barbearia ligada ao barbearia_id
# (NULL quando não há barbearia, como no usuário MASTER).
dbe("IF OBJECT_ID(N'dbo.fn_nome_barbearia',N'FN') IS NULL
EXEC('CREATE FUNCTION dbo.fn_nome_barbearia(@id INT)
RETURNS NVARCHAR(200)
AS
BEGIN
  RETURN (SELECT nome FROM dbo.barbearias WHERE id=@id)
END')")

dbe("IF OBJECT_ID(N'dbo.fn_nome_barbearia_usuario',N'FN') IS NULL
EXEC('CREATE FUNCTION dbo.fn_nome_barbearia_usuario(@usuario_id INT)
RETURNS NVARCHAR(200)
AS
BEGIN
  RETURN (SELECT b.nome FROM dbo.usuarios u
          JOIN dbo.barbearias b ON b.id=u.barbearia_id
          WHERE u.id=@usuario_id)
END')")

# Tabelas que possuem barbearia_id.
for(tb in c("clientes","usuarios","assinaturas","atendimentos",
            "cadastros_clientes","dados_barbearia","servicos","gastos"))
  dbe(sprintf("IF COL_LENGTH(N'dbo.%s',N'nome_barbearia') IS NULL
ALTER TABLE dbo.%s ADD nome_barbearia AS dbo.fn_nome_barbearia(barbearia_id)",tb,tb))

# recuperacao_senha não tem barbearia_id: o nome vem pelo usuário.
dbe("IF COL_LENGTH(N'dbo.recuperacao_senha',N'nome_barbearia') IS NULL
ALTER TABLE dbo.recuperacao_senha ADD nome_barbearia AS dbo.fn_nome_barbearia_usuario(usuario_id)")

# Mantém na tabela principal barbearias os dados do responsável que já existiam
# em dados_barbearia. Isso preenche as novas colunas sem perder o modelo anterior.
dbe("UPDATE b
     SET b.nome_responsavel=d.nome_responsavel, b.cpf_cnpj=d.cpf_cnpj
     FROM dbo.barbearias b
     INNER JOIN dbo.dados_barbearia d ON d.barbearia_id=b.id
     WHERE (b.nome_responsavel IS NULL OR LTRIM(RTRIM(b.nome_responsavel))='')
        OR (b.cpf_cnpj IS NULL OR LTRIM(RTRIM(b.cpf_cnpj))='')")

# ---------------------------------------------------------
# TABELA FINANCEIRO
# ---------------------------------------------------------
# Reúne em um só lugar todas as ENTRADAS (atendimentos CONCLUÍDOS) e todos
# os GASTOS de cada barbearia. É mantida automaticamente por triggers em
# atendimentos e gastos, então acompanha qualquer alteração (pelo app ou
# direto no SQL Server). Se algo falhar aqui, o app segue funcionando.
tryCatch({
  dbe("IF OBJECT_ID(N'dbo.financeiro',N'U') IS NULL
CREATE TABLE dbo.financeiro(
 id INT IDENTITY(1,1) NOT NULL CONSTRAINT PK_financeiro PRIMARY KEY,
 barbearia_id INT NULL,
 tipo NVARCHAR(10) NOT NULL,
 origem NVARCHAR(20) NOT NULL,
 origem_id INT NOT NULL,
 data DATE NOT NULL,
 descricao NVARCHAR(300) NULL,
 categoria NVARCHAR(100) NULL,
 valor DECIMAL(18,2) NOT NULL DEFAULT 0,
 valor_liquido AS (CASE WHEN tipo=N'GASTO' THEN -valor ELSE valor END)
)")

  dbe("IF COL_LENGTH(N'dbo.financeiro',N'nome_barbearia') IS NULL
ALTER TABLE dbo.financeiro ADD nome_barbearia AS dbo.fn_nome_barbearia(barbearia_id)")

  ensure_index("ux_financeiro_origem","financeiro","origem,origem_id",TRUE)
  ensure_index("ix_financeiro_barbearia_data","financeiro","barbearia_id,data,tipo")

  # Trigger dos atendimentos: só entram os CONCLUÍDOS (literal montado com
  # NCHAR(205) = Í para não depender do code page).
  dbe("IF OBJECT_ID(N'dbo.trg_atendimentos_financeiro',N'TR') IS NULL
EXEC('CREATE TRIGGER dbo.trg_atendimentos_financeiro ON dbo.atendimentos
AFTER INSERT,UPDATE,DELETE
AS
BEGIN
  SET NOCOUNT ON;
  DELETE f FROM dbo.financeiro f
  WHERE f.origem=''ATENDIMENTO''
    AND f.origem_id IN (SELECT id FROM inserted UNION SELECT id FROM deleted);
  INSERT INTO dbo.financeiro(tipo,origem,origem_id,barbearia_id,data,descricao,categoria,valor)
  SELECT ''ENTRADA'',''ATENDIMENTO'',a.id,a.barbearia_id,a.data,
         s.nome+N'' - ''+c.nome,N''Atendimento'',a.preco
  FROM inserted a
  LEFT JOIN dbo.servicos s ON s.id=a.servico_id
  LEFT JOIN dbo.clientes c ON c.id=a.cliente_id
  WHERE a.status=N''CONCLU''+NCHAR(205)+N''DO'';
END')")

  dbe("IF OBJECT_ID(N'dbo.trg_gastos_financeiro',N'TR') IS NULL
EXEC('CREATE TRIGGER dbo.trg_gastos_financeiro ON dbo.gastos
AFTER INSERT,UPDATE,DELETE
AS
BEGIN
  SET NOCOUNT ON;
  DELETE f FROM dbo.financeiro f
  WHERE f.origem=''GASTO''
    AND f.origem_id IN (SELECT id FROM inserted UNION SELECT id FROM deleted);
  INSERT INTO dbo.financeiro(tipo,origem,origem_id,barbearia_id,data,descricao,categoria,valor)
  SELECT ''GASTO'',''GASTO'',g.id,g.barbearia_id,g.data,g.descricao,g.categoria,g.valor_total
  FROM inserted g;
END')")

  # Carga inicial dos registros que já existiam antes da tabela.
  dbe("INSERT INTO financeiro(tipo,origem,origem_id,barbearia_id,data,descricao,categoria,valor)
SELECT 'ENTRADA','ATENDIMENTO',a.id,a.barbearia_id,a.data,s.nome+N' - '+c.nome,N'Atendimento',a.preco
FROM atendimentos a
LEFT JOIN servicos s ON s.id=a.servico_id
LEFT JOIN clientes c ON c.id=a.cliente_id
WHERE a.status='CONCLUÍDO'
  AND NOT EXISTS (SELECT 1 FROM financeiro f WHERE f.origem='ATENDIMENTO' AND f.origem_id=a.id)")

  dbe("INSERT INTO financeiro(tipo,origem,origem_id,barbearia_id,data,descricao,categoria,valor)
SELECT 'GASTO','GASTO',g.id,g.barbearia_id,g.data,g.descricao,g.categoria,g.valor_total
FROM gastos g
WHERE NOT EXISTS (SELECT 1 FROM financeiro f WHERE f.origem='GASTO' AND f.origem_id=g.id)")
}, error=function(e)
  message("Aviso: não foi possível preparar a tabela financeiro: ",
          conditionMessage(e)))

# Índices da aplicação. Os nomes foram mantidos para facilitar a auditoria.
ensure_index("ux_barbearia_slug_publico","barbearias","slug_publico",TRUE,"slug_publico IS NOT NULL AND slug_publico <> ''")
ensure_index("ux_cadastro_cliente_email_barb","cadastros_clientes","barbearia_id,email",TRUE,"email IS NOT NULL AND email <> ''")
ensure_index("ux_cadastro_cliente_usuario","cadastros_clientes","usuario_id",TRUE,"usuario_id IS NOT NULL")
ensure_index("idx_cadastros_clientes_barbearia","cadastros_clientes","barbearia_id,status,data_cadastro")
ensure_index("idx_clientes_cadastro_publico","clientes","cadastro_cliente_id")
ensure_index("idx_historico_barbearia","historico_alteracoes","barbearia_id,entidade,campo,data_alteracao")

# Remove índices legados e redundantes de versões SQLite anteriores, caso existam.
for(x in list(
  list(name="ux_user_email",table="usuarios"),
  list(name="ux_client_email",table="clientes"),
  list(name="ux_user_login_global",table="usuarios"),
  list(name="ux_user_staff_email",table="usuarios"),
  list(name="ux_cliente_email_barbearia",table="clientes"),
  list(name="ux_user_cliente_email_barbearia",table="usuarios"),
  list(name="ux_client_user_email_barb",table="usuarios"),
  list(name="ux_client_email_barb",table="clientes")
)) drop_index_if_exists(x$name,x$table)

ensure_index("ux_user_login","usuarios","nome_usuario",TRUE)
ensure_index("ux_user_email_nonclient","usuarios","email",TRUE,"perfil <> 'CLIENTE' AND email IS NOT NULL AND email <> ''")
ensure_index("ux_client_user_email_barb","usuarios","barbearia_id,email",TRUE,"perfil='CLIENTE' AND email IS NOT NULL AND email <> ''")
ensure_index("ux_client_email_barb","clientes","barbearia_id,email",TRUE,"email IS NOT NULL AND email <> ''")

# Índices para as relações mais consultadas.
ensure_index("ix_assinaturas_barbearia","assinaturas","barbearia_id,id")
ensure_index("ix_clientes_barbearia","clientes","barbearia_id,id")
ensure_index("ix_servicos_barbearia","servicos","barbearia_id,status,id")
ensure_index("ix_atendimentos_barbearia_data_hora","atendimentos","barbearia_id,data,hora,status")
ensure_index("ix_atendimentos_barbeiro_data_hora","atendimentos","barbearia_id,barbeiro_id,data,hora,status")
ensure_index("ix_gastos_barbearia_data","gastos","barbearia_id,data")
ensure_index("ix_usuarios_barbearia_perfil","usuarios","barbearia_id,perfil,id")

# ---------------------------------------------------------
# MIGRAÇÃO DA BASE EXISTENTE
# ---------------------------------------------------------
base <- dbq("SELECT id FROM barbearias ORDER BY id LIMIT 1")
if(nrow(base)==0) {
  dbe("
    INSERT INTO barbearias(nome,telefone,status_acesso,data_cadastro)
    VALUES('Barbearia Principal','','BLOQUEADA',?)
  ",params=list(as.character(Sys.Date())))
}
barbearia_principal_id <- as.integer(dbq(
  "SELECT id FROM barbearias ORDER BY id LIMIT 1")$id[1])

if(nrow(dbq("SELECT id FROM dados_barbearia WHERE barbearia_id=?",
                   params=list(barbearia_principal_id)))==0)
  dbe("INSERT INTO dados_barbearia(barbearia_id) VALUES(?)",
            params=list(barbearia_principal_id))

for(tab in c("clientes","servicos","atendimentos","gastos"))
  dbe(paste0("UPDATE ",tab," SET barbearia_id=? WHERE barbearia_id IS NULL"),
            params=list(barbearia_principal_id))

if(nrow(dbq("SELECT id FROM assinaturas WHERE barbearia_id=? LIMIT 1",
                   params=list(barbearia_principal_id)))==0)
  dbe("
    INSERT INTO assinaturas(barbearia_id,plano,valor_mensal,data_inicio,data_vencimento,status)
    VALUES(?,'MENSAL',0,?,?, 'BLOQUEADA')
  ",params=list(barbearia_principal_id,as.character(Sys.Date()),as.character(Sys.Date())))

`%||%` <- function(x,y) {
  if(is.null(x) || length(x)==0 || is.na(x[1])) y else x[1]
}

# ---------------------------------------------------------
# SLUG PÚBLICO DAS BARBEARIAS
# ---------------------------------------------------------
# O slug é gerado uma única vez por barbearia e permanece estável mesmo
# que o nome comercial seja alterado depois. Isso evita quebrar links
# públicos já divulgados.
slug_base <- function(nome) {
  x <- trimws(as.character(nome %||% ""))
  if(x=="") x <- "barbearia"
  x <- iconv(x,from="UTF-8",to="ASCII//TRANSLIT")
  if(is.na(x)||!nzchar(x)) x <- "barbearia"
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+","-",x)
  x <- gsub("^-+|-+$","",x)
  if(x=="") x <- "barbearia"
  substr(x,1,60)
}

slug_unico <- function(nome,id_excluir=NA_integer_) {
  base_slug <- slug_base(nome)
  candidato <- base_slug
  n <- 1L
  repeat {
    q <- if(is.na(id_excluir))
      dbq("SELECT id FROM barbearias WHERE LOWER(slug_publico)=LOWER(?) LIMIT 1",params=list(candidato))
    else
      dbq("SELECT id FROM barbearias WHERE LOWER(slug_publico)=LOWER(?) AND id<>? LIMIT 1",params=list(candidato,id_excluir))
    if(nrow(q)==0) return(candidato)
    n <- n + 1L
    candidato <- paste0(base_slug,"-",n)
  }
}

# Migra barbearias existentes e gera slug apenas para quem ainda não possui.
barbs_sem_slug <- dbq("
  SELECT id,nome FROM barbearias
  WHERE slug_publico IS NULL OR LTRIM(RTRIM(slug_publico))=''
  ORDER BY id
")
if(nrow(barbs_sem_slug)>0) {
  for(i in seq_len(nrow(barbs_sem_slug))) {
    bid <- as.integer(barbs_sem_slug$id[i])
    slug <- slug_unico(barbs_sem_slug$nome[i],id_excluir=bid)
    dbe("UPDATE barbearias SET slug_publico=? WHERE id=?",
              params=list(slug,bid))
  }
}

ensure_index("ux_barbearia_slug_publico","barbearias","slug_publico",TRUE,"slug_publico IS NOT NULL AND slug_publico <> ''")

# ---------------------------------------------------------
# MIGRAÇÃO DOS CADASTROS PÚBLICOS EXISTENTES
# ---------------------------------------------------------
# Versões anteriores criavam diretamente em clientes quando o cadastro
# público era realizado. Migramos esses registros para cadastros_clientes.
# Se ainda não existir atendimento, o registro sai de clientes e permanece
# apenas no cadastro público. Se já existir atendimento, ele continua em
# clientes e fica marcado como CONVERTIDO.
contas_cliente_existentes <- dbq("
  SELECT u.id AS usuario_id,u.barbearia_id,u.cliente_id,u.email,
         c.id AS cliente_id_real,c.nome,c.telefone,c.data_cadastro
  FROM usuarios u
  JOIN clientes c ON c.id=u.cliente_id AND c.barbearia_id=u.barbearia_id
  WHERE u.perfil='CLIENTE'
  ORDER BY u.id
")

if(nrow(contas_cliente_existentes)>0) {
  for(i in seq_len(nrow(contas_cliente_existentes))) {
    r <- contas_cliente_existentes[i,]
    cad <- dbq("SELECT id FROM cadastros_clientes WHERE usuario_id=? LIMIT 1",
                      params=list(as.integer(r$usuario_id)))

    tem_atendimento <- dbq("SELECT id FROM atendimentos WHERE cliente_id=? LIMIT 1",
                                  params=list(as.integer(r$cliente_id_real)))

    if(nrow(cad)==0) {
      status_cad <- if(nrow(tem_atendimento)>0) 'CONVERTIDO' else 'CADASTRADO'
      data_conv <- if(nrow(tem_atendimento)>0) as.character(Sys.Date()) else NA_character_
      dbe("
        INSERT INTO cadastros_clientes(
          barbearia_id,usuario_id,cliente_id,nome,telefone,email,data_cadastro,status,data_conversao)
        VALUES(?,?,?,?,?,?,?,?,?)",
        params=list(as.integer(r$barbearia_id),as.integer(r$usuario_id),
                    if(nrow(tem_atendimento)>0) as.integer(r$cliente_id_real) else NA_integer_,
                    as.character(r$nome),as.character(r$telefone),as.character(r$email),
                    as.character(r$data_cadastro),status_cad,data_conv))
      cad <- dbq("SELECT id FROM cadastros_clientes WHERE usuario_id=? LIMIT 1",
                        params=list(as.integer(r$usuario_id)))
    }

    if(nrow(tem_atendimento)==0) {
      dbe("UPDATE usuarios SET cliente_id=NULL WHERE id=? AND perfil='CLIENTE'",
                params=list(as.integer(r$usuario_id)))
      dbe("DELETE FROM clientes WHERE id=?",
                params=list(as.integer(r$cliente_id_real)))
      dbe("UPDATE cadastros_clientes SET cliente_id=NULL,status='CADASTRADO',data_conversao=NULL WHERE id=?",
                params=list(as.integer(cad$id[1])))
    }
  }
}

# Índices da nova estrutura: e-mail é único somente dentro da barbearia.
ensure_index("ux_cadastro_cliente_email_barb","cadastros_clientes","barbearia_id,email",TRUE,"email IS NOT NULL AND email <> ''")
ensure_index("ux_cadastro_cliente_usuario","cadastros_clientes","usuario_id",TRUE,"usuario_id IS NOT NULL")
ensure_index("idx_cadastros_clientes_barbearia","cadastros_clientes","barbearia_id,status,data_cadastro")
ensure_index("idx_clientes_cadastro_publico","clientes","cadastro_cliente_id")

# ---------------------------------------------------------
# ÍNDICES DE LOGIN E E-MAIL
# ---------------------------------------------------------
# Usuários MASTER/BARBEIRO continuam com e-mail globalmente único.
# Clientes possuem conta independente por barbearia, então o mesmo e-mail
# pode existir em barbearias diferentes.
# Remove índices legados/redundantes de versões anteriores.
legacy_indexes <- list(
  ux_user_email=c("usuarios"),
  ux_client_email=c("clientes"),
  ux_user_login_global=c("usuarios"),
  ux_user_staff_email=c("usuarios"),
  ux_cliente_email_barbearia=c("clientes"),
  ux_user_cliente_email_barbearia=c("usuarios"),
  ux_client_user_email_barb=c("usuarios"),
  ux_client_email_barb=c("clientes")
)
for(idx in names(legacy_indexes)) {
  tab <- legacy_indexes[[idx]][1]
  drop_index_if_exists(idx,tab)
}

ensure_index("ux_user_login","usuarios","nome_usuario",TRUE)
ensure_index("ux_user_email_nonclient","usuarios","email",TRUE,"perfil <> 'CLIENTE' AND email IS NOT NULL AND email <> ''")
ensure_index("ux_client_user_email_barb","usuarios","barbearia_id,email",TRUE,"perfil='CLIENTE' AND email IS NOT NULL AND email <> ''")
ensure_index("ux_client_email_barb","clientes","barbearia_id,email",TRUE,"email IS NOT NULL AND email <> ''")

# ---------------------------------------------------------
# SERVIÇOS PADRÃO
# ---------------------------------------------------------
servicos_padrao <- data.frame(
  nome=c("Corte de cabelo","Cabelo + Barba","Barba"),
  preco=c(30,55,25),
  duracao_minutos=c(30,30,30),
  stringsAsFactors=FALSE
)

garantir_servicos <- function(id_barb) {
  qtd <- dbq("SELECT COUNT(*) qtd FROM servicos WHERE barbearia_id=?",params=list(id_barb))
  if(nrow(qtd)>0 && !is.na(qtd$qtd[1]) && as.integer(qtd$qtd[1])>0) return(invisible(TRUE))
  for(i in seq_len(nrow(servicos_padrao))) {
    x <- servicos_padrao[i,]
    dbe("
      INSERT INTO servicos(nome,preco,duracao_minutos,status,barbearia_id)
      VALUES(?,?,?,'ATIVO',?)",
      params=list(x$nome,x$preco,x$duracao_minutos,id_barb))
  }
  invisible(TRUE)
}
for(id in dbq("SELECT id FROM barbearias ORDER BY id")$id)
  garantir_servicos(as.integer(id))

# ---------------------------------------------------------
# SEGURANÇA
# ---------------------------------------------------------
validar_email <- function(x)
  grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$",trimws(x))

# ---------------------------------------------------------
# CPF / CNPJ
# ---------------------------------------------------------
# Aceita apenas os formatos usuais de CPF/CNPJ (com ou sem pontuação),
# mas grava no banco somente os dígitos para manter um padrão único.
normalizar_cpf_cnpj <- function(x) {
  if(is.null(x) || is.na(x)) return("")
  trimws(x)
}

validar_cpf_cnpj <- function(x, permitir_vazio=TRUE) {
  x <- normalizar_cpf_cnpj(x)
  if(x=="") {
    if(permitir_vazio) return(list(ok=TRUE,digitos="",tipo=""))
    return(list(ok=FALSE,msg="Informe um CPF ou CNPJ."))
  }

  # Permite somente dígitos, ponto, hífen, barra e espaços.
  if(!grepl("^[0-9./ -]+$",x))
    return(list(ok=FALSE,msg="CPF/CNPJ deve conter apenas números e a pontuação de CPF/CNPJ."))

  digitos <- gsub("[^0-9]","",x)

  if(nchar(digitos)==11)
    return(list(ok=TRUE,digitos=digitos,tipo="CPF"))

  if(nchar(digitos)==14)
    return(list(ok=TRUE,digitos=digitos,tipo="CNPJ"))

  list(ok=FALSE,
       msg="CPF deve ter 11 dígitos ou CNPJ deve ter 14 dígitos, com ou sem pontuação.")
}

formatar_cpf_cnpj <- function(x) {
  v <- validar_cpf_cnpj(x, permitir_vazio=TRUE)
  if(!v$ok || v$digitos=="") return("")
  d <- v$digitos
  if(v$tipo=="CPF")
    return(paste0(substr(d,1,3),".",substr(d,4,6),".",substr(d,7,9),"-",substr(d,10,11)))
  paste0(substr(d,1,2),".",substr(d,3,5),".",substr(d,6,8),"/",substr(d,9,12),"-",substr(d,13,14))
}

validar_senha <- function(s) {
  er <- character()
  if(is.null(s)||is.na(s)||nchar(s)<8) er <- c(er,"A senha deve ter pelo menos 8 caracteres.")
  if(!grepl("[A-Z]",s)) er <- c(er,"Use pelo menos 1 letra maiúscula.")
  if(!grepl("[a-z]",s)) er <- c(er,"Use pelo menos 1 letra minúscula.")
  if(!grepl("[0-9]",s)) er <- c(er,"Use pelo menos 1 número.")
  if(!grepl("[^A-Za-z0-9]",s)) er <- c(er,"Use pelo menos 1 caractere especial.")
  seqs <- c("0123","1234","2345","3456","4567","5678","6789",
            "9876","8765","7654","6543","5432","4321")
  if(any(vapply(seqs,function(z)grepl(z,s,fixed=TRUE),logical(1))))
    er <- c(er,"Não use sequências numéricas simples, como 1234 ou 9876.")
  if(grepl("(.)\\1\\1\\1",s,perl=TRUE))
    er <- c(er,"Não repita o mesmo caractere 4 vezes seguidas.")
  er
}

# Nome de usuário escolhido pelo cliente no cadastro público.
# Regras: 3 a 30 caracteres; só letras sem acento, números, ponto, hífen e
# underline; sem espaços nem "@" (para não se confundir com e-mail no login);
# e o padrão "cliente_<número>" é reservado ao sistema.
validar_nome_usuario <- function(x) {
  x <- trimws(as.character(if(is.null(x)) "" else x))
  if(length(x)==0 || is.na(x) || x=="")
    return(list(ok=FALSE,msg="Informe um nome de usuário."))
  if(nchar(x)<3 || nchar(x)>30)
    return(list(ok=FALSE,msg="O nome de usuário deve ter de 3 a 30 caracteres."))
  if(!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$",x))
    return(list(ok=FALSE,msg=paste0("Use apenas letras sem acento, números, ponto, hífen ",
                                     "e underline, sem espaços, começando por letra ou número.")))
  if(grepl("^cliente_[0-9]+$",x,ignore.case=TRUE))
    return(list(ok=FALSE,msg="Esse formato de nome de usuário é reservado. Escolha outro."))
  list(ok=TRUE,valor=x)
}

hash_senha <- function(s) sodium::password_store(s)
verificar_senha <- function(h,s)
  tryCatch(sodium::password_verify(h,s),error=function(e)FALSE)
gerar_token <- function() sodium::bin2hex(sodium::random(32))
hash_token <- function(t) sodium::bin2hex(sodium::hash(charToRaw(t),size=32))

formata_reais <- function(x) {
  x <- if(length(x)==0 || is.na(x[1])) 0 else as.numeric(x[1])
  paste0("R$ ",formatC(x, format="f", digits=2, decimal.mark=",", big.mark="."))
}

# ---------------------------------------------------------
# E-MAIL
# ---------------------------------------------------------
get_app_url <- function(session) {
  # APP_URL tem prioridade. Isso garante que links enviados por e-mail e
  # exibidos ao MASTER usem o domínio público mesmo quando o MASTER abriu
  # o sistema por um endereço local.
  app_url_env <- trimws(Sys.getenv("APP_URL", APP_URL))
  if(nzchar(app_url_env))
    return(sub("/+$","",app_url_env))

  # Fallback: detecta o endereço efetivamente usado pelo navegador.
  # Ao entrar pelo Dev Tunnel, o hostname recebido será o endereço público.
  proto <- as.character(session$clientData$url_protocol %||% "")
  proto <- sub(":$","",proto)
  host <- as.character(session$clientData$url_hostname %||% "")
  port <- as.character(session$clientData$url_port %||% "")

  if(!nzchar(proto) || !nzchar(host)) return("")

  # Evita anexar porta padrão. Em URLs públicas HTTPS do Dev Tunnel a porta
  # externa não deve ser substituída pela porta local do Shiny.
  host_publico <- grepl("devtunnels\\.ms$",host,ignore.case=TRUE)
  p <- ""
  if(nzchar(port) && !host_publico &&
     !((proto=="http" && port=="80") || (proto=="https" && port=="443")))
    p <- paste0(":",port)

  paste0(proto,"://",host,p)
}

# Gera o link público usando query string porque o servidor local do Shiny
# não roteia automaticamente URLs arbitrárias como /barbearia/<slug>.
# O formato /?barbearia=<slug> funciona tanto localmente quanto em um deploy
# padrão do Shiny e mantém o slug no endereço de forma estável.
link_publico_barbearia <- function(session,id_barb) {
  if(is.null(id_barb)||length(id_barb)==0||is.na(id_barb)) return("")
  b <- dbq("SELECT slug_publico FROM barbearias WHERE id=? LIMIT 1",
                  params=list(as.integer(id_barb)))
  if(nrow(b)==0||is.na(b$slug_publico[1])||!nzchar(trimws(b$slug_publico[1])))
    return("")
  slug <- utils::URLencode(trimws(b$slug_publico[1]),reserved=TRUE)
  base <- get_app_url(session)
  if(!nzchar(base)) return(paste0("/?barbearia=",slug))
  paste0(base,"/?barbearia=",slug)
}

# Obtém o slug público preferencialmente pela query string ?barbearia=<slug>.
# O formato antigo /barbearia/<slug> permanece aceito quando o servidor/deploy
# estiver configurado para encaminhar esse caminho ao aplicativo.
extrair_slug_publico <- function(path,query_string="") {
  qs <- as.character(query_string %||% "")
  if(nzchar(qs)) {
    q <- parseQueryString(qs)
    slug_q <- q$barbearia
    if(!is.null(slug_q) && length(slug_q)>0 && nzchar(trimws(slug_q[1])))
      return(trimws(utils::URLdecode(slug_q[1])))
  }

  path <- as.character(path %||% "")
  if(!nzchar(path)) return("")
  m <- regexec("/barbearia/([^/]+)/*$",path,perl=TRUE)
  partes <- regmatches(path,m)[[1]]
  if(length(partes)<2) return("")
  utils::URLdecode(partes[2])
}

buscar_barbearia_slug <- function(slug) {
  slug <- trimws(as.character(slug %||% ""))
  if(!nzchar(slug)) return(NULL)
  x <- dbq("
    SELECT b.id,b.nome,b.telefone,b.status_acesso,b.slug_publico,
           d.nome_responsavel,d.cep,d.endereco,d.numero,d.bairro,d.cidade,d.complemento,
           a.status AS status_assinatura,a.data_vencimento
    FROM barbearias b
    LEFT JOIN dados_barbearia d ON d.barbearia_id=b.id
    LEFT JOIN assinaturas a ON a.id=(
      SELECT MAX(a2.id) FROM assinaturas a2 WHERE a2.barbearia_id=b.id)
    WHERE LOWER(b.slug_publico)=LOWER(?)
    LIMIT 1",params=list(slug))
  if(nrow(x)==0) NULL else x[1,]
}

smtp_creds <- function() {
  if(!nzchar(SMTP_USER))
    stop("SMTP_USER não foi configurado.")
  if(!nzchar(SMTP_PASSWORD))
    stop("SMTP_PASSWORD não foi configurada. Confira o arquivo .Renviron.")

  port <- if(nzchar(SMTP_PORT)) suppressWarnings(as.integer(SMTP_PORT)) else 587L
  if(is.na(port)) port <- 587L

  ssl <- if(nzchar(SMTP_USE_SSL))
    tolower(SMTP_USE_SSL) %in% c("true","1","yes","sim") else FALSE

  # Monta o objeto de credenciais diretamente para não depender do helper
  # creds_envvar() do blastula. Essa versão recebe o valor da senha em memória,
  # então não precisa procurar uma variável de ambiente novamente.
  cred <- list(
    host = if(nzchar(SMTP_HOST)) SMTP_HOST else "smtp.gmail.com",
    port = port,
    use_ssl = ssl,
    user = SMTP_USER,
    password = SMTP_PASSWORD
  )
  class(cred) <- c("creds","blastula_creds")
  cred
}

enviar_email <- function(session,to,subject,body) {
  if(!nzchar(SMTP_FROM))
    return(list(ok=FALSE,msg="SMTP_FROM não foi configurado."))
  base <- get_app_url(session)
  if(!nzchar(base))
    return(list(ok=FALSE,msg="APP_URL não foi configurada."))
  tryCatch({
    email <- blastula::compose_email(body=blastula::md(body))
    blastula::smtp_send(
      email=email,to=to,from=SMTP_FROM,subject=subject,
      credentials=smtp_creds())
    list(ok=TRUE,msg="")
  },error=function(e) {
    msg <- conditionMessage(e)
    if(grepl("login denied|535|not accepted|authentication",msg,ignore.case=TRUE))
      msg <- paste0(msg," — Confira se a senha de app tem 16 caracteres, se foi ",
                    "criada para a conta ",SMTP_USER," e se a verificação em duas ",
                    "etapas está ativa nela.")
    list(ok=FALSE,msg=msg)
  })
}


# ---------------------------------------------------------
# CENTRAL DE NOTIFICAÇÕES
# ---------------------------------------------------------
enviar_email_direto <- function(to,subject,body) {
  if(!nzchar(SMTP_FROM)) return(list(ok=FALSE,msg="SMTP_FROM não foi configurado."))
  if(!nzchar(trimws(as.character(to %||% ""))))
    return(list(ok=FALSE,msg="Destinatário não informado."))
  tryCatch({
    email <- blastula::compose_email(body=blastula::md(body))
    blastula::smtp_send(email=email,to=to,from=SMTP_FROM,subject=subject,
                        credentials=smtp_creds())
    list(ok=TRUE,msg="")
  },error=function(e) list(ok=FALSE,msg=conditionMessage(e)))
}

enfileirar_notificacao_email <- function(barbearia_id,atendimento_id,cliente_id,
                                         destinatario,assunto,mensagem,
                                         criado_por_usuario_id=NULL,
                                         criado_por_perfil=NULL,
                                         tipo="NOVO_AGENDAMENTO") {
  destinatario <- trimws(as.character(destinatario %||% ""))
  if(!validar_email(destinatario)) return(invisible(FALSE))

  # A mensagem entra imediatamente na fila. O processador roda em ciclos de 60 s,
  # portanto, com o processo Shiny ativo, o envio normalmente ocorre em 1–2 minutos.
  dbe("
    INSERT INTO notificacoes(
      barbearia_id,atendimento_id,cliente_id,canal,tipo,destinatario,
      assunto,mensagem,status,programado_em,criado_em,
      criado_por_usuario_id,criado_por_perfil)
    VALUES(?,?,?,'EMAIL',?,?,?,?, 'PENDENTE',?,?,?,?)",
    params=list(barbearia_id,atendimento_id,cliente_id,tipo,destinatario,
                assunto,mensagem,
                format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
                format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
                criado_por_usuario_id,criado_por_perfil))
  invisible(TRUE)
}

montar_email_agendamento <- function(bid,atendimento_id) {
  x <- dbq("
    SELECT a.id,a.data,a.hora,a.status_confirmacao,
           c.nome AS cliente,c.email,
           s.nome AS servico,
           COALESCE(u.nome_usuario,'Não informado') AS barbeiro,
           b.nome AS barbearia
    FROM atendimentos a
    JOIN clientes c ON c.id=a.cliente_id
    JOIN servicos s ON s.id=a.servico_id
    JOIN barbearias b ON b.id=a.barbearia_id
    LEFT JOIN usuarios u ON u.id=a.barbeiro_id
    WHERE a.id=? AND a.barbearia_id=? LIMIT 1",
    params=list(atendimento_id,bid))
  if(nrow(x)==0) return(NULL)

  data_txt <- format(as.Date(x$data[1]),"%d/%m/%Y")
  hora_txt <- substr(as.character(x$hora[1]),1,5)
  conf <- as.character(x$status_confirmacao[1] %||% "")
  conf_txt <- if(identical(conf,"CONFIRMADO")) "Confirmado" else
              if(identical(conf,"RECUSADO")) "Recusado" else "Pendente"

  list(
    to=as.character(x$email[1]),
    subject=paste0("Agendamento - ",x$barbearia[1]),
    body=paste0(
      "Olá, **",x$cliente[1],"**!\n\n",
      "Seu agendamento foi registrado na **",x$barbearia[1],"**.\n\n",
      "- **Data:** ",data_txt,"\n",
      "- **Horário:** ",hora_txt,"\n",
      "- **Serviço:** ",x$servico[1],"\n",
      "- **Barbeiro:** ",x$barbeiro[1],"\n",
      "- **Confirmação:** ",conf_txt,"\n\n",
      "Caso precise alterar o horário, entre em contato com a barbearia."
    )
  )
}

processar_fila_email <- function(limite=20L) {
  pend <- dbq("
    SELECT TOP (?) id
    FROM notificacoes
    WHERE canal='EMAIL' AND status IN ('PENDENTE','ERRO')
      AND programado_em<=?
      AND tentativas<3
    ORDER BY programado_em,id",
    params=list(as.integer(limite),format(Sys.time(),"%Y-%m-%d %H:%M:%S")))
  if(nrow(pend)==0) return(invisible(0L))

  enviados <- 0L
  for(nid in pend$id) {
    # Claim atômico: evita que duas sessões enviem a mesma notificação.
    afet <- dbe("
      UPDATE notificacoes
      SET status='PROCESSANDO',processando_em=?,tentativas=tentativas+1,erro=NULL
      WHERE id=? AND status IN ('PENDENTE','ERRO')",
      params=list(format(Sys.time(),"%Y-%m-%d %H:%M:%S"),nid))
    if(is.null(afet) || afet<1) next

    n <- dbq("SELECT destinatario,assunto,mensagem FROM notificacoes WHERE id=? LIMIT 1",
             params=list(nid))
    if(nrow(n)==0) next

    r <- enviar_email_direto(as.character(n$destinatario[1]),
                             as.character(n$assunto[1]),
                             as.character(n$mensagem[1]))
    if(isTRUE(r$ok)) {
      dbe("UPDATE notificacoes
           SET status='ENVIADA',enviado_em=?,erro=NULL
           WHERE id=?",
          params=list(format(Sys.time(),"%Y-%m-%d %H:%M:%S"),nid))
      enviados <- enviados+1L
    } else {
      dbe("UPDATE notificacoes
           SET status='ERRO',erro=?
           WHERE id=?",
          params=list(substr(as.character(r$msg),1,4000),nid))
    }
  }
  invisible(enviados)
}

# ---------------------------------------------------------
# ASSINATURA
# ---------------------------------------------------------
checar_assinatura <- function(id_barb) {
  a <- dbq("
    SELECT id,data_vencimento,status
    FROM assinaturas
    WHERE barbearia_id=? ORDER BY id DESC LIMIT 1",
    params=list(id_barb))
  if(nrow(a)==0) return(list(ok=FALSE,msg="Assinatura não encontrada."))
  b <- dbq("SELECT status_acesso FROM barbearias WHERE id=? LIMIT 1",
                  params=list(id_barb))
  sa <- b$status_acesso[1]
  st <- a$status[1]
  venc <- as.Date(a$data_vencimento[1])
  if(sa=="CANCELADA"||st=="CANCELADA")
    return(list(ok=FALSE,msg="A assinatura está cancelada."))
  if(sa=="BLOQUEADA"||st=="BLOQUEADA")
    return(list(ok=FALSE,msg="O acesso está bloqueado."))
  if(venc<Sys.Date()) {
    dbe("UPDATE assinaturas SET status='ATRASADA' WHERE id=?",params=list(a$id[1]))
    dbe("UPDATE barbearias SET status_acesso='BLOQUEADA' WHERE id=?",params=list(id_barb))
    return(list(ok=FALSE,msg="A assinatura está em atraso."))
  }
  if(st!="ATIVA") return(list(ok=FALSE,msg="A assinatura não está liberada."))
  list(ok=TRUE,msg="OK")
}

# ---------------------------------------------------------
# TOKEN
# ---------------------------------------------------------
buscar_token <- function(token) {
  th <- hash_token(token)
  x <- dbq("
    SELECT r.id AS token_id,r.usuario_id,r.tipo,u.perfil,u.email
    FROM recuperacao_senha r
    JOIN usuarios u ON u.id=r.usuario_id
    WHERE r.token_hash=? AND r.usado=0 AND r.expira_em>datetime('now')
    ORDER BY r.id DESC LIMIT 1",
    params=list(th))
  if(nrow(x)==0) NULL else x
}

# =========================================================
# UI
# =========================================================

login_ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#ecf0f5}
    .login-card{max-width:420px;margin:8% auto;background:#fff;padding:30px;border-radius:8px;box-shadow:0 2px 12px rgba(0,0,0,.15)}
    .login-title{text-align:center;margin-bottom:25px}
    .small-help{font-size:12px;color:#777}
  "))),
  div(class="login-card",
      h2(class="login-title","💈 Barbearia"),
      p(class="text-center","Acesso à plataforma"),
      textInput("login_usuario","Usuário ou e-mail"),
      passwordInput("login_senha","Senha"),
      actionButton("entrar","Entrar",icon=icon("sign-in"),class="btn-primary btn-block"),
      br(),
      div(style="display:flex;justify-content:space-between;align-items:center;gap:12px;",
          actionLink("abrir_recuperacao","Esqueci minha senha"),
          actionLink("login_criar_conta","Criar conta",icon=icon("user-plus")))
  )
)

setup_ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#ecf0f5}
    .setup-card{max-width:600px;margin:5% auto;background:#fff;padding:30px;border-radius:8px;box-shadow:0 2px 12px rgba(0,0,0,.15)}
  "))),
  div(class="setup-card",
      h2("🛡️ Configuração inicial"),
      p("Crie o seu usuário MASTER. Este acesso terá controle total da plataforma."),
      textInput("setup_nome","Nome do administrador"),
      textInput("setup_usuario","Nome de usuário"),
      textInput("setup_email","E-mail"),
      passwordInput("setup_senha","Senha"),
      passwordInput("setup_confirmar","Confirmar senha"),
      p(class="small-help",
        "Mínimo de 8 caracteres, maiúscula, minúscula, número e caractere especial. Sequências como 1234 não são permitidas."),
      actionButton("criar_master","Criar usuário MASTER",
                   icon=icon("user-shield"),class="btn-success btn-block")
  )
)

troca_senha_ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#ecf0f5}
    .pass-card{max-width:500px;margin:8% auto;background:#fff;padding:30px;border-radius:8px;box-shadow:0 2px 12px rgba(0,0,0,.15)}
  "))),
  div(class="pass-card",
      h2("Troca obrigatória de senha"),
      p("A senha inicial não pode continuar sendo usada."),
      passwordInput("troca_nova","Nova senha"),
      passwordInput("troca_confirmar","Confirmar senha"),
      p(class="small-help","Mínimo de 8 caracteres, com maiúscula, minúscula, número e caractere especial."),
      actionButton("salvar_troca","Salvar nova senha",icon=icon("save"),class="btn-primary btn-block"),
      br(),
      actionButton("sair_troca","Sair",icon=icon("sign-out"),class="btn-default btn-block")
  )
)

reset_ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#ecf0f5}
    .pass-card{max-width:500px;margin:8% auto;background:#fff;padding:30px;border-radius:8px;box-shadow:0 2px 12px rgba(0,0,0,.15)}
  "))),
  div(class="pass-card",
      h2("Redefinir senha"),
      passwordInput("reset_nova","Nova senha"),
      passwordInput("reset_confirmar","Confirmar senha"),
      p(class="small-help","O link expira e só pode ser usado uma vez."),
      actionButton("salvar_reset","Salvar nova senha",icon=icon("key"),class="btn-primary btn-block"),
      br(),
      actionButton("voltar_login_reset","Voltar ao login",class="btn-default btn-block")
  )
)

gestao_ui <- dashboardPage(
  dashboardHeader(title="💈 Barbearia"),
  dashboardSidebar(
    uiOutput("gestao_sidebar")
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      /* =====================================================
         RESPONSIVIDADE GLOBAL - CELULAR / TABLET
         Mantém o conteúdo dentro da largura real da tela e faz
         tabelas largas rolarem horizontalmente dentro do próprio card.
         ===================================================== */
      html, body { max-width:100%; overflow-x:hidden; }
      .wrapper, .content-wrapper, .right-side, .main-footer {
        max-width:100%;
      }
      .content { max-width:100%; overflow-x:hidden; }
      .content .row { margin-left:-7px; margin-right:-7px; }
      .content [class*='col-'] { padding-left:7px; padding-right:7px; }

      /* DataTables não devem aumentar a largura da página inteira. */
      .dataTables_wrapper {
        width:100% !important;
        max-width:100% !important;
        overflow-x:auto;
        -webkit-overflow-scrolling:touch;
      }
      .dataTables_wrapper table.dataTable {
        width:100% !important;
        max-width:none;
      }
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        max-width:100%;
      }

      @media (max-width:767px) {
        /* AdminLTE: ocupa toda a tela quando a barra lateral está recolhida. */
        .content-wrapper, .right-side, .main-footer {
          margin-left:0 !important;
          width:100% !important;
          min-width:0 !important;
        }
        .main-header .logo { width:50px !important; }
        .main-header .navbar { margin-left:50px !important; }

        .content { padding:10px !important; }
        .box, .availability-card, .agenda-hero, .finance-hero,
        .finance-filter, .finance-panel, .clients-hero, .clients-panel,
        .online-panel, .master-hero, .master-panel, .unit-card,
        .services-hero, .services-panel, .exec-panel, .dash-filter-box,
        .client-card, .client-welcome {
          max-width:100% !important;
          min-width:0 !important;
        }

        /* Formulários e ações ficam confortáveis no toque. */
        .availability-actions, .clients-actions, .services-actions {
          display:flex !important;
          flex-direction:column !important;
          align-items:stretch !important;
          gap:8px !important;
        }
        .availability-actions .btn,
        .clients-actions .btn,
        .services-actions .btn {
          width:100% !important;
          margin-left:0 !important;
          margin-right:0 !important;
        }

        /* Tabelas: cabeçalhos/células não estouram a página.
           Quando houver muitas colunas, o usuário desliza somente a tabela. */
        .dataTables_wrapper {
          display:block !important;
          overflow-x:auto !important;
          border-radius:8px;
        }
        table.dataTable {
          min-width:720px;
          font-size:13px;
        }
        table.dataTable th, table.dataTable td {
          white-space:nowrap;
        }

        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate {
          float:none !important;
          text-align:left !important;
          width:100% !important;
          margin:6px 0 !important;
        }
        .dataTables_wrapper .dataTables_filter input {
          max-width:70% !important;
        }

        /* Evita grids com largura mínima de desktop no celular. */
        .block-grid, .finance-expense-grid, .unit-form-grid, .unit-data-grid {
          grid-template-columns:minmax(0,1fr) !important;
        }
        .week-row { min-width:0 !important; }
        .form-control, .selectize-control, .selectize-input {
          max-width:100% !important;
        }
      }

      @media (max-width:480px) {
        .content { padding:8px !important; }
        .agenda-kpis, .finance-kpis, .clients-kpis, .online-kpis,
        .master-kpis, .master-status-grid, .services-kpis,
        .availability-mini, .mini-strip, .booking-steps {
          grid-template-columns:minmax(0,1fr) !important;
        }
        .agenda-hero, .finance-hero, .clients-hero, .services-hero,
        .master-hero, .client-welcome {
          padding:15px !important;
        }
        .availability-body, .finance-panel-body, .clients-panel-body,
        .services-panel-body, .exec-panel-body, .client-card {
          padding:12px !important;
        }
        .dash-title, .agenda-hero h2, .finance-hero h2,
        .clients-hero h2, .services-hero h2, .client-welcome h2 {
          font-size:22px !important;
        }
      }

      /* Disponibilidade: funcionamento, jornada e bloqueios */
      .availability-section { margin-top:18px; }
      .availability-card { background:#fff; border:1px solid #e5e7eb; border-radius:16px;
                           box-shadow:0 4px 16px rgba(15,23,42,.05); margin-bottom:18px; overflow:hidden; }
      .availability-head { padding:18px 20px 14px; border-bottom:1px solid #eef2f7; }
      .availability-title { color:#172033; font-size:18px; font-weight:750; margin:0 0 4px; }
      .availability-sub { color:#64748b; font-size:13px; margin:0; }
      .availability-body { padding:16px 20px 18px; }
      .week-grid-head, .week-row { display:grid; grid-template-columns:1.15fr 1.8fr 1.8fr .85fr;
                                   gap:14px; align-items:center; }
      .week-grid-head { color:#94a3b8; font-size:11px; font-weight:750; text-transform:uppercase;
                        letter-spacing:.05em; padding:0 12px 8px; }
      .week-row { background:#f8fafc; border:1px solid #edf1f5; border-radius:12px;
                  padding:10px 12px 4px; margin-bottom:8px; min-height:62px; }
      .week-row.is-closed { background:#fbfbfc; opacity:.72; }
      .week-day { font-size:14px; font-weight:750; color:#1f2937; }
      .week-status .checkbox { margin:4px 0 7px; }
      .week-status .checkbox label { font-weight:600; color:#475569; }
      .week-row .form-group { margin-bottom:6px; }
      .week-row .control-label { display:none; }
      .week-row .form-control { border-radius:8px; height:36px; border-color:#dbe2ea; background:#fff; }
      .week-note { font-size:11px; color:#64748b; line-height:1.25; }
      .week-badge { display:inline-block; border-radius:999px; padding:5px 9px; font-size:11px;
                    font-weight:700; background:#eef2f7; color:#475569; white-space:nowrap; }
      .week-badge.inherited { background:#eff6ff; color:#2563eb; }
      .availability-actions { display:flex; justify-content:flex-end; padding-top:8px; }
      .availability-actions .btn { border-radius:9px; font-weight:650; padding:9px 16px; }
      .barber-select-wrap { background:#f8fafc; border:1px solid #edf1f5; border-radius:12px;
                            padding:12px 14px 2px; margin-bottom:14px; }
      .block-grid { display:grid; grid-template-columns:minmax(300px,.78fr) minmax(520px,1.22fr); gap:18px; }
      .block-form .form-control { border-radius:9px; min-height:38px; }
      .block-form .btn { border-radius:9px; font-weight:650; }
      .block-table-wrap .dataTables_wrapper { padding-top:0; }
      .availability-mini { display:grid; grid-template-columns:repeat(3,1fr); gap:10px; margin-bottom:14px; }
      .availability-mini-card { border:1px solid #edf1f5; background:#f8fafc; border-radius:11px; padding:11px 13px; }
      .availability-mini-label { color:#64748b; font-size:11px; font-weight:700; text-transform:uppercase; }
      .availability-mini-value { color:#172033; font-size:21px; font-weight:750; margin-top:2px; }
      @media(max-width:900px){
        .week-grid-head{display:none}.week-row{grid-template-columns:1fr 1fr;}
        .block-grid{grid-template-columns:1fr}.availability-mini{grid-template-columns:1fr 1fr 1fr;}
      }
      @media(max-width:600px){.week-row{grid-template-columns:1fr}.availability-mini{grid-template-columns:1fr;}}
      .agenda-modern { padding-bottom:24px; }
      .agenda-hero { background:#fff; border:1px solid #e5e7eb; border-radius:16px;
                     padding:20px 22px; margin:4px 0 18px; box-shadow:0 4px 16px rgba(15,23,42,.05); }
      .agenda-hero h2 { margin:0 0 5px; font-weight:750; color:#172033; }
      .agenda-hero p { margin:0; color:#6b7280; }
      .agenda-kpis { display:grid; grid-template-columns:repeat(4,1fr); gap:12px; margin-bottom:18px; }
      .agenda-kpi { background:#fff; border:1px solid #e5e7eb; border-radius:13px; padding:15px 17px;
                    box-shadow:0 3px 12px rgba(15,23,42,.045); }
      .agenda-kpi .ak-label { color:#6b7280; font-size:12px; font-weight:700; text-transform:uppercase; letter-spacing:.04em; }
      .agenda-kpi .ak-value { color:#172033; font-size:26px; font-weight:750; margin-top:3px; }
      .agenda-modern .box { border-radius:13px; overflow:hidden; border-top:0; box-shadow:0 4px 14px rgba(15,23,42,.06); }
      .agenda-modern .box-header { padding:14px 16px; }
      .agenda-modern .box-title { font-weight:700; }
      .agenda-modern .form-control { border-radius:8px; min-height:38px; }
      .agenda-modern .btn { border-radius:8px; font-weight:600; }
      @media(max-width:767px){ .agenda-kpis{grid-template-columns:repeat(2,1fr);} }
      .finance-exec{padding-bottom:26px}
      .finance-hero{background:#fff;border:1px solid #e5e7eb;border-radius:16px;padding:20px 22px;
                    margin:4px 0 16px;box-shadow:0 4px 16px rgba(15,23,42,.05)}
      .finance-hero h2{margin:0 0 5px;font-weight:750;color:#172033}
      .finance-hero p{margin:0;color:#64748b}
      .finance-filter{background:#fff;border:1px solid #e5e7eb;border-radius:14px;padding:15px 17px 4px;
                      margin-bottom:15px;box-shadow:0 3px 12px rgba(15,23,42,.045)}
      .finance-filter-title{font-size:14px;font-weight:750;color:#334155;margin-bottom:8px}
      .finance-kpis{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin-bottom:15px}
      .finance-kpi{background:#fff;border:1px solid #e5e7eb;border-radius:14px;padding:16px 17px;
                   box-shadow:0 3px 12px rgba(15,23,42,.045);position:relative;min-height:112px}
      .finance-kpi:before{content:'';position:absolute;left:0;top:14px;bottom:14px;width:4px;border-radius:0 4px 4px 0;background:#94a3b8}
      .finance-kpi.revenue:before{background:#16a34a}.finance-kpi.expense:before{background:#dc2626}
      .finance-kpi.profit:before{background:#2563eb}.finance-kpi.forecast:before{background:#64748b}
      .finance-kpi-label{font-size:12px;font-weight:750;color:#64748b;text-transform:uppercase;letter-spacing:.035em}
      .finance-kpi-value{font-size:27px;font-weight:800;color:#172033;margin-top:8px}
      .finance-kpi-note{font-size:12px;color:#94a3b8;margin-top:4px}
      .finance-panel{background:#fff;border:1px solid #e5e7eb;border-radius:14px;margin-bottom:15px;
                     box-shadow:0 3px 12px rgba(15,23,42,.045);overflow:hidden}
      .finance-panel-head{padding:15px 17px 11px;border-bottom:1px solid #eef2f7}
      .finance-panel-title{font-size:16px;font-weight:750;color:#1e293b}
      .finance-panel-sub{font-size:12px;color:#64748b;margin-top:3px}
      .finance-panel-body{padding:15px 17px}
      .finance-expense-grid{display:grid;grid-template-columns:minmax(300px,.72fr) minmax(520px,1.55fr);gap:15px;align-items:start}
      .finance-total{background:#f8fafc;border:1px solid #e5e7eb;border-radius:10px;padding:11px 13px;margin:4px 0 14px}
      .finance-total-label{font-size:12px;color:#64748b;font-weight:700}.finance-total-value{font-size:21px;color:#172033;font-weight:800}
      .finance-exec .form-control{border-radius:8px;min-height:38px}
      .finance-exec .btn{border-radius:8px;font-weight:650}
      .finance-exec table.dataTable thead th{color:#475569;font-weight:700;border-bottom:1px solid #e5e7eb}
      @media(max-width:1050px){.finance-kpis{grid-template-columns:repeat(2,1fr)}.finance-expense-grid{grid-template-columns:1fr}}
      @media(max-width:650px){.finance-kpis{grid-template-columns:1fr}}
      .clients-exec{padding-bottom:26px}
      .clients-hero{background:#fff;border:1px solid #e5e7eb;border-radius:16px;padding:20px 22px;
                    margin:4px 0 16px;box-shadow:0 4px 16px rgba(15,23,42,.05)}
      .clients-hero h2{margin:0 0 5px;font-weight:750;color:#172033}
      .clients-hero p{margin:0;color:#64748b}
      .clients-kpis{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin-bottom:15px}
      .clients-kpi{background:#fff;border:1px solid #e5e7eb;border-radius:14px;padding:15px 17px;
                   box-shadow:0 3px 12px rgba(15,23,42,.045);position:relative}
      .clients-kpi:before{content:'';position:absolute;left:0;top:13px;bottom:13px;width:4px;border-radius:0 4px 4px 0;background:#94a3b8}
      .clients-kpi.active:before{background:#16a34a}.clients-kpi.access:before{background:#2563eb}
      .clients-kpi.pending:before{background:#d97706}.clients-kpi.total:before{background:#64748b}
      .clients-kpi-label{font-size:12px;font-weight:750;color:#64748b;text-transform:uppercase;letter-spacing:.035em}
      .clients-kpi-value{font-size:27px;font-weight:800;color:#172033;margin-top:6px}
      .clients-kpi-note{font-size:12px;color:#94a3b8;margin-top:2px}
      .clients-panel{background:#fff;border:1px solid #e5e7eb;border-radius:14px;
                     box-shadow:0 3px 12px rgba(15,23,42,.045);overflow:hidden}
      .clients-panel-head{padding:15px 17px 11px;border-bottom:1px solid #eef2f7;
                          display:flex;justify-content:space-between;align-items:center;gap:12px}
      .clients-panel-title{font-size:16px;font-weight:750;color:#1e293b}
      .clients-panel-sub{font-size:12px;color:#64748b;margin-top:3px}
      .clients-panel-body{padding:12px 16px 16px}
      .clients-actions{display:flex;gap:9px;flex-wrap:wrap;padding-top:12px;border-top:1px solid #eef2f7;margin-top:8px}
      .clients-exec .btn{border-radius:8px;font-weight:650}
      .clients-exec .dataTables_wrapper{color:#475569}
      .clients-exec table.dataTable thead th{color:#475569;font-weight:700;border-bottom:1px solid #e5e7eb}
      .client-badge{display:inline-block;padding:4px 8px;border-radius:999px;font-size:11px;font-weight:750;white-space:nowrap}
      .client-badge.ok{background:#dcfce7;color:#166534}.client-badge.pending{background:#fef3c7;color:#92400e}
      .client-badge.blocked{background:#fee2e2;color:#991b1b}.client-badge.neutral{background:#f1f5f9;color:#475569}
      @media(max-width:900px){.clients-kpis{grid-template-columns:repeat(2,1fr)}}
      @media(max-width:600px){.clients-kpis{grid-template-columns:1fr}.clients-panel-head{display:block}}
      .online-exec{padding-bottom:26px}
      .online-hero{background:#fff;border:1px solid #e5e7eb;border-radius:16px;padding:20px 22px;margin:4px 0 16px;box-shadow:0 4px 16px rgba(15,23,42,.05)}
      .online-hero h2{margin:0 0 5px;font-weight:750;color:#172033}.online-hero p{margin:0;color:#64748b}
      .online-kpis{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin-bottom:15px}
      .online-kpi{background:#fff;border:1px solid #e5e7eb;border-radius:14px;padding:15px 17px;box-shadow:0 3px 12px rgba(15,23,42,.045);position:relative}
      .online-kpi:before{content:'';position:absolute;left:0;top:13px;bottom:13px;width:4px;border-radius:0 4px 4px 0;background:#64748b}
      .online-kpi.waiting:before{background:#d97706}.online-kpi.converted:before{background:#16a34a}.online-kpi.access:before{background:#2563eb}
      .online-kpi-label{font-size:12px;font-weight:750;color:#64748b;text-transform:uppercase;letter-spacing:.035em}
      .online-kpi-value{font-size:27px;font-weight:800;color:#172033;margin-top:6px}.online-kpi-note{font-size:12px;color:#94a3b8;margin-top:2px}
      .online-panel{background:#fff;border:1px solid #e5e7eb;border-radius:14px;box-shadow:0 3px 12px rgba(15,23,42,.045);overflow:hidden}
      .online-panel-head{padding:15px 17px 11px;border-bottom:1px solid #eef2f7}.online-panel-title{font-size:16px;font-weight:750;color:#1e293b}.online-panel-sub{font-size:12px;color:#64748b;margin-top:3px}.online-panel-body{padding:12px 16px 16px}
      .online-exec table.dataTable thead th{color:#475569;font-weight:700;border-bottom:1px solid #e5e7eb}
      .online-badge{display:inline-block;padding:4px 8px;border-radius:999px;font-size:11px;font-weight:750;white-space:nowrap}.online-badge.ok{background:#dcfce7;color:#166534}.online-badge.pending{background:#fef3c7;color:#92400e}.online-badge.blocked{background:#fee2e2;color:#991b1b}.online-badge.neutral{background:#f1f5f9;color:#475569}
      @media(max-width:900px){.online-kpis{grid-template-columns:repeat(2,1fr)}}@media(max-width:600px){.online-kpis{grid-template-columns:1fr}}
      .master-exec{padding-bottom:26px}
      .master-hero{background:#fff;border:1px solid #e5e7eb;border-radius:16px;padding:20px 22px;margin:4px 0 16px;box-shadow:0 4px 16px rgba(15,23,42,.05)}
      .master-hero h2{margin:0 0 5px;font-weight:750;color:#172033}.master-hero p{margin:0;color:#64748b}
      .master-kpis{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin-bottom:15px}
      .master-kpi{background:#fff;border:1px solid #e5e7eb;border-radius:14px;padding:15px 17px;box-shadow:0 3px 12px rgba(15,23,42,.045);position:relative}
      .master-kpi:before{content:'';position:absolute;left:0;top:13px;bottom:13px;width:4px;border-radius:0 4px 4px 0;background:#64748b}
      .master-kpi.active:before{background:#16a34a}.master-kpi.team:before{background:#2563eb}.master-kpi.revenue:before{background:#7c3aed}
      .master-kpi-label{font-size:12px;font-weight:750;color:#64748b;text-transform:uppercase;letter-spacing:.035em}
      .master-kpi-value{font-size:27px;font-weight:800;color:#172033;margin-top:6px}.master-kpi-note{font-size:12px;color:#94a3b8;margin-top:2px}
      .master-panel{background:#fff;border:1px solid #e5e7eb;border-radius:14px;box-shadow:0 3px 12px rgba(15,23,42,.045);overflow:hidden;margin-bottom:15px}
      .master-panel-head{padding:15px 17px 11px;border-bottom:1px solid #eef2f7}.master-panel-title{font-size:16px;font-weight:750;color:#1e293b}.master-panel-sub{font-size:12px;color:#64748b;margin-top:3px}.master-panel-body{padding:14px 16px 16px}
      .master-actions{display:flex;gap:8px;flex-wrap:wrap;margin-top:10px}.master-actions .btn{border-radius:8px;font-weight:650}
      .master-status-grid{display:grid;grid-template-columns:1.25fr repeat(3,1fr);gap:10px;margin:12px 0}
      .master-status-item{background:#f8fafc;border:1px solid #e5e7eb;border-radius:10px;padding:11px 12px}
      .master-status-label{font-size:11px;color:#64748b;font-weight:750;text-transform:uppercase}.master-status-value{font-size:14px;color:#172033;font-weight:750;margin-top:3px}
      .master-badge{display:inline-block;padding:4px 8px;border-radius:999px;font-size:11px;font-weight:750;white-space:nowrap}
      .master-badge.ok{background:#dcfce7;color:#166534}.master-badge.warn{background:#fef3c7;color:#92400e}.master-badge.bad{background:#fee2e2;color:#991b1b}.master-badge.neutral{background:#f1f5f9;color:#475569}
      .master-exec table.dataTable thead th{color:#475569;font-weight:700;border-bottom:1px solid #e5e7eb}
      @media(max-width:1000px){.master-kpis{grid-template-columns:repeat(2,1fr)}.master-status-grid{grid-template-columns:repeat(2,1fr)}}
      @media(max-width:600px){.master-kpis,.master-status-grid{grid-template-columns:1fr}}
      .unit-form-exec{padding-bottom:26px}
      .unit-form-hero{background:#fff;border:1px solid #e5e7eb;border-radius:16px;padding:20px 22px;margin:4px 0 16px;box-shadow:0 4px 16px rgba(15,23,42,.05)}
      .unit-form-hero h2{margin:0 0 5px;font-weight:750;color:#172033}.unit-form-hero p{margin:0;color:#64748b}
      .unit-form-grid{display:grid;grid-template-columns:minmax(360px,.82fr) minmax(520px,1.18fr);gap:15px;align-items:start}
      .unit-data-grid{display:grid;grid-template-columns:minmax(520px,1.55fr) minmax(300px,.75fr);gap:15px;align-items:start}
      .unit-card{background:#fff;border:1px solid #e5e7eb;border-radius:14px;box-shadow:0 3px 12px rgba(15,23,42,.045);overflow:hidden;margin-bottom:15px}
      .unit-card-head{padding:15px 17px 11px;border-bottom:1px solid #eef2f7}.unit-card-title{font-size:16px;font-weight:750;color:#1e293b}.unit-card-sub{font-size:12px;color:#64748b;margin-top:3px}.unit-card-body{padding:15px 17px}
      .unit-form-exec .form-control{border-radius:8px;min-height:38px}.unit-form-exec .btn{border-radius:8px;font-weight:650}
      .unit-fields-2{display:grid;grid-template-columns:1fr 1fr;gap:0 14px}.unit-fields-3{display:grid;grid-template-columns:1.35fr .55fr 1fr;gap:0 14px}
      .unit-note{background:#f8fafc;border:1px solid #e5e7eb;border-radius:10px;padding:11px 13px;color:#64748b;font-size:12px;margin:5px 0 14px}
      .unit-info-item{padding:11px 0;border-bottom:1px solid #eef2f7}.unit-info-item:last-child{border-bottom:0}.unit-info-label{font-size:11px;color:#64748b;font-weight:750;text-transform:uppercase}.unit-info-value{font-size:15px;color:#172033;font-weight:750;margin-top:3px;word-break:break-word}
      .unit-public-link{background:#f8fafc;border:1px solid #e5e7eb;border-radius:10px;padding:12px;margin-top:12px;word-break:break-all}
      @media(max-width:1050px){.unit-form-grid,.unit-data-grid{grid-template-columns:1fr}}
      @media(max-width:650px){.unit-fields-2,.unit-fields-3{grid-template-columns:1fr}}
      .services-exec{padding-bottom:26px}
      .services-hero{background:#fff;border:1px solid #e5e7eb;border-radius:16px;padding:20px 22px;margin:4px 0 16px;box-shadow:0 4px 16px rgba(15,23,42,.05)}
      .services-hero h2{margin:0 0 5px;font-weight:750;color:#172033}.services-hero p{margin:0;color:#64748b}
      .services-kpis{display:grid;grid-template-columns:repeat(3,1fr);gap:12px;margin-bottom:15px}
      .services-kpi{background:#fff;border:1px solid #e5e7eb;border-radius:14px;padding:15px 17px;box-shadow:0 3px 12px rgba(15,23,42,.045);position:relative}
      .services-kpi:before{content:'';position:absolute;left:0;top:13px;bottom:13px;width:4px;border-radius:0 4px 4px 0;background:#64748b}
      .services-kpi.active:before{background:#16a34a}.services-kpi.ticket:before{background:#2563eb}
      .services-kpi-label{font-size:12px;font-weight:750;color:#64748b;text-transform:uppercase;letter-spacing:.035em}
      .services-kpi-value{font-size:27px;font-weight:800;color:#172033;margin-top:6px}.services-kpi-note{font-size:12px;color:#94a3b8;margin-top:2px}
      .services-panel{background:#fff;border:1px solid #e5e7eb;border-radius:14px;box-shadow:0 3px 12px rgba(15,23,42,.045);overflow:hidden}
      .services-panel-head{padding:15px 17px 11px;border-bottom:1px solid #eef2f7;display:flex;justify-content:space-between;align-items:center;gap:12px}
      .services-panel-title{font-size:16px;font-weight:750;color:#1e293b}.services-panel-sub{font-size:12px;color:#64748b;margin-top:3px}.services-panel-body{padding:13px 16px 16px}
      .services-actions{display:flex;gap:8px;flex-wrap:wrap;padding-top:12px;border-top:1px solid #eef2f7;margin-top:9px}.services-actions .btn{border-radius:8px;font-weight:650}
      .service-badge{display:inline-block;padding:4px 8px;border-radius:999px;font-size:11px;font-weight:750}.service-badge.ok{background:#dcfce7;color:#166534}.service-badge.off{background:#f1f5f9;color:#64748b}
      @media(max-width:700px){.services-kpis{grid-template-columns:1fr}.services-panel-head{display:block}}
      .dash-exec { padding-bottom: 24px; }
      .dash-exec .content-header { padding-bottom: 4px; }
      .dash-title { font-size: 30px; font-weight: 700; color: #1f2937; margin: 2px 0 2px 0; }
      .dash-subtitle { color: #6b7280; margin-bottom: 18px; }
      .dash-filter-box { background: #fff; border: 1px solid #e5e7eb; border-radius: 12px;
                         padding: 14px 16px 4px 16px; margin-bottom: 18px;
                         box-shadow: 0 2px 8px rgba(15,23,42,.05); }
      .exec-kpi { background:#fff; border:1px solid #e7ebf0; border-radius:14px;
                  min-height:126px; padding:18px 18px 14px; margin-bottom:16px;
                  box-shadow:0 4px 14px rgba(15,23,42,.06); position:relative; overflow:hidden; }
      .exec-kpi:before { content:''; position:absolute; left:0; top:0; bottom:0; width:5px; background:#2f80ed; }
      .exec-kpi.kpi-green:before { background:#16a34a; }
      .exec-kpi.kpi-amber:before { background:#f59e0b; }
      .exec-kpi.kpi-purple:before { background:#7c3aed; }
      .exec-kpi-label { font-size:12px; letter-spacing:.06em; text-transform:uppercase;
                        font-weight:700; color:#6b7280; margin-bottom:7px; }
      .exec-kpi-value { font-size:29px; line-height:1.1; font-weight:750; color:#111827; }
      .exec-kpi-foot { font-size:12px; margin-top:10px; color:#6b7280; }
      .exec-kpi-foot .up { color:#15803d; font-weight:700; }
      .exec-kpi-foot .down { color:#b91c1c; font-weight:700; }
      .exec-kpi-foot .flat { color:#6b7280; font-weight:700; }
      .mini-strip { display:grid; grid-template-columns:repeat(4,1fr); gap:12px; margin:0 0 18px 0; }
      .mini-metric { background:#fff; border:1px solid #e5e7eb; border-radius:11px;
                     padding:13px 15px; box-shadow:0 2px 8px rgba(15,23,42,.04); }
      .mini-label { color:#6b7280; font-size:12px; font-weight:600; }
      .mini-value { color:#111827; font-size:21px; font-weight:750; margin-top:3px; }
      .exec-panel { background:#fff; border:1px solid #e5e7eb; border-radius:13px;
                    margin-bottom:18px; box-shadow:0 3px 12px rgba(15,23,42,.05); overflow:hidden; }
      .exec-panel-head { padding:15px 18px 12px; border-bottom:1px solid #eef0f3; }
      .exec-panel-title { font-size:16px; font-weight:700; color:#1f2937; }
      .exec-panel-sub { font-size:12px; color:#8a94a3; margin-top:2px; }
      .exec-panel-body { padding:12px 16px 16px; min-height:280px; }
      .exec-panel-body.compact { min-height:235px; }
      .rank-row { padding:11px 2px; border-bottom:1px solid #f0f2f5; }
      .rank-row:last-child { border-bottom:0; }
      .rank-top { display:flex; justify-content:space-between; gap:12px; align-items:center; }
      .rank-name { color:#253044; font-weight:650; }
      .rank-value { color:#111827; font-weight:700; white-space:nowrap; }
      .rank-sub { color:#7b8492; font-size:12px; margin-top:2px; }
      .rank-bar { height:6px; background:#edf1f5; border-radius:999px; overflow:hidden; margin-top:8px; }
      .rank-fill { height:100%; background:#3b82f6; border-radius:999px; }
      .empty-exec { color:#8a94a3; text-align:center; padding:70px 12px; }
      .dash-period-label { color:#6b7280; font-size:12px; margin-top:3px; }
      @media(max-width: 991px){ .mini-strip{grid-template-columns:repeat(2,1fr);} }
      @media(max-width: 600px){ .mini-strip{grid-template-columns:1fr;} .exec-kpi-value{font-size:25px;} }
    "))),
    tabItems(
      # ---------------------------------------------------
      # ADMINISTRAÇÃO MASTER - LISTA
      # ---------------------------------------------------
      tabItem("m_lista",
        div(class="master-exec",
          div(class="master-hero",
              h2(icon("building")," Gestão de Unidades"),
              p("Visão administrativa das barbearias, assinaturas, acessos e equipes cadastradas na plataforma.")),

          uiOutput("master_kpis"),

          div(class="master-panel",
              div(class="master-panel-head",
                  div(class="master-panel-title","Unidades cadastradas"),
                  div(class="master-panel-sub","Acompanhe situação da conta, assinatura, mensalidade, equipe e endereço público de cada unidade.")),
              div(class="master-panel-body",
                  DTOutput("master_tabela"))
          ),

          div(class="master-panel",
              div(class="master-panel-head",
                  div(class="master-panel-title",icon("sliders-h")," Administração da unidade"),
                  div(class="master-panel-sub","Selecione a barbearia que deseja administrar. As ações abaixo mantêm as regras atuais de assinatura e acesso.")),
              div(class="master-panel-body",
                  selectInput("master_sel_lista","Unidade selecionada",choices=character(0)),
                  uiOutput("master_unidade_resumo"),
                  div(class="master-actions",
                      actionButton("master_liberar","Liberar acesso",icon=icon("unlock"),class="btn-success"),
                      actionButton("master_bloquear","Bloquear acesso",icon=icon("lock"),class="btn-warning"),
                      actionButton("master_cancelar","Cancelar assinatura",icon=icon("ban"),class="btn-danger"),
                      actionButton("master_renovar","Renovar por 30 dias",icon=icon("refresh"),class="btn-primary")),
                  div(class="master-actions",
                      actionButton("master_editar","Editar dados da unidade",icon=icon("edit"),class="btn-default"),
                      actionButton("master_dados","Visualizar dados completos",icon=icon("building"),class="btn-default"),
                      actionButton("master_excluir_barbearia","Excluir barbearia",icon=icon("trash"),class="btn-danger"))
              )
          ),

          div(class="master-panel",
              div(class="master-panel-head",
                  div(class="master-panel-title",icon("users")," Equipe da unidade"),
                  div(class="master-panel-sub","Gerencie as contas de barbeiro vinculadas à barbearia selecionada.")),
              div(class="master-panel-body",
                  uiOutput("master_equipe_resumo"),
                  DTOutput("master_tabela_barbeiros"),
                  br(),
                  selectInput("master_barbeiro_sel","Profissional selecionado",choices=character(0)),
                  div(class="master-actions",
                      actionButton("master_criar_barbeiro","Adicionar barbeiro",icon=icon("user-plus"),class="btn-primary"),
                      actionButton("master_senha","Redefinir senha",icon=icon("key"),class="btn-default"),
                      actionButton("master_ativar_barbeiro","Ativar login",icon=icon("unlock"),class="btn-success"),
                      actionButton("master_desativar_barbeiro","Desativar login",icon=icon("lock"),class="btn-warning"))
              )
          )
        )
      ),

      # ---------------------------------------------------
      # ADMINISTRAÇÃO MASTER - CADASTRO
      # ---------------------------------------------------
      tabItem("m_cadastro",
        div(class="unit-form-exec",
          div(class="unit-form-hero",
              h2(icon("plus-circle")," Cadastrar nova unidade"),
              p("Crie a barbearia, defina o acesso inicial e registre os dados administrativos em uma única etapa.")),
          div(class="unit-form-grid",
            div(class="unit-card",
                div(class="unit-card-head",
                    div(class="unit-card-title",icon("user")," Conta de acesso inicial"),
                    div(class="unit-card-sub","Credenciais do primeiro barbeiro responsável pela nova unidade.")),
                div(class="unit-card-body",
                    textInput("cad_usuario","Nome de usuário *"),
                    textInput("cad_email","E-mail *"),
                    passwordInput("cad_senha","Senha inicial *"),
                    passwordInput("cad_confirmar","Confirmar senha *"),
                    div(class="unit-note",
                        icon("info-circle")," A senha inicial deverá ser alterada no primeiro acesso. O link público será gerado automaticamente a partir do nome da barbearia.")
                )
            ),
            div(class="unit-card",
                div(class="unit-card-head",
                    div(class="unit-card-title",icon("building")," Dados da unidade"),
                    div(class="unit-card-sub","Informações cadastrais, endereço e configuração comercial.")),
                div(class="unit-card-body",
                    div(class="unit-fields-2",
                        textInput("cad_nome","Nome da barbearia *"),
                        textInput("cad_telefone","Telefone *")),
                    div(class="unit-fields-2",
                        textInput("cad_responsavel","Nome do responsável"),
                        textInput("cad_cpf_cnpj","CPF/CNPJ",placeholder="CPF: 333.333.333-90 | CNPJ: 33.333.333/3333-90")),
                    div(class="unit-fields-3",
                        textInput("cad_cep","CEP"),
                        textInput("cad_numero","Número"),
                        textInput("cad_bairro","Bairro")),
                    textInput("cad_endereco","Endereço"),
                    div(class="unit-fields-2",
                        textInput("cad_cidade","Cidade"),
                        textInput("cad_complemento","Complemento")),
                    div(class="unit-fields-2",
                        numericInput("cad_valor","Mensalidade",0,min=0,step=.01),
                        div(style="padding-top:25px;",checkboxInput("cad_liberar","Liberar acesso imediatamente",TRUE))),
                    actionButton("cad_criar","Cadastrar unidade",icon=icon("save"),class="btn-primary btn-block")
                )
            )
          )
        )
      ),

      # ---------------------------------------------------
      # GESTÃO DA BARBEARIA SELECIONADA
      # Os mesmos recursos do painel do barbeiro ficam
      # disponíveis ao MASTER para a barbearia escolhida.
      # ---------------------------------------------------
      tabItem("dashboard",
        div(class="dash-exec",
          div(class="dash-title","Dashboard Executivo"),
          div(class="dash-subtitle","Visão consolidada de desempenho, clientes e agenda da barbearia."),

          div(class="dash-filter-box",
            fluidRow(
              column(5,dateRangeInput("dash_periodo","Período",
                       start=as.Date(format(Sys.Date(),"%Y-%m-01")),
                       end=Sys.Date(),format="dd/mm/yyyy",
                       separator=" até ",language="pt-BR")),
              column(5,selectInput("dash_barbeiro","Barbeiro",
                       choices=c("Todos os barbeiros"="TODOS"))),
              column(2,br(),actionButton("dash_limpar","Mês atual",
                       icon=icon("refresh"),class="btn-default btn-block"))
            )
          ),

          uiOutput("dash_cards"),
          uiOutput("dash_mini_indicadores"),

          fluidRow(
            column(8,
              div(class="exec-panel",
                div(class="exec-panel-head",
                    div(class="exec-panel-title","Evolução do faturamento"),
                    div(class="exec-panel-sub","Receita de atendimentos concluídos no período")),
                div(class="exec-panel-body",
                    plotOutput("dash_grafico_faturamento",height="285px"))
              )
            ),
            column(4,
              div(class="exec-panel",
                div(class="exec-panel-head",
                    div(class="exec-panel-title","Status dos atendimentos"),
                    div(class="exec-panel-sub","Distribuição da agenda no período")),
                div(class="exec-panel-body",
                    plotOutput("dash_grafico_status",height="285px"))
              )
            )
          ),

          fluidRow(
            column(7,
              div(class="exec-panel",
                div(class="exec-panel-head",
                    div(class="exec-panel-title","Produção por barbeiro"),
                    div(class="exec-panel-sub","Faturamento, atendimentos e participação na receita")),
                div(class="exec-panel-body compact",
                    plotOutput("dash_grafico_barbeiros",height="255px"))
              )
            ),
            column(5,
              div(class="exec-panel",
                div(class="exec-panel-head",
                    div(class="exec-panel-title","Top serviços"),
                    div(class="exec-panel-sub","Serviços concluídos com maior participação")),
                div(class="exec-panel-body compact",
                    uiOutput("dash_servicos_rank"))
              )
            )
          ),

          fluidRow(
            column(6,
              div(class="exec-panel",
                div(class="exec-panel-head",
                    div(class="exec-panel-title","Dias com maior movimento"),
                    div(class="exec-panel-sub","Datas com maior volume de atendimentos")),
                div(class="exec-panel-body compact",
                    uiOutput("dash_dias_rank"))
              )
            ),
            column(6,
              div(class="exec-panel",
                div(class="exec-panel-head",
                    div(class="exec-panel-title","Horários de pico"),
                    div(class="exec-panel-sub","Faixas de horário mais procuradas")),
                div(class="exec-panel-body compact",
                    plotOutput("dash_grafico_horarios",height="235px"))
              )
            )
          )
        )
      ),

      tabItem("clientes",
        div(class="clients-exec",
          div(class="clients-hero",
              h2(icon("users")," Gestão de Clientes"),
              p("Consulte cadastros, acompanhe o último atendimento e gerencie o acesso dos clientes à plataforma.")),
          uiOutput("clientes_kpis"),
          div(class="clients-panel",
              div(class="clients-panel-head",
                  div(
                    div(class="clients-panel-title","Base de clientes"),
                    div(class="clients-panel-sub","Status do cliente, último atendimento e acesso à plataforma em uma única visão.")
                  ),
                  div(style="font-size:12px;color:#64748b;",
                      icon("info-circle")," Selecione uma linha para editar ou gerenciar o acesso")
              ),
              div(class="clients-panel-body",
                  DTOutput("tabela_clientes"),
                  div(class="clients-actions",
                      actionButton("editar_cliente","Editar cliente",icon=icon("edit"),class="btn-primary"),
                      actionButton("acesso_cliente","Criar / Reenviar acesso",icon=icon("envelope"),class="btn-default"))
              )
          )
        )
      ),

      tabItem("notificacoes",
        div(class="dash-exec",
          div(class="dash-title","Central de Notificações"),
          div(class="dash-subtitle","Acompanhe confirmações, fila de e-mails, envios e falhas. O canal WhatsApp permanece preparado para integração futura."),

          fluidRow(
            column(4,selectInput("notif_status","Status",
                     choices=c("Todos"="TODOS","Pendentes"="PENDENTE",
                               "Enviadas"="ENVIADA","Com erro"="ERRO",
                               "Processando"="PROCESSANDO"))),
            column(4,selectInput("notif_canal","Canal",
                     choices=c("Todos"="TODOS","E-mail"="EMAIL","WhatsApp (futuro)"="WHATSAPP"))),
            column(4,br(),
                   actionButton("notif_processar","Processar fila agora",icon=icon("send"),class="btn-primary"),
                   actionButton("notif_reenviar","Reenviar selecionada",icon=icon("refresh"),class="btn-default"))
          ),
          uiOutput("notif_cards"),
          div(class="exec-panel",
            div(class="exec-panel-head",
                div(class="exec-panel-title","Histórico de notificações"),
                div(class="exec-panel-sub","Os e-mails entram na fila imediatamente. Use “Processar fila agora” nesta fase local; na publicação, o processador será executado por um serviço independente.")),
            div(class="exec-panel-body",DTOutput("notif_tabela"))
          )
        )
      ),

      tabItem("crm",
        div(class="dash-exec",
          div(class="dash-title","CRM de Clientes"),
          div(class="dash-subtitle","Relacionamento, recorrência e histórico de consumo dos clientes."),

          div(class="dash-filter-box",
            fluidRow(
              column(4,selectInput("crm_segmento","Segmento",
                       choices=c("Todos"="TODOS","Ativos"="ATIVO","Novos"="NOVO",
                                 "Recorrentes"="RECORRENTE","Sem retornar"="SEM_RETORNAR",
                                 "Com faltas"="COM_FALTAS"))),
              column(5,textInput("crm_busca","Pesquisar cliente",
                       placeholder="Nome, telefone ou e-mail")),
              column(3,br(),actionButton("crm_atualizar","Atualizar",
                       icon=icon("refresh"),class="btn-default btn-block"))
            )
          ),

          uiOutput("crm_cards"),

          fluidRow(
            column(12,
              div(class="exec-panel",
                div(class="exec-panel-head",
                    div(class="exec-panel-title","Base de clientes"),
                    div(class="exec-panel-sub","Selecione um cliente para abrir a ficha 360°")),
                div(class="exec-panel-body",
                    DTOutput("crm_tabela"))
              )
            )
          ),

          fluidRow(
            column(12,
              div(class="exec-panel",
                div(class="exec-panel-head",
                    div(class="exec-panel-title","Ficha 360° do cliente"),
                    div(class="exec-panel-sub","Indicadores e histórico completo do cliente selecionado")),
                div(class="exec-panel-body",
                    uiOutput("crm_ficha"),
                    DTOutput("crm_historico"))
              )
            )
          )
        )
      ),

      tabItem("cadastros_clientes",
        div(class="online-exec",
          div(class="online-hero",
              h2(icon("user-plus")," Clientes Online"),
              p("Acompanhe os clientes que criaram uma conta pela página pública da barbearia.")),
          uiOutput("cadastros_online_kpis"),
          div(class="online-panel",
              div(class="online-panel-head",
                  div(class="online-panel-title","Cadastros pela página pública"),
                  div(class="online-panel-sub","O cadastro online só entra na base operacional de Clientes após o primeiro agendamento nesta barbearia.")),
              div(class="online-panel-body",DTOutput("tabela_cadastros_clientes"))
          )
        )
      ),

      tabItem("servicos",
        div(class="services-exec",
          div(class="services-hero",
              h2(icon("cut")," Gestão de Serviços"),
              p("Cadastre e mantenha o catálogo de serviços oferecidos pela barbearia. Os serviços ativos ficam disponíveis para novos agendamentos.")),
          uiOutput("servicos_kpis"),
          div(class="services-panel",
              div(class="services-panel-head",
                  div(
                    div(class="services-panel-title","Catálogo de serviços"),
                    div(class="services-panel-sub","Defina nome, preço, duração e disponibilidade de cada serviço.")
                  ),
                  actionButton("servico_novo","Novo serviço",icon=icon("plus"),class="btn-primary")
              ),
              div(class="services-panel-body",
                  DTOutput("tabela_servicos"),
                  div(class="services-actions",
                      actionButton("servico_editar","Editar serviço selecionado",icon=icon("edit"),class="btn-default"))
              )
          )
        )
      ),

      tabItem("agenda",
        div(class="agenda-modern",
          div(class="agenda-hero",
              h2(icon("calendar-alt")," Agenda"),
              p("Organize os atendimentos do dia, acompanhe os status e gerencie a disponibilidade da equipe.")),
          uiOutput("agenda_kpis"),
          fluidRow(
            box(title=tagList(icon("calendar-plus")," Novo atendimento"),width=5,status="primary",solidHeader=TRUE,
                h4(style="font-weight:700;margin-top:2px;","Dados do cliente"),
                textInput("agenda_nome","Nome do cliente *"),
                fluidRow(
                  column(6,textInput("agenda_telefone","Telefone *",placeholder="(14) 99999-9999")),
                  column(6,textInput("agenda_email","E-mail *",placeholder="cliente@email.com"))
                ),
                h4(style="font-weight:700;margin-top:14px;","Agendamento"),
                selectInput("agenda_servico","Serviço *",choices=character(0)),
                selectInput("agenda_barbeiro","Barbeiro *",choices=character(0)),
                div(style="background:#f8fafc;border:1px solid #e5e7eb;border-radius:9px;padding:10px 12px;margin-bottom:12px;",
                    strong("Preço: "),textOutput("agenda_preco",inline=TRUE),
                    span(style="margin:0 10px;color:#cbd5e1;","|"),
                    strong("Duração: "),textOutput("agenda_duracao",inline=TRUE)),
                fluidRow(
                  column(6,dateInput("agenda_data","Data *",value=Sys.Date(),format="dd/mm/yyyy",language="pt-BR")),
                  column(6,timeInput("agenda_hora","Horário *",value="10:00"))
                ),
                selectInput("agenda_confirmacao","Confirmação do cliente",
                            choices=c("Pendente"="PENDENTE","Confirmado"="CONFIRMADO","Recusado"="RECUSADO"),
                            selected="PENDENTE"),
                textAreaInput("agenda_obs","Observação",rows=2),
                actionButton("salvar_agendamento","Salvar atendimento",
                             icon=icon("check"),class="btn-primary btn-block")),
            box(title=tagList(icon("list")," Atendimentos"),width=7,status="primary",solidHeader=TRUE,
                selectInput("agenda_filtro_barbeiro","Exibir agenda de",choices=c("Todos os barbeiros"="TODOS")),
                DTOutput("tabela_agenda"))
          ),
        div(class="availability-section",
          div(class="availability-card",
            div(class="availability-head",
                div(class="availability-title",icon("store")," Horário de funcionamento"),
                div(class="availability-sub","Defina a janela semanal da barbearia. Fora desses períodos nenhum cliente poderá agendar.")),
            div(class="availability-body",
                uiOutput("ui_horarios_funcionamento"),
                div(class="availability-actions",
                    actionButton("salvar_horarios_funcionamento","Salvar funcionamento",
                                 icon=icon("save"),class="btn-primary")))
          ),
          div(class="availability-card",
            div(class="availability-head",
                div(class="availability-title",icon("user-clock")," Jornada individual dos barbeiros"),
                div(class="availability-sub","A jornada individual respeita os limites de funcionamento da barbearia e define quando cada profissional pode receber agendamentos.")),
            div(class="availability-body",
                div(class="barber-select-wrap",
                    selectInput("jornada_barbeiro","Barbeiro",choices=character(0))),
                uiOutput("ui_jornada_resumo"),
                uiOutput("ui_jornada_barbeiro"),
                div(class="availability-actions",
                    actionButton("salvar_jornada_barbeiro","Salvar jornada",
                                 icon=icon("save"),class="btn-primary")))
          ),
          div(class="block-grid",
            div(class="availability-card block-form",
              div(class="availability-head",
                  div(class="availability-title",icon("ban")," Novo bloqueio"),
                  div(class="availability-sub","Reserve um período para almoço, compromisso, manutenção ou indisponibilidade.")),
              div(class="availability-body",
                  selectInput("bloqueio_alvo","Aplicar bloqueio a",choices=c("Toda a barbearia"="TODOS")),
                  dateInput("bloqueio_data","Data *",value=Sys.Date(),format="dd/mm/yyyy",language="pt-BR"),
                  fluidRow(
                    column(6,timeInput("bloqueio_inicio","Das *",value="12:00")),
                    column(6,timeInput("bloqueio_fim","Até *",value="13:00"))
                  ),
                  textInput("bloqueio_motivo","Motivo",placeholder="Ex.: almoço, compromisso, manutenção"),
                  actionButton("salvar_bloqueio","Criar bloqueio",icon=icon("ban"),class="btn-primary btn-block"))
            ),
            div(class="availability-card block-table-wrap",
              div(class="availability-head",
                  div(class="availability-title",icon("calendar-times")," Bloqueios cadastrados"),
                  div(class="availability-sub","Consulte os períodos indisponíveis e remova um bloqueio quando necessário.")),
              div(class="availability-body",
                  uiOutput("bloqueios_kpis"),
                  DTOutput("tabela_bloqueios"),
                  div(class="availability-actions",
                      actionButton("remover_bloqueio","Remover selecionado",icon=icon("trash"),class="btn-danger")))
            )
          )
        )
        )
      ),

      tabItem("financeiro",
        div(class="finance-exec",
          div(class="finance-hero",
              h2(icon("chart-line")," Visão Financeira"),
              p("Acompanhe faturamento, despesas, resultado operacional e projeções da barbearia.")),

          div(class="finance-filter",
              div(class="finance-filter-title",icon("filter")," Filtros da análise"),
              fluidRow(
                column(5,
                  dateRangeInput("fin_periodo","Período de análise",
                    start=as.Date(format(Sys.Date(),"%Y-%m-01")),
                    end=Sys.Date(),format="dd/mm/yyyy",language="pt-BR",separator=" até ")
                ),
                column(5,
                  selectInput("fin_barbeiro","Visão por profissional",
                    choices=c("Todos os barbeiros"="TODOS"))
                ),
                column(2,br(),
                  actionButton("fin_limpar_filtros","Mês atual",
                    icon=icon("refresh"),class="btn-default btn-block")
                )
              )
          ),

          uiOutput("fin_cards"),

          div(class="finance-panel",
              div(class="finance-panel-head",
                  div(class="finance-panel-title",icon("line-chart")," ",textOutput("fin_titulo_projecao",inline=TRUE)),
                  div(class="finance-panel-sub","Estimativa baseada no desempenho observado no período selecionado.")),
              div(class="finance-panel-body",
                  p(style="margin:0;color:#475569;",textOutput("fin_projecao_info")))
          ),

          div(class="finance-panel",
              div(class="finance-panel-head",
                  div(class="finance-panel-title",icon("exchange")," ",textOutput("fin_titulo_comparativo",inline=TRUE)),
                  div(class="finance-panel-sub",textOutput("fin_comparativo_periodo"))),
              div(class="finance-panel-body",DTOutput("fin_comparativo"))
          ),

          div(class="finance-expense-grid",
            div(class="finance-panel",
                div(class="finance-panel-head",
                    div(class="finance-panel-title",icon("plus-circle")," Registrar despesa"),
                    div(class="finance-panel-sub","Inclua os custos operacionais da barbearia.")),
                div(class="finance-panel-body",
                    dateInput("gasto_data","Data *",value=Sys.Date(),format="dd/mm/yyyy",language="pt-BR"),
                    textInput("gasto_desc","Descrição *",placeholder="Ex.: compra de materiais"),
                    selectInput("gasto_categoria","Categoria *",
                                c("Materiais","Produtos","Contas","Equipamentos","Aluguel","Outros")),
                    fluidRow(
                      column(6,numericInput("gasto_qtd","Quantidade *",value=1,min=.01,step=1)),
                      column(6,numericInput("gasto_unit","Valor unitário *",value=0,min=0,step=.01))
                    ),
                    div(class="finance-total",
                        div(class="finance-total-label","VALOR TOTAL DA DESPESA"),
                        div(class="finance-total-value",textOutput("gasto_total",inline=TRUE))),
                    actionButton("salvar_gasto","Registrar despesa",icon=icon("plus"),class="btn-primary btn-block"))
            ),
            div(class="finance-panel",
                div(class="finance-panel-head",
                    div(class="finance-panel-title",icon("file-invoice-dollar")," Histórico de despesas"),
                    div(class="finance-panel-sub","Consulte os gastos registrados no período selecionado.")),
                div(class="finance-panel-body",DTOutput("tabela_gastos"))
            )
          )
        )
      ),

      tabItem("dados",
        div(class="unit-form-exec",
          div(class="unit-form-hero",
              h2(icon("building")," Dados da barbearia"),
              p("Mantenha as informações cadastrais da unidade atualizadas e consulte os dados da assinatura e do endereço público.")),
          div(class="unit-data-grid",
            div(class="unit-card",
                div(class="unit-card-head",
                    div(class="unit-card-title","Informações da unidade"),
                    div(class="unit-card-sub","Dados cadastrais, responsável e endereço da barbearia.")),
                div(class="unit-card-body",
                    div(class="unit-fields-2",
                        textInput("p_nome","Nome da barbearia"),
                        textInput("p_telefone","Telefone")),
                    div(class="unit-fields-2",
                        textInput("p_responsavel","Nome do responsável"),
                        textInput("p_cpf_cnpj","CPF/CNPJ",placeholder="CPF: 333.333.333-90 | CNPJ: 33.333.333/3333-90")),
                    div(class="unit-fields-3",
                        textInput("p_cep","CEP"),
                        textInput("p_numero","Número"),
                        textInput("p_bairro","Bairro")),
                    textInput("p_endereco","Endereço"),
                    div(class="unit-fields-2",
                        textInput("p_cidade","Cidade"),
                        textInput("p_complemento","Complemento")),
                    actionButton("salvar_dados","Salvar alterações",icon=icon("save"),class="btn-primary")
                )
            ),
            div(class="unit-card",
                div(class="unit-card-head",
                    div(class="unit-card-title",icon("credit-card")," Assinatura e acesso público"),
                    div(class="unit-card-sub","Resumo comercial e endereço da página pública da unidade.")),
                div(class="unit-card-body",
                    div(class="unit-info-item",
                        div(class="unit-info-label","Assinatura"),
                        div(class="unit-info-value",textOutput("p_status",inline=TRUE))),
                    div(class="unit-info-item",
                        div(class="unit-info-label","Próximo vencimento"),
                        div(class="unit-info-value",textOutput("p_vencimento",inline=TRUE))),
                    div(class="unit-public-link",
                        div(class="unit-info-label","Link público da barbearia"),
                        div(style="font-size:12px;color:#64748b;margin:4px 0 8px;","Use este endereço para direcionar clientes à página pública da unidade."),
                        div(class="unit-info-value",textOutput("p_link_publico",inline=TRUE)))
                )
            )
          )
        )
      )
    )
  )
)

public_ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#ecf0f5}
    .public-card{max-width:760px;margin:6% auto;background:#fff;padding:36px;border-radius:10px;box-shadow:0 2px 14px rgba(0,0,0,.15)}
    .public-title{text-align:center;margin-bottom:8px}
    .public-subtitle{text-align:center;color:#777;margin-bottom:28px}
    .public-section{margin-top:22px;padding-top:18px;border-top:1px solid #eee}
    .public-slug{font-family:monospace;background:#f5f5f5;padding:6px 9px;border-radius:4px}
  "))),
  div(class="public-card",
      h1(class="public-title","💈 ",textOutput("public_nome",inline=TRUE)),
      p(class="public-subtitle","Página pública da barbearia"),
      uiOutput("public_status"),
      div(class="public-section",
          h4("Informações da barbearia"),
          p(textOutput("public_telefone")),
          p(textOutput("public_endereco")),
          p(textOutput("public_responsavel"))),
      div(class="public-section",
          h4("Acesso do cliente"),
          p("Já possui conta? Entre para acessar sua agenda. Se for seu primeiro acesso, crie uma conta vinculada a esta barbearia."),
          actionButton("public_abrir_login","Entrar na minha conta",
                       icon=icon("sign-in"),class="btn-primary btn-block"),
          br(),
          actionButton("public_abrir_cadastro","Criar minha conta",
                       icon=icon("user-plus"),class="btn-success btn-block")),
      div(class="public-section",
          h4("Endereço público"),
          p("Slug: ",span(class="public-slug",textOutput("public_slug",inline=TRUE))),
          p(textOutput("public_link"))),
      div(class="public-section",
          p(class="small-help",
            "No Shiny local, o endereço utiliza o parâmetro ?barbearia=slug. O login do cliente será acrescentado na próxima etapa.")))
)

public_cadastro_ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#ecf0f5}
    .public-signup-card{max-width:560px;margin:5% auto;background:#fff;padding:34px;border-radius:10px;box-shadow:0 2px 14px rgba(0,0,0,.15)}
    .public-signup-title{text-align:center;margin-bottom:8px}
    .public-signup-subtitle{text-align:center;color:#777;margin-bottom:25px}
  "))),
  div(class="public-signup-card",
      h2(class="public-signup-title","Criar minha conta"),
      p(class="public-signup-subtitle",
        "Conta para ",textOutput("public_cadastro_nome",inline=TRUE)),
      textInput("public_cad_nome","Nome completo *"),
      textInput("public_cad_usuario","Nome de usuário *",placeholder="ex.: mateus"),
      p(class="small-help",
        "Escolha um nome de usuário único para entrar no sistema. Use de 3 a 30 caracteres: letras sem acento, números, ponto, hífen ou underline. Se já estiver em uso, você precisará escolher outro."),
      textInput("public_cad_telefone","Telefone *",placeholder="(14) 99999-9999"),
      textInput("public_cad_email","E-mail *",placeholder="cliente@email.com"),
      passwordInput("public_cad_senha","Senha *"),
      passwordInput("public_cad_confirmar","Confirmar senha *"),
      p(class="small-help",
        "A senha deve ter pelo menos 8 caracteres, com letra maiúscula, minúscula, número e caractere especial."),
      p(class="small-help",
        "Seu cadastro será criado somente nesta barbearia. A mesma pessoa poderá ter outra conta em outra barbearia."),
      actionButton("public_salvar_cadastro","Criar conta",
                   icon=icon("user-plus"),class="btn-success btn-block"),
      br(),
      actionButton("public_voltar","Voltar",
                   icon=icon("arrow-left"),class="btn-default btn-block")
  )
)

public_cadastro_sucesso_ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#ecf0f5}
    .public-success-card{max-width:600px;margin:10% auto;background:#fff;padding:36px;border-radius:10px;box-shadow:0 2px 14px rgba(0,0,0,.15);text-align:center}
  "))),
  div(class="public-success-card",
      h2("✅ Cadastro realizado"),
      h4(textOutput("public_sucesso_nome")),
      p("Sua conta foi criada nesta barbearia."),
      p("Seu cadastro ficou disponível para a equipe da barbearia, mas ainda não foi incluído na tabela operacional de Clientes."),
      p("Quando houver um agendamento, seus dados serão vinculados automaticamente ao cadastro de cliente."),
      br(),
      actionButton("public_sucesso_login","Entrar na minha conta",
                   icon=icon("sign-in"),class="btn-success"),
      br(),br(),
      actionButton("public_sucesso_voltar","Voltar para a página da barbearia",
                   icon=icon("arrow-left"),class="btn-primary"))
)

public_erro_ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#ecf0f5}
    .public-error-card{max-width:600px;margin:10% auto;background:#fff;padding:36px;border-radius:10px;box-shadow:0 2px 14px rgba(0,0,0,.15);text-align:center}
  "))),
  div(class="public-error-card",
      h2("💈 Barbearia"),
      h3("Barbearia não encontrada"),
      p("O endereço público informado não corresponde a nenhuma barbearia cadastrada."),
      p(class="small-help",textOutput("public_slug_erro")),
      br(),
      actionButton("public_erro_login","Ir para o login",icon=icon("sign-in"),class="btn-primary"))
)

cliente_ui <- dashboardPage(
  dashboardHeader(title="💈 Minha Agenda"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Minha agenda",tabName="c_agenda",icon=icon("calendar")),
      menuItem("Meus dados",tabName="c_dados",icon=icon("user")),
      menuItem("Alterar senha",tabName="c_senha",icon=icon("lock")),
      br(),
      div(style="padding:10px;",actionButton("cliente_logout","Sair",icon=icon("sign-out"),class="btn-danger btn-block"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      html, body { max-width:100%; overflow-x:hidden; }
      .wrapper, .content-wrapper, .right-side, .main-footer { max-width:100%; }
      .content { max-width:100%; overflow-x:hidden; }
      .dataTables_wrapper { width:100% !important; max-width:100% !important; overflow-x:auto; -webkit-overflow-scrolling:touch; }
      .dataTables_wrapper table.dataTable { width:100% !important; max-width:none; }

      @media(max-width:767px){
        .content-wrapper,.right-side,.main-footer{margin-left:0 !important;width:100% !important;min-width:0 !important;}
        .main-header .logo{width:50px !important;}
        .main-header .navbar{margin-left:50px !important;}
        .content{padding:10px !important;}
        .client-booking,.client-card,.client-welcome,.client-history,.box{max-width:100% !important;min-width:0 !important;}
        .client-card .btn{width:100%;}
        .dataTables_wrapper{display:block !important;overflow-x:auto !important;}
        table.dataTable{min-width:680px;font-size:13px;}
        table.dataTable th,table.dataTable td{white-space:nowrap;}
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate{
          float:none !important;text-align:left !important;width:100% !important;margin:6px 0 !important;
        }
      }
      @media(max-width:480px){
        .booking-steps{grid-template-columns:1fr !important;}
        .client-welcome,.client-card{padding:14px !important;}
        .client-welcome h2{font-size:22px !important;}
      }

      .client-booking { max-width:1180px; margin:0 auto; padding-bottom:25px; }
      .client-welcome { background:linear-gradient(135deg,#ffffff,#f8fafc); border:1px solid #e5e7eb;
                        border-radius:18px; padding:22px 24px; margin:5px 0 18px;
                        box-shadow:0 5px 18px rgba(15,23,42,.06); }
      .client-welcome h2 { margin:0 0 6px; color:#172033; font-weight:750; }
      .client-welcome p { margin:0;color:#64748b; }
      .booking-steps { display:grid;grid-template-columns:repeat(4,1fr);gap:9px;margin:0 0 16px; }
      .booking-step { background:#fff;border:1px solid #e5e7eb;border-radius:11px;padding:10px 12px;
                      color:#475569;font-weight:650;text-align:center; }
      .booking-step span { display:inline-flex;width:24px;height:24px;border-radius:50%;background:#334155;
                           color:#fff;align-items:center;justify-content:center;margin-right:6px;font-size:12px; }
      .client-card { background:#fff;border:1px solid #e5e7eb;border-radius:15px;padding:20px;
                     box-shadow:0 4px 15px rgba(15,23,42,.055);margin-bottom:18px; }
      .client-card h3 { margin:0 0 4px;font-size:19px;font-weight:750;color:#172033; }
      .client-card .sub { color:#64748b;margin-bottom:17px; }
      .booking-summary { background:#f8fafc;border:1px solid #e5e7eb;border-radius:11px;
                         padding:13px 15px;margin:5px 0 14px; }
      .booking-summary .summary-item { display:inline-block;margin-right:28px;color:#475569; }
      .client-card .form-control { border-radius:9px;min-height:40px; }
      .client-card .btn { border-radius:9px;font-weight:650;padding:10px 18px; }
      .client-history .box { border-radius:14px;overflow:hidden;border-top:0;box-shadow:0 4px 14px rgba(15,23,42,.055); }
      @media(max-width:767px){.booking-steps{grid-template-columns:repeat(2,1fr);}}
    "))),
    tabItems(
      tabItem("c_agenda",
        div(class="client-booking",
          div(class="client-welcome",
              h2("Agende seu horário"),
              p("Escolha o serviço, o profissional e o melhor horário para você.")),
          div(class="booking-steps",
              div(class="booking-step",span("1"),"Serviço"),
              div(class="booking-step",span("2"),"Barbeiro"),
              div(class="booking-step",span("3"),"Data e horário"),
              div(class="booking-step",span("4"),"Confirmar")),
          div(class="client-card",
              h3("Novo agendamento"),
              div(class="sub","Preencha as opções abaixo. Mostraremos apenas os horários realmente disponíveis."),
              fluidRow(
                column(6,selectInput("c_ag_servico","1. Escolha o serviço",choices=character(0))),
                column(6,selectInput("c_ag_barbeiro","2. Escolha o barbeiro",choices=character(0)))
              ),
              fluidRow(
                column(6,dateInput("c_ag_data","3. Escolha a data",value=Sys.Date(),min=Sys.Date(),
                                   format="dd/mm/yyyy",language="pt-BR")),
                column(6,
                       selectInput("c_ag_hora","Horário disponível",choices=character(0)),
                       uiOutput("c_ag_hora_aviso"))
              ),
              div(class="booking-summary",
                  div(class="summary-item",strong("Preço: "),textOutput("c_ag_preco",inline=TRUE)),
                  div(class="summary-item",strong("Duração: "),textOutput("c_ag_duracao",inline=TRUE))),
              textAreaInput("c_ag_obs","Observação",rows=2,placeholder="Opcional"),
              actionButton("c_ag_salvar","Confirmar agendamento",
                           icon=icon("check-circle"),class="btn-primary btn-block")
          ),
          div(class="client-history",
            box(title=tagList(icon("history")," Meus atendimentos"),width=12,status="primary",solidHeader=TRUE,
                DTOutput("minha_agenda"))
          )
        )
      ),
      tabItem("c_dados",
              h2("Meus dados"),
              box(title="Cadastro",width=8,status="primary",solidHeader=TRUE,
                  textOutput("c_nome"),br(),textOutput("c_telefone"),br(),textOutput("c_email"))),
      tabItem("c_senha",
              h2("Alterar senha"),
              box(title="Nova senha",width=6,status="primary",solidHeader=TRUE,
                  passwordInput("c_nova_senha","Nova senha"),
                  passwordInput("c_conf_senha","Confirmar senha"),
                  actionButton("c_salvar_senha","Salvar senha",icon=icon("save"),class="btn-primary")))
    )
  )
)

ui <- fluidPage(uiOutput("root_ui"))

# =========================================================
# SERVER
# =========================================================

server <- function(input,output,session) {

  estado <- reactiveValues(
    pagina = if(nrow(dbq("SELECT id FROM usuarios WHERE perfil='MASTER' LIMIT 1"))==0) "setup" else "login",
    usuario = NULL,
    token = NULL,
    public_barb_id = NULL,
    public_slug = ""
  )

  clientes_upd <- reactiveVal(0)
  crm_upd <- reactiveVal(0)
  notificacoes_upd <- reactiveVal(0)
  cadastros_upd <- reactiveVal(0)
  servicos_upd <- reactiveVal(0)
  agenda_upd <- reactiveVal(0)
  bloqueios_upd <- reactiveVal(0)
  funcionamento_upd <- reactiveVal(0)
  jornada_upd <- reactiveVal(0)
  gastos_upd <- reactiveVal(0)
  master_upd <- reactiveVal(0)
  cliente_hora_aviso <- reactiveVal("")

  # -------------------------------------------------------
  # SINCRONIZAÇÃO VISUAL AUTOMÁTICA INTELIGENTE
  # -------------------------------------------------------
  # Antes, todos os *_upd eram incrementados a cada 3 segundos,
  # mesmo sem nenhuma alteração no banco. Isso reconstruía tabelas,
  # cards e outros componentes continuamente, causando "pisca-pisca".
  #
  # Agora a sessão consulta somente um número (versao) a cada 3 s.
  # Se a versão continua igual, NÃO atualiza nenhuma tela.
  # Somente quando algum INSERT/UPDATE/DELETE realmente ocorreu,
  # os gatilhos reativos são disparados uma única vez.

  versao_banco_inicial <- tryCatch({
    z <- DBI::dbGetQuery(con,
      "SELECT CAST(versao AS BIGINT) AS versao
         FROM dbo.app_sync_version WHERE id=1")
    if(nrow(z)>0) as.numeric(z$versao[1]) else 0
  }, error=function(e) 0)

  ultima_versao_banco <- reactiveVal(versao_banco_inicial)

  observe({
    invalidateLater(3000, session)

    versao_atual <- tryCatch({
      z <- DBI::dbGetQuery(con,
        "SELECT CAST(versao AS BIGINT) AS versao
           FROM dbo.app_sync_version WHERE id=1")
      if(nrow(z)>0) as.numeric(z$versao[1]) else isolate(ultima_versao_banco())
    }, error=function(e) isolate(ultima_versao_banco()))

    anterior <- isolate(ultima_versao_banco())

    if(!is.na(versao_atual) && !identical(versao_atual, anterior)) {
      isolate({
        ultima_versao_banco(versao_atual)

        clientes_upd(clientes_upd()+1)
        crm_upd(crm_upd()+1)
        notificacoes_upd(notificacoes_upd()+1)
        cadastros_upd(cadastros_upd()+1)
        servicos_upd(servicos_upd()+1)
        agenda_upd(agenda_upd()+1)
        bloqueios_upd(bloqueios_upd()+1)
        funcionamento_upd(funcionamento_upd()+1)
        jornada_upd(jornada_upd()+1)
        gastos_upd(gastos_upd()+1)
        master_upd(master_upd()+1)
      })
    }
  })

  master_id_selecionada <- function() {
    # O seletor lateral é a fonte única da barbearia em visualização.
    # O seletor da tela "Barbearias" apenas atualiza este valor.
    v <- suppressWarnings(as.integer(input$master_sel[1]))
    if(length(v)==0 || is.na(v)) return(NULL)
    v
  }

  id_barb <- function() {
    if(!is.null(estado$usuario) &&
       !is.null(estado$usuario$perfil) &&
       estado$usuario$perfil[1]=="MASTER") {
      return(master_id_selecionada())
    }

    if(is.null(estado$usuario)||is.null(estado$usuario$barbearia_id)||
       length(estado$usuario$barbearia_id)==0 || is.na(estado$usuario$barbearia_id[1])) return(NULL)
    as.integer(estado$usuario$barbearia_id[1])
  }

  id_cli <- function() {
    if(is.null(estado$usuario)||is.null(estado$usuario$cliente_id)||
       is.na(estado$usuario$cliente_id)) return(NULL)
    as.integer(estado$usuario$cliente_id[1])
  }

  # Registra no banco somente alterações relevantes.
  # Nunca recebe ou armazena senhas.
  registrar_historico <- function(barbearia_id, acao, entidade, campo=NULL,
                                  valor_anterior=NULL, valor_novo=NULL) {
    actor_id <- if(!is.null(estado$usuario) && !is.null(estado$usuario$id))
      as.integer(estado$usuario$id[1]) else NA_integer_
    perfil <- if(!is.null(estado$usuario) && !is.null(estado$usuario$perfil))
      as.character(estado$usuario$perfil[1]) else NA_character_
    antes <- if(is.null(valor_anterior) || length(valor_anterior)==0 || is.na(valor_anterior)) "" else as.character(valor_anterior)
    depois <- if(is.null(valor_novo) || length(valor_novo)==0 || is.na(valor_novo)) "" else as.character(valor_novo)
    if(identical(antes,depois) && acao=="ALTERACAO") return(invisible(NULL))
    dbe("
      INSERT INTO historico_alteracoes(
        barbearia_id,usuario_id,perfil_usuario,acao,entidade,campo,
        valor_anterior,valor_novo,data_alteracao)
      VALUES(?,?,?,?,?,?,?,?,?)",
      params=list(as.integer(barbearia_id),actor_id,perfil,acao,entidade,campo,
                  antes,depois,format(Sys.time(),"%Y-%m-%d %H:%M:%S")))
    invisible(NULL)
  }

  output$p_link_publico <- renderText({
    bid <- id_barb()
    if(is.null(bid)) return("-")
    link_publico_barbearia(session,bid)
  })

  output$gestao_sidebar <- renderUI({
    if(is.null(estado$usuario) || !estado$pagina %in% c("gestao")) return(NULL)
    master <- identical(as.character(estado$usuario$perfil[1]), "MASTER")

    if(master) {
      tagList(
        div(style="padding:10px 10px 0 10px;",
            selectInput("master_sel", "Barbearia em visualização", choices=character(0))),
        sidebarMenu(
          tags$li(class="header","ADMINISTRAÇÃO MASTER"),
          menuItem("Barbearias",tabName="m_lista",icon=icon("building")),
          menuItem("Cadastrar barbearia",tabName="m_cadastro",icon=icon("plus")),
          tags$li(class="header","GESTÃO DA BARBEARIA SELECIONADA"),
          menuItem("Dashboard",tabName="dashboard",icon=icon("dashboard")),
          menuItem("Clientes",tabName="clientes",icon=icon("users")),
          menuItem("CRM",tabName="crm",icon=icon("address-card")),
          menuItem("Notificações",tabName="notificacoes",icon=icon("envelope")),
          menuItem("Cadastros online",tabName="cadastros_clientes",icon=icon("user-plus")),
          menuItem("Serviços",tabName="servicos",icon=icon("scissors")),
          menuItem("Agenda",tabName="agenda",icon=icon("calendar")),
          menuItem("Financeiro",tabName="financeiro",icon=icon("money")),
          menuItem("Dados da barbearia",tabName="dados",icon=icon("building")),
          br(),
          div(style="padding:10px;",actionButton("logout","Sair",icon=icon("sign-out"),class="btn-danger btn-block"))
        )
      )
    } else {
      sidebarMenu(
        menuItem("Dashboard",tabName="dashboard",icon=icon("dashboard")),
        menuItem("Clientes",tabName="clientes",icon=icon("users")),
        menuItem("CRM",tabName="crm",icon=icon("address-card")),
        menuItem("Notificações",tabName="notificacoes",icon=icon("envelope")),
        menuItem("Cadastros online",tabName="cadastros_clientes",icon=icon("user-plus")),
        menuItem("Serviços",tabName="servicos",icon=icon("scissors")),
        menuItem("Agenda",tabName="agenda",icon=icon("calendar")),
        menuItem("Financeiro",tabName="financeiro",icon=icon("money")),
        menuItem("Dados da barbearia",tabName="dados",icon=icon("building")),
        br(),
        div(style="padding:10px;",actionButton("logout","Sair",icon=icon("sign-out"),class="btn-danger btn-block"))
      )
    }
  })

  output$root_ui <- renderUI({
    switch(estado$pagina,
      setup=setup_ui,
      login=login_ui,
      reset=reset_ui,
      troca=troca_senha_ui,
      gestao=gestao_ui,
      client=cliente_ui,
      public=public_ui,
      public_cadastro=public_cadastro_ui,
      public_cadastro_sucesso=public_cadastro_sucesso_ui,
      public_erro=public_erro_ui,
      login_ui)
  })

  # -------------------------------------------------------
  # PÁGINA PÚBLICA DA BARBEARIA
  # -------------------------------------------------------
  public_dados <- reactive({
    if(is.null(estado$public_barb_id) || is.na(estado$public_barb_id)) return(NULL)
    dbq("
      SELECT b.id,b.nome,b.telefone,b.status_acesso,b.slug_publico,
             d.nome_responsavel,d.cep,d.endereco,d.numero,d.bairro,d.cidade,d.complemento,
             a.status AS status_assinatura,a.data_vencimento
      FROM barbearias b
      LEFT JOIN dados_barbearia d ON d.barbearia_id=b.id
      LEFT JOIN assinaturas a ON a.id=(
        SELECT MAX(a2.id) FROM assinaturas a2 WHERE a2.barbearia_id=b.id)
      WHERE b.id=? LIMIT 1",params=list(as.integer(estado$public_barb_id)))
  })

  output$public_nome <- renderText({
    b <- public_dados()
    if(is.null(b)||nrow(b)==0) "Barbearia" else b$nome[1]
  })

  output$public_slug <- renderText({
    b <- public_dados()
    if(is.null(b)||nrow(b)==0) estado$public_slug else b$slug_publico[1]
  })

  output$public_telefone <- renderText({
    b <- public_dados()
    if(is.null(b)||nrow(b)==0) return("")
    tel <- ifelse(is.na(b$telefone[1]),"",b$telefone[1])
    if(!nzchar(tel)) "Telefone não informado" else paste("Telefone:",tel)
  })

  output$public_endereco <- renderText({
    b <- public_dados()
    if(is.null(b)||nrow(b)==0) return("")
    partes <- c(
      ifelse(is.na(b$endereco[1]),"",trimws(b$endereco[1])),
      ifelse(is.na(b$numero[1]),"",trimws(b$numero[1])),
      ifelse(is.na(b$bairro[1]),"",trimws(b$bairro[1])),
      ifelse(is.na(b$cidade[1]),"",trimws(b$cidade[1])),
      ifelse(is.na(b$cep[1]),"",paste0("CEP ",trimws(b$cep[1])))
    )
    partes <- partes[nzchar(partes)]
    if(length(partes)==0) "Endereço não informado" else paste("Endereço:",paste(partes,collapse=", "))
  })

  output$public_responsavel <- renderText({
    b <- public_dados()
    if(is.null(b)||nrow(b)==0) return("")
    r <- ifelse(is.na(b$nome_responsavel[1]),"",trimws(b$nome_responsavel[1]))
    if(!nzchar(r)) "" else paste("Responsável:",r)
  })

  output$public_status <- renderUI({
    b <- public_dados()
    if(is.null(b)||nrow(b)==0) return(NULL)

    acesso <- as.character(b$status_acesso[1] %||% "")
    ass <- as.character(b$status_assinatura[1] %||% "")
    venc <- b$data_vencimento[1]

    ativa <- identical(acesso,"ATIVA") && identical(ass,"ATIVA") &&
      !is.na(venc) && as.Date(venc) >= Sys.Date()

    if(ativa) {
      div(class="alert alert-success",
          strong("Acesso disponível. "),
          "Esta barbearia está com a página pública ativa.")
    } else {
      div(class="alert alert-warning",
          strong("Atenção: "),
          "o acesso desta barbearia não está liberado no momento.")
    }
  })

  output$public_link <- renderText({
    b <- public_dados()
    if(is.null(b)||nrow(b)==0) return("")
    link_publico_barbearia(session,as.integer(b$id[1]))
  })

  output$public_slug_erro <- renderText({
    if(!nzchar(estado$public_slug)) "" else paste("Slug informado:",estado$public_slug)
  })

  output$public_cadastro_nome <- renderText({
    b <- public_dados()
    if(is.null(b)||nrow(b)==0) "Barbearia" else b$nome[1]
  })

  output$public_sucesso_nome <- renderText({
    b <- public_dados()
    if(is.null(b)||nrow(b)==0) "Cadastro concluído" else b$nome[1]
  })

  observeEvent(input$public_abrir_cadastro,{
    slug_url <- extrair_slug_publico(session$clientData$url_pathname,
                                     session$clientData$url_search)
    if(!nzchar(slug_url)) {
      showNotification("O link da barbearia não foi identificado.",type="error")
      return()
    }

    b <- buscar_barbearia_slug(slug_url)
    if(is.null(b)) {
      estado$public_barb_id <- NULL
      estado$public_slug <- slug_url
      estado$pagina <- "public_erro"
      return()
    }

    estado$public_barb_id <- as.integer(b$id[1])
    estado$public_slug <- as.character(b$slug_publico[1] %||% slug_url)
    updateTextInput(session,"public_cad_nome",value="")
    updateTextInput(session,"public_cad_usuario",value="")
    updateTextInput(session,"public_cad_telefone",value="")
    updateTextInput(session,"public_cad_email",value="")
    estado$pagina <- "public_cadastro"
  })

  observeEvent(input$public_voltar,{
    estado$pagina <- "public"
  })

  # Login iniciado pela página pública. Mantemos public_barb_id/public_slug
  # para que contas CLIENTE sejam autenticadas dentro da unidade correta.
  observeEvent(input$public_abrir_login,{
    slug_url <- extrair_slug_publico(session$clientData$url_pathname,
                                     session$clientData$url_search)
    if(!nzchar(slug_url)) {
      showNotification("O link da barbearia não foi identificado.",type="error")
      return()
    }
    b <- buscar_barbearia_slug(slug_url)
    if(is.null(b)) {
      estado$public_barb_id <- NULL
      estado$public_slug <- slug_url
      estado$pagina <- "public_erro"
      return()
    }
    estado$public_barb_id <- as.integer(b$id[1])
    estado$public_slug <- as.character(b$slug_publico[1] %||% slug_url)
    estado$pagina <- "login"
  })

  observeEvent(input$public_sucesso_login,{
    # Após o cadastro, a unidade já está armazenada no estado.
    if(is.null(estado$public_barb_id) || is.na(estado$public_barb_id)) {
      estado$pagina <- "public"
      return()
    }
    estado$pagina <- "login"
  })

  observeEvent(input$public_sucesso_voltar,{
    estado$pagina <- "public"
  })

  observeEvent(input$public_salvar_cadastro,{
    bid <- estado$public_barb_id
    if(is.null(bid) || is.na(bid)) {
      showNotification("Barbearia não identificada.",type="error"); return()
    }

    # Quando o cadastro veio de uma URL pública, ela é a fonte de verdade.
    slug_url <- extrair_slug_publico(session$clientData$url_pathname,
                                     session$clientData$url_search)
    if(nzchar(slug_url)) {
      b_url <- buscar_barbearia_slug(slug_url)
      if(is.null(b_url) || as.integer(b_url$id[1]) != as.integer(bid)) {
        showNotification("O link da barbearia não corresponde ao cadastro atual.",
                         type="error",duration=8)
        return()
      }
      bid <- as.integer(b_url$id[1])
      estado$public_barb_id <- bid
      estado$public_slug <- as.character(b_url$slug_publico[1] %||% slug_url)
    }

    nome <- trimws(input$public_cad_nome)
    tel <- gsub("[^0-9]","",trimws(input$public_cad_telefone))
    email <- trimws(input$public_cad_email)
    senha <- input$public_cad_senha
    confirmar <- input$public_cad_confirmar

    if(nome=="") {
      showNotification("Informe seu nome.",type="error"); return()
    }

    vu <- validar_nome_usuario(input$public_cad_usuario)
    if(!vu$ok) {
      showNotification(vu$msg,type="error",duration=8); return()
    }
    nome_usuario <- vu$valor

    # O nome de usuário é único em TODO o sistema (todas as barbearias),
    # sem diferenciar maiúsculas de minúsculas: "Usuario" e "usuario" são o mesmo.
    usuario_em_uso <- dbq("
      SELECT id FROM usuarios
      WHERE LOWER(nome_usuario)=LOWER(?)
      LIMIT 1",params=list(nome_usuario))
    if(nrow(usuario_em_uso)>0) {
      showNotification(paste0("O nome de usuário \"",nome_usuario,
                              "\" já está em uso. Escolha outro."),
                       type="error",duration=8); return()
    }

    if(nchar(tel)!=11) {
      showNotification("O telefone deve conter exatamente 11 dígitos.",type="error"); return()
    }
    if(!validar_email(email)) {
      showNotification("Informe um e-mail válido.",type="error"); return()
    }

    erros_senha <- validar_senha(if(is.null(senha)) "" else senha)
    if(length(erros_senha)>0) {
      showNotification(paste(erros_senha,collapse="\n"),type="error",duration=8); return()
    }
    if(senha!=confirmar) {
      showNotification("As senhas não coincidem.",type="error"); return()
    }

    existente_cadastro <- dbq("
      SELECT id FROM cadastros_clientes
      WHERE barbearia_id=? AND LOWER(email)=LOWER(?)
      LIMIT 1",params=list(as.integer(bid),email))
    if(nrow(existente_cadastro)>0) {
      showNotification("Este e-mail já está cadastrado nesta barbearia.",type="error",duration=8); return()
    }

    existente_usuario <- dbq("
      SELECT id FROM usuarios
      WHERE perfil='CLIENTE' AND barbearia_id=? AND LOWER(email)=LOWER(?)
      LIMIT 1",params=list(as.integer(bid),email))
    if(nrow(existente_usuario)>0) {
      showNotification("Este e-mail já possui uma conta nesta barbearia.",type="error",duration=8); return()
    }

    cliente_existente <- dbq("
      SELECT id FROM clientes
      WHERE barbearia_id=? AND LOWER(email)=LOWER(?)
      LIMIT 1",params=list(as.integer(bid),email))
    cliente_id_existente <- if(nrow(cliente_existente)>0) as.integer(cliente_existente$id[1]) else NA_integer_

    tryCatch({
      dbWithTransaction(con,{
        dbe("
          INSERT INTO cadastros_clientes(
            barbearia_id,usuario_id,cliente_id,nome,telefone,email,data_cadastro,hora_cadastro,status,data_conversao)
          VALUES(?,NULL,?,?,?,?,?,?,'CADASTRADO',?)",
          params=list(as.integer(bid),cliente_id_existente,nome,tel,email,as.character(Sys.Date()),
                      format(Sys.time(),"%H:%M:%S"),
                      if(!is.na(cliente_id_existente)) as.character(Sys.Date()) else NA_character_))

        cadastro_id <- as.integer(db_last_id())

        dbe("
          INSERT INTO usuarios(barbearia_id,cliente_id,nome_usuario,email,senha_hash,perfil,
                               ativo,troca_senha_obrigatoria,data_cadastro)
          VALUES(?,?,?,?,?,'CLIENTE',1,0,?)",
          params=list(as.integer(bid),cliente_id_existente,nome_usuario,email,hash_senha(senha),as.character(Sys.Date())))

        usuario_id <- as.integer(db_last_id())
        dbe("
          UPDATE cadastros_clientes
          SET usuario_id=?,cliente_id=?,status=?,data_conversao=?
          WHERE id=?",
          params=list(usuario_id,cliente_id_existente,
                      if(!is.na(cliente_id_existente)) 'CONVERTIDO' else 'CADASTRADO',
                      if(!is.na(cliente_id_existente)) as.character(Sys.Date()) else NA_character_,
                      cadastro_id))

        if(!is.na(cliente_id_existente))
          dbe("
            UPDATE clientes SET cadastro_cliente_id=?
            WHERE id=? AND barbearia_id=?",
            params=list(cadastro_id,cliente_id_existente,as.integer(bid)))

        registrar_historico(bid,"CRIACAO","CADASTRO_CLIENTE","nome","",nome)
        registrar_historico(bid,"CRIACAO","CADASTRO_CLIENTE","telefone","",tel)
        registrar_historico(bid,"CRIACAO","CADASTRO_CLIENTE","email","",email)
        registrar_historico(bid,"CRIACAO","USUARIO_CLIENTE","nome_usuario","",nome_usuario)
        registrar_historico(bid,"CRIACAO","USUARIO_CLIENTE","email","",email)
      })

      cadastros_upd(cadastros_upd()+1)
      estado$pagina <- "public_cadastro_sucesso"
      showNotification("Conta criada com sucesso.",type="message",duration=5)
    },error=function(e) {
      msg <- conditionMessage(e)
      if(grepl("ux_user_login",msg,fixed=TRUE))
        showNotification(paste0("O nome de usuário \"",nome_usuario,
                                "\" acabou de ser usado por outra pessoa. Escolha outro."),
                         type="error",duration=10)
      else
        showNotification(paste("Não foi possível criar a conta:",msg),
                         type="error",duration=10)
    })
  })

  # -------------------------------------------------------
  # ROTEAMENTO INICIAL
  # -------------------------------------------------------
  # O token continua tendo prioridade para recuperação/ativação de senha.
  # Para a página pública, usamos ?barbearia=<slug> no servidor local do Shiny.
  # O caminho /barbearia/<slug> continua sendo reconhecido quando o deploy
  # estiver configurado com encaminhamento de rota.
  observeEvent(session$clientData$url_search,{
    q <- parseQueryString(session$clientData$url_search)
    path <- session$clientData$url_pathname

    if(!is.null(q$token)&&nzchar(q$token)) {
      estado$token <- q$token
      estado$pagina <- "reset"
      return()
    }

    slug <- extrair_slug_publico(path,session$clientData$url_search)
    if(nzchar(slug)) {
      estado$public_slug <- slug
      b <- buscar_barbearia_slug(slug)
      if(is.null(b)) {
        estado$public_barb_id <- NULL
        estado$pagina <- "public_erro"
      } else {
        estado$public_barb_id <- as.integer(b$id[1])
        estado$pagina <- "public"
      }
    }
  },once=TRUE,ignoreNULL=FALSE)

  observeEvent(input$public_erro_login,{
    estado$public_barb_id <- NULL
    estado$public_slug <- ""
    estado$pagina <- "login"
  })

  observeEvent(input$entrar,{
    login <- trimws(input$login_usuario)
    senha <- input$login_senha
    if(login==""||is.null(senha)||senha=="") {
      showNotification("Informe usuário/e-mail e senha.",type="error"); return()
    }

    # Pela página pública, priorizamos a conta CLIENTE da barbearia do link.
    # MASTER/BARBEIRO continuam podendo entrar normalmente.
    bid_publico <- suppressWarnings(as.integer(estado$public_barb_id %||% NA_integer_))

    if(!is.na(bid_publico)) {
      u <- dbq("
        SELECT TOP 1 u.*
        FROM usuarios u
        WHERE (LOWER(u.nome_usuario)=LOWER(?) OR LOWER(u.email)=LOWER(?))
          AND (
            u.perfil IN ('MASTER','BARBEIRO')
            OR (u.perfil='CLIENTE' AND u.barbearia_id=?)
          )
        ORDER BY CASE WHEN u.perfil='CLIENTE' AND u.barbearia_id=? THEN 0 ELSE 1 END, u.id
      ",params=list(login,login,bid_publico,bid_publico))
    } else {
      u <- dbq("
        SELECT TOP 1 u.*
        FROM usuarios u
        WHERE LOWER(u.nome_usuario)=LOWER(?) OR LOWER(u.email)=LOWER(?)
        ORDER BY u.id
      ",params=list(login,login))
    }

    if(nrow(u)==0||u$ativo[1]!=1||!verificar_senha(u$senha_hash[1],senha)) {
      showNotification("Usuário ou senha inválidos.",type="error"); return()
    }

    if(u$perfil[1]!="MASTER") {
      acc <- checar_assinatura(as.integer(u$barbearia_id[1]))
      if(!acc$ok) {
        showNotification(acc$msg,type="error",duration=8); return()
      }
    }

    estado$usuario <- u[1,]

    if(u$troca_senha_obrigatoria[1]==1) {
      estado$pagina <- "troca"
    } else if(u$perfil[1]=="MASTER") {
      estado$pagina <- "gestao"
    } else if(u$perfil[1]=="BARBEIRO") {
      estado$pagina <- "gestao"
    } else if(u$perfil[1]=="CLIENTE") {
      estado$pagina <- "client"
    }
  })

  # -------------------------------------------------------
  # PRIMEIRO MASTER
  # -------------------------------------------------------
  observeEvent(input$criar_master,{
    nome <- trimws(input$setup_nome)
    usuario <- trimws(input$setup_usuario)
    email <- trimws(input$setup_email)
    s <- input$setup_senha
    c <- input$setup_confirmar

    if(nome==""||usuario==""||email=="") {
      showNotification("Preencha os campos.",type="error"); return()
    }
    if(!validar_email(email)) {
      showNotification("Informe um e-mail válido.",type="error"); return()
    }
    er <- validar_senha(s)
    if(length(er)>0) {
      showNotification(paste(er,collapse="\n"),type="error",duration=8); return()
    }
    if(s!=c) {
      showNotification("As senhas não coincidem.",type="error"); return()
    }
    dup <- dbq("
      SELECT id FROM usuarios
      WHERE LOWER(nome_usuario)=LOWER(?) OR LOWER(email)=LOWER(?) LIMIT 1",
      params=list(usuario,email))
    if(nrow(dup)>0) {
      showNotification("Usuário ou e-mail já cadastrado.",type="error"); return()
    }
    dbe("
      INSERT INTO usuarios(barbearia_id,cliente_id,nome_usuario,email,senha_hash,
                           perfil,ativo,troca_senha_obrigatoria,data_cadastro)
      VALUES(NULL,NULL,?,?,?,'MASTER',1,0,?)",
      params=list(usuario,email,hash_senha(s),as.character(Sys.Date())))
    estado$pagina <- "login"
    showNotification("MASTER criado. Faça o login.",type="message",duration=6)
  })

  # -------------------------------------------------------
  # TROCA OBRIGATÓRIA
  # -------------------------------------------------------
  observeEvent(input$salvar_troca,{
    er <- validar_senha(input$troca_nova)
    if(length(er)>0) {
      showNotification(paste(er,collapse="\n"),type="error",duration=8); return()
    }
    if(input$troca_nova!=input$troca_confirmar) {
      showNotification("As senhas não coincidem.",type="error"); return()
    }
    dbe("UPDATE usuarios SET senha_hash=?,troca_senha_obrigatoria=0 WHERE id=?",
              params=list(hash_senha(input$troca_nova),estado$usuario$id[1]))
    estado$usuario$senha_hash <- hash_senha(input$troca_nova)
    estado$usuario$troca_senha_obrigatoria <- 0
    estado$pagina <- switch(estado$usuario$perfil[1],MASTER="gestao",BARBEIRO="gestao",CLIENTE="client","login")
    showNotification("Senha alterada com sucesso.",type="message")
  })
  observeEvent(input$sair_troca,{estado$usuario<-NULL;estado$pagina<-"login"})

  # -------------------------------------------------------
  # RECUPERAÇÃO DE CLIENTE
  # -------------------------------------------------------
  # Cadastro de cliente iniciado pela tela de login. Como a conta de CLIENTE
  # pertence a uma barbearia específica, primeiro o usuário escolhe a unidade.
  observeEvent(input$login_criar_conta,{
    # Se o cliente chegou por uma URL pública da barbearia, a unidade já está
    # identificada e não deve ser possível escolher outra no cadastro.
    slug_url <- extrair_slug_publico(session$clientData$url_pathname,
                                     session$clientData$url_search)

    if(nzchar(slug_url)) {
      b <- buscar_barbearia_slug(slug_url)
      if(is.null(b)) {
        estado$public_barb_id <- NULL
        estado$public_slug <- slug_url
        estado$pagina <- "public_erro"
        return()
      }

      estado$public_barb_id <- as.integer(b$id[1])
      estado$public_slug <- as.character(b$slug_publico[1] %||% slug_url)

      updateTextInput(session,"public_cad_nome",value="")
      updateTextInput(session,"public_cad_usuario",value="")
      updateTextInput(session,"public_cad_telefone",value="")
      updateTextInput(session,"public_cad_email",value="")
      estado$pagina <- "public_cadastro"
      return()
    }

    # Tela genérica da plataforma, sem ?barbearia=<slug>:
    # mantém a seleção de unidade como fallback administrativo/legado.
    barbs <- dbq("SELECT id,nome FROM barbearias WHERE status_acesso<>'CANCELADA' ORDER BY nome")
    if(nrow(barbs)==0) {
      showNotification("Não há barbearias disponíveis para cadastro.",type="warning")
      return()
    }

    choices <- setNames(as.character(barbs$id),as.character(barbs$nome))
    showModal(modalDialog(
      title="Criar conta de cliente",
      p("Você acessou a plataforma sem o link de uma barbearia. Selecione a unidade para continuar."),
      selectInput("login_cadastro_barbearia","Barbearia *",choices=choices),
      footer=tagList(
        modalButton("Cancelar"),
        actionButton("login_confirmar_cadastro","Continuar",icon=icon("arrow-right"),class="btn-success")
      ),
      easyClose=TRUE
    ))
  })

  observeEvent(input$login_confirmar_cadastro,{
    # Este fluxo só existe quando a tela foi aberta sem uma URL pública.
    bid <- suppressWarnings(as.integer(input$login_cadastro_barbearia))
    if(is.na(bid)) {
      showNotification("Selecione uma barbearia.",type="warning")
      return()
    }

    b <- dbq("SELECT id,slug_publico FROM barbearias WHERE id=? AND status_acesso<>'CANCELADA' LIMIT 1",
             params=list(bid))
    if(nrow(b)==0) {
      showNotification("Barbearia não encontrada ou indisponível.",type="error")
      return()
    }

    estado$public_barb_id <- as.integer(b$id[1])
    estado$public_slug <- as.character(b$slug_publico[1] %||% "")
    removeModal()
    estado$pagina <- "public_cadastro"
  })

  observeEvent(input$abrir_recuperacao,{
    showModal(modalDialog(
      title="Recuperar senha",
      textInput("rec_email","E-mail do cliente"),
      footer=tagList(modalButton("Cancelar"),
                      actionButton("rec_enviar","Enviar link",icon=icon("envelope"),
                                   class="btn-primary"))
    ))
  })

  observeEvent(input$rec_enviar,{
    email <- trimws(input$rec_email)
    if(!validar_email(email)) {
      showNotification("Informe um e-mail válido.",type="error"); return()
    }
    bid_publico <- suppressWarnings(as.integer(estado$public_barb_id %||% NA_integer_))
    if(!is.na(bid_publico)) {
      u <- dbq("
        SELECT TOP 1 u.*
        FROM usuarios u
        WHERE LOWER(u.email)=LOWER(?)
          AND u.perfil='CLIENTE'
          AND u.ativo=1
          AND u.barbearia_id=?
        ORDER BY u.id",
        params=list(email,bid_publico))
    } else {
      u <- dbq("
        SELECT TOP 1 u.*
        FROM usuarios u
        WHERE LOWER(u.email)=LOWER(?) AND u.perfil='CLIENTE' AND u.ativo=1
        ORDER BY u.id",
        params=list(email))
    }
    if(nrow(u)==0) {
      removeModal()
      showNotification("Se o e-mail estiver cadastrado, um link será enviado.",
                       type="message",duration=7)
      return()
    }

    token <- gerar_token()
    dbe("UPDATE recuperacao_senha SET usado=1 WHERE usuario_id=? AND usado=0",
              params=list(u$id[1]))
    dbe("
      INSERT INTO recuperacao_senha(usuario_id,token_hash,tipo,expira_em,usado,criado_em)
      VALUES(?,?, 'RECUPERACAO',datetime('now','+30 minutes'),0,datetime('now'))",
      params=list(u$id[1],hash_token(token)))

    link <- paste0(get_app_url(session),"?token=",token)
    corpo <- paste0(
      "Olá!\n\nRecebemos um pedido para alterar a senha da sua conta na ",
      u$nome_barbearia[1],".\n\n",
      "[Redefinir minha senha](",link,")\n\n",
      "O link é válido por 30 minutos e só pode ser usado uma vez."
    )
    envio <- enviar_email(session,email,"Recuperação de senha - Barbearia",corpo)
    removeModal()
    if(envio$ok)
      showNotification("Se o e-mail estiver cadastrado, o link foi enviado.",
                       type="message",duration=8)
    else
      showNotification(paste("Não foi possível enviar o e-mail:",envio$msg),
                       type="error",duration=10)
  })

  observeEvent(input$salvar_reset,{
    if(is.null(estado$token)) {
      showNotification("Token inválido.",type="error"); return()
    }
    tok <- buscar_token(estado$token)
    if(is.null(tok)) {
      showNotification("Link inválido, expirado ou já utilizado.",type="error"); return()
    }
    er <- validar_senha(input$reset_nova)
    if(length(er)>0) {
      showNotification(paste(er,collapse="\n"),type="error",duration=8); return()
    }
    if(input$reset_nova!=input$reset_confirmar) {
      showNotification("As senhas não coincidem.",type="error"); return()
    }
    dbe("UPDATE usuarios SET senha_hash=?,troca_senha_obrigatoria=0 WHERE id=?",
              params=list(hash_senha(input$reset_nova),tok$usuario_id[1]))
    dbe("UPDATE recuperacao_senha SET usado=1 WHERE id=?",
              params=list(tok$token_id[1]))
    estado$token <- NULL
    estado$pagina <- "login"
    showNotification("Senha alterada com sucesso. Faça o login.",type="message",duration=6)
  })
  observeEvent(input$voltar_login_reset,{estado$token<-NULL;estado$pagina<-"login"})

  # -------------------------------------------------------
  # LOGOUTS
  # -------------------------------------------------------
  observeEvent(input$logout,{estado$usuario<-NULL;estado$pagina<-"login"})
  observeEvent(input$cliente_logout,{estado$usuario<-NULL;estado$pagina<-"login"})
  observeEvent(input$master_logout,{estado$usuario<-NULL;estado$pagina<-"login"})

  # -------------------------------------------------------
  # CADASTRO DE BARBEARIA PELO MASTER
  # -------------------------------------------------------
  observeEvent(input$cad_criar,{
    usuario <- trimws(input$cad_usuario)
    email <- trimws(input$cad_email)
    nome <- trimws(input$cad_nome)
    tel <- gsub("[^0-9]","",input$cad_telefone)
    s <- input$cad_senha
    c <- input$cad_confirmar

    if(usuario==""||email==""||nome==""||tel=="") {
      showNotification("Preencha os campos obrigatórios.",type="error"); return()
    }
    if(!validar_email(email)) {
      showNotification("E-mail inválido.",type="error"); return()
    }
    if(nchar(tel)<10||nchar(tel)>11) {
      showNotification("O telefone deve ter 10 ou 11 dígitos.",type="error"); return()
    }

    doc <- validar_cpf_cnpj(input$cad_cpf_cnpj, permitir_vazio=TRUE)
    if(!doc$ok) {
      showNotification(doc$msg,type="error"); return()
    }
    cpf_cnpj_novo <- doc$digitos

    er <- validar_senha(s)
    if(length(er)>0) {
      showNotification(paste(er,collapse="\n"),type="error",duration=8); return()
    }
    if(s!=c) {
      showNotification("As senhas não coincidem.",type="error"); return()
    }
    dup <- dbq("
      SELECT id FROM usuarios
      WHERE LOWER(nome_usuario)=LOWER(?) OR LOWER(email)=LOWER(?) LIMIT 1",
      params=list(usuario,email))
    if(nrow(dup)>0) {
      showNotification("Usuário ou e-mail já cadastrado.",type="error"); return()
    }

    liberada <- isTRUE(input$cad_liberar)
    slug_novo <- slug_unico(nome)
    dbWithTransaction(con,{
      dbe("
        INSERT INTO barbearias(nome,telefone,status_acesso,data_cadastro,nome_responsavel,cpf_cnpj,slug_publico)
        VALUES(?,?,?,?,?,?,?)",
        params=list(nome,tel,ifelse(liberada,"ATIVA","BLOQUEADA"),as.character(Sys.Date()),
                    trimws(input$cad_responsavel),cpf_cnpj_novo,slug_novo))
      bid <- db_last_id()
      dbe("
        INSERT INTO dados_barbearia(barbearia_id,nome_responsavel,cpf_cnpj,cep,endereco,
                                    numero,bairro,cidade,complemento)
        VALUES(?,?,?,?,?,?,?,?,?)",
        params=list(bid,trimws(input$cad_responsavel),cpf_cnpj_novo,
                    trimws(input$cad_cep),trimws(input$cad_endereco),trimws(input$cad_numero),
                    trimws(input$cad_bairro),trimws(input$cad_cidade),trimws(input$cad_complemento)))
      dbe("
        INSERT INTO assinaturas(barbearia_id,plano,valor_mensal,data_inicio,data_vencimento,status)
        VALUES(?,'MENSAL',?,?,?,?)",
        params=list(bid,input$cad_valor,as.character(Sys.Date()),
                    as.character(Sys.Date()+30),ifelse(liberada,"ATIVA","BLOQUEADA")))
      dbe("
        INSERT INTO usuarios(barbearia_id,cliente_id,nome_usuario,email,senha_hash,perfil,
                             ativo,troca_senha_obrigatoria,data_cadastro)
        VALUES(?,?,?, ?,?,'BARBEIRO',1,1,?)",
        params=list(bid,NA_integer_,usuario,email,hash_senha(s),as.character(Sys.Date())))

      # Registro da criação: serve como primeiro ponto da linha do tempo.
      registrar_historico(bid,"CRIACAO","BARBEARIA","nome","",nome)
      registrar_historico(bid,"CRIACAO","BARBEARIA","telefone","",tel)
      registrar_historico(bid,"CRIACAO","USUARIO_BARBEIRO","nome_usuario","",usuario)
      registrar_historico(bid,"CRIACAO","USUARIO_BARBEIRO","email","",email)
      registrar_historico(bid,"CRIACAO","DADOS_BARBEARIA","nome_responsavel","",trimws(input$cad_responsavel))
      registrar_historico(bid,"CRIACAO","DADOS_BARBEARIA","cpf_cnpj","",cpf_cnpj_novo)
      registrar_historico(bid,"CRIACAO","DADOS_BARBEARIA","cep","",trimws(input$cad_cep))
      registrar_historico(bid,"CRIACAO","DADOS_BARBEARIA","endereco","",trimws(input$cad_endereco))
      registrar_historico(bid,"CRIACAO","DADOS_BARBEARIA","numero","",trimws(input$cad_numero))
      registrar_historico(bid,"CRIACAO","DADOS_BARBEARIA","bairro","",trimws(input$cad_bairro))
      registrar_historico(bid,"CRIACAO","DADOS_BARBEARIA","cidade","",trimws(input$cad_cidade))
      registrar_historico(bid,"CRIACAO","DADOS_BARBEARIA","complemento","",trimws(input$cad_complemento))
      registrar_historico(bid,"CRIACAO","ASSINATURA","valor_mensal","",format(input$cad_valor,nsmall=2))
      registrar_historico(bid,"CRIACAO","ASSINATURA","data_inicio","",as.character(Sys.Date()))
      registrar_historico(bid,"CRIACAO","ASSINATURA","data_vencimento","",as.character(Sys.Date()+30))
      registrar_historico(bid,"CRIACAO","ASSINATURA","status","",ifelse(liberada,"ATIVA","BLOQUEADA"))

      garantir_servicos(as.integer(bid))
    })
    master_upd(master_upd()+1)
    showNotification("Barbearia e acesso do barbeiro criados.",type="message",duration=8)
  })

  # -------------------------------------------------------
  # MASTER - TABELA E AÇÕES
  # -------------------------------------------------------
  output$master_kpis <- renderUI({
    master_upd()
    x <- dbq("
      SELECT
        COUNT(*) unidades,
        SUM(CASE WHEN b.status_acesso='ATIVA' THEN 1 ELSE 0 END) unidades_ativas,
        (SELECT COUNT(*) FROM usuarios u WHERE u.perfil='BARBEIRO' AND u.ativo=1) barbeiros_ativos,
        COALESCE(SUM(CASE WHEN a.status='ATIVA' THEN a.valor_mensal ELSE 0 END),0) receita_mensal
      FROM barbearias b
      LEFT JOIN assinaturas a ON a.id=(
        SELECT MAX(a2.id) FROM assinaturas a2 WHERE a2.barbearia_id=b.id)")
    val <- function(n){
      z <- suppressWarnings(as.numeric(x[[n]][1]))
      if(length(z)==0 || is.na(z)) 0 else z
    }
    kpi <- function(classe,label,valor,nota)
      div(class=paste("master-kpi",classe),
          div(class="master-kpi-label",label),
          div(class="master-kpi-value",valor),
          div(class="master-kpi-note",nota))
    div(class="master-kpis",
        kpi("total","Unidades cadastradas",format(val("unidades"),big.mark=".",decimal.mark=","),"Total de barbearias na plataforma"),
        kpi("active","Contas ativas",format(val("unidades_ativas"),big.mark=".",decimal.mark=","),"Unidades com acesso liberado"),
        kpi("team","Barbeiros ativos",format(val("barbeiros_ativos"),big.mark=".",decimal.mark=","),"Profissionais com login ativo"),
        kpi("revenue","Receita mensal contratada",formata_reais(val("receita_mensal")),"Mensalidades de assinaturas ativas"))
  })

  output$master_unidade_resumo <- renderUI({
    master_upd()
    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)) return(NULL)
    x <- dbq("
      SELECT b.nome,b.status_acesso,a.status assinatura,a.valor_mensal,a.data_vencimento
      FROM barbearias b
      LEFT JOIN assinaturas a ON a.id=(
        SELECT MAX(a2.id) FROM assinaturas a2 WHERE a2.barbearia_id=b.id)
      WHERE b.id=? LIMIT 1",params=list(bid))
    if(nrow(x)==0) return(NULL)
    txt <- function(z,def="-") if(length(z)==0 || is.na(z) || trimws(as.character(z))=="") def else as.character(z)
    venc <- if(is.na(x$data_vencimento[1])) "-" else format(as.Date(x$data_vencimento[1]),"%d/%m/%Y")
    mensal <- formata_reais(ifelse(is.na(x$valor_mensal[1]),0,x$valor_mensal[1]))
    div(class="master-status-grid",
        div(class="master-status-item",div(class="master-status-label","Unidade"),div(class="master-status-value",txt(x$nome[1]))),
        div(class="master-status-item",div(class="master-status-label","Situação da conta"),div(class="master-status-value",txt(x$status_acesso[1]))),
        div(class="master-status-item",div(class="master-status-label","Status da assinatura"),div(class="master-status-value",txt(x$assinatura[1]))),
        div(class="master-status-item",div(class="master-status-label","Mensalidade / vencimento"),div(class="master-status-value",paste(mensal,"•",venc))))
  })

  output$master_equipe_resumo <- renderUI({
    master_upd()
    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)) return(NULL)
    x <- dbq("SELECT COUNT(*) total,SUM(CASE WHEN ativo=1 THEN 1 ELSE 0 END) ativos
              FROM usuarios WHERE barbearia_id=? AND perfil='BARBEIRO'",params=list(bid))
    total <- suppressWarnings(as.integer(x$total[1])); if(is.na(total)) total <- 0L
    ativos <- suppressWarnings(as.integer(x$ativos[1])); if(is.na(ativos)) ativos <- 0L
    inativos <- total-ativos
    div(style="display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px;",
        span(class="master-badge neutral",paste(total,"profissional(is)")),
        span(class="master-badge ok",paste(ativos,"ativo(s)")),
        span(class="master-badge warn",paste(inativos,"inativo(s)")))
  })

  output$master_tabela <- renderDT({
    master_upd()
    x <- dbq("
      SELECT b.id,b.nome,b.telefone,b.status_acesso,b.slug_publico,
             (SELECT COUNT(*) FROM usuarios u WHERE u.barbearia_id=b.id AND u.perfil='BARBEIRO') barbeiros,
             (SELECT COUNT(*) FROM usuarios u WHERE u.barbearia_id=b.id AND u.perfil='BARBEIRO' AND u.ativo=1) barbeiros_ativos,
             a.valor_mensal,a.data_vencimento,a.status AS assinatura
      FROM barbearias b
      LEFT JOIN assinaturas a ON a.id=(
        SELECT MAX(a2.id) FROM assinaturas a2 WHERE a2.barbearia_id=b.id)
      ORDER BY b.id DESC")
    if(nrow(x)>0) {
      x$link_publico <- vapply(seq_len(nrow(x)),function(i){
        if(is.na(x$slug_publico[i]) || trimws(as.character(x$slug_publico[i]))=="") return("-")
        link_publico_barbearia(session,as.integer(x$id[i]))
      },character(1))
      x$data_vencimento <- ifelse(is.na(x$data_vencimento),"-",
        format(as.Date(x$data_vencimento),"%d/%m/%Y"))
      x$valor_mensal <- ifelse(is.na(x$valor_mensal),0,x$valor_mensal)
      x$barbeiros <- ifelse(is.na(x$barbeiros),0,x$barbeiros)
      x$barbeiros_ativos <- ifelse(is.na(x$barbeiros_ativos),0,x$barbeiros_ativos)
      x$assinatura <- ifelse(is.na(x$assinatura),"-",x$assinatura)
      badge_master <- function(z){
        z <- as.character(z)
        cls <- ifelse(z=="ATIVA","ok",
               ifelse(z %in% c("BLOQUEADA","VENCIDA"),"warn",
               ifelse(z=="CANCELADA","bad","neutral")))
        paste0("<span class='master-badge ",cls,"'>",z,"</span>")
      }
      x$status_acesso <- vapply(x$status_acesso,badge_master,character(1))
      x$assinatura <- vapply(x$assinatura,badge_master,character(1))
    }
    names(x)<-c("ID","Unidade","Telefone","Situação da conta","Slug público","Link público",
                "Barbeiros","Barbeiros ativos","Mensalidade","Vencimento","Status da assinatura")
    datatable(x,rownames=FALSE,selection="single",
      options=list(pageLength=10,language=list(
        search="Pesquisar:",lengthMenu="Mostrar _MENU_ registros",
        info="Mostrando _START_ até _END_ de _TOTAL_ registros",
        zeroRecords="Nenhuma barbearia encontrada",emptyTable="Nenhuma barbearia cadastrada",
        paginate=list(first="Primeiro",last="Último",'next'="Próximo",previous="Anterior")))) |>
      formatCurrency(columns="Mensalidade",currency="R$ ",digits=2,mark=".",dec=",")
  })

  observe({
    # O seletor lateral é a fonte oficial. O seletor da tela "Barbearias"
    # acompanha automaticamente a mesma seleção.
    if(estado$pagina=="gestao" && !is.null(estado$usuario) &&
       estado$usuario$perfil[1]=="MASTER") {
      master_upd()
      x <- dbq("SELECT id,nome FROM barbearias ORDER BY nome")
      escolhas <- if(nrow(x)>0) setNames(x$id,x$nome) else character(0)

      atual <- suppressWarnings(as.integer(input$master_sel[1]))
      if(length(atual)==0 || is.na(atual) || !atual %in% x$id)
        atual <- if(nrow(x)>0) x$id[1] else NA_integer_

      updateSelectInput(session,"master_sel",choices=escolhas,
                        selected=if(is.na(atual)) character(0) else atual)
      updateSelectInput(session,"master_sel_lista",choices=escolhas,
                        selected=if(is.na(atual)) character(0) else atual)
    }
  })

  observeEvent(input$master_sel_lista,{
    if(estado$pagina=="gestao" && !is.null(estado$usuario) &&
       estado$usuario$perfil[1]=="MASTER") {
      bid <- suppressWarnings(as.integer(input$master_sel_lista[1]))
      if(length(bid)>0 && !is.na(bid))
        updateSelectInput(session,"master_sel",selected=bid)
    }
  },ignoreInit=TRUE)

  observeEvent(input$master_liberar,{
    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)) return()
    a <- dbq("SELECT id,data_vencimento,status FROM assinaturas
                         WHERE barbearia_id=? ORDER BY id DESC LIMIT 1",params=list(bid))
    if(nrow(a)==0)return()
    if(as.Date(a$data_vencimento[1])<Sys.Date()) {
      showNotification("Assinatura vencida. Renove primeiro.",type="error");return()
    }
    if(a$status[1]=="CANCELADA") {
      showNotification("Assinatura cancelada. Renove primeiro.",type="error");return()
    }
    status_acesso_antigo <- dbq("SELECT status_acesso FROM barbearias WHERE id=?",params=list(bid))$status_acesso[1]
    dbe("UPDATE assinaturas SET status='ATIVA' WHERE id=?",params=list(a$id[1]))
    dbe("UPDATE barbearias SET status_acesso='ATIVA' WHERE id=?",params=list(bid))
    registrar_historico(bid,"ALTERACAO","ASSINATURA","status",a$status[1],"ATIVA")
    registrar_historico(bid,"ALTERACAO","BARBEARIA","status_acesso",status_acesso_antigo,"ATIVA")
    master_upd(master_upd()+1)
    showNotification("Acesso liberado.",type="message")
  })

  observeEvent(input$master_bloquear,{
    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)) return()
    status_acesso_antigo <- dbq("SELECT status_acesso FROM barbearias WHERE id=?",params=list(bid))$status_acesso[1]
    a <- dbq("SELECT id,status FROM assinaturas WHERE barbearia_id=? ORDER BY id DESC LIMIT 1",params=list(bid))
    dbe("UPDATE barbearias SET status_acesso='BLOQUEADA' WHERE id=?",params=list(bid))
    if(nrow(a)>0) dbe("UPDATE assinaturas SET status='BLOQUEADA' WHERE id=?",params=list(a$id[1]))
    registrar_historico(bid,"ALTERACAO","BARBEARIA","status_acesso",status_acesso_antigo,"BLOQUEADA")
    if(nrow(a)>0) registrar_historico(bid,"ALTERACAO","ASSINATURA","status",a$status[1],"BLOQUEADA")
    master_upd(master_upd()+1)
    showNotification("Acesso bloqueado.",type="message")
  })

  observeEvent(input$master_cancelar,{
    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)) return()
    status_acesso_antigo <- dbq("SELECT status_acesso FROM barbearias WHERE id=?",params=list(bid))$status_acesso[1]
    a <- dbq("SELECT id,status FROM assinaturas WHERE barbearia_id=? ORDER BY id DESC LIMIT 1",params=list(bid))
    dbe("UPDATE barbearias SET status_acesso='CANCELADA' WHERE id=?",params=list(bid))
    if(nrow(a)>0) dbe("UPDATE assinaturas SET status='CANCELADA',data_cancelamento=? WHERE id=?",
                            params=list(as.character(Sys.Date()),a$id[1]))
    registrar_historico(bid,"ALTERACAO","BARBEARIA","status_acesso",status_acesso_antigo,"CANCELADA")
    if(nrow(a)>0) registrar_historico(bid,"ALTERACAO","ASSINATURA","status",a$status[1],"CANCELADA")
    master_upd(master_upd()+1)
    showNotification("Assinatura cancelada e acesso bloqueado.",type="message",duration=6)
  })

  observeEvent(input$master_renovar,{
    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)) return()
    a <- dbq("SELECT id,data_vencimento,status FROM assinaturas
                         WHERE barbearia_id=? ORDER BY id DESC LIMIT 1",params=list(bid))
    if(nrow(a)==0)return()
    nova <- max(Sys.Date(),as.Date(a$data_vencimento[1]))+30
    status_acesso_antigo <- dbq("SELECT status_acesso FROM barbearias WHERE id=?",params=list(bid))$status_acesso[1]
    dbe("UPDATE assinaturas SET data_vencimento=?,status='ATIVA',data_cancelamento=NULL
                WHERE id=?",params=list(as.character(nova),a$id[1]))
    dbe("UPDATE barbearias SET status_acesso='ATIVA' WHERE id=?",params=list(bid))
    registrar_historico(bid,"ALTERACAO","ASSINATURA","data_vencimento",a$data_vencimento[1],as.character(nova))
    registrar_historico(bid,"ALTERACAO","ASSINATURA","status",a$status[1],"ATIVA")
    registrar_historico(bid,"ALTERACAO","BARBEARIA","status_acesso",status_acesso_antigo,"ATIVA")
    master_upd(master_upd()+1)
    showNotification(paste("Renovada até",format(nova,"%d/%m/%Y")),type="message")
  })

  # -------------------------------------------------------
  # MASTER - EXCLUSÃO DEFINITIVA DE BARBEARIA
  # -------------------------------------------------------
  # A exclusão é propositalmente restrita ao MASTER e exige que o nome exato
  # da unidade seja digitado. O processo remove somente os dados vinculados
  # à barbearia escolhida; a conta MASTER não é vinculada a uma barbearia e
  # portanto nunca entra nos DELETEs abaixo.
  master_exclusao_alvo <- reactiveVal(NULL)

  observeEvent(input$master_excluir_barbearia,{
    req(!is.null(estado$usuario), estado$usuario$perfil[1]=="MASTER")

    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)) {
      showNotification("Selecione uma barbearia para excluir.",type="error")
      return()
    }

    b <- dbq("SELECT id,nome FROM barbearias WHERE id=?",params=list(bid))
    if(nrow(b)==0) {
      showNotification("A barbearia selecionada não existe mais.",type="error")
      master_upd(master_upd()+1)
      return()
    }

    master_exclusao_alvo(list(id=as.integer(b$id[1]),nome=as.character(b$nome[1])))

    showModal(modalDialog(
      title=tagList(icon("exclamation-triangle")," Excluir barbearia definitivamente"),
      div(
        class="alert alert-danger",
        tags$b("Atenção: esta ação não pode ser desfeita pelo sistema."),
        tags$br(),
        "Serão excluídos os dados desta unidade, incluindo barbeiros, clientes, cadastros online, serviços, atendimentos, horários, bloqueios, gastos, financeiro, notificações, assinatura e histórico."
      ),
      p("Unidade selecionada: ",tags$b(b$nome[1])),
      p("Para confirmar, digite exatamente o nome da unidade abaixo:"),
      textInput("master_excluir_confirmacao","Nome da unidade",value=""),
      footer=tagList(
        modalButton("Cancelar"),
        actionButton("master_excluir_confirmar","Excluir definitivamente",
                     icon=icon("trash"),class="btn-danger")
      ),
      easyClose=FALSE
    ))
  })

  observeEvent(input$master_excluir_confirmar,{
    req(!is.null(estado$usuario), estado$usuario$perfil[1]=="MASTER")

    alvo <- master_exclusao_alvo()
    if(is.null(alvo) || is.null(alvo$id) || is.null(alvo$nome)) {
      removeModal()
      showNotification("Não foi possível identificar a unidade a excluir.",type="error")
      return()
    }

    digitado <- trimws(as.character(input$master_excluir_confirmacao))
    if(!identical(digitado,trimws(as.character(alvo$nome)))) {
      showNotification("O nome digitado não corresponde exatamente ao nome da unidade.",type="error",duration=6)
      return()
    }

    # Revalida no banco imediatamente antes da exclusão. Isso impede que uma
    # seleção antiga da interface seja usada para apagar outra unidade.
    atual <- dbq("SELECT id,nome FROM barbearias WHERE id=?",params=list(as.integer(alvo$id)))
    if(nrow(atual)==0 || !identical(trimws(as.character(atual$nome[1])),trimws(as.character(alvo$nome)))) {
      removeModal()
      master_exclusao_alvo(NULL)
      showNotification("A unidade mudou ou já foi excluída. Operação cancelada.",type="error",duration=7)
      master_upd(master_upd()+1)
      return()
    }

    bid <- as.integer(alvo$id)

    ok <- tryCatch({
      dbWithTransaction(con,{
        # Filas e dados derivados/dependentes.
        dbe("DELETE FROM notificacoes WHERE barbearia_id=?",params=list(bid))
        dbe("DELETE FROM financeiro WHERE barbearia_id=?",params=list(bid))
        dbe("DELETE FROM historico_alteracoes WHERE barbearia_id=?",params=list(bid))
        dbe("DELETE FROM bloqueios_agenda WHERE barbearia_id=?",params=list(bid))
        dbe("DELETE FROM horarios_barbeiros WHERE barbearia_id=?",params=list(bid))
        dbe("DELETE FROM horarios_funcionamento WHERE barbearia_id=?",params=list(bid))

        # Tokens pertencentes aos usuários da unidade precisam sair antes
        # da remoção desses usuários por causa da FK de recuperacao_senha.
        dbe("DELETE r
             FROM recuperacao_senha r
             INNER JOIN usuarios u ON u.id=r.usuario_id
             WHERE u.barbearia_id=?",params=list(bid))

        # Atendimentos referenciam clientes/serviços.
        dbe("DELETE FROM atendimentos WHERE barbearia_id=?",params=list(bid))

        # clientes, cadastros_clientes e usuarios possuem referências entre si.
        # Primeiro quebramos apenas os vínculos DA UNIDADE que será excluída.
        dbe("UPDATE cadastros_clientes
             SET usuario_id=NULL,cliente_id=NULL
             WHERE barbearia_id=?",params=list(bid))
        dbe("UPDATE usuarios
             SET cliente_id=NULL
             WHERE barbearia_id=?",params=list(bid))
        dbe("UPDATE clientes
             SET cadastro_cliente_id=NULL
             WHERE barbearia_id=?",params=list(bid))

        # Agora as entidades podem ser removidas sem violar as FKs.
        dbe("DELETE FROM usuarios WHERE barbearia_id=?",params=list(bid))
        dbe("DELETE FROM clientes WHERE barbearia_id=?",params=list(bid))
        dbe("DELETE FROM cadastros_clientes WHERE barbearia_id=?",params=list(bid))

        dbe("DELETE FROM servicos WHERE barbearia_id=?",params=list(bid))
        dbe("DELETE FROM gastos WHERE barbearia_id=?",params=list(bid))
        dbe("DELETE FROM assinaturas WHERE barbearia_id=?",params=list(bid))
        dbe("DELETE FROM dados_barbearia WHERE barbearia_id=?",params=list(bid))

        # A unidade é sempre a última entidade removida.
        dbe("DELETE FROM barbearias WHERE id=?",params=list(bid))
      })
      TRUE
    },error=function(e){
      showNotification(
        paste0("A exclusão foi cancelada e revertida. Detalhe: ",conditionMessage(e)),
        type="error",duration=NULL
      )
      FALSE
    })

    if(!ok) return()

    removeModal()
    master_exclusao_alvo(NULL)

    # Atualiza todos os módulos que podem ter dados da unidade excluída.
    master_upd(master_upd()+1)
    clientes_upd(clientes_upd()+1)
    crm_upd(crm_upd()+1)
    notificacoes_upd(notificacoes_upd()+1)
    cadastros_upd(cadastros_upd()+1)
    servicos_upd(servicos_upd()+1)
    agenda_upd(agenda_upd()+1)
    bloqueios_upd(bloqueios_upd()+1)
    funcionamento_upd(funcionamento_upd()+1)
    jornada_upd(jornada_upd()+1)
    gastos_upd(gastos_upd()+1)

    showNotification(
      paste0("Barbearia “",alvo$nome,"” e todos os dados vinculados foram excluídos."),
      type="message",duration=8
    )
  })

  # -------------------------------------------------------
  # MASTER - CRIAR CONTA DE ACESSO DO BARBEIRO
  # -------------------------------------------------------
  # Permite criar o acesso de uma barbearia já existente. A criação de uma
  # nova barbearia continua criando seu primeiro BARBEIRO automaticamente.
  observeEvent(input$master_criar_barbeiro,{
    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)) {
      showNotification("Selecione uma barbearia primeiro.",type="error"); return()
    }

    barb <- dbq("SELECT id,nome FROM barbearias WHERE id=? LIMIT 1",params=list(bid))
    if(nrow(barb)==0) {
      showNotification("Barbearia não encontrada.",type="error"); return()
    }


    showModal(modalDialog(
      title=paste("Criar conta do barbeiro -",barb$nome[1]),
      textInput("novo_barbeiro_usuario","Nome de usuário *"),
      textInput("novo_barbeiro_email","E-mail *"),
      passwordInput("novo_barbeiro_senha","Senha inicial *"),
      passwordInput("novo_barbeiro_confirmar","Confirmar senha *"),
      p(class="small-help",
        "A senha inicial deverá ser trocada pelo barbeiro no primeiro acesso."),
      footer=tagList(
        modalButton("Cancelar"),
        actionButton("master_salvar_barbeiro","Criar conta",icon=icon("user-plus"),class="btn-primary")
      )
    ))
  })

  observeEvent(input$master_salvar_barbeiro,{
    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)) {
      showNotification("Selecione uma barbearia primeiro.",type="error"); return()
    }


    usuario <- trimws(as.character(input$novo_barbeiro_usuario %||% ""))
    email <- trimws(as.character(input$novo_barbeiro_email %||% ""))
    senha <- as.character(input$novo_barbeiro_senha %||% "")
    confirmar <- as.character(input$novo_barbeiro_confirmar %||% "")

    if(usuario=="" || email=="" || senha=="" || confirmar=="") {
      showNotification("Preencha todos os campos obrigatórios.",type="error"); return()
    }
    if(!validar_email(email)) {
      showNotification("Informe um e-mail válido.",type="error"); return()
    }

    # Nome de usuário e e-mail de BARBEIRO/MASTER são únicos no sistema.
    duplicado <- dbq("SELECT id FROM usuarios WHERE LOWER(nome_usuario)=LOWER(?) OR LOWER(email)=LOWER(?) LIMIT 1",
                     params=list(usuario,email))
    if(nrow(duplicado)>0) {
      showNotification("Nome de usuário ou e-mail já cadastrado.",type="error",duration=8); return()
    }

    erros <- validar_senha(senha)
    if(length(erros)>0) {
      showNotification(paste(erros,collapse="\n"),type="error",duration=8); return()
    }
    if(senha!=confirmar) {
      showNotification("As senhas não coincidem.",type="error"); return()
    }

    dbe("INSERT INTO usuarios(barbearia_id,cliente_id,nome_usuario,email,senha_hash,perfil,ativo,troca_senha_obrigatoria,data_cadastro) VALUES(?,NULL,?,?,?,'BARBEIRO',1,1,?)",
        params=list(bid,usuario,email,hash_senha(senha),as.character(Sys.Date())))

    registrar_historico(bid,"CRIACAO","USUARIO_BARBEIRO","nome_usuario","",usuario)
    registrar_historico(bid,"CRIACAO","USUARIO_BARBEIRO","email","",email)
    master_upd(master_upd()+1)
    removeModal()
    showNotification("Conta do barbeiro criada. Ele deverá trocar a senha no primeiro acesso.",
                     type="message",duration=8)
  })


  # Lista todas as contas de barbeiro da barbearia selecionada.
  output$master_tabela_barbeiros <- renderDT({
    master_upd()
    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)) return(datatable(data.frame(),rownames=FALSE))
    x <- dbq("SELECT id,nome_usuario,email,ativo,data_cadastro
              FROM usuarios
              WHERE barbearia_id=? AND perfil='BARBEIRO'
              ORDER BY nome_usuario,id",params=list(bid))
    if(nrow(x)>0) {
      x$ativo <- ifelse(x$ativo==1,
        "<span class='master-badge ok'>ATIVO</span>",
        "<span class='master-badge warn'>DESATIVADO</span>")
      x$data_cadastro <- format(as.Date(x$data_cadastro),"%d/%m/%Y")
    }
    names(x) <- c("ID","Profissional","E-mail","Status do login","Cadastro")
    datatable(x,rownames=FALSE,selection="single",
      options=list(pageLength=8,language=list(
        search="Pesquisar:",lengthMenu="Mostrar _MENU_ registros",
        info="Mostrando _START_ até _END_ de _TOTAL_ registros",
        zeroRecords="Nenhum barbeiro encontrado",emptyTable="Nenhum barbeiro cadastrado")))
  })

  observe({
    master_upd()
    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)) return()
    x <- dbq("SELECT id,nome_usuario,ativo FROM usuarios
              WHERE barbearia_id=? AND perfil='BARBEIRO'
              ORDER BY nome_usuario,id",params=list(bid))
    escolhas <- if(nrow(x)==0) character(0) else
      stats::setNames(as.character(x$id),
        paste0(x$nome_usuario,ifelse(x$ativo==1,""," (desativado)")))
    atual <- isolate(input$master_barbeiro_sel)
    sel <- if(!is.null(atual) && atual %in% escolhas) atual else
      if(length(escolhas)>0) escolhas[1] else character(0)
    updateSelectInput(session,"master_barbeiro_sel",choices=escolhas,selected=sel)
  })

  observeEvent(input$master_tabela_barbeiros_rows_selected,{
    bid <- master_id_selecionada()
    i <- input$master_tabela_barbeiros_rows_selected
    if(is.null(bid) || length(i)==0) return()
    x <- dbq("SELECT id FROM usuarios WHERE barbearia_id=? AND perfil='BARBEIRO'
              ORDER BY nome_usuario,id",params=list(bid))
    if(i[1] <= nrow(x))
      updateSelectInput(session,"master_barbeiro_sel",selected=as.character(x$id[i[1]]))
  })

  alterar_login_barbeiro <- function(ativo_novo) {
    bid <- master_id_selecionada()
    uid <- suppressWarnings(as.integer(input$master_barbeiro_sel))
    if(is.null(bid) || is.na(bid) || is.na(uid)) {
      showNotification("Selecione um barbeiro.",type="error"); return()
    }
    u <- dbq("SELECT id,nome_usuario,ativo FROM usuarios
              WHERE id=? AND barbearia_id=? AND perfil='BARBEIRO' LIMIT 1",
             params=list(uid,bid))
    if(nrow(u)==0) {showNotification("Barbeiro não encontrado.",type="error");return()}
    dbe("UPDATE usuarios SET ativo=? WHERE id=? AND barbearia_id=? AND perfil='BARBEIRO'",
        params=list(as.integer(ativo_novo),uid,bid))
    registrar_historico(bid,"ALTERACAO","USUARIO_BARBEIRO","ativo",
                         as.character(u$ativo[1]),as.character(as.integer(ativo_novo)))
    master_upd(master_upd()+1)
    showNotification(if(ativo_novo==1)
      paste("Login de",u$nome_usuario[1],"ativado.")
      else paste("Login de",u$nome_usuario[1],"desativado."),
      type="message",duration=6)
  }

  observeEvent(input$master_ativar_barbeiro,{ alterar_login_barbeiro(1) })
  observeEvent(input$master_desativar_barbeiro,{ alterar_login_barbeiro(0) })

  observeEvent(input$master_senha,{
    bid <- master_id_selecionada()
    uid <- suppressWarnings(as.integer(input$master_barbeiro_sel))
    if(is.null(bid) || is.na(bid) || is.na(uid)){
      showNotification("Selecione um barbeiro primeiro.",type="error"); return()
    }
    b <- dbq("SELECT id,nome_usuario FROM usuarios
              WHERE id=? AND barbearia_id=? AND perfil='BARBEIRO' LIMIT 1",
             params=list(uid,bid))
    if(nrow(b)==0){showNotification("Barbeiro não encontrado.",type="error");return()}
    showModal(modalDialog(
      title=paste("Redefinir senha de",b$nome_usuario[1]),
      passwordInput("master_nova","Nova senha"),
      passwordInput("master_conf","Confirmar senha"),
      footer=tagList(modalButton("Cancelar"),
                      actionButton("master_salvar","Salvar",icon=icon("save"),class="btn-primary"))))
  })

  observeEvent(input$master_salvar,{
    bid <- master_id_selecionada()
    uid <- suppressWarnings(as.integer(input$master_barbeiro_sel))
    if(is.null(bid) || is.na(bid) || is.na(uid)) return()
    er <- validar_senha(input$master_nova)
    if(length(er)>0){showNotification(paste(er,collapse="
"),type="error",duration=8);return()}
    if(input$master_nova!=input$master_conf){showNotification("As senhas não coincidem.",type="error");return()}
    b <- dbq("SELECT id FROM usuarios
              WHERE id=? AND barbearia_id=? AND perfil='BARBEIRO' LIMIT 1",
             params=list(uid,bid))
    if(nrow(b)==0)return()
    dbe("UPDATE usuarios SET senha_hash=?,troca_senha_obrigatoria=1 WHERE id=?",
        params=list(hash_senha(input$master_nova),b$id[1]))
    registrar_historico(bid,"REDEFINICAO_SENHA","USUARIO_BARBEIRO","senha",
                        "PROTEGIDO","PROTEGIDO")
    removeModal()
    showNotification("Senha redefinida. O barbeiro deverá trocá-la no próximo acesso.",
                     type="message",duration=8)
  })

  # -------------------------------------------------------
  # MASTER - EDITAR DADOS DA BARBEARIA
  # -------------------------------------------------------
  observeEvent(input$master_editar,{
    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)){
      showNotification("Selecione uma barbearia primeiro.",type="error"); return()
    }

    b <- dbq("SELECT nome,telefone FROM barbearias WHERE id=? LIMIT 1",
                    params=list(bid))
    d <- dbq("SELECT nome_responsavel,cpf_cnpj,cep,endereco,numero,bairro,cidade,complemento
                         FROM dados_barbearia WHERE barbearia_id=? LIMIT 1",
                    params=list(bid))
    a <- dbq("SELECT valor_mensal,data_vencimento FROM assinaturas
                         WHERE barbearia_id=? ORDER BY id DESC LIMIT 1",
                    params=list(bid))

    if(nrow(b)==0){
      showNotification("Barbearia não encontrada.",type="error"); return()
    }

    getv <- function(df,col,default="") {
      if(nrow(df)==0 || is.null(df[[col]]) || is.na(df[[col]][1])) default else as.character(df[[col]][1])
    }

    showModal(modalDialog(
      title=paste("Editar barbearia:",b$nome[1]),
      size="l",
      fluidRow(
        column(6,
               textInput("edit_nome_barbearia","Nome da barbearia *",value=getv(b,"nome")),
               textInput("edit_telefone","Telefone *",value=getv(b,"telefone")),
               numericInput("edit_valor_mensal","Valor mensal",
                            value=if(nrow(a)>0 && !is.na(a$valor_mensal[1]))a$valor_mensal[1] else 0,
                            min=0,step=.01)),
        column(6,
               textInput("edit_responsavel","Nome do responsável",value=getv(d,"nome_responsavel")),
               textInput("edit_cpf_cnpj","CPF/CNPJ",value=formatar_cpf_cnpj(getv(d,"cpf_cnpj")),placeholder="CPF: 333.333.333-90 | CNPJ: 33.333.333/3333-90"),
               textInput("edit_cep","CEP",value=getv(d,"cep")),
               textInput("edit_endereco","Endereço",value=getv(d,"endereco")),
               textInput("edit_numero","Número",value=getv(d,"numero")),
               textInput("edit_bairro","Bairro",value=getv(d,"bairro")),
               textInput("edit_cidade","Cidade",value=getv(d,"cidade")),
               textInput("edit_complemento","Complemento",value=getv(d,"complemento"))),
        column(12,
               dateInput("edit_vencimento","Vencimento da assinatura",
                         value=if(nrow(a)>0 && !is.na(a$data_vencimento[1]))as.Date(a$data_vencimento[1]) else Sys.Date(),
                         format="dd/mm/yyyy",language="pt-BR"))
      ),
      footer=tagList(
        modalButton("Cancelar"),
        actionButton("master_salvar_edicao","Salvar alterações",icon=icon("save"),class="btn-primary")
      ),
      easyClose=FALSE
    ))
  })

  observeEvent(input$master_salvar_edicao,{
    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)){
      showNotification("Barbearia não selecionada.",type="error"); return()
    }

    nome_novo <- trimws(input$edit_nome_barbearia)
    telefone_novo <- gsub("[^0-9]","",trimws(input$edit_telefone))
    valor_novo <- if(is.null(input$edit_valor_mensal)||is.na(input$edit_valor_mensal)) 0 else as.numeric(input$edit_valor_mensal)
    venc_novo <- as.character(input$edit_vencimento)

    if(nome_novo==""||telefone_novo==""){
      showNotification("Preencha os campos obrigatórios.",type="error"); return()
    }
    if(nchar(telefone_novo)<10 || nchar(telefone_novo)>11){
      showNotification("O telefone deve ter 10 ou 11 dígitos.",type="error"); return()
    }
    if(is.na(input$edit_vencimento)){
      showNotification("Informe um vencimento válido.",type="error"); return()
    }

    doc <- validar_cpf_cnpj(input$edit_cpf_cnpj, permitir_vazio=TRUE)
    if(!doc$ok) {
      showNotification(doc$msg,type="error"); return()
    }
    cpf_cnpj_novo <- doc$digitos

    anterior_b <- dbq("SELECT nome,telefone FROM barbearias WHERE id=? LIMIT 1",params=list(bid))
    anterior_d <- dbq("SELECT nome_responsavel,cpf_cnpj,cep,endereco,numero,bairro,cidade,complemento FROM dados_barbearia WHERE barbearia_id=? LIMIT 1",params=list(bid))
    anterior_a <- dbq("SELECT id,valor_mensal,data_vencimento FROM assinaturas WHERE barbearia_id=? ORDER BY id DESC LIMIT 1",params=list(bid))

    dbWithTransaction(con,{
      if(nrow(anterior_b)>0){
        registrar_historico(bid,"ALTERACAO","BARBEARIA","nome",anterior_b$nome[1],nome_novo)
        registrar_historico(bid,"ALTERACAO","BARBEARIA","telefone",anterior_b$telefone[1],telefone_novo)
      }
      if(nrow(anterior_d)>0){
        novos_d <- list(
          nome_responsavel=trimws(input$edit_responsavel),
          cpf_cnpj=cpf_cnpj_novo,
          cep=trimws(input$edit_cep),
          endereco=trimws(input$edit_endereco),
          numero=trimws(input$edit_numero),
          bairro=trimws(input$edit_bairro),
          cidade=trimws(input$edit_cidade),
          complemento=trimws(input$edit_complemento)
        )
        antigos_d <- list(
          nome_responsavel=anterior_d$nome_responsavel[1],
          cpf_cnpj=anterior_d$cpf_cnpj[1],
          cep=anterior_d$cep[1],
          endereco=anterior_d$endereco[1],
          numero=anterior_d$numero[1],
          bairro=anterior_d$bairro[1],
          cidade=anterior_d$cidade[1],
          complemento=anterior_d$complemento[1]
        )
        for(campo in names(novos_d)) registrar_historico(bid,"ALTERACAO","DADOS_BARBEARIA",campo,antigos_d[[campo]],novos_d[[campo]])
      }
      if(nrow(anterior_a)>0){
        registrar_historico(bid,"ALTERACAO","ASSINATURA","valor_mensal",format(anterior_a$valor_mensal[1],nsmall=2),format(valor_novo,nsmall=2))
        registrar_historico(bid,"ALTERACAO","ASSINATURA","data_vencimento",anterior_a$data_vencimento[1],venc_novo)
      }

      dbe("UPDATE barbearias SET nome=?,telefone=?,nome_responsavel=?,cpf_cnpj=? WHERE id=?",
                params=list(nome_novo,telefone_novo,trimws(input$edit_responsavel),cpf_cnpj_novo,bid))
      dbe("
        UPDATE dados_barbearia SET nome_responsavel=?,cpf_cnpj=?,cep=?,endereco=?,
        numero=?,bairro=?,cidade=?,complemento=? WHERE barbearia_id=?",
        params=list(trimws(input$edit_responsavel),cpf_cnpj_novo,
                    trimws(input$edit_cep),trimws(input$edit_endereco),trimws(input$edit_numero),
                    trimws(input$edit_bairro),trimws(input$edit_cidade),trimws(input$edit_complemento),bid))
      if(nrow(anterior_a)>0)
        dbe("UPDATE assinaturas SET valor_mensal=?,data_vencimento=? WHERE id=?",
                  params=list(valor_novo,venc_novo,anterior_a$id[1]))
    })

    master_upd(master_upd()+1)
    removeModal()
    showNotification("Dados da barbearia atualizados e registrados no histórico.",type="message",duration=7)
  })

  # -------------------------------------------------------
  # MASTER - VER DADOS COMPLETOS DA BARBEARIA
  # -------------------------------------------------------
  observeEvent(input$master_dados,{
    bid <- master_id_selecionada()
    if(is.null(bid) || is.na(bid)) return()

    b <- dbq("SELECT nome,telefone,slug_publico FROM barbearias WHERE id=? LIMIT 1",params=list(bid))
    d <- dbq("SELECT nome_responsavel,cpf_cnpj,cep,endereco,numero,bairro,cidade,complemento
                         FROM dados_barbearia WHERE barbearia_id=? LIMIT 1",params=list(bid))
    if(nrow(b)==0)return()

    if(nrow(d)==0) d <- data.frame(
      nome_responsavel="",cpf_cnpj="",cep="",endereco="",numero="",
      bairro="",cidade="",complemento="",stringsAsFactors=FALSE)

    showModal(modalDialog(
      title=paste("Dados da barbearia:",b$nome[1]),
      h4("Identificação"),
      p(strong("Nome: "),b$nome[1]),
      p(strong("Telefone: "),ifelse(is.na(b$telefone[1]),"",b$telefone[1])),
      p(strong("Slug público: "),ifelse(is.na(b$slug_publico[1]),"",b$slug_publico[1])),
      p(strong("Link público: "),link_publico_barbearia(session,bid)),
      h4("Dados cadastrais"),
      p(strong("Responsável: "),ifelse(is.na(d$nome_responsavel[1]),"",d$nome_responsavel[1])),
      p(strong("CPF/CNPJ: "),formatar_cpf_cnpj(d$cpf_cnpj[1])),
      p(strong("CEP: "),ifelse(is.na(d$cep[1]),"",d$cep[1])),
      p(strong("Endereço: "),paste(
        ifelse(is.na(d$endereco[1]),"",d$endereco[1]),
        ifelse(is.na(d$numero[1]),"",d$numero[1])))
      ,
      p(strong("Bairro: "),ifelse(is.na(d$bairro[1]),"",d$bairro[1])),
      p(strong("Cidade: "),ifelse(is.na(d$cidade[1]),"",d$cidade[1])),
      p(strong("Complemento: "),ifelse(is.na(d$complemento[1]),"",d$complemento[1])),
      easyClose=TRUE,
      footer=modalButton("Fechar")
    ))
  })

  # -------------------------------------------------------
  # SERVIÇOS DA AGENDA
  # -------------------------------------------------------
  atualizar_servicos_input <- function() {
    bid <- id_barb()
    if(is.null(bid))return()
    x <- dbq("SELECT id,nome FROM servicos
                         WHERE barbearia_id=? AND status='ATIVO' ORDER BY id",
                    params=list(bid))
    updateSelectInput(session,"agenda_servico",
                      choices=if(nrow(x)>0)setNames(x$id,x$nome) else character(0),
                      selected=if(nrow(x)>0)x$id[1] else character(0))
  }

  output$agenda_preco <- renderText({
    bid <- id_barb(); sid <- input$agenda_servico
    if(is.null(bid)||is.null(sid)||sid=="")return("Selecione um serviço")
    x <- dbq("SELECT preco FROM servicos
                         WHERE id=? AND barbearia_id=? AND status='ATIVO' LIMIT 1",
                    params=list(as.integer(sid),bid))
    if(nrow(x)==0)"Serviço não encontrado" else formata_reais(x$preco[1])
  })

  output$agenda_duracao <- renderText({
    bid <- id_barb(); sid <- input$agenda_servico
    if(is.null(bid)||is.null(sid)||sid=="")return("Selecione um serviço")
    x <- dbq("SELECT duracao_minutos FROM servicos
                         WHERE id=? AND barbearia_id=? AND status='ATIVO' LIMIT 1",
                    params=list(as.integer(sid),bid))
    if(nrow(x)==0)"Serviço não encontrado" else paste0(x$duracao_minutos[1]," minutos")
  })

  # -------------------------------------------------------
  # CLIENTE: criar/vincular acesso e cadastro público
  # -------------------------------------------------------
  criar_acesso_cliente <- function(cliente_id,nome,email,bid,forcar_reenvio=FALSE) {
    email <- trimws(email)
    if(!validar_email(email))
      return(list(ok=FALSE,msg="O e-mail do cliente é inválido.",enviado=FALSE))

    # Primeiro procura a conta já vinculada ao cliente.
    u <- dbq("
      SELECT id,senha_hash,cliente_id FROM usuarios
      WHERE cliente_id=? AND barbearia_id=? AND perfil='CLIENTE' LIMIT 1",
      params=list(cliente_id,bid))

    # Se não houver, procura uma conta criada previamente pela página pública.
    if(nrow(u)==0) {
      u <- dbq("
        SELECT id,senha_hash,cliente_id FROM usuarios
        WHERE barbearia_id=? AND LOWER(email)=LOWER(?) AND perfil='CLIENTE' LIMIT 1",
        params=list(bid,email))
    }

    # Mantém a restrição de e-mail dentro da própria barbearia.
    outro <- dbq("
      SELECT id FROM usuarios
      WHERE barbearia_id=? AND LOWER(email)=LOWER(?) AND perfil='CLIENTE'
        AND id<>? LIMIT 1",
      params=list(bid,email,if(nrow(u)>0) u$id[1] else -1L))
    if(nrow(outro)>0)
      return(list(ok=FALSE,msg="Esse e-mail já pertence a outra conta nesta barbearia.",enviado=FALSE))

    cad <- dbq("
      SELECT id,usuario_id,status FROM cadastros_clientes
      WHERE barbearia_id=? AND LOWER(email)=LOWER(?) LIMIT 1",
      params=list(bid,email))

    if(nrow(u)==0) {
      nome_usuario <- if(nrow(cad)>0) paste0("cliente_",cad$id[1]) else paste0("cliente_",cliente_id)
      dbe("
        INSERT INTO usuarios(barbearia_id,cliente_id,nome_usuario,email,senha_hash,perfil,
                             ativo,troca_senha_obrigatoria,data_cadastro)
        VALUES(?,?,?, ?,NULL,'CLIENTE',1,1,?)",
        params=list(bid,cliente_id,nome_usuario,email,as.character(Sys.Date())))
      uid <- db_last_id()
      u <- dbq("SELECT id,senha_hash,cliente_id FROM usuarios WHERE id=?",params=list(uid))

      if(nrow(cad)>0)
        dbe("UPDATE cadastros_clientes SET usuario_id=?,cliente_id=?,status='CONVERTIDO',data_conversao=? WHERE id=?",
                  params=list(uid,cliente_id,as.character(Sys.Date()),cad$id[1]))
    } else {
      uid <- u$id[1]
      dbe("UPDATE usuarios SET barbearia_id=?,cliente_id=?,email=?,ativo=1 WHERE id=?",
                params=list(bid,cliente_id,email,uid))
      if(nrow(cad)>0)
        dbe("UPDATE cadastros_clientes SET usuario_id=?,cliente_id=?,status='CONVERTIDO',data_conversao=? WHERE id=?",
                  params=list(uid,cliente_id,as.character(Sys.Date()),cad$id[1]))
      senha_ja_definida <- !is.na(u$senha_hash[1]) && nzchar(u$senha_hash[1])
      if(senha_ja_definida && !forcar_reenvio)
        return(list(ok=TRUE,enviado=FALSE,msg="O acesso do cliente já está ativo."))
    }

    dbe("UPDATE clientes SET cadastro_cliente_id=? WHERE id=? AND barbearia_id=?",
              params=list(if(nrow(cad)>0) cad$id[1] else NA_integer_,cliente_id,bid))

    dbe("UPDATE recuperacao_senha SET usado=1 WHERE usuario_id=? AND usado=0",
              params=list(uid))
    token <- gerar_token()
    dbe("
      INSERT INTO recuperacao_senha(usuario_id,token_hash,tipo,expira_em,usado,criado_em)
      VALUES(?,?,'ATIVACAO',datetime('now','+24 hours'),0,datetime('now'))",
      params=list(uid,hash_token(token)))

    barb <- dbq("SELECT nome FROM barbearias WHERE id=? LIMIT 1",params=list(bid))
    link <- paste0(get_app_url(session),"?token=",token)
    corpo <- paste0(
      "Olá ",nome,"!\n\n",
      "Seu acesso à ",barb$nome[1]," foi criado.\n\n",
      "[Ativar minha conta e criar senha](",link,")\n\n",
      "O link é válido por 24 horas e só pode ser usado uma vez."
    )
    envio <- enviar_email(session,email,paste("Acesso à",barb$nome[1]),corpo)
    list(ok=TRUE,enviado=envio$ok,msg=if(envio$ok)
      "Acesso criado e e-mail enviado."
      else paste("Acesso criado, mas o e-mail não foi enviado:",envio$msg))
  }

  # -------------------------------------------------------
  # BARBEIROS DISPONÍVEIS NA AGENDA
  # -------------------------------------------------------
  atualizar_barbeiros_agenda <- function() {
    bid <- id_barb()
    if(is.null(bid)) return(invisible(NULL))
    x <- dbq("SELECT id,nome_usuario FROM usuarios
              WHERE barbearia_id=? AND perfil='BARBEIRO' AND ativo=1
              ORDER BY nome_usuario,id",params=list(bid))
    escolhas <- if(nrow(x)==0) character(0) else
      stats::setNames(as.character(x$id),as.character(x$nome_usuario))
    updateSelectInput(session,"agenda_barbeiro",choices=escolhas,
                      selected=if(length(escolhas)>0) escolhas[1] else character(0))
    updateSelectInput(session,"bloqueio_alvo",
                      choices=c("Toda a barbearia"="TODOS",escolhas),
                      selected="TODOS")

    perfil_atual <- if(!is.null(estado$usuario$perfil)) as.character(estado$usuario$perfil[1]) else ""
    if(perfil_atual=="MASTER"){
      todos <- dbq("SELECT id,nome_usuario,ativo FROM usuarios
                    WHERE barbearia_id=? AND perfil='BARBEIRO'
                    ORDER BY ativo DESC,nome_usuario,id",params=list(bid))
      if(nrow(todos)>0){
        labs <- ifelse(todos$ativo==1,todos$nome_usuario,
                       paste0(todos$nome_usuario," (desativado)"))
        jornada_choices <- stats::setNames(as.character(todos$id),labs)
      } else jornada_choices <- character(0)
    } else if(perfil_atual=="BARBEIRO"){
      uid <- as.integer(estado$usuario$id[1])
      nm <- as.character(estado$usuario$nome_usuario[1])
      jornada_choices <- stats::setNames(as.character(uid),nm)
    } else jornada_choices <- character(0)

    atual_jornada <- isolate(input$jornada_barbeiro)
    sel_jornada <- if(length(atual_jornada)>0 && atual_jornada %in% unname(jornada_choices))
      atual_jornada else if(length(jornada_choices)>0) jornada_choices[1] else character(0)
    updateSelectInput(session,"jornada_barbeiro",
                      choices=jornada_choices,selected=sel_jornada)
    filtro <- c("Todos os barbeiros"="TODOS",escolhas)
    # Para o barbeiro logado, deixa o próprio nome disponível sem esconder os demais.
    selecionado <- "TODOS"
    updateSelectInput(session,"agenda_filtro_barbeiro",choices=filtro,selected=selecionado)
    invisible(NULL)
  }

  observeEvent(list(estado$pagina,input$master_sel,input$master_sel_lista,master_upd()),{
    if(estado$pagina=="gestao") atualizar_barbeiros_agenda()
  },ignoreInit=FALSE)

  # -------------------------------------------------------
  # HORÁRIO DE FUNCIONAMENTO DA BARBEARIA
  # -------------------------------------------------------
  usuario_auditoria_nome <- function() {
    u <- estado$usuario
    if(is.null(u)) return(NA_character_)
    if(!is.null(u$nome_usuario) && length(u$nome_usuario)>0 &&
       !is.na(u$nome_usuario[1]) && nzchar(trimws(as.character(u$nome_usuario[1]))))
      return(as.character(u$nome_usuario[1]))
    if(!is.null(u$usuario) && length(u$usuario)>0 &&
       !is.na(u$usuario[1]) && nzchar(trimws(as.character(u$usuario[1]))))
      return(as.character(u$usuario[1]))
    if(!is.null(u$email) && length(u$email)>0 &&
       !is.na(u$email[1]) && nzchar(trimws(as.character(u$email[1]))))
      return(as.character(u$email[1]))
    NA_character_
  }

  dias_funcionamento <- data.frame(
    dia_semana=1:7,
    nome_dia=c("Segunda","Terça","Quarta","Quinta","Sexta","Sábado","Domingo"),
    stringsAsFactors=FALSE
  )

  dia_semana_num <- function(data) {
    # as.POSIXlt$wday: 0=domingo ... 6=sábado.
    w <- as.POSIXlt(as.Date(data))$wday
    ifelse(w==0,7,w)
  }

  garantir_horarios_funcionamento <- function(bid) {
    b <- dbq("SELECT nome FROM barbearias WHERE id=? LIMIT 1",params=list(bid))
    if(nrow(b)==0) return(invisible(FALSE))
    for(i in seq_len(nrow(dias_funcionamento))) {
      d <- dias_funcionamento[i,]
      ex <- dbq("SELECT id FROM horarios_funcionamento
                 WHERE barbearia_id=? AND dia_semana=? LIMIT 1",
                params=list(bid,d$dia_semana))
      if(nrow(ex)==0) {
        dbe("INSERT INTO horarios_funcionamento(
               barbearia_id,nome_barbearia,dia_semana,nome_dia,aberto,
               hora_abertura,hora_fechamento,atualizado_por_usuario_id,
               atualizado_por_usuario,atualizado_por_perfil,atualizado_em)
             VALUES(?,?,?,?,1,CAST('00:00' AS TIME),CAST('23:59' AS TIME),NULL,NULL,'MIGRACAO',?)",
            params=list(bid,as.character(b$nome[1]),d$dia_semana,d$nome_dia,
                        format(Sys.time(),"%Y-%m-%d %H:%M:%S")))
      }
    }
    invisible(TRUE)
  }

  funcionamento_dia <- function(bid,data) {
    garantir_horarios_funcionamento(bid)
    ds <- dia_semana_num(data)
    dbq("SELECT TOP 1 id,nome_barbearia,dia_semana,nome_dia,aberto,
                hora_abertura,hora_fechamento
         FROM horarios_funcionamento
         WHERE barbearia_id=? AND dia_semana=?",
        params=list(bid,ds))
  }

  horario_dentro_funcionamento <- function(bid,data,hora) {
    h <- funcionamento_dia(bid,data)
    if(nrow(h)==0 || is.na(h$aberto[1]) || as.integer(h$aberto[1])!=1) return(FALSE)
    ab <- substr(as.character(h$hora_abertura[1]),1,5)
    fe <- substr(as.character(h$hora_fechamento[1]),1,5)
    if(is.na(ab) || is.na(fe) || !nzchar(ab) || !nzchar(fe)) return(FALSE)
    hora >= ab && hora < fe
  }

  output$ui_horarios_funcionamento <- renderUI({
    bid <- id_barb(); funcionamento_upd()
    if(is.null(bid)) return(NULL)
    garantir_horarios_funcionamento(bid)
    h <- dbq("SELECT dia_semana,nome_dia,aberto,hora_abertura,hora_fechamento
              FROM horarios_funcionamento WHERE barbearia_id=?
              ORDER BY dia_semana",params=list(bid))

    op_ab <- c(sprintf("%02d:%02d",rep(0:23,each=2),rep(c(0,30),24)))
    op_fe <- unique(c(op_ab[-1],"23:59"))

    tagList(
      div(class="week-grid-head",
          div("Dia / situação"),div("Abertura"),div("Fechamento"),div("Status")),
      lapply(seq_len(nrow(dias_funcionamento)),function(i){
        d <- dias_funcionamento[i,]
        r <- h[h$dia_semana==d$dia_semana,,drop=FALSE]
        aberto <- if(nrow(r)==0) TRUE else as.integer(r$aberto[1])==1
        ha <- if(nrow(r)==0 || is.na(r$hora_abertura[1])) "00:00" else substr(as.character(r$hora_abertura[1]),1,5)
        hf <- if(nrow(r)==0 || is.na(r$hora_fechamento[1])) "23:59" else substr(as.character(r$hora_fechamento[1]),1,5)
        div(class=paste("week-row",if(!aberto) "is-closed" else ""),
            div(class="week-status",
                div(class="week-day",d$nome_dia),
                checkboxInput(paste0("func_aberto_",d$dia_semana),"Aberto",value=aberto)),
            selectInput(paste0("func_abertura_",d$dia_semana),NULL,choices=op_ab,selected=ha),
            selectInput(paste0("func_fechamento_",d$dia_semana),NULL,choices=op_fe,selected=hf),
            div(if(aberto) span(class="week-badge","Em funcionamento")
                else span(class="week-badge","Fechado"))
        )
      })
    )
  })

  observeEvent(input$salvar_horarios_funcionamento,{
    bid <- id_barb()
    if(is.null(bid)) return()
    b <- dbq("SELECT nome FROM barbearias WHERE id=? LIMIT 1",params=list(bid))
    if(nrow(b)==0){showNotification("Barbearia não encontrada.",type="error");return()}

    dados <- lapply(seq_len(nrow(dias_funcionamento)),function(i){
      d <- dias_funcionamento[i,]
      aberto <- isTRUE(input[[paste0("func_aberto_",d$dia_semana)]])
      ha <- as.character(input[[paste0("func_abertura_",d$dia_semana)]] %||% "")
      hf <- as.character(input[[paste0("func_fechamento_",d$dia_semana)]] %||% "")
      if(aberto && (!grepl("^[0-9]{2}:[0-9]{2}$",ha) ||
                    !grepl("^[0-9]{2}:[0-9]{2}$",hf) || hf<=ha))
        stop(paste0(d$nome_dia,": o fechamento deve ser posterior à abertura."))
      list(d=d,aberto=aberto,ha=ha,hf=hf)
    })

    tryCatch({
      ator_id <- if(!is.null(estado$usuario$id)) as.integer(estado$usuario$id[1]) else NA_integer_
      ator_nome <- usuario_auditoria_nome()
      ator_perfil <- if(!is.null(estado$usuario$perfil)) as.character(estado$usuario$perfil[1]) else NA_character_
      agora <- format(Sys.time(),"%Y-%m-%d %H:%M:%S")

      dbWithTransaction(con,{
        for(z in dados) {
          ex <- dbq("SELECT id FROM horarios_funcionamento
                     WHERE barbearia_id=? AND dia_semana=? LIMIT 1",
                    params=list(bid,z$d$dia_semana))
          ha_db <- if(z$aberto) z$ha else NA_character_
          hf_db <- if(z$aberto) z$hf else NA_character_
          if(nrow(ex)>0) {
            dbe("UPDATE horarios_funcionamento
                 SET nome_barbearia=?,nome_dia=?,aberto=?,hora_abertura=?,hora_fechamento=?,
                     atualizado_por_usuario_id=?,atualizado_por_usuario=?,
                     atualizado_por_perfil=?,atualizado_em=?
                 WHERE id=? AND barbearia_id=?",
                params=list(as.character(b$nome[1]),z$d$nome_dia,as.integer(z$aberto),
                            ha_db,hf_db,ator_id,ator_nome,ator_perfil,agora,ex$id[1],bid))
          } else {
            dbe("INSERT INTO horarios_funcionamento(
                   barbearia_id,nome_barbearia,dia_semana,nome_dia,aberto,
                   hora_abertura,hora_fechamento,atualizado_por_usuario_id,
                   atualizado_por_usuario,atualizado_por_perfil,atualizado_em)
                 VALUES(?,?,?,?,?,?,?,?,?,?,?)",
                params=list(bid,as.character(b$nome[1]),z$d$dia_semana,z$d$nome_dia,
                            as.integer(z$aberto),ha_db,hf_db,ator_id,ator_nome,ator_perfil,agora))
          }
        }
      })

      registrar_historico(bid,"ALTERACAO","HORARIO_FUNCIONAMENTO","semana","",
                          "Horários semanais atualizados")
      funcionamento_upd(funcionamento_upd()+1)
      jornada_upd(jornada_upd()+1)

      invalidas <- dbq("
        SELECT COUNT(*) qtd
        FROM horarios_barbeiros j
        INNER JOIN horarios_funcionamento h
          ON h.barbearia_id=j.barbearia_id AND h.dia_semana=j.dia_semana
        WHERE j.barbearia_id=? AND j.trabalha=1
          AND (h.aberto=0 OR j.hora_inicio<h.hora_abertura OR j.hora_fim>h.hora_fechamento)",
        params=list(bid))
      if(nrow(invalidas)>0 && as.integer(invalidas$qtd[1])>0)
        showNotification("Funcionamento salvo. Há jornadas de barbeiros fora do novo horário; ajuste-as antes de novos agendamentos nesses períodos.",
                         type="warning",duration=10)
      else
        showNotification("Horários de funcionamento atualizados.",type="message")
    },error=function(e){
      showNotification(paste("Não foi possível salvar:",conditionMessage(e)),type="error",duration=8)
    })
  })

  # -------------------------------------------------------
  # JORNADA INDIVIDUAL DOS BARBEIROS
  # -------------------------------------------------------
  jornada_dia <- function(bid,barbeiro_id,data) {
    ds <- dia_semana_num(data)
    j <- dbq("SELECT TOP 1 id,nome_barbearia,barbeiro_id,nome_barbeiro,
                     dia_semana,nome_dia,trabalha,hora_inicio,hora_fim
              FROM horarios_barbeiros
              WHERE barbearia_id=? AND barbeiro_id=? AND dia_semana=?",
             params=list(bid,barbeiro_id,ds))

    # Sem configuração individual, o barbeiro herda o funcionamento da barbearia.
    # Isso preserva o comportamento atual e também atende novos barbeiros.
    if(nrow(j)==0){
      h <- funcionamento_dia(bid,data)
      if(nrow(h)==0) return(data.frame())
      return(data.frame(
        id=NA_integer_,
        nome_barbearia=as.character(h$nome_barbearia[1]),
        barbeiro_id=barbeiro_id,
        nome_barbeiro=NA_character_,
        dia_semana=as.integer(h$dia_semana[1]),
        nome_dia=as.character(h$nome_dia[1]),
        trabalha=as.integer(h$aberto[1]),
        hora_inicio=if(as.integer(h$aberto[1])==1) as.character(h$hora_abertura[1]) else NA_character_,
        hora_fim=if(as.integer(h$aberto[1])==1) as.character(h$hora_fechamento[1]) else NA_character_,
        stringsAsFactors=FALSE
      ))
    }
    j
  }

  horario_dentro_jornada <- function(bid,barbeiro_id,data,hora) {
    if(is.na(barbeiro_id)) return(FALSE)
    j <- jornada_dia(bid,barbeiro_id,data)
    if(nrow(j)==0 || is.na(j$trabalha[1]) || as.integer(j$trabalha[1])!=1) return(FALSE)
    hi <- substr(as.character(j$hora_inicio[1]),1,5)
    hf <- substr(as.character(j$hora_fim[1]),1,5)
    if(is.na(hi)||is.na(hf)||!nzchar(hi)||!nzchar(hf)) return(FALSE)
    hora>=hi && hora<hf
  }

  output$ui_jornada_resumo <- renderUI({
    bid <- id_barb(); jornada_upd(); funcionamento_upd()
    barbeiro_id <- suppressWarnings(as.integer(input$jornada_barbeiro))
    if(is.null(bid) || is.na(barbeiro_id)) return(NULL)
    j <- dbq("SELECT trabalha,hora_inicio,hora_fim FROM horarios_barbeiros
              WHERE barbearia_id=? AND barbeiro_id=? ORDER BY dia_semana",
             params=list(bid,barbeiro_id))
    if(nrow(j)==0) return(div(class="availability-mini",
      div(class="availability-mini-card",div(class="availability-mini-label","Configuração"),div(class="availability-mini-value",style="font-size:16px;","Herdada da barbearia"))))
    dias <- sum(as.integer(j$trabalha)==1,na.rm=TRUE)
    horas <- if(dias>0) {
      ini <- substr(as.character(j$hora_inicio[as.integer(j$trabalha)==1]),1,5)
      fim <- substr(as.character(j$hora_fim[as.integer(j$trabalha)==1]),1,5)
      paste0(min(ini,na.rm=TRUE)," – ",max(fim,na.rm=TRUE))
    } else "Sem jornada"
    div(class="availability-mini",
        div(class="availability-mini-card",div(class="availability-mini-label","Dias de trabalho"),div(class="availability-mini-value",dias)),
        div(class="availability-mini-card",div(class="availability-mini-label","Faixa semanal"),div(class="availability-mini-value",style="font-size:17px;",horas)),
        div(class="availability-mini-card",div(class="availability-mini-label","Regra"),div(class="availability-mini-value",style="font-size:16px;","Jornada individual")))
  })

  output$ui_jornada_barbeiro <- renderUI({
    bid <- id_barb(); jornada_upd(); funcionamento_upd()
    barbeiro_id <- suppressWarnings(as.integer(input$jornada_barbeiro))
    if(is.null(bid) || is.na(barbeiro_id))
      return(p(class="text-muted","Cadastre ou selecione um barbeiro."))

    bx <- dbq("SELECT id,nome_usuario FROM usuarios
               WHERE id=? AND barbearia_id=? AND perfil='BARBEIRO' LIMIT 1",
              params=list(barbeiro_id,bid))
    if(nrow(bx)==0) return(p(class="text-danger","Barbeiro não encontrado."))
    op <- c(sprintf("%02d:%02d",rep(0:23,each=2),rep(c(0,30),24)),"23:59")

    tagList(
      div(class="week-grid-head",
          div("Dia / situação"),div("Início"),div("Fim"),div("Origem")),
      lapply(seq_len(nrow(dias_funcionamento)),function(i){
        d <- dias_funcionamento[i,]
        ref <- as.Date("2026-09-28") + (d$dia_semana-1)
        h <- funcionamento_dia(bid,ref)
        jdb <- dbq("SELECT trabalha,hora_inicio,hora_fim FROM horarios_barbeiros
                    WHERE barbearia_id=? AND barbeiro_id=? AND dia_semana=? LIMIT 1",
                   params=list(bid,barbeiro_id,d$dia_semana))
        herdado <- nrow(jdb)==0
        if(herdado){
          trabalha <- nrow(h)>0 && as.integer(h$aberto[1])==1
          hi <- if(trabalha) substr(as.character(h$hora_abertura[1]),1,5) else "08:00"
          hf <- if(trabalha) substr(as.character(h$hora_fechamento[1]),1,5) else "18:00"
        } else {
          trabalha <- as.integer(jdb$trabalha[1])==1
          hi <- if(!is.na(jdb$hora_inicio[1])) substr(as.character(jdb$hora_inicio[1]),1,5) else "08:00"
          hf <- if(!is.na(jdb$hora_fim[1])) substr(as.character(jdb$hora_fim[1]),1,5) else "18:00"
        }
        limite <- if(nrow(h)>0 && as.integer(h$aberto[1])==1)
          paste0("Limite: ",substr(as.character(h$hora_abertura[1]),1,5),"–",substr(as.character(h$hora_fechamento[1]),1,5))
        else "Barbearia fechada"

        div(class=paste("week-row",if(!trabalha) "is-closed" else ""),
            div(class="week-status",
                div(class="week-day",d$nome_dia),
                checkboxInput(paste0("jor_trabalha_",d$dia_semana),"Trabalha",value=trabalha),
                div(class="week-note",limite)),
            selectInput(paste0("jor_inicio_",d$dia_semana),NULL,choices=op,selected=hi),
            selectInput(paste0("jor_fim_",d$dia_semana),NULL,choices=op,selected=hf),
            div(if(herdado) span(class="week-badge inherited","Herdado")
                else span(class="week-badge","Personalizado"))
        )
      })
    )
  })

  observeEvent(input$salvar_jornada_barbeiro,{
    bid <- id_barb()
    barbeiro_id <- suppressWarnings(as.integer(input$jornada_barbeiro))
    if(is.null(bid)||is.na(barbeiro_id)) {
      showNotification("Selecione um barbeiro.",type="error");return()
    }

    perfil_atual <- as.character(estado$usuario$perfil[1])
    if(perfil_atual=="BARBEIRO" && barbeiro_id!=as.integer(estado$usuario$id[1])){
      showNotification("O barbeiro pode alterar somente a própria jornada.",type="error");return()
    }

    b <- dbq("SELECT nome FROM barbearias WHERE id=? LIMIT 1",params=list(bid))
    bx <- dbq("SELECT id,nome_usuario FROM usuarios
               WHERE id=? AND barbearia_id=? AND perfil='BARBEIRO' LIMIT 1",
              params=list(barbeiro_id,bid))
    if(nrow(b)==0||nrow(bx)==0){
      showNotification("Barbearia ou barbeiro não encontrado.",type="error");return()
    }

    dados <- tryCatch(lapply(seq_len(nrow(dias_funcionamento)),function(i){
      d <- dias_funcionamento[i,]
      trabalha <- isTRUE(input[[paste0("jor_trabalha_",d$dia_semana)]])
      hi <- as.character(input[[paste0("jor_inicio_",d$dia_semana)]] %||% "")
      hf <- as.character(input[[paste0("jor_fim_",d$dia_semana)]] %||% "")

      ref <- as.Date("2026-09-28") + (d$dia_semana-1)
      h <- funcionamento_dia(bid,ref)

      if(trabalha){
        if(nrow(h)==0 || as.integer(h$aberto[1])!=1)
          stop(paste0(d$nome_dia,": a barbearia está fechada nesse dia."))

        hab <- substr(as.character(h$hora_abertura[1]),1,5)
        hfe <- substr(as.character(h$hora_fechamento[1]),1,5)

        if(!grepl("^[0-9]{2}:[0-9]{2}$",hi) ||
           !grepl("^[0-9]{2}:[0-9]{2}$",hf) || hf<=hi)
          stop(paste0(d$nome_dia,": o fim da jornada deve ser posterior ao início."))

        if(hi<hab || hf>hfe)
          stop(paste0(d$nome_dia,": a jornada deve ficar entre ",hab," e ",hfe,"."))
      }
      list(d=d,trabalha=trabalha,hi=hi,hf=hf)
    }),error=function(e)e)

    if(inherits(dados,"error")){
      showNotification(conditionMessage(dados),type="error",duration=10);return()
    }

    ator_id <- as.integer(estado$usuario$id[1])
    ator_nome <- usuario_auditoria_nome()
    ator_perfil <- as.character(estado$usuario$perfil[1])
    agora <- format(Sys.time(),"%Y-%m-%d %H:%M:%S")

    tryCatch({
      dbWithTransaction(con,{
        for(z in dados){
          ex <- dbq("SELECT id FROM horarios_barbeiros
                     WHERE barbearia_id=? AND barbeiro_id=? AND dia_semana=? LIMIT 1",
                    params=list(bid,barbeiro_id,z$d$dia_semana))
          hi_db <- if(z$trabalha) z$hi else NA_character_
          hf_db <- if(z$trabalha) z$hf else NA_character_

          if(nrow(ex)>0){
            dbe("UPDATE horarios_barbeiros
                 SET nome_barbearia=?,nome_barbeiro=?,nome_dia=?,trabalha=?,
                     hora_inicio=?,hora_fim=?,atualizado_por_usuario_id=?,
                     atualizado_por_usuario=?,atualizado_por_perfil=?,atualizado_em=?
                 WHERE id=? AND barbearia_id=? AND barbeiro_id=?",
                params=list(as.character(b$nome[1]),as.character(bx$nome_usuario[1]),
                            z$d$nome_dia,as.integer(z$trabalha),hi_db,hf_db,
                            ator_id,ator_nome,ator_perfil,agora,ex$id[1],bid,barbeiro_id))
          } else {
            dbe("INSERT INTO horarios_barbeiros(
                   barbearia_id,nome_barbearia,barbeiro_id,nome_barbeiro,
                   dia_semana,nome_dia,trabalha,hora_inicio,hora_fim,
                   atualizado_por_usuario_id,atualizado_por_usuario,
                   atualizado_por_perfil,atualizado_em)
                 VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)",
                params=list(bid,as.character(b$nome[1]),barbeiro_id,
                            as.character(bx$nome_usuario[1]),z$d$dia_semana,z$d$nome_dia,
                            as.integer(z$trabalha),hi_db,hf_db,
                            ator_id,ator_nome,ator_perfil,agora))
          }
        }
      })

      registrar_historico(bid,"ALTERACAO","JORNADA_BARBEIRO",
                          paste0("barbeiro_",barbeiro_id),"",
                          paste0("Jornada semanal atualizada: ",bx$nome_usuario[1]))
      jornada_upd(jornada_upd()+1)
      agenda_upd(agenda_upd()+1)
      showNotification("Jornada do barbeiro atualizada.",type="message")
    },error=function(e){
      showNotification(paste("Não foi possível salvar a jornada:",conditionMessage(e)),
                       type="error",duration=10)
    })
  })

  # -------------------------------------------------------
  # BLOQUEIOS DE AGENDA
  # -------------------------------------------------------
  hora_hhmm <- function(x) {
    if(inherits(x,c("POSIXct","POSIXlt"))) format(x,"%H:%M")
    else substr(as.character(x),1,5)
  }

  horario_bloqueado <- function(bid,barbeiro_id,data,hora) {
    x <- dbq("
      SELECT TOP 1 id,nome_barbearia,nome_barbeiro,hora_inicio,hora_fim,motivo
      FROM bloqueios_agenda
      WHERE barbearia_id=? AND data=?
        AND status='ATIVO'
        AND (barbeiro_id IS NULL OR barbeiro_id=?)
        AND CAST(? AS TIME) >= hora_inicio
        AND CAST(? AS TIME) < hora_fim
      ORDER BY CASE WHEN barbeiro_id IS NULL THEN 0 ELSE 1 END,id",
      params=list(bid,as.character(data),barbeiro_id,hora,hora))
    x
  }

  observeEvent(input$salvar_bloqueio,{
    bid <- id_barb()
    if(is.null(bid)) return()

    dat <- as.character(input$bloqueio_data)
    hi <- hora_hhmm(input$bloqueio_inicio)
    hf <- hora_hhmm(input$bloqueio_fim)
    alvo <- as.character(input$bloqueio_alvo %||% "TODOS")
    motivo <- trimws(as.character(input$bloqueio_motivo %||% ""))

    if(is.na(as.Date(dat))){showNotification("Informe uma data válida.",type="error");return()}
    if(!grepl("^[0-9]{2}:[0-9]{2}$",hi) || !grepl("^[0-9]{2}:[0-9]{2}$",hf)){
      showNotification("Informe horários válidos.",type="error");return()
    }
    if(hf<=hi){
      showNotification("O horário final deve ser maior que o horário inicial.",type="error");return()
    }

    b <- dbq("SELECT nome FROM barbearias WHERE id=? LIMIT 1",params=list(bid))
    if(nrow(b)==0){showNotification("Barbearia não encontrada.",type="error");return()}

    barbeiro_id <- NA_integer_
    nome_barbeiro <- NA_character_
    if(alvo!="TODOS"){
      barbeiro_id <- suppressWarnings(as.integer(alvo))
      bx <- dbq("SELECT nome_usuario FROM usuarios
                 WHERE id=? AND barbearia_id=? AND perfil='BARBEIRO' LIMIT 1",
                params=list(barbeiro_id,bid))
      if(nrow(bx)==0){showNotification("Barbeiro não encontrado.",type="error");return()}
      nome_barbeiro <- as.character(bx$nome_usuario[1])
    }

    # Não duplica/sobrepõe bloqueios do mesmo alvo no mesmo dia.
    if(is.na(barbeiro_id)){
      dup <- dbq("SELECT id FROM bloqueios_agenda
                  WHERE barbearia_id=? AND barbeiro_id IS NULL AND data=?
                    AND status='ATIVO'
                    AND hora_inicio < CAST(? AS TIME) AND hora_fim > CAST(? AS TIME)
                  LIMIT 1",params=list(bid,dat,hf,hi))
    } else {
      dup <- dbq("SELECT id FROM bloqueios_agenda
                  WHERE barbearia_id=? AND barbeiro_id=? AND data=?
                    AND status='ATIVO'
                    AND hora_inicio < CAST(? AS TIME) AND hora_fim > CAST(? AS TIME)
                  LIMIT 1",params=list(bid,barbeiro_id,dat,hf,hi))
    }
    if(nrow(dup)>0){
      showNotification("Já existe um bloqueio sobreposto para esse período.",type="error");return()
    }

    ator_id <- if(!is.null(estado$usuario$id)) as.integer(estado$usuario$id[1]) else NA_integer_
    ator_nome <- usuario_auditoria_nome()
    ator_perfil <- if(!is.null(estado$usuario$perfil)) as.character(estado$usuario$perfil[1]) else NA_character_

    dbe("INSERT INTO bloqueios_agenda(
           barbearia_id,nome_barbearia,barbeiro_id,nome_barbeiro,data,
           hora_inicio,hora_fim,motivo,criado_por_usuario_id,criado_por_usuario,
           criado_por_perfil,criado_em,status)
         VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)",
        params=list(bid,as.character(b$nome[1]),barbeiro_id,nome_barbeiro,dat,
                    hi,hf,if(motivo=="") NA_character_ else motivo,
                    ator_id,ator_nome,ator_perfil,format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
                    "ATIVO"))

    registrar_historico(bid,"CRIACAO","BLOQUEIO_AGENDA","periodo","",
                        paste(dat,hi,hf,ifelse(is.na(nome_barbeiro),"TODA BARBEARIA",nome_barbeiro)))
    bloqueios_upd(bloqueios_upd()+1)
    showNotification("Horário bloqueado.",type="message")
  })

  output$bloqueios_kpis <- renderUI({
    bid <- id_barb(); bloqueios_upd()
    if(is.null(bid)) return(NULL)
    hoje <- as.character(Sys.Date())
    x <- dbq("SELECT
                SUM(CASE WHEN data>=? THEN 1 ELSE 0 END) futuros,
                SUM(CASE WHEN data=? THEN 1 ELSE 0 END) hoje,
                COUNT(DISTINCT CASE WHEN data>=? AND barbeiro_id IS NOT NULL THEN barbeiro_id END) barbeiros
              FROM bloqueios_agenda WHERE barbearia_id=? AND status='ATIVO'",
             params=list(hoje,hoje,hoje,bid))
    v <- function(n){ z<-suppressWarnings(as.integer(x[[n]][1])); if(length(z)==0||is.na(z)) 0L else z }
    div(class="availability-mini",
        div(class="availability-mini-card",div(class="availability-mini-label","Bloqueios futuros"),div(class="availability-mini-value",v("futuros"))),
        div(class="availability-mini-card",div(class="availability-mini-label","Hoje"),div(class="availability-mini-value",v("hoje"))),
        div(class="availability-mini-card",div(class="availability-mini-label","Barbeiros afetados"),div(class="availability-mini-value",v("barbeiros"))))
  })

  output$tabela_bloqueios <- renderDT({
    bid <- id_barb(); bloqueios_upd()
    if(is.null(bid)) return(datatable(data.frame(),rownames=FALSE))
    x <- dbq("SELECT id,nome_barbearia,
                     CASE WHEN barbeiro_id IS NULL THEN 'TODA A BARBEARIA'
                          ELSE nome_barbeiro END aplicado_a,
                     data,hora_inicio,hora_fim,motivo,criado_por_usuario,
                     criado_por_perfil,criado_em
              FROM bloqueios_agenda
              WHERE barbearia_id=? AND status='ATIVO'
              ORDER BY data DESC,hora_inicio DESC,id DESC",params=list(bid))
    if(nrow(x)>0){
      x$data <- format(as.Date(x$data),"%d/%m/%Y")
      x$hora_inicio <- substr(as.character(x$hora_inicio),1,5)
      x$hora_fim <- substr(as.character(x$hora_fim),1,5)
      x$motivo <- ifelse(is.na(x$motivo),"",x$motivo)
      x$criado_em <- format(as.POSIXct(x$criado_em),"%d/%m/%Y %H:%M")
    }
    names(x) <- c("ID","Barbearia","Aplicado a","Data","Início","Fim",
                  "Motivo","Usuário","Perfil","Criado em")
    datatable(x,rownames=FALSE,selection="single",escape=FALSE,options=list(pageLength=10,language=list(
      search="Pesquisar:",lengthMenu="Mostrar _MENU_ registros",
      info="Mostrando _START_ até _END_ de _TOTAL_ registros",
      zeroRecords="Nenhum bloqueio encontrado",emptyTable="Nenhum horário bloqueado",
      paginate=list(first="Primeiro",last="Último",'next'="Próximo",previous="Anterior"))))
  })

  observeEvent(input$remover_bloqueio,{
    bid <- id_barb()
    idx <- input$tabela_bloqueios_rows_selected
    if(is.null(bid) || length(idx)==0){
      showNotification("Selecione um bloqueio na tabela.",type="error");return()
    }
    x <- dbq("SELECT id,data,hora_inicio,hora_fim,nome_barbeiro,barbeiro_id
              FROM bloqueios_agenda
              WHERE barbearia_id=? AND status='ATIVO'
              ORDER BY data DESC,hora_inicio DESC,id DESC",params=list(bid))
    if(idx[1]<1 || idx[1]>nrow(x)) return()
    r <- x[idx[1],]

    ator_id <- if(!is.null(estado$usuario$id)) as.integer(estado$usuario$id[1]) else NA_integer_
    ator_nome <- usuario_auditoria_nome()
    ator_perfil <- if(!is.null(estado$usuario$perfil)) as.character(estado$usuario$perfil[1]) else NA_character_
    agora <- format(Sys.time(),"%Y-%m-%d %H:%M:%S")

    dbe("UPDATE bloqueios_agenda
         SET status='EXCLUIDO',
             excluido_por_usuario_id=?,
             excluido_por_usuario=?,
             excluido_por_perfil=?,
             excluido_em=?
         WHERE id=? AND barbearia_id=? AND status='ATIVO'",
        params=list(ator_id,ator_nome,ator_perfil,agora,r$id,bid))

    registrar_historico(bid,"EXCLUSAO","BLOQUEIO_AGENDA","periodo",
                        paste(r$data,r$hora_inicio,r$hora_fim),"EXCLUIDO")
    bloqueios_upd(bloqueios_upd()+1)
    agenda_upd(agenda_upd()+1)
    showNotification("Bloqueio removido da agenda e preservado no histórico.",type="message")
  })

  # -------------------------------------------------------
  # NOVO AGENDAMENTO
  # -------------------------------------------------------
  observeEvent(input$salvar_agendamento,{
    bid <- id_barb()
    if(is.null(bid))return()

    nome <- trimws(input$agenda_nome)
    tel <- gsub("[^0-9]","",trimws(input$agenda_telefone))
    email <- trimws(input$agenda_email)

    if(nome==""){showNotification("Preencha o nome do cliente.",type="error");return()}
    if(nchar(tel)!=11){showNotification("O telefone deve conter exatamente 11 dígitos.",type="error");return()}
    if(!validar_email(email)){showNotification("Informe um e-mail válido.",type="error");return()}
    if(is.null(input$agenda_servico)||input$agenda_servico==""){
      showNotification("Selecione um serviço.",type="error");return()
    }
    barbeiro_id <- suppressWarnings(as.integer(input$agenda_barbeiro))
    if(is.na(barbeiro_id)){
      showNotification("Selecione um barbeiro.",type="error");return()
    }
    barb_ok <- dbq("SELECT id FROM usuarios WHERE id=? AND barbearia_id=? AND perfil='BARBEIRO' AND ativo=1 LIMIT 1",
                   params=list(barbeiro_id,bid))
    if(nrow(barb_ok)==0){
      showNotification("O barbeiro selecionado não está disponível.",type="error");return()
    }
    if(is.null(input$agenda_data)||is.na(input$agenda_data)){
      showNotification("Selecione uma data.",type="error");return()
    }

    hi <- input$agenda_hora
    hora <- if(inherits(hi,"POSIXct")||inherits(hi,"POSIXlt")) format(hi,"%H:%M") else substr(as.character(hi),1,5)
    dat <- as.character(input$agenda_data)

    if(!horario_dentro_funcionamento(bid,dat,hora)){
      hfunc <- funcionamento_dia(bid,dat)
      msg <- if(nrow(hfunc)==0 || as.integer(hfunc$aberto[1])!=1)
        "A barbearia está fechada nesse dia."
      else paste0("Horário fora do funcionamento (",
                  substr(as.character(hfunc$hora_abertura[1]),1,5)," às ",
                  substr(as.character(hfunc$hora_fechamento[1]),1,5),").")
      showNotification(msg,type="error",duration=8);return()
    }

    if(!horario_dentro_jornada(bid,barbeiro_id,dat,hora)){
      j <- jornada_dia(bid,barbeiro_id,dat)
      msg <- if(nrow(j)==0 || as.integer(j$trabalha[1])!=1)
        "O barbeiro não trabalha nesse dia."
      else paste0("Horário fora da jornada do barbeiro (",
                  substr(as.character(j$hora_inicio[1]),1,5)," às ",
                  substr(as.character(j$hora_fim[1]),1,5),").")
      showNotification(msg,type="error",duration=8);return()
    }

    bloqueado <- horario_bloqueado(bid,barbeiro_id,dat,hora)
    if(nrow(bloqueado)>0){
      alvo <- if(is.na(bloqueado$nome_barbeiro[1])) "a barbearia" else bloqueado$nome_barbeiro[1]
      showNotification(paste0("Horário indisponível: existe um bloqueio para ",alvo,
                              " das ",substr(as.character(bloqueado$hora_inicio[1]),1,5),
                              " às ",substr(as.character(bloqueado$hora_fim[1]),1,5),"."),
                       type="error",duration=8);return()
    }

    ocupado <- dbq("
      SELECT id FROM atendimentos
      WHERE barbearia_id=? AND barbeiro_id=? AND data=? AND hora=? AND status<>'CANCELADO' LIMIT 1",
      params=list(bid,barbeiro_id,dat,hora))
    if(nrow(ocupado)>0){
      showNotification(paste("O horário",hora,"do dia",format(as.Date(dat),"%d/%m/%Y"),"já está ocupado."),
                       type="error");return()
    }

    serv <- dbq("
      SELECT id,preco,duracao_minutos FROM servicos
      WHERE id=? AND barbearia_id=? AND status='ATIVO' LIMIT 1",
      params=list(as.integer(input$agenda_servico),bid))
    if(nrow(serv)==0){showNotification("Serviço não encontrado.",type="error");return()}

    # 1) Procura um cliente operacional já existente.
    cli <- dbq("SELECT * FROM clientes WHERE barbearia_id=? AND LOWER(email)=LOWER(?) LIMIT 1",
                      params=list(bid,email))
    if(nrow(cli)==0)
      cli <- dbq("SELECT * FROM clientes WHERE barbearia_id=? AND telefone=? LIMIT 1",
                        params=list(bid,tel))
    if(nrow(cli)==0)
      cli <- dbq("SELECT * FROM clientes WHERE barbearia_id=? AND LOWER(nome)=LOWER(?) LIMIT 1",
                        params=list(bid,nome))

    # 2) Se o cliente ainda não existe operacionalmente, ele nasce aqui,
    #    no momento do agendamento. Cadastro público sozinho não cria esta linha.
    if(nrow(cli)>0) {
      cliente_id <- as.integer(cli$id[1])
      dbe("UPDATE clientes SET nome=?,telefone=?,email=?,status='ATIVO'
                  WHERE id=? AND barbearia_id=?",
                params=list(nome,tel,email,cliente_id,bid))
    } else {
      cadastro_match <- dbq("
        SELECT id FROM cadastros_clientes
        WHERE barbearia_id=? AND (LOWER(email)=LOWER(?) OR telefone=?)
        ORDER BY CASE WHEN LOWER(email)=LOWER(?) THEN 0 ELSE 1 END,id
        LIMIT 1",
        params=list(bid,email,tel,email))
      cadastro_id <- if(nrow(cadastro_match)>0) as.integer(cadastro_match$id[1]) else NA_integer_

      dbe("
        INSERT INTO clientes(nome,telefone,email,data_cadastro,hora_cadastro,status,barbearia_id,cadastro_cliente_id)
        VALUES(?,?,?,?,?, 'ATIVO',?,?)",
        params=list(nome,tel,email,as.character(Sys.Date()),format(Sys.time(),"%H:%M:%S"),bid,cadastro_id))
      cliente_id <- as.integer(db_last_id())
    }

    # 3) Vincula o cadastro público à ficha operacional e ao usuário de acesso.
    cadastro_match <- dbq("
      SELECT id FROM cadastros_clientes
      WHERE barbearia_id=? AND (LOWER(email)=LOWER(?) OR telefone=?)
      ORDER BY CASE WHEN LOWER(email)=LOWER(?) THEN 0 ELSE 1 END,id
      LIMIT 1",
      params=list(bid,email,tel,email))
    if(nrow(cadastro_match)>0) {
      cadastro_id <- as.integer(cadastro_match$id[1])
      dbe("
        UPDATE cadastros_clientes
        SET cliente_id=?,nome=?,telefone=?,email=?,status='CONVERTIDO',data_conversao=?
        WHERE id=? AND barbearia_id=?",
        params=list(cliente_id,nome,tel,email,as.character(Sys.Date()),cadastro_id,bid))
      dbe("
        UPDATE clientes SET cadastro_cliente_id=?
        WHERE id=? AND barbearia_id=?",
        params=list(cadastro_id,cliente_id,bid))
    }

    acesso <- tryCatch(
      criar_acesso_cliente(cliente_id,nome,email,bid),
      error=function(e)list(ok=FALSE,enviado=FALSE,msg=conditionMessage(e))
    )
    if(!acesso$ok){showNotification(acesso$msg,type="error",duration=8);return()}
    cadastros_upd(cadastros_upd()+1)

    dbe("
      INSERT INTO atendimentos(cliente_id,servico_id,data,hora,preco,duracao_minutos,
                               status,observacao,data_criacao,barbearia_id,barbeiro_id,
                               status_confirmacao,criado_por_perfil,criado_por_usuario_id,
                               confirmado_em,confirmado_por_usuario_id)
      VALUES(?,?,?,?,? ,?,'AGENDADO',?,?,?,?,?,?,?,?,?)",
      params=list(cliente_id,serv$id[1],dat,hora,serv$preco[1],serv$duracao_minutos[1],
                  input$agenda_obs,format(Sys.time(),"%Y-%m-%d %H:%M:%S"),bid,barbeiro_id,
                  as.character(input$agenda_confirmacao %||% "PENDENTE"),
                  as.character(estado$usuario$perfil[1]),
                  as.integer(estado$usuario$id[1]),
                  if(identical(as.character(input$agenda_confirmacao),"CONFIRMADO"))
                    format(Sys.time(),"%Y-%m-%d %H:%M:%S") else NULL,
                  if(identical(as.character(input$agenda_confirmacao),"CONFIRMADO"))
                    as.integer(estado$usuario$id[1]) else NULL))
    atendimento_id <- as.integer(db_last_id())

    em <- montar_email_agendamento(bid,atendimento_id)
    if(!is.null(em) && validar_email(em$to)) {
      enfileirar_notificacao_email(
        bid,atendimento_id,cliente_id,em$to,em$subject,em$body,
        as.integer(estado$usuario$id[1]),as.character(estado$usuario$perfil[1]))
      notificacoes_upd(notificacoes_upd()+1)
    }

    clientes_upd(clientes_upd()+1); agenda_upd(agenda_upd()+1)
    msg <- if(acesso$enviado)
      "Atendimento cadastrado. Acesso do cliente criado e e-mail enviado."
      else "Atendimento cadastrado. O acesso foi criado, mas o e-mail não foi enviado."
    showNotification(msg,type="message",duration=8)

    updateTextInput(session,"agenda_nome",value="")
    updateTextInput(session,"agenda_telefone",value="")
    updateTextInput(session,"agenda_email",value="")
    updateTextAreaInput(session,"agenda_obs",value="")
    updateDateInput(session,"agenda_data",value=Sys.Date())
    updateTimeInput(session,"agenda_hora",value="10:00")
  })

  # -------------------------------------------------------
  # DASHBOARD EXECUTIVO
  # -------------------------------------------------------
  atualizar_barbeiros_dashboard <- function() {
    bid <- id_barb()
    if(is.null(bid)) return(invisible(NULL))
    x <- dbq("SELECT id,nome_usuario,ativo
              FROM usuarios
              WHERE barbearia_id=? AND perfil='BARBEIRO'
              ORDER BY nome_usuario,id",params=list(bid))
    escolhas <- c("Todos os barbeiros"="TODOS")
    if(nrow(x)>0){
      labs <- paste0(x$nome_usuario,ifelse(x$ativo==1,""," (desativado)"))
      escolhas <- c(escolhas,stats::setNames(as.character(x$id),labs))
    }
    atual <- isolate(input$dash_barbeiro)
    if(is.null(atual) || !as.character(atual) %in% unname(escolhas)) atual <- "TODOS"
    updateSelectInput(session,"dash_barbeiro",choices=escolhas,selected=atual)
    invisible(NULL)
  }

  observeEvent(list(estado$pagina,input$master_sel,master_upd()),{
    if(estado$pagina=="gestao") atualizar_barbeiros_dashboard()
  },ignoreInit=FALSE)

  observeEvent(input$dash_limpar,{
    hoje <- Sys.Date()
    updateDateRangeInput(session,"dash_periodo",
                         start=as.Date(format(hoje,"%Y-%m-01")),end=hoje)
    updateSelectInput(session,"dash_barbeiro",selected="TODOS")
  })

  dash_filtro <- reactive({
    bid <- id_barb()
    req(!is.null(bid))
    p <- input$dash_periodo
    if(is.null(p)||length(p)<2||any(is.na(p))){
      fim <- Sys.Date()
      inicio <- as.Date(format(fim,"%Y-%m-01"))
    } else {
      inicio <- as.Date(p[1]); fim <- as.Date(p[2])
    }
    if(fim<inicio){z<-inicio;inicio<-fim;fim<-z}

    raw <- as.character(input$dash_barbeiro %||% "TODOS")
    barbeiro_id <- suppressWarnings(as.integer(raw))
    todos <- identical(raw,"TODOS") || is.na(barbeiro_id)
    nome <- "Todos os barbeiros"
    if(!todos){
      bx <- dbq("SELECT nome_usuario FROM usuarios
                 WHERE id=? AND barbearia_id=? AND perfil='BARBEIRO' LIMIT 1",
                params=list(barbeiro_id,bid))
      if(nrow(bx)>0) nome <- as.character(bx$nome_usuario[1])
      else {todos<-TRUE;barbeiro_id<-NA_integer_}
    }
    list(bid=bid,inicio=inicio,fim=fim,todos=todos,
         barbeiro_id=barbeiro_id,nome=nome)
  })

  dash_where_barbeiro <- function(f){
    if(f$todos) list(sql="",params=list())
    else list(sql=" AND a.barbeiro_id=?",params=list(f$barbeiro_id))
  }

  dash_metricas <- reactive({
    f <- dash_filtro(); wb <- dash_where_barbeiro(f)
    sql <- paste0(
      "SELECT
         COUNT(*) total,
         COALESCE(SUM(CASE WHEN a.status='CONCLUÍDO' THEN 1 ELSE 0 END),0) concluidos,
         COALESCE(SUM(CASE WHEN a.status='AGENDADO' THEN 1 ELSE 0 END),0) agendados,
         COALESCE(SUM(CASE WHEN a.status='FALTA' THEN 1 ELSE 0 END),0) faltas,
         COALESCE(SUM(CASE WHEN a.status='CANCELADO' THEN 1 ELSE 0 END),0) cancelados,
         COALESCE(SUM(CASE WHEN a.status='CONCLUÍDO' THEN a.preco ELSE 0 END),0) faturamento,
         COUNT(DISTINCT CASE WHEN a.status='CONCLUÍDO' THEN a.cliente_id END) clientes
       FROM atendimentos a
       WHERE a.barbearia_id=? AND a.data>=? AND a.data<=?",wb$sql)
    x <- dbq(sql,params=c(list(f$bid,as.character(f$inicio),as.character(f$fim)),wb$params))
    fat <- as.numeric(x$faturamento[1] %||% 0)
    conc <- as.integer(x$concluidos[1] %||% 0)
    list(total=as.integer(x$total[1] %||% 0),
         concluidos=conc,
         agendados=as.integer(x$agendados[1] %||% 0),
         faltas=as.integer(x$faltas[1] %||% 0),
         cancelados=as.integer(x$cancelados[1] %||% 0),
         faturamento=fat,
         clientes=as.integer(x$clientes[1] %||% 0),
         ticket=if(conc>0) fat/conc else 0)
  })

  dash_periodo_anterior_metricas <- reactive({
    f <- dash_filtro()
    dias <- as.integer(f$fim-f$inicio)+1L
    fim_ant <- f$inicio-1
    ini_ant <- fim_ant-(dias-1L)
    wb <- dash_where_barbeiro(f)
    sql <- paste0(
      "SELECT
         COALESCE(SUM(CASE WHEN a.status='CONCLUÍDO' THEN 1 ELSE 0 END),0) concluidos,
         COALESCE(SUM(CASE WHEN a.status='CONCLUÍDO' THEN a.preco ELSE 0 END),0) faturamento,
         COUNT(DISTINCT CASE WHEN a.status='CONCLUÍDO' THEN a.cliente_id END) clientes
       FROM atendimentos a
       WHERE a.barbearia_id=? AND a.data>=? AND a.data<=?",wb$sql)
    x <- dbq(sql,params=c(list(f$bid,as.character(ini_ant),as.character(fim_ant)),wb$params))
    fat <- as.numeric(x$faturamento[1] %||% 0)
    conc <- as.integer(x$concluidos[1] %||% 0)
    list(inicio=ini_ant,fim=fim_ant,faturamento=fat,concluidos=conc,
         clientes=as.integer(x$clientes[1] %||% 0),
         ticket=if(conc>0) fat/conc else 0)
  })

  variacao_html <- function(atual,anterior){
    atual <- as.numeric(atual %||% 0); anterior <- as.numeric(anterior %||% 0)
    if(anterior==0 && atual==0) return(tags$span(class="flat","— sem variação"))
    if(anterior==0 && atual>0) return(tags$span(class="up","▲ novo no período"))
    pct <- ((atual-anterior)/abs(anterior))*100
    cls <- if(pct>0) "up" else if(pct<0) "down" else "flat"
    seta <- if(pct>0) "▲" else if(pct<0) "▼" else "—"
    tags$span(class=cls,paste0(seta," ",format(abs(pct),digits=1,nsmall=1,
                                               decimal.mark=",",trim=TRUE),
                              "% vs. período anterior"))
  }

  dash_ocupacao <- reactive({
    f <- dash_filtro()
    # Ocupação executiva = compromissos não cancelados / capacidade teórica
    # da jornada, em slots de 30 minutos. Bloqueios extraordinários não
    # entram na capacidade teórica para manter o indicador comparável.
    barbs <- if(f$todos)
      dbq("SELECT id FROM usuarios WHERE barbearia_id=? AND perfil='BARBEIRO'",
          params=list(f$bid))
    else data.frame(id=f$barbeiro_id)

    if(nrow(barbs)==0) return(list(pct=0,ocupados=0,capacidade=0))

    hf <- dbq("SELECT dia_semana,aberto,hora_abertura,hora_fechamento
               FROM horarios_funcionamento WHERE barbearia_id=?",
              params=list(f$bid))
    hj <- dbq("SELECT barbeiro_id,dia_semana,trabalha,hora_inicio,hora_fim
               FROM horarios_barbeiros WHERE barbearia_id=?",
              params=list(f$bid))

    datas <- seq(f$inicio,f$fim,by="day")
    capacidade <- 0L
    minutos <- function(x){
      z <- strsplit(substr(as.character(x),1,5),":",fixed=TRUE)[[1]]
      as.integer(z[1])*60L+as.integer(z[2])
    }

    for(bid_b in as.integer(barbs$id)){
      for(dat in datas){
        ds <- dia_semana_num(dat)
        h <- hf[hf$dia_semana==ds,,drop=FALSE]
        if(nrow(h)==0 || as.integer(h$aberto[1])!=1) next
        ab <- minutos(h$hora_abertura[1]); fe <- minutos(h$hora_fechamento[1])

        j <- hj[hj$barbeiro_id==bid_b & hj$dia_semana==ds,,drop=FALSE]
        if(nrow(j)>0){
          if(as.integer(j$trabalha[1])!=1) next
          ab <- max(ab,minutos(j$hora_inicio[1]))
          fe <- min(fe,minutos(j$hora_fim[1]))
        }
        if(fe>ab) capacidade <- capacidade + floor((fe-ab)/30)
      }
    }

    wb <- dash_where_barbeiro(f)
    sql <- paste0("SELECT COUNT(*) qtd FROM atendimentos a
                   WHERE a.barbearia_id=? AND a.data>=? AND a.data<=?
                     AND a.status<>'CANCELADO'",wb$sql)
    x <- dbq(sql,params=c(list(f$bid,as.character(f$inicio),as.character(f$fim)),wb$params))
    ocupados <- as.integer(x$qtd[1] %||% 0)
    pct <- if(capacidade>0) min(100,ocupados/capacidade*100) else 0
    list(pct=pct,ocupados=ocupados,capacidade=capacidade)
  })

  output$dash_cards <- renderUI({
    m <- dash_metricas(); ant <- dash_periodo_anterior_metricas()
    kpi <- function(label,value,comparacao,classe=""){
      div(class=paste("exec-kpi",classe),
          div(class="exec-kpi-label",label),
          div(class="exec-kpi-value",value),
          div(class="exec-kpi-foot",comparacao))
    }
    fluidRow(
      column(3,kpi("Faturamento",formata_reais(m$faturamento),
                   variacao_html(m$faturamento,ant$faturamento),"kpi-green")),
      column(3,kpi("Concluídos",format(m$concluidos,big.mark=".",decimal.mark=","),
                   variacao_html(m$concluidos,ant$concluidos))),
      column(3,kpi("Ticket médio",formata_reais(m$ticket),
                   variacao_html(m$ticket,ant$ticket),"kpi-amber")),
      column(3,kpi("Clientes atendidos",format(m$clientes,big.mark=".",decimal.mark=","),
                   variacao_html(m$clientes,ant$clientes),"kpi-purple"))
    )
  })

  output$dash_mini_indicadores <- renderUI({
    m <- dash_metricas(); oc <- dash_ocupacao()
    div(class="mini-strip",
      div(class="mini-metric",
          div(class="mini-label","Agendados"),
          div(class="mini-value",format(m$agendados,big.mark=".",decimal.mark=","))),
      div(class="mini-metric",
          div(class="mini-label","Faltas"),
          div(class="mini-value",format(m$faltas,big.mark=".",decimal.mark=","))),
      div(class="mini-metric",
          div(class="mini-label","Cancelamentos"),
          div(class="mini-value",format(m$cancelados,big.mark=".",decimal.mark=","))),
      div(class="mini-metric",
          div(class="mini-label","Ocupação da agenda"),
          div(class="mini-value",paste0(format(round(oc$pct,1),nsmall=1,decimal.mark=","),"%")),
          div(class="dash-period-label",
              paste0(oc$ocupados," compromisso(s) / ",oc$capacidade," slots teóricos")))
    )
  })

  output$dash_grafico_faturamento <- renderPlot({
    f <- dash_filtro(); wb <- dash_where_barbeiro(f)
    sql <- paste0(
      "SELECT a.data,COALESCE(SUM(a.preco),0) faturamento
       FROM atendimentos a
       WHERE a.barbearia_id=? AND a.data>=? AND a.data<=?
         AND a.status='CONCLUÍDO'",wb$sql,"
       GROUP BY a.data ORDER BY a.data")
    x <- dbq(sql,params=c(list(f$bid,as.character(f$inicio),as.character(f$fim)),wb$params))
    par(mar=c(4,5,1,1))
    if(nrow(x)==0){
      plot.new(); text(.5,.55,"Sem faturamento concluído no período",cex=1.05)
      text(.5,.43,"Altere o período para visualizar a tendência.",cex=.85); return()
    }
    x$data <- as.Date(x$data)
    x$faturamento <- as.numeric(x$faturamento)

    if(nrow(x)==1){
      plot.new()
      text(.5,.61,formata_reais(x$faturamento[1]),cex=2,font=2)
      text(.5,.47,format(x$data[1],"%d/%m/%Y"),cex=1.05)
      text(.5,.34,"Há somente um dia com faturamento no período.",cex=.85)
      return()
    }

    ylim <- range(c(0,x$faturamento),na.rm=TRUE)
    plot(x$data,x$faturamento,type="o",pch=16,lwd=2,
         xaxt="n",xlab="",ylab="Faturamento (R$)",ylim=ylim)
    at <- pretty(x$data,n=6)
    axis.Date(1,at=at,format="%d/%m")
    grid()
  })

  output$dash_grafico_status <- renderPlot({
    m <- dash_metricas()
    vals <- c(m$concluidos,m$agendados,m$faltas,m$cancelados)
    labs <- c("Concluídos","Agendados","Faltas","Cancelados")

    if(sum(vals)==0){
      par(mar=c(1,1,1,1))
      plot.new()
      text(.5,.55,"Nenhum atendimento no período",cex=1)
      return()
    }

    # Paleta executiva: tons sóbrios e semanticamente distintos.
    cols <- c("#334155","#64748B","#94A3B8","#CBD5E1")
    keep <- vals > 0
    vals_p <- vals[keep]
    labs_p <- labs[keep]
    cols_p <- cols[keep]

    par(mar=c(1.5,1.5,1.5,1.5),xpd=NA)
    pie(vals_p,
        labels=NA,
        col=cols_p,
        border="white",
        lwd=2,
        clockwise=TRUE,
        init.angle=90,
        radius=.88)

    # Efeito donut para leitura mais limpa.
    symbols(0,0,circles=.47,inches=FALSE,add=TRUE,bg="white",fg="white")
    text(0,.08,sum(vals),cex=1.7,font=2,col="#111827")
    text(0,-.14,"atendimentos",cex=.82,col="#64748B")

    legend("bottom",
           legend=paste0(labs_p,"  ",vals_p),
           fill=cols_p,
           border=NA,
           horiz=TRUE,
           bty="n",
           cex=.78,
           inset=c(0,-.05),
           xpd=NA)
  })

  output$dash_grafico_barbeiros <- renderPlot({
    f <- dash_filtro()

    cond <- if(f$todos) "" else " AND u.id=?"
    params <- list(f$bid,as.character(f$inicio),as.character(f$fim),f$bid)
    if(!f$todos) params <- c(params,list(f$barbeiro_id))

    x <- dbq(paste0(
      "SELECT u.nome_usuario,
              COALESCE(SUM(CASE WHEN a.status='CONCLUÍDO' THEN 1 ELSE 0 END),0) atendimentos,
              COALESCE(SUM(CASE WHEN a.status='CONCLUÍDO' THEN a.preco ELSE 0 END),0) faturamento
       FROM usuarios u
       LEFT JOIN atendimentos a ON a.barbeiro_id=u.id
         AND a.barbearia_id=? AND a.data>=? AND a.data<=?
       WHERE u.barbearia_id=? AND u.perfil='BARBEIRO'",cond,"
       GROUP BY u.id,u.nome_usuario
       ORDER BY faturamento ASC,atendimentos ASC,u.nome_usuario"),params=params)

    if(nrow(x)==0){
      par(mar=c(1,1,1,1))
      plot.new()
      text(.5,.5,"Nenhum barbeiro encontrado",cex=1)
      return()
    }

    fat <- as.numeric(x$faturamento)
    nomes <- as.character(x$nome_usuario)
    total <- sum(fat,na.rm=TRUE)

    # Mantém zero visível sem distorcer o valor real.
    max_fat <- max(fat,na.rm=TRUE)
    xmax <- if(max_fat>0) max_fat*1.28 else 1

    par(mar=c(4,9,1,3))
    bp <- barplot(fat,
                  names.arg=nomes,
                  horiz=TRUE,
                  las=1,
                  xlim=c(0,xmax),
                  col="#475569",
                  border=NA,
                  xlab="Faturamento concluído (R$)",
                  cex.names=.9)

    grid(nx=NULL,ny=NA,col="#E5E7EB",lty=1)
    # Redesenha barras sobre a grade.
    barplot(fat,
            names.arg=rep("",length(fat)),
            horiz=TRUE,
            axes=FALSE,
            add=TRUE,
            col="#475569",
            border=NA)

    for(i in seq_along(fat)){
      qtd <- as.integer(x$atendimentos[i])
      part <- if(total>0) fat[i]/total*100 else 0
      rot <- paste0(formata_reais(fat[i]),"  ·  ",qtd," atend.  ·  ",
                    format(round(part,1),nsmall=1,decimal.mark=","),"%")
      xpos <- if(fat[i]>0) min(fat[i]+xmax*.025,xmax*.98) else xmax*.025
      text(xpos,bp[i],labels=rot,pos=4,cex=.78,col="#334155",xpd=NA)
    }
  })

  output$dash_servicos_rank <- renderUI({
    f <- dash_filtro(); wb <- dash_where_barbeiro(f)
    sql <- paste0(
      "SELECT TOP 5 s.nome servico,COUNT(*) quantidade,
              COALESCE(SUM(a.preco),0) faturamento
       FROM atendimentos a
       INNER JOIN servicos s ON s.id=a.servico_id
       WHERE a.barbearia_id=? AND a.data>=? AND a.data<=?
         AND a.status='CONCLUÍDO'",wb$sql,"
       GROUP BY s.id,s.nome
       ORDER BY quantidade DESC,faturamento DESC")
    x <- dbq(sql,params=c(list(f$bid,as.character(f$inicio),as.character(f$fim)),wb$params))
    if(nrow(x)==0) return(div(class="empty-exec","Nenhum serviço concluído no período."))
    totalq <- sum(as.numeric(x$quantidade))
    maxq <- max(as.numeric(x$quantidade))
    tagList(lapply(seq_len(nrow(x)),function(i){
      qtd <- as.numeric(x$quantidade[i])
      pct <- if(totalq>0) qtd/totalq*100 else 0
      div(class="rank-row",
          div(class="rank-top",
              div(class="rank-name",paste0(i,". ",x$servico[i])),
              div(class="rank-value",paste0(qtd,"x"))),
          div(class="rank-sub",
              paste0(formata_reais(as.numeric(x$faturamento[i]))," · ",
                     format(round(pct,1),nsmall=1,decimal.mark=","),"% dos concluídos")),
          div(class="rank-bar",
              div(class="rank-fill",style=paste0("width:",round(if(maxq>0) qtd/maxq*100 else 0,1),"%;"))))
    }))
  })

  output$dash_dias_rank <- renderUI({
    f <- dash_filtro(); wb <- dash_where_barbeiro(f)
    sql <- paste0(
      "SELECT TOP 5 a.data,COUNT(*) quantidade
       FROM atendimentos a
       WHERE a.barbearia_id=? AND a.data>=? AND a.data<=?
         AND a.status<>'CANCELADO'",wb$sql,"
       GROUP BY a.data ORDER BY quantidade DESC,a.data DESC")
    x <- dbq(sql,params=c(list(f$bid,as.character(f$inicio),as.character(f$fim)),wb$params))
    if(nrow(x)==0) return(div(class="empty-exec","Nenhum movimento no período."))
    mx <- max(as.numeric(x$quantidade))
    tagList(lapply(seq_len(nrow(x)),function(i){
      qtd <- as.numeric(x$quantidade[i])
      div(class="rank-row",
          div(class="rank-top",
              div(class="rank-name",format(as.Date(x$data[i]),"%d/%m/%Y")),
              div(class="rank-value",paste0(qtd," atendimento(s)"))),
          div(class="rank-bar",
              div(class="rank-fill",style=paste0("width:",round(qtd/mx*100,1),"%;"))))
    }))
  })

  output$dash_grafico_horarios <- renderPlot({
    f <- dash_filtro(); wb <- dash_where_barbeiro(f)

    # Agrupa por horário e mantém a ordem natural do dia.
    sql <- paste0(
      "SELECT CONVERT(VARCHAR(5),a.hora,108) horario,COUNT(*) quantidade
       FROM atendimentos a
       WHERE a.barbearia_id=? AND a.data>=? AND a.data<=?
         AND a.status<>'CANCELADO'",wb$sql,"
       GROUP BY CONVERT(VARCHAR(5),a.hora,108)
       ORDER BY horario")

    x <- dbq(sql,params=c(
      list(f$bid,as.character(f$inicio),as.character(f$fim)),
      wb$params
    ))

    par(mar=c(4.5,4.5,1,1))

    if(nrow(x)==0){
      plot.new()
      text(.5,.55,"Nenhum horário no período",cex=1)
      return()
    }

    x$quantidade <- as.numeric(x$quantidade)
    pos <- seq_len(nrow(x))
    ymax <- max(x$quantidade,na.rm=TRUE)
    ylim_max <- max(1,ymax*1.25)

    plot(pos,x$quantidade,
         type="o",
         pch=16,
         lwd=2.2,
         col="#475569",
         xaxt="n",
         xlab="Horário",
         ylab="Atendimentos",
         ylim=c(0,ylim_max))

    axis(1,at=pos,labels=x$horario,las=1,cex.axis=.85)
    grid(col="#E5E7EB",lty=1)

    # Reforça a linha/pontos sobre a grade.
    lines(pos,x$quantidade,lwd=2.2,col="#475569")
    points(pos,x$quantidade,pch=16,cex=1.15,col="#334155")

    text(pos,x$quantidade,
         labels=x$quantidade,
         pos=3,
         offset=.55,
         cex=.8,
         font=2,
         col="#334155")
  })

  output$bem_vindo <- renderText({
    bid <- id_barb()
    if(!is.null(estado$usuario) &&
       !is.null(estado$usuario$perfil) &&
       estado$usuario$perfil[1]=="MASTER" && !is.null(bid)) {
      b <- dbq("SELECT nome FROM barbearias WHERE id=? LIMIT 1",params=list(bid))
      if(nrow(b)>0) return(paste("MASTER visualizando:",b$nome[1]))
    }
    if(is.null(estado$usuario)) "Sistema de Gestão da Barbearia"
    else paste("Bem-vindo,",estado$usuario$nome_usuario[1])
  })

  # -------------------------------------------------------
  # CENTRAL DE NOTIFICAÇÕES
  # -------------------------------------------------------
  observeEvent(input$notif_processar,{
    n <- tryCatch(processar_fila_email(50L),error=function(e){
      showNotification(conditionMessage(e),type="error",duration=8); NA_integer_
    })
    notificacoes_upd(notificacoes_upd()+1)
    if(!is.na(n)) showNotification(paste(n,"e-mail(is) enviado(s)."),type="message")
  })

  notif_base <- reactive({
    notificacoes_upd()
    bid <- id_barb(); req(!is.null(bid))
    x <- dbq("
      SELECT n.id,n.criado_em,n.programado_em,n.enviado_em,n.canal,n.tipo,
             n.destinatario,n.status,n.tentativas,n.erro,
             c.nome AS cliente,n.atendimento_id
      FROM notificacoes n
      LEFT JOIN clientes c ON c.id=n.cliente_id
      WHERE n.barbearia_id=?
      ORDER BY n.id DESC",params=list(bid))
    if(nrow(x)>0){
      st <- as.character(input$notif_status %||% "TODOS")
      ca <- as.character(input$notif_canal %||% "TODOS")
      if(st!="TODOS") x <- x[x$status==st,,drop=FALSE]
      if(ca!="TODOS") x <- x[x$canal==ca,,drop=FALSE]
    }
    x
  })

  output$notif_cards <- renderUI({
    bid <- id_barb(); notificacoes_upd()
    if(is.null(bid)) return(NULL)
    x <- dbq("SELECT status,COUNT(*) qtd FROM notificacoes WHERE barbearia_id=? GROUP BY status",
             params=list(bid))
    qtd <- function(s) if(nrow(x)==0 || !s %in% x$status) 0 else sum(x$qtd[x$status==s])
    fluidRow(
      column(3,valueBox(qtd("PENDENTE"),"Pendentes",icon=icon("clock"),color="yellow")),
      column(3,valueBox(qtd("ENVIADA"),"Enviadas",icon=icon("check"),color="green")),
      column(3,valueBox(qtd("ERRO"),"Com erro",icon=icon("warning"),color="red")),
      column(3,valueBox(qtd("PROCESSANDO"),"Processando",icon=icon("send"),color="aqua"))
    )
  })

  output$notif_tabela <- renderDT({
    x <- notif_base()
    if(nrow(x)==0)
      return(datatable(data.frame(),rownames=FALSE,selection="single",
                       options=list(language=list(emptyTable="Nenhuma notificação encontrada"))))
    fmt_dt <- function(z) {
      z <- as.character(z)
      z[is.na(z) | z=="NA"] <- "-"
      z
    }
    y <- data.frame(
      ID=x$id,
      Criado=fmt_dt(x$criado_em),
      Cliente=ifelse(is.na(x$cliente),"-",x$cliente),
      Atendimento=ifelse(is.na(x$atendimento_id),"-",x$atendimento_id),
      Canal=x$canal,
      Tipo=x$tipo,
      Destinatário=x$destinatario,
      Status=x$status,
      Tentativas=x$tentativas,
      Enviado=fmt_dt(x$enviado_em),
      Erro=ifelse(is.na(x$erro),"",x$erro),
      check.names=FALSE
    )
    datatable(y,rownames=FALSE,selection="single",
              options=list(pageLength=10,order=list(list(0,"desc")),
                           scrollX=TRUE,
                           language=list(search="Pesquisar:",
                                         emptyTable="Nenhuma notificação encontrada",
                                         zeroRecords="Nenhuma notificação encontrada",
                                         paginate=list('next'="Próximo",previous="Anterior"))))
  })

  observeEvent(input$notif_reenviar,{
    idx <- input$notif_tabela_rows_selected
    x <- notif_base()
    if(length(idx)!=1 || nrow(x)<idx){
      showNotification("Selecione uma notificação.",type="error");return()
    }
    nid <- as.integer(x$id[idx])
    if(!identical(as.character(x$canal[idx]),"EMAIL")){
      showNotification("O WhatsApp ainda não está integrado.",type="warning");return()
    }
    dbe("UPDATE notificacoes
         SET status='PENDENTE',programado_em=?,processando_em=NULL,enviado_em=NULL,erro=NULL,tentativas=0
         WHERE id=? AND barbearia_id=?",
        params=list(format(Sys.time(),"%Y-%m-%d %H:%M:%S"),nid,id_barb()))
    notificacoes_upd(notificacoes_upd()+1)
    showNotification("Notificação recolocada na fila.",type="message")
  })

  # -------------------------------------------------------
  # CRM / INTELIGÊNCIA DE CLIENTES
  # -------------------------------------------------------
  crm_base <- reactive({
    bid <- id_barb(); crm_upd(); clientes_upd(); agenda_upd()
    req(!is.null(bid))

    # Segmentação:
    # NOVO = somente 1 atendimento concluído
    # RECORRENTE = 2+ atendimentos concluídos
    # SEM_RETORNAR = já concluiu atendimento, mas última visita foi há 45+ dias
    # ATIVO = última visita concluída há menos de 45 dias
    x <- dbq("
      SELECT
        c.id,c.nome,c.telefone,c.email,c.data_cadastro,c.status AS status_cliente,
        COUNT(CASE WHEN a.status='CONCLUÍDO' THEN 1 END) AS visitas,
        MAX(CASE WHEN a.status='CONCLUÍDO' THEN a.data END) AS ultima_visita,
        COALESCE(SUM(CASE WHEN a.status='CONCLUÍDO' THEN a.preco ELSE 0 END),0) AS total_gasto,
        COALESCE(AVG(CASE WHEN a.status='CONCLUÍDO' THEN a.preco END),0) AS ticket_medio,
        COUNT(CASE WHEN a.status='FALTA' THEN 1 END) AS faltas,
        COUNT(CASE WHEN a.status='CANCELADO' THEN 1 END) AS cancelamentos,
        MAX(CASE WHEN a.status='CONCLUÍDO' THEN a.id END) AS ultimo_atendimento_id
      FROM clientes c
      LEFT JOIN atendimentos a
        ON a.cliente_id=c.id AND a.barbearia_id=c.barbearia_id
      WHERE c.barbearia_id=?
      GROUP BY c.id,c.nome,c.telefone,c.email,c.data_cadastro,c.status
      ORDER BY c.nome",params=list(bid))

    if(nrow(x)==0) return(x)

    ult <- dbq("
      SELECT a.id,s.nome AS ultimo_servico,u.nome_usuario AS ultimo_barbeiro
      FROM atendimentos a
      LEFT JOIN servicos s ON s.id=a.servico_id
      LEFT JOIN usuarios u ON u.id=a.barbeiro_id
      WHERE a.barbearia_id=? AND a.status='CONCLUÍDO'",
      params=list(bid))
    if(nrow(ult)>0){
      names(ult)[1] <- "ultimo_atendimento_id"
      x <- merge(x,ult,by="ultimo_atendimento_id",all.x=TRUE,sort=FALSE)
    } else {
      x$ultimo_servico <- NA_character_
      x$ultimo_barbeiro <- NA_character_
    }

    x$visitas <- as.integer(x$visitas)
    x$faltas <- as.integer(x$faltas)
    x$cancelamentos <- as.integer(x$cancelamentos)
    x$total_gasto <- as.numeric(x$total_gasto)
    x$ticket_medio <- as.numeric(x$ticket_medio)
    x$ultima_visita_date <- as.Date(x$ultima_visita)

    hoje <- Sys.Date()
    x$dias_sem_retorno <- ifelse(is.na(x$ultima_visita_date),NA_integer_,
                                 as.integer(hoje-x$ultima_visita_date))
    x$segmento <- ifelse(
      x$visitas==0,"SEM VISITA",
      ifelse(x$dias_sem_retorno>=45,"SEM RETORNAR",
             ifelse(x$visitas==1,"NOVO","RECORRENTE"))
    )
    x
  })

  crm_filtrado <- reactive({
    x <- crm_base()
    if(nrow(x)==0) return(x)

    seg <- as.character(input$crm_segmento %||% "TODOS")
    if(seg=="ATIVO")
      x <- x[x$visitas>0 & !is.na(x$dias_sem_retorno) & x$dias_sem_retorno<45,,drop=FALSE]
    else if(seg=="NOVO")
      x <- x[x$segmento=="NOVO",,drop=FALSE]
    else if(seg=="RECORRENTE")
      x <- x[x$segmento=="RECORRENTE",,drop=FALSE]
    else if(seg=="SEM_RETORNAR")
      x <- x[x$segmento=="SEM RETORNAR",,drop=FALSE]
    else if(seg=="COM_FALTAS")
      x <- x[x$faltas>0,,drop=FALSE]

    busca <- trimws(as.character(input$crm_busca %||% ""))
    if(nzchar(busca) && nrow(x)>0){
      b <- tolower(busca)
      ok <- grepl(b,tolower(ifelse(is.na(x$nome),"",x$nome)),fixed=TRUE) |
            grepl(b,tolower(ifelse(is.na(x$telefone),"",x$telefone)),fixed=TRUE) |
            grepl(b,tolower(ifelse(is.na(x$email),"",x$email)),fixed=TRUE)
      x <- x[ok,,drop=FALSE]
    }
    x
  })

  observeEvent(input$crm_atualizar,{
    crm_upd(crm_upd()+1)
    showNotification("CRM atualizado.",type="message")
  })

  output$crm_cards <- renderUI({
    x <- crm_base()
    total <- nrow(x)
    ativos <- if(total>0) sum(x$visitas>0 & !is.na(x$dias_sem_retorno) & x$dias_sem_retorno<45) else 0
    novos <- if(total>0) sum(x$segmento=="NOVO") else 0
    recorr <- if(total>0) sum(x$segmento=="RECORRENTE") else 0
    semret <- if(total>0) sum(x$segmento=="SEM RETORNAR") else 0

    kpi <- function(label,value,classe=""){
      div(class=paste("exec-kpi",classe),
          div(class="exec-kpi-label",label),
          div(class="exec-kpi-value",format(value,big.mark=".",decimal.mark=",")),
          div(class="exec-kpi-foot",paste0("de ",total," cliente(s) cadastrados")))
    }
    fluidRow(
      column(3,kpi("Clientes ativos",ativos,"kpi-green")),
      column(3,kpi("Novos clientes",novos)),
      column(3,kpi("Clientes recorrentes",recorr,"kpi-purple")),
      column(3,kpi("Sem retornar",semret,"kpi-amber"))
    )
  })

  output$crm_tabela <- renderDT({
    x <- crm_filtrado()
    if(nrow(x)==0)
      return(datatable(data.frame(),rownames=FALSE,selection="single",
                       options=list(language=list(emptyTable="Nenhum cliente encontrado"))))

    y <- x
    y$ultima_visita <- ifelse(is.na(y$ultima_visita_date),"-",
                              format(y$ultima_visita_date,"%d/%m/%Y"))
    y$ultimo_servico <- ifelse(is.na(y$ultimo_servico),"-",as.character(y$ultimo_servico))
    y$segmento <- ifelse(y$segmento=="SEM VISITA","Sem visita",
                  ifelse(y$segmento=="SEM RETORNAR","Sem retornar",
                  ifelse(y$segmento=="RECORRENTE","Recorrente","Novo")))

    y <- y[c("id","nome","visitas","ultima_visita","total_gasto","ticket_medio",
             "faltas","cancelamentos","ultimo_servico","segmento")]
    names(y) <- c("ID","Cliente","Visitas","Última visita","Total gasto","Ticket médio",
                  "Faltas","Cancelamentos","Último serviço","Situação")

    datatable(y,rownames=FALSE,selection="single",
      options=list(pageLength=10,order=list(list(1,"asc")),
        language=list(search="Pesquisar:",lengthMenu="Mostrar _MENU_ clientes",
          info="Mostrando _START_ até _END_ de _TOTAL_ clientes",
          zeroRecords="Nenhum cliente encontrado",emptyTable="Nenhum cliente cadastrado",
          paginate=list(first="Primeiro",last="Último",'next'="Próximo",previous="Anterior")))) |>
      formatCurrency(c("Total gasto","Ticket médio"),"R$ ",digits=2,mark=".",dec=",")
  })

  crm_cliente_selecionado <- reactive({
    sel <- input$crm_tabela_rows_selected
    x <- crm_filtrado()
    if(length(sel)!=1 || nrow(x)<sel) return(NULL)
    x[sel,,drop=FALSE]
  })

  output$crm_ficha <- renderUI({
    c <- crm_cliente_selecionado()
    if(is.null(c))
      return(div(class="empty-exec","Selecione um cliente na tabela acima para visualizar a ficha 360°."))

    ultima <- if(is.na(c$ultima_visita_date[1])) "Nenhuma visita concluída"
              else format(c$ultima_visita_date[1],"%d/%m/%Y")
    serv <- if(is.na(c$ultimo_servico[1])) "-" else as.character(c$ultimo_servico[1])
    barb <- if(is.na(c$ultimo_barbeiro[1])) "-" else as.character(c$ultimo_barbeiro[1])

    tagList(
      h3(as.character(c$nome[1]),style="margin-top:0;font-weight:700;"),
      p(style="color:#6b7280;",
        paste0(ifelse(is.na(c$telefone[1]),"-",c$telefone[1])," · ",
               ifelse(is.na(c$email[1]),"-",c$email[1]))),
      div(class="mini-strip",
        div(class="mini-metric",div(class="mini-label","Visitas concluídas"),
            div(class="mini-value",c$visitas[1])),
        div(class="mini-metric",div(class="mini-label","Total gasto"),
            div(class="mini-value",formata_reais(c$total_gasto[1]))),
        div(class="mini-metric",div(class="mini-label","Ticket médio"),
            div(class="mini-value",formata_reais(c$ticket_medio[1]))),
        div(class="mini-metric",div(class="mini-label","Última visita"),
            div(class="mini-value",style="font-size:17px;",ultima))
      ),
      fluidRow(
        column(4,strong("Último serviço: "),serv),
        column(4,strong("Último barbeiro: "),barb),
        column(4,strong("Faltas / cancelamentos: "),
               paste0(c$faltas[1]," / ",c$cancelamentos[1]))
      ),
      hr()
    )
  })

  output$crm_historico <- renderDT({
    c <- crm_cliente_selecionado()
    if(is.null(c)) return(datatable(data.frame(),rownames=FALSE))
    bid <- id_barb()

    h <- dbq("
      SELECT a.data,a.hora,u.nome_usuario AS barbeiro,s.nome AS servico,
             a.preco,a.status,a.observacao
      FROM atendimentos a
      LEFT JOIN usuarios u ON u.id=a.barbeiro_id
      LEFT JOIN servicos s ON s.id=a.servico_id
      WHERE a.barbearia_id=? AND a.cliente_id=?
      ORDER BY a.data DESC,a.hora DESC,a.id DESC",
      params=list(bid,as.integer(c$id[1])))

    if(nrow(h)>0){
      h$data <- format(as.Date(h$data),"%d/%m/%Y")
      h$hora <- substr(as.character(h$hora),1,5)
      h$barbeiro <- ifelse(is.na(h$barbeiro),"-",h$barbeiro)
      h$servico <- ifelse(is.na(h$servico),"-",h$servico)
      h$observacao <- ifelse(is.na(h$observacao)|trimws(as.character(h$observacao))=="","-",h$observacao)
      names(h) <- c("Data","Horário","Barbeiro","Serviço","Valor","Status","Observação")
    }
    datatable(h,rownames=FALSE,options=list(pageLength=8,order=list(),
      language=list(emptyTable="Este cliente ainda não possui atendimentos",
                    zeroRecords="Nenhum atendimento encontrado",
                    paginate=list('next'="Próximo",previous="Anterior")))) |>
      formatCurrency("Valor","R$ ",digits=2,mark=".",dec=",")
  })

  # -------------------------------------------------------
  # CLIENTES
  # -------------------------------------------------------
  output$clientes_kpis <- renderUI({
    bid <- id_barb(); clientes_upd(); cadastros_upd()
    req(!is.null(bid))
    x <- dbq("
      SELECT
        COUNT(*) total,
        SUM(CASE WHEN c.status='ATIVO' THEN 1 ELSE 0 END) ativos,
        SUM(CASE WHEN u.id IS NOT NULL AND u.senha_hash IS NOT NULL AND u.ativo=1 THEN 1 ELSE 0 END) com_acesso,
        SUM(CASE WHEN u.id IS NOT NULL AND u.senha_hash IS NULL THEN 1 ELSE 0 END) pendentes
      FROM clientes c
      LEFT JOIN usuarios u ON u.cliente_id=c.id AND u.perfil='CLIENTE'
      WHERE c.barbearia_id=?",params=list(bid))
    val <- function(n){
      z <- suppressWarnings(as.integer(x[[n]][1]))
      if(length(z)==0 || is.na(z)) 0L else z
    }
    kpi <- function(classe,label,valor,nota)
      div(class=paste("clients-kpi",classe),
          div(class="clients-kpi-label",label),
          div(class="clients-kpi-value",format(valor,big.mark=".",decimal.mark=",")),
          div(class="clients-kpi-note",nota))
    div(class="clients-kpis",
        kpi("total","Clientes cadastrados",val("total"),"Base operacional da barbearia"),
        kpi("active","Clientes ativos",val("ativos"),"Cadastros com status ativo"),
        kpi("access","Acesso à plataforma",val("com_acesso"),"Clientes com acesso ativo"),
        kpi("pending","Ativação pendente",val("pendentes"),"Aguardando definição de senha"))
  })

  output$tabela_clientes <- renderDT({
    bid<-id_barb(); clientes_upd()
    if(is.null(bid))return(datatable(data.frame(),rownames=FALSE))
    x<-dbq("
      SELECT c.id,c.nome,c.telefone,c.email,c.data_cadastro,c.status AS status_cliente,
             a.id AS atendimento_id,a.data AS data_atendimento,a.hora AS hora_atendimento,
             s.nome AS servico,a.status AS status_atendimento,
             CASE WHEN u.id IS NULL THEN 'SEM ACESSO'
                  WHEN u.senha_hash IS NULL THEN 'ATIVAÇÃO PENDENTE'
                  WHEN u.ativo=1 THEN 'ATIVO' ELSE 'BLOQUEADO' END AS acesso
      FROM clientes c
      LEFT JOIN atendimentos a ON a.id=(
        SELECT MAX(a2.id) FROM atendimentos a2
        WHERE a2.cliente_id=c.id AND a2.barbearia_id=?)
      LEFT JOIN servicos s ON s.id=a.servico_id AND s.barbearia_id=?
      LEFT JOIN usuarios u ON u.cliente_id=c.id AND u.perfil='CLIENTE'
      WHERE c.barbearia_id=? ORDER BY c.id DESC",
      params=list(bid,bid,bid))

    status_select <- function(id,status) {
      if(is.na(id) || is.null(id)) return("-")
      op <- c("AGENDADO","CONCLUÍDO","FALTA","CANCELADO")
      opts <- paste0("<option value='",op,"'",
                     ifelse(op==status," selected",""),">",op,"</option>",
                     collapse="")
      paste0("<select class='status-atendimento form-control input-sm' ",
             "data-atendimento-id='",id,"' style='min-width:125px'>",opts,"</select>")
    }

    if(nrow(x)>0){
      x$data_cadastro <- ifelse(is.na(x$data_cadastro)|trimws(as.character(x$data_cadastro))=="",
                                "-",format(as.Date(x$data_cadastro),"%d/%m/%Y"))
      x$data_atendimento <- ifelse(is.na(x$data_atendimento)|trimws(as.character(x$data_atendimento))=="",
                                   "-",format(as.Date(x$data_atendimento),"%d/%m/%Y"))
      x$hora_atendimento <- ifelse(is.na(x$hora_atendimento),"-",substr(as.character(x$hora_atendimento),1,5))
      x$servico <- ifelse(is.na(x$servico),"-",as.character(x$servico))
      x$status_atendimento <- mapply(status_select,x$atendimento_id,x$status_atendimento,
                                    USE.NAMES=FALSE)
      x$email <- ifelse(is.na(x$email)|trimws(as.character(x$email))=="","-",as.character(x$email))

      badge_cliente <- function(z){
        z <- as.character(z)
        cls <- ifelse(z=="ATIVO","ok",ifelse(z=="BLOQUEADO","blocked","neutral"))
        paste0("<span class='client-badge ",cls,"'>",z,"</span>")
      }
      badge_acesso <- function(z){
        z <- as.character(z)
        cls <- ifelse(z=="ATIVO","ok",
               ifelse(z=="ATIVAÇÃO PENDENTE","pending",
               ifelse(z=="BLOQUEADO","blocked","neutral")))
        paste0("<span class='client-badge ",cls,"'>",z,"</span>")
      }
      x$status_cliente <- vapply(x$status_cliente,badge_cliente,character(1))
      x$acesso <- vapply(x$acesso,badge_acesso,character(1))
    }

    x<-x[c("id","nome","telefone","email","data_cadastro","status_cliente",
           "data_atendimento","hora_atendimento","servico","status_atendimento","acesso")]
    names(x)<-c("ID","Cliente","Telefone","E-mail","Cadastro","Situação do cliente",
                "Último atendimento","Horário","Serviço","Status do atendimento","Acesso à plataforma")

    cb <- JS("
      table.on('change', 'select.status-atendimento', function(){
        Shiny.setInputValue('cliente_status_dropdown', {
          atendimento_id: parseInt($(this).attr('data-atendimento-id')),
          status: $(this).val(),
          nonce: Math.random()
        }, {priority: 'event'});
      });
    ")

    datatable(x,rownames=FALSE,selection="single",escape=FALSE,callback=cb,
      editable=list(target="cell",disable=list(columns=c(0,1,2,3,4,6,7,8,9,10))),
      options=list(pageLength=10,language=list(
        search="Pesquisar:",lengthMenu="Mostrar _MENU_ registros",
        info="Mostrando _START_ até _END_ de _TOTAL_ registros",
        zeroRecords="Nenhum cliente encontrado",emptyTable="Nenhum cliente cadastrado",
        paginate=list(first="Primeiro",last="Último",'next'="Próximo",previous="Anterior"))))
  })


  # -------------------------------------------------------
  # CADASTROS ONLINE
  # -------------------------------------------------------
  output$cadastros_online_kpis <- renderUI({
    bid <- id_barb(); cadastros_upd(); clientes_upd()
    req(!is.null(bid))
    x <- dbq("
      SELECT
        COUNT(*) total,
        SUM(CASE WHEN cc.status<>'CONVERTIDO' OR cc.status IS NULL THEN 1 ELSE 0 END) aguardando,
        SUM(CASE WHEN cc.status='CONVERTIDO' THEN 1 ELSE 0 END) convertidos,
        SUM(CASE WHEN u.id IS NOT NULL AND u.senha_hash IS NOT NULL AND u.ativo=1 THEN 1 ELSE 0 END) acesso_ativo
      FROM cadastros_clientes cc
      LEFT JOIN usuarios u ON u.id=cc.usuario_id AND u.perfil='CLIENTE'
      WHERE cc.barbearia_id=?",params=list(bid))
    val <- function(n){ z<-suppressWarnings(as.integer(x[[n]][1])); if(length(z)==0 || is.na(z)) 0L else z }
    kpi <- function(classe,label,valor,nota)
      div(class=paste("online-kpi",classe),div(class="online-kpi-label",label),
          div(class="online-kpi-value",format(valor,big.mark=".",decimal.mark=",")),div(class="online-kpi-note",nota))
    div(class="online-kpis",
        kpi("total","Cadastros online",val("total"),"Contas criadas pela página pública"),
        kpi("waiting","Aguardando 1º agendamento",val("aguardando"),"Ainda fora da base operacional"),
        kpi("converted","Convertidos em clientes",val("convertidos"),"Já realizaram o primeiro agendamento"),
        kpi("access","Acesso ativo",val("acesso_ativo"),"Contas aptas para login"))
  })

  output$tabela_cadastros_clientes <- renderDT({
    bid <- id_barb()
    cadastros_upd()
    if(is.null(bid)) return(datatable(data.frame(),rownames=FALSE))

    x <- dbq("
      SELECT cc.id,cc.nome,cc.telefone,cc.email,cc.data_cadastro,cc.status,
             cc.cliente_id,
             CASE WHEN u.id IS NULL THEN 'SEM CONTA'
                  WHEN u.senha_hash IS NULL THEN 'ATIVAÇÃO PENDENTE'
                  WHEN u.ativo=1 THEN 'ATIVO' ELSE 'BLOQUEADO' END AS acesso
      FROM cadastros_clientes cc
      LEFT JOIN usuarios u ON u.id=cc.usuario_id AND u.perfil='CLIENTE'
      WHERE cc.barbearia_id=?
      ORDER BY cc.id DESC",params=list(bid))

    if(nrow(x)>0) {
      # CORREÇÃO: operador vetorizado "|" (com "||" dá erro no R >= 4.3).
      x$data_cadastro <- ifelse(is.na(x$data_cadastro)|trimws(as.character(x$data_cadastro))=="","-",
                                format(as.Date(x$data_cadastro),"%d/%m/%Y"))
      x$status <- ifelse(x$status=="CONVERTIDO","CONVERTIDO EM CLIENTE","CADASTRADO")
      x$cliente_id <- ifelse(is.na(x$cliente_id),"-",as.character(x$cliente_id))
      badge_status <- function(z){
        z<-as.character(z); cls<-ifelse(z=="CONVERTIDO EM CLIENTE","ok","pending")
        paste0("<span class='online-badge ",cls,"'>",z,"</span>")
      }
      badge_acesso <- function(z){
        z<-as.character(z); cls<-ifelse(z=="ATIVO","ok",ifelse(z=="ATIVAÇÃO PENDENTE","pending",ifelse(z=="BLOQUEADO","blocked","neutral")))
        paste0("<span class='online-badge ",cls,"'>",z,"</span>")
      }
      x$status <- vapply(x$status,badge_status,character(1))
      x$acesso <- vapply(x$acesso,badge_acesso,character(1))
    }

    x <- x[c("id","nome","telefone","email","data_cadastro","status","cliente_id","acesso")]
    names(x) <- c("ID","Cliente","Telefone","E-mail","Cadastro","Situação","Cliente ID","Acesso à plataforma")

    datatable(x,rownames=FALSE,selection="single",options=list(pageLength=10,language=list(
      search="Pesquisar:",lengthMenu="Mostrar _MENU_ registros",
      info="Mostrando _START_ até _END_ de _TOTAL_ registros",
      zeroRecords="Nenhum cadastro encontrado",emptyTable="Nenhum cadastro online",
      paginate=list(first="Primeiro",last="Último",'next'="Próximo",previous="Anterior"))))
  })

  observeEvent(input$tabela_clientes_cell_edit,{
    bid<-id_barb(); info<-input$tabela_clientes_cell_edit
    if(is.null(bid) || length(info$col)==0 || info$col[1]!=5) return()
    x<-dbq("SELECT id,status FROM clientes WHERE barbearia_id=? ORDER BY id DESC",
           params=list(bid))
    if(length(info$row)==0 || info$row[1]<1 || info$row[1]>nrow(x)) return()
    r<-x[info$row[1],]
    st<-toupper(trimws(as.character(info$value)))
    if(!st%in%c("ATIVO","INATIVO")){
      showNotification("Use apenas ATIVO ou INATIVO.",type="error")
      clientes_upd(clientes_upd()+1);return()
    }
    dbe("UPDATE clientes SET status=? WHERE id=? AND barbearia_id=?",
        params=list(st,r$id,bid))
    clientes_upd(clientes_upd()+1)
  })

  # O dropdown atualiza somente o atendimento cujo ID está na própria linha.
  # Assim o histórico de FALTA/CONCLUÍDO/AGENDADO de outros dias não é sobrescrito.
  observeEvent(input$cliente_status_dropdown,{
    bid <- id_barb()
    ev <- input$cliente_status_dropdown
    if(is.null(bid) || is.null(ev$atendimento_id) || is.null(ev$status)) return()

    aid <- suppressWarnings(as.integer(ev$atendimento_id))
    st <- toupper(trimws(as.character(ev$status)))
    if(is.na(aid) || !st%in%c("AGENDADO","CONCLUÍDO","FALTA","CANCELADO")) return()

    a <- dbq("SELECT id,data,hora,barbeiro_id,status FROM atendimentos
              WHERE id=? AND barbearia_id=? LIMIT 1",params=list(aid,bid))
    if(nrow(a)==0){
      showNotification("Atendimento não encontrado.",type="error")
      clientes_upd(clientes_upd()+1);return()
    }

    if(st!="CANCELADO" && !is.na(a$barbeiro_id[1])){
      bloqueado <- horario_bloqueado(bid,as.integer(a$barbeiro_id[1]),
                                     as.character(a$data[1]),
                                     substr(as.character(a$hora[1]),1,5))
      # Um bloqueio criado depois do agendamento não impede marcar o atendimento
      # já existente como CONCLUÍDO/FALTA. Ele serve para impedir novos agendamentos.
      cfl <- dbq("SELECT id FROM atendimentos
                  WHERE barbearia_id=? AND barbeiro_id=? AND data=? AND hora=?
                    AND status<>'CANCELADO' AND id<>? LIMIT 1",
                 params=list(bid,a$barbeiro_id[1],as.character(a$data[1]),
                             as.character(a$hora[1]),aid))
      if(nrow(cfl)>0){
        showNotification("Esse barbeiro já possui outro atendimento nesse horário.",type="error")
        clientes_upd(clientes_upd()+1);return()
      }
    }

    anterior <- as.character(a$status[1])
    dbe("UPDATE atendimentos SET status=? WHERE id=? AND barbearia_id=?",
        params=list(st,aid,bid))
    registrar_historico(bid,"ALTERACAO","ATENDIMENTO","status",anterior,st)
    clientes_upd(clientes_upd()+1)
    agenda_upd(agenda_upd()+1)
    showNotification(paste("Status do atendimento",aid,"alterado para",st),type="message")
  })


  # -------------------------------------------------------
  # EDIÇÃO DE CLIENTE + REMARCAÇÃO DO ATENDIMENTO EXIBIDO
  # -------------------------------------------------------
  horarios_edicao_atendimento <- function(bid,barbeiro_id,data,aid){
    if(is.na(barbeiro_id) || is.na(as.Date(data))) return(character(0))

    h <- funcionamento_dia(bid,data)
    j <- jornada_dia(bid,barbeiro_id,data)
    slots <- character(0)
    if(nrow(h)>0 && as.integer(h$aberto[1])==1 &&
       nrow(j)>0 && as.integer(j$trabalha[1])==1){
      ab <- max(substr(as.character(h$hora_abertura[1]),1,5),
                substr(as.character(j$hora_inicio[1]),1,5))
      fe <- min(substr(as.character(h$hora_fechamento[1]),1,5),
                substr(as.character(j$hora_fim[1]),1,5))
      hm <- function(x){p<-as.integer(strsplit(x,":",fixed=TRUE)[[1]]);p[1]*60+p[2]}
      if(hm(fe)>hm(ab)){
        m <- seq(hm(ab),hm(fe)-1,by=30)
        slots <- sprintf("%02d:%02d",m%/%60,m%%60)
        slots <- slots[slots<fe]
      }
    }

    if(length(slots)>0){
      bl <- dbq("SELECT hora_inicio,hora_fim FROM bloqueios_agenda
                 WHERE barbearia_id=? AND data=?
                   AND status='ATIVO'
                   AND (barbeiro_id IS NULL OR barbeiro_id=?)",
                params=list(bid,as.character(data),barbeiro_id))
      if(nrow(bl)>0) for(i in seq_len(nrow(bl))){
        hi<-substr(as.character(bl$hora_inicio[i]),1,5)
        hf<-substr(as.character(bl$hora_fim[i]),1,5)
        slots<-slots[!(slots>=hi & slots<hf)]
      }

      oc <- dbq("SELECT hora FROM atendimentos
                 WHERE barbearia_id=? AND barbeiro_id=? AND data=?
                   AND status<>'CANCELADO' AND id<>?",
                params=list(bid,barbeiro_id,as.character(data),aid))
      if(nrow(oc)>0)
        slots <- slots[!slots %in% substr(as.character(oc$hora),1,5)]
    }
    slots
  }

  observeEvent(input$editar_cliente,{
    bid<-id_barb()
    idx<-input$tabela_clientes_rows_selected
    if(is.null(bid)||length(idx)==0){
      showNotification("Selecione um cliente na tabela.",type="error");return()
    }

    # Usa exatamente a mesma regra da tabela Clientes: último atendimento pelo ID.
    x<-dbq("
      SELECT c.id cliente_id,c.nome,c.telefone,c.email,c.status status_cliente,
             a.id atendimento_id,a.data data_atendimento,a.hora hora_atendimento,
             a.servico_id,a.barbeiro_id,a.status status_atendimento
      FROM clientes c
      LEFT JOIN atendimentos a ON a.id=(
        SELECT MAX(a2.id) FROM atendimentos a2
        WHERE a2.cliente_id=c.id AND a2.barbearia_id=?)
      WHERE c.barbearia_id=? ORDER BY c.id DESC",
      params=list(bid,bid))
    if(idx[1]>nrow(x)) return()
    r<-x[idx[1],]

    estado$edit_cliente_id<-as.integer(r$cliente_id[1])
    estado$edit_atendimento_id<-if(is.na(r$atendimento_id[1])) NULL else as.integer(r$atendimento_id[1])

    serv<-dbq("SELECT id,nome,preco FROM servicos
               WHERE barbearia_id=? AND status='ATIVO' ORDER BY nome",params=list(bid))
    sch<-if(nrow(serv)>0) setNames(as.character(serv$id),
      paste0(serv$nome," - R$ ",format(serv$preco,nsmall=2,decimal.mark=","))) else character(0)

    barbs<-dbq("SELECT id,nome_usuario,ativo FROM usuarios
                WHERE barbearia_id=? AND perfil='BARBEIRO'
                ORDER BY ativo DESC,nome_usuario",params=list(bid))
    bch<-if(nrow(barbs)>0) setNames(as.character(barbs$id),
      ifelse(barbs$ativo==1,barbs$nome_usuario,paste0(barbs$nome_usuario," (desativado)"))) else character(0)

    atui<-if(is.null(estado$edit_atendimento_id)) {
      tagList(tags$hr(),p(class="text-muted","Este cliente ainda não possui atendimento para editar."))
    } else {
      tagList(
        tags$hr(),tags$h4("Atendimento selecionado"),
        p(class="text-muted",paste0("Editando o atendimento #",estado$edit_atendimento_id,
          ". A remarcação mantém o mesmo ID e não apaga o histórico de outros atendimentos.")),
        fluidRow(
          column(6,dateInput("ed_at_data","Data do atendimento *",
                 value=as.Date(r$data_atendimento[1]),format="dd/mm/yyyy",language="pt-BR")),
          column(6,selectInput("ed_at_hora","Horário *",choices=character(0)))
        ),
        fluidRow(
          column(6,selectInput("ed_at_servico","Serviço *",choices=sch,
                               selected=as.character(r$servico_id[1]))),
          column(6,selectInput("ed_at_barbeiro","Barbeiro *",choices=bch,
                               selected=as.character(r$barbeiro_id[1])))
        ),
        selectInput("ed_at_status","Status do atendimento",
                    choices=c("AGENDADO","CONCLUÍDO","FALTA","CANCELADO"),
                    selected=as.character(r$status_atendimento[1]))
      )
    }

    showModal(modalDialog(
      title="Editar cliente e atendimento",size="l",easyClose=FALSE,
      textInput("ed_nome","Nome *",as.character(r$nome[1])),
      textInput("ed_tel","Telefone *",as.character(r$telefone[1]),placeholder="(14) 99999-9999"),
      textInput("ed_email","E-mail *",ifelse(is.na(r$email[1]),"",as.character(r$email[1]))),
      selectInput("ed_cliente_status","Status do cliente",c("ATIVO","INATIVO"),
                  selected=as.character(r$status_cliente[1])),
      atui,
      footer=tagList(modalButton("Cancelar"),
                     actionButton("ed_salvar","Salvar alterações",icon=icon("save"),class="btn-primary"))
    ))
  })

  observeEvent(list(input$ed_at_data,input$ed_at_barbeiro,
                    bloqueios_upd(),funcionamento_upd(),jornada_upd(),agenda_upd()),{
    aid<-estado$edit_atendimento_id
    bid<-id_barb()
    if(is.null(aid)||is.null(bid)||is.null(input$ed_at_data)||is.null(input$ed_at_barbeiro)) return()

    barb<-suppressWarnings(as.integer(input$ed_at_barbeiro))
    dat<-as.character(input$ed_at_data)
    slots<-horarios_edicao_atendimento(bid,barb,dat,aid)

    a<-dbq("SELECT data,hora FROM atendimentos WHERE id=? AND barbearia_id=? LIMIT 1",
           params=list(aid,bid))
    atual<-if(nrow(a)>0 && as.character(a$data[1])==dat)
      substr(as.character(a$hora[1]),1,5) else character(0)
    sel<-if(length(atual)>0 && atual %in% slots) atual else if(length(slots)>0) slots[1] else character(0)
    updateSelectInput(session,"ed_at_hora",choices=slots,selected=sel)
  },ignoreInit=FALSE)

  observeEvent(input$ed_salvar,{
    bid<-id_barb(); cid<-estado$edit_cliente_id; aid<-estado$edit_atendimento_id
    if(is.null(bid)||is.null(cid)) return()

    nome<-trimws(as.character(input$ed_nome %||% ""))
    tel<-gsub("[^0-9]","",as.character(input$ed_tel %||% ""))
    email<-tolower(trimws(as.character(input$ed_email %||% "")))
    stc<-toupper(as.character(input$ed_cliente_status %||% "ATIVO"))

    if(nome==""){showNotification("Nome obrigatório.",type="error");return()}
    if(nchar(tel)!=11){showNotification("Telefone deve conter 11 dígitos.",type="error");return()}
    if(!validar_email(email)){showNotification("E-mail inválido.",type="error");return()}

    dup<-dbq("SELECT id FROM clientes WHERE barbearia_id=? AND LOWER(email)=LOWER(?) AND id<>? LIMIT 1",
             params=list(bid,email,cid))
    if(nrow(dup)>0){showNotification("Esse e-mail já pertence a outro cliente.",type="error");return()}

    ca<-dbq("SELECT nome,telefone,email,status FROM clientes WHERE id=? AND barbearia_id=? LIMIT 1",
            params=list(cid,bid))
    if(nrow(ca)==0){showNotification("Cliente não encontrado.",type="error");return()}

    aa<-NULL; serv<-NULL; barb<-NULL
    if(!is.null(aid)){
      aa<-dbq("SELECT id,data,hora,servico_id,barbeiro_id,status,preco,duracao_minutos
               FROM atendimentos WHERE id=? AND barbearia_id=? AND cliente_id=? LIMIT 1",
              params=list(aid,bid,cid))
      if(nrow(aa)==0){showNotification("Atendimento não encontrado.",type="error");return()}

      dat<-as.character(input$ed_at_data)
      hora<-substr(as.character(input$ed_at_hora %||% ""),1,5)
      sid<-suppressWarnings(as.integer(input$ed_at_servico))
      barbeiro_id<-suppressWarnings(as.integer(input$ed_at_barbeiro))
      sta<-toupper(as.character(input$ed_at_status %||% ""))

      if(is.na(as.Date(dat))||!grepl("^[0-9]{2}:[0-9]{2}$",hora)){
        showNotification("Selecione uma data e um horário disponíveis.",type="error");return()
      }
      if(is.na(sid)||is.na(barbeiro_id)){showNotification("Selecione serviço e barbeiro.",type="error");return()}
      if(!sta%in%c("AGENDADO","CONCLUÍDO","FALTA","CANCELADO")){
        showNotification("Status do atendimento inválido.",type="error");return()
      }

      serv<-dbq("SELECT id,nome,preco,duracao_minutos FROM servicos
                 WHERE id=? AND barbearia_id=? AND status='ATIVO' LIMIT 1",params=list(sid,bid))
      barb<-dbq("SELECT id,nome_usuario,ativo FROM usuarios
                 WHERE id=? AND barbearia_id=? AND perfil='BARBEIRO' LIMIT 1",
                params=list(barbeiro_id,bid))
      if(nrow(serv)==0||nrow(barb)==0){showNotification("Serviço ou barbeiro inválido.",type="error");return()}

      mudou_agenda<-as.character(aa$data[1])!=dat ||
                    substr(as.character(aa$hora[1]),1,5)!=hora ||
                    is.na(aa$barbeiro_id[1]) || as.integer(aa$barbeiro_id[1])!=barbeiro_id

      if(mudou_agenda && as.integer(barb$ativo[1])!=1){
        showNotification("Não é possível remarcar para um barbeiro desativado.",type="error");return()
      }

      if(sta!="CANCELADO"){
        if(!horario_dentro_funcionamento(bid,dat,hora)){
          showNotification("O horário está fora do funcionamento da barbearia.",type="error");return()
        }
        if(!horario_dentro_jornada(bid,barbeiro_id,dat,hora)){
          showNotification("O horário está fora da jornada do barbeiro.",type="error");return()
        }
        if(mudou_agenda && nrow(horario_bloqueado(bid,barbeiro_id,dat,hora))>0){
          showNotification("O novo horário está bloqueado.",type="error");return()
        }
        cfl<-dbq("SELECT id FROM atendimentos
                  WHERE barbearia_id=? AND barbeiro_id=? AND data=? AND hora=?
                    AND status<>'CANCELADO' AND id<>? LIMIT 1",
                 params=list(bid,barbeiro_id,dat,hora,aid))
        if(nrow(cfl)>0){showNotification("Esse barbeiro já possui atendimento nesse horário.",type="error");return()}
      }
    }

    tryCatch({
      dbWithTransaction(con,{
        dbe("UPDATE clientes SET nome=?,telefone=?,email=?,status=? WHERE id=? AND barbearia_id=?",
            params=list(nome,tel,email,stc,cid,bid))
        dbe("UPDATE cadastros_clientes SET nome=?,telefone=?,email=? WHERE cliente_id=? AND barbearia_id=?",
            params=list(nome,tel,email,cid,bid))
        u<-dbq("SELECT id FROM usuarios WHERE cliente_id=? AND perfil='CLIENTE' LIMIT 1",params=list(cid))
        if(nrow(u)>0) dbe("UPDATE usuarios SET email=? WHERE id=?",params=list(email,u$id[1]))

        if(!is.null(aid)){
          dbe("UPDATE atendimentos
               SET data=?,hora=?,servico_id=?,barbeiro_id=?,preco=?,duracao_minutos=?,status=?
               WHERE id=? AND barbearia_id=? AND cliente_id=?",
              params=list(dat,hora,sid,barbeiro_id,serv$preco[1],serv$duracao_minutos[1],
                          sta,aid,bid,cid))
        }
      })

      if(!is.null(aid)){
        sa<-dbq("SELECT nome FROM servicos WHERE id=? LIMIT 1",params=list(aa$servico_id[1]))
        ba<-if(is.na(aa$barbeiro_id[1])) data.frame() else
          dbq("SELECT nome_usuario FROM usuarios WHERE id=? LIMIT 1",params=list(aa$barbeiro_id[1]))
        antigos<-c(as.character(aa$data[1]),substr(as.character(aa$hora[1]),1,5),
                   if(nrow(sa)>0) as.character(sa$nome[1]) else as.character(aa$servico_id[1]),
                   if(nrow(ba)>0) as.character(ba$nome_usuario[1]) else "-",
                   as.character(aa$status[1]),as.character(aa$preco[1]))
        novos<-c(dat,hora,as.character(serv$nome[1]),as.character(barb$nome_usuario[1]),
                 sta,as.character(serv$preco[1]))
        campos<-c("data","hora","servico","barbeiro","status","preco")
        for(i in seq_along(campos)) if(!identical(antigos[i],novos[i]))
          registrar_historico(bid,"ALTERACAO","ATENDIMENTO",
                              paste0("atendimento_",aid,"_",campos[i]),antigos[i],novos[i])
      }

      clientes_upd(clientes_upd()+1);agenda_upd(agenda_upd()+1)
      removeModal()
      showNotification("Cliente e atendimento atualizados.",type="message")
    },error=function(e){
      showNotification(paste("Não foi possível salvar:",conditionMessage(e)),type="error",duration=10)
    })
  })

  observeEvent(input$acesso_cliente,{
    bid<-id_barb()
    if(is.null(bid)||length(input$tabela_clientes_rows_selected)==0){
      showNotification("Selecione um cliente.",type="error");return()
    }
    x<-dbq("SELECT id,nome,email FROM clientes WHERE barbearia_id=? ORDER BY id DESC",params=list(bid))
    r<-x[input$tabela_clientes_rows_selected,]
    if(is.na(r$email)||r$email==""){showNotification("Informe o e-mail do cliente primeiro.",type="error");return()}
    a<-tryCatch(criar_acesso_cliente(r$id,r$nome,r$email,bid,TRUE),
                error=function(e)list(ok=FALSE,msg=conditionMessage(e),enviado=FALSE))
    clientes_upd(clientes_upd()+1)
    showNotification(a$msg,type=ifelse(a$enviado,"message","error"),duration=10)
  })

  # -------------------------------------------------------
  # SERVIÇOS
  # MASTER e BARBEIRO podem cadastrar e editar serviços da própria barbearia.
  # Serviços INATIVOS permanecem no histórico, mas não entram em novos agendamentos.
  # -------------------------------------------------------
  output$servicos_kpis <- renderUI({
    bid<-id_barb(); servicos_upd()
    req(!is.null(bid))
    x<-dbq("
      SELECT COUNT(*) total,
             SUM(CASE WHEN status='ATIVO' THEN 1 ELSE 0 END) ativos,
             AVG(CASE WHEN status='ATIVO' THEN preco END) preco_medio
      FROM servicos WHERE barbearia_id=?",params=list(bid))
    num<-function(n){z<-suppressWarnings(as.numeric(x[[n]][1]));if(length(z)==0||is.na(z))0 else z}
    kpi<-function(classe,label,valor,nota)
      div(class=paste("services-kpi",classe),
          div(class="services-kpi-label",label),
          div(class="services-kpi-value",valor),
          div(class="services-kpi-note",nota))
    div(class="services-kpis",
        kpi("total","Serviços cadastrados",format(num("total"),big.mark=".",decimal.mark=","),"Catálogo completo da unidade"),
        kpi("active","Serviços ativos",format(num("ativos"),big.mark=".",decimal.mark=","),"Disponíveis para novos agendamentos"),
        kpi("ticket","Preço médio",formata_reais(num("preco_medio")),"Média dos serviços ativos"))
  })

  output$tabela_servicos <- renderDT({
    bid<-id_barb();servicos_upd()
    if(is.null(bid))return(datatable(data.frame(),rownames=FALSE))
    x<-dbq("SELECT id,nome,preco,duracao_minutos,status
           FROM servicos WHERE barbearia_id=? ORDER BY
           CASE WHEN status='ATIVO' THEN 0 ELSE 1 END,nome,id",params=list(bid))
    if(nrow(x)>0){
      x$status<-ifelse(x$status=="ATIVO",
        "<span class='service-badge ok'>ATIVO</span>",
        "<span class='service-badge off'>INATIVO</span>")
    }
    names(x)<-c("ID","Serviço","Preço","Duração (min)","Situação")
    datatable(x,rownames=FALSE,escape=FALSE,selection="single",
      options=list(pageLength=10,language=list(
        search="Pesquisar:",lengthMenu="Mostrar _MENU_ registros",
        info="Mostrando _START_ até _END_ de _TOTAL_ registros",
        zeroRecords="Nenhum serviço encontrado",emptyTable="Nenhum serviço cadastrado",
        paginate=list(first="Primeiro",last="Último",'next'="Próximo",previous="Anterior")))) |>
      formatCurrency(columns="Preço",currency="R$ ",digits=2,mark=".",dec=",")
  })

  abrir_modal_servico <- function(id=NULL){
    bid<-id_barb()
    if(is.null(bid)) return(invisible(NULL))
    editar<-!is.null(id) && !is.na(id)
    if(editar){
      x<-dbq("SELECT id,nome,preco,duracao_minutos,status FROM servicos
              WHERE id=? AND barbearia_id=? LIMIT 1",params=list(id,bid))
      if(nrow(x)==0){showNotification("Serviço não encontrado.",type="error");return(invisible(NULL))}
      nome<-as.character(x$nome[1]); preco<-as.numeric(x$preco[1])
      duracao<-as.integer(x$duracao_minutos[1]); status<-as.character(x$status[1])
    } else {
      nome<-""; preco<-0; duracao<-30L; status<-"ATIVO"
    }
    showModal(modalDialog(
      title=if(editar) "Editar serviço" else "Cadastrar novo serviço",
      textInput("servico_form_nome","Nome do serviço *",value=nome,
                placeholder="Ex.: Corte degradê"),
      fluidRow(
        column(6,numericInput("servico_form_preco","Preço *",value=preco,min=0,step=.01)),
        column(6,numericInput("servico_form_duracao","Duração em minutos *",value=duracao,min=5,step=5))
      ),
      selectInput("servico_form_status","Situação",
                  choices=c("Ativo"="ATIVO","Inativo"="INATIVO"),selected=status),
      div(style="font-size:12px;color:#64748b;",
          icon("info-circle"),
          " Serviços inativos deixam de aparecer em novos agendamentos, mas o histórico dos atendimentos é preservado."),
      footer=tagList(
        modalButton("Cancelar"),
        actionButton("servico_form_salvar","Salvar serviço",icon=icon("save"),class="btn-primary")
      ),easyClose=FALSE
    ))
    session$userData$servico_edicao_id<-if(editar) as.integer(id) else NA_integer_
  }

  observeEvent(input$servico_novo,{
    perfil<-if(is.null(estado$usuario)) "" else as.character(estado$usuario$perfil[1])
    if(!perfil %in% c("MASTER","BARBEIRO")) return()
    abrir_modal_servico()
  })

  observeEvent(input$servico_editar,{
    perfil<-if(is.null(estado$usuario)) "" else as.character(estado$usuario$perfil[1])
    if(!perfil %in% c("MASTER","BARBEIRO")) return()
    sel<-input$tabela_servicos_rows_selected
    if(length(sel)==0){showNotification("Selecione um serviço para editar.",type="warning");return()}
    bid<-id_barb()
    ids<-dbq("SELECT id FROM servicos WHERE barbearia_id=? ORDER BY
              CASE WHEN status='ATIVO' THEN 0 ELSE 1 END,nome,id",params=list(bid))
    if(nrow(ids)<sel[1]) return()
    abrir_modal_servico(as.integer(ids$id[sel[1]]))
  })

  observeEvent(input$servico_form_salvar,{
    perfil<-if(is.null(estado$usuario)) "" else as.character(estado$usuario$perfil[1])
    if(!perfil %in% c("MASTER","BARBEIRO")) return()
    bid<-id_barb(); req(!is.null(bid))
    nome<-trimws(as.character(input$servico_form_nome))
    preco<-suppressWarnings(as.numeric(input$servico_form_preco))
    duracao<-suppressWarnings(as.integer(input$servico_form_duracao))
    status<-as.character(input$servico_form_status)
    if(!nzchar(nome)){showNotification("Informe o nome do serviço.",type="error");return()}
    if(is.na(preco)||preco<0){showNotification("Informe um preço válido.",type="error");return()}
    if(is.na(duracao)||duracao<5){showNotification("A duração deve ser de pelo menos 5 minutos.",type="error");return()}
    if(!status %in% c("ATIVO","INATIVO")) status<-"ATIVO"

    sid<-session$userData$servico_edicao_id
    dup<-if(is.null(sid)||is.na(sid))
      dbq("SELECT id FROM servicos WHERE barbearia_id=? AND LOWER(LTRIM(RTRIM(nome)))=LOWER(?) LIMIT 1",
          params=list(bid,nome))
    else
      dbq("SELECT id FROM servicos WHERE barbearia_id=? AND LOWER(LTRIM(RTRIM(nome)))=LOWER(?) AND id<>? LIMIT 1",
          params=list(bid,nome,sid))
    if(nrow(dup)>0){showNotification("Já existe um serviço com esse nome nesta barbearia.",type="error");return()}

    if(is.null(sid)||is.na(sid)){
      dbe("INSERT INTO servicos(nome,preco,duracao_minutos,status,barbearia_id)
           VALUES(?,?,?,?,?)",params=list(nome,preco,duracao,status,bid))
      msg<-"Serviço cadastrado com sucesso."
    } else {
      dbe("UPDATE servicos SET nome=?,preco=?,duracao_minutos=?,status=?
           WHERE id=? AND barbearia_id=?",params=list(nome,preco,duracao,status,sid,bid))
      msg<-"Serviço atualizado com sucesso."
    }
    removeModal()
    session$userData$servico_edicao_id<-NA_integer_
    servicos_upd(servicos_upd()+1)
    agenda_upd(agenda_upd()+1)
    showNotification(msg,type="message")
  })

  # -------------------------------------------------------
  # AGENDA - INDICADORES DO DIA
  # -------------------------------------------------------
  output$agenda_kpis <- renderUI({
    bid <- id_barb(); agenda_upd()
    req(!is.null(bid))
    hoje <- as.character(Sys.Date())
    x <- dbq("
      SELECT
        COUNT(*) total,
        SUM(CASE WHEN status='AGENDADO' THEN 1 ELSE 0 END) agendados,
        SUM(CASE WHEN status='CONCLUÍDO' THEN 1 ELSE 0 END) concluidos,
        SUM(CASE WHEN status='FALTA' THEN 1 ELSE 0 END) faltas
      FROM atendimentos
      WHERE barbearia_id=? AND data=? AND status<>'CANCELADO'",
      params=list(bid,hoje))
    val <- function(n) {
      z <- suppressWarnings(as.integer(x[[n]][1]))
      if(length(z)==0 || is.na(z)) 0L else z
    }
    div(class="agenda-kpis",
        div(class="agenda-kpi",div(class="ak-label","Agendados hoje"),div(class="ak-value",val("agendados"))),
        div(class="agenda-kpi",div(class="ak-label","Concluídos hoje"),div(class="ak-value",val("concluidos"))),
        div(class="agenda-kpi",div(class="ak-label","Faltas hoje"),div(class="ak-value",val("faltas"))),
        div(class="agenda-kpi",div(class="ak-label","Atendimentos do dia"),div(class="ak-value",val("total"))))
  })

  # -------------------------------------------------------
  # AGENDA
  # -------------------------------------------------------
  output$tabela_agenda <- renderDT({
    bid<-id_barb();agenda_upd()
    if(is.null(bid))return(datatable(data.frame(),rownames=FALSE))
    filtro <- as.character(input$agenda_filtro_barbeiro %||% "TODOS")
    params <- list(bid)
    cond <- ""
    if(filtro!="TODOS" && grepl("^[0-9]+$",filtro)){
      cond <- " AND a.barbeiro_id=? "
      params <- list(bid,as.integer(filtro))
    }
    x<-dbq(paste0("
      SELECT a.id,c.nome cliente,c.telefone,s.nome servico,
             COALESCE(u.nome_usuario,'Não informado') barbeiro,
             a.data,a.hora,a.preco,a.duracao_minutos,a.status,a.observacao
      FROM atendimentos a
      JOIN clientes c ON c.id=a.cliente_id AND c.barbearia_id=a.barbearia_id
      JOIN servicos s ON s.id=a.servico_id AND s.barbearia_id=a.barbearia_id
      LEFT JOIN usuarios u ON u.id=a.barbeiro_id AND u.barbearia_id=a.barbearia_id
                           AND u.perfil='BARBEIRO'
      WHERE a.barbearia_id=?",cond,"
      ORDER BY a.data DESC,a.hora DESC,a.id DESC"),params=params)
    if(nrow(x)>0){
      x$data<-format(as.Date(x$data),"%d/%m/%Y")
      x$hora<-substr(as.character(x$hora),1,5)
    }
    datatable(x,rownames=FALSE,editable=list(target="cell",
      disable=list(columns=c(0,1,2,3,4,5,6,7,8,10))),
      options=list(pageLength=15,language=list(
        search="Pesquisar:",lengthMenu="Mostrar _MENU_ registros",
        info="Mostrando _START_ até _END_ de _TOTAL_ registros",
        zeroRecords="Nenhum atendimento encontrado",emptyTable="Nenhum atendimento cadastrado",
        paginate=list(first="Primeiro",last="Último",'next'="Próximo",previous="Anterior")))) |>
      formatCurrency(columns="preco",currency="R$ ",digits=2,mark=".",dec=",")
  })

  observeEvent(input$tabela_agenda_cell_edit,{
    bid<-id_barb();info<-input$tabela_agenda_cell_edit
    if(is.null(bid) || info$col!=9)return()
    st<-toupper(trimws(as.character(info$value)))
    if(!st%in%c("AGENDADO","CONCLUÍDO","FALTA","CANCELADO")){
      showNotification("Use AGENDADO, CONCLUÍDO, FALTA ou CANCELADO.",type="error")
      agenda_upd(agenda_upd()+1);return()
    }
    filtro <- as.character(input$agenda_filtro_barbeiro %||% "TODOS")
    params <- list(bid)
    cond <- ""
    if(filtro!="TODOS" && grepl("^[0-9]+$",filtro)){
      cond <- " AND barbeiro_id=? "
      params <- list(bid,as.integer(filtro))
    }
    x<-dbq(paste0("SELECT id,data,hora,barbeiro_id FROM atendimentos
                   WHERE barbearia_id=?",cond,"
                   ORDER BY data DESC,hora DESC,id DESC"),params=params)
    if(info$row<1||info$row>nrow(x))return()
    r<-x[info$row,]
    if(st!="CANCELADO" && !is.na(r$barbeiro_id)){
      cfl<-dbq("SELECT id FROM atendimentos
                WHERE barbearia_id=? AND barbeiro_id=? AND data=? AND hora=?
                  AND status<>'CANCELADO' AND id<>? LIMIT 1",
               params=list(bid,r$barbeiro_id,as.character(r$data),as.character(r$hora),r$id))
      if(nrow(cfl)>0){
        showNotification("Esse barbeiro já possui atendimento nesse horário.",type="error")
        agenda_upd(agenda_upd()+1);return()
      }
    }
    dbe("UPDATE atendimentos SET status=? WHERE id=? AND barbearia_id=?",
        params=list(st,r$id,bid))
    agenda_upd(agenda_upd()+1);clientes_upd(clientes_upd()+1)
  })

  # -------------------------------------------------------
  # FINANCEIRO
  # -------------------------------------------------------
  output$gasto_total <- renderText({
    q<-if(is.null(input$gasto_qtd)||is.na(input$gasto_qtd))0 else input$gasto_qtd
    v<-if(is.null(input$gasto_unit)||is.na(input$gasto_unit))0 else input$gasto_unit
    formata_reais(q*v)
  })

  observeEvent(input$salvar_gasto,{
    bid<-id_barb()
    if(is.null(bid))return()
    if(trimws(input$gasto_desc)==""){showNotification("Preencha a descrição.",type="error");return()}
    if(is.null(input$gasto_qtd)||is.na(input$gasto_qtd)||input$gasto_qtd<=0){
      showNotification("Quantidade deve ser maior que zero.",type="error");return()}
    if(is.null(input$gasto_unit)||is.na(input$gasto_unit)||input$gasto_unit<=0){
      showNotification("Valor unitário deve ser maior que zero.",type="error");return()}
    total<-input$gasto_qtd*input$gasto_unit
    dbe("
      INSERT INTO gastos(data,descricao,categoria,quantidade,valor_unitario,valor_total,barbearia_id)
      VALUES(?,?,?,?,?,?,?)",
      params=list(as.character(input$gasto_data),trimws(input$gasto_desc),
                  input$gasto_categoria,input$gasto_qtd,input$gasto_unit,total,bid))
    gastos_upd(gastos_upd()+1)
    updateTextInput(session,"gasto_desc",value="")
    updateNumericInput(session,"gasto_qtd",value=1)
    updateNumericInput(session,"gasto_unit",value=0)
    showNotification("Gasto cadastrado.",type="message")
  })

  output$tabela_gastos <- renderDT({
    bid<-id_barb();gastos_upd()
    if(is.null(bid))return(datatable(data.frame(),rownames=FALSE))
    periodo <- input$fin_periodo
    if(is.null(periodo) || length(periodo)<2 || any(is.na(periodo))) {
      fim <- Sys.Date(); inicio <- as.Date(format(fim,"%Y-%m-01"))
    } else {
      inicio <- as.Date(periodo[1]); fim <- as.Date(periodo[2])
    }
    x<-dbq("SELECT id,data,descricao,categoria,quantidade,valor_unitario,valor_total
                       FROM gastos WHERE barbearia_id=? AND data>=? AND data<=?
                       ORDER BY data DESC,id DESC",
           params=list(bid,as.character(inicio),as.character(fim)))
    if(nrow(x)>0)x$data<-format(as.Date(x$data),"%d/%m/%Y")
    names(x)<-c("ID","Data","Descrição","Categoria","Quantidade","Valor unitário","Valor total")
    datatable(x,rownames=FALSE,options=list(pageLength=10,language=list(
      search="Pesquisar:",lengthMenu="Mostrar _MENU_ registros",
      info="Mostrando _START_ até _END_ de _TOTAL_ registros",
      zeroRecords="Nenhum gasto encontrado",emptyTable="Nenhum gasto cadastrado",
      paginate=list(first="Primeiro",last="Último",'next'="Próximo",previous="Anterior")))) |>
      formatCurrency(columns=c("Valor unitário","Valor total"),
                     currency="R$ ",digits=2,mark=".",dec=",")
  })


  # -------------------------------------------------------
  # FILTROS E ANÁLISE FINANCEIRA / PRODUÇÃO POR BARBEIRO
  # -------------------------------------------------------

  # Atualiza a lista de barbeiros da barbearia selecionada.
  atualizar_barbeiros_financeiro <- function() {
    bid <- id_barb()
    if(is.null(bid)) return(invisible(NULL))
    x <- dbq("SELECT id,nome_usuario,ativo
              FROM usuarios
              WHERE barbearia_id=? AND perfil='BARBEIRO'
              ORDER BY nome_usuario,id",params=list(bid))
    escolhas <- c("Todos os barbeiros"="TODOS")
    if(nrow(x)>0) {
      nomes <- paste0(x$nome_usuario,ifelse(x$ativo==1,""," (desativado)"))
      escolhas <- c(escolhas,stats::setNames(as.character(x$id),nomes))
    }
    atual <- isolate(input$fin_barbeiro)
    if(is.null(atual) || !as.character(atual) %in% unname(escolhas)) atual <- "TODOS"
    updateSelectInput(session,"fin_barbeiro",choices=escolhas,selected=atual)
    invisible(NULL)
  }

  observeEvent(list(estado$pagina,input$master_sel,master_upd()),{
    if(estado$pagina=="gestao") atualizar_barbeiros_financeiro()
  },ignoreInit=FALSE)

  observeEvent(input$fin_limpar_filtros,{
    hoje <- Sys.Date()
    updateDateRangeInput(session,"fin_periodo",
      start=as.Date(format(hoje,"%Y-%m-01")),end=hoje)
    updateSelectInput(session,"fin_barbeiro",selected="TODOS")
  })

  fin_filtro <- reactive({
    bid <- id_barb()
    req(!is.null(bid))
    p <- input$fin_periodo
    if(is.null(p) || length(p)<2 || any(is.na(p))) {
      fim <- Sys.Date()
      inicio <- as.Date(format(fim,"%Y-%m-01"))
    } else {
      inicio <- as.Date(p[1]); fim <- as.Date(p[2])
    }
    if(fim < inicio) {
      z <- inicio; inicio <- fim; fim <- z
    }

    barbeiro_raw <- as.character(input$fin_barbeiro %||% "TODOS")
    barbeiro_id <- suppressWarnings(as.integer(barbeiro_raw))
    todos <- identical(barbeiro_raw,"TODOS") || is.na(barbeiro_id)

    nome <- "Todos os barbeiros"
    if(!todos) {
      bx <- dbq("SELECT nome_usuario FROM usuarios
                 WHERE id=? AND barbearia_id=? AND perfil='BARBEIRO' LIMIT 1",
                params=list(barbeiro_id,bid))
      if(nrow(bx)>0) nome <- as.character(bx$nome_usuario[1])
      else { todos <- TRUE; barbeiro_id <- NA_integer_ }
    }

    list(bid=bid,inicio=inicio,fim=fim,todos=todos,
         barbeiro_id=barbeiro_id,nome=nome)
  })

  # Produção significa somente atendimentos CONCLUÍDOS.
  fin_producao <- function(f,inicio=f$inicio,fim=f$fim) {
    if(f$todos) {
      x <- dbq("SELECT COALESCE(SUM(preco),0) valor,COUNT(*) qtd
                FROM atendimentos
                WHERE barbearia_id=? AND status='CONCLUÍDO'
                  AND data>=? AND data<=?",
               params=list(f$bid,as.character(inicio),as.character(fim)))
    } else {
      x <- dbq("SELECT COALESCE(SUM(preco),0) valor,COUNT(*) qtd
                FROM atendimentos
                WHERE barbearia_id=? AND barbeiro_id=? AND status='CONCLUÍDO'
                  AND data>=? AND data<=?",
               params=list(f$bid,f$barbeiro_id,as.character(inicio),as.character(fim)))
    }
    list(valor=as.numeric(x$valor[1]),qtd=as.integer(x$qtd[1]))
  }

  fin_gastos_periodo <- function(f,inicio=f$inicio,fim=f$fim) {
    x <- dbq("SELECT COALESCE(SUM(valor_total),0) valor
              FROM gastos
              WHERE barbearia_id=? AND data>=? AND data<=?",
             params=list(f$bid,as.character(inicio),as.character(fim)))
    as.numeric(x$valor[1])
  }

  # Desloca o período um mês para trás preservando os dias sempre que possível.
  # Ex.: 01/10–10/10 -> 01/09–10/09.
  fin_periodo_anterior <- function(inicio,fim) {
    shift_one <- function(d) {
      primeiro <- as.Date(format(d,"%Y-%m-01"))
      ant_fim <- primeiro - 1
      ant_inicio <- as.Date(format(ant_fim,"%Y-%m-01"))
      dia <- as.integer(format(d,"%d"))
      ant_inicio + min(dia,as.integer(format(ant_fim,"%d"))) - 1
    }
    list(inicio=shift_one(inicio),fim=shift_one(fim))
  }

  # Projeta a produção/saldo até o último dia do mês do fim do filtro.
  fin_projecao <- reactive({
    f <- fin_filtro()
    p <- fin_producao(f)
    g <- if(f$todos) fin_gastos_periodo(f) else 0

    inicio_mes <- as.Date(format(f$fim,"%Y-%m-01"))
    fim_mes <- seq(inicio_mes,by="month",length.out=2)[2]-1

    # Para projeção mensal, usa do início do mês até a data final escolhida.
    # Se o filtro começar depois do início do mês, respeita o início escolhido.
    inicio_base <- max(f$inicio,inicio_mes)
    fim_base <- min(f$fim,fim_mes)
    dias <- max(1,as.integer(fim_base-inicio_base)+1)
    dias_restantes_base <- as.integer(fim_mes-inicio_base)+1

    base_prod <- fin_producao(f,inicio_base,fim_base)
    if(f$todos) {
      base_gastos <- fin_gastos_periodo(f,inicio_base,fim_base)
      base_valor <- base_prod$valor-base_gastos
    } else {
      base_valor <- base_prod$valor
    }
    valor <- base_valor/dias*dias_restantes_base
    list(valor=valor,inicio=inicio_base,fim=fim_base,fim_mes=fim_mes,
         dias=dias,dias_total=dias_restantes_base)
  })

  output$fin_cards <- renderUI({
    f <- fin_filtro()
    p <- fin_producao(f)
    proj <- fin_projecao()

    kpi <- function(classe,label,valor,nota){
      div(class=paste("finance-kpi",classe),
          div(class="finance-kpi-label",label),
          div(class="finance-kpi-value",valor),
          div(class="finance-kpi-note",nota))
    }

    if(f$todos) {
      g <- fin_gastos_periodo(f)
      saldo <- p$valor-g
      div(class="finance-kpis",
          kpi("revenue","Faturamento no período",formata_reais(p$valor),
              "Receita de atendimentos concluídos"),
          kpi("expense","Despesas operacionais",formata_reais(g),
              "Gastos registrados pela barbearia"),
          kpi("profit","Lucro operacional",formata_reais(saldo),
              "Faturamento menos despesas registradas"),
          kpi("forecast","Projeção de lucro",formata_reais(proj$valor),
              "Estimativa até o fim do mês")
      )
    } else {
      ticket <- if(p$qtd>0) p$valor/p$qtd else 0
      div(class="finance-kpis",
          kpi("revenue",paste("Faturamento gerado -",f$nome),formata_reais(p$valor),
              "Receita dos atendimentos concluídos"),
          kpi("profit","Atendimentos concluídos",
              format(p$qtd,big.mark=".",decimal.mark=","),
              "Produção realizada no período"),
          kpi("expense","Ticket médio",formata_reais(ticket),
              "Faturamento médio por atendimento"),
          kpi("forecast","Projeção de faturamento",formata_reais(proj$valor),
              "Estimativa de produção até o fim do mês")
      )
    }
  })

  output$fin_titulo_projecao <- renderText({
    f <- fin_filtro()
    if(f$todos) "Projeção de resultado operacional" else paste("Projeção de faturamento -",f$nome)
  })

  output$fin_projecao_info <- renderText({
    f <- fin_filtro(); pr <- fin_projecao()
    if(f$todos) {
      paste0("A projeção usa a média diária do resultado operacional (faturamento concluído menos despesas da barbearia) de ",
             format(pr$inicio,"%d/%m/%Y")," até ",format(pr$fim,"%d/%m/%Y"),
             " e mantém esse ritmo até ",format(pr$fim_mes,"%d/%m/%Y"),
             ". Dias considerados: ",pr$dias,"; dias projetados no intervalo mensal: ",pr$dias_total,".")
    } else {
      paste0("A projeção usa a média diária do faturamento concluído de ",f$nome," de ",
             format(pr$inicio,"%d/%m/%Y")," até ",format(pr$fim,"%d/%m/%Y"),
             " e mantém esse ritmo até ",format(pr$fim_mes,"%d/%m/%Y"),
             ". As despesas não são atribuídas ao barbeiro; continuam pertencendo à barbearia.")
    }
  })

  output$fin_titulo_comparativo <- renderText({
    f <- fin_filtro()
    if(f$todos) "Comparativo de desempenho financeiro" else paste("Comparativo de faturamento -",f$nome)
  })

  output$fin_comparativo_periodo <- renderText({
    f <- fin_filtro()
    ant <- fin_periodo_anterior(f$inicio,f$fim)
    paste0("Comparando ",format(f$inicio,"%d/%m/%Y")," a ",format(f$fim,"%d/%m/%Y"),
           " com ",format(ant$inicio,"%d/%m/%Y")," a ",format(ant$fim,"%d/%m/%Y"),
           if(f$todos) "." else paste0(" para ",f$nome,"."))
  })

  output$fin_comparativo <- renderDT({
    f <- fin_filtro()
    antp <- fin_periodo_anterior(f$inicio,f$fim)

    pa <- fin_producao(f,f$inicio,f$fim)
    pp <- fin_producao(f,antp$inicio,antp$fim)

    pct <- function(a,b) if(isTRUE(b==0)) NA_real_ else (a-b)/abs(b)*100
    fmt_pct <- function(v) if(is.na(v)) "-" else
      paste0(format(round(v,2),nsmall=2,decimal.mark=",",big.mark="."),"%")

    if(f$todos) {
      ga <- fin_gastos_periodo(f,f$inicio,f$fim)
      gp <- fin_gastos_periodo(f,antp$inicio,antp$fim)
      atual <- c(Faturamento=pa$valor,Despesas=ga,`Lucro operacional`=pa$valor-ga)
      anterior <- c(Faturamento=pp$valor,Despesas=gp,`Lucro operacional`=pp$valor-gp)
      variacao <- atual-anterior
      x <- data.frame(
        Indicador=names(atual),
        `Período selecionado`=sapply(atual,formata_reais),
        `Período anterior`=sapply(anterior,formata_reais),
        `Variação`=sapply(variacao,formata_reais),
        `Variação %`=vapply(seq_along(atual),
          function(i) fmt_pct(pct(atual[i],anterior[i])),character(1)),
        check.names=FALSE,stringsAsFactors=FALSE
      )
    } else {
      ta <- if(pa$qtd>0) pa$valor/pa$qtd else 0
      tp <- if(pp$qtd>0) pp$valor/pp$qtd else 0
      atual_num <- c(pa$valor,pa$qtd,ta)
      ant_num <- c(pp$valor,pp$qtd,tp)
      var_num <- atual_num-ant_num
      x <- data.frame(
        Indicador=c("Produção","Atendimentos concluídos","Ticket médio"),
        `Período selecionado`=c(formata_reais(pa$valor),
          format(pa$qtd,big.mark=".",decimal.mark=","),
          formata_reais(ta)),
        `Período anterior`=c(formata_reais(pp$valor),
          format(pp$qtd,big.mark=".",decimal.mark=","),
          formata_reais(tp)),
        `Variação`=c(formata_reais(var_num[1]),
          format(var_num[2],big.mark=".",decimal.mark=","),
          formata_reais(var_num[3])),
        `Variação %`=vapply(seq_along(atual_num),
          function(i) fmt_pct(pct(atual_num[i],ant_num[i])),character(1)),
        check.names=FALSE,stringsAsFactors=FALSE
      )
    }

    datatable(x,rownames=FALSE,options=list(pageLength=3,dom='t',
      language=list(emptyTable="Sem dados para o período")))
  })

  # -------------------------------------------------------
  # DADOS DA BARBEARIA
  # -------------------------------------------------------
  carregar_perfil <- function(){
    bid<-id_barb();if(is.null(bid))return()
    b<-dbq("SELECT nome,telefone FROM barbearias WHERE id=? LIMIT 1",params=list(bid))
    d<-dbq("SELECT * FROM dados_barbearia WHERE barbearia_id=? LIMIT 1",params=list(bid))
    if(nrow(b)>0){
      updateTextInput(session,"p_nome",value=b$nome[1])
      updateTextInput(session,"p_telefone",value=b$telefone[1])
    }
    if(nrow(d)>0){
      vals<-c(p_responsavel=d$nome_responsavel[1],p_cpf_cnpj=formatar_cpf_cnpj(d$cpf_cnpj[1]),p_cep=d$cep[1],
              p_endereco=d$endereco[1],p_numero=d$numero[1],p_bairro=d$bairro[1],
              p_cidade=d$cidade[1],p_complemento=d$complemento[1])
      for(id in names(vals))updateTextInput(session,id,value=ifelse(is.na(vals[[id]]),"",vals[[id]]))
    }
  }

  observeEvent(input$salvar_dados,{
    bid<-id_barb();if(is.null(bid))return()

    nome_novo <- trimws(input$p_nome)
    telefone_novo <- gsub("[^0-9]","",trimws(input$p_telefone))
    doc <- validar_cpf_cnpj(input$p_cpf_cnpj, permitir_vazio=TRUE)

    if(!doc$ok){
      showNotification(doc$msg,type="error");return()
    }

    novos_d <- list(
      nome_responsavel=trimws(input$p_responsavel),
      cpf_cnpj=doc$digitos,
      cep=trimws(input$p_cep),
      endereco=trimws(input$p_endereco),
      numero=trimws(input$p_numero),
      bairro=trimws(input$p_bairro),
      cidade=trimws(input$p_cidade),
      complemento=trimws(input$p_complemento)
    )

    if(nome_novo=="" || telefone_novo==""){
      showNotification("Nome e telefone da barbearia são obrigatórios.",type="error");return()
    }
    if(nchar(telefone_novo)<10 || nchar(telefone_novo)>11){
      showNotification("O telefone deve ter 10 ou 11 dígitos.",type="error");return()
    }

    antigo_b <- dbq("SELECT nome,telefone FROM barbearias WHERE id=? LIMIT 1",params=list(bid))
    antigo_d <- dbq("SELECT nome_responsavel,cpf_cnpj,cep,endereco,numero,bairro,cidade,complemento
                                FROM dados_barbearia WHERE barbearia_id=? LIMIT 1",params=list(bid))
    if(nrow(antigo_d)==0){
      dbe("INSERT INTO dados_barbearia(barbearia_id) VALUES(?)",params=list(bid))
      antigo_d <- dbq("SELECT nome_responsavel,cpf_cnpj,cep,endereco,numero,bairro,cidade,complemento
                                  FROM dados_barbearia WHERE barbearia_id=? LIMIT 1",params=list(bid))
    }

    antigos_d <- list(
      nome_responsavel=antigo_d$nome_responsavel[1],
      cpf_cnpj=antigo_d$cpf_cnpj[1],
      cep=antigo_d$cep[1],
      endereco=antigo_d$endereco[1],
      numero=antigo_d$numero[1],
      bairro=antigo_d$bairro[1],
      cidade=antigo_d$cidade[1],
      complemento=antigo_d$complemento[1]
    )

    dbWithTransaction(con,{
      if(nrow(antigo_b)>0){
        registrar_historico(bid,"ALTERACAO","BARBEARIA","nome",antigo_b$nome[1],nome_novo)
        registrar_historico(bid,"ALTERACAO","BARBEARIA","telefone",antigo_b$telefone[1],telefone_novo)
      }
      for(campo in names(novos_d))
        registrar_historico(bid,"ALTERACAO","DADOS_BARBEARIA",campo,antigos_d[[campo]],novos_d[[campo]])

      dbe("UPDATE barbearias SET nome=?,telefone=?,nome_responsavel=?,cpf_cnpj=? WHERE id=?",
                params=list(nome_novo,telefone_novo,novos_d$nome_responsavel,novos_d$cpf_cnpj,bid))
      dbe("
        UPDATE dados_barbearia SET nome_responsavel=?,cpf_cnpj=?,cep=?,endereco=?,
        numero=?,bairro=?,cidade=?,complemento=? WHERE barbearia_id=?",
        params=list(novos_d$nome_responsavel,novos_d$cpf_cnpj,novos_d$cep,novos_d$endereco,
                    novos_d$numero,novos_d$bairro,novos_d$cidade,novos_d$complemento,bid))
    })

    showNotification("Dados atualizados e registrados no histórico.",type="message")
  })

  output$p_status <- renderText({
    bid<-id_barb();if(is.null(bid))return("")
    a<-dbq("SELECT status,valor_mensal FROM assinaturas
                       WHERE barbearia_id=? ORDER BY id DESC LIMIT 1",params=list(bid))
    if(nrow(a)==0)return("Assinatura não encontrada")
    paste(a$status[1],"-",formata_reais(a$valor_mensal[1]))
  })
  output$p_vencimento <- renderText({
    bid<-id_barb();if(is.null(bid))return("")
    a<-dbq("SELECT data_vencimento FROM assinaturas
                       WHERE barbearia_id=? ORDER BY id DESC LIMIT 1",params=list(bid))
    if(nrow(a)==0)return("")
    format(as.Date(a$data_vencimento[1]),"%d/%m/%Y")
  })

  # -------------------------------------------------------
  # CLIENTE
  # -------------------------------------------------------

  # Carrega somente os serviços ativos da barbearia vinculada ao cliente.
  atualizar_servicos_cliente <- function() {
    u <- estado$usuario
    if(is.null(u) || estado$pagina!="client" ||
       is.null(u$barbearia_id) || is.na(u$barbearia_id[1])) return(invisible(NULL))

    x <- dbq("
      SELECT id,nome,preco,duracao_minutos
      FROM servicos
      WHERE barbearia_id=? AND status='ATIVO'
      ORDER BY nome,id",
      params=list(as.integer(u$barbearia_id[1])))

    escolhas <- if(nrow(x)==0) character(0) else
      stats::setNames(as.character(x$id),
                      paste0(x$nome," - ",vapply(x$preco,formata_reais,character(1))))
    updateSelectInput(session,"c_ag_servico",choices=escolhas,
                      selected=if(length(escolhas)>0) escolhas[1] else character(0))
    barbeiros <- dbq("SELECT id,nome_usuario FROM usuarios
                      WHERE barbearia_id=? AND perfil='BARBEIRO' AND ativo=1
                      ORDER BY nome_usuario,id",
                     params=list(as.integer(u$barbearia_id[1])))
    esc_b <- if(nrow(barbeiros)==0) character(0) else
      stats::setNames(as.character(barbeiros$id),as.character(barbeiros$nome_usuario))
    updateSelectInput(session,"c_ag_barbeiro",choices=esc_b,
                      selected=if(length(esc_b)>0) esc_b[1] else character(0))
    invisible(NULL)
  }

  observeEvent(estado$pagina,{
    if(estado$pagina=="client") atualizar_servicos_cliente()
  },ignoreInit=FALSE)

  output$c_ag_hora_aviso <- renderUI({
    msg <- cliente_hora_aviso()
    if(is.null(msg) || !nzchar(msg)) return(NULL)
    div(style="margin-top:-8px;font-size:12px;color:#b45309;background:#fffbeb;border:1px solid #fde68a;border-radius:8px;padding:8px 10px;",
        icon("info-circle"), paste0(" ",msg))
  })

  # Horários oferecidos ao CLIENTE: intervalos de 30 minutos.
  # Horários bloqueados para a barbearia, bloqueados para o barbeiro e horários
  # já ocupados por esse barbeiro não aparecem no seletor.
  atualizar_horarios_cliente <- function() {
    u <- estado$usuario
    if(is.null(u) || estado$pagina!="client" || u$perfil[1]!="CLIENTE") return(invisible(NULL))

    bid <- as.integer(u$barbearia_id[1])
    barbeiro_id <- suppressWarnings(as.integer(input$c_ag_barbeiro))
    dat <- if(is.null(input$c_ag_data)) NA_character_ else as.character(input$c_ag_data)
    if(is.na(barbeiro_id) || is.na(dat) || !nzchar(dat)) {
      cliente_hora_aviso("Selecione o barbeiro e a data para consultar os horários.")
      updateSelectInput(session,"c_ag_hora",choices=character(0))
      return(invisible(NULL))
    }
    cliente_hora_aviso("")

    # A lista nasce da interseção entre funcionamento da barbearia
    # e jornada individual do barbeiro escolhido.
    hfunc <- funcionamento_dia(bid,dat)
    jfunc <- jornada_dia(bid,barbeiro_id,dat)
    slots <- character(0)
    if(nrow(hfunc)>0 && !is.na(hfunc$aberto[1]) && as.integer(hfunc$aberto[1])==1 &&
       nrow(jfunc)>0 && !is.na(jfunc$trabalha[1]) && as.integer(jfunc$trabalha[1])==1) {
      ab <- max(substr(as.character(hfunc$hora_abertura[1]),1,5),
                substr(as.character(jfunc$hora_inicio[1]),1,5))
      fe <- min(substr(as.character(hfunc$hora_fechamento[1]),1,5),
                substr(as.character(jfunc$hora_fim[1]),1,5))
      para_min <- function(h) {
        p <- as.integer(strsplit(h,":",fixed=TRUE)[[1]])
        p[1]*60+p[2]
      }
      ini <- para_min(ab)
      fim <- para_min(fe)
      if(!is.na(ini) && !is.na(fim) && fim > ini) {
        mins <- seq(ini,fim-1,by=30)
        slots <- sprintf("%02d:%02d",mins %/% 60,mins %% 60)
        slots <- slots[slots < fe]
      }
    }

    slots_jornada <- slots
    if(as.Date(dat)==Sys.Date())
      slots <- slots[slots > format(Sys.time(),"%H:%M")]
    if(as.Date(dat)<Sys.Date()) slots <- character(0)

    if(length(slots)>0) {
      bl <- dbq("SELECT barbeiro_id,hora_inicio,hora_fim
                 FROM bloqueios_agenda
                 WHERE barbearia_id=? AND data=?
                   AND status='ATIVO'
                   AND (barbeiro_id IS NULL OR barbeiro_id=?)",
                params=list(bid,dat,barbeiro_id))
      if(nrow(bl)>0) {
        for(i in seq_len(nrow(bl))) {
          hi <- substr(as.character(bl$hora_inicio[i]),1,5)
          hf <- substr(as.character(bl$hora_fim[i]),1,5)
          slots <- slots[!(slots>=hi & slots<hf)]
        }
      }

      oc <- dbq("SELECT hora FROM atendimentos
                 WHERE barbearia_id=? AND barbeiro_id=? AND data=?
                   AND status<>'CANCELADO'",
                params=list(bid,barbeiro_id,dat))
      if(nrow(oc)>0) {
        ocupados <- substr(as.character(oc$hora),1,5)
        slots <- slots[!slots %in% ocupados]
      }
    }

    if(length(slots)==0) {
      if(as.Date(dat)<Sys.Date()) {
        cliente_hora_aviso("A data selecionada já passou.")
      } else if(length(slots_jornada)==0) {
        cliente_hora_aviso("O barbeiro não possui jornada disponível nesta data.")
      } else if(as.Date(dat)==Sys.Date() && all(slots_jornada <= format(Sys.time(),"%H:%M"))) {
        cliente_hora_aviso("Não há mais horários futuros na jornada deste barbeiro hoje. Escolha outra data ou outro barbeiro.")
      } else {
        cliente_hora_aviso("Todos os horários desta data estão ocupados ou bloqueados. Escolha outra data ou outro barbeiro.")
      }
    } else {
      cliente_hora_aviso("")
    }

    atual <- isolate(input$c_ag_hora)
    selecionado <- if(!is.null(atual) && atual %in% slots) atual else
      if("10:00" %in% slots) "10:00" else if(length(slots)>0) slots[1] else character(0)
    updateSelectInput(session,"c_ag_hora",choices=slots,selected=selecionado)
    invisible(NULL)
  }

  observeEvent(list(input$c_ag_barbeiro,input$c_ag_data,agenda_upd(),
                    bloqueios_upd(),funcionamento_upd(),jornada_upd(),estado$pagina),{
    atualizar_horarios_cliente()
  },ignoreInit=FALSE)

  output$c_ag_preco <- renderText({
    u <- estado$usuario
    sid <- suppressWarnings(as.integer(input$c_ag_servico))
    if(is.null(u) || is.na(sid)) return("Selecione um serviço")
    x <- dbq("SELECT preco FROM servicos WHERE id=? AND barbearia_id=? AND status='ATIVO' LIMIT 1",
             params=list(sid,as.integer(u$barbearia_id[1])))
    if(nrow(x)==0) "Serviço indisponível" else formata_reais(x$preco[1])
  })

  output$c_ag_duracao <- renderText({
    u <- estado$usuario
    sid <- suppressWarnings(as.integer(input$c_ag_servico))
    if(is.null(u) || is.na(sid)) return("-")
    x <- dbq("SELECT duracao_minutos FROM servicos WHERE id=? AND barbearia_id=? AND status='ATIVO' LIMIT 1",
             params=list(sid,as.integer(u$barbearia_id[1])))
    if(nrow(x)==0) "-" else paste0(x$duracao_minutos[1]," minutos")
  })

  # O cadastro público só vira cliente operacional no primeiro agendamento.
  # A conta logada é a fonte da identidade; o cliente não informa nome/e-mail
  # novamente, evitando que um usuário agende em nome de outra pessoa.
  observeEvent(input$c_ag_salvar,{
    u <- estado$usuario
    if(is.null(u) || estado$pagina!="client" || u$perfil[1]!="CLIENTE") return()

    bid <- as.integer(u$barbearia_id[1])
    uid <- as.integer(u$id[1])
    sid <- suppressWarnings(as.integer(input$c_ag_servico))
    barbeiro_id <- suppressWarnings(as.integer(input$c_ag_barbeiro))
    dat <- if(is.null(input$c_ag_data)) NA_character_ else as.character(input$c_ag_data)
    hi <- input$c_ag_hora
    hora <- if(is.null(hi) || length(hi)==0) "" else substr(as.character(hi),1,5)
    obs <- trimws(as.character(input$c_ag_obs %||% ""))

    if(length(sid)==0 || is.na(sid)){showNotification("Selecione um serviço.",type="error");return()}
    if(length(barbeiro_id)==0 || is.na(barbeiro_id)){showNotification("Selecione um barbeiro.",type="error");return()}
    barb <- dbq("SELECT id,nome_usuario FROM usuarios
                 WHERE id=? AND barbearia_id=? AND perfil='BARBEIRO' AND ativo=1 LIMIT 1",
                params=list(barbeiro_id,bid))
    if(nrow(barb)==0){showNotification("O barbeiro selecionado não está disponível.",type="error");return()}
    if(is.na(dat) || !nzchar(dat)){showNotification("Selecione uma data.",type="error");return()}
    if(!grepl("^[0-9]{2}:[0-9]{2}$",hora)){showNotification("Informe um horário válido.",type="error");return()}
    if(as.Date(dat) < Sys.Date()){showNotification("Não é possível agendar em uma data passada.",type="error");return()}
    if(as.Date(dat)==Sys.Date() && hora <= format(Sys.time(),"%H:%M")){
      showNotification("Escolha um horário futuro.",type="error");return()
    }

    serv <- dbq("
      SELECT id,nome,preco,duracao_minutos
      FROM servicos
      WHERE id=? AND barbearia_id=? AND status='ATIVO' LIMIT 1",
      params=list(sid,bid))
    if(nrow(serv)==0){showNotification("O serviço selecionado não está disponível.",type="error");return()}

    if(!horario_dentro_funcionamento(bid,dat,hora)){
      showNotification("Esse horário está fora do funcionamento da barbearia. Escolha um horário disponível.",
                       type="error",duration=8);return()
    }
    if(!horario_dentro_jornada(bid,barbeiro_id,dat,hora)){
      showNotification("Esse horário está fora da jornada do barbeiro escolhido. Escolha outro horário.",
                       type="error",duration=8);return()
    }

    bloqueado <- horario_bloqueado(bid,barbeiro_id,dat,hora)
    if(nrow(bloqueado)>0){
      showNotification(paste0("Esse horário não está disponível para ",barb$nome_usuario[1],
                              ". Escolha outro horário ou outro barbeiro."),
                       type="error",duration=8);return()
    }

    ocupado <- dbq("
      SELECT id FROM atendimentos
      WHERE barbearia_id=? AND barbeiro_id=? AND data=? AND hora=? AND status<>'CANCELADO' LIMIT 1",
      params=list(bid,barbeiro_id,dat,hora))
    if(nrow(ocupado)>0){
      showNotification(paste0(barb$nome_usuario[1]," já possui um atendimento nesse horário. Escolha outro horário ou outro barbeiro."),
                       type="error");return()
    }

    tryCatch({
      novo_cliente_id <- dbWithTransaction(con,{
        # Busca o cadastro público pertencente exatamente ao usuário logado.
        cad <- dbq("
          SELECT id,nome,telefone,email,cliente_id
          FROM cadastros_clientes
          WHERE usuario_id=? AND barbearia_id=? LIMIT 1",
          params=list(uid,bid))
        if(nrow(cad)==0) stop("Cadastro do cliente não foi encontrado para esta barbearia.")

        cliente_id <- suppressWarnings(as.integer(u$cliente_id[1]))
        if(length(cliente_id)==0 || is.na(cliente_id))
          cliente_id <- suppressWarnings(as.integer(cad$cliente_id[1]))

        # Primeiro agendamento: cria a ficha operacional do cliente.
        if(length(cliente_id)==0 || is.na(cliente_id)) {
          existente <- dbq("
            SELECT id FROM clientes
            WHERE barbearia_id=? AND LOWER(email)=LOWER(?) LIMIT 1",
            params=list(bid,as.character(cad$email[1])))

          if(nrow(existente)>0) {
            cliente_id <- as.integer(existente$id[1])
            dbe("UPDATE clientes SET nome=?,telefone=?,email=?,status='ATIVO',cadastro_cliente_id=?
                 WHERE id=? AND barbearia_id=?",
                params=list(as.character(cad$nome[1]),as.character(cad$telefone[1]),
                            as.character(cad$email[1]),as.integer(cad$id[1]),cliente_id,bid))
          } else {
            dbe("
              INSERT INTO clientes(nome,telefone,email,data_cadastro,hora_cadastro,status,barbearia_id,cadastro_cliente_id)
              VALUES(?,?,?,?,?, 'ATIVO',?,?)",
              params=list(as.character(cad$nome[1]),as.character(cad$telefone[1]),
                          as.character(cad$email[1]),as.character(Sys.Date()),format(Sys.time(),"%H:%M:%S"),
                          bid,as.integer(cad$id[1])))
            cliente_id <- as.integer(db_last_id())
          }

          dbe("UPDATE usuarios SET cliente_id=? WHERE id=? AND perfil='CLIENTE' AND barbearia_id=?",
              params=list(cliente_id,uid,bid))
          dbe("UPDATE cadastros_clientes
               SET cliente_id=?,status='CONVERTIDO',data_conversao=?
               WHERE id=? AND usuario_id=? AND barbearia_id=?",
              params=list(cliente_id,as.character(Sys.Date()),as.integer(cad$id[1]),uid,bid))
        }

        dbe("
          INSERT INTO atendimentos(cliente_id,servico_id,data,hora,preco,duracao_minutos,
                                   status,observacao,data_criacao,barbearia_id,barbeiro_id,
                                   status_confirmacao,criado_por_perfil,criado_por_usuario_id,
                                   confirmado_em,confirmado_por_usuario_id)
          VALUES(?,?,?,?,?,?,'AGENDADO',?,?,?,?,?,'CLIENTE',?,?,?)",
          params=list(cliente_id,sid,dat,hora,serv$preco[1],serv$duracao_minutos[1],
                      obs,format(Sys.time(),"%Y-%m-%d %H:%M:%S"),bid,barbeiro_id,
                      "CONFIRMADO",uid,
                      format(Sys.time(),"%Y-%m-%d %H:%M:%S"),uid))
        atendimento_id <- as.integer(db_last_id())

        em <- montar_email_agendamento(bid,atendimento_id)
        if(!is.null(em) && validar_email(em$to)) {
          enfileirar_notificacao_email(
            bid,atendimento_id,cliente_id,em$to,em$subject,em$body,
            uid,"CLIENTE")
          notificacoes_upd(notificacoes_upd()+1)
        }

        registrar_historico(bid,"CRIACAO","ATENDIMENTO_CLIENTE","status","","AGENDADO")
        cliente_id
      })

      # Atualiza a sessão sem exigir que o cliente saia e entre novamente.
      u_atualizado <- estado$usuario
      u_atualizado$cliente_id[1] <- as.integer(novo_cliente_id)
      estado$usuario <- u_atualizado
      agenda_upd(agenda_upd()+1)
      clientes_upd(clientes_upd()+1)
      cadastros_upd(cadastros_upd()+1)
      updateTextAreaInput(session,"c_ag_obs",value="")
      updateDateInput(session,"c_ag_data",value=Sys.Date(),min=Sys.Date())
      showNotification("Agendamento realizado com sucesso.",type="message",duration=6)
    },error=function(e){
      showNotification(paste("Não foi possível realizar o agendamento:",conditionMessage(e)),
                       type="error",duration=10)
    })
  })

  output$minha_agenda <- renderDT({
    agenda_upd()
    u<-estado$usuario
    if(estado$pagina!="client"||is.null(u))return(datatable(data.frame(),rownames=FALSE))
    x<-dbq("
      SELECT a.data,a.hora,s.nome servico,
             COALESCE(u.nome_usuario,'Não informado') barbeiro,
             a.preco,a.status,a.observacao
      FROM atendimentos a
      JOIN servicos s ON s.id=a.servico_id AND s.barbearia_id=a.barbearia_id
      LEFT JOIN usuarios u ON u.id=a.barbeiro_id AND u.barbearia_id=a.barbearia_id
                           AND u.perfil='BARBEIRO' 
      WHERE a.cliente_id=? AND a.barbearia_id=?
      ORDER BY a.data DESC,a.hora DESC,a.id DESC",
      params=list(u$cliente_id[1],u$barbearia_id[1]))
    if(nrow(x)>0){
      x$data<-format(as.Date(x$data),"%d/%m/%Y")
      x$hora<-substr(as.character(x$hora),1,5)
    }
    names(x)<-c("Data","Horário","Serviço","Barbeiro","Valor","Status","Observação")
    datatable(x,rownames=FALSE,options=list(pageLength=10,language=list(
      search="Pesquisar:",lengthMenu="Mostrar _MENU_ registros",
      info="Mostrando _START_ até _END_ de _TOTAL_ registros",
      zeroRecords="Nenhum atendimento encontrado",emptyTable="Nenhum atendimento cadastrado",
      paginate=list(first="Primeiro",last="Último",'next'="Próximo",previous="Anterior")))) |>
      formatCurrency(columns="Valor",currency="R$ ",digits=2,mark=".",dec=",")
  })

  output$c_nome <- renderText({
    u<-estado$usuario;if(is.null(u))return("")
    x<-dbq("SELECT nome FROM clientes WHERE id=? AND barbearia_id=? LIMIT 1",
                  params=list(u$cliente_id[1],u$barbearia_id[1]))
    if(nrow(x)==0)"" else paste("Nome:",x$nome[1])
  })
  output$c_telefone <- renderText({
    u<-estado$usuario;if(is.null(u))return("")
    x<-dbq("SELECT telefone FROM clientes WHERE id=? AND barbearia_id=? LIMIT 1",
                  params=list(u$cliente_id[1],u$barbearia_id[1]))
    if(nrow(x)==0)"" else paste("Telefone:",x$telefone[1])
  })
  output$c_email <- renderText({
    u<-estado$usuario;if(is.null(u))"" else paste("E-mail:",u$email[1])
  })

  observeEvent(input$c_salvar_senha,{
    er<-validar_senha(input$c_nova_senha)
    if(length(er)>0){showNotification(paste(er,collapse="\n"),type="error",duration=8);return()}
    if(input$c_nova_senha!=input$c_conf_senha){showNotification("As senhas não coincidem.",type="error");return()}
    dbe("UPDATE usuarios SET senha_hash=?,troca_senha_obrigatoria=0
                WHERE id=? AND perfil='CLIENTE'",
              params=list(hash_senha(input$c_nova_senha),estado$usuario$id[1]))
    updatePasswordInput(session,"c_nova_senha",value="")
    updatePasswordInput(session,"c_conf_senha",value="")
    showNotification("Senha alterada.",type="message")
  })

  # -------------------------------------------------------
  # AO ENTRAR NO PAINEL DO BARBEIRO, CARREGAR PERFIL
  # -------------------------------------------------------
  observeEvent(estado$pagina,{
    if(estado$pagina=="gestao") {
      atualizar_servicos_input()
      atualizar_barbeiros_agenda()
      carregar_perfil()
    }
  },ignoreInit=FALSE)

  # Quando o MASTER troca a barbearia selecionada, todas as telas
  # da área de gestão passam imediatamente a apontar para a nova conta.
  observeEvent(list(input$master_sel,input$master_sel_lista),{
    if(estado$pagina=="gestao" && !is.null(estado$usuario) &&
       estado$usuario$perfil[1]=="MASTER") {
      atualizar_servicos_input()
      atualizar_barbeiros_agenda()
      carregar_perfil()
    }
  },ignoreInit=TRUE)

  # -------------------------------------------------------
  # LIMPAR URL QUANDO VOLTA AO LOGIN (EVITA TOKEN VISÍVEL)
  # -------------------------------------------------------
  observeEvent(estado$pagina,{
    if(estado$pagina=="login" && !is.null(estado$token)) {
      estado$token <- NULL
      updateQueryString("?",mode="replace",session=session)
    }
  })

}

# =========================================================
# EXECUÇÃO
# =========================================================

onStop(function()try(dbDisconnect(con),silent=TRUE))

shinyApp(ui=ui,server=server)
