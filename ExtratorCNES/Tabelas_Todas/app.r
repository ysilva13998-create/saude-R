# ==============================================================================
# EXTRATOR CNES - Cadastro Nacional de Estabelecimentos de Saúde
# Versão standalone: salva em Parquet + atualiza DuckDB (cnes.duckdb)
# ==============================================================================
# COMO USAR:
#   1. Tenha o R instalado (https://cran.r-project.org/)
#   2. Abra este arquivo no RStudio OU dê duplo clique em Iniciar_App.bat
#   3. O navegador abrirá automaticamente
# ==============================================================================

# 0. AVISO INICIAL -----------------------------------------------------------
cat("\n======================================================================\n")
cat("   EXTRATOR CNES - INICIANDO\n")
cat("======================================================================\n")

# 1. INSTALAÇÃO AUTOMÁTICA DE PACOTES ---------------------------------------
pacotes_necessarios <- c(
  "shiny", "shinydashboard", "shinyWidgets", "microdatasus",
  "dplyr", "arrow", "stringr", "DT", "DBI", "duckdb"
)

faltantes <- pacotes_necessarios[
  !sapply(pacotes_necessarios, requireNamespace, quietly = TRUE)
]

if (length(faltantes) > 0) {
  cat("\n⚠  Pacotes faltantes:\n")
  cat(paste0("   - ", faltantes, collapse = "\n"), "\n")
  cat("\nInstalando automaticamente (só na primeira execução)...\n\n")

  ok <- tryCatch({
    install.packages(faltantes, dependencies = TRUE)
    TRUE
  }, error = function(e) {
    cat("\n❌ Erro ao instalar:\n", conditionMessage(e), "\n")
    FALSE
  })

  if (!ok) stop("Não foi possível instalar os pacotes. Abortando.")

  ainda <- faltantes[!sapply(faltantes, requireNamespace, quietly = TRUE)]
  if (length(ainda) > 0) {
    stop(paste("Ainda faltam pacotes:", paste(ainda, collapse = ", ")))
  }
  cat("\n✅ Pacotes instalados!\n")
} else {
  cat("✅ Todos os pacotes já estão instalados.\n")
}

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(microdatasus)
  library(dplyr)
  library(arrow)
  library(stringr)
  library(DT)
  library(DBI)
  library(duckdb)
})

# 2. CONFIGURAÇÃO DE AMBIENTE -----------------------------------------------
pasta_app <- getwd()
RAIZ      <- file.path(pasta_app, "Tabelas_Todas")
DB_PATH   <- file.path(pasta_app, "cnes.duckdb")

# Caminho para o parquet de municípios (mesmo padrão do SIA/SIH)
MUNICIPIOS_PATH <- file.path(RAIZ, "_METADADOS", "br_bd_diretorios_brasil_municipio.parquet")

dir.create(file.path(RAIZ, "CNES", "FINAL"), recursive = TRUE, showWarnings = FALSE)

cat("📂 Pasta do app   :", pasta_app, "\n")
cat("📂 Pasta de dados :", RAIZ, "\n")
cat("📂 Banco DuckDB   :", DB_PATH, "\n")
cat("📂 Metadados      :", MUNICIPIOS_PATH, "\n\n")

# 3. CONEXÃO E CRIAÇÃO DA TABELA NO DUCKDB ----------------------------------
con <- dbConnect(duckdb::duckdb(), dbdir = DB_PATH, read_only = FALSE)

# Garante que o banco feche ao sair
onStop(function() {
  try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)
})

criar_tabela_cnes <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS cnes (
      cnes VARCHAR PRIMARY KEY,
      ibge VARCHAR,
      nome_estabelecimento VARCHAR,
      latitude DOUBLE,
      longitude DOUBLE,
      ultima_atualizacao TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_cnes_ibge ON cnes(ibge);")
}
criar_tabela_cnes(con)
cat("✅ Tabela 'cnes' pronta no banco DuckDB.\n\n")

# 4. CONSTANTES -------------------------------------------------------------
MESES_PT <- c("Janeiro","Fevereiro","Março","Abril","Maio","Junho",
              "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")

ESTADOS_BR <- c("Acre"="AC","Alagoas"="AL","Amapá"="AP","Amazonas"="AM",
  "Bahia"="BA","Ceará"="CE","Distrito Federal"="DF","Espírito Santo"="ES",
  "Goiás"="GO","Maranhão"="MA","Mato Grosso"="MT","Mato Grosso do Sul"="MS",
  "Minas Gerais"="MG","Pará"="PA","Paraíba"="PB","Paraná"="PR",
  "Pernambuco"="PE","Piauí"="PI","Rio de Janeiro"="RJ","Rio Grande do Norte"="RN",
  "Rio Grande do Sul"="RS","Rondônia"="RO","Roraima"="RR","Santa Catarina"="SC",
  "São Paulo"="SP","Sergipe"="SE","Tocantins"="TO")

SISTEMAS_CNES <- c(
  "CNES - Estabelecimentos (ST)"      = "CNES-ST",
  "CNES - Profissionais (PF)"         = "CNES-PF",
  "CNES - Leitos (LT)"                = "CNES-LT",
  "CNES - Dados Complementares (DC)"  = "CNES-DC",
  "CNES - Equipamentos (EQ)"          = "CNES-EQ",
  "CNES - Serviços Especializados (SR)" = "CNES-SR",
  "CNES - Habilitações (HB)"          = "CNES-HB",
  "CNES - Equipes (EP)"               = "CNES-EP",
  "CNES - Regras Contratuais (RC)"    = "CNES-RC",
  "CNES - Incentivos (IN)"            = "CNES-IN",
  "CNES - Estabelecimento Ensino (EE)" = "CNES-EE",
  "CNES - Estabelecimento Filantrópico (EF)" = "CNES-EF",
  "CNES - Gestão e Metas (GM)"        = "CNES-GM"
)

ANOS_DISPONIVEIS <- 2005:2026

# 5. UI DO MANUAL -----------------------------------------------------------
ui_manual <- function() {
  fluidPage(
    tags$head(tags$style(HTML("
      .manual-box { background:#f8f9fa; border-left:5px solid #0d6efd;
        padding:18px 22px; margin-bottom:18px; border-radius:10px; line-height:1.7; }
      .manual-box h4 { color:#0d6efd; margin-top:0; }
      .manual-step { background:#fff; border:1px solid #dee2e6;
        border-radius:8px; padding:12px 16px; margin-bottom:10px; }
      .manual-code { background:#2c3e50; color:#ecf0f1; padding:10px 14px;
        border-radius:6px; font-family:monospace; font-size:.9em; margin:8px 0; }
      .manual-alert { background:#fff3cd; border-left:5px solid #ffc107;
        padding:12px 16px; border-radius:8px; margin-bottom:14px; }
      .manual-ok { background:#d1e7dd; border-left:5px solid #198754;
        padding:12px 16px; border-radius:8px; margin-bottom:14px; }
    "))),

    titlePanel(h2(icon("hospital"), " Manual do Extrator CNES", style = "color:#2c3e50;")),
    br(),

    div(class = "manual-box",
      h4(icon("rocket"), " Início rápido"),
      p("Este app baixa os microdados do CNES (Cadastro Nacional de Estabelecimentos de Saúde) do DATASUS e salva em Parquet + atualiza o banco DuckDB."),
      tags$ol(
        tags$li(strong("Escolha o sistema CNES"), " (ST, PF, LT, etc)."),
        tags$li(strong("Escolha a UF"), " (ex.: PR)."),
        tags$li(strong("Marque os municípios"), " (pode selecionar vários ou 'Todas')."),
        tags$li(strong("Marque os anos e meses"), " (pode selecionar vários)."),
        tags$li(strong("Clique em Processar"), " e aguarde.")
      )
    ),

    div(class = "manual-box",
      h4(icon("folder-tree"), " Onde os arquivos são salvos"),
      p("Dados brutos (Parquet):"),
      div(class = "manual-code", "Tabelas_Todas/CNES/FINAL/FINAL_CNES-<SISTEMA>_<UF>_<CIDADE>_<ANO>_<MÊS>.parquet"),
      p("Banco de dados (DuckDB):"),
      div(class = "manual-code", "cnes.duckdb (tabela cnes)"),
      p("A tabela cnes guarda apenas:"),
      tags$ul(
        tags$li(code("cnes"), " - Código do estabelecimento (vem do DATASUS)"),
        tags$li(code("ibge"), " - Código do município (vem do DATASUS)"),
        tags$li(code("nome_estabelecimento"), " - Nome (NÃO vem do DATASUS, fica NULL)"),
        tags$li(code("latitude"), " - Latitude (NÃO vem do DATASUS, fica NULL)"),
        tags$li(code("longitude"), " - Longitude (NÃO vem do DATASUS, fica NULL)")
      )
    ),

    div(class = "manual-alert",
      icon("triangle-exclamation"), strong(" Importante:"),
      " O DATASUS não fornece nome, latitude e longitude dos estabelecimentos. ",
      "Essas colunas são criadas no banco (vazias) para que você possa preenchê-las depois ",
      "(via API do Google Maps, base dos dados, ou manualmente). O código do CNES e o IBGE são preenchidos automaticamente."
    ),

    div(class = "manual-box",
      h4(icon("gears"), " Como funciona por dentro"),
      p("O algoritmo faz três coisas para cada mês/ano baixado:"),
      tags$ul(
        tags$li(strong("1."), " Baixa o microdado bruto via ", code("fetch_datasus")),
        tags$li(strong("2."), " Salva o Parquet em ", code("Tabelas_Todas/CNES/FINAL/")),
        tags$li(strong("3."), " Extrai ", code("CNES"), " e ", code("CODUFMUN"), " e faz um UPSERT (insert ou update) no banco ", code("cnes.duckdb"))
      )
    ),

    div(class = "manual-ok",
      icon("circle-check"), strong(" Dica:"), " O banco não duplica dados. Se você reprocessar o mesmo ano, ele atualiza o registro em vez de inserir de novo."
    ),

    div(class = "manual-box",
      h4(icon("info-circle"), " Sobre"),
      p("Extrator CNES - versão standalone."),
      p("Fonte: DATASUS via pacote ", code("microdatasus"), ".")
    )
  )
}

# 6. UI PRINCIPAL -----------------------------------------------------------
ui <- navbarPage(
  id    = "navbar_id",
  title = tags$div(icon("hospital"), " Extrator CNES"),
  header = tags$head(tags$style(HTML(".navbar-default { background-color:#2c3e50; }
                                      .navbar-default .navbar-brand,
                                      .navbar-default .navbar-nav > li > a { color:#ecf0f1; }"))),

  tabPanel("Extrator", icon = icon("database"),
    fluidPage(
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("sistema", "Sistema CNES:", choices = SISTEMAS_CNES),
          selectInput("uf", "UF:", choices = ESTADOS_BR, selected = "PR"),

          # 👇 NOVO: Seleção de cidade
          pickerInput(
            "municipios", "Municípios (pode escolher vários):",
            choices = NULL, multiple = TRUE,
            options = list(
              `actions-box`       = TRUE,
              `live-search`       = TRUE,
              `select-all-text`   = "Todas da UF",
              `deselect-all-text` = "Nenhum",
              `none-selected-text`= "Selecione..."
            )
          ),

          pickerInput(
            "anos", "Anos (pode escolher vários):",
            choices = ANOS_DISPONIVEIS, selected = 2024, multiple = TRUE,
            options = list(
              `actions-box`       = TRUE,
              `select-all-text`   = "Todos",
              `deselect-all-text` = "Nenhum"
            )
          ),

          pickerInput(
            "meses", "Meses (pode escolher vários):",
            choices = setNames(1:12, MESES_PT), selected = 1, multiple = TRUE,
            options = list(
              `actions-box`       = TRUE,
              `select-all-text`   = "Todos",
              `deselect-all-text` = "Nenhum"
            )
          ),

          actionButton("processar", "Processar", class = "btn-primary btn-lg",
                       icon = icon("play"), style = "width:100%;"),
          br(), br(),
          helpText("Salva Parquet em: ", tags$code("Tabelas_Todas/CNES/FINAL/")),
          helpText("Atualiza banco: ", tags$code("cnes.duckdb")),
          helpText("Timeout: 600s por download.")
        ),

        mainPanel(
          width = 9,
          fluidRow(
            column(4, h4(textOutput("status"))),
            column(4, h4(textOutput("linhas"))),
            column(4, h4(textOutput("tempo")))
          ),
          br(),
          tabsetPanel(
            tabPanel("Log",      verbatimTextOutput("log")),
            tabPanel("Banco CNES", DTOutput("tabela_banco"))
          )
        )
      )
    )
  ),

  tabPanel("Manual", icon = icon("book-open"), ui_manual())
)

# 7. SERVER ----------------------------------------------------------------
server <- function(input, output, session) {

  stats <- reactiveValues(
    start_time = NULL, end_time = NULL, linhas = 0,
    status = "Aguardando", arquivos = character(0),
    banco_inseridos = 0, banco_atualizados = 0
  )

  # Carrega lista de municípios para o filtro
  municipios_df <- reactive({
    if (!file.exists(MUNICIPIOS_PATH)) {
      showNotification("Metadados de municípios não encontrados em _METADADOS/.", type = "error")
      return(NULL)
    }
    tryCatch({
      read_parquet(MUNICIPIOS_PATH) %>%
        mutate(id6 = as.character(id_municipio_6)) %>%
        select(id6, nome, sigla_uf)
    }, error = function(e) {
      showNotification(paste("Erro ao ler metadados:", e$message), type = "error")
      NULL
    })
  })

  # Atualiza cidades conforme a UF
  observeEvent(input$uf, {
    req(municipios_df())
    cidades <- municipios_df() %>% filter(sigla_uf == input$uf) %>% arrange(nome)
    updatePickerInput(
      session, "municipios",
      choices = setNames(cidades$id6, cidades$nome),
      selected = character(0)
    )
  })

  # Função para fazer UPSERT no banco
  upsert_cnes <- function(df, con) {
    col_cnes <- intersect(names(df), c("CNES", "CO_CNES"))[1]
    col_ibge <- intersect(names(df), c("CODUFMUN", "CO_MUNICIPIO", "MUNIC_RES", "CODMUNRES"))[1]

    if (is.na(col_cnes) || is.na(col_ibge)) {
      return(list(inseridos = 0, atualizados = 0))
    }

    novos <- df %>%
      dplyr::transmute(
        cnes = as.character(.data[[col_cnes]]),
        ibge = as.character(.data[[col_ibge]])
      ) %>%
      dplyr::distinct(cnes, .keep_all = TRUE) %>%
      dplyr::filter(!is.na(cnes) & cnes != "")

    if (nrow(novos) == 0) return(list(inseridos = 0, atualizados = 0))

    inseridos <- 0
    atualizados <- 0

    for(i in 1:nrow(novos)) {
      existe <- dbGetQuery(con, "SELECT COUNT(*) as total FROM cnes WHERE cnes = ?",
                          list(novos$cnes[i]))$total[1] > 0

      if(existe) {
        res <- dbExecute(con, "
          UPDATE cnes
          SET ibge = COALESCE(?, ibge),
              ultima_atualizacao = CURRENT_TIMESTAMP
          WHERE cnes = ?
        ", list(novos$ibge[i], novos$cnes[i]))
        atualizados <- atualizados + res
      } else {
        res <- dbExecute(con, "
          INSERT INTO cnes (cnes, ibge, nome_estabelecimento, latitude, longitude)
          VALUES (?, ?, NULL, NULL, NULL)
        ", list(novos$cnes[i], novos$ibge[i]))
        inseridos <- inseridos + res
      }
    }

    return(list(inseridos = inseridos, atualizados = atualizados))
  }

  # Processamento
  observeEvent(input$processar, {
    stats$start_time <- Sys.time()
    stats$status     <- "Processando..."
    stats$linhas     <- 0
    stats$arquivos   <- character(0)
    stats$banco_inseridos <- 0
    stats$banco_atualizados <- 0
    stats$end_time   <- NULL

    anos <- sort(unique(as.integer(input$anos)))
    meses <- sort(unique(as.integer(input$meses)))
    cidades_cod <- input$municipios

    if (length(anos) == 0 || length(meses) == 0) {
      showNotification("Selecione ao menos um ano e um mês.", type = "warning")
      stats$status <- "Aguardando seleção"
      return()
    }

    if (is.null(cidades_cod) || length(cidades_cod) == 0) {
      showNotification("Selecione ao menos um município.", type = "warning")
      stats$status <- "Aguardando seleção"
      return()
    }

    cidades_nomes <- sapply(cidades_cod, function(cod) {
      municipios_df() %>% filter(id6 == cod) %>% pull(nome) %>% first()
    })

    cat("\n═══════════════════════════════════════════\n")
    cat("SISTEMA:", input$sistema, "\n")
    cat("UF:", input$uf, "\n")
    cat("CIDADES:", paste(cidades_nomes, collapse = ", "), "\n")
    cat("ANOS:", paste(anos, collapse = ", "), "\n")
    cat("MESES:", paste(MESES_PT[meses], collapse = ", "), "\n")
    cat("═══════════════════════════════════════════\n\n")

    total_linhas <- 0
    arquivos <- character(0)
    total_tarefas <- length(anos) * length(meses)
    k <- 0
    total_inseridos <- 0
    total_atualizados <- 0

    withProgress(message = "Baixando CNES...", value = 0, {
      for (a in anos) {
        for (m in meses) {
          k <- k + 1
          incProgress(1 / total_tarefas * 0.97, detail = paste("Baixando", a, MESES_PT[m]))

          cat(sprintf("\n⬇️  [%d/%d] Baixando %s - %d - %s\n",
                      k, total_tarefas, input$uf, a, MESES_PT[m]))

          # Download por UF (o filtro de cidade é feito no R após o download)
          dados_full <- tryCatch(
            fetch_datasus(
              year_start = a, year_end = a,
              month_start = m, month_end = m,
              uf = input$uf, information_system = input$sistema,
              timeout = 600
            ),
            error = function(e) {
              cat("   ⚠️ erro/timeout:", conditionMessage(e), "\n")
              NULL
            }
          )

          if (is.null(dados_full) || nrow(dados_full) == 0) {
            cat("   ⚠️ Sem dados para", a, "/", MESES_PT[m], "\n")
            next
          }

          names(dados_full) <- make.names(names(dados_full))

          # Identificar coluna de município
          col_ibge <- intersect(names(dados_full), c("CODUFMUN", "CO_MUNICIPIO", "MUNIC_RES", "CODMUNRES"))[1]

          if (is.na(col_ibge)) {
             cat("   ⚠️ Coluna de município não encontrada.\n")
             rm(dados_full); next
          }

          # Filtra pelas cidades selecionadas
          for (i in seq_along(cidades_cod)) {
            cod_cidade <- cidades_cod[i]
            nom_cidade <- cidades_nomes[i]

            # Os códigos no DATASUS CNES costumam vir com 6 dígitos, às vezes 7
            dados_filtrados <- dados_full %>%
              filter(substr(as.character(.data[[col_ibge]]), 1, 6) == cod_cidade)

            if (nrow(dados_filtrados) == 0) {
              cat("   ⚠️ Sem dados para", nom_cidade, "em", a, "/", MESES_PT[m], "\n")
              next
            }

            # Limpeza de encoding
            dados_filtrados <- dados_filtrados %>% 
              mutate(across(where(is.character), ~ iconv(., from = "latin1", to = "UTF-8", sub = "")))

            # 1. Salvar Parquet
            path_final <- file.path(RAIZ, "CNES", "FINAL")
            mes_fmt <- sprintf("%02d", m)
            nome_cidade_clean <- str_replace_all(tolower(iconv(nom_cidade, to="ASCII//TRANSLIT")), "[^a-z0-9]", "_")
            
            nome_arq <- paste0("FINAL_", input$sistema, "_", input$uf, "_", nome_cidade_clean, "_", a, "_", mes_fmt, ".parquet")
            caminho <- file.path(path_final, nome_arq)

            write_parquet(dados_filtrados, caminho)
            cat("   ✅ Parquet salvo:", basename(caminho), "-", format(nrow(dados_filtrados), big.mark = "."), "linhas\n")

            arquivos <- c(arquivos, caminho)
            total_linhas <- total_linhas + nrow(dados_filtrados)

            # 2. Atualizar Banco (apenas se for CNES-ST)
            if (input$sistema == "CNES-ST") {
              res_banco <- upsert_cnes(dados_filtrados, con)
              total_inseridos <- total_inseridos + res_banco$inseridos
              total_atualizados <- total_atualizados + res_banco$atualizados
              cat("   🗄️  Banco: +", res_banco$inseridos, "inseridos |",
                  res_banco$atualizados, "atualizados\n")
            }
          }

          # Liberar memória
          rm(dados_full); gc(verbose = FALSE)
        }
      }
      incProgress(0.03, detail = "Concluído!")
    })

    stats$end_time   <- Sys.time()
    stats$linhas     <- total_linhas
    stats$arquivos   <- arquivos
    stats$banco_inseridos <- total_inseridos
    stats$banco_atualizados <- total_atualizados
    stats$status     <- if (length(arquivos) > 0) "Concluído" else "Sem dados"

    cat("\n═══════════════════════════════════════════\n")
    cat("✅ CONCLUÍDO:", length(arquivos), "arquivos Parquet gerados\n")
    cat("🗄️  Banco CNES: +", total_inseridos, "| ~", total_atualizados, "\n")
    cat("═══════════════════════════════════════════\n")
  })

  output$status <- renderText({ stats$status })
  output$linhas <- renderText({ format(stats$linhas, big.mark = ".") })
  output$tempo  <- renderText({
    if (!is.null(stats$start_time) && !is.null(stats$end_time)) {
      paste0(round(difftime(stats$end_time, stats$start_time, units = "secs"), 1), "s")
    } else "0s"
  })

  output$log <- renderPrint({
    req(length(stats$arquivos) > 0 || stats$status == "Sem dados")
    cat("╔══════════════════════════════════════════════════╗\n")
    cat("║              PROCESSAMENTO CONCLUÍDO             ║\n")
    cat("╚══════════════════════════════════════════════════╝\n\n")
    cat("Status:", stats$status, "\n")
    cat("Arquivos Parquet gerados:", length(stats$arquivos), "\n")
    cat("Linhas baixadas:", format(stats$linhas, big.mark = "."), "\n")
    cat("Banco CNES (novos):", stats$banco_inseridos, "\n")
    cat("Banco CNES (atualizados):", stats$banco_atualizados, "\n\n")
    for (f in stats$arquivos) cat("  ✅", basename(f), "\n")
  })

  # Visualização do Banco
  output$tabela_banco <- renderDT({
    df <- tryCatch({
      dbGetQuery(con, "SELECT cnes, ibge, nome_estabelecimento, latitude, longitude, ultima_atualizacao FROM cnes LIMIT 500")
    }, error = function(e) {
      data.frame(Mensagem = "Nenhum registro no banco ainda.")
    })

    datatable(df,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE,
              caption = "Tabela cnes (DuckDB)")
  })

  # Modal de boas-vindas
  ja_mostrou <- reactiveVal(FALSE)
  observe({
    if (!ja_mostrou()) {
      ja_mostrou(TRUE)
      showModal(modalDialog(
        title = tags$div(icon("hand-wave"), " Bem-vindo ao Extrator CNES!"),
        size = "l", easyClose = TRUE,
        footer = tagList(
          actionButton("ir_manual_cnes", "📖 Abrir Manual", class = "btn-info"),
          modalButton("✅ Começar")
        ),
        tags$div(class = "manual-box",
          style = "border-left-color:#198754;",
          tags$p(tags$b("Este app baixa microdados do CNES/DATASUS."), " Tudo roda localmente."),
          tags$ul(
            tags$li("Use o menu superior para alternar entre ", strong("Extrator"), " e ", strong("Manual"), "."),
            tags$li("Selecione sistema, UF, municípios, anos e meses; clique em ", strong("Processar"), "."),
            tags$li("Parquet vão para ", code("Tabelas_Todas/CNES/FINAL/"), "."),
            tags$li("Banco atualiza ", code("cnes.duckdb"), " com CNES + IBGE.")
          )
        )
      ))
    }
  })
  observeEvent(input$ir_manual_cnes, {
    removeModal()
    updateNavbarPage(session, "navbar_id", selected = "Manual")
  })
}

# 8. INICIALIZAÇÃO ----------------------------------------------------------
shinyApp(ui, server, options = list(launch.browser = TRUE))