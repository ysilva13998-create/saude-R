# ==============================================================================
# EXTRATOR SIA - Múltiplos anos × meses × cidades
# Versão standalone com manual embutido + avisos
# ==============================================================================
# COMO USAR:
#   1. Tenha o R instalado (https://cran.r-project.org/)
#   2. Abra este arquivo no RStudio OU dê duplo clique em Iniciar_App.bat
#   3. O navegador abrirá automaticamente
# ==============================================================================

# 0. AVISO INICIAL -----------------------------------------------------------
cat("\n======================================================================\n")
cat("   EXTRATOR SIA - INICIANDO\n")
cat("======================================================================\n")

# 1. INSTALAÇÃO AUTOMÁTICA DE PACOTES ---------------------------------------
pacotes_necessarios <- c(
  "shiny", "microdatasus", "dplyr", "arrow",
  "stringr", "DT", "shinyWidgets"
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
  library(microdatasus)
  library(dplyr)
  library(arrow)
  library(stringr)
  library(DT)
  library(shinyWidgets)
})

# 2. CONFIGURAÇÃO DE AMBIENTE -----------------------------------------------
# Detecta a pasta do app automaticamente (funciona com duplo clique)
pasta_app <- getwd()
RAIZ      <- file.path(pasta_app, "Tabelas_Todas")
MUNICIPIOS <- file.path(RAIZ, "_METADADOS",
                        "br_bd_diretorios_brasil_municipio.parquet")

# Cria estrutura se não existir
dir.create(file.path(RAIZ, "_METADADOS"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RAIZ, "SIA", "FINAL"), recursive = TRUE, showWarnings = FALSE)

cat("📂 Pasta do app   :", pasta_app, "\n")
cat("📂 Pasta de dados :", RAIZ, "\n")
if (!file.exists(MUNICIPIOS)) {
  cat("⚠  Metadados de municípios ausentes:", MUNICIPIOS, "\n")
  cat("   O app vai rodar, mas não conseguirá carregar a lista de cidades.\n")
  cat("   Coloque o arquivo parquet em:", MUNICIPIOS, "\n")
}

MESES_PT <- c("Janeiro","Fevereiro","Março","Abril","Maio","Junho",
              "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")

ESTADOS_BR <- c("Acre"="AC","Alagoas"="AL","Amapá"="AP","Amazonas"="AM",
  "Bahia"="BA","Ceará"="CE","Distrito Federal"="DF","Espírito Santo"="ES",
  "Goiás"="GO","Maranhão"="MA","Mato Grosso"="MT","Mato Grosso do Sul"="MS",
  "Minas Gerais"="MG","Pará"="PA","Paraíba"="PB","Paraná"="PR",
  "Pernambuco"="PE","Piauí"="PI","Rio de Janeiro"="RJ","Rio Grande do Norte"="RN",
  "Rio Grande do Sul"="RS","Rondônia"="RO","Roraima"="RR","Santa Catarina"="SC",
  "São Paulo"="SP","Sergipe"="SE","Tocantins"="TO")

SISTEMAS_SIA <- c(
  "SIA - Produção (PA)"        = "SIA-PA",
  "SIA - BPA Consolidado (BI)" = "SIA-BI",
  "SIA - APAC Quimio (AQ)"     = "SIA-AQ",
  "SIA - APAC Nefro (AN)"      = "SIA-AN"
)

ANOS_DISPONIVEIS <- 2000:2026

# 3. UI DO MANUAL (NOVA ABA) -------------------------------------------------
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

    titlePanel(h2(icon("book-open"), " Manual do Extrator SIA",
                  style = "color:#2c3e50;")),
    br(),

    div(class = "manual-box",
      h4(icon("rocket"), " Início rápido"),
      p("Este app baixa dados do SIA (Sistema de Informações Ambulatoriais) do DATASUS."),
      tags$ol(
        tags$li(strong("Escolha o sistema"), " (PA, BI, AQ ou AN)."),
        tags$li(strong("Escolha a UF"), " (ex.: PR)."),
        tags$li(strong("Marque os anos"), " (pode selecionar vários)."),
        tags$li(strong("Marque os municípios"), " (pode selecionar vários)."),
        tags$li(strong("Marque os meses"), " (ou deixe 'Baixar TODOS os 12 meses')."),
        tags$li(strong("Clique em Processar"), " e aguarde.")
      )
    ),

    div(class = "manual-alert",
      icon("triangle-exclamation"), strong(" Primeira execução:"),
      " a instalação de pacotes pode levar de 5 a 15 minutos. Depois disso abre em segundos."
    ),

    div(class = "manual-box",
      h4(icon("folder-tree"), " Estrutura de pastas"),
      p("Os arquivos são salvos em ", code("Tabelas_Todas/SIA/FINAL/"), ":"),
      div(class = "manual-code",
        "ExtratorSIA/", br(),
        "  └─ Tabelas_Todas/", br(),
        "      ├─ _METADADOS/", br(),
        "      │   └─ br_bd_diretorios_brasil_municipio.parquet", br(),
        "      └─ SIA/FINAL/", br(),
        "          └─ FINAL_SIA-PA_PR_Irati_2024_01.parquet"
      ),
      p("Formato do nome: ", code("FINAL_<SISTEMA>_<UF>_<CIDADE>_<ANO>_<MÊS>.parquet"))
    ),

    div(class = "manual-box",
      h4(icon("database"), " Sobre o metadados de municípios"),
      p("O app lê a lista de cidades deste arquivo:"),
      div(class = "manual-code", "Tabelas_Todas/_METADADOS/br_bd_diretorios_brasil_municipio.parquet"),
      p("Se você não tiver esse arquivo, baixe no portal Base dos Dados:"),
      tags$a(href = "https://basedosdados.org/dataset/br-bd-diretorios-brasil",
             target = "_blank", "basedosdados.org → br_bd_diretorios_brasil"),
      p("Depois coloque-o dentro da pasta ", code("_METADADOS/"), ".")
    ),

    div(class = "manual-box",
      h4(icon("gears"), " Como funciona por dentro"),
      p("O algoritmo otimiza o download em 3 laços aninhados:"),
      tags$ul(
        tags$li(strong("Ano → Mês → Cidade.")),
        tags$li("Para cada ", strong("mês"), ", ele baixa os dados ", strong("uma vez"), " (do estado todo), ",
                "filtra todas as cidades do mês e depois ", strong("libera o cache"), " (memória)."),
        tags$li("Isso evita baixar o mesmo mês várias vezes para cidades diferentes.")
      ),
      p("Timeout por requisição: ", strong("600 segundos"), " (10 min).")
    ),

    div(class = "manual-box",
      h4(icon("table"), " Aba Log"),
      p("Mostra o resumo final:"),
      tags$ul(
        tags$li("Status (Concluído / Sem dados)"),
        tags$li("Total de linhas baixadas"),
        tags$li("Cidades com dados encontrados"),
        tags$li("Arquivos gerados (caminho completo)")
      )
    ),

    div(class = "manual-box",
      h4(icon("file-export"), " Aba Arquivos"),
      p("Tabela com nome e tamanho (MB) de cada arquivo gerado.")
    ),

    div(class = "manual-ok",
      icon("circle-check"), strong(" Dica:"), " combinações sem dados (ex.: cidade pequena em mês com poucos atendimentos) ",
      "aparecem no log como 'combinações sem dados' — é normal, não é erro."
    ),

    div(class = "manual-box",
      h4(icon("bug"), " Solução de problemas"),
      tags$ul(
        tags$li(strong("“Nenhum município aparece”:"), " verifique se o arquivo ",
                code("_METADADOS/br_bd_diretorios_brasil_municipio.parquet"), " existe."),
        tags$li(strong("“Timeout ao baixar”:"), " reduza a quantidade de anos/meses por vez."),
        tags$li(strong("“Erro de conexão com DATASUS”:"), " o servidor do DATASUS pode estar instável; tente de novo mais tarde."),
        tags$li(strong("“Navegador não abriu”:"), " copie a URL do console (ex.: ",
                code("http://127.0.0.1:XXXX"), ") e cole no navegador."),
        tags$li(strong("“Acentos quebrados nos arquivos”:"), " o app já converte de latin1 para UTF-8 automaticamente.")
      )
    ),

    div(class = "manual-box",
      h4(icon("info-circle"), " Sobre"),
      p("Extrator SIA - versão standalone."),
      p("Fonte: DATASUS via pacote ", code("microdatasus"), ".")
    )
  )
}

# 4. UI PRINCIPAL -----------------------------------------------------------
ui <- navbarPage(
  title = tags$div(icon("file-medical"), " Extrator SIA"),
  header = tags$head(tags$style(HTML(".navbar-default { background-color:#2c3e50; }
                                      .navbar-default .navbar-brand,
                                      .navbar-default .navbar-nav > li > a { color:#ecf0f1; }"))),

  tabPanel("Extrator", icon = icon("database"),
    fluidPage(
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("sistema", "Sistema SIA:", choices = SISTEMAS_SIA),
          selectInput("uf", "UF:", choices = ESTADOS_BR, selected = "PR"),

          pickerInput(
            "anos", "Anos (pode escolher vários):",
            choices = ANOS_DISPONIVEIS, selected = 2024, multiple = TRUE,
            options = list(
              `actions-box`       = TRUE,
              `select-all-text`   = "Todos",
              `deselect-all-text` = "Nenhum",
              `none-selected-text`= "Selecione..."
            )
          ),

          pickerInput(
            "municipios", "Municípios (pode escolher vários):",
            choices = NULL, multiple = TRUE,
            options = list(
              `actions-box`       = TRUE,
              `live-search`       = TRUE,
              `select-all-text`   = "Todos da UF",
              `deselect-all-text` = "Nenhum",
              `none-selected-text`= "Selecione..."
            )
          ),

          prettyCheckbox("todos_meses", "Baixar TODOS os 12 meses",
                         value = TRUE, icon = icon("check"),
                         status = "success", shape = "curve"),

          conditionalPanel(
            condition = "!input.todos_meses",
            pickerInput(
              "meses", "Meses a baixar:",
              choices = setNames(1:12, MESES_PT), selected = 1, multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `select-all-text` = "Todos",
                `deselect-all-text` = "Nenhum"
              )
            )
          ),

          actionButton("processar", "Processar", class = "btn-primary btn-lg",
                       icon = icon("play"), style = "width:100%;"),
          br(), br(),
          helpText("Salva em: ", tags$code("Tabelas_Todas/SIA/FINAL/")),
          helpText("1 arquivo por cidade/ano/mês. Timeout: 600s."),
          helpText("Ordem: ano → mês → cidades (cache liberado a cada mês).")
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
            tabPanel("Arquivos", DTOutput("arquivos_gerados"))
          )
        )
      )
    )
  ),

  tabPanel("Manual", icon = icon("book-open"), ui_manual())
)

# 5. SERVER (mesma lógica do seu, só troquei RAIZ fixo pela variável já definida) 
server <- function(input, output, session) {

  stats <- reactiveValues(
    start_time = NULL, end_time = NULL, linhas = 0,
    status = "Aguardando", arquivos = character(0),
    falhas = character(0), cidades_ok = character(0)
  )

  municipios_df <- reactive({
    if (!file.exists(MUNICIPIOS)) {
      showNotification(
        "❌ Metadados de municípios não encontrados em _METADADOS/. Veja o Manual.",
        type = "error", duration = 12
      )
      return(NULL)
    }
    tryCatch({
      read_parquet(MUNICIPIOS) %>%
        mutate(id6 = as.character(id_municipio_6)) %>%
        select(id6, nome, sigla_uf)
    }, error = function(e) {
      showNotification(paste("❌ Erro ao ler metadados:", e$message),
                       type = "error", duration = 12)
      NULL
    })
  })

  observeEvent(input$uf, {
    req(municipios_df())
    cidades <- municipios_df() %>% filter(sigla_uf == input$uf) %>% arrange(nome)
    updatePickerInput(
      session, "municipios",
      choices = setNames(cidades$id6, cidades$nome),
      selected = character(0)
    )
  })

  salvar_parquet <- function(df, sistema, uf, ano, mes, mun_nome) {
    path_final <- file.path(RAIZ, "SIA", "FINAL")
    if (!dir.exists(path_final)) dir.create(path_final, recursive = TRUE)

    names(df) <- make.names(names(df))
    df <- df %>% mutate(across(where(is.character),
                               ~ iconv(., from = "latin1", to = "UTF-8", sub = "")))

    mes_fmt <- sprintf("%02d", as.numeric(mes))

    nome <- if (!is.null(mun_nome) && mun_nome != "") {
      paste0("FINAL_", sistema, "_", uf, "_", mun_nome, "_", ano, "_", mes_fmt, ".parquet")
    } else {
      paste0("FINAL_", sistema, "_", uf, "_", ano, "_", mes_fmt, ".parquet")
    }

    caminho <- file.path(path_final, nome)
    write_parquet(df, caminho)
    cat("   ✅", basename(nome), "-", format(nrow(df), big.mark = "."), "linhas\n")
    return(caminho)
  }

  observeEvent(input$processar, {
    stats$start_time <- Sys.time()
    stats$status     <- "Processando..."
    stats$linhas     <- 0
    stats$arquivos   <- character(0)
    stats$falhas     <- character(0)
    stats$cidades_ok <- character(0)
    stats$end_time   <- NULL

    if (is.null(municipios_df())) {
      showNotification("Metadados ausentes. Veja o Manual.", type = "error", duration = 8)
      stats$status <- "Erro: metadados ausentes"
      return()
    }

    anos <- sort(unique(as.integer(input$anos)))
    if (length(anos) == 0) {
      showNotification("Selecione ao menos um ano.", type = "warning")
      stats$status <- "Aguardando seleção"
      return()
    }

    cidades_cod <- input$municipios
    if (is.null(cidades_cod) || length(cidades_cod) == 0) {
      showNotification("Selecione ao menos um município.", type = "warning")
      stats$status <- "Aguardando seleção"
      return()
    }

    cidades_nomes <- sapply(cidades_cod, function(cod) {
      municipios_df() %>% filter(id6 == cod) %>% pull(nome) %>% first()
    })

    meses <- if (isTRUE(input$todos_meses)) 1:12 else as.integer(input$meses)
    if (is.null(meses) || length(meses) == 0) {
      showNotification("Selecione ao menos um mês.", type = "warning")
      stats$status <- "Aguardando seleção"
      return()
    }
    meses <- sort(unique(meses))

    cat("\n═══════════════════════════════════════════\n")
    cat("ANOS:", length(anos), "→", paste(anos, collapse = ", "), "\n")
    cat("MESES:", length(meses), "→", paste(MESES_PT[meses], collapse = ", "), "\n")
    cat("CIDADES:", length(cidades_cod), "→", paste(cidades_nomes, collapse = " | "), "\n")
    cat("TOTAL DE TAREFAS:", length(anos) * length(meses) * length(cidades_cod), "\n")
    cat("═══════════════════════════════════════════\n\n")

    total_linhas <- 0
    arquivos     <- character(0)
    falhas       <- character(0)
    cidades_ok   <- character(0)
    total_tarefas <- length(anos) * length(meses) * length(cidades_cod)
    k <- 0

    withProgress(message = "Baixando SIA...", value = 0, {
      for (a in seq_along(anos)) {
        ano_atual <- anos[a]
        for (j in seq_along(meses)) {
          m <- meses[j]
          cat(sprintf("\n⬇️  [Ano %d/%d | Mês %d/%d] Baixando %s - %d - %s\n",
                      a, length(anos), j, length(meses),
                      input$uf, ano_atual, MESES_PT[m]))

          dados_full <- tryCatch(
            fetch_datasus(
              year_start = ano_atual, year_end = ano_atual,
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
            cat("   ⚠️ Sem dados para", ano_atual, "/", MESES_PT[m], "\n")
            falhas <- c(falhas, paste("(todos)", ano_atual, MESES_PT[m], sep = " / "))
            k <- k + length(cidades_cod)
            incProgress(length(cidades_cod) / total_tarefas * 0.97,
                        detail = paste(ano_atual, MESES_PT[m], "- sem dados"))
            next
          }

          names(dados_full) <- trimws(names(dados_full))
          cat("   📦", format(nrow(dados_full), big.mark = "."), "linhas baixadas\n")

          for (i in seq_along(cidades_cod)) {
            cod_cidade <- cidades_cod[i]
            nom_cidade <- cidades_nomes[i]
            k <- k + 1

            incProgress(1 / total_tarefas * 0.97,
                        detail = paste(ano_atual, MESES_PT[m], "-", nom_cidade))

            dados <- dados_full
            col_prod <- intersect(names(dados), c("PA_UFMUN","UFMUN","MUNIC_PRES"))[1]
            col_cons <- intersect(names(dados), c("PA_MUNPCN","MUNPCN","MUNIC_RES"))[1]

            if (!is.na(col_prod) && !is.na(col_cons)) {
              dados <- dados %>%
                mutate(
                  prod_join = substr(str_pad(as.character(!!sym(col_prod)), 6, pad="0"), 1, 6),
                  cons_join = substr(str_pad(as.character(!!sym(col_cons)), 6, pad="0"), 1, 6)
                ) %>%
                filter(prod_join == cod_cidade | cons_join == cod_cidade) %>%
                select(-prod_join, -cons_join)
            }

            if (nrow(dados) == 0) {
              cat(sprintf("   ⚠️ %s: sem dados em %d/%s\n",
                          nom_cidade, ano_atual, MESES_PT[m]))
              falhas <- c(falhas, paste(nom_cidade, ano_atual, MESES_PT[m], sep = " / "))
              next
            }

            caminho <- salvar_parquet(dados, input$sistema, input$uf,
                                      ano_atual, m, nom_cidade)
            arquivos     <- c(arquivos, caminho)
            total_linhas <- total_linhas + nrow(dados)
            if (!(nom_cidade %in% cidades_ok)) cidades_ok <- c(cidades_ok, nom_cidade)
            rm(dados)
          }

          cat(sprintf("🗑️  Cache %d/%s liberado (%.0f MB)\n",
                      ano_atual, MESES_PT[m],
                      as.numeric(object.size(dados_full)) / 1024^2))
          rm(dados_full); gc(verbose = FALSE)
        }
      }
      incProgress(0.03, detail = "Concluído!")
    })

    stats$end_time   <- Sys.time()
    stats$linhas     <- total_linhas
    stats$arquivos   <- arquivos
    stats$falhas     <- falhas
    stats$cidades_ok <- cidades_ok
    stats$status     <- if (length(arquivos) > 0) "Concluído" else "Sem dados"
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
    cat("Linhas totais:", format(stats$linhas, big.mark = "."), "\n")
    cat("Cidades com dados:", paste(stats$cidades_ok, collapse = ", "), "\n")
    cat("Arquivos gerados:", length(stats$arquivos), "\n")
    if (length(stats$falhas) > 0) {
      cat("Combinações sem dados:", length(stats$falhas), "\n")
    }
    cat("\n")
    for (f in stats$arquivos) cat("  ✅", basename(f), "\n")
  })

  output$arquivos_gerados <- renderDT({
    req(length(stats$arquivos) > 0)
    df <- data.frame(
      Arquivo    = basename(stats$arquivos),
      Tamanho_MB = round(file.info(stats$arquivos)$size / 1024 / 1024, 2)
    )
    datatable(df, options = list(pageLength = 12), rownames = FALSE)
  })

  # Modal de boas-vindas na 1ª execução
  ja_mostrou <- reactiveVal(FALSE)
  observe({
    if (!ja_mostrou()) {
      ja_mostrou(TRUE)
      showModal(modalDialog(
        title = tags$div(icon("hand-wave"), " Bem-vindo ao Extrator SIA!"),
        size = "l", easyClose = TRUE,
        footer = tagList(
          actionButton("ir_manual_sia", "📖 Abrir Manual", class = "btn-info"),
          modalButton("✅ Começar")
        ),
        tags$div(class = "manual-box",
          style = "border-left-color:#198754;",
          tags$p(tags$b("Este app baixa dados do DATASUS/SIA."), " Tudo roda localmente."),
          tags$ul(
            tags$li("Use o menu superior para alternar entre ", strong("Extrator"), " e ", strong("Manual"), "."),
            tags$li("Selecione sistema, UF, anos, municípios e meses; clique em ", strong("Processar"), "."),
            tags$li("Os arquivos vão para ", code("Tabelas_Todas/SIA/FINAL/"), "."),
            tags$li("Se a lista de cidades estiver vazia, falta o arquivo em ",
                    code("_METADADOS/"), " — veja o Manual.")
          )
        )
      ))
    }
  })
  observeEvent(input$ir_manual_sia, {
    removeModal()
    updateNavbarPage(session, "navbar_id", selected = "Manual")
  })
}

# 6. INICIALIZAÇÃO ----------------------------------------------------------
shinyApp(ui, server, options = list(launch.browser = TRUE))