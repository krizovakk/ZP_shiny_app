
# rsconnect::deployApp("C:/Users/krizova/Documents/R/02 cenoveKalkukacky/_vyvoj/shiny_app")

library(shiny) # aplikace
library(bslib) # aplikace (layouty)
library(shinycssloaders) # wip spinner

library(tidyverse)
library(readxl)
library(writexl)

library(zoo) # ?
library(rvest) # html
library(DT) # render table


app_online <- TRUE
# app_online <- FALSE # v pripade potreby vynout appku z nasi strany

options(shiny.launch.browser = TRUE)

# Sys.setenv(R_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")

start_date <- as.Date(cut(Sys.Date(), "month")) + months(1) # 1. den nasledujiciho mesice


# ---------------------------------------------------- UI


ui <- page_fillable(
  
  titlePanel(
    
    tags$div(
      style = "display:flex; align-items:center; gap:20px;",
      tags$img(
        src = "22743_SPP_logo spp_final update.jpg",
        height = "50px"),
      span("Kalkulačka fixní ceny ZP")
    )
  ),
  
  input_dark_mode(id = "mode"), 
  
  layout_columns( # cards beside each other
    
    card( 
      
      card_header(tags$span("Vstupní informace", class = "fs-5", style = "color: #d4af37;")),
      
      fluidRow(
        
        column(3, textInput(
          "text1", tagList("Obchodník",span("*", style = "color:red")), placeholder = "")),
        column(3, textInput(
          "text2",tagList("Zákazník",span("*", style = "color:red")), placeholder = ""))),
      
      dateRangeInput(
        inputId = "date",
        label = "Období dodávky pro vytvoření nabídky", 
        separator = " - ",
        start = start_date,
        end = start_date %m+% months(1),
        min = start_date,                      # nepůjde zadat dřívější datum
        # max = ceiling_date(Sys.Date() %m+% years(3), "month")
        max <- floor_date(Sys.Date() %m+% years(4), "year")), # 3 cele roky doprecdu
      
      tagList(
        tags$div("Vyplňte ACQ pro relevantní období.",
                 style = "color: grey;
                            margin-top: 15px;
                            margin-bottom: 0px;
                            font-style: italic;
                            font-size: 0.85em;")),
      
      fluidRow(
        
        column(3, numericInput("acq1", "ACQ 2026", "")),
        column(3, numericInput("acq2", "ACQ 2027", "")),
        column(3, numericInput("acq3", "ACQ 2028", "")),
        column(3, numericInput("acq4", "ACQ 2029", ""))),
      
      tagList(
        tags$div("Nahrajte profil ve formátu XLS/XLSX.\n
                 Soubor musí obsahovat dva sloupce: datum a profil v MWh.", 
                 style = "color: grey;
                          margin-top: 10px;
                          margin-bottom: 0px;
                          font-style: italic;
                          font-size: 0.85em;")),
      
      fileInput(
        inputId = "upload", 
        label = NULL,         # skryjeme původní label
        buttonLabel = "Nahraj profil",
        placeholder = "",
        accept = c(".xls", ".xlsx")),
      
      plotOutput("plot")
      
    ), # konec prvni karty
    
    card( 
      
      card_header(tags$span("Výpočet ceny", class = "fs-5", style = "color: #d4af37;")),
      
      actionButton(
        inputId = "run", 
        label = "Výpočet ceny"),
      
      DT::DTOutput("results") %>% withSpinner(type = 6, color = "gold"),
      
      
      HTML('<span style="color:DarkGoldenRod">Předávací ani prodejní cena neobsahují náklad na BSD a toleranci.</span>'),
      
      
      fluidRow(
        column(12,
               textAreaInput(
                 inputId = "note",
                 label = "Poznámka do PDF reportu",
                 value = "",
                 rows = 3))),
      
      # textAreaInput(
      #   inputId = "note",
      #   label = "Poznámka do PDF reportu",
      #   value = "",
      #   cols = 200,
      #   rows = 3),
      
      downloadButton("downloadReport", 
                     label = "Stáhnout PDF report")
      
      
    ) # konec druhe karty
  ) # konec layout_columns
) # konec UI


# ---------------------------------------------------- SERVER


server <- function(input, output, session) {
  
  
  # ---- OMEZENI DENNI DOBY PROVOZU APLIKACE ----
  
  ted <- Sys.time()
  hodina <- as.numeric(format(ted, "%H"))
  den <- weekdays(ted)
  
  outside_hours <- hodina < 8 || hodina >= 18
  outside_weekdays <- den %in% c("Saturday", "Sunday")
  
  otc_info <- file.info("X:/OTC/CSV/CZ-VTP.csv")
  otc_tms <- otc_info$mtime
  
  if (!app_online) {
    showModal(modalDialog(
      title = "Aplikace je z provozních důvodů momentálně nedostupná",
      "Prosím, kontaktujte Nákupní oddělení.",
      easyClose = FALSE,
      footer = NULL
    ))
    session$close()
    return()
  }
  
  if (outside_hours || outside_weekdays) {
    showModal(modalDialog(
      title = "Aplikace není k dispozici",
      "Aplikace je dostupná v pracovní dny  od 10:30 do 15:00.",
      easyClose = FALSE,
      footer = NULL))
    
    session$close()
    return()
  }
  
  if (ted-otc_tms > 60) { # timediff se pocita v minutach
    showModal(modalDialog(
      title = "Aplikace není k dispozici",
      "Vstupní data nejsou aktuální, prosím, kontaktujte Nákupní oddělení.",
      easyClose = FALSE,
      footer = NULL))
    
    session$close()
    return()
  }
  
  
  # ---- REAKTIVNÍ NAČTENÍ EXCELU ----
  
  data_upload <- reactive({
    req(input$upload)
    profil <- read_excel(input$upload$datapath)
    profil <- profil[, 1:2] # range A:B
    profil
  })
  
  # ---- PLOT ----
  
  output$plot <- renderPlot({
    profil <- data_upload()
    req(profil)
    colnames(profil) <- c("datum", "profilMWh")
    ggplot(profil, aes(datum, profilMWh)) +
      # geom_line(linewidth = 2, color = "gold") +
      geom_col(fill = "gold") +
      labs(x = "měsíc dodávky",
           y = "profil spotřeby [MWh]",
           title = "Profil spotřeby klienta") +
      scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      scale_y_continuous(breaks = seq(0, 700, by = 50))+
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  # ---- ZADANI OBDOBI DODAVKY ----
  
  observeEvent(input$run, {
    req(input$date)
    
    start <- as.Date(format(input$date[1], "%Y-%m-01"))
    end   <- as.Date(format(input$date[2], "%Y-%m-01"))
    
    updateDateRangeInput( # uprava datumu na cele mesice
      session,
      "date",
      start = start,
      end = end
    )
    
    delOd <- start
    delDo <- end
  })
  
  # ---- SPUSTENI VYPOCTU ----
  # ---- GENEROVANI PDF ----
  
  vysledek <- eventReactive(input$run, {
    
    req(input$upload, input$date, input$text1, input$text2)
    
    profil <- read_excel(input$upload$datapath) %>%
      select("datum" = 1, "profilMWh" = 2) %>% 
      mutate(mesic = month(datum),
             rok = year(datum))  # načtení nahraného profilu
    
    start <- as.Date(format(input$date[1], "%Y-%m-01"))
    end <- as.Date(format(input$date[2], "%Y-%m-01"))
    obch <- input$text1
    zak <- input$text2
    
    delOd <- start
    delDo <- end
    
    source("analyza.R") 
    
    analyza_data(
      profil,
      start,
      end,
      obch = input$text1,
      zak  = input$text2,
      acq1  = input$acq1,
      acq2  = input$acq2,
      acq3  = input$acq3,
      acq4  = input$acq4,
      path = "data/")
  })
  
  output$results <- DT::renderDT({
    
    result <- vysledek()
    
    fix_cena <- result$fix_cena
    validate(need(is.data.frame(fix_cena), "Výsledek není datová tabulka"))
    
    DT::datatable(
      fix_cena,
      rownames = FALSE,
      options = list(
        dom = 't',       # odstraní paging a search
        ordering = FALSE,
        paging = FALSE))
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("VypocetFixCenyZP_report_", Sys.time(), ".pdf")
    },
    contentType = "application/pdf",
    content = function(file) {
      
      result <- vysledek()
      
      rmarkdown::render(
        input = "report.Rmd",
        output_format = "pdf_document",
        output_file = file,
        params = list(
          obchodnik = input$text1,
          zakaznik = input$text2,
          datum_od = input$date[1],
          datum_do = input$date[2],
          profil = data_upload(),
          # plot_profil = output$plot,
          fwd = result$fwd,
          otc = result$otc,
          fix_cena = result$fix_cena,
          note = input$note),
        envir = new.env(parent = globalenv()))
    }
  )
} # konec serveru


# ---------------------------------------------------- APP


shinyApp(ui, server)


