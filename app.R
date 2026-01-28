
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
  
  # add_busy_spinner(spin = "fading-circle", color = "red"),
  
  titlePanel(
    
    # puvodni funkcni s logem
    tags$div(
      style = "display:flex; align-items:center; gap:20px;",
      tags$img(
        src = "22743_SPP_logo spp_final update.jpg",
        height = "50px"
        # src = "flowers-wolf.gif",
        # height = "200px"
      ),
      span("Kalkulačka fixní ceny ZP")
    )
  ),
  
  input_dark_mode(id = "mode"), 
  
  layout_columns( # cards beside each other
    
    card( 
      
      card_header("Vstupní informace"),
      
      textInput( 
        inputId = "text1", 
        label = tagList("Obchodník", 
                        span("*", style = "color:red")), 
        placeholder = ""
      ),
      
      textInput( 
        inputId = "text2", 
        label = tagList("Zákazník", 
                        span("*", style = "color:red")),
        placeholder = "",
        value = ""
      ) %>% 
        tagAppendAttributes(required = "required" # snaha o nastaveni povinneho pole
        ),
      
      tagList(
        tags$div("Nahrajte profil ve formátu XLS/XLSX. 
                 Soubor musí obsahovat dva sloupce: datum a profil v MWh.", 
                 style = "color: grey; margin-bottom: 5px;"),
        
        fileInput(
          inputId = "upload", 
          label = NULL,         # skryjeme původní label
          buttonLabel = "Nahraj profil",
          placeholder = "",
          accept = c(".xls", ".xlsx")
        ),
        
        plotOutput("plot")
      )
    ),
    
    card( 
      
      card_header("Výpočet ceny"),
      
      tags$div(
        tags$label("Období dodávky"),
        tags$div(style = "color: grey; margin-top: 10px; margin-bottom: 10px;",
                 "Vyberte období, pro které chcete vytvořit nabídku."),
        dateRangeInput(
          inputId = "date",
          label = NULL,     # skryjeme původní label
          separator = " - ",
          start = start_date,
          end = start_date %m+% months(1),
          min = start_date,                      # nepůjde zadat dřívější datum
          # max = ceiling_date(Sys.Date() %m+% years(3), "month")
          max <- floor_date(Sys.Date() %m+% years(4), "year") # 3 cele roky doprecdu
        )
      ),
      
      
      actionButton(
        inputId = "run", 
        label = "Výpočet ceny"),
      DT::DTOutput("results") %>% withSpinner(type = 6, color = "gold"),
      
      # textAreaInput( 
      #   inputId = "text", 
      #   label = "Poznámka", 
      #   value = "(možnost napsat komentář do pdf)"), 
      # 
      
      HTML('<span style="color:DarkGoldenRod">Předávací ani prodejní cena neobsahují náklad na BSD a toleranci.</span>'),
      
      downloadButton("downloadReport", 
                     label = "Stáhnout PDF report")
      
      
    )
  )
)


# ---------------------------------------------------- SERVER


server <- function(input, output, session) {
  
  
  # ---- OMEZENI DENNI DOBY PROVOZU APLIKACE ----
  
  ted <- Sys.time()
  hodina <- as.numeric(format(ted, "%H"))
  den <- weekdays(ted)
  
  outside_hours <- hodina < 8 || hodina >= 18
  outside_weekdays <- den %in% c("Saturday", "Sunday")
  
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
      "Aplikace je dostupná v pracovní dny  od 8:00 do 15:00.",
      easyClose = FALSE,
      footer = NULL
    ))
    
    session$close()
    return()
  }
  
  
  # ---- REAKTIVNÍ NAČTENÍ EXCELU ----
  
  data_upload <- reactive({
    req(input$upload)
    profil <- read_excel(input$upload$datapath)
    profil <- profil[, 1:2] # rande A:B
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
    
    # if (input$date[1] < min || input$date[2] > max) {
    #   
    #   showNotification(
    #     paste0(
    #       "Zvolené období musí být mezi ",
    #       format(min, "%d.%m.%Y"),
    #       " a ",
    #       format(max, "%d.%m.%Y")
    #     ),
    #     type = "error",
    #     duration = 6
    #   )
    #   
    #   # návrat na platné hodnoty
    #   updateDateRangeInput(
    #     session,
    #     "date",
    #     start = min,
    #     end = start %m+% months(1)
    #   )
    #   
    #   return()  # zastaví další zpracování
    # }
    # 
    # # zaokrouhlení na celé měsíce
    # start <- as.Date(format(input$date[1], "%Y-%m-01"))
    # end   <- as.Date(format(input$date[2], "%Y-%m-01"))
    # 
    # updateDateRangeInput(
    #   session,
    #   "date",
    #   start = start,
    #   end = end
    # )
    # 
    # delOd <- start
    # delDo <- end
    
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
      path = "data/"
    )
    
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
        paging = FALSE
      )
    )
    
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
          fix_cena = result$fix_cena
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}


# ---------------------------------------------------- APP


shinyApp(ui, server)


