# ------------------------------------------------------------------------------ SETUP


library(tidyverse)
library(readxl)

analyza_data <- function(profil, delOd, delDo, obch, zak, acq1, acq2, acq3, acq4, path = "data/") {
  
  tms_now <- Sys.time()
  print(tms_now)
  colnames(profil) <- c("datum", "profilMWh", "mesic", "rok")
  print(head(profil))
  
  denni <- read_excel(file.path("data","input_denniData.xlsx"), range = "A1:B8", col_names = T)
  print(denni)
  
   
  # conditionDEL2 <- is.Date(start)
  # if (conditionDEL2) {
  #   stop('Zadej platne datum OD')
  # }
  # 
  # conditionDEL <- is.Date(end)
  # if (conditionDEL) {
  #   stop('Zadej platne datum DO')
  # }

  head(profil)
  print(acq1)
  print(acq2)
  print(acq3)
  print(acq4)
  
  # ---------------------------------------------------------------------------- INPUT :: forward - OK
  

  a <- read_excel(file.path("data", "input_fwdKrivka.xlsx"), # ve slozce aplikace - treba denne aktualizovat
                  sheet = "Rentry")
  a$mesic <- as.Date(a$mesic, origin = "1899-12-30")
  fwd <- a %>% 
    rename("PFC" = NCG, "FX" = 'FX rate') %>% 
    filter(mesic > tms_now)  # hodnoty fwd krivky od nasledujiciho mesice
   
  
  # ------------------ kontrola ***
  
  fwdcheck <- fwd %>% filter(mesic>=delOd)
  
  # test
  # fwdcheck [20,3] <- NA
  
  conditionFWD <- any(is.na(fwdcheck$PFC))|any(fwdcheck$PFC == 0)
  if (conditionFWD) {
    stop('Neuplna FWD krivka, kontaktuj Nakup.')
  }

  print(head(fwd))
  
  # ---------------------------------------------------------------------------- INPUT :: OTC - OK
  
  
  otc_info <- file.info("X:/OTC/CSV/CZ-VTP.csv")
  otc_tms <- otc_info$mtime
  
  # b <- read.csv(file.path(path, "CZ-VTP.csv"), header = TRUE, sep = ",") # funguje i hostovane - staticky soubor
  b <- read.csv("X:/OTC/CSV/CZ-VTP.csv", header = TRUE, sep = ",") # funguje lokalne - aktualizave 15'
  otc <- b %>%
    select("season" = 1, "price" = 2) %>%
    filter(str_detect(season, "^CZ")) %>%
    mutate(
      price = as.numeric(str_replace(price, ",", ".")),
      year = case_when(
        str_detect(season, "2025|25") ~ 2025,
        str_detect(season, "2026|26") ~ 2026,
        str_detect(season, "2027|27") ~ 2027,
        str_detect(season, "2028|28") ~ 2028,
        str_detect(season, "2029|29") ~ 2029,
        TRUE ~ NA
      ),
      quater = case_when(
        str_detect(season, "Q1") ~ "Q1",
        str_detect(season, "Q2") ~ "Q2",
        str_detect(season, "Q3") ~ "Q3",
        str_detect(season, "Q4") ~ "Q4",
        TRUE ~ NA
      ),
      month = case_when(
        str_detect(season, "Jan-") ~ 1,
        str_detect(season, "Feb-") ~ 2,
        str_detect(season, "Mar-") ~ 3,
        str_detect(season, "Apr-") ~ 4,
        str_detect(season, "May-") ~ 5,
        str_detect(season, "Jun-") ~ 6,
        str_detect(season, "Jul-") ~ 7,
        str_detect(season, "Aug-") ~ 8,
        str_detect(season, "Sep-") ~ 9,
        str_detect(season, "Oct-") ~ 10,
        str_detect(season, "Nov-") ~ 11,
        str_detect(season, "Dec-") ~ 12,
        TRUE ~ NA
      ),
      cal = as.character(ifelse(str_detect(season, "^CZ VTP \\d{4}$"), paste0("Cal", str_remove(year, "^..")), NA))) %>% 
    unite(product, c("cal", "month", "quater", "year"),
          sep = "/", na.rm = T, remove = FALSE)

  print(head(otc))
  
  
  # ---------------------------------------------------------------------------- CREATE :: frame - OK
  
  
  frameOd <- as.Date("2026-01-01") # --- treba upravit pri prechodu do noveho roku
  frameDo <- as.Date("2029-12-31")
  framePer <- seq(from = frameOd, to = frameDo, by = "month")
  
  delPer <- as.POSIXct(seq(from = delOd, to = delDo, by = "month") %>% head(-1)) # head = maze posledni element (1.1.2027)
  # print(delPer)
  
  frame <- data.frame(framePer) %>%
    mutate(
      year = year(framePer),
      month = month(framePer),
      quater = case_when(
        month <= 3 ~ "Q1",
        month <= 6 ~ "Q2",
        month <= 9 ~ "Q3",
        month <= 12 ~ "Q4"
      ),
      now = ifelse(year == year(tms_now) & month == month(tms_now), 1, 0), # jaky mesic je ted
      dodavka = ifelse(framePer %in% seq(from = delOd, to = delDo, by = "month"), 1, 0),
      facq = rep(c("acq1", "acq2", "acq3", "acq4"), each = 12)
    ) %>%
    left_join(profil, by = c("framePer" = "datum")) %>%
    left_join(fwd, by = c("framePer" = "mesic")) %>% 
    mutate(dodavka = ifelse(framePer %in% delPer, 1, 0)) %>%  
    select(framePer, year, quater, month, now, dodavka, facq, profilMWh, PFC, FX)
  
  print(head(frame))
  
  
  # ---------------------------------------------------------------------------- CREATE :: data_vstup - OK
  
  
  low_spot <- denni[[2]][1]
  aktual_spot <- denni[[2]][3]
  surcharge <- denni[[2]][0]
  bsd <- denni[[2]][5]
  
  join <- frame %>%
    
    # cena komodity
    
    group_by(year) %>%
    mutate(yRatio = PFC/mean(PFC)) %>% # kdyz neni cely rok, hodi pres prumer NA
    ungroup() %>% group_by(year, quater) %>%
    
    mutate(celyQ = if_else(all(!is.na(PFC)) & is.na(yRatio), "ANO", "NE"),
           qRatio = ifelse(celyQ == "ANO", PFC/mean(PFC), NA),
           PFCratio = coalesce(yRatio, qRatio)) %>% ungroup() %>%
    select(-yRatio, -qRatio) %>%
    left_join(otc %>%
                select(year, month, "monPrice" = price), by = c("year", "month")) %>%
    left_join(otc %>%
                select(year, quater, "qPrice" = price), by = c("year", "quater")) %>%
    left_join(otc %>%
                filter(!is.na(cal)) %>%
                select(year, "calPrice" = price), by = c("year")) %>%
    mutate(otcPrice = case_when(celyQ == "NE" & is.na(PFCratio) ~ monPrice,
                                celyQ == "ANO" ~ qPrice,
                                TRUE ~ calPrice),
           PFCprepoc = round(ifelse(!is.na(PFCratio), otcPrice*PFCratio, otcPrice), 3),
           
           # kurz EUR
           
           swapPoint = round((FX-low_spot)*1000, 2), 
           FXrecalc0 = aktual_spot+swapPoint/1000, # +surcharge (ale v excelu je 0)
           # FXrecalc = round(FXrecalc0+surcharge, 4),
           FXrecalc = (FXrecalc0),
           
           cenaEUR = profilMWh*PFCprepoc,
           vazenaCena = profilMWh*FXrecalc,
           # product = paste(month, quater, year))
           product = case_when(year(tms_now) != year ~ paste0("Cal", str_remove(year, "^..")),
                               TRUE ~ paste0(month, "/", year)))
  
  data_vstup <- join %>%
    select(year, month, dodavka, facq, profilMWh, product, otcPrice, PFCprepoc, cenaEUR, FXrecalc, vazenaCena) %>%
    filter(dodavka == 1) # final df to match table on sheet Kalkulace
  
  print(head(data_vstup))
  
  
  # ------------------ kontrola ***

  # test
  # data_vstup[11, 6] <- NA
  # data_vstup[18, 4] <- -5

  conditionOTC <- any(is.na(data_vstup$otcPrice))
  if (conditionOTC) {
    prod <- data_vstup$product[is.na(data_vstup$otcPrice)]
    stop(paste0("Momentalne neni dostupna vstupni cena pro ", prod, ". Zkuste vypocet znovu za 15 min."))
    # stop("Chybi vstupni cena pro vypocet.")
  }
 
  conditionPROF <- any(is.na(data_vstup$profilMWh))
  if (conditionPROF) {
    stop('Neuplny profil, zkontrolujte vstupni data.')
  }
 
  conditionZAP <- any(data_vstup$profilMWh < 0)
  if (conditionZAP) {
    stop('Zaporna data v profilu, zkontrolujte vstupni data.')
  }

  conditionDUP <- any(duplicated(profil$datum))
  if (conditionDUP) {
    stop('V profilu se objevují duplicity, zkontrolujte vstupni data.')
  }

  acq_map <- c(acq1 = acq1, acq2 = acq2, acq3 = acq3, acq4 = acq4)
  df_acq <- data_vstup %>% 
    group_by(facq) %>% 
    summarise(acq = round(sum(profilMWh), 0)) %>% 
    ungroup() %>% 
    mutate(inp = acq_map[match(as.character(facq), names(acq_map))], 
           check = acq == inp)
  
  conditionACQ <- any(df_acq$check == F)|any(is.na(df_acq$check))
  if (conditionACQ) {
    stop('Zadane ACQ nesouhlasi se souctem v profilu, zkontrolujte vstupni data.')
  }

  print(df_acq)
  
  # ---------------------------------------------------------------------------- CALCULATE :: fix_cena - OK
  
  
  # vypocty pod tabulkou
  
  suma_profil <- round(sum(data_vstup$profilMWh, na.rm = TRUE), 0)
  acq <- data_vstup %>% 
    group_by(year) %>% summarise(rocni_odber = round(sum(profilMWh, na.rm = T), 0))
  suma_cenaEUR <- sum(data_vstup$cenaEUR, na.rm = TRUE)
  suma_vazenaCena <- sum(data_vstup$vazenaCena, na.rm = TRUE)
  mean_PFC <- mean(data_vstup$PFCprepoc, na.rm = TRUE)
  nakup <- suma_cenaEUR/suma_profil
  prirazka_nakup <- 0.000 #  ???
  kurz <- suma_vazenaCena/suma_profil
  prodej_eur <- nakup+prirazka_nakup
  prodej_czk <- prodej_eur*kurz
  
  # # vypocty nad tabulkou
  
  naklad_profil <- round(nakup-mean_PFC, 2)
  fin_cenaEUR <- ceiling(prodej_eur/0.025) * 0.025 # zaokrouhleni na nejblizsi nejvyssi hranici 0,025
  fin_cenaCZK <- ceiling((fin_cenaEUR*kurz)/0.05) * 0.05 # zaokrouhleni na nejblizsi nejvyssi hranici 0,05
  
  print("Vypocty probehly :-)")
  
  # ------------------ kontrola ***
  
  conditionNPF <- naklad_profil < 0
  if (conditionNPF) {
    print('Zaporny naklad na profil')
  }

  
  # ---------------------------------------------------------------------------- CREATE :: marze - IP
  
  
  marzeMin <- denni[[2]][6]
  marzeDop <- denni[[2]][7]
  # marzeMin <- 1.2
  # marzeDop <- 6
  txt_marzeMin <- sprintf("%.2f", marzeMin) # text, zobrazuje cislo s presne 2 decimals
  txt_marzeDop <- sprintf("%.2f", marzeDop)
  
  
  # ---------------------------------------------------------------------------- TABLE :: fix_cena - OK
  
  
  fix_cena <- data.frame(
    Parametr = c(
      "Obchodník",
      "Zákazník",
      "Období dodávky",
      "Vytvoření nabídky",
      "Platnost nabídky",
      "CQ [MWh]",
      "ACQ [MWh]",
      "Předávací cena pro obchod [€]",
      "Předávací cena pro obchod [CZK]",
      "HM1 [€]",
      "Prodejní cena pro zákazníka [€]",
      "Prodejní cena pro zákazníka [CZK]"
    #   "Prumer EUR", # pro testovani, pak odstranit
    #   "Suma ceny EUR", # pro testovani, pak odstranit
    #   "Suma vazene ceny", # pro testovani, pak odstranit
    #   "Nakup", # pro testovani, pak odstranit
    #   "Prodej", # pro testovani, pak odstranit
    #   "Kurz", # pro testovani, pak odstranit
    #   "Naklad na profil" # pro testovani, pak odstranit
    ),
    
    Hodnota = c(
      obch,
      zak,
      paste0(delOd, " až ", delDo),
      format(as.POSIXct(tms_now, origin = "1899-12-30"), "%F %R"),
      paste0(format(as.POSIXct(Sys.Date(), origin = "1899-12-30"), "%F"), " 15:00"),
      suma_profil,
      paste(
        paste(acq$year, acq$rocni_odber, sep = ": "),
        collapse = ", "),
      round(fin_cenaEUR, 2),
      round(fin_cenaCZK, 2),
      paste("Minimální:", txt_marzeMin, " /  Doporučená:", txt_marzeDop),
      paste("Minimální:",  round(fin_cenaEUR+marzeMin, 2), 
            " /  Doporučená:",  round(fin_cenaEUR+marzeDop)),
      paste("Minimální:", round(fin_cenaCZK+marzeMin*kurz, 2), 
            " /  Doporučená:", round(fin_cenaCZK+marzeDop*kurz, 2))
      # round(mean_PFC, 3), # pro testovani, pak odstranit
      # round(suma_cenaEUR, 0), # pro testovani, pak odstranit
      # round(suma_vazenaCena, 0), # pro testovani, pak odstranit
      # round(nakup, 3), # pro testovani, pak odstranit
      # round(prodej_czk, 3), # pro testovani, pak odstranit
      # round(kurz, 2), # pro testovani, pak odstranit
      # naklad_profil # pro testovani, pak odstranit
    )
  )
  
  colnames(fix_cena) <- NULL
  
  
  # ---------------------------------------------------------------------------- REPORTS :: txt, pdf - DONE
  
  # simple txt
  
  on.exit({
    log <- paste(tms_now, obch, zak, suma_profil, delOd, delDo, fin_cenaEUR, sep = ";")
    write(log, "data/kalkulackaZP_log.txt", append = TRUE)
  })
  
  # pdf pro Nakup
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  p <- ggplot(profil, aes(datum, profilMWh)) +
    # geom_line(linewidth = 2, color = "gold") +
    geom_col(fill = "gold") +
    labs(x = "měsíc dodávky",
         y = "profil spotřeby [MWh]",
         title = "Profil spotřeby klienta") +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    scale_y_continuous(breaks = seq(0, 700, by = 50))+
    theme_light() +
    theme(axis.text.x = element_text(angle = 90))
  
  rmarkdown::render(
    input = "reportNakup.Rmd",
    output_file = paste0(path, "pdf_logy/proNakup_VypocetFixCenyZP_report_", timestamp, ".pdf"),
    output_format = "pdf_document",
    # output_dir = output_dir,
    params = list(
      obchodnik = obch,
      zakaznik = zak,
      datum_od = delOd,
      datum_do = delDo,
      profil = profil,
      plot_profil = p,
      fwd = fwd,
      otc = otc,
      fix_cena = fix_cena
    ),
    # envir = new.env(parent = globalenv())
    # ),
    quiet = TRUE
  )

  
  # ---------------------------------------------------------------------------- RETURN :: fix_cena - OK
  
    
  return(list(
    profil = profil,
    fwd = fwd,
    otc = otc,
    fix_cena = fix_cena
  ))
}

