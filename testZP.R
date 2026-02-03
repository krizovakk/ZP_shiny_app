
# require(tidyverse)
# require(readxl)

tms_now <- Sys.time()
print(tms_now)
colnames(profil) <- c("datum", "profilMWh", "mesic", "rok")
print(head(profil))

# pro test

delOd <- as.Date("2026-02-01")
delDo <- as.Date("2028-03-01")

profil <- read_excel("C:/Users/krizova/Documents/R/02 cenoveKalkukacky/ZP/ZP_shiny_app/data/input_profil.xlsx")


# ---------------------------------------------------------------------------- INPUT :: forward - OK

# test
a <- read_excel("C:/Users/krizova/Documents/R/02 cenoveKalkukacky/ZP/ZP_shiny_app/data/input_fwdKrivka.xlsx", sheet = "Rentry")
a$mesic <- as.Date(a$mesic, origin = "1899-12-30")
fwd <- a %>% 
  rename("PFC" = NCG, "FX" = 'FX rate') %>% 
  filter(mesic > tms_now)   # hodnoty fwd krivky od nasledujiciho mesice
  
# fwd_akt <- lubridate::date(unique(fwd$akt))
# 
# # if (any(is.na(fwd$PFC))) stop("Nemáš komplet PFC krivku")
# # if (any(is.na(fwd$FX))) stop("Nemáš komplet FX krivku")

# test

# fwd [20,3] <- NA
# fwd [20,3] <- 0

fwdcheck <- fwd %>% filter(mesic>=delOd)

conditionFWD <- any(is.na(fwdcheck$PFC))|any(fwdcheck$PFC == 0)
if (conditionFWD) {
  stop('Neuplna FWD krivka')
}

# ---------------------------------------------------------------------------- INPUT :: OTC - OK


b <- read.csv("C:/Users/krizova/Documents/R/02 cenoveKalkukacky/ZP/ZP_shiny_app/data/CZ-VTP.csv", header = TRUE, sep = ",") # test
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
    cal = as.character(ifelse(str_detect(season, "^CZ VTP \\d{4}$"), paste0("Cal", str_remove(year, "^..")), NA))
  ) %>% 
  # mutate(product = paste0(cal, month, quater, year), collapse = NULL, na.rm = FALSE))
    unite(product, c("cal", "month", "quater", "year"),
      sep = "/", na.rm = T, remove = FALSE) 

# test

# otc[8,2] <- NA
# otc[8,2] <- 31.270



# ---------------------------------------------------------------------------- CREATE :: frame - OK


# frameOd <- as.Date("2025-01-01")
# frameDo <- as.Date("2028-12-31")
frameOd <- as.Date("2026-01-01")
frameDo <- as.Date("2029-12-31")
framePer <- seq(from = frameOd, to = frameDo, by = "month")
factor <- c("acq1", "acq2", "acq3", "acq4")

delPer <- as.POSIXct(seq(from = delOd, to = delDo, by = "month") %>% head(-1)) # head = maze posledni element (1.1.2027)
print(delPer)

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

# ---------------------------------------------------------------------------- CREATE :: data_vstup - OK


low_spot <- 24.30
aktual_spot <- low_spot + 0.4
surcharge <- 0.05 
bsd <- 1.2

join <- frame %>%
  
  # cena komodity
  
  group_by(year) %>%
  mutate(yRatio = PFC/mean(PFC)) %>% # kdyz neni cely rok, hodi pres prumer NA
  ungroup() %>% group_by(year, quater) %>%
  
  mutate(celyQ = if_else(all(!is.na(PFC)) & is.na(yRatio), "ANO", "NE"),
         # avg = mean(PFC),
         qRatio = ifelse(celyQ == "ANO", PFC/mean(PFC), NA),
         PFCratio = coalesce(yRatio, qRatio)) %>% ungroup() %>%
  
  # select(-yRatio, -qRatio) %>%
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
         PFCprepoc = ifelse(!is.na(PFCratio), otcPrice*PFCratio, otcPrice),
         
         # kurz EUR
         
         swapPoint = (FX-low_spot)*1000,
         FXrecalc0 = aktual_spot+swapPoint/1000, # +surcharge (ale v excelu je 0)
         FXrecalc = FXrecalc0+surcharge,
         
         cenaEUR = profilMWh*PFCprepoc,
         vazenaCena = profilMWh*FXrecalc,
         # product = paste(month, quater, year))
         product = case_when(year(tms_now) != year ~ paste0("Cal", str_remove(year, "^..")),
                             TRUE ~ paste0(month, "/", year)))

data_vstup <- join %>%
  select(year, month, dodavka, facq, profilMWh, product, otcPrice, PFCprepoc, cenaEUR, FXrecalc, vazenaCena) %>%
  filter(dodavka == 1) # final df to match table on sheet Kalkulace

conditionPROF <- any(is.na(data_vstup$profilMWh))
if (conditionPROF) {
  stop('Neuplny profil')
}

acq_map <- c(
  acq1 = acq1,
  acq2 = acq2,
  acq3 = acq3,
  acq4 = acq4
)

df_acq <- data_vstup %>% 
  group_by(facq) %>% 
  summarise(acq = round(sum(profilMWh), 0)) %>% 
  ungroup() %>% 
  mutate(inp = acq_map[match(as.character(facq), names(acq_map))], 
         check = acq == inp)

# test
conditionACQ <- !is.na(acq1))  
if (conditionACQ) {
  stop('Zadane ACQ nesouhlasi se souctem v profilu')
}


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


# ---------------------------------------------------------------------------- CREATE :: marze - IP


marzeMin <- 1.2
marzeDop <- 6
txt_marzeMin <- sprintf("%.2f", marzeMin) # text, zobrazuje cislo s presne 2 decimals
txt_marzeDop <- sprintf("%.2f", marzeDop)


# ---------------------------------------------------------------------------- RETURN :: fix_cena - OK

conditionNPF <- naklad_profil<0
if (conditionNPF) {
  print('Zaporny naklad na profil')
}

fix_cena2 <- data.frame(
  Parametr = c(
    "Období dodávky",
    "ACQ [MWh]",
    "Předávací cena pro obchod [€]",
    "Předávací cena pro obchod [CZK]",
    "Suma profil",
    "Prumer EUR",
    "Suma ceny EUR",
    "Suma vazene ceny",
    "Nakup",
    "Prodej",
    "Kurz",
    "Naklad na profil"
   
  ),
  
  Hodnota = c(
    paste0(delOd, " až ", delDo),
    paste(
      paste(acq$year, acq$rocni_odber, sep = ": "),
      collapse = ", "),
    round(fin_cenaEUR, 2),
    round(fin_cenaCZK, 2),
    round(suma_profil, 0),
    round(mean_PFC, 3),
    round(suma_cenaEUR, 0),
    round(suma_vazenaCena, 0),
    round(nakup, 3),
    round(prodej_czk, 3),
    round(kurz, 2),
    naklad_profil
  )
)

view(fix_cena2)


# ---------------------------------------------------------------------------- RETURN :: plot


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
