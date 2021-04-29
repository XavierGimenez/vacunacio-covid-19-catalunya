library("dplyr")
library("tidyr")
library("jsonlite")


# ----------------------------
# load Covid-19 data by available coverages

df_covid_area <- read.csv2('26_4_2020_Vacunaci__per_al_COVID-19_per__rea_b_sica_de_salut.csv', sep = ',', header = TRUE, stringsAsFactors = FALSE)
#df_covid_comarca <- read.csv2('Vacunaci__per_al_COVID-19_per_comarca.csv', sep = ',', header = TRUE, stringsAsFactors = FALSE)
#df_covid_municipi <- read.csv2('Vacunaci__per_al_COVID-19_per_municipi.csv', sep = ',', header = TRUE, stringsAsFactors = FALSE)

# quick EDA
#View(unique(df_covid_area$SEXE))
#View(unique(df_covid_comarca$SEXE))
#View(unique(df_covid_municipi$SEXE))

#df_covid_area %>% group_by(SEXE) %>% count()
#Dona           367564
#Home           249119
#No classificat    333
#df_covid_comarca %>% group_by(SEXE) %>% count()
#Dona           76582
#Home           61553
#No classificat   333
#df_covid_municipi %>% group_by(SEXE) %>% count()
#Dona           97213
#Home           78376
#No classificat   333
#df_covid_area %>% group_by(NO_VACUNAT) %>% count()
#""           597269
#2 "No vacunat"  19747
#View(df_covid_area %>% group_by(NO_VACUNAT, DOSI) %>% count())


#so:
df_covid_area <- df_covid_area %>% 
  filter(NO_VACUNAT != 'No vacunat') %>%
  dplyr::group_by(SEXE, EDAT, DOSI) %>%
  dplyr::summarise(n = sum(RECOMPTE))



# ----------------------------
# load overall population data

# data from Statistical Institute of Catalonia (2020 January 1st)
# https://www.idescat.cat/pub/?id=ep&n=9124
df_population <- read.csv2(
    't9124.csv', 
    sep = ';', 
    header = TRUE, 
    stringsAsFactors = TRUE
  )

df_population_covid_age_spans <- df_population %>%
  # make values in column 'Edat' to match same age spans as the Covid-19 datasets
  # levels(df_population$Edat) vs levels(df_covid_area$EDAT)
  dplyr::mutate(
    Edat = case_when(
      Edat == "De 0 a 4 anys" ~ "0 a 14",
      Edat == "De 5 a 9 anys" ~ "0 a 14",
      Edat == "De 10 a 14 anys" ~ "0 a 14",
      Edat == "De 15 a 19 anys" ~ "15 a 19",
      Edat == "De 20 a 24 anys" ~ "20 a 24",
      Edat == "De 25 a 29 anys" ~ "25 a 29",
      Edat == "De 30 a 34 anys" ~ "30 a 34",
      Edat == "De 35 a 39 anys" ~ "35 a 39",
      Edat == "De 40 a 44 anys" ~ "40 a 44",
      Edat == "De 45 a 49 anys" ~ "45 a 49",
      Edat == "De 50 a 54 anys" ~ "50 a 54",
      Edat == "De 55 a 59 anys" ~ "55 a 59",
      Edat == "De 60 a 64 anys" ~ "60 a 64",
      Edat == "De 65 a 69 anys" ~ "65 a 69",
      Edat == "De 70 a 74 anys" ~ "70 a 74",
      Edat == "De 75 a 79 anys" ~ "75 a 79",
      Edat == "De 80 a 84 anys" ~ "80 o més",
      Edat == "De 85 a 89 anys" ~ "80 o més",
      Edat == "De 90 a 94 anys" ~ "80 o més",
      Edat == "De 95 anys i més" ~ "80 o més"
    )
  ) %>%
  dplyr::group_by(Edat) %>%
  dplyr::summarize(
    Homes = sum(Homes),
    Dones = sum(Dones),
    Total = sum(Total)
  ) %>%
  ## tidify by gather 'Homes' and 'Dones' into a single column
  pivot_longer(
    cols = c('Homes', 'Dones'),
    names_to = 'SEXE'
  )




# ...and dump the data into csv and json files

write.csv2(df_covid_area, 'vaccinated.csv')
jsonlite::write_json(df_covid_area, 'vaccinated.json')

write.csv2(df_population_covid_age_spans, 'population.csv')
jsonlite::write_json(df_population_covid_age_spans, 'population.json')
