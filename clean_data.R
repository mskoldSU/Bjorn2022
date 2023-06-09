library(tidyverse)
library(sf)

map <- st_read("LanSweref99TM/Lan_Sweref99TM_region.shp") %>% 
  filter(LnKod %in% c("01", "03", "17", "18", "19", "20", "21"))

# Alla individ och könsbestämda spillningsprover
rovbase_prover <- readxl::read_excel("data_raw/MSKO12052023085244516.xlsx", guess_max = Inf) %>% 
  filter(`Art (Prøve)` == "Björn", 
         !is.na(Individ),
         lubridate::year(Funnetdato) %in% c(2012, 2017, 2022), lubridate::month(Funnetdato) %in% 8:10, 
         Prøvetype == "Spillning",
         Fylke %in% c("Värmlands län (S)", "Dalarnas län (S)", "Gävleborgs län (S)", 
                      "Uppsala län (S)", "Västmanlands län (S)", "Örebro län (S)", 
                      "Stockholms län (S)")) |> 
  mutate(sample_county = str_remove(Fylke, " \\(S\\)"),
         id = str_sub(Individ, 1, 8),
         year = lubridate::year(Funnetdato),
         week = lubridate::isoweek(Funnetdato),
         sex = Kjønn,
         date = format(Funnetdato, "%Y-%m-%d")) |> 
  group_by(id) |> 
  mutate(sex = case_when(sum(sex == "Hona") > sum(sex == "Hane") ~ "Hona", 
                         sum(sex == "Hane") > sum(sex == "Hona") ~ "Hane", 
                         TRUE ~ "Okänt"), # Majority vote
         north = `Nord (UTM33/SWEREF99 TM)`, 
         east = `Øst (UTM33/SWEREF99 TM)`) |> 
  ungroup() |> 
  filter(sex != "Okänt") |> 
  group_by(id, year) |> 
  mutate(m_north = mean(north), m_east = mean(east)) |> 
  st_as_sf(crs = st_crs(map), coords = c("m_east", "m_north"), remove = FALSE) |> 
  st_join(map |> nngeo::st_remove_holes()) |> 
  filter(!is.na(LnNamn)) |> 
  mutate(survey_county = paste(LnNamn, "län")) |> 
  as_tibble() |> 
  select(id, sex, year, week, date, sample_county, north, east, survey_county, m_north, m_east)


write_csv(rovbase_prover, "data/prover2022.csv")
