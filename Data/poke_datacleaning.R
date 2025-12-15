###Pokedex####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#load libraries#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tigris)
library(sf)
library(dplyr)
library(tidyverse)
library(tidytuesdayR)
library(readr)
library(readxl)
options(tigris_use_cache = TRUE)

#load data#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tuesdata <- tidytuesdayR::tt_load('2025-04-01')
co_counties <- counties(state = "CO", cb = TRUE, class = "sf")
pokemon_df <- tuesdata$pokemon_df

pokemon_ecotones_df <- read_xlsx("pokemon_ecotones.xlsx")

#cleaning data#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pokemon_clean_df <- pokemon_df %>% 
  left_join(pokemon_ecotones_df, by = "pokemon")

co_counties <- co_counties %>%  
  dplyr::select(GEOID, NAME)

#adding ecotones to Co counties#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

county_ecotones <- tibble::tribble(
  ~NAME,        ~ecotone,
  "Adams",      "Urban",
  "Alamosa",    "Sand Dunes",
  "Arapahoe",   "Urban",
  "Archuleta",  "Forest",
  "Baca",       "Agriculture",
  "Bent",       "Agriculture",
  "Boulder",    "Urban",
  "Broomfield", "Urban",
  "Chaffee",    "Mountain",
  "Cheyenne",   "Grassland",
  "Clear Creek","Mountain",
  "Conejos",    "Riparian",
  "Costilla",   "Grassland",
  "Crowley",    "Agriculture",
  "Custer",     "Forest",
  "Delta",      "Steppe",
  "Denver",     "Urban",
  "Dolores",    "Steppe",
  "Douglas",    "Urban",
  "Eagle",      "Mountain",
  "El Paso",    "Urban",
  "Elbert",     "Grassland",
  "Fremont",    "Mountain",
  "Garfield",   "Steppe",
  "Gilpin",     "Mountain",
  "Grand",      "Water",
  "Gunnison",   "Water",
  "Hinsdale",   "Mountain",
  "Huerfano",   "Forest",
  "Jackson",    "Riparian",
  "Jefferson",  "Urban",
  "Kiowa",      "Agriculture",
  "Kit Carson", "Agriculture",
  "La Plata",   "Forest",
  "Lake",       "Mountain",
  "Larimer",    "Water",
  "Las Animas", "Grassland",
  "Lincoln",    "Agriculture",
  "Logan",      "Agriculture",
  "Mesa",       "Water",
  "Mineral",    "Mountain",
  "Moffat",     "Steppe",
  "Montezuma",  "Steppe",
  "Montrose",   "Steppe",
  "Morgan",     "Agriculture",
  "Otero",      "Agriculture",
  "Ouray",      "Mountain",
  "Park",       "Mountain",
  "Phillips",   "Agriculture",
  "Pitkin",     "Mountain",
  "Prowers",    "Agriculture",
  "Pueblo",     "Water",
  "Rio Blanco", "Steppe",
  "Rio Grande", "Riparian",
  "Routt",      "Mountain",
  "Saguache",   "Sand Dunes",
  "San Juan",   "Mountain",
  "San Miguel", "Mountain",
  "Sedgwick",   "Agriculture",
  "Summit",     "Water",
  "Teller",     "Mountain",
  "Washington", "Agriculture",
  "Weld",       "Grassland",
  "Yuma",       "Agriculture")

co_counties_ecotones <- co_counties %>%
  dplyr::left_join(county_ecotones, by = "NAME")

#clean data frames#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

co_counties_ecotones
pokemon_clean_df

#summarising pokemon by ecotone#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pokemon_by_ecotone <- pokemon_clean_df %>%
  group_by(ecotone) %>%
  summarise(
    pokemon_list = paste(pokemon, collapse = ", "),
    n_pokemon    = n(),
    .groups = "drop")

#joining counties to pokemon_by_ecotone#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

co_counties_pokemon <- co_counties_ecotones %>%
  left_join(pokemon_by_ecotone, by = "ecotone") %>%
  st_transform(4326)

#saving dfs for pokemap flexboard#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save(co_counties_ecotones, pokemon_clean_df, file = "poke_data.RData")















