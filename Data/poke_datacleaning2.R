library(tidyverse)
library(tidytuesdayR)
library(readxl)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

# 1. Load data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tuesdata <- tidytuesdayR::tt_load("2025-04-01")
pokemon_raw <- tuesdata$pokemon_df

pokemon_evolution   <- readxl::read_excel("pokemon_evolution.xlsx")
pokemon_ecotones_df <- readxl::read_excel("pokemon_ecotones.xlsx")

co_counties <- counties(state = "CO", cb = TRUE, class = "sf") %>%
  dplyr::select(GEOID, NAME)

# 2. Filtering evolution list~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Make capitalization consistent
pokemon_evo <- pokemon_raw %>%
  mutate(pokemon = stringr::str_to_title(pokemon))


pokemon_ecotones_df <- pokemon_ecotones_df %>%
  mutate(pokemon = stringr::str_to_title(pokemon))

# Combining all evolutions
evo_names <- pokemon_evolution %>%
  select(unevolved, evolution_one, evolution_two) %>%
  unlist(use.names = FALSE) %>%
  unique()

# Filtering to pokemon in pokemon_evo df
pokemon_evo <- pokemon_evo %>%
  filter(pokemon %in% evo_names)

# Evolution stage reference
evo_long <- pokemon_evolution %>%
  mutate(Base_pokemon = unevolved) %>%
  pivot_longer(
    cols      = c(unevolved, evolution_one, evolution_two),
    names_to  = "Stage",
    values_to = "Pokemon"
  ) %>%
  mutate(Evolution_stage = dplyr::case_when(
    Stage == "unevolved"     ~ 0L,
    Stage == "evolution_one" ~ 1L,
    Stage == "evolution_two" ~ 2L
  )) %>%
  drop_na() %>%
  select(Pokemon, Base_pokemon, Evolution_stage)

# Attaching evolution info to the pruned list
pokemon_evo <- pokemon_evo %>%
  left_join(evo_long, by = c("pokemon" = "Pokemon")) %>%
  select(pokemon, Evolution_stage, Base_pokemon, dplyr::everything()) %>%
  distinct(pokemon, .keep_all = TRUE)

# 3. Joining ecotones to pruned pokemon list~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pokemon_clean_df <- pokemon_evo %>%
  left_join(pokemon_ecotones_df, by = "pokemon")

# 4. Assigning ecotones to Co counties~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  "Yuma",       "Agriculture"
)

co_counties_ecotones <- co_counties %>%
  left_join(county_ecotones, by = "NAME")

# 5. Summarise pokemon by by ecotone~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pokemon_by_ecotone <- pokemon_clean_df %>%
  group_by(ecotone) %>%
  summarise(
    pokemon_list = paste(pokemon, collapse = ", "),
    n_pokemon    = n(),
    .groups = "drop"
  )

# 6. Joining counties to pokemon_by_ecotone~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

co_counties_pokemon <- co_counties_ecotones %>%
  left_join(pokemon_by_ecotone, by = "ecotone") %>%
  st_transform(4326)

# 7. Build poke_by_county (one row per county–Pokémon combo)~~~~~~~~~~~~~~~~~~~~

co_counties_map <- co_counties_ecotones %>%
  st_transform(4326)

poke_by_county <- co_counties_map %>%
  st_drop_geometry() %>%
  select(NAME, ecotone) %>%
  inner_join(
    pokemon_clean_df,
    by = "ecotone",
    relationship = "many-to-many"
  )

# 8. Saving for use in flexboars~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save(
  co_counties_ecotones,
  pokemon_clean_df,
  poke_by_county,
  file = "poke_data.RData"
)
