# Option 2: Read data file directly from GitHub

pokemon_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')

# Capitalize pokemon names 
pokemon_df$pokemon <- str_to_title(pokemon_df$pokemon)

#Filter pokemon_df to pokemon_evolution list 
library(dplyr)
library(tidyverse)

# Combine all evolution-related names into one character vector
evo_names <- pokemon_evolution %>%
  select(unevolved, evolution_one, evolution_two) %>%
  unlist(use.names = FALSE) %>%
  unique()

# Filter pokemon_df so pokemon matches any of those names
pokemon_df <- pokemon_df %>%
  filter(pokemon %in% evo_names) 

# Add reference columns to state what the base pokemon and evolution stage are 
evo_long <- pokemon_evolution %>%
  mutate(Base_pokemon = unevolved) %>% 
  pivot_longer(
    cols = c(unevolved, evolution_one, evolution_two),
    names_to = "Stage",
    values_to = "Pokemon") %>% 
mutate(Evolution_stage = case_when(
  Stage == "unevolved" ~ 0L,
  Stage == "evolution_one" ~ 1L,
  Stage == "evolution_two" ~ 2L)) %>%
  drop_na() %>% 
  select(Pokemon, Base_pokemon, Evolution_stage) 

# Merge the evolution reference to pokemon_df
pokemon_df <- pokemon_df %>%
  left_join(evo_long %>% select(Pokemon, Base_pokemon, Evolution_stage),
    by = c("pokemon" = "Pokemon"))

#Re-order columns in pokemon_df
pokemon_df <- pokemon_df %>%
  select(pokemon, Evolution_stage, Base_pokemon, everything())

