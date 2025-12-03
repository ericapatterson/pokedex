# Using R
# Option 1: tidytuesdayR R package 
#install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-04-01')
## OR
tuesdata <- tidytuesdayR::tt_load(2025, week = 13)

pokemon_df <- tuesdata$pokemon_df

# Option 2: Read directly from GitHub

pokemon_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')

