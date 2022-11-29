library(tidyverse)
library(partycoloR) # devtools::install_github("lwarode/partycoloR")

wikipedia_de <- read_csv("https://raw.githubusercontent.com/hdigital/partyfactsdata/master/import/wikipedia/wikipedia.csv") %>% 
    filter(country == "DEU")

wikipedia_de_url_list <- as.list(wikipedia_de$url)

party_de_color <- partycoloR::wikipedia_party_color(wikipedia_de_url_list)

party_colors <- wikipedia_de %>%
    left_join(party_de_color, by = "url") %>%
    filter(!is.na(color_1)) %>%
    # switching first color
    mutate(
        color_1 = case_when(
            name_short %in% c("CDU", "DIE/LINKE") ~ color_2,
            name_short == "NPD" ~ color_4,
            TRUE ~ color_1
        ),
        name_short = case_when(
            name_short == "GRÜNE" ~ "Grunen",
            name_short == "DIE LINKE" ~ "LINKE",
            name_short == "PIRATEN" ~ "Piraten",
            TRUE ~ name_short
        )
    ) %>% 
    select(color_1, name_short) %>% 
    right_join(ches_all_de_scores, by = c("name_short" = "party")) %>% 
    rename(party = name_short) %>% 
    mutate(
        color_1 = case_when(
            party %in% c("Linkspartei/PDS", "LINKE", "PDS") ~ "#BE3075",
            party == "GRUNEN" ~ "#64A12D",
            TRUE ~ color_1
        )
    )



