library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(ggrepel)
library(gganimate)

load("data_processed/dfm_raw.rda")

dfm_party <- dfm_raw %>% 
    dfm_group(party_short) %>% 
    dfm_trim(min_termfreq = 25) %>% 
    dfm_subset(!(party_short %in% c("Fraktionslos", "not found"))) 

# check dimension of party
docvars(dfm_party, "party_short")
dfm_party$party_short

# CHES data: https://www.chesdata.eu/ches-europe
ches_de <- read_csv("data/1999-2019_CHES_dataset_means(v3).csv") %>% 
    filter(country == 3) %>% 
    filter(year == max(year)) %>% 
    select(party, vote, eu_position, lrgen, lrecon, galtan) %>% 
    add_row(party = "CDU/CSU") 
    
cdu_csu_weight <- function(df, scale) {
    cdu_scale <- df %>% 
        filter(party == "CDU") %>% 
        pull({{scale}})
    
    csu_scale <- df %>% 
        filter(party == "CSU") %>% 
        pull({{scale}})
    
    cdu_vs <- df %>% 
        filter(party == "CDU") %>% 
        pull(vote)
    
    csu_vs <- df %>% 
        filter(party == "CSU") %>% 
        pull(vote)
    
    sum_vs <- cdu_vs + csu_vs
    
    weighted_scale <- cdu_scale * (cdu_vs/sum_vs) + csu_scale * (csu_vs/sum_vs)
    
    return(weighted_scale)
}

ches_scores <- ches_de %>% 
    mutate(across(3:last_col(), ~ ifelse(party == "CDU/CSU", cdu_csu_weight(ches_de, .x), .x))) %>% 
    filter(party %in% c("AfD", "CDU/CSU", "LINKE", "FDP", "GRUNEN", "SPD"))  %>% 
    mutate(party = factor(party, levels =  c("AfD", "CDU/CSU", "LINKE", "FDP", "GRUNEN", "SPD"))) %>% 
    arrange(party)

ches_scores

ches_all_de <- read_csv("data/1999-2019_CHES_dataset_means(v3).csv") %>% 
    filter(country == 3) 
    
ches_all_de_scores <- ches_all_de %>%
    select(party, year, vote, eu_position, lrgen, lrecon, galtan) %>%
    group_by(year) %>%
    group_modify( ~ add_row(.x, party = "CDU/CSU")) %>%
    ungroup() %>%
    group_map(~ mutate(.x, across(
        4:last_col(), ~ ifelse(party == "CDU/CSU", cdu_csu_weight(ches_all_de, .x), .x)
    ))) %>%
    bind_rows()

?bind_rows

ches_all_de_scores %>% bind_rows()

cdu_csu_weight(ches_all_de, eu_position)

# add scale scores for wordscores approach
dfm_party$lrgen <- ches_scores %>% pull(lrgen)
dfm_party$galtan <- ches_scores %>% pull(galtan)
dfm_party$lrecon <- ches_scores %>% pull(lrecon)
dfm_party$eu_position <- ches_scores %>% pull(eu_position)

# dfm_party %>% View

# models ------------------------------------------------------------------

# lrgen
model_lrgen <- textmodel_wordscores(dfm_party, y = dfm_party$lrgen, smooth = 1)
pred_model_lrgen <- predict(model_lrgen, se.fit = TRUE, newdata = dfm_party)
textplot_scale1d(pred_model_lrgen)

# galtan
model_galtan <- textmodel_wordscores(dfm_party, y = dfm_party$galtan, smooth = 1)
pred_model_galtan <- predict(model_galtan, se.fit = TRUE, newdata = dfm_party)
textplot_scale1d(pred_model_galtan) 

# lrecon
model_lrecon <- textmodel_wordscores(dfm_party, y = dfm_party$lrecon, smooth = 1)
pred_model_lrecon <- predict(model_lrecon, se.fit = TRUE, newdata = dfm_party)
textplot_scale1d(pred_model_lrecon) 

# eu_position
model_eu_position <- textmodel_wordscores(dfm_party, y = dfm_party$eu_position, smooth = 1)
pred_model_eu_position <- predict(model_eu_position, se.fit = TRUE, newdata = dfm_party)
textplot_scale1d(pred_model_eu_position) 

pred_model_lrgen$fit %>% names

# TO-DO: FOR ALL COEFFICIENT PLOTS: ADD 2ND X AXIS WITH ORIGINAL CHES SCORE RESPECTIVELY AND HIGHLIGHT BIGGEST DIFFERENCES



# visualize deviations ----------------------------------------------------

# approach 2 scale relative approach: both ches and wordscores range
# general concept: 2 axis approach lrecon galtan
# write function that extracts scores and party names per model
# df_pred <- function(pred) {
#     name_pred <- pred %>% substitute() %>% deparse() 
#     
#     col_name <- tibble(
#         party_name = names(pred$fit)
#     )
#     
#     col_pred <- as_tibble_col(
#         pred$fit,
#         column_name = pred %>% 
#             substitute() %>% 
#             deparse() 
#     )
#     
#     col_name %>% 
#         bind_cols(col_pred)
#     
# }

df_pred <- tibble(
    name_party = names(pred_model_lrgen$fit),
    pred_lrgen = pred_model_lrgen$fit,
    pred_lrecon = pred_model_lrecon$fit,
    pred_galtan = pred_model_galtan$fit,
    pred_eu_position = pred_model_eu_position$fit
)

df_pred %>% 
    ggplot(aes(x = pred_lrecon, y = pred_galtan)) + 
    geom_point() + 
    theme_minimal() 

ches_all_de_scores %>% 
    ggplot(aes(x = lrecon, y = galtan, color = party)) +
    geom_point() + 
    geom_text_repel(aes(label = paste(party, year, " ")), family = "Corbel", show.legend = F) +
    theme_minimal() +
    theme(text = element_text(family = "Corbel")) +
    geom_vline(xintercept = 5, alpha = 0.25) +
    geom_hline(yintercept = 5, alpha = 0.25) +
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) 


# using party colors ------------------------------------------------------
source("party_colors.R")
# named character with color codes
ches_all_de_color <- ches_all_de_scores %>% 
    left_join(party_colors) %>% 
    mutate(
        party = case_when(
            party == "Linkspartei/PDS" ~ "LINKE",
            party == "Grunen" ~ "GRUNEN",
            TRUE ~ party
        )
    ) %>% 
    filter(
        party %in% (ches_all_de_scores %>%
                        filter(year == max(year)) %>%
                        pull(party))
    ) %>% 
    distinct(party, year, .keep_all = T)

party_colors_vec <- ches_all_de_color %>%
    distinct(party, color_1) %>% 
    na.omit(color_1) %>%
    pull(color_1)

names(party_colors_vec) <- ches_all_de_color %>%
    distinct(party, color_1) %>% 
    na.omit(color_1) %>%
    pull(party)

party_colors_vec
names(party_colors_vec)

ches_all_de_color %>% 
    filter(!party %in% c("CDU", "CSU")) %>% 
    filter(party %in% names(party_colors_vec)) %>% 
    ggplot(aes(x = lrecon, y = galtan, color = party)) +
    geom_point() + 
    geom_text_repel(aes(label = paste(party, year, " ")), family = "Corbel", show.legend = F) +
    theme_minimal() +
    theme(text = element_text(family = "Corbel")) +
    geom_vline(xintercept = 5, alpha = 0.25) +
    geom_hline(yintercept = 5, alpha = 0.25) +
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_color_manual(values = party_colors_vec) +
    labs(x = "Left-Right Economical", y = "GAL-TAN", color = "Party (Faction)")

library(gganimate)                    
ggplot(mtcars, aes(factor(cyl), mpg)) + 
    geom_boxplot() + 
    # Here comes the gganimate code
    transition_states(
        gear,
        transition_length = 2,
        state_length = 1
    ) +
    enter_fade() + 
    exit_shrink() +
    ease_aes('sine-in-out')

party_lr_movement <- ches_all_de_color %>% 
    filter(!party %in% c("CDU", "CSU")) %>% 
    filter(party %in% names(party_colors_vec)) %>% 
    mutate(year = as.integer(year)) %>% 
    ggplot(aes(x = lrecon, y = galtan, color = party)) +
    geom_point() +
    geom_text(aes(label = paste(party, year, " ")), family = "Corbel", show.legend = F, nudge_y = -0.25) +
    theme_minimal() +
    theme(text = element_text(family = "Corbel")) +
    geom_vline(xintercept = 5, alpha = 0.25) +
    geom_hline(yintercept = 5, alpha = 0.25) +
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_color_manual(values = party_colors_vec) +
    labs(
        title = 'Year: {frame_time}',
        x = "Left-Right Economical",
        y = "GAL-TAN",
        color = "Party (Faction)"
    ) +
    transition_time(year) +
    ease_aes('linear')

party_lr_movement


normalize_fun <- function() {
    
}

# highlight deviation with geom_segment
# annimate movement of model score deviations with gganimate (with or without geom_segment)

library(tidyverse)


# get data ----------------------------------------------------------------


