library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(ggrepel)
library(gganimate)
library(magrittr)

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

ches_scores_2 <- ches_scores %>% 
    mutate(
        party_new = case_when(
            party == "LINKE" ~ "DIE LINKE.",
            party == "GRUNEN" ~ "Grüne",
            TRUE ~ as.character(party)
        )
    )

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

# ?bind_rows

# ches_all_de_scores %>% bind_rows()

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

                  
# ggplot(mtcars, aes(factor(cyl), mpg)) + 
#     geom_boxplot() + 
#     # Here comes the gganimate code
#     transition_states(
#         gear,
#         transition_length = 2,
#         state_length = 1
#     ) +
#     enter_fade() + 
#     exit_shrink() +
#     ease_aes('sine-in-out')


# visualise ches 2 dimensions ---------------------------------------------


party_lr_movement <- ches_all_de_color %>% 
    filter(!party %in% c("CDU", "CSU")) %>% 
    filter(party %in% names(party_colors_vec)) %>% 
    mutate(year = as.integer(year)) %>% 
    ggplot(aes(x = lrecon, y = galtan, color = party)) +
    geom_point() +
    geom_text(aes(label = paste(party, year, " ")), family = "Corbel", show.legend = F, nudge_y = -0.25) +
    theme_minimal() +
    theme(text = element_text(family = "Corbel"), legend.position = "top") +
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

anim_save(
    "figures/party_lr_movement.gif",
    animation = party_lr_movement
)

anim_save(
    "figures/party_lr_movement_high_res.gif",
    animation = party_lr_movement,
    res = 180,
    width = 1000,
    height = 1200
)

party_lr <- ches_all_de_color %>% 
    filter(!party %in% c("CDU", "CSU")) %>% 
    filter(party %in% names(party_colors_vec)) %>% 
    mutate(year = as.integer(year)) %>% 
    ggplot(aes(x = lrecon, y = galtan, color = party)) +
    geom_point() +
    geom_text_repel(aes(label = paste(party, year, " ")), family = "Corbel", show.legend = F, nudge_y = -0.25) +
    theme_minimal() +
    theme(text = element_text(family = "Corbel"), legend.position = "top") +
    geom_vline(xintercept = 5, alpha = 0.25) +
    geom_hline(yintercept = 5, alpha = 0.25) +
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_color_manual(values = party_colors_vec) +
    labs(
        title = 'Ideological Position of German Parties, 1999-2019',
        x = "Left-Right Economical",
        y = "GAL-TAN",
        color = "Party (Faction)"
    ) 

party_lr

ggsave(
    "figures/party_lr.png",
    party_lr,
    width = 7,
    height = 7 * (12/10),
    dpi = 300
)



# replicate ches movement with wordscores but just most recent electoral term --------

# group session in steps of 10
group_session <- function(session) {
    session <- session * 10
    
    if (nchar(session) == 2) {
        return(str_sub(session, 2, 2))
    } 
    if (nchar(session) == 3) {
        return(str_sub(session, 1, 1))
    }
    if (nchar(session) == 4) {
        return(str_sub(session, 1, 2))
    }
}

## 2 APPROACHES
# APPROACH 1: group by both session and party and 
# APPROACH 2: group by party and apply approach per every session and build function to store predictions --> APPLIED

ches_lrecon_de <- ches_scores %>% pull(lrecon)
ches_galtan_de <- ches_scores %>% pull(galtan)

dfm_raw_2 <- dfm_raw %>% 
    dfm_subset(!party_short %in% c("not found", "Fraktionslos")) 

# dfm_full_session <- dfm_raw_2 %>% 
#     dfm_subset(!session %in% c(19))




    # dfm_subset(session == 1) %>% 
    # dfm_group(party_short) %>% 
    # .$party_short


sessionwise_party_scores <- function(dfm, session_nr, score) {
    dfm_new <- dfm %>% 
        dfm_subset(session == session_nr) %>% 
        dfm_group(party_short)
    
    score <- score[dfm_new$party_short]
    
    dfm_new$score_new <- score
    
    model <- textmodel_wordscores(
        dfm_new, y = dfm_new$score_new, smooth = 1
    )
    
    pred_model <- predict(
        model, se.fit = TRUE
    )
    
    # df <- tibble(
    #     party = names(pred_model[["fit"]]),
    #     score = pred_model[["fit"]]
    # )
    # 
    # return(df)
    
    print(paste0("Done: ", as.character(session_nr)))
    
    output_list <- list(
        party = names(pred_model[["fit"]]),
        score = pred_model[["fit"]],
        se_score = pred_model[["se.fit"]]
    )

    return(output_list)
}

dfm_full_session <- dfm_raw_2 %>% 
    dfm_subset(session != 19)
    # dfm_subset(!session %in% c(19))

# dfm_full_session$session %>% unique %>% sort

# lrecon wordscores
ches_scores %>% pull(party) %>% unique
ches_lrecon <- ches_scores %>% pull(lrecon)
names(ches_lrecon) <- ches_scores_2 %>% pull(party_new)
ches_lrecon

sessionwise_party_scores(dfm_raw_2, 20, ches_lrecon)

models_list_lrecon <- map(
    (dfm_full_session$session %>% unique %>% sort),
    ~ sessionwise_party_scores(
        dfm_raw_2, .x, ches_lrecon
    )
)

models_df_lrecon <- models_list_lrecon %>% 
    bind_rows(.id = "session_nr") %>% 
    rename(lrecon_wordscore = score,
           se_lrecon_wordscore = se_score)


# galtan wordscores
ches_scores %>% pull(party) %>% unique
ches_galtan <- ches_scores %>% pull(galtan)
names(ches_galtan) <- ches_scores_2 %>% pull(party_new)
ches_galtan

models_list_galtan <- map(
    (dfm_full_session$session %>% unique %>% sort),
    ~ sessionwise_party_scores(
        dfm_raw_2, .x, ches_galtan
    )
)

models_df_galtan <- models_list_galtan %>% 
    bind_rows(.id = "session_nr") %>% 
    rename(galtan_wordscore = score,
           se_galtan_wordscore = se_score)



# visualise 2 dimensional wordscores --------------------------------------
models_df_both <- models_df_lrecon %>% 
    left_join(models_df_galtan)

# normalize_fun <- function() {
#     
# }

party_lr_movement_wordscores <- models_df_both %>% 
    mutate(
        party = case_when(
            party == "Grüne" ~ "GRUNEN",
            party == "DIE LINKE." ~ "LINKE",
            TRUE ~ party
        )
    ) %>% 
    # filter(!party %in% c("CDU", "CSU")) %>% 
    filter(party %in% names(party_colors_vec)) %>% 
    mutate(session_nr = as.integer(session_nr)) %>% 
    ggplot(aes(x = lrecon_wordscore, y = galtan_wordscore, color = party)) +
    geom_point() +
    # geom_text(aes(label = paste(party, session_nr, " ")), family = "Corbel", show.legend = F, nudge_y = -0.0025) +
    theme_minimal() +
    theme(text = element_text(family = "Corbel"), legend.position = "top") +
    # geom_vline(xintercept = 5, alpha = 0.25) +
    # geom_hline(yintercept = 5, alpha = 0.25) +
    # scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    # scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_color_manual(values = party_colors_vec) +
    labs(
        title = 'Session (LP 19): {frame_time}',
        x = "Left-Right Economical (Wordscore Estimate)",
        y = "GAL-TAN (Wordscore Estimate)",
        color = "Party (Faction)"
    ) +
    transition_time(session_nr) +
    ease_aes('linear')

party_lr_movement_wordscores

anim_save(
    "figures/party_lr_movement_wordscores.gif",
    animation = party_lr_movement_wordscores
)

anim_save(
    "figures/party_lr_movement_wordscores_high_res.gif",
    animation = party_lr_movement_wordscores,
    res = 180,
    width = 1000,
    height = 1200
)

party_lr_wordscores <- models_df_both %>% 
    mutate(
        party = case_when(
            party == "Grüne" ~ "GRUNEN",
            party == "DIE LINKE." ~ "LINKE",
            TRUE ~ party
        )
    ) %>% 
    # filter(!party %in% c("CDU", "CSU")) %>% 
    filter(party %in% names(party_colors_vec)) %>% 
    mutate(session_nr = as.integer(session_nr)) %>% 
    ggplot(aes(x = lrecon_wordscore, y = galtan_wordscore, color = party)) +
    geom_point() +
    # geom_text(aes(label = paste(party, session_nr, " ")), family = "Corbel", show.legend = F, nudge_y = -0.0025) +
    theme_minimal() +
    theme(text = element_text(family = "Corbel"), legend.position = "top") +
    # geom_vline(xintercept = 5, alpha = 0.25) +
    # geom_hline(yintercept = 5, alpha = 0.25) +
    # scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    # scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_color_manual(values = party_colors_vec) +
    labs(
        title = 'Wordscore Estimates for LP19',
        x = "Left-Right Economical (Wordscore Estimate)",
        y = "GAL-TAN (Wordscore Estimate)",
        color = "Party (Faction)"
    )

ggsave(
    "figures/party_lr_wordscores.png",
    party_lr_wordscores,
    width = 8,
    height = 7,
    dpi = 300
)




# add standard errors to plot ---------------------------------------------


party_lr_movement_wordscores_se <- models_df_both %>% 
    mutate(
        party = case_when(
            party == "Grüne" ~ "GRUNEN",
            party == "DIE LINKE." ~ "LINKE",
            TRUE ~ party
        )
    ) %>% 
    # filter(!party %in% c("CDU", "CSU")) %>% 
    filter(party %in% names(party_colors_vec)) %>% 
    mutate(session_nr = as.integer(session_nr)) %>% 
    ggplot(aes(x = lrecon_wordscore, y = galtan_wordscore, color = party)) +
    # geom_point() +
    geom_pointrange(
        aes(
            ymin = galtan_wordscore - (se_galtan_wordscore * 1.96),
            ymax = galtan_wordscore + (se_galtan_wordscore * 1.96)
        ),
        # alpha = 0.25
    ) +
    geom_linerange(
        aes(
            xmin = lrecon_wordscore - (se_lrecon_wordscore * 1.96),
            xmax = lrecon_wordscore + (se_lrecon_wordscore * 1.96)
        ),
        # alpha = 0.25
    ) +
    # ggpointdensity::geom_pointdensity() +
    # geom_hex(aes(fill = party))+
    # geom_text(aes(label = paste(party, session_nr, " ")), family = "Corbel", show.legend = F, nudge_y = -0.0025) +
    theme_minimal() +
    theme(text = element_text(family = "Corbel"), legend.position = "top") +
    # geom_vline(xintercept = 5, alpha = 0.25) +
    # geom_hline(yintercept = 5, alpha = 0.25) +
    # scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    # scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_color_manual(values = party_colors_vec) +
    labs(
        title = 'Session (LP 19): {frame_time}',
        x = "Left-Right Economical (Wordscore Estimate)",
        y = "GAL-TAN (Wordscore Estimate)",
        color = "Party (Faction)"
    ) +
    transition_time(session_nr) +
    ease_aes('linear')

party_lr_movement_wordscores_se

anim_save(
    "figures/party_lr_movement_wordscores_se.gif",
    animation = party_lr_movement_wordscores_se,
    fps = 4
)

anim_save(
    "figures/party_lr_movement_wordscores_se_high_res.gif",
    animation = party_lr_movement_wordscores_se,
    res = 180,
    width = 1000,
    height = 1200,
    fps = 4
)

party_lr_wordscores_se <- models_df_both %>% 
    mutate(
        party = case_when(
            party == "Grüne" ~ "GRUNEN",
            party == "DIE LINKE." ~ "LINKE",
            TRUE ~ party
        )
    ) %>% 
    # filter(!party %in% c("CDU", "CSU")) %>% 
    filter(party %in% names(party_colors_vec)) %>% 
    mutate(session_nr = as.integer(session_nr)) %>% 
    ggplot(aes(x = lrecon_wordscore, y = galtan_wordscore, color = party)) +
    # geom_point() +
    geom_pointrange(
        aes(
            ymin = galtan_wordscore - (se_galtan_wordscore * 1.96),
            ymax = galtan_wordscore + (se_galtan_wordscore * 1.96)
        ),
        alpha = 0.25
    ) +
    geom_linerange(
        aes(
            xmin = lrecon_wordscore - (se_lrecon_wordscore * 1.96),
            xmax = lrecon_wordscore + (se_lrecon_wordscore * 1.96)
        ),
        alpha = 0.25
    ) +
    # ggpointdensity::geom_pointdensity() +
    # geom_hex(aes(fill = party))+
    # geom_text(aes(label = paste(party, session_nr, " ")), family = "Corbel", show.legend = F, nudge_y = -0.0025) +
    theme_minimal() +
    theme(text = element_text(family = "Corbel"), legend.position = "top") +
    # geom_vline(xintercept = 5, alpha = 0.25) +
    # geom_hline(yintercept = 5, alpha = 0.25) +
    # scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    # scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_color_manual(values = party_colors_vec) +
    # scale_fill_manual(values = party_colors_vec) +
    labs(
        title = 'Wordscore Estimates for LP19',
        x = "Left-Right Economical (Wordscore Estimate)",
        y = "GAL-TAN (Wordscore Estimate)",
        color = "Party (Faction)"
    )

# ?geom_point
party_lr_wordscores_se

ggsave(
    "figures/party_lr_wordscores_se.png",
    party_lr_wordscores_se,
    width = 8,
    height = 7,
    dpi = 300
)

# party_session_scores_lrecon <- list()
# 
# for (i in 1:238) {
#     party_session_scores[[i]] <- sessionwise_party_scores(
#         dfm_full_session, i, ches_scores %>% pull(lrecon)
#     )
# }
# 
# sessionwise_party_scores(dfm_raw_2, 1, ches_lrecon_de)
# 
# ches_scores %>% pull(party) %>% unique
# ches_lrecon <- ches_scores %>% pull(lrecon)
# names(ches_lrecon) <- ches_scores_2 %>% pull(party_new)
# ches_lrecon
# ches_lrecon[dfm_party$party_short]
# 
# 
# sessionwise_party_scores(dfm_raw_2, 230, ches_lrecon)
# 
# 
# 
# sessionwise_party_scores(dfm_raw_2, 17, ches_lrecon_de)
# sessionwise_party_scores(dfm_raw_2, 18, ches_lrecon_de)
# sessionwise_party_scores(dfm_raw_2, 19, ches_lrecon_de)
# dfm_raw_2 %>% 
#     dfm_subset(session == 19) %>% 
#     .$party_short
# 
# 
# sessionwise_party_scores(dfm_raw_2, 1, ches_scores %>% pull(galtan))
# sessionwise_party_scores(dfm_raw_2, 1, ches_scores %>% pull(lrecon))
# 
# sessionwise_party_scores(dfm_raw_2, 2, ches_scores %>% pull(galtan))
# sessionwise_party_scores(dfm_raw_2, 2, ches_scores %>% pull(lrecon))
# 
# sessionwise_party_scores(dfm_raw_2, session_nr = 20, ches_scores %>% pull(lrecon))
# sessionwise_party_scores(dfm_raw_2, 200, ches_scores %>% pull(lrecon))
# 
# names(pred_model_galtan[["fit"]])

# dfm_session_party <- dfm_raw %>% 
#     dfm_group(groups = interaction(party_short, session)) %>% 
#     dfm_subset(!party_short %in% c("not found", "Fraktionslos")) %>% 
#     dfm_trim(min_termfreq = 25)
# 
# ches_scores %>% pull(galtan)
# ches_scores %>% pull(lrecon)
# 
# 
# ches_scores %>% pull(lrgen)
# 
# # ndoc(dfm_session_party$party_short)
# 
# dfm_party$party_short
# dfm_session_party
# # dfm_subset(
# #     dfm_session_party, 
# #     !party_short %in% c("not found", "Fraktionslos")) %>% 
# #     dfm_sort() %>% 
# #     docvars(party_short)
# 
# 
# 
# dfm_agg_session_party <- dfm_raw 
# dfm_agg_session_party$lrgen <- ches_scores %>% pull(lrgen)
# dfm_party$galtan <- ches_scores %>% pull(galtan)
# dfm_party$lrecon <- ches_scores %>% pull(lrecon)
# dfm_party$eu_position <- ches_scores %>% pull(eu_position)
# dfm_agg_session_party$session_group <- map_chr(
#     dfm_agg_session_party$session, ~ group_session(.x)
# )
# dfm_agg_session_party <- dfm_agg_session_party %>% 
#     dfm_group(groups = interaction(session_group, party_short)) %>% 
#     dfm_trim(min_termfreq = 25)
# 
# # add scale scores for wordscores approach
# dfm_agg_session_party$lrgen <- ches_scores %>% pull(lrgen)
# dfm_party$galtan <- ches_scores %>% pull(galtan)
# dfm_party$lrecon <- ches_scores %>% pull(lrecon)
# dfm_party$eu_position <- ches_scores %>% pull(eu_position)
# 
# # model GAL-TAN
# model_galtan_dfm_agg_session_party <- textmodel_wordscores(
#     dfm_agg_session_party, 
#     y = dfm_agg_session_party$galtan,
#     smooth = 1
# )
# 
# pred_model_galtan_dfm_agg_session_party <- predict(
#     model_galtan_dfm_agg_session_party,
#     
# )
# 
# 
# # galtan
# model_galtan <- textmodel_wordscores(dfm_party, y = dfm_party$galtan, smooth = 1)
# pred_model_galtan <- predict(model_galtan, se.fit = TRUE)
# pred_model_galtan
# # textplot_scale1d(pred_model_galtan) 
# 
# # lrecon
# model_lrecon <- textmodel_wordscores(dfm_party, y = dfm_party$lrecon, smooth = 1)
# pred_model_lrecon <- predict(model_lrecon, se.fit = TRUE, newdata = dfm_party)
# # textplot_scale1d(pred_model_lrecon) 
# 
# dfm_raw$session %>% unique() %>% length()
# dfm_raw$session %>% unique() %>% class()
# 
# 
# 
# 
# "42" %>% length()
# "42" %>% nchar()
# group_session(session = 2)
# 
# 
# "1110" %>% group_session()
# 
# "30" %>% group_session()
# "110" %>% group_session()
# 
# "110" %>% group_session()
# dfm_raw$session %>% unique() %>% multiply_by(10) %>% 
#     group_session()
# 
# dfm_raw$session %>% unique() 
# dfm_raw$session %>% unique() %>% multiply_by(10)
# dfm_raw$session %>% unique() %>% multiply_by(10) %>% as.character() %>% 
#     map_chr(~ group_session(.)) %>% 
#     as.numeric()
# dfm_raw$session %>% unique() %>% multiply_by(10) %>% as.character() %>% str_sub(1, 1)
# 
# 
# 
# dfm_raw$session %>% unique() %>% cut_width(width = length(239/10))




# highlight deviation with geom_segment
# annimate movement of model score deviations with gganimate (with or without geom_segment)

library(tidyverse)


# get data ----------------------------------------------------------------


