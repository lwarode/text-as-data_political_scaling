library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)

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
    cdu_scale <- ches_de %>% 
        filter(party == "CDU") %>% 
        pull({{scale}})
    
    csu_scale <- ches_de %>% 
        filter(party == "CSU") %>% 
        pull({{scale}})
    
    cdu_vs <- ches_de %>% 
        filter(party == "CDU") %>% 
        pull(vote)
    
    csu_vs <- ches_de %>% 
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


# add scale scores for wordscores approach
dfm_party$lrgen <- ches_scores %>% pull(lrgen)
dfm_party$galtan <- ches_scores %>% pull(galtan)
dfm_party$lrecon <- ches_scores %>% pull(lrecon)
dfm_party$eu_position <- ches_scores %>% pull(eu_position)

dfm_party %>% View

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



# TO-DO: FOR ALL COEFFICIENT PLOTS: ADD 2ND X AXIS WITH ORIGINAL CHES SCORE RESPECTIVELY AND HIGHLIGHT BIGGEST DIFFERENCES


