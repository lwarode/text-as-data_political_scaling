library(tidyverse)
library(quanteda)
library(stm)

# load data ---------------------------------------------------------------
expected_data_files <- c(
    "electoral_terms.rda",
    "factions.rda",
    "politicians.rda",    
    "speeches_et_19.csv" 
) 

if (all(list.files("data") == expected_data_files)) {
 speeches_et_19 <- read_csv("data/speeches_et_19.csv")
 load("data/factions.rda")
 load("data/politicians.rda")
 load("data/electoral_terms.rda")
}


# CORPUS ------------------------------------------------------------------


# process data ------------------------------------------------------------
corpus_et_19 <- speeches_et_19 %>%
    left_join(factions %>% 
                  mutate(id = as.double(id)), 
              by = c("faction_id" = "id")) %>% 
    mutate(id = id %>% as.character()) %>% 
    rename(party_short = abbreviation) %>% 
    corpus(
        docid_field = "id", 
        unique_docnames = FALSE,
        text_field = "speech_content"
    ) %>% 
    tokens(remove_punct = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords("german"))

# store corpus ------------------------------------------------------------
if (!dir.exists("data_processed")) {
    dir.create("data_processed")
    save(corpus_et_19, file = "data_processed/corpus_et_19.rda")
} else if (!file.exists("data_processed/corpus_et_19.rda")) {
    save(corpus_et_19, file = "data_processed/corpus_et_19.rda")
}



# DFM ---------------------------------------------------------------------

# process data ------------------------------------------------------------
dfm_raw <- corpus_et_19 %>% 
    dfm()

dfm_min_term_5 <- dfm_raw %>% 
    dfm_trim(min_termfreq = 5)

dfm_min_term_10 <- dfm_raw %>% 
    dfm_trim(min_termfreq = 10)

dfm_min_term_25 <- dfm_raw %>% 
    dfm_trim(min_termfreq = 25)

dfm_min_term_50 <- dfm_raw %>% 
    dfm_trim(min_termfreq = 50)

dfm_min_term_100 <- dfm_raw %>% 
    dfm_trim(min_termfreq = 100)

dfm_session <- dfm_raw %>% 
    dfm_group(session)

dfm_party <- dfm_raw %>% 
    dfm_group(party_short)

dfm_session_party_1 <- dfm_raw %>% 
    dfm_group(groups = interaction(session, party_short)) %>% 
    dfm_trim(min_termfreq = 25)

dfm_session_party_2 <- dfm_raw %>% 
    dfm_trim(min_termfreq = 25) %>% 
    dfm_group(groups = interaction(session, party_short)) 

# check whether trimming is affected by dfm_group()
length(dfm_session_party_1) == length(dfm_session_party_2)
    
# store dfm ---------------------------------------------------------------

# dfm no groups
if (!dir.exists("data_processed")) {
    dir.create("data_processed")
    save(dfm_raw, file = "data_processed/dfm_raw.rda")
} else if (!file.exists("data_processed/dfm_raw.rda")) {
    save(dfm_raw, file = "data_processed/dfm_raw.rda")
}

# dfm removed infrequent terms

# dfm grouped per session

# dfm grouped per party

# dfm grouped per session and party



