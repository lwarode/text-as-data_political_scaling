library(RPostgreSQL)
library(tidyverse)

# db_connection -----------------------------------------------------------
db <- "next"
host_db <- "localhost"
db_port <- "5432"
db_user <- "postgres"
db_password <- "postgres"
con <-
    dbConnect(
        RPostgres::Postgres(),
        dbname = db,
        host = host_db,
        port = db_port,
        user = db_user,
        password = db_password
    )


### have docker container running ###

# get data tables ---------------------------------------------------------
speeches <- dbGetQuery(con,
                       "SELECT *
                       FROM open_discourse.speeches;")

factions <- dbGetQuery(con,
                       "SELECT *
                       FROM open_discourse.factions;")

politicians <- dbGetQuery(con,
                          "SELECT *
                       FROM open_discourse.politicians;")

# contributions_simplified <- dbGetQuery(con,
#                                        "SELECT *
#                        FROM open_discourse.contributions_simplified;")
# 
# contributions_extended <- dbGetQuery(con,
#                                      "SELECT *
#                        FROM open_discourse.contributions_extended;")

electoral_terms <- dbGetQuery(con,
                              "SELECT *
                       FROM open_discourse.electoral_terms;")

# store data tables (electoral term 19) for local analysis ------------------------------------
if (!dir.exists("data")) {
    dir.create("data")
    write_csv(
        speeches %>%
            filter(electoral_term == 19),
        file = "data/speeches_et_19.csv"
    )
} else if (!file.exists("data/speeches_et_19.csv")) {
    save(speeches, file = "data/speeches_et_19.csv")
}

if (!dir.exists("data")) {
    dir.create("data")
    save(factions, file = "data/factions.rda")
} else if (!file.exists("data/factions.rda")) {
    save(factions, file = "data/factions.rda")
}

if (!dir.exists("data")) {
    dir.create("data")
    save(politicians, file = "data/politicians.rda")
} else if (!file.exists("data/politicians.rda")) {
    save(politicians, file = "data/politicians.rda")
}

if (!dir.exists("data")) {
    dir.create("data")
    save(electoral_terms, file = "data/electoral_terms.rda")
} else if (!file.exists("data/electoral_terms.rda")) {
    save(electoral_terms, file = "data/electoral_terms.rda")
}

# if (!dir.exists("data")) {
#     dir.create("data")
#     save(contributions_simplified, file = "data/contributions_simplified.rda")
# } else if (!file.exists("data/contributions_simplified.rda")) {
#     save(contributions_simplified, file = "data/contributions_simplified.rda")
# }
# 
# 
# if (!dir.exists("data")) {
#     dir.create("data")
#     save(contributions_extended, file = "data/contributions_extended.rda")
# } else if (!file.exists("data/contributions_extended.rda")) {
#     save(contributions_extended, file = "data/contributions_extended.rda")
# }







