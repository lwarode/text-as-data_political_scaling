library(tidyverse)
library(quanteda)
library(stm)
library(tidymodels)

load("C:/Users/laura/OneDrive/Documenti/LAURA/HERTIE 22-23/Text_as_Data_class/text-as-data_political_scaling/data_processed/dfm_raw.rda")

### small preprocessing

stopwords1 <- c("dass", "0", "müssen", "herr", "ja", "kollegen", "herren", "mehr", "menschen", "damen", "dank", "vielen", "schon", "kolleginnen","präsident")

dfm_1 <- dfm_raw %>% dfm_remove(stopwords1) 

####################################### group by batches of 10 sessions

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


dfm_1$session_group <- map_chr(
  dfm_1$session, ~ group_session(.x))

###################################### topic modeling per session group

topwords_list <- list()
models <- list()
  
run_model <- function (dfm, session_nr){
  
  dfm_sub <- dfm_1 %>% dfm_subset(session_group == session_nr)
  print(session_nr)

  model <- stm(dfm_sub, K = 10, max.em.its = 75, init.type = "Spectral", verbose = FALSE) # run STM model

  labels <- labelTopics(model, n=10)
  
  # only keep FREX weighting
  topwords <- data.frame("features" = t(labels$frex))
  
  # create list with all the models named to eventually plot the one we choose
  # models_list <- append(model)
  # topwords_list <- append(topwords)
  
  return(topwords)
} 

model1 <- run_model(dfm_1, "1")
model2 <- run_model(dfm_1, "2")
model3 <- run_model(dfm_1, "3")
model4 <- run_model(dfm_1, "4")
model5 <- run_model(dfm_1, "5")
model6 <- run_model(dfm_1, "6")
model7 <- run_model(dfm_1, "7")
model8 <- run_model(dfm_1, "8")
model9 <- run_model(dfm_1, "9")
model10 <- run_model(dfm_1, "10")
model11 <- run_model(dfm_1, "11")
model12 <- run_model(dfm_1, "12")
model13 <- run_model(dfm_1, "13")
model14 <- run_model(dfm_1, "14")
model15 <- run_model(dfm_1, "15")
model16 <- run_model(dfm_1, "16")
model17 <- run_model(dfm_1, "17") 
model18 <- run_model(dfm_1, "18")
model19 <- run_model(dfm_1, "19")
model20 <- run_model(dfm_1, "20")
model21 <- run_model(dfm_1, "21")
model22 <- run_model(dfm_1, "22")
model23 <- run_model(dfm_1, "23")
model0 <- run_model(dfm_1, "0")


if (!dir.exists("models")) {
  dir.create("models")
  save(model0, file = "models/model0.rda")
} else if (!file.exists("models/model0.rda")) {
  save(model0, file = "models/model0.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model1, file = "models/model1.rda")
} else if (!file.exists("models/model1.rda")) {
  save(model1, file = "models/model1.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model2, file = "models/model2.rda")
} else if (!file.exists("models/model2.rda")) {
  save(model2, file = "models/model2.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model3, file = "models/model3.rda")
} else if (!file.exists("models/model3.rda")) {
  save(model3, file = "models/model3.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model4, file = "models/model4.rda")
} else if (!file.exists("models/model4.rda")) {
  save(model4, file = "models/model4.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model5, file = "models/model5.rda")
} else if (!file.exists("models/model5.rda")) {
  save(model5, file = "models/model5.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model6, file = "models/model6.rda")
} else if (!file.exists("models/model6.rda")) {
  save(model6, file = "models/model6.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model7, file = "models/model7.rda")
} else if (!file.exists("models/model7.rda")) {
  save(model7, file = "models/model7.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model8, file = "models/model8.rda")
} else if (!file.exists("models/model8.rda")) {
  save(model8, file = "models/model8.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model9, file = "models/model9.rda")
} else if (!file.exists("models/model9.rda")) {
  save(model9, file = "models/model9.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model10, file = "models/model10.rda")
} else if (!file.exists("models/model10.rda")) {
  save(model10, file = "models/model10.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model11, file = "models/model11.rda")
} else if (!file.exists("models/model11.rda")) {
  save(model11, file = "models/model11.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model12, file = "models/model12.rda")
} else if (!file.exists("models/model12.rda")) {
  save(model12, file = "models/model12.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model13, file = "models/model13.rda")
} else if (!file.exists("models/model13.rda")) {
  save(model13, file = "models/model13.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model14, file = "models/model14.rda")
} else if (!file.exists("models/model14.rda")) {
  save(model14, file = "models/model14.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model15, file = "models/model15.rda")
} else if (!file.exists("models/model15.rda")) {
  save(model15, file = "models/model15.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model16, file = "models/model16.rda")
} else if (!file.exists("models/model16.rda")) {
  save(model16, file = "models/model16.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model17, file = "models/model17.rda")
} else if (!file.exists("models/model17.rda")) {
  save(model17, file = "models/model17.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model18, file = "models/model18.rda")
} else if (!file.exists("models/model18.rda")) {
  save(model18, file = "models/model18.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model19, file = "models/model19.rda")
} else if (!file.exists("models/model19.rda")) {
  save(model19, file = "models/model19.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model20, file = "models/model20.rda")
} else if (!file.exists("models/model20.rda")) {
  save(model20, file = "models/model20.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model21, file = "models/model21.rda")
} else if (!file.exists("models/model21.rda")) {
  save(model21, file = "models/model21.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model22, file = "models/model22.rda")
} else if (!file.exists("models/model22.rda")) {
  save(model22, file = "models/model22.rda")
}

if (!dir.exists("models")) {
  dir.create("models")
  save(model23, file = "models/model23.rda")
} else if (!file.exists("models/model23.rda")) {
  save(model23, file = "models/model23.rda")
}

######################### choose topic 

# plot topic with top words by FREX weighting 

# find out which documents belong more to the selected topic 

model11 %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")



# probability of a topic to belong to a specific document
theta <- make.dt(model11)
theta[1,1:10]

# select documents in a specific dfm 

###################### wordscores for the new dfm


