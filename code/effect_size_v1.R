library(effectsize)
library(tidyverse)

session_important <- read_csv("data/key_info_1.csv")

#########################################################
#                   accuracy
#########################################################

df_accuracy <- session_important %>% 
  group_by(mode_paper) %>%
  summarise(accuracy_mean = mean(scr30_percent),
            accuracy_sd = sd(scr30_percent)) 

a <- session_important %>% count(mode_paper)

df_accuracy <- df_accuracy %>% 
  mutate(N = a$n) %>% 
  mutate(accuracy_ci95 = 1.96 * (accuracy_sd/ sqrt(N)))

df_accuracy %>% write.csv("data/effect_size_accuracy_CI.csv")

b <- combn(df_accuracy$mode_paper,2,FUN = NULL, simplify = TRUE)
b <- as.character(b)

df_comb <- data.frame("mode1" = NA, "mode2"= NA, "accuracy_pooled_sd"= NA) 

for (i in 1:(length(b)/2)){
  index <- (i-1) * 2 + 1
  df_comb[i, 1] <- b[index]
  df_comb[i, 2] <- b[index + 1]
  
  z <- session_important[which(session_important$mode_paper == b[index] |
                                 session_important$mode_paper == b[index + 1]),]
  z$mode_paper <- factor(as.character(z$mode_paper))
  df_comb[i, 3] = sd_pooled(scr30_percent ~ mode_paper, data = z)
  
}


df_comb <- df_comb %>% 
  left_join(df_accuracy, by = c("mode1" = "mode_paper")) %>%
  left_join(df_accuracy, by = c("mode2" = "mode_paper")) %>%
  mutate(accuracy_cohen_d = (accuracy_mean.y - accuracy_mean.x) / accuracy_pooled_sd) %>%
  select(mode1, mode2, accuracy_pooled_sd, accuracy_cohen_d, everything())

df_comb %>% write.csv("data/effect_size_accuracy_cohen_d.csv")



#########################################################
#                   Time
#########################################################

df_duration_main <- session_important %>% 
  group_by(mode_paper) %>%
  summarise(duration_main_mean = mean(duration_main),
            duration_main_sd = sd(duration_main)) 

a <- session_important %>% count(mode_paper)

df_duration_main <- df_duration_main %>% 
  mutate(N = a$n) %>% 
  mutate(duration_main_ci95 = 1.96 * (duration_main_sd/ sqrt(N)))

df_duration_main %>% write.csv("data/effect_size_duration_main_CI.csv")

b <- combn(df_duration_main$mode_paper,2,FUN = NULL, simplify = TRUE)
b <- as.character(b)

df_comb <- data.frame("mode1" = NA, "mode2"= NA, "duration_main_pooled_sd"= NA) 

for (i in 1:(length(b)/2)){
  index <- (i-1) * 2 + 1
  df_comb[i, 1] <- b[index]
  df_comb[i, 2] <- b[index + 1]
  
  z <- session_important[which(session_important$mode_paper == b[index] |
                                 session_important$mode_paper == b[index + 1]),]
  z$mode_paper <- factor(as.character(z$mode_paper))
  df_comb[i, 3] = sd_pooled(duration_main ~ mode_paper, data = z)
  
}


df_comb <- df_comb %>% 
  left_join(df_duration_main, by = c("mode1" = "mode_paper")) %>%
  left_join(df_duration_main, by = c("mode2" = "mode_paper")) %>%
  mutate(duration_main_cohen_d = (duration_main_mean.y - duration_main_mean.x) / duration_main_pooled_sd) %>%
  select(mode1, mode2, duration_main_pooled_sd, duration_main_cohen_d, everything())

df_comb %>% write.csv("data/effect_size_duration_main_cohen_d.csv")


#########################################################
#                   mean_confidence
#########################################################

df_mean_confidence <- session_important %>% 
  group_by(mode_paper) %>%
  summarise(mean_confidence_mean = mean(mean_confidence),
            mean_confidence_sd = sd(mean_confidence)) 

a <- session_important %>% count(mode_paper)

df_mean_confidence <- df_mean_confidence %>% 
  mutate(N = a$n) %>% 
  mutate(mean_confidence_ci95 = 1.96 * (mean_confidence_sd/ sqrt(N)))

df_mean_confidence %>% write.csv("data/effect_size_mean_confidence_CI.csv")

b <- combn(df_mean_confidence$mode_paper,2,FUN = NULL, simplify = TRUE)
b <- as.character(b)

df_comb <- data.frame("mode1" = NA, "mode2"= NA, "mean_confidence_pooled_sd"= NA) 

for (i in 1:(length(b)/2)){
  index <- (i-1) * 2 + 1
  df_comb[i, 1] <- b[index]
  df_comb[i, 2] <- b[index + 1]
  
  z <- session_important[which(session_important$mode_paper == b[index] |
                                 session_important$mode_paper == b[index + 1]),]
  z$mode_paper <- factor(as.character(z$mode_paper))
  df_comb[i, 3] = sd_pooled(mean_confidence ~ mode_paper, data = z)
  
}


df_comb <- df_comb %>% 
  left_join(df_mean_confidence, by = c("mode1" = "mode_paper")) %>%
  left_join(df_mean_confidence, by = c("mode2" = "mode_paper")) %>%
  mutate(mean_confidence_cohen_d = (mean_confidence_mean.y - mean_confidence_mean.x) / mean_confidence_pooled_sd) %>%
  select(mode1, mode2, mean_confidence_pooled_sd, mean_confidence_cohen_d, everything())

df_comb %>% write.csv("data/effect_size_mean_confidence_cohen_d.csv")



