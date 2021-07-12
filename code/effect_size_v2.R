if (!is.element("devtools", installed.packages()[,1])) {
  install.packages("devtools", dep = TRUE)
}

if (!is.element("esci", installed.packages()[,1])) {
  devtools::install_github(repo = "rcalinjageman/esci")
}


library("esci")
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
  mutate(N = a$n)

df_accuracy %>% write.csv("data/effect_size_v2/effect_size_accuracy_CI.csv")

b <- combn(df_accuracy$mode_paper,2,FUN = NULL, simplify = TRUE)
b <- as.character(b)

df_comb <- data.frame("mode1" = NA, "mode2"= NA) 

for (i in 1:(length(b)/2)){
  index <- (i-1) * 2 + 1
  df_comb[i, 1] <- b[index]
  df_comb[i, 2] <- b[index + 1]
}


df_comb <- df_comb %>% 
  left_join(df_accuracy, by = c("mode1" = "mode_paper")) %>%
  left_join(df_accuracy, by = c("mode2" = "mode_paper"))


df_comb <- df_comb %>% 
  mutate(accuracy_cohen_d = estimateStandardizedMeanDifference(m1 = accuracy_mean.x,
                                                               m2 = accuracy_mean.y,
                                                               s1 = accuracy_sd.x,
                                                               s2 = accuracy_sd.y,
                                                               n1 = N.x,
                                                               n2 = N.y,
                                                               conf.level = .95)[["cohend"]],
         accuracy_cohen_d_low = estimateStandardizedMeanDifference(m1 = accuracy_mean.x,
                                                               m2 = accuracy_mean.y,
                                                               s1 = accuracy_sd.x,
                                                               s2 = accuracy_sd.y,
                                                               n1 = N.x,
                                                               n2 = N.y,
                                                               conf.level = .95)[["cohend.low"]],
         accuracy_cohen_d_high = estimateStandardizedMeanDifference(m1 = accuracy_mean.x,
                                                                m2 = accuracy_mean.y,
                                                                s1 = accuracy_sd.x,
                                                                s2 = accuracy_sd.y,
                                                                n1 = N.x,
                                                                n2 = N.y,
                                                                conf.level = .95)[["cohend.high"]]) %>%
  select(mode1, mode2, accuracy_cohen_d, accuracy_cohen_d_low, accuracy_cohen_d_high)
df_comb %>% write.csv("data/effect_size_v2/effect_size_accuracy_cohen_d.csv")



#########################################################
#                   Time
#########################################################

df_duration_main <- session_important %>% 
  group_by(mode_paper) %>%
  summarise(duration_main_mean = mean(duration_main),
            duration_main_sd = sd(duration_main)) 

a <- session_important %>% count(mode_paper)

df_duration_main <- df_duration_main %>% 
  mutate(N = a$n) 
df_duration_main %>% write.csv("data/effect_size_v2/effect_size_duration_main_CI.csv")

b <- combn(df_duration_main$mode_paper,2,FUN = NULL, simplify = TRUE)
b <- as.character(b)

df_comb <- data.frame("mode1" = NA, "mode2"= NA) 

for (i in 1:(length(b)/2)){
  index <- (i-1) * 2 + 1
  df_comb[i, 1] <- b[index]
  df_comb[i, 2] <- b[index + 1]
}


df_comb <- df_comb %>% 
  left_join(df_duration_main, by = c("mode1" = "mode_paper")) %>%
  left_join(df_duration_main, by = c("mode2" = "mode_paper"))

df_comb <- df_comb %>%
  mutate(duration_main_cohen_d = estimateStandardizedMeanDifference(m1 = duration_main_mean.x,
                                                                    m2 = duration_main_mean.y,
                                                                    s1 = duration_main_sd.x,
                                                                    s2 = duration_main_sd.y,
                                                                    n1 = N.x,
                                                                    n2 = N.y,
                                                                    conf.level = .95)[["cohend"]],
         duration_main_cohen_d_low = estimateStandardizedMeanDifference(m1 = duration_main_mean.x,
                                                                m2 = duration_main_mean.y,
                                                                s1 = duration_main_sd.x,
                                                                s2 = duration_main_sd.y,
                                                                n1 = N.x,
                                                                n2 = N.y,
                                                                conf.level = .95)[["cohend.low"]],
         duration_main_cohen_d_high = estimateStandardizedMeanDifference(m1 = duration_main_mean.x,
                                                                 m2 = duration_main_mean.y,
                                                                 s1 = duration_main_sd.x,
                                                                 s2 = duration_main_sd.y,
                                                                 n1 = N.x,
                                                                 n2 = N.y,
                                                                 conf.level = .95)[["cohend.high"]]) %>%
  select(mode1, mode2, duration_main_cohen_d, duration_main_cohen_d_low, duration_main_cohen_d_high)

           
df_comb %>% write.csv("data/effect_size_v2/effect_size_duration_main_cohen_d.csv")


#########################################################
#                   mean_confidence
#########################################################

df_mean_confidence <- session_important %>% 
  group_by(mode_paper) %>%
  summarise(mean_confidence_mean = mean(mean_confidence),
            mean_confidence_sd = sd(mean_confidence)) 

a <- session_important %>% count(mode_paper)

df_mean_confidence <- df_mean_confidence %>% 
  mutate(N = a$n) 
df_mean_confidence %>% write.csv("data/effect_size_v2/effect_size_mean_confidence_CI.csv")

b <- combn(df_mean_confidence$mode_paper,2,FUN = NULL, simplify = TRUE)
b <- as.character(b)

df_comb <- data.frame("mode1" = NA, "mode2"= NA) 

for (i in 1:(length(b)/2)){
  index <- (i-1) * 2 + 1
  df_comb[i, 1] <- b[index]
  df_comb[i, 2] <- b[index + 1]
}


df_comb <- df_comb %>% 
  left_join(df_mean_confidence, by = c("mode1" = "mode_paper")) %>%
  left_join(df_mean_confidence, by = c("mode2" = "mode_paper"))

df_comb <- df_comb %>%
  mutate(mean_confidence_cohen_d = estimateStandardizedMeanDifference(m1 = mean_confidence_mean.x,
                                                                      m2 = mean_confidence_mean.y,
                                                                      s1 = mean_confidence_sd.x,
                                                                      s2 = mean_confidence_sd.y,
                                                                      n1 = N.x,
                                                                      n2 = N.y,
                                                                      conf.level = .95)[["cohend"]],
         mean_confidence_cohen_d_low = estimateStandardizedMeanDifference(m1 = mean_confidence_mean.x,
                                                                m2 = mean_confidence_mean.y,
                                                                s1 = mean_confidence_sd.x,
                                                                s2 = mean_confidence_sd.y,
                                                                n1 = N.x,
                                                                n2 = N.y,
                                                                conf.level = .95)[["cohend.low"]],
         mean_confidence_cohen_d_high = estimateStandardizedMeanDifference(m1 = mean_confidence_mean.x,
                                                                 m2 = mean_confidence_mean.y,
                                                                 s1 = mean_confidence_sd.x,
                                                                 s2 = mean_confidence_sd.y,
                                                                 n1 = N.x,
                                                                 n2 = N.y,
                                                                 conf.level = .95)[["cohend.high"]]) %>%
  select(mode1, mode2, mean_confidence_cohen_d, mean_confidence_cohen_d_low, mean_confidence_cohen_d_high)

df_comb %>% write.csv("data/effect_size_v2/effect_size_mean_confidence_cohen_d.csv")



