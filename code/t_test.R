library(effectsize)
library(tidyverse)

session_important <- read_csv("data/key_info_1.csv")

Baseline <- session_important[which(session_important$mode_paper == "Baseline"),]
Full <- session_important[which(session_important$mode_paper == "Full"),]
Low <- session_important[which(session_important$mode_paper == "Low"),]
Masked <- session_important[which(session_important$mode_paper == "Masked"),]
Moderate <- session_important[which(session_important$mode_paper == "Moderate"),]


#########################################################
#                   accuracy
#########################################################

t.test(Baseline$scr30_percent, Full$scr30_percent)
t.test(Baseline$scr30_percent, Low$scr30_percent)
t.test(Baseline$scr30_percent, Masked$scr30_percent)
t.test(Baseline$scr30_percent, Moderate$scr30_percent)
t.test(Full$scr30_percent, Low$scr30_percent)
t.test(Full$scr30_percent, Masked$scr30_percent)
t.test(Full$scr30_percent, Moderate$scr30_percent)
t.test(Low$scr30_percent, Masked$scr30_percent)
t.test(Low$scr30_percent, Moderate$scr30_percent)
t.test(Masked$scr30_percent, Moderate$scr30_percent)


#########################################################
#                   Confidence 
#########################################################

t.test(session_important$mean_confidence_right, session_important$mean_confidence_wrong)

confidence_pooled_sd <- sd_pooled(mean_confidence_right ~ mean_confidence_wrong, data = session_important)
confidence_cohen_d = (mean(session_important$mean_confidence_right) - 
                        mean(session_important$mean_confidence_wrong)) / 
                        confidence_pooled_sd

