#the packages I need
#import package
library(tidyverse)
library(gridExtra)
library(lubridate)
library(Hmisc)
if (!require(psych)) install.packages("psych"); library(psych)  
if (!require(janitor)) install.packages("janitor"); library(janitor)  #for tabyl function
if (!require(RColorBrewer)) install.packages("RColorBrewer"); library(RColorBrewer)  
if (!require(MASS)) install.packages("MASS"); library(MASS)  #for nb.glm (from glm models.R)
if (!require(car)) install.packages("car"); library(car)  #for additional lm and glm plots and functions (from glm models.R)
if (!require(effects)) install.packages("effects"); library(effects)  #for plotting effects (from glm models.R)
if (!require(AER)) install.packages("AER"); library(AER)  #overdispersion test (from glm models.R)
if (!require(stargazer)) install.packages("stargazer"); library(stargazer)  #tables
if (!require(plm)) install.packages("plm"); library(plm)  #balanced panels
if (!require(anytime)) install.packages("anytime"); library(anytime) 
if (!require(tidyr)) install.packages("tidyr"); library(tidyr) #to get the "pivot_wider" 
if (!require(MatchIt)) install.packages("MatchIt"); library(MatchIt) #package for matching
if (!require(optmatch)) install.packages("optmatch"); library(optmatch) #package for optimal matching
if (!require(broom)) install.packages("broom"); library(broom) #for understanding regression results
if (!require(fixest)) install.packages("fixest"); library(fixest) #for the point estimate and error bar plots for pretrends
if (!require(lfe)) install.packages("lfe"); library(lfe) #for linear models with multiway clustering and fixed effects
if (!require(alpaca)) install.packages("alpaca"); library(alpaca) #for non-linear models with multiway clustering and fixed effects
if (!require(boot)) install.packages("boot"); library(boot) #for bootstrapped standard errors
if (!require(clusterSEs)) install.packages("clusterSEs"); library(clusterSEs) #for bootstrapped standard errors

#the dataframes I have to load
#the point where I start to clean the panel data from users
load("C:/R work/Research/stack and e14 data/qual_pre_panel.Rda")
load("C:/R work/Research/stack and e14 data/qual_pre_panel_ee.Rda")
#the data needed for calculating words per answer and questions per week
load("C:/R work/Research/stack and e14 data/posts_ex_text_reg_users_6m.Rda") 
load("C:/R work/Research/stack and e14 data/posts_ex_text_reg_users_6m_ee.Rda")
#the data needed to add in the time of registration, activity etc. (line 50)
load("C:/R work/Research/stack and e14 data/qual_merged_pre_pt0_merged.Rda")
load("C:/R work/Research/stack and e14 data/IDs_reg_merged.Rda")
posts_e14 <- 
posts_ee

#users to exclude
users_exclude_e14 <- load("C:/R work/Research/stack and e14 data/users_ex_coded_e14_vec.Rda") 
users_exclude_ee <- load("C:/R work/Research/stack and e14 data/users_ex_coded_ee_vec.Rda") 
users_exclude_ee_trans <- load("C:/R work/Research/stack and e14 data/users_ex_coded_ee_vec_trans.Rda") 

#cleaning the posts_ex_text data from users that my students coded

#clean the data frames
qual_pre_panel_clean_coded <- qual_pre_panel %>% filter(!user_ID %in% users_ex_coded_e14_vec)
qual_pre_panel_ee_coded <- qual_pre_panel_ee %>% filter(!user_ID %in% users_ex_coded_ee_vec_trans)

#add in the group variable
qual_pre_panel_clean_coded$group <- 1


#add in the ratio of likes to replies / answers variable
qual_pre_panel_clean_coded$ratio_qual <- qual_pre_panel_clean_coded$qual_sum / qual_pre_panel_clean_coded$actions

#merge the dataframes -- line 1614 in analysis code
panel_2y_dly_merged <- bind_rows(qual_pre_panel_clean, qual_pre_panel_ee)

#create a new dummy variable that indicates whethere a user is a user who was active 2 years prior. 
panel_2y_dly_merged$user_active <- ifelse(panel_2y_dly_merged$user_ID %in% qual_merged_pre_pt0_merged$user_ID, 1, 0)

#creat a new dummy variable that indicates whether a user was registered before the shock
panel_2y_dly_merged$user_regist <- ifelse(panel_2y_dly_merged$user_ID %in% IDs_reg_merged$user_ID,1, 0)

#preparatory work to make sure that I don't kick out users that didn't post any answers: setting actions to 0 if they aren't answers
panel_2y_dly_merged$actions_asw <- ifelse(panel_2y_dly_merged$answers == 1, panel_2y_dly_merged$actions, 0)


#creating the weekly panel data frame
w_panel_did_qual_as_regUs_6m <- panel_2y_dly_merged %>%
  filter(t_shock_w>= -24, t_shock_w<= 23) %>%
  filter(answers == 1) %>%
  filter(user_regist == 1) %>%
  group_by(user_ID, t_shock_w, post_treatment, group) %>%
  summarise(actions = sum(actions), qual_sum = sum(qual_sum), qual_ratio = sum(qual_sum)/sum(actions))

#get the word count data per user per week
## element 14
w_word_count_e14 <- posts_ex_text_reg_users_6m %>%
  dplyr::filter(is.na(reply_state) == F) %>%
  group_by(t_shock_w, user_ID) %>%
  summarise(word_count = sum(word_count))

## stack exchange
w_word_count_ee <- posts_ex_text_reg_users_6m_ee %>%
  filter(PostTypeId > 1) %>%
  group_by(t_shock_w, OwnerUserId) %>%
  summarise(word_count = sum(word_count))

colnames(w_word_count_ee) <- colnames(w_word_count_e14)

#merge by row
w_word_count_merged <- rbind(w_word_count_e14, w_word_count_ee)

#merge with weekly panel data
w_panel_did_qual_as_regUs_6m <- merge(x = w_panel_did_qual_as_regUs_6m, y = w_word_count_merged, by=c("user_ID", "t_shock_w"))

#check word count --> checks out
## posts_ex_text_reg_users_6m_ee %>% filter(OwnerUserId == 103300000, t_shock_w == 11) %>% dplyr::select(word_count)

#add in a word per reply count
w_panel_did_qual_as_regUs_6m$words_p_asw <- w_panel_did_qual_as_regUs_6m$word_count /w_panel_did_qual_as_regUs_6m$actions 

#balance the panel -> qual_sum and qual_ratio NAs left as NAs as 0 has meaning
## if as 0s, then I will need to restandardize the data
#balance the panel data
w_panel_did_qual_as_regUs_6m_b <- make.pbalanced(w_panel_did_qual_as_regUs_6m, balance.type = "fill", index = c("user_ID", "t_shock_w"))



#redjust the NA data
## add in seperate column with word_count NAs replaced with 0s to check how this influences the results
w_panel_did_qual_as_regUs_6m_b$word_count_NAless <- ifelse(is.na(w_panel_did_qual_as_regUs_6m_b$word_count) == T, 0, 
                                                           w_panel_did_qual_as_regUs_6m_b$word_count)
w_panel_did_qual_as_regUs_6m_b$words_p_asw_NAless <- ifelse(is.na(w_panel_did_qual_as_regUs_6m_b$words_p_asw) == T, 0, 
                                                            w_panel_did_qual_as_regUs_6m_b$words_p_asw)
#icnlude the group variable for missing - groups
e14_Ids <- w_panel_did_qual_as_regUs_6m_b %>% filter(group == 1) %>% dplyr::select(user_ID)
w_panel_did_qual_as_regUs_6m_b$group <- ifelse(w_panel_did_qual_as_regUs_6m_b$user_ID %in% e14_Ids$user_ID, 1, 0)
#insert missing actions
w_panel_did_qual_as_regUs_6m_b$actions <- ifelse(is.na(w_panel_did_qual_as_regUs_6m_b$actions) == T,0, w_panel_did_qual_as_regUs_6m_b$actions)
#insert missing post_treatment variable
w_panel_did_qual_as_regUs_6m_b$post_treatment <- ifelse(w_panel_did_qual_as_regUs_6m_b$t_shock_w >= 0,1,0)

#add in number of answers per week
w_panel_did_qual_as_regUs_6m_b <- w_panel_did_qual_as_regUs_6m_b %>%
  group_by(group, t_shock_w) %>%
  mutate(answers_per_week = sum(actions))


## add in number of questions questions per week

#questions per week e14
ques_per_week_e14 <- posts_ex_text_reg_users_6m %>%
  filter(t_shock_w>= -24, t_shock_w<= 23) %>%
  filter(is.na(question_state) == F) %>%
  group_by(t_shock_w) %>%
  summarise(questions_per_week = n()) %>%
  mutate(group = 1)

#questions per week ee
ques_per_week_ee <- posts_ex_text_reg_users_6m_ee %>%
  filter(t_shock_w>= -24, t_shock_w<= 23) %>%
  filter(PostTypeId == 1) %>%
  group_by(t_shock_w) %>%
  summarise(questions_per_week = n()) %>%
  mutate(group = 0)

colnames(ques_per_week_ee) <- colnames(ques_per_week_e14)

#row bind the two
ques_per_week_6m_both <- bind_rows(ques_per_week_ee,ques_per_week_e14)

#merge the number of questions to the regUs df
w_panel_did_qual_as_regUs_6m_b <- merge(x = w_panel_did_qual_as_regUs_6m_b, y = ques_per_week_6m_both, by=c("t_shock_w", "group"))

#save the data into the "data" folder
save(w_panel_did_qual_as_regUs_6m_b, file = "data/w_panel_did_qual_as_regUs_6m_b.Rda")
save(qual_pre_panel_clean_coded, file = "data/qual_pre_panel_clean_coded.Rda")
save(qual_pre_panel_ee_coded, file = "data/qual_pre_panel_ee_coded.Rda")
