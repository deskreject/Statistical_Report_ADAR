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
if (!require(gtools)) install.packages("gtools"); library(gtools) #package including quantcut for creating quantile variables from continuous ones 
if (!require(countreg)) install.packages("countreg"); library(countreg) #package including quantcut for creating quantile variables from continuous ones 

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

#load data necessary for the matching procedure
posts_df <- read_csv("C:/R work/Research/stack and e14 data/posts_2.csv")
posts_ex_text_2 <- read_csv("C:/R work/Research/stack and e14 data/posts_ex_text_2.csv")
users_df <- read_csv("C:/R work/Research/stack and e14 data/users.csv")
#load necessary ee dataframes for matching
load("C:/R work/Research/stack and e14 data/crosssec_ee_match.Rda")
load("C:/R work/Research/stack and e14 data/posts_ex_text_reg_users_6m_ee.Rda")

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



#### matching work - setting up the necessary dataframe ####

#first need to filter only by registered users that are relevant to the 6M timeframe
e14_users_6m <- w_panel_did_qual_as_regUs_6m_b %>% filter(group == 1) %>% dplyr::select(user_ID) %>% unique()
e14_users_6m <- as.integer(unique(e14_users_6m$user_ID))  

#filter down the posts_df data to the users who are relevant for the 6M period
posts_df_reg_users_6m <- posts_df %>% filter(user_ID %in% e14_users_6m)

#count the words
##create a test dataframe to see how the function works
posts_df_reg_users_6m$word_count <- lengths(strsplit(posts_df_reg_users_6m$text, " "))

#extract the word_count and message_id variable in order to merge with the posts_ex_text_2 for the relevant users
word_count_df <- posts_df_reg_users_6m %>% dplyr::select(message_ID, word_count)

#filter down the posts_ex_text_2 dataframe to the registered users
posts_ex_text_reg_users_6m <- posts_ex_text_2 %>% filter(user_ID %in% e14_users_6m)
#filter out non-matching message IDs from the word_count_df
word_count_df_2 <- word_count_df %>% filter(message_ID %in% posts_ex_text_reg_users_6m$message_ID)

#merge the dataframes to include the word count variable
posts_ex_text_reg_users_6m <- merge(x = posts_ex_text_reg_users_6m, y = word_count_df_2, by = "message_ID")

### add in the q_start_date to the ee dataframe 
# find the minimum day of each quarter in order to have running values of days since registration by quarter
#get the minimum starting day according to the ee data
test_min_q_ee <- posts_ex_text_reg_users_6m_ee %>%
  group_by(t_shock_q) %>%
  summarise(min_ee = min(day))

#get the minimum starting day  according to the e14 data
test_min_q_e14 <- posts_ex_text_reg_users_6m %>% 
  group_by(t_shock_q) %>%
  summarise(min_e14 = min(day))

#merge to compare if they match
test_min_q_both <- merge(x = test_min_q_e14, y = test_min_q_ee, by = "t_shock_q", all= T)
#as they don't match and e14 tends to have the earlier date, I take e14 values as the true starting dates for quarters
#exception is the final quarter (not important, however annoying for matching)
test_min_q_both$q_start_date <- test_min_q_both$min_e14

#add in the quarterly dates, but exclude the final NA value
merge_q_dates_df <- test_min_q_both[-53,c(1,4)]

posts_ex_text_reg_users_6m <- merge(x = posts_ex_text_reg_users_6m,
                                    y = merge_q_dates_df,
                                    by = "t_shock_q")

#do the work for the registration dates --> check the script from the EE to make sure I don't forget anything

#add in the floor dates
users_df$reg_day <- as.Date(ceiling_date(users_df$registrationdate), unit = "day")

#filter by relevant users
users_df <- users_df %>% filter(ID %in% e14_users_6m)

#rename the ID column of the users_df dataframe to match that of the posts_ex_text column
users_df <- rename(users_df, user_ID = `ID`)

#merge the registration day to the relevant users
posts_ex_text_reg_users_6m <- merge(x = posts_ex_text_reg_users_6m, y = users_df[, c(1,14)], by = "user_ID")

#rearrange the df
posts_ex_text_reg_users_6m <- posts_ex_text_reg_users_6m[,c(1,3:41,2,43,42,44)]

#subtract the quarter day variable from the registration day variable

posts_ex_text_reg_users_6m$d_since_reg_2 <- as.numeric(posts_ex_text_reg_users_6m$q_start_date) - as.numeric(posts_ex_text_reg_users_6m$reg_day) 
posts_ex_text_reg_users_6m <- rename(posts_ex_text_reg_users_6m, d_since_reg = `d_since_reg_2`)

#### matching_panel_e14_6m_b: create the matching DF
#summarising with small df
matching_panel_e14_6m <- posts_ex_text_reg_users_6m %>%
  filter(day <= "2015-04-13") %>%
  group_by(user_ID, user_name, t_shock_q,q_start_date,reg_day) %>%
  summarise(answers = sum(ifelse(is.na(reply_state) == F, 1,0)),
            questions = sum(ifelse(is.na(question_state) == F, 1, 0)),
            orig_posts = sum(ifelse(action %in% c("blogcomment", "doccomment", "eventcomment", "reply","roadtestcomment","roadTestReviewscomment","videocomment"),0,1)),
            all_posts =n(),
            d_since_reg = max(d_since_reg),
            views = sum(views),
            words_all = sum(word_count),
            words_answer = sum(ifelse(is.na(reply_state) == F, word_count,0)))

#test something else
user_Ids_matching_panel <- as.integer(unique(matching_panel_e14_6m[,1]))
user_Ids_matching_panel <- unlist(user_Ids_matching_panel)

#try and get them added in

non_actives_pre_shock <- posts_ex_text_reg_users_6m %>%
  filter(!user_ID %in% user_Ids_matching_panel) %>%
  group_by(user_ID, user_name,reg_day) %>%
  summarise(q_start_date = NA,
            t_shock_q = 0,
            answers = 0,
            questions = 0,
            orig_posts = 0,
            all_posts =0,
            d_since_reg = max(d_since_reg),
            views = 0,
            words_all = 0,
            words_answer = 0)

matching_panel_e14_6m <- bind_rows(matching_panel_e14_6m, non_actives_pre_shock)


#balance the panel so that every person has an observation in every quarter
matching_panel_e14_6m_b <- make.pbalanced(matching_panel_e14_6m, balance.type = "fill", index = c("user_ID", "t_shock_q"))

#remove NAs - merging/matching required

#reg_day + user_name
#get a mergable table of the user_name and registration day info
merge_ids <- posts_ex_text_reg_users_6m %>% group_by(user_ID, user_name) %>% summarise(reg_day = unique(reg_day))

#get a mergable table of the q's and q_start_day
merge_qs <- posts_ex_text_reg_users_6m %>% filter(day <= "2015-04-13") %>% group_by(t_shock_q) %>% summarise(q_start_date = unique(q_start_date))

#do the merging process
matching_panel_e14_6m_b <- merge(x = matching_panel_e14_6m_b[,-4], y = merge_qs, by = "t_shock_q")
matching_panel_e14_6m_b <- merge(x = matching_panel_e14_6m_b[,-c(3,4)], y = merge_ids, by = "user_ID")
#reorder
matching_panel_e14_6m_b <- matching_panel_e14_6m_b[,c(1,12,13,2,11,3:10)]

#check whether there are any remaining NA values
sapply(matching_panel_e14_6m_b,function(x) sum(is.na(x)))

#remove the NAs from the summarised variables
matching_panel_e14_6m_b[,c(6:13)] <- as.data.frame(sapply(matching_panel_e14_6m_b[,c(6:13)], function(x) ifelse(is.na(x) ==T,0,x)))

#add in the days since registration variable
matching_panel_e14_6m_b$d_since_reg <- as.integer(matching_panel_e14_6m_b$q_start_date - matching_panel_e14_6m_b$reg_day)

#add in the per post/answer variables, adding in 0s when devide by 0
matching_panel_e14_6m_b$views_p_post <- round(ifelse(matching_panel_e14_6m_b$views> 0, 
                                                     matching_panel_e14_6m_b$views / matching_panel_e14_6m_b$orig_posts,
                                                     0),2)
matching_panel_e14_6m_b$words_p_post <- round(ifelse(matching_panel_e14_6m_b$words_all> 0,
                                                     matching_panel_e14_6m_b$words_all / matching_panel_e14_6m_b$all_posts,
                                                     0),2)
matching_panel_e14_6m_b$words_p_answer <- round(ifelse(matching_panel_e14_6m_b$answers > 0,
                                                       matching_panel_e14_6m_b$words_answer / matching_panel_e14_6m_b$answers,
                                                       0),2)

#add in the per post/answer variable, addin in NAs when divided by 0

matching_panel_e14_6m_b_alt <- matching_panel_e14_6m_b

matching_panel_e14_6m_b_alt$views_p_post <- round(ifelse(matching_panel_e14_6m_b_alt$views> 0, 
                                                         matching_panel_e14_6m_b_alt$views / matching_panel_e14_6m_b_alt$orig_posts,
                                                         NA),2)
matching_panel_e14_6m_b_alt$words_p_post <- round(ifelse(matching_panel_e14_6m_b_alt$words_all> 0,
                                                         matching_panel_e14_6m_b_alt$words_all / matching_panel_e14_6m_b_alt$all_posts,
                                                         NA),2)
matching_panel_e14_6m_b_alt$words_p_answer <- round(ifelse(matching_panel_e14_6m_b_alt$answers > 0,
                                                           matching_panel_e14_6m_b_alt$words_answer / matching_panel_e14_6m_b_alt$answers,
                                                           NA),2)



#add in the treatment group variable
matching_panel_e14_6m_b$t_groups <- 1
matching_panel_e14_6m_b_alt$t_groups <- 1



#summarise on the user_id level retaining only necessary variables (with _alt)
## not cutting the q's to -24 as cross sectional and shouldn't have impact on per post/answer variables?
### e14
crosssec_e14_match <- matching_panel_e14_6m_b_alt %>%
  group_by(user_ID,user_name, t_groups) %>%
  #max d since reg as I want the days since reg as of the treatment intro
  summarise(d_since_reg = max(d_since_reg),
            #original_posts as this is what I have the words on. also too little questions?
            orig_posts = sum(orig_posts),
            views_p_post = round(mean(views_p_post, na.rm = T),2),
            words_p_all_posts = round(mean(words_p_post, na.rm = T),2),
            words_p_answer = round(mean(words_p_answer, na.rm = T),2),
            answers_total = sum(answers))

# e14: add in the answer sum for each member per year for the last 3 years

#create an empty matrix with ncol for 3 years + userID column and nrow = number of users
y_asw_count_m <- matrix(ncol = 4, nrow = length(as.vector(crosssec_e14_match$user_ID)))
colnames(y_asw_count_m) <- c("user_ID", "answers_t_1", "answers_t_2", "answers_t_3")

#iterate over every user ID
for(i in 1:length(y_asw_count_m[,1])){
  
  #add in user ID
  y_asw_count_m[i,1] <- as.integer(crosssec_e14_match[i,1])
  #set the counter for the quarters to 0
  k = 0
  
  #iterate across the columns from 2nd to final
  for( j in 2:length(y_asw_count_m[1,])){
    
    #count the answers for quarter k to quarter k-4 (approx. 1 year)
    y_asw_count_m[i,j] <- matching_panel_e14_6m_b_alt %>% filter(user_ID == as.integer(crosssec_e14_match[i,1])) %>% 
      filter(t_shock_q <= k, t_shock_q > k-4) %>% 
      summarise(asw = sum(answers)) %>% .[1,1]
    
    #subtract 4 from the quarter counter to get the year previous
    k= k-4
  }
}

#add the new columns
crosssec_e14_match <- merge(x = crosssec_e14_match, y = y_asw_count_m, by = "user_ID")

#### generation of quantile variables ####

#IN CASE MATCHING DON'T WORK: create quantiles of the views per post with the tutorial on SO 
#crosssec_e14_match_test <- within(crosssec_e14_match_test, 
#                                p_views_qs_2 <- as.integer(cut(views_p_post, 
#                                                              quantile(views_p_post, probs=0:4/4, na.rm = T), 
#                                                             include.lowest=TRUE)))

#create quantiles of the views per post with quantcut package - 4

crosssec_e14_match$p_views_qs <- quantcut(crosssec_e14_match$views_p_post, q=5, na.rm = T)
crosssec_ee_match$p_views_qs <- quantcut(crosssec_ee_match$views_p_post, q=5, na.rm = T)

#d since registration
crosssec_e14_match$d_since_reg_qs <- quantcut(crosssec_e14_match$d_since_reg, q=5)
crosssec_ee_match$d_since_reg_qs <- quantcut(crosssec_ee_match$d_since_reg, q=5)

#orig posts

crosssec_e14_match$orig_posts_qs <- quantcut(crosssec_e14_match$orig_posts, q=3, na.rm = T)
crosssec_ee_match$orig_posts_qs <- quantcut(crosssec_ee_match$orig_posts, q=3, na.rm = T)

#words p answers
##replace NAs with 0 first
crosssec_e14_match$words_p_answer <- na.replace(crosssec_e14_match$words_p_answer, 0)
crosssec_ee_match$words_p_answer <- na.replace(crosssec_ee_match$words_p_answer, 0)


crosssec_e14_match$words_p_answer_qs <- quantcut(crosssec_e14_match$words_p_answer, q=5, na.rm = T)
crosssec_ee_match$words_p_answer_qs <- quantcut(crosssec_ee_match$words_p_answer, q=5, na.rm = T)

#answer total
crosssec_e14_match$answers_total_qs <- quantcut(crosssec_e14_match$answers_total, q = 5, na.rm = T)
crosssec_ee_match$answers_total_qs <- quantcut(crosssec_ee_match$answers_total, q = 5, na.rm = T)

#answers t -1
crosssec_e14_match$answers_t_1_qs <- quantcut(crosssec_e14_match$answers_t_1, q=3, na.rm = T)
crosssec_ee_match$answers_t_1_qs <- quantcut(crosssec_ee_match$answers_t_1, q=3, na.rm = T)

#answers t -2
crosssec_e14_match$answers_t_2_qs <- quantcut(crosssec_e14_match$answers_t_2, q=2, na.rm = T)
crosssec_ee_match$answers_t_2_qs <- quantcut(crosssec_ee_match$answers_t_2, q=2, na.rm = T)

#answers t -3
crosssec_e14_match$answers_t_3_qs <- quantcut(crosssec_e14_match$answers_t_3, na.rm = T)
crosssec_ee_match$answers_t_3_qs<- quantcut(crosssec_ee_match$answers_t_3, na.rm = T)


#save the new crosssec files
save(crosssec_e14_match, file="crosssec_e14_match.Rda")
save(crosssec_ee_match, file="crosssec_ee_match.Rda")

###dealing with the factor variables not converging when merged both e14 and 22 data issue ####

#labels = F for the quantcut package
#views per post quantiles
crosssec_e14_match$p_views_qs_2 <- as.factor(quantcut(crosssec_e14_match$views_p_post, q=5, na.rm = T, labels = F))
crosssec_ee_match$p_views_qs_2 <- as.factor(quantcut(crosssec_ee_match$views_p_post, q=5, na.rm = T, labels = F))

#d since registration
crosssec_e14_match$d_since_reg_qs_2 <- as.factor(quantcut(crosssec_e14_match$d_since_reg, q=5, labels = F))
crosssec_ee_match$d_since_reg_qs_2 <- as.factor(quantcut(crosssec_ee_match$d_since_reg, q=5, labels = F))

#orig posts
crosssec_e14_match$orig_posts_qs_2 <- as.factor(quantcut(crosssec_e14_match$orig_posts, q=3, na.rm = T, labels = F))
crosssec_ee_match$orig_posts_qs_2 <-plyr::revalue(crosssec_ee_match$orig_posts_qs, c("0" = "1", "(0,1]" = "2", "(1,146]" = "3"))

#words p answers
crosssec_e14_match$words_p_answer_qs_2 <- as.factor(quantcut(crosssec_e14_match$words_p_answer, q=5, na.rm = T, labels = F))
crosssec_ee_match$words_p_answer_qs_2 <- as.factor(quantcut(crosssec_ee_match$words_p_answer, q=5, na.rm = T, labels = F))

#answer total
crosssec_e14_match$answers_total_qs_2 <- plyr::revalue(crosssec_e14_match$answers_total_qs, c("[0,1)" = "1", "1" = "2", "(1,2]" = "3", "(2,6]" = "4", "(6,718]" = "5")) 
crosssec_ee_match$answers_total_qs_2 <- plyr::revalue(crosssec_ee_match$answers_total_qs, c("[0,1)" = "1", "1" = "2", "(1,2]" = "3", "(2,7]" = "4", "(7,3.3]" = "5")) 

#answers t -1
crosssec_e14_match$answers_t_1_qs_2 <- as.factor(quantcut(crosssec_e14_match$answers_t_1, q=3, na.rm = T, labels = F))
crosssec_ee_match$answers_t_1_qs_2 <- as.factor(quantcut(crosssec_ee_match$answers_t_1, q=3, na.rm = T, labels = F))

#answers t -2
crosssec_e14_match$answers_t_2_qs_2 <- plyr::revalue(crosssec_e14_match$answers_t_2_qs, c("0" = "1", "(0,360]" = "2")) 
crosssec_ee_match$answers_t_2_qs_2 <- plyr::revalue(crosssec_ee_match$answers_t_2_qs, c("0" = "1", "(0,1.65]" = "2")) 

#answers t -3
crosssec_e14_match$answers_t_3_qs_2 <- plyr::revalue(crosssec_e14_match$answers_t_3_qs, c("0" = "1", "(0,142]" = "2")) 
crosssec_ee_match$answers_t_3_qs_2<- plyr::revalue(crosssec_ee_match$answers_t_3_qs, c("0" = "1", "(0,751]" = "2")) 

#row bind both e14 and ee

crosssec_both_match <- rbind(crosssec_e14_match, crosssec_ee_match)
crosssec_both_match$t_groups <- as.factor(crosssec_both_match$t_groups)

save(crosssec_both_match, file = "C:/R work/Advanced data analytics with R/Statistical Report/data/crosssec_both_match.Rda")


####  do the propensity score calculations
#quantile propensity score calculations
m_ps_qs_2 <- glm(t_groups ~ d_since_reg_qs_2 + 
                   orig_posts_qs_2 +
                   words_p_answer_qs_2 +
                   answers_total_qs_2 + 
                   answers_t_1_qs_2 +
                   answers_t_2_qs_2 +
                   answers_t_3_qs_2,
                 family = binomial(), data = crosssec_both_match)

#unaltered propensity score calculations
m_ps_unalt_1 <- glm(t_groups ~  d_since_reg +
                      orig_posts +
                      words_p_answer +
                      answers_total +
                      answers_t_1 + 
                      answers_t_2 + 
                      answers_t_3, family = binomial(), data = crosssec_both_match)


##execute NN matching ordered for un-quantiled, un-scaled with best fitting model
crosssec_both_match$PS_unalt <- predict(m_ps_unalt_1)
crosssec_both_match_unalt_ord <- crosssec_both_match %>% arrange(desc(PS_unalt))

#execute the matching by nearest neighbour
crosssec_both_match_unalt_ord_NoNas <- crosssec_both_match_unalt_ord %>% dplyr::select(c(-views_p_post, -p_views_qs, -p_views_qs_2))

NN_match_unalt_ord <- matchit(t_groups ~  d_since_reg +
                                orig_posts +
                                words_p_answer +
                                answers_total +
                                answers_t_1 + 
                                answers_t_2 + 
                                answers_t_3, method = "nearest", data = crosssec_both_match_unalt_ord_NoNas)


summary(NN_match_unalt_ord)
plot(NN_match_unalt_ord, type = "jitter", interactive = F)
plot(NN_match_unalt_ord, type = "hist")

## Ordered quantile matching
#Order first by number of answers 
crosssec_both_match$PS <- predict(m_ps_qs_2)

crosssec_both_match_ord <- crosssec_both_match %>% arrange(desc(PS))

##execute the matching by nearest neighbor with quantiles after ordering
matching_panel_both_ord_noNas <- crosssec_both_match_ord %>% dplyr::select(c(-views_p_post, -p_views_qs, -p_views_qs_2))

NN_match_qs_ord <- matchit(t_groups ~ d_since_reg_qs_2 + 
                             orig_posts_qs_2 +
                             words_p_answer_qs_2 +
                             answers_total_qs_2 + 
                             answers_t_1_qs_2 +
                             answers_t_2_qs_2 +
                             answers_t_3_qs_2, method = "nearest", data = matching_panel_both_ord_noNas)

summary(NN_match_qs_ord)
plot(NN_match_qs_ord, type = "jitter", interactive = F)
plot(NN_match_qs_ord, type = "hist")

#saving the 2 matching procedures that I ended up choosing

match_data_qs <- match.data(NN_match_qs_ord)
match_data_unalt <- match.data(NN_match_unalt_ord)

#filtering down the matched panel data to the useres that were matched

#include only individuals that were matched on quantile matching variables
w_panel_matched_qual_6m_qs_b <- w_panel_did_qual_as_regUs_6m_b %>% filter(user_ID %in% match_data_qs$user_ID)

#include only individuals that were matched on unaltered matching variables
w_panel_matched_qual_6m_unalt_b <- w_panel_did_qual_as_regUs_6m_b %>% filter(user_ID %in% match_data_unalt$user_ID)

#### A data simulation experiment #####

#simulate a simple before and after dataset of counts based on poisson distribution
set.seed(543)
n <- 50000
group_1 <- sample(c(0,1), size = n, replace = T)
treatment <- sample(c(0,1), size = n, replace = T)
y_sim <- rpois(n = n, lambda = exp(1.6 + 0.5*group_1 - 0.69*treatment - 0.8*group_1*treatment))

#what does the distribution show
table(y_sim, group_1)

#what does the ord_plot show
Ord_plot(y_sim)

#run a model according to the correct specifications
m1 <- glm(y_sim ~ group_1*treatment, family = poisson)
#run a model according to incorrect specifications
m2 <- glm(y_sim ~ group_1, family = poisson)
#run a ols model
m3 <- lm(y_sim ~ group_1*treatment)
#run a logged ols model
m4 <- lm(log(y_sim +1) ~ group_1*treatment)

#show the different models
stargazer(m1,m2,m3,m4,
          type = "text")

exp(m1$coefficients)          #### shows that all of the above models are pretty good in terms of similar coefffs ####

#plot qq_plots of the model
qqnorm(residuals(m4))
qqline(residuals(m4))

#plot rootgrams for the models
countreg::rootogram(m1)
countreg::rootogram(m2)


### fitting a 0 infalted model

set.seed(543)

#set the number of observations
n <- 50000
#set the dummy variable for group
group_1 <- sample(c(0,1), size = n, replace = TRUE)
#set the dummy variable for treatment
treatment <- sample(c(0,1), size = n, replace = TRUE)

#simulate the 0s, giving a probability for the occurence of 1
z <- rbinom(n = n, size = 1, prob = 0.3) 

#simulate the negative binomial distributed, 0 infalted ys. Based loosly on the answer parameters from my models
y_sim <- ifelse(z == 0, 0, 
                rnbinom(n = n, 
                        mu = exp(1.6 + 0.5*group_1 - 0.69*treatment - 0.8*group_1*treatment), 
                        size = 0.35))

#plot the distribution
barplot(table(y_sim))
barplot(table(w_panel_matched_qual_6m_unalt_b$actions))

###fit some models
formula_m <- y_sim ~ group_1*treatment
#the correct specification
m1 <- pscl::zeroinfl(y_sim ~ group_1*treatment|1, dist = "negbin")
#the correct specification without 0 inflation
m2 <- glm.nb(y_sim ~ group_1*treatment)
#the poisson specification without 0 inflation
m3 <- glm(formula_m, family = poisson)
#an ols model
m4 <- lm(formula_m)
#an logged ols model
m5 <- lm(log(y_sim + 1) ~ group_1*treatment)

#show the summary of the models
stargazer(m2,m3,m4,m5,
          type = "text")


summary(m1)

exp(m1$coefficients[[1]]) 

#show the distribution of errors
qqnorm(residuals(m5))
qqline(residuals(m5))

#show the rootograms
par(mfrow = c(1, 3))
countreg::rootogram(m1)
countreg::rootogram(m2)
countreg::rootogram(m3)
par(mfrow = c(1, 1))
