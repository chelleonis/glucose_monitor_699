#Initial Glucose Data Cleaning
#Allen Li

#spell-check is f7 btw LOL

#part 1: continual data portion

#can do preliminary analysis on those who completed the optional vs those who didn't

library(dplyr)
library(tidyr)
library(ggplot2)

c_glu <- read.csv("C:/Users/typer321/Documents/cgm_glucose_biost699.csv", header = TRUE, 
                  sep = ",",fileEncoding="UTF-8-BOM")

#create variable time past visit


#below target (hypoglycemic), bg < 70, < 54
#above target BG > 180 and 250)

c_glu2 <- c_glu %>% rename(glucose = Historic.Glucose.mg.dL.) %>% 
  dplyr::mutate(combined = format(strptime(paste(Date,Time), "%m/%d/%Y %I:%M %p"))) %>%
  dplyr::select(-c(Date,Time)) %>%
  group_by(ID) %>% dplyr::arrange(combined) %>% dplyr::mutate(init = first(combined)) %>%
  dplyr::mutate(delta_time = as.numeric(difftime(combined,init))/3600)
#336 hours = 2 weeks, since each glucose measurement period was 2 weeks
c_glu_separate <- c_glu2 %>% mutate(period = cut(delta_time, breaks= c(-Inf,345,Inf),labels = c(1,2))) %>%
  group_by(ID,period) %>% dplyr::arrange(combined) %>% dplyr::mutate(init = first(combined)) %>%
  dplyr::mutate(delta_time_group = as.numeric(difftime(combined,init))/3600) 

#delta_time is in hours

#TO-DO: CREATE THE RIGHT VERSION OF DESIGN
merge_attempt_2 <- inner_join(c_glu_separate, c_dem2, by = "ID") %>% rename(tg = Treatment.group) %>%
  mutate(drug_type = case_when( (tg == 'Group A' & period == 1) | (tg == 'Group B' & period == 2) ~ 'x',
         (tg == 'Group B' & period == 1) | (tg == 'Group A' & period == 2) ~ 'y')) 

#Group A: received drug x then drug y
#Group B: received drug y then drug x

#adding exploratory variables here 
#time past variable
#Time in Range
#AUC variable(?) 
#above target variable
#below target variable (hypoglycemia)
at_1 = 180
at_2 = 250 #above target
bt_1 = 70
bt_2 = 54 #below target

#need to account for long breaks when calculating delta
#split into breaks in groups?
#biggest break > 2 hours

merge_attempt_3 <- merge_attempt_2 %>% group_by(ID,period) %>%
  mutate(dt_group = delta_time_group-lag(delta_time_group)) %>%
  rename(dt_init = delta_time_group) %>%
  mutate(gluc_avg = (glucose+lag(glucose))/2) %>%
  mutate(missings = cut(dt_group, breaks= c(-Inf,2,Inf), labels = c(0,1))) %>%
  dplyr::select(ID,glucose,period,tg,drug_type,dt_init,dt_group, gluc_avg) 

merge_attempt_3[is.na(merge_attempt_3)] <- 0

merge_attempt_4 <- merge_attempt_3 %>% 
  mutate(d_at_1 = glucose-at_1) %>% 
  mutate(d_at_2 = glucose-at_2) %>%
  mutate(d_bt_1 = glucose-bt_1) %>%
  mutate(d_bt_2 = glucose-bt_2)
  
#normalize AUC to total time

short_summary <- merge_attempt_4 %>% group_by(ID,drug_type) %>%
  summarize(time_at_1 = sum(dt_group[d_at_1 > 0]/max(dt_init)), 
            time_at_2 = sum(dt_group[d_at_2 > 0]/max(dt_init)),
            time_bt_1 = sum(dt_group[d_bt_1 < 0]/max(dt_init)),
            time_bt_2 = sum(dt_group[d_bt_2 < 0]/max(dt_init)),
            n_auc_at_1 = sum((dt_group[d_at_1 > 0]*gluc_avg[d_at_1>0])/max(dt_init)),
            n_auc_at_2 = sum((dt_group[d_at_2 > 0]*gluc_avg[d_at_2>0])/max(dt_init)),
            n_auc_bt_1 = sum((dt_group[d_bt_1 < 0]*gluc_avg[d_bt_1<0])/max(dt_init)),
            n_auc_bt_2 = sum((dt_group[d_bt_2 < 0]*gluc_avg[d_bt_2<0])/max(dt_init)),
            mean_gl = mean(glucose),
            var_gl = var(glucose)) 

#AUC normalized by total time

c_dem <- read.csv("C:/Users/typer321/Documents/cgm_demographics_biost699.csv", header = TRUE, 
                  sep = ",",fileEncoding="UTF-8-BOM") %>% rename(ID = id)
glimpse(c_dem)
#variable recoding here
#drug y = 0, x = 1
#sex 0 = male, 1 = female
#race 0 = other, 1 = white
#ethnicity 0 non-latino, 1= latino/hispanic

c_dem2 <- c_dem %>% 
  mutate(Sex = ifelse(grepl(1,as.character(c_dem$Sex)),1,0)) %>%
  mutate(Race = ifelse(grepl(4,as.character(c_dem$Race)),1,0)) %>%
  mutate(Ethnicity = ifelse(grepl(0,as.character(c_dem$Ethnicity)),1,0))
  

merge_attempt_5 <- inner_join(short_summary, c_dem, by = "ID")

#with appropriated coded variables:

merge_attempt_6 <- inner_join(short_summary, c_dem2, by = "ID") %>%
  mutate(drug_type = ifelse(grepl('x',drug_type),1,0))



