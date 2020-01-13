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
  dplyr::mutate(delta_time = as.numeric(difftime(combined,init))/3600) %>% 
  filter(ID == 21 | ID == 36)
#336 hours = 2 weeks, since each glucose measurement period was 2 weekss
c_glu_separate <- c_glu2 %>% mutate(period = cut(delta_time, breaks= c(-Inf,336,Inf),labels = c(1,2))) %>%
  group_by(ID,period) %>% dplyr::arrange(combined) %>% dplyr::mutate(init = first(combined)) %>%
  dplyr::mutate(delta_time_group = as.numeric(difftime(combined,init))/3600) 

#delta_time is in hours

#adding exploratory variables here 
#time past variable
#Time in Range
#AUC variable(?) 
#above target variable
#below target variable



summary_stats <- c_glu2 %>% group_by(ID) %>% summarise(mean_gl = mean(glucose, na.rm = TRUE))

#compare to the other measurement method xddddd
#need to import lol

c_dem <- read.csv("C:/Users/typer321/Documents/cgm_demographics_biost699.csv", header = TRUE, 
                  sep = ",",fileEncoding="UTF-8-BOM")

c_dem <- c_dem %>% rename(ID = id)

merge_attempt <- inner_join(summary_stats, c_dem, by = "ID")

#by_id
test_plot <- ggplot(data = c_glu2, aes(x = delta_time, y = glucose, color = ID)) + 
  geom_line(aes(group = ID))

test_plot2 <- ggplot(data = c_glu_separate, aes(x = delta_time_group, y = glucose, color = ID)) + 
  geom_line(aes(group = ID))

#new var, time post initial measurement

#separate by testing periods?
#ANOVA for AUC? -> time past, AUC