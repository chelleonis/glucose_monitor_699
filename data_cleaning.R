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
  dplyr::mutate(Time24 = format(strptime(Time, "%I:%M %p"), format="%H:%M")) %>% 
  tidyr::separate(Time24,c("Hour","Minute"), sep = ":") %>%
  tidyr::separate(Date,c("Month","Day","Year"), sep = "/") %>%
  dplyr::select(-c(Time))  

#adding exploratory variables here 
#time past variable
#AUC variable(?) 
#above target variable
#below target variable
c_glu_key <- c_glu2 %>%  group_by(ID) %>% 
  dplyr::mutate(init = paste(Month,Day,Year,Hour,Minute,sep = "-")) %>%
  dplyr::mutate(init = first(init)) %>%
 


summary_stats <- c_glu2 %>% group_by(ID) %>% summarise(mean_gl = mean(glucose, na.rm = TRUE))

#compare to the other measurement method xddddd
#need to import lol

c_dem <- read.csv("C:/Users/typer321/Documents/cgm_demographics_biost699.csv", header = TRUE, 
                  sep = ",",fileEncoding="UTF-8-BOM")

c_dem <- c_dem %>% rename(ID = id)

merge_attempt <- inner_join(summary_stats, c_dem, by = "ID")

#by_id
test_plot <- ggplot(data = c_glu2, aes(x = Time, y = glucose, color = ID)) + 
  geom_line(aes(group = ID))

#new var, time post initial measurement

#separate by testing periods?
#ANOVA for AUC? -> time past, AUC