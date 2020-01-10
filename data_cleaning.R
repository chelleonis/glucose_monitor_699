#Initial Glucose Data Cleaning
#Allen Li

#spell-check is f7 btw LOL

#part 1: continual data portion

#can do preliminary analysis on those who completed the optional vs those who didn't

library(dplyr)

c_glu <- read.csv("C:/Users/typer321/Documents/cgm_glucose_biost699.csv", header = TRUE, 
                  sep = ",",fileEncoding="UTF-8-BOM")

#create variable time past visit


#below target (hypoglycemic), bg < 70, < 54
#above target BG > 180 and 250)

c_glu2 <- c_glu %>% rename(glucose = Historic.Glucose.mg.dL.)

summary_stats <- c_glu2 %>% group_by(ID) %>% summarise(mean_gl = mean(glucose, na.rm = TRUE))

#compare to the other measurement method xddddd
#need to import lol

c_dem <- read.csv("C:/Users/typer321/Documents/cgm_demographics_biost699.csv", header = TRUE, 
                  sep = ",",fileEncoding="UTF-8-BOM")

c_dem <- c_dem %>% rename(ID = id)

merge_attempt <- inner_join(summary_stats, c_dem, by = "ID")

