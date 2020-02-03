#exploratory data analysis

#descriptive stats

#look at how well the trial was randomized xd
#harder since 44 -> 25 data points omegalul

c_dem <- read.csv("C:/Users/typer321/Documents/cgm_demographics_biost699.csv", header = TRUE, 
                  sep = ",",fileEncoding="UTF-8-BOM")

#do counts of male and toher stuff lol (19-26 ranodmization we'll see)
#questin the eligibliity crteria and stuff


#randmozid in blocks lol

#randomziation criteria stuff
# stuff unequal amount of subjects on drug

tester <- merge_attempt_2 %>% filter(drug_type != 'y')
length(unique(tester$ID))
