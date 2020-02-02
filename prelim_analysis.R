#exploratory data analysis

#descriptive stats

#ANOVA with AUC data
at1dt <- aov(n_auc_at_1 ~ drug_type ,data = merge_attempt_5)
at2dt <- aov(n_auc_at_2 ~ drug_type ,data = merge_attempt_5)
bt1dt <- aov(n_auc_bt_1 ~ drug_type ,data = merge_attempt_5)
bt2dt <- aov(n_auc_bt_2 ~ drug_type ,data = merge_attempt_5)

summary(at1dt)
summary(at2dt)
summary(bt1dt)
summary(bt2dt)
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
