library(tidyverse)
library(broom)
library(glmnet)

#ANOVA with AUC data
at1dt <- aov(n_auc_at_1 ~ drug_type ,data = merge_attempt_5)
at2dt <- aov(n_auc_at_2 ~ drug_type ,data = merge_attempt_5)
bt1dt <- aov(n_auc_bt_1 ~ drug_type ,data = merge_attempt_5)
bt2dt <- aov(n_auc_bt_2 ~ drug_type ,data = merge_attempt_5)

summary(at1d)
summary(at2dt)
summary(bt1dt)
summary(bt2dt)

#time below results (scaled)
at1t <- aov(time_at_1 ~ drug_type ,data = merge_attempt_5)
at2t <- aov(time_at_2 ~ drug_type ,data = merge_attempt_5)
bt1t <- aov(time_bt_1 ~ drug_type ,data = merge_attempt_5)
bt2t <- aov(time_bt_2 ~ drug_type ,data = merge_attempt_5)

summary(at1t)
summary(at2t)
summary(bt1t)
summary(bt2t)

#other misc. comparisons using bt_1

at1dt <- aov(time_bt_1 ~ drug_type ,data = merge_attempt_5)

#only bt1dt seeems to be sig

#model formation:
#Lmm? with time_at_1, n_auc_bt_1, sex, race, ethnicity, age, baseline height, bmi, treatment group?
#other things
#response: either time_bt_1, or mean_gl, or n_auc_bt_1
library(lme4)

#variable recoding here
#drug y = 0, x = 1
#sex 0 = male, 1 = female
#race 0 = other, 1 = white
#ethnicity 0 non-latino, 1= latino/hispanic

#intercept only for now
test_model <- lmer(formula = var_gl ~ 1 + mean_gl + n_auc_bt_1 + Sex + Age + Ethnicity + drug_type +
                   (1|ID) , data = merge_attempt_6) 
#diagnostics
plot(fitted(model5), resid(model5, type = "pearson"))# this will create the plot
abline(0,0, col="red")

#model5<-lmer(formula = popular ~ 1 + sex + extrav + texp+ extrav:texp + (1 + extrav | class), 
#             data    = popular2data)

#qq plot for fixed effect 
qqnorm(resid(model5)) 
qqline(resid(model5), col = "red") # add a perfect fit line

#qq plot for random effects
qqnorm(ranef(model5)$class[,1] )
qqline(ranef(model5)$class[,1], col = "red")

#beta regression
#percent time (0-1)
library(betareg)
#analagous to GLM

#transform, since our data contains a lot of 0s zzzzzzzzzzzzZZZZ
n = as.integer(length(merge_attempt_6$time_bt_1))

for_beta_model <- merge_attempt_6 %>% 
  mutate(time_bt_1_tr = (time_bt_1 *(n-1) + 0.5)/ n)

beta_model <- betareg(formula = time_bt_1_tr ~ Sex + Age + Ethnicity + drug_type, 
                      data = for_beta_model) 
summary(beta_model)


logit_link <- glm(time_bt_1 ~ Sex + Age + Ethnicity + drug_type, 
                  family = binomial,
                  data = merge_attempt_6)
summary(logit_link)


#ridge regression if you have time lol



