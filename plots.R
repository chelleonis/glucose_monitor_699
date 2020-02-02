#c_dem2 <- c_dem %>% select(c(ID,Treatment.group))
library(ggplot2)

summary(aov(auc_bt_2~drug_type, data=  merge_attempt_5))
summary(aov(time_bt_2~drug_type, data=  merge_attempt_5))

summary(lm(log(auc_bt_2+1) ~ Age, data = merge_attempt_5))


#50 and 29
id_50 <- merge_attempt_4 %>% filter(ID == 50) %>% mutate(dt_init = dt_init/24)
#by_id
test_plot <- ggplot(data = id_50, aes(x = dt_init, y = glucose, color = drug_type)) + 
  geom_line(aes(group = drug_type)) +
  labs(x = "Time (Days)", y = "Glucose Level", title = "Continuous Glucose Monitoring Plot for subject 50") +
  geom_hline(yintercept = 250, linetype="dashed", color = "darkred") +
  geom_hline(yintercept = 54, linetype="dashed", color = "darkred") +
  geom_hline(yintercept = 180, linetype="dashed", color = "orange") +
  geom_hline(yintercept = 70, linetype="dashed", color = "orange") +
  theme(plot.title = element_text(hjust = 0.5, size = 18), axis.text = element_text(size = 12), 
        axis.title = element_text(size = 15)) +
  scale_x_continuous(limits = c(0,14), breaks = c(0,7,14)) +
  scale_color_manual(values=c("#0000ff", "#00CC00"))

#wtih 2 lines, make another ver with 4 lines

#ADD MEAN WITHIN RANGE CATEGORICAL VARIABLE

test_plot

ggsave("hmm.png", plot = test_plot)

#test_plot2 <- ggplot(data = c_glu_separate, aes(x = delta_time_group, y = glucose, color = ID)) + 
#  geom_line(aes(group = ID))

#two group t-test usual assumptions: independent groups -> have to use a different method


ggplot(REGION1,
       aes(x = Region,
           y = Year2018,
           fill = Region)) +
  geom_bar(stat="identity") +
  coord_flip()
