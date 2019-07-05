library(ggplot2)
library(dplyr)

load("brfss2013.RData")

#research question 1
women_adt <- brfss2013 %>% filter(sex == 'Female' & X_age65yr == "Age 18 to 64" ) 

women_adt %>% 
  group_by(X_incomg, na.rm = T) %>%
  summarise(Percentage_of_High_Blood_Pressure = sum(bphigh4 %in% c('Yes', 'Yes, but female told only during pregnancy'))/sum(bphigh4 %in% c('Yes', 'Yes, but female told only during pregnancy', 'No', 'Told borderline high or pre-hypertensive'))) %>%
  arrange(desc(Percentage_of_High_Blood_Pressure))

#reserach question 2
women_adt %>% 
  group_by(X_incomg, na.rm = T) %>%
  summarise(Rate_of_Nervous = sum(mishopls %in% c('All', 'Most') & misrstls  %in% c('All', 'Most') & misdeprd %in% c('All', 'Most'))/sum(misrstls %in% c('All', 'Most', 'Some', "A little", "None") & mishopls %in% c('All', 'Most', 'Some', "A little", "None") & misdeprd %in% c('All', 'Most', 'Some', "A little", "None"))) %>%
  arrange(desc(Rate_of_Nervous))

#reserach question 3
women_adt_veg <- women_adt %>% filter(fvgreen %in% c(101:120, 201:225))
ggplot(women_adt_veg, na.rm = TRUE,  aes(x =fvgreen , fill = X_incomg)) +
  geom_histogram(binwidth = 50) +
  labs(title = "Volume of Dark Green Vegetables Consumed",
       fill = "Income Level", 
       x = "Dark Green Vegetables",
       y = "Number of Women") +
  scale_fill_manual(values = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", '#253494')) 