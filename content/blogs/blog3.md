---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
description: Analyzing weight distributions
draft: false
image: 1A.png
keywords: ""
slug: tempus
title: Exploratory Data Analysis 
---

The first step of the study was to analyze the weight of participants, in kilograms. 
Therefore, we used visualization and summary statistics to describe the distribution of weights. 

```{r, eda_on_weight}
# Distribution including missing values. 
ggplot(yrbss, aes(x = weight)) +
  geom_density(alpha = 0.2) + #density plot with tranparency set to 20%
  theme_bw() + #theme
  labs (x="Weight", 
         title = "Weight Distribution",
         y = "Density")

# Removing the NAs from the data.
yrbss_NA_removed <- yrbss %>% 
  select(weight) %>% #selecting the weight column
  na.omit() #removing NAs
skim(yrbss_NA_removed) #confirms that there are no missing values
  
#Distribution without NA values. 
ggplot(yrbss_NA_removed, aes(x = weight)) +
  geom_density(alpha = 0.2) + #density plot with tranparency set to 20%
  theme_bw() + #theme
  labs ( x="Weight", title = "Weight Distribution (NAs removed)",
    y = "Density") #assigning axis titles and main title
    
```

Then, we considered the possible relationship between a high schooler’s weight and their physical activity. 

We created a new variable "physical_3plus", which will be "yes"" if they are physically active for at least 3 days a week, and "no"" otherwise.

```{r}
yrbss <- yrbss %>% 
  mutate(physical_3plus = ifelse(physically_active_7d >= 3, "yes", "no"))

#Create overview with average and median weight for people with different physical activity.
yrbss_weight_physical_3plus <- yrbss %>% filter(!is.na(physical_3plus), !is.na(weight)) %>% 
  group_by(physical_3plus) %>% 
  summarise(
    count = n(),
    mean_weight = mean(weight),
    median_weight = median(weight)) %>% 
  mutate(prop= count/sum(count))

#Make table from overview created above
yrbss_weight_physical_3plus %>% 
  kbl(col.names = c("Physical_3plus", "Count","Mean Weight", "Median Weight", "Proportion")) %>%
  kable_material(c("striped", "hover")) %>%
  kable_styling(fixed_thead = T)

#remove NAs for plotting in order to spot trends.
yrbss_physical_3plus_NA_rmeoved <- yrbss %>% 
  filter(physical_3plus =="yes" | physical_3plus == "no")

#Boxplots to compare weights of people with different physical activity. 

ggplot(yrbss_physical_3plus_NA_rmeoved, aes(y = weight)) +
  
  #boxplot
  geom_boxplot() + 
  facet_wrap(~physical_3plus) +
  
  #theme
  theme_bw() +
  
  #assigning axis titles and main title
  labs ( x="", title = "Weight Distribution",
         y="Weight")

```

Third, we used the following code to provide a 95% confidence interval for the population proportion of high schools that are not active 3 or more days per week.

```{r, CI_prop_physical_3plus}
#Create the CI
yrbss_physical_3plus_prop <- yrbss %>% 
  
  #Remove NAs
  filter(!is.na(physical_3plus)) %>%  
  
  #Group
  group_by(physical_3plus) %>% 
  
  #sum of non NAs for physical_3plus
  summarise(
    count = n()) %>%
  mutate(
    #proportion yes and no
    prop= count/sum(count), 
    
    #formula for SE since conditions np and n(1-p) >= 10 are met
    SE = sqrt(prop*(1-prop)/count), 
    
    #assuming normal distribution
    critical_Z = qnorm(0.975), 
    lower_end = prop - critical_Z*SE, 
    higher_end = prop + critical_Z*SE) 

#Create table to present CI
yrbss_physical_3plus_prop %>% 
  filter(physical_3plus =="yes") %>% 
  select(physical_3plus,lower_end,higher_end) %>% 
  kbl(col.names = c("Physical_3plus", "Lower CI","Upper CI")) %>%
  kable_material(c("striped", "hover")) %>%
  kable_styling(fixed_thead = T)

```

