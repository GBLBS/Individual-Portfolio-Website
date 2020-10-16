---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
description: Nullam et orci eu lorem consequat tincidunt vivamus et sagittis magna sed nunc rhoncus condimentum sem. In efficitur ligula tate urna. Maecenas massa sed magna lacinia magna pellentesque lorem ipsum dolor. Nullam et orci eu lorem consequat tincidunt. Vivamus et sagittis tempus.
draft: false
image: pic08.jpg
keywords: ""
slug: tempus
title: Exploratory Data Analysis 
---

The first step of the study was to analyze the weight of participants, in kilograms. 
Therefore, we used visualization and summary statistics tp describe the distribution of weights. 

```{r, eda_on_weight}


#Distribution including missing values. 
ggplot(yrbss, aes(x = weight)) +
  geom_density(alpha = 0.2) + #density plot with tranparency set to 20%
  theme_bw() +                #theme
  labs ( x="Weight", 
         title = "Weight Distribution",
         y = "Density" 
  )

#Removing the NAs from the data.
yrbss_NA_removed <- yrbss %>% 
  select(weight) %>% #selecting the weight column
  na.omit() #removing NAs
skim(yrbss_NA_removed) #confirms that there are no missing values
  
#Distribution without NA values. 
ggplot(yrbss_NA_removed, aes(x = weight)) +
  geom_density(alpha = 0.2) + #density plot with tranparency set to 20%
  theme_bw() +                #theme
  labs ( x="Weight", title = "Weight Distribution (NAs removed)",
    y     = "Density" 
  ) #assigning axis titles and main title

```

image: 1A.png



