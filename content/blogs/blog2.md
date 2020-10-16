---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Analysis: Relationship between salary and gender
draft: false
image: omega.png
keywords: ""
slug: magna
title: Omega Case Study
---

Analysis: Relationship between salary and gender

---
Case 
---

The case situation, as provided by Kostis Christodoulou, was the following. 

At the last board meeting of Omega Group Plc., the headquarters of a large multinational company, the issue was raised that women were being discriminated in the company, in the sense that the salaries were not the same for male and female executives. A quick analysis of a sample of 50 employees (of which 24 men and 26 women) revealed that the average salary for men was about 8,700 higher than for women. This seemed like a considerable difference, so it was decided that a further analysis of the company salaries was warranted. 

You are asked to carry out the analysis. The objective is to find out whether there is indeed a significant difference between the salaries of men and women, and whether the difference is due to discrimination or whether it is based on another, possibly valid, determining factor. 

The data frame "omega"  contains the salaries for the sample of 50 executives in the company. Can you conclude that there is a significant difference between the salaries of the male and female executives?

---
Resolution 
---

Naturally, our first step was to load the data. 

```{r load_omega_data}
omega <- read_csv(here::here("data", "omega.csv"))
glimpse(omega) # examine the data frame
```

Then, we calculated summary statistics on salary by gender. We created and printed a dataframe where, for each gender was shown: mean, SD, sample size, the t-critical, the SE, the margin of error, and the low/high endpoints of a 95% confidence interval. 


```{r, confint_single_valiables}
# Summary Statistics of salary by gender
mosaic::favstats (salary ~ gender, data=omega)

# Dataframe with two rows (male-female) and having as columns gender, mean, SD, sample size, 
# the t-critical value, the standard error, the margin of error, 
# and the low/high endpoints of a 95% confidence interval

# create 95% CI for the male and female salaries 
formula_ci <- omega %>% 
  group_by(gender) %>% 
  summarise(mean_salary = mean(salary, na.rm=TRUE),
            sd_salary = sd(salary),
            count = n(),
            # get t-critical value with (n-1) degrees of freedom
            t_critical = qt(0.975, count-1),
            se_salary = sd(salary)/sqrt(count),
            margin_of_error = t_critical * se_salary,
            CI_low = mean_salary - margin_of_error,
            CI_high = mean_salary + margin_of_error)

# Printing out the CI in table format
formula_ci %>%
  kbl(col.names = c("Gender", "Mean Salary", "SD Salary", "Sample Size", "T-Value", "SE", "Margin of Error", "CI lower", "CI upper")) %>% 
  kable_classic(c("hover"), html_font = "Cambria") %>%
  kable_styling()

```

And indeed, by calculating the means and confidence intervals of the Omega's Executive salaries by gender, we observed that the confidence intervals of female and male executive's salaries do not overlap. Therefore, a significant difference between the salaries of male and female executives was concluded. 

