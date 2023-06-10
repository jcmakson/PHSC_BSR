# loading packages 

library(tidyverse)
library(lubridate)
library(unibeCols)
library(stats4)
library(esquisse)
library(gtsummary)
library(ggplot2)
library(dplyr)
library(knitr)
library(ggcorrplot)
library(epitools)

# Summary of data 

my_data_processed <- read_csv("~/Documents/PHSC_BSR/data/processed/my_data_processed.csv", 
                              col_types = cols(gender = col_factor(levels = c()), 
                                               result = col_factor(levels = c())))
 my_data_processed %>%
   dplyr::select(gender, pan_day, demo_group, test_id, drive_thru_ind, ct_result, orderset,
                 payor_group, col_rec_tat, rec_ver_tat, result) %>%
   tbl_summary(by = gender, type = c(rad_num = "continuous", logical = "categorical")) %>%
   add_overall() %>%
   add_p() %>%
   add_n() %>%
   modify_header(p.value ~ "Proportion") %>%
   as_gt()
 
 
 ## Plot_line
 
 ### Age distribution of the population considering the gender 
 
 my_data_processed %>%
 ggplot(aes(y = age, x = gender, fill = gender)) + 
   geom_boxplot() + 
   theme_bw()+
   labs(caption = "Fig. 1: Bareplot of Age by gender")
 
 ### Density of our population 
 
my_data_processed %>%
ggplot(my_data_processed, mapping = aes()) + 
  geom_histogram(mapping =  aes(x = age, y = after_stat(density), colour = gender, fill = gender ),
                  alpha = 0.4, bins = 100 ) +
  geom_density(mapping = aes(x = age , colour = gender), linewidth = 1.5 ) +
  theme(text = element_text(size=20), legend.position = "top") +
  xlab( "Age in year" ) + 
  scale_colour_manual(name = "" , values=c("female"=unibePastelS()[1],
                                           "male"=unibeIceS()[1]), labels = c("Female", "Male")) +
  scale_fill_manual(name = "", values=c("female"=unibePastelS()[1],
                                        "male"=unibeIceS()[1]), labels = c("Female", "Male")) +
  geom_vline(mapping = aes(xintercept = median(age)), color = unibeRedS()[1], linewidth = 1) +
  theme_bw()+
  labs(caption = "Fig. 2: Density of our population by Age by gender ")

### Generate a quantile-quantile (QQ) plot using.

my_data_processed %>%
  ggplot(mapping = aes(sample = age)) + 
  geom_qq_line(distribution = stats::qnorm) +
  geom_qq(color = unibePastelS()[1], distribution = stats::qnorm) + 
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme(text = element_text(size=20), legend.position = "top")+
  labs(caption = "Fig. 3: the Q.Q-plot of the study poulation")


### Test if the distribution is non normal by the Kolmogorov-Smirnov test

ks.test(my_data_processed$age, "pnorm")

my_data_processed %>% 
  mut

### Transformation of the skew left to look more normal

my_data_processed %>%
  ggplot(my_data_processed, mapping = aes()) + 
  geom_histogram(mapping =  aes(log(x = age), y = after_stat(density), colour = gender, fill = gender ),
                 alpha = 0.4, bins = 100 ) +
  geom_density(mapping = aes(log(x = age) , colour = gender), linewidth = 1.5 ) +
  theme(text = element_text(size=20), legend.position = "top") +
  xlab( "Age in year" ) + 
  scale_colour_manual(name = "" , values=c("female"=unibePastelS()[1],
                                           "male"=unibeIceS()[1]), labels = c("Female", "Male")) +
  scale_fill_manual(name = "", values=c("female"=unibePastelS()[1],
                                        "male"=unibeIceS()[1]), labels = c("Female", "Male")) +
  geom_vline(mapping = aes(xintercept = median(age)), color = unibeRedS()[1], linewidth = 1)+
  labs(caption = "Fig.4 Normalization of our data using the log function")


### Create correlation matrix of all numerical measurements and plot the matrix 

cor_matrix <- my_data_processed %>% 
  dplyr::select(pan_day,age, ct_result, col_rec_tat, rec_ver_tat) %>%
  cor(use = "complete.obs", method = "spearman")

cor_matrix %>%
  kable(digits = 2)

ggcorrplot(cor_matrix, lab=TRUE)+
  labs(caption= "Fig.5: the correlation matrix using quantitative variable")

### comparing two variables using Bland-Altman plot 

data <- data.frame(method1 =c(my_data_processed$age), 
                   method2 = c(my_data_processed$ct_result))

my_data_processed$Diff <- my_data_processed$method1 - my_data_processed$method2
my_data_processed$Avarage <- (my_data_processed$method1 + my_data_processed$method2)/2

ggplot(my_data_processed, aes(x = Average, y = Diff)) +
  geom_point() +
  geom_hline(yintercept = mean(my_data_processed$Diff), linetype = "dashed", color = "red") +
  geom_abline(intercept = mean(my_data_processed$Diff), slope = 0, linetype = "dotted", color = "black") +
  labs(x = "Average", y = "Difference") +
  theme_minimal()

#### Comparing two variable with geom_boplox, geom_point and geom_line

my_data_processed %>%
  ggplot(mapping =  aes(ct_result, age)) +
  geom_boxplot(width = 0.25, fill = "red") +
  geom_point(colour ="blue", size = 1) +
  geom_line(aes(group = subject_id), colour = "steelblue", linetype = "11") +
  theme_bw()+
  labs(caption = "Fig. 5 : Geom point and geomline comparing the age to result")

### Inference using proportions

## Create a dichotomic variable using my outcome (result): for this I assign the 1= "positive result" and  0 ="not positive results"

my_data_processed$outcomed <- 0
my_data_processed$outcomed [my_data_processed$result=="positive"] <-1
 my_data_processed$outcomed <- factor(my_data_processed$outcomed, levels= c(0,1), labels=c("Not positive", "Positive"))

## calculate the proportion of test result according to the gender 
 
 outcomed_t <-  table(my_data_processed$gender, my_data_processed$outcomed)
 outcomed_t 

 n_outcomed_p <- rowSums(outcomed_t)
 n_outcomed_p
 as.matrix(n_outcomed_p)
 
#outcomed_p <- n_outcomed_p [,1]/n_outcomed_p 
as.matrix(outcomed_p) 

## Calculate the 95% Confidence intervals (CI)

SE_outcomed <- sqrt(outcomed_p * (1- outcomed_p)/n_outcomed_p)
lowerCI_outcome_p <- outcomed_p - qnorm(0.975)* SE_outcomed
upperCI_outcome_p <- outcomed_p + qnorm(0.975)* SE_outcomed
cbind(outcomed_p, lowerCI_outcome_p, upperCI_outcome_p)

prop.test(outcomed_t[1,2],n=n_outcomed_p[1])

CI95_result <-prop.test(outcomed_t[1,2],n=n_outcomed_p[1])

names(CI95_result)
CI95_result$conf.int

## Sample test for equality of proportions with continuity  correction


chisq.test(cbind(outcomed_t[, 2], outcomed_t[, 1]))

## Pearson's Chi-squared test with Yates' continuity  correction



## Risk ratios and odds ratios: calculation of RR and OR for population to have the disease considering the gender

RR_outcome <- n_outcomed_p["female"] / n_outcomed_p["male"]
RR_outcome


riskratio(outcomed_t, correction= TRUE)

riskratio(outcomed_t, correction= TRUE, rev= "rows")
oddsratio(outcomed_t, correction = TRUE, rev = "rows")

# the risk of having a covid19 test positive when your are female is   1.060034 (95%-CI: 0.9311 1.2067) and the corresponding Odd Ration is  1.0636 (95%-CI:0.9272 - 1.2203)


