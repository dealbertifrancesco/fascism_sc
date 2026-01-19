rm(list = ls())

### Packages
library(fixest)
library(stats)
library(tableone)
library(survey)
library(ggplot2)

### Load data
setwd(here())
clean_data_dir <- here("data", "processed")

df_iv <- read.csv(file.path(clean_data_dir, "df_iv.csv"))

#### Matching
## Propensity Score
ps_model <- glm(ass1900s_d ~ lpop1911 + larea + centre_alt + max_alt +
                  I(lpop1911^2) + I(larea^2) + I(centre_alt^2) + I(max_alt^2) +
                  veterans + ind_workers + dlab + bourgeoisie + landlord_ass + literacy +
                  I(veterans^2) + I(ind_workers^2) + I(dlab^2) + I(bourgeoisie^2) +
                  I(landlord_ass^2) + I(literacy^2), 
                data = df_iv, 
                family = binomial)

df_iv$pscore <- ps_model$fitted.values
p_treated <- mean(df_iv$ass1900s_d)
p_control <- 1 - p_treated

df_iv <- df_iv %>%
  mutate(weight = ifelse(ass1900s_d == 1, 
                         p_treated / pscore,
                         p_control / (1 - pscore)))

## Diagnostics
ggplot(df_iv, aes(x = pscore, fill = as.factor(ass1900s_d))) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(fill = "Treatment Group") +
  theme_minimal()

library(tableone)
library(survey)
survey_design <- svydesign(ids = ~1, data = df_iv, weights = ~weight)
table1 <- svyCreateTableOne(vars = c("lpop1911", "larea", "centre_alt", "max_alt", 
                                     "veterans", "ind_workers", "dlab", "bourgeoisie", 
                                     "landlord_ass", "literacy"), 
                            strata = "ass1900s_d", 
                            data = survey_design, 
                            test = T)
print(table1)

dfl_1 <- feols(fascist_branch ~ ass1900s_d, 
                   data = df_iv%>%filter(pscore<0.9), 
                   weights = ~weight)
summary(dfl_1)

dfl_2 <- feols(fascist_violence ~ ass1900s_d, 
                   data = df_iv, 
                   weights = ~weight)
summary(dfl_2)

dfl_2_1 <- feols(fascist_violence ~ ass1900s_d+fascist1921_vv, 
               data = df_iv, 
               weights = ~weight)
summary(dfl_2_1)

dfl_3 <- feols(fascist1921_vv ~ ass1900s_d, 
                   data = df_iv, 
                   weights = ~weight)
summary(dfl_3)

dfl_4 <- feols(fascist1919_vv ~ ass1900s_d, 
               data = df_iv, 
               weights = ~weight)
summary(dfl_4)

dfl_5 <- feols(fascist1924_vv ~ ass1900s_d, 
               data = df_iv, 
               weights = ~weight)
summary(dfl_5)

dfl_6 <- feols(fascist_spread_21 ~ ass1900s_d, 
               data = df_iv, 
               weights = ~weight)
summary(dfl_6)

dfl_7 <- feols(fascist_spread_24 ~ ass1900s_d, 
               data = df_iv, 
               weights = ~weight)
summary(dfl_7)