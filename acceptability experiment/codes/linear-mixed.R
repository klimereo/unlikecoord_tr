# Reading Data

lm_cat <- read.csv('lm_cat.csv')
lm_case <- read.csv('lm_case.csv')
lm_cat_1factor <- read.csv('cat_basic.csv')
fillers <- read.csv("fillers.csv")

# Install/load necessary packages and dependencies
install.packages("lme4", dependencies = TRUE)
install.packages("Matrix")
install.packages("sjPlot")
install.packages("lmerTest", dependencies = TRUE)
install.packages("ggpubr")
install.packages("stargazer")
install.packages("lattice")
instal.packages("emmeans")
library(ggpubr)
library(emmeans)
library(sjPlot)
library(sjmisc)
library(lme4)
library(lmerTest)
library(lattice)
library(stargazer)
library(optimx)


# 1 independent variable 4 levels CAT model

## Fitting the model

control <- lmerControl(optimizer = "bobyqa")

model_cat_basic = lmer(rating ~ condition + 
                      (1 + condition|participant_ID) +
                      (1 + condition|token_set), data=lm_cat_1factor, 
                       control = control)


## Model Summary

summary(model_cat_basic)
model_performance(model_cat_basic)
emm <- emmeans(model_cat_basic, ~ condition)
comp <- pairs(emm)
summary(comp)

# 2 independent variable with binary levels (no-interact) CAT model

## Convert function_match and category_match to factors
lm_cat$function_match <- factor(lm_cat$function_match)
lm_cat$category_match <- factor(lm_cat$category_match)

## Relevel the function_match and category_match variables
lm_cat$function_match <- relevel(lm_cat$function_match, ref = "1")
lm_cat$category_match <- relevel(lm_cat$category_match, ref = "1")

## Fit the modified model
model_cat <- lmer(rating ~ function_match + category_match + 
                    (function_match + category_match | participant_ID) +
                    (function_match + category_match | token_set), 
                     data = lm_cat)
## Model Summary
summary(model_cat)
model_performance(model_cat)


# 2 independent variable with binary levels (YES-interact) CAT model
lm_cat$function_match <- factor(lm_cat$function_match)
lm_cat$category_match <- factor(lm_cat$category_match)

## Relevel the function_match and category_match variables
lm_cat$function_match <- relevel(lm_cat$function_match, ref = "1")
lm_cat$category_match <- relevel(lm_cat$category_match, ref = "1")

ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000))

model_cat_interact <- lmer(rating ~ function_match * category_match + 
                    (function_match + category_match | participant_ID) +
                    (function_match + category_match | token_set), 
                     data=lm_cat,
                    control=ctrl)


summary(model_cat_interact)
model_performance(model_cat_interact)


# 1 Independent variable 3 levels CASE model

model_case = lmer(rating ~ condition + 
                    (1 + condition|participant_ID) +
                    (1 + condition|token_set), 
                     data=lm_case,
                     control=lmerControl(optimizer="bobyqa"))


summary(model_case)
model_performance(model_case)

## Tabular summaries of models

tab_model(model_case, file = "modelcase.doc")
tab_model(model_cat, file = "modelcat.doc")
tab_model(model_cat_interact, file = "modelcatinteract.doc")

