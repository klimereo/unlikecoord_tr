# Summary statistics
## Fillers

install.packages("vtable")
library(vtable)
st(fillers, digits = 3, out='latex',file='mytable1.tex')

median(fillers$grammatical)
median(fillers$ungrammatical)

## Unlike Category

library(dplyr)
install.packages('xtable')
library(xtable)

summary_stats <- lm_cat_1factor %>%
  group_by(condition) %>%
  summarise(mean = mean(rating),
            sd = sd(rating),
            min = min(rating),
            max = max(rating),
            median = median(rating),
            IQR = IQR(rating))

print(xtable(summary_stats, caption = "Summary statistics table"), 
      include.rownames = FALSE)


table(lm_cat_1factor$condition)

LCATM_ratings <- lm_cat_1factor$rating[lm_cat_1factor$condition == "LCATM"]
UCATM_ratings <- lm_cat_1factor$rating[lm_cat_1factor$condition == "UCATM"]
UCATU_ratings <- lm_cat_1factor$rating[lm_cat_1factor$condition == "UCATU"]
LCATU_ratings <- lm_cat_1factor$rating[lm_cat_1factor$condition == "LCATU"]

wilcox.test(LCATM_ratings, UCATM_ratings, paired = TRUE)

wilcox.test(UCATM_ratings, UCATU_ratings, paired = TRUE)
wilcox.test(UCATM_ratings, LCATU_ratings, paired = TRUE)

## Unlike Case

library(dplyr)
install.packages('xtable')
library(xtable)

summary_stats <- lm_case %>%
  group_by(condition) %>%
  summarise(mean = mean(rating),
            sd = sd(rating),
            min = min(rating),
            max = max(rating),
            median = median(rating),
            IQR = IQR(rating))

print(xtable(summary_stats, caption = "Summary statistics table"), 
      include.rownames = FALSE)


table(lm_case$condition)


LCASEM_ratings <- lm_case$rating[lm_case$condition == "LCASEM"]
UCASEM_ratings <- lm_case$rating[lm_case$condition == "UCASEM"]
UCASEU_ratings <- lm_case$rating[lm_case$condition == "UCASEU"]

wilcox.test(LCASEM_ratings, UCASEM_ratings, paired = TRUE)
wilcox.test(UCASEM_ratings, UCASEU_ratings, paired = TRUE)


## Additional comparisons

ts4ucatm_values <- lm_cat %>%
  filter(token_set == 4, function_match == 1, category_match == 0) %>%
  select(rating) %>%
  pull()

ts12ucatm_values <- lm_cat %>%
  filter(token_set == 12, function_match == 1, category_match == 0) %>%
  select(rating) %>%
  pull()

other_values <- lm_cat %>%
  filter(function_match == 1, category_match == 0, !(token_set %in% c(4, 12))) %>%
  select(rating) %>%
  pull()


mean(ts4ucatm_values)
median(ts4ucatm_values)
sd(ts4ucatm_values)
mean(ts12ucatm_values)
median(ts12ucatm_values)
sd(ts12ucatm_values)

mean(other_values)
median(other_values)



