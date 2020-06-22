# Dataset description https://stat.ethz.ch/R-manual/R-devel/library/survival/html/colon.html

library(tidyverse)
library(reshape2)
library(survival)
library(magrittr)


colon_data <- survival::colon

head(colon_data)

colon_data <- colon_data %>%
  mutate(three_year_survival=case_when(
    etype == 2 & time < 365*3 ~ 0, # Died
    TRUE ~ 1 # Survived
  ))

table(colon_data$three_year_survival)

# Now we want to determine the odds associated with three year survival based on therapy they got
manual_odds_table <- colon_data %>%
  group_by(rx, three_year_survival) %>%
  dplyr::summarise(count=n()) %>%
  dcast(rx ~ three_year_survival, value.var='count')

single_therapy_odds <- (manual_odds_table[2,'1']/manual_odds_table[2,'0'])/(manual_odds_table[1,'1']/manual_odds_table[1,'0'])
single_therapy_odds

combination_therapy_odds <- (manual_odds_table[3,'1']/manual_odds_table[3,'0'])/(manual_odds_table[1,'1']/manual_odds_table[1,'0'])
combination_therapy_odds

# How would we do it using R functions
model <- glm(three_year_survival ~ rx, data=colon_data, family = 'binomial')
summary(model)

# Extracting the values we want...
exp(coefficients(model))
exp(confint(model))

# Making a plot
combined_values <- as.data.frame(cbind((coefficients(model)), confint(model)))
colnames(combined_values) <- c('OR', colnames(confint(model)))
combined_values$variable <- row.names(combined_values) 

combined_values <- combined_values %>% filter(variable != '(Intercept)')

# Add a reference (OR=1)
combined_values <- rbind(
  combined_values, 
  data.frame('OR'=0, '2.5 %'=0, '97.5 %'=0, 'variable'='no therapy') %>%
    set_colnames(c('OR', '2.5 %', '97.5 %', 'variable'))
)

ggplot(combined_values, aes(variable, OR)) +
  geom_linerange(aes(ymin = `2.5 %`, ymax = `97.5 %`)) +
  geom_point(pch=21, fill='orange', size=5) +
  coord_flip()


