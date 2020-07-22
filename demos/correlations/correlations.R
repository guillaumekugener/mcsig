library(tidyverse)
library(magrittr)
library(ggpubr)

# Load data
covid_data <- read_csv('~/Downloads/COVID swab data - Sheet1.csv') %>%
  mutate(Type=factor(Type, levels=c('Tongue', 'Nasal', 'Mid-Turbinate')))

ggplot(covid_data, aes(`Other Swab`, `Nasopharyngeal Swab`)) +
  geom_point() +
  facet_wrap(~Type)

# Compute correlation for a single group here
tongue_only <- covid_data %>%
  filter(Type=='Tongue')

cor.test(
  tongue_only$`Other Swab`,
  tongue_only$`Nasopharyngeal Swab`,
  method='pearson' # We can specify what method we may want to use instead
)

# Can do all the correlations at once

individual_correlations <- covid_data %>%
  group_by(Type) %>%
  dplyr::summarise(
    correlation=cor.test(`Other Swab`, `Nasopharyngeal Swab`, method='pearson')$estimate
  )

ggplot(covid_data, aes(`Other Swab`, `Nasopharyngeal Swab`)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor() +
  facet_wrap(~Type)



