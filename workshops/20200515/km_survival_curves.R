library(tidyverse)
library(survival)
library(survminer)

ovarian <- read_csv('~/Documents/USC/cssig/ovarian_data.csv')

head(ovarian)

surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)
fit1 <- survfit(surv_object ~ treatment, data = ovarian)

ggsurvplot(fit1, data = ovarian, pval=TRUE)

