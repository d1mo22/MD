setwd('//wsl.localhost/Ubuntu-24.04/home/david/MD')
install.packages('tidyverse','mice',"")

library(dplyr)
library(mice)
library(tidyverse)
library(VIM)

?KNNimp()
dd <- read.csv("hotel_sample_5000.csv")
summary(dd)

# Remove all the blank spaces in the char columns
dd <- dd %>%
  mutate(across(where(is.character), trimws))

# NULL/Undefined --> NA
dd <- dd %>%
  mutate(meal = na_if(meal, "Undefined"))

dd <- dd %>%
  mutate(country = na_if(country, "NULL"))

dd <- dd %>%
  mutate(agent = na_if(agent, "NULL"))

dd <- dd %>%
  mutate(company = na_if(company, "NULL"))

# Verify
table(dd$country, useNA = "always")
table(dd$company, useNA = "always")
table(dd$meal, useNA = "always")

# See how many NA values we have in each column
dd %>%
  select(where(is.character)) %>%
  summarise(across(everything(), ~sum(is.na(.))))

dd <- subset(dd, select = -company)

# Use variables as factor to apply MICE
dd$country <- as.factor(dd$country)
dd$meal <- as.factor(dd$meal)
dd$agent <- as.factor(dd$agent)

# Little plot to see combination of missing values
md.pattern(dd)

# Set a random seed and run MICE with the to impute data
set.seed(123)
imputed_data <- mice(dd, m=10, method = "rf")

summary(imputed_data)
imputed_data$imp$country
finished_imputed_data <- complete(imputed_data,1)

md.pattern(finished_imputed_data)

