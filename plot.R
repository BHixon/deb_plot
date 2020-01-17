library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(ggplot2)
library(openxlsx)

## In order to run this script, set the working directory to the `plot` folder containing this script, 
## the regression output, and the plots.

## Import Regression Output
table_2013_raw <- read.xlsx('Table 2 Logistic Models_PLan outcome_by year_vars for plot.xlsx', sheet = 3, startRow = 4)
table_2014_raw <- read.xlsx('Table 2 Logistic Models_PLan outcome_by year_vars for plot.xlsx', sheet = 4, startRow = 4)
table_2015_raw <- read.xlsx('Table 2 Logistic Models_PLan outcome_by year_vars for plot.xlsx', sheet = 5, startRow = 4)

## Select relevant covariates
# 2013
table_2013_DHMO <- table_2013_raw[,1:4] %>%
  rename(covariate = X1) %>%
  filter(covariate %in% c("Count members (2 vs 1) ","Count members (3 vs 1) ", "Count members (4+ vs 1) ", "Any female aged 19-44",
                   "Any member age 0-2", "Any member age 3-18", "Diabetes","Asthma", "HTN", "CKD", "Arthritis")) %>%
  mutate(choice = "DHMO", Year = 2013) 
  
table_2013_HDHP <- cbind(table_2013_raw[,1], table_2013_raw[,6:8]) %>%
  rename(covariate = "table_2013_raw[, 1]") %>%
  filter(covariate %in% c("Count members (2 vs 1) ","Count members (3 vs 1) ", "Count members (4+ vs 1) ", "Any female aged 19-44",
                          "Any member age 0-2", "Any member age 3-18", "Diabetes","Asthma", "HTN", "CKD", "Arthritis")) %>%
  mutate(choice = "HDHP", Year = 2013)

table_2013 <- rbind(table_2013_HDHP, table_2013_DHMO)

# 2014
table_2014_DHMO <- table_2014_raw[,1:4] %>%
  rename(covariate = X1) %>%
  filter(covariate %in% c("Count members (2 vs 1) ","Count members (3 vs 1) ", "Count members (4+ vs 1) ", "Any female aged 19-44",
                          "Any member age 0-2", "Any member age 3-18", "Diabetes","Asthma", "HTN", "CKD", "Arthritis")) %>%
  mutate(choice = "DHMO", Year = 2014) 

table_2014_HDHP <- cbind(table_2014_raw[,1], table_2014_raw[,6:8]) %>%
  rename(covariate = "table_2014_raw[, 1]") %>%
  filter(covariate %in% c("Count members (2 vs 1) ","Count members (3 vs 1) ", "Count members (4+ vs 1) ", "Any female aged 19-44",
                          "Any member age 0-2", "Any member age 3-18", "Diabetes","Asthma", "HTN", "CKD", "Arthritis")) %>%
  mutate(choice = "HDHP", Year = 2014)

table_2014 <- rbind(table_2014_HDHP, table_2014_DHMO)

# 2015
table_2015_DHMO <- table_2015_raw[,1:4] %>%
  rename(covariate = X1) %>%
  filter(covariate %in% c("Count members (2 vs 1) ","Count members (3 vs 1) ", "Count members (4+ vs 1) ", "Any female aged 19-44",
                          "Any member age 0-2", "Any member age 3-18", "Diabetes","Asthma", "HTN", "CKD", "Arthritis")) %>%
  mutate(choice = "DHMO", Year = 2015) 

table_2015_HDHP <- cbind(table_2015_raw[,1], table_2015_raw[,6:8]) %>%
  rename(covariate = "table_2015_raw[, 1]") %>%
  filter(covariate %in% c("Count members (2 vs 1) ","Count members (3 vs 1) ", "Count members (4+ vs 1) ", "Any female aged 19-44",
                          "Any member age 0-2", "Any member age 3-18", "Diabetes","Asthma", "HTN", "CKD", "Arthritis")) %>%
  mutate(choice = "HDHP", Year = 2015)

table_2015 <- rbind(table_2015_HDHP, table_2015_DHMO)

# Bring years together
table <- rbind(table_2013, table_2014, table_2015) %>%
  mutate(Year = as.character(Year))

## Plot
plot <- ggplot(table, aes(fill = Year, covariate)) +
  geom_bar(position = 'dodge', aes(weight = OR), color = "grey40", alpha = .5) +
  scale_fill_manual(values = c("lightblue", "lightcoral", "purple")) + 
  geom_errorbar(aes(ymin = LCL, ymax = UCL, width =.2), position = position_dodge(.9)) + 
  theme_bw() +
  coord_flip() +
  labs(y = "Odds Ratio", x = "Factor") + 
  scale_x_discrete(limits=c("Count members (2 vs 1) ","Count members (3 vs 1) ", "Count members (4+ vs 1) ", "Any female aged 19-44",
                            "Any member age 0-2", "Any member age 3-18", "Diabetes","Asthma", "HTN", "CKD", "Arthritis")) +
  facet_wrap(~choice)
ggsave(filename = 'plot.png')
