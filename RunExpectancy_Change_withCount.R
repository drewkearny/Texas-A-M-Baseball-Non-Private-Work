library(dplyr)
library(baseballr)
library(Lahman)
library(odbc)
library(baseballr)
library(tidyverse)
library(stringr)
library(rvest)
library(matrixStats)
library(openxlsx)

##
# To do each year you would load each years run expectancy individually

run_expectancy <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\runexp_matrix_bycount.xlsx")
merged21 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\merged21.xlsx")
merged22 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\merged22.xlsx")
merged23 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\merged23.xlsx")


merged21$run_exp <- ifelse(merged21$Outs == 0, run_expectancy$Outs_0[match(merged21$base_state_wcount, run_expectancy$base_state_wcount)],
                               ifelse(merged21$Outs == 1, run_expectancy$Outs_1[match(merged21$base_state_wcount, run_expectancy$base_state_wcount)],
                                      run_expectancy$Outs_2[match(merged21$base_state_wcount, run_expectancy$base_state_wcount)]))
merged22$run_exp <- ifelse(merged22$Outs == 0, run_expectancy$Outs_0[match(merged22$base_state_wcount, run_expectancy$base_state_wcount)],
                                    ifelse(merged22$Outs == 1, run_expectancy$Outs_1[match(merged22$base_state_wcount, run_expectancy$base_state_wcount)],
                                           run_expectancy$Outs_2[match(merged22$base_state_wcount, run_expectancy$base_state_wcount)]))
merged23$run_exp <- ifelse(merged23$Outs == 0, run_expectancy$Outs_0[match(merged23$base_state_wcount, run_expectancy$base_state_wcount)],
                                    ifelse(merged23$Outs == 1, run_expectancy$Outs_1[match(merged23$base_state_wcount, run_expectancy$base_state_wcount)],
                                           run_expectancy$Outs_2[match(merged23$base_state_wcount, run_expectancy$base_state_wcount)]))

merged21$gameDate <- as.Date(merged21$gameDate, format = "%Y-%m-%d")
merged21$`Top.Bottom` <- factor(merged21$`Top.Bottom`, levels = c('Top', 'Bottom'))
merged22$gameDate <- as.Date(merged22$gameDate, format = "%Y-%m-%d")
merged22$`Top.Bottom` <- factor(merged22$`Top.Bottom`, levels = c('Top', 'Bottom'))
merged23$gameDate <- as.Date(merged23$gameDate, format = "%Y-%m-%d")
merged23$`Top.Bottom` <- factor(merged23$`Top.Bottom`, levels = c('Top', 'Bottom'))

# Compute Change In run expectancy values caused by each play
merged21 <- merged21 %>%
  group_by(gameDate, trackmanGameID, Inning, `Top.Bottom`) %>%
  mutate(run_exp_changeVal = lead(run_exp) - run_exp) %>%
  ungroup() %>%
  mutate(run_exp_changeVal = run_exp_changeVal + RunsScored)

# Order the frame and replace the values for the last play of each inning
merged21 <- merged21 %>% 
  select(run_exp_changeVal, run_exp, Outs, Balls, Strikes, PlayResult, everything())
merged21$run_exp_changeVal <- ifelse(is.na(merged21$run_exp_changeVal), -merged21$run_exp, merged21$run_exp_changeVal)

merged22 <- merged22 %>%
  group_by(gameDate, trackmanGameID, Inning, `Top.Bottom`) %>%
  mutate(run_exp_changeVal = lead(run_exp) - run_exp) %>%
  ungroup() %>%
  mutate(run_exp_changeVal = run_exp_changeVal + RunsScored)

merged22 <- merged22 %>% 
  select(run_exp_changeVal, run_exp, Outs, Balls, Strikes, PlayResult, everything())
merged22$run_exp_changeVal <- ifelse(is.na(merged22$run_exp_changeVal), -merged22$run_exp, merged22$run_exp_changeVal)

merged23 <- merged23 %>%
  group_by(gameDate, trackmanGameID, Inning, `Top.Bottom`) %>%
  mutate(run_exp_changeVal = lead(run_exp) - run_exp) %>%
  ungroup() %>%
  mutate(run_exp_changeVal = run_exp_changeVal + RunsScored)

merged23 <- merged23 %>% 
  select(run_exp_changeVal, run_exp, Outs, Balls, Strikes, PlayResult, everything())
merged23$run_exp_changeVal <- ifelse(is.na(merged23$run_exp_changeVal), -merged23$run_exp, merged23$run_exp_changeVal)


#
##
# Run Value 24
run_expectancy21 <- merged21 %>%
  group_by(base_state, Outs) %>%
  summarise(
    total_runs = sum(runs_rest_of_inn),
    instances = n()
  ) %>%
  mutate(run_expectancy = total_runs / instances)

# Pivot the data to create the run expectancy matrix
run_expectancy_matrix21 <- run_expectancy21 %>%
  pivot_wider(
    names_from = Outs,
    values_from = run_expectancy,
    names_prefix = "Outs_"
  ) %>%
  group_by(base_state_wcount) %>%
  summarise_all(~ifelse(all(is.na(.)), NA, first(na.omit(.))))

run_expectancy_matrix21 <- run_expectancy_matrix21 %>% 
  select(-Outs_3, -total_runs, -instances)

# 2022
run_expectancy22 <- merged22 %>%
  group_by(base_state_wcount, Outs) %>%
  summarise(
    total_runs = sum(runs_rest_of_inn),
    instances = n()
  ) %>%
  mutate(run_expectancy = total_runs / instances)

# Pivot the data to create the run expectancy matrix
run_expectancy_matrix22 <- run_expectancy22 %>%
  pivot_wider(
    names_from = Outs,
    values_from = run_expectancy,
    names_prefix = "Outs_"
  ) %>%
  group_by(base_state_wcount) %>%
  summarise_all(~ifelse(all(is.na(.)), NA, first(na.omit(.))))

run_expectancy_matrix22 <- run_expectancy_matrix22 %>% 
  select(-total_runs, -instances)

# 2023
run_expectancy23 <- merged23 %>%
  group_by(base_state_wcount, Outs) %>%
  summarise(
    total_runs = sum(runs_rest_of_inn),
    instances = n()
  ) %>%
  mutate(run_expectancy = total_runs / instances)

# Pivot the data to create the run expectancy matrix
run_expectancy_matrix23 <- run_expectancy23 %>%
  pivot_wider(
    names_from = Outs,
    values_from = run_expectancy,
    names_prefix = "Outs_"
  ) %>%
  group_by(base_state_wcount) %>%
  summarise_all(~ifelse(all(is.na(.)), NA, first(na.omit(.))))

run_expectancy_matrix23 <- run_expectancy_matrix23 %>% 
  select(-Outs_3, -total_runs, -instances)


# All Years combined
run_expectancy_all <- rbind(merged23, merged22, merged21) %>%
  group_by(base_state_wcount, Outs) %>%
  summarise(
    total_runs = sum(runs_rest_of_inn),
    instances = n()
  ) %>%
  mutate(run_expectancy = total_runs / instances)

# Pivot the data to create the run expectancy matrix
run_expectancy_matrix_all <- run_expectancy_all %>%
  pivot_wider(
    names_from = Outs,
    values_from = run_expectancy,
    names_prefix = "Outs_"
  ) %>%
  group_by(base_state_wcount) %>%
  summarise_all(~ifelse(all(is.na(.)), NA, first(na.omit(.))))

run_expectancy_matrix_all <- run_expectancy_matrix_all %>% 
  select(-Outs_3, -total_runs, -instances)

# Some final cleaning
run_expectancy_matrix23 <- run_expectancy_matrix23[-49,]

run_expectancy_matrix21 <- run_expectancy_matrix21[-52,]

# Saving run expectancy by year
write.xlsx(run_expectancy_matrix21, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\RunExpMatrix21.xlsx")
write.xlsx(run_expectancy_matrix22, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\RunExpMatrix22.xlsx")
write.xlsx(run_expectancy_matrix23, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\RunExpMatrix23.xlsx")
write.xlsx(run_expectancy_matrix_all, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\RunExpMatrixCombined.xlsx")


# Saving average matrix
write.xlsx(average_runExpectancy, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\runexp_matrix_bycount.xlsx")
