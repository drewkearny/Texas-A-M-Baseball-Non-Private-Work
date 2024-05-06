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

# Trackman
trackman_21 <- read.csv("D:\\TruMedia Files\\Trackman_2021_All.csv")
trackman_22 <- read.csv("D:\\TruMedia Files\\Trackman_2022_All.csv")
trackman_23 <- read.csv("D:\\TruMedia Files\\Trackman_2023_All.csv")


# TruMedia Files
a21 <- read.csv("D:\\TruMedia Files\\2021-2.csv")
b21 <- read.csv("D:\\TruMedia Files\\2021-3.csv")
c21 <- read.csv("D:\\TruMedia Files\\2021-4.csv")
d21 <- read.csv("D:\\TruMedia Files\\2021-5.csv")
a22 <- read.csv("D:\\TruMedia Files\\2022-2.csv")
b22 <- read.csv("D:\\TruMedia Files\\2022-3.csv")
c22 <- read.csv("D:\\TruMedia Files\\2022-4.csv")
d22 <- read.csv("D:\\TruMedia Files\\2022-5.csv")
e22 <- read.csv("D:\\TruMedia Files\\2022-6.csv")
a23 <- read.csv("D:\\TruMedia Files\\2023-2.csv")
b23 <- read.csv("D:\\TruMedia Files\\2023-3.csv")
c23 <- read.csv("D:\\TruMedia Files\\2023-4.csv")
d23 <- read.csv("D:\\TruMedia Files\\2023-5.csv")

# Get Each Year of TruMedia Data
full2021 <- rbind(a21, b21, c21, d21)
full2022 <- rbind(a22, b22, c22, d22, e22)
full2023 <- rbind(a23, b23, c23, d23)

# Filter the data
full2021 <- full2021 %>% 
  rename(PitchUID = trackmanPitchUID) %>% 
  select(gameDate, seasonYear, trackmanGameID, PitchUID, manOnFirst, manOnSecond, manOnThird,
         atBatResult, exitVelocity, launchAngle, pitchingTeamName, battingTeamName)

full2022 <- full2022 %>% 
  rename(PitchUID = trackmanPitchUID) %>% 
  select(gameDate, seasonYear, trackmanGameID, PitchUID, manOnFirst, manOnSecond, manOnThird,
         atBatResult, exitVelocity, launchAngle, pitchingTeamName, battingTeamName)

full2023 <- full2023 %>% 
  rename(PitchUID = trackmanPitchUID) %>% 
  select(gameDate, seasonYear, trackmanGameID, PitchUID, manOnFirst, manOnSecond, manOnThird,
         atBatResult, exitVelocity, launchAngle, pitchingTeamName, battingTeamName)

# Merge Each Year with our trackman Data
merged21 <- merge(trackman_21, full2021, by = "PitchUID", all = FALSE)
merged22 <- merge(trackman_22, full2022, by = "PitchUID", all = FALSE)
merged23 <- merge(trackman_23, full2023, by = "PitchUID", all = FALSE)


# Filter The data
merged21$gameDate <- as.Date(merged21$gameDate, format = "%Y-%m-%d")
merged21$`Top.Bottom` <- factor(merged21$`Top.Bottom`, levels = c('Top', 'Bottom'))

merged21 <- merged21 %>% 
  arrange(gameDate, trackmanGameID, Inning, `Top.Bottom`, Outs, PitchNo)

# Runs Scored Rest of the Inning
merged21 <- merged21 %>% 
  group_by(trackmanGameID, Inning, `Top.Bottom`) %>%
  mutate(
    RunsScoredFromHere = cumsum(RunsScored),  # Runs scored from this row forward
  ) %>%
  ungroup() 

merged21 <- merged21 %>%
  group_by(gameDate, trackmanGameID, Inning, `Top.Bottom`) %>%
  mutate(
    RunsScoredInInning = max(RunsScoredFromHere) - RunsScoredFromHere + RunsScored
  ) %>%
  ungroup()

merged21 <- merged21 %>% 
  rename(runs_rest_of_inn = RunsScoredInInning) %>% 
  select(-RunsScoredFromHere)

merged21 <- merged21 %>% 
  arrange(gameDate, trackmanGameID, Inning, `Top.Bottom`, Outs, PitchNo)

merged21 <- merged21 %>% 
  select(gameDate, runs_rest_of_inn, manOnFirst, manOnSecond, manOnThird
         , Outs, Balls, Strikes, everything())

# 2022
merged22$gameDate <- as.Date(merged22$gameDate, format = "%Y-%m-%d")
merged22$`Top.Bottom` <- factor(merged22$`Top.Bottom`, levels = c('Top', 'Bottom'))

merged22 <- merged22 %>% 
  arrange(gameDate, trackmanGameID, Inning, `Top.Bottom`, Outs, PitchNo)

# Runs Scored Rest of the Inning
merged22 <- merged22 %>% 
  group_by(trackmanGameID, Inning, `Top.Bottom`) %>%
  mutate(
    RunsScoredFromHere = cumsum(RunsScored),  # Runs scored from this row forward
  ) %>%
  ungroup() 

merged22 <- merged22 %>%
  group_by(gameDate, trackmanGameID, Inning, `Top.Bottom`) %>%
  mutate(
    RunsScoredInInning = max(RunsScoredFromHere) - RunsScoredFromHere + RunsScored
  ) %>%
  ungroup()

merged22 <- merged22 %>% 
  rename(runs_rest_of_inn = RunsScoredInInning) %>% 
  select(-RunsScoredFromHere)

merged22 <- merged22 %>% 
  arrange(gameDate, trackmanGameID, Inning, `Top.Bottom`, Outs, PitchNo)

merged22 <- merged22 %>% 
  select(gameDate, runs_rest_of_inn, manOnFirst, manOnSecond, manOnThird
         , Outs, Balls, Strikes, everything())
# 2023
merged23$gameDate <- as.Date(merged23$gameDate, format = "%Y-%m-%d")
merged23$`Top.Bottom` <- factor(merged23$`Top.Bottom`, levels = c('Top', 'Bottom'))

merged23 <- merged23 %>% 
  arrange(gameDate, trackmanGameID, Inning, `Top.Bottom`, Outs, PitchNo)

# Runs Scored Rest of the Inning
merged23 <- merged23 %>% 
  group_by(trackmanGameID, Inning, `Top.Bottom`) %>%
  mutate(
    RunsScoredFromHere = cumsum(RunsScored),  # Runs scored from this row forward
  ) %>%
  ungroup() 

merged23 <- merged23 %>%
  group_by(gameDate, trackmanGameID, Inning, `Top.Bottom`) %>%
  mutate(
    RunsScoredInInning = max(RunsScoredFromHere) - RunsScoredFromHere + RunsScored
  ) %>%
  ungroup()

merged23 <- merged23 %>% 
  rename(runs_rest_of_inn = RunsScoredInInning) %>% 
  select(-RunsScoredFromHere)

merged23 <- merged23 %>% 
  arrange(gameDate, trackmanGameID, Inning, `Top.Bottom`, Outs, PitchNo)

merged23 <- merged23 %>% 
  select(gameDate, runs_rest_of_inn, manOnFirst, manOnSecond, manOnThird
         , Outs, Balls, Strikes, everything())

# Getting Base and Count States
# 2021
merged21$manOnFirst <- as.character(merged21$manOnFirst)
merged21$manOnFirst[merged21$manOnFirst == "true"] <- "1"
merged21$manOnFirst[merged21$manOnFirst == "false"] <- "0"

merged21$manOnSecond <- as.character(merged21$manOnSecond)
merged21$manOnSecond[merged21$manOnSecond == "true"] <- "1"
merged21$manOnSecond[merged21$manOnSecond == "false"] <- "0"

merged21$manOnThird <- as.character(merged21$manOnThird)
merged21$manOnThird[merged21$manOnThird == "true"] <- "1"
merged21$manOnThird[merged21$manOnThird == "false"] <- "0"

# Create a new column with the combined values
merged21$base_state <- paste0(merged21$manOnFirst, merged21$manOnSecond, merged21$manOnThird)

merged21$base_state <- as.character(merged21$base_state)

merged21 <- merged21 %>% 
  select(base_state, everything())

merged21$base_state_wcount <- paste0(merged21$base_state, " ", merged21$Balls, merged21$Strikes)

merged21$base_state_wcount <- as.character(merged21$base_state_wcount)

merged21 <- merged21 %>% 
  select(base_state_wcount, everything())


# 2022
# Getting Base and Count States

merged22$manOnFirst <- as.character(merged22$manOnFirst)
merged22$manOnFirst[merged22$manOnFirst == "true"] <- "1"
merged22$manOnFirst[merged22$manOnFirst == "false"] <- "0"

merged22$manOnSecond <- as.character(merged22$manOnSecond)
merged22$manOnSecond[merged22$manOnSecond == "true"] <- "1"
merged22$manOnSecond[merged22$manOnSecond == "false"] <- "0"

merged22$manOnThird <- as.character(merged22$manOnThird)
merged22$manOnThird[merged22$manOnThird == "true"] <- "1"
merged22$manOnThird[merged22$manOnThird == "false"] <- "0"

# Create a new column with the combined values
merged22$base_state <- paste0(merged22$manOnFirst, merged22$manOnSecond, merged22$manOnThird)

merged22$base_state <- as.character(merged22$base_state)

merged22 <- merged22 %>% 
  select(base_state, everything())

merged22$base_state_wcount <- paste0(merged22$base_state, " ", merged22$Balls, merged22$Strikes)

merged22$base_state_wcount <- as.character(merged22$base_state_wcount)

merged22 <- merged22 %>% 
  select(base_state_wcount, everything())

# 2023
# Getting Base and Count States

merged23$manOnFirst <- as.character(merged23$manOnFirst)
merged23$manOnFirst[merged23$manOnFirst == "true"] <- "1"
merged23$manOnFirst[merged23$manOnFirst == "false"] <- "0"

merged23$manOnSecond <- as.character(merged23$manOnSecond)
merged23$manOnSecond[merged23$manOnSecond == "true"] <- "1"
merged23$manOnSecond[merged23$manOnSecond == "false"] <- "0"

merged23$manOnThird <- as.character(merged23$manOnThird)
merged23$manOnThird[merged23$manOnThird == "true"] <- "1"
merged23$manOnThird[merged23$manOnThird == "false"] <- "0"

# Create a new column with the combined values
merged23$base_state <- paste0(merged23$manOnFirst, merged23$manOnSecond, merged23$manOnThird)

merged23$base_state <- as.character(merged23$base_state)

merged23 <- merged23 %>% 
  select(base_state, everything())

merged23$base_state_wcount <- paste0(merged23$base_state, " ", merged23$Balls, merged23$Strikes)

merged23$base_state_wcount <- as.character(merged23$base_state_wcount)

merged23 <- merged23 %>% 
  select(base_state_wcount, everything())


#
#
# Creating The Run Expectancy Matrix
# Run Expectancy with Count
# 2021
run_expectancy_count21 <- merged21 %>%
  group_by(base_state_wcount, Outs) %>%
  summarise(
    total_runs = sum(runs_rest_of_inn),
    instances = n()
  ) %>%
  mutate(run_expectancy = total_runs / instances)

# Pivot the data to create the run expectancy matrix
run_expectancy_count_matrix21 <- run_expectancy_count21 %>%
  pivot_wider(
    names_from = Outs,
    values_from = run_expectancy,
    names_prefix = "Outs_"
  ) %>%
  group_by(base_state_wcount) %>%
  summarise_all(~ifelse(all(is.na(.)), NA, first(na.omit(.))))

run_expectancy_count_matrix21 <- run_expectancy_count_matrix21 %>% 
  select(-Outs_3, -total_runs, -instances)

# 2022
run_expectancy_count22 <- merged22 %>%
  group_by(base_state_wcount, Outs) %>%
  summarise(
    total_runs = sum(runs_rest_of_inn),
    instances = n()
  ) %>%
  mutate(run_expectancy = total_runs / instances)

# Pivot the data to create the run expectancy matrix
run_expectancy_count_matrix22 <- run_expectancy_count22 %>%
  pivot_wider(
    names_from = Outs,
    values_from = run_expectancy,
    names_prefix = "Outs_"
  ) %>%
  group_by(base_state_wcount) %>%
  summarise_all(~ifelse(all(is.na(.)), NA, first(na.omit(.))))

run_expectancy_count_matrix22 <- run_expectancy_count_matrix22 %>% 
  select(-total_runs, -instances)

# 2023
run_expectancy_count23 <- merged23 %>%
  group_by(base_state_wcount, Outs) %>%
  summarise(
    total_runs = sum(runs_rest_of_inn),
    instances = n()
  ) %>%
  mutate(run_expectancy = total_runs / instances)

# Pivot the data to create the run expectancy matrix
run_expectancy_count_matrix23 <- run_expectancy_count23 %>%
  pivot_wider(
    names_from = Outs,
    values_from = run_expectancy,
    names_prefix = "Outs_"
  ) %>%
  group_by(base_state_wcount) %>%
  summarise_all(~ifelse(all(is.na(.)), NA, first(na.omit(.))))

run_expectancy_count_matrix23 <- run_expectancy_count_matrix23 %>% 
  select(-Outs_3, -total_runs, -instances)

# Some final cleaning
run_expectancy_count_matrix23 <- run_expectancy_count_matrix23[-49,]

run_expectancy_count_matrix21 <- run_expectancy_count_matrix21[-52,]

# Saving run expectancy by year
write.xlsx(run_expectancy_count_matrix21, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\RunExpMatrix_Count21.xlsx")
write.xlsx(run_expectancy_count_matrix22, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\RunExpMatrix_Count22.xlsx")
write.xlsx(run_expectancy_count_matrix23, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\RunExpMatrix_Count23.xlsx")


#
# Calculate average matrix by Count For all years
run_expectancy_all_count <- rbind(merged23, merged22, merged21) %>%
  group_by(base_state_wcount, Outs) %>%
  summarise(
    total_runs = sum(runs_rest_of_inn),
    instances = n()
  ) %>%
  mutate(run_expectancy = total_runs / instances)

# Pivot the data to create the run expectancy matrix
run_expectancy_matrix_all_count <- run_expectancy_all_count %>%
  pivot_wider(
    names_from = Outs,
    values_from = run_expectancy,
    names_prefix = "Outs_"
  ) %>%
  group_by(base_state_wcount) %>%
  summarise_all(~ifelse(all(is.na(.)), NA, first(na.omit(.))))

run_expectancy_matrix_all_count <- run_expectancy_matrix_all_count %>% 
  select(-Outs_3, -total_runs, -instances)

run_expectancy_matrix_all_count <- run_expectancy_matrix_all_count[-49,]

run_expectancy_matrix_all_count <- run_expectancy_matrix_all_count[-52,]


# Saving average matrix
write.xlsx(run_expectancy_matrix_all_count, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\runexp_matrix_bycount.xlsx")

write.xlsx(merged21, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\merged21.xlsx")
write.xlsx(merged22, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\merged22.xlsx")
write.xlsx(merged23, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\merged23.xlsx")


