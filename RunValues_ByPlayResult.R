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
library(ggplot2)
library(reshape2)

##
# To do each year you would load each years run expectancy individually
run_expectancy <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\runexp_matrix__all_count.xlsx")
merged21 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\merged21.xlsx")
merged22 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\merged22.xlsx")
merged23 <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\merged23.xlsx")

# Change The Run expectancy frame we are reading from based on by year or combined
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

# Combining years
combined_years <- rbind(merged21, merged22, merged23)

merged21 <- merged21 %>% 
  arrange(gameDate, trackmanGameID, Inning, `Top.Bottom`, Outs, PitchNo)
merged22 <- merged22 %>% 
  arrange(gameDate, trackmanGameID, Inning, `Top.Bottom`, Outs, PitchNo)
merged23 <- merged23 %>% 
  arrange(gameDate, trackmanGameID, Inning, `Top.Bottom`, Outs, PitchNo)

merged21 <- merged21 %>% 
  select(-atBatResult, -CatcherThrowLocationConfidence, -CatcherThrowReleaseConfidence,
         -CatcherThrowCatchConfidence, -HitLandingConfidence, -HitLaunchConfidence, -PitchMovementConfidence
         , -PitchLocationConfidence, -PitchReleaseConfidence, -PlayID, -AutoHitType,
         -UTCDate, UTCTime, -UTCDateTime, -LocalDateTime, -GameUID, -ThrowPositionX,
         -ThrowPositionY, -ThrowPositionZ, -ThrowTrajectoryXc0, -ThrowTrajectoryXc1,
         -ThrowTrajectoryXc2, -ThrowTrajectoryYc0, -ThrowTrajectoryYc1, -ThrowTrajectoryYc2)

merged22 <- merged22 %>% 
  select(-atBatResult, -CatcherThrowLocationConfidence, -CatcherThrowReleaseConfidence,
         -CatcherThrowCatchConfidence, -HitLandingConfidence, -HitLaunchConfidence, -PitchMovementConfidence
         , -PitchLocationConfidence, -PitchReleaseConfidence, -PlayID, -AutoHitType,
         -UTCDate, UTCTime, -UTCDateTime, -LocalDateTime, -GameUID, -ThrowPositionX,
         -ThrowPositionY, -ThrowPositionZ, -ThrowTrajectoryXc0, -ThrowTrajectoryXc1,
         -ThrowTrajectoryXc2, -ThrowTrajectoryYc0, -ThrowTrajectoryYc1, -ThrowTrajectoryYc2)

merged23 <- merged23 %>% 
  select(-atBatResult, -CatcherThrowLocationConfidence, -CatcherThrowReleaseConfidence,
         -CatcherThrowCatchConfidence, -HitLandingConfidence, -HitLaunchConfidence, -PitchMovementConfidence
         , -PitchLocationConfidence, -PitchReleaseConfidence, -PlayID, -AutoHitType,
         -UTCDate, UTCTime, -UTCDateTime, -LocalDateTime, -GameUID, -ThrowPositionX,
         -ThrowPositionY, -ThrowPositionZ, -ThrowTrajectoryXc0, -ThrowTrajectoryXc1,
         -ThrowTrajectoryXc2, -ThrowTrajectoryYc0, -ThrowTrajectoryYc1, -ThrowTrajectoryYc2)

combined_years <- combined_years %>% 
  select(-atBatResult, -CatcherThrowLocationConfidence, -CatcherThrowReleaseConfidence,
         -CatcherThrowCatchConfidence, -HitLandingConfidence, -HitLaunchConfidence, -PitchMovementConfidence
         , -PitchLocationConfidence, -PitchReleaseConfidence, -PlayID, -AutoHitType,
         -UTCDate, UTCTime, -UTCDateTime, -LocalDateTime, -GameUID, -ThrowPositionX,
         -ThrowPositionY, -ThrowPositionZ, -ThrowTrajectoryXc0, -ThrowTrajectoryXc1,
         -ThrowTrajectoryXc2, -ThrowTrajectoryYc0, -ThrowTrajectoryYc1, -ThrowTrajectoryYc2)


write.xlsx(merged21, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\merged21.xlsx")
write.xlsx(merged22, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\merged22.xlsx")
write.xlsx(merged23, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\merged23.xlsx")
write.xlsx(combined_years, "C:\\Users\\kearn\\R Projects\\Aggie Analymemtics\\Baseball_Analytics\\Combined.xlsx")

# Get the run exp changeval for each play for type of
# event and divide by total number of those events
# Use Pitch modeling by sam walsh
# Filter out rows with "Undefined" in the "PlayResult" column
filtered_data <- combined_years %>%
  filter(PlayResult != "Undefined")

Undefined_Play <- combined_years %>%
  filter(PlayResult == "Undefined")

# Undefined Play For count based ONLY
Undefined_Play <- Undefined_Play %>% 
  filter(PitchCall != "Undefined") %>% 
  filter(PitchCall != "NA")

filtered_data$PlayResult <- ifelse(filtered_data$PlayResult == "OUt", "Out", filtered_data$PlayResult)
filtered_data$PlayResult <- ifelse(filtered_data$PlayResult == "Fielderschoice", "FieldersChoice", filtered_data$PlayResult)
filtered_data$PlayResult <- ifelse(filtered_data$PlayResult == "Homerun", "HomeRun", filtered_data$PlayResult)
filtered_data$PlayResult <- ifelse(filtered_data$PlayResult == "GroundBall", "Out", filtered_data$PlayResult)
filtered_data$PlayResult <- ifelse(filtered_data$PlayResult == "FlyBall", "Out", filtered_data$PlayResult)
filtered_data$PlayResult <- ifelse(filtered_data$PlayResult == "Popup", "Out", filtered_data$PlayResult)

#
# ONLY USE For count based
#
filtered_data$PlayResult <- paste0(filtered_data$PlayResult, " ", filtered_data$Balls, "-", filtered_data$Strikes)

##
#
# Combining fielders choice and Outs
filtered_data_2 <- filtered_data %>%
  filter(PlayResult %in% c("Out", "FieldersChoice"))

Out_with_fieldersChoice <- filtered_data_2 %>%
  summarize(Run_Value = sum(run_exp_changeVal) / n())

# All Outs

# Sacrifice Fly
Sacrifice <- filtered_data %>%
  filter(PlayResult == "Sacrifice") %>% 
  filter(TaggedHitType != "Bunt")

Sac_fly <- Sacrifice %>% 
  group_by(PlayResult) %>% 
  summarize(Run_Value = sum(run_exp_changeVal) / n())

# Strikeout Run Values
Strikeout <- Undefined_Play %>% 
  filter(KorBB == "Strikeout")

Strikeouts <- Strikeout %>% 
  summarize(Run_Value = sum(run_exp_changeVal) / n())

# All outs Run values
all_outs <- rbind(filtered_data_2, Sacrifice, Strikeout)
  
all_outs_rv <- all_outs %>% 
  summarize(Run_Value = sum(run_exp_changeVal) / n())

# All Outs Column
new_column_data <- data.frame(
  PlayResult = c("All Out Types")
)

all_outs_rv <- cbind(new_column_data, all_outs_rv)

# Calculate the average of "run_value" for each group
result_inplay <- filtered_data %>%
  group_by(PlayResult) %>% 
  summarize(Run_Value = sum(run_exp_changeVal) / n()) %>% 
  ungroup()
# Not count based
result_inplay <- result_inplay[-1,]
# For count based
result_inplay <- result_inplay[-(1:12),]


Undefined_Play$PitchCall <- ifelse(Undefined_Play$PitchCall == "ballCalled", "BallCalled", Undefined_Play$PitchCall)
Undefined_Play$PitchCall <- ifelse(Undefined_Play$PitchCall == "SwinginStrike", "StrikeSwinging", Undefined_Play$PitchCall)
Undefined_Play$PitchCall <- ifelse(Undefined_Play$PitchCall == "StriekSwinging", "StrikeSwinging", Undefined_Play$PitchCall)

# Count Bases
Undefined_Play$PitchCall <- paste0(Undefined_Play$PitchCall, " ", Undefined_Play$Balls, "-", Undefined_Play$Strikes)

# For Count based ONLY
result_NOTinplay <- Undefined_Play %>%
  group_by(PitchCall) %>% 
  summarise(Run_Value = sum(run_exp_changeVal) / n()) %>% 
  ungroup()
result_NOTinplay <- result_NOTinplay[-13,]
result_NOTinplay <- result_NOTinplay[-(37:42),]
result_NOTinplay <- result_NOTinplay[-(61:67),]

# Hit by Pitch for non count based
hbp <- Undefined_Play %>% 
  filter(PitchCall == "HitByPitch")

HitByPitch <- hbp %>% 
  group_by(PitchCall) %>% 
  summarise(Run_Value = sum(run_exp_changeVal) / n())

# K or BB for non count based
KorWalk <- Undefined_Play %>% 
  filter(KorBB %in% c("Walk", "Strikeout"))

# Change to mean to fix walks
Strikeout_Walk <- KorWalk %>% 
  group_by(KorBB) %>% 
  summarize(Run_Value = sum(run_exp_changeVal) / n()) %>% 
  ungroup()

# Cleaning Up the Data Frame
names(result_NOTinplay)[names(result_NOTinplay) == "PitchCall"] <- "PlayResult"

# Run Values in General without combining Out Types
Run_values <- rbind(result_inplay, result_NOTinplay)
Run_values$PlayResult <- ifelse(substr(Run_values$PlayResult, 1, 3) == "Out", paste("Out In Play", substr(Run_values$PlayResult, 4, nchar(Run_values$PlayResult))), Run_values$PlayResult)

Run_values <- Run_values[-183,]
Run_values <- Run_values %>% arrange(Run_Value)

write.xlsx(Run_values, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\RE288Values.xlsx")
Run_values <- readxl::read_excel("C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\RE288Values.xlsx")

mean(Run_values$Run_Value[c(192:196,198:199,201:202)])

# New run values for getting xWOBA by combining Outs
names(HitByPitch)[names(HitByPitch) == "PitchCall"] <- "PlayResult"

names(Strikeout_Walk)[names(Strikeout_Walk) == "KorBB"] <- "PlayResult"

Sac_fly$PlayResult <- ifelse(Sac_fly$PlayResult == "Sacrifice", "Sacrifice Fly (Non Bunts)", Sac_fly$PlayResult)

new_Run_values <- rbind(result_inplay, HitByPitch, Strikeout_Walk, Sac_fly, all_outs_rv)
write.xlsx(new_Run_values, "C:\\Users\\kearn\\R Projects\\Aggie Analytics\\Baseball_Analytics\\RunValuesAllOuts.xlsx")




##
# Correlation Matrix

numeric_vars <- combined_years[,sapply(combined_years, is.numeric)]

Ptype <- combined_years[, 31]

numeric_vars <- numeric_vars[, -c(36:111)]
numeric_vars <- numeric_vars[, -c(3:14)]
numeric_vars <- numeric_vars[, -c(20:23)]


#numeric_vars['ABS_Horizontal'] = abs(numeric_vars['HorzBreak'])
#numeric_vars['differential_break'] = abs(numeric_vars['InducedVertBreak'] - numeric_vars['ABS_Horizontal'])
numeric_vars['ABS_RelSide'] = abs(numeric_vars['RelSide'])
numeric_vars['ABS_HorzBreak'] = abs(numeric_vars['HorzBreak'])
numeric_vars['ABS_HorzApprAngle'] = abs(numeric_vars['HorzApprAngle'])

numeric_vars <- cbind(numeric_vars, Ptype)

dfb3 = numeric_vars[numeric_vars$TaggedPitchType %in% c('Fastball', 'Sinker', 'TwoSeamFastBall', 'FourSeamFastBall', 'OneSeamFastBall'),]
dsl3 = numeric_vars[numeric_vars$TaggedPitchType %in% "Slider",]
dcb3 = numeric_vars[numeric_vars$TaggedPitchType %in% "Curveball",]
dch3 = numeric_vars[numeric_vars$TaggedPitchType %in% c("ChangeUp", "Splitter"),]


dfb3 <- dfb3[, -23]
dsl3 <- dsl3[, -23]
dcb3 <- dcb3[, -23]
dch3 <- dch3[, -23]

options(scipen = 999)

fbcorrelation_matrix <- cor(dfb3[,], method = "pearson", use = "pairwise.complete.obs")
slcorrelation_matrix <- cor(dsl3[,], method = "pearson", use = "pairwise.complete.obs")
cbcorrelation_matrix <- cor(dcb3[,], method = "pearson", use = "pairwise.complete.obs")
chcorrelation_matrix <- cor(dch3[,], method = "pearson", use = "pairwise.complete.obs")


# For Fastballs
correlation_df <- melt(fbcorrelation_matrix)
# Create a heatmap with correlation values
heatmap_plot <- ggplot(correlation_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap For Fastballs", x = "", y = "") +
  geom_text(aes(label = sprintf("%.3f", value)), vjust = 1) +
  theme(legend.position = "right")
heatmap_plot

# For Sliders
correlation_df <- melt(slcorrelation_matrix)
# Create a heatmap with correlation values
heatmap_plot <- ggplot(correlation_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap For Sliders", x = "", y = "") +
  geom_text(aes(label = sprintf("%.3f", value)), vjust = 1) +
  theme(legend.position = "right")
heatmap_plot

# For Curveballs
correlation_df <- melt(cbcorrelation_matrix)
# Create a heatmap with correlation values
heatmap_plot <- ggplot(correlation_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap For Curveballs", x = "", y = "") +
  geom_text(aes(label = sprintf("%.3f", value)), vjust = 1) +
  theme(legend.position = "right")
heatmap_plot

# For Changeups
correlation_df <- melt(chcorrelation_matrix)
# Create a heatmap with correlation values
heatmap_plot <- ggplot(correlation_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap For Changeups", x = "", y = "") +
  geom_text(aes(label = sprintf("%.3f", value)), vjust = 1) +
  theme(legend.position = "right")
heatmap_plot

dev.off()
# Format the matrix to display in non-scientific notation
formatted_matrix <- format(correlation_matrix, scientific = FALSE)
formatted_matrix

correlation_value <- formatted_matrix[,1]
correlation_value
