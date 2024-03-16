library(baseballr)
library(retrosheet)
library(ggplot2)
library(dplyr)
library(magrittr)

df <- read.csv("2023events.csv")
df$Date <- rep(as.Date("2023-01-01"), nrow(df))
df[4, 1]

for (i in 1:nrow(df)) {
  date_string <- substr(df[i, 1], 4, nchar(df[i, 1]) - 1)
  #print(date_string)
  formatted_date <- as.Date(date_string, format = "%Y%m%d")
  df[i, 98] <- formatted_date
}


game_outcome <- get_retrosheet("game", 2023)
# Filter rows where the date is later than April 7th
#game_outcome <- game_outcome[game_outcome$Date > as.Date("2023-03-0"), ]

df_b9_r23_1o <- df[df$INN_CT == 9 & df$OUTS_CT == 1 & df$BAT_HOME_ID == 1 & df$AWAY_SCORE_CT - df$HOME_SCORE_CT == 1 & df$BASE1_RUN_ID == "" & df$BASE2_RUN_ID != "" & df$BASE3_RUN_ID != "",]
df_t9_r0_0o <- df[df$INN_CT == 9 & df$OUTS_CT == 0 & df$BAT_HOME_ID == 0 & df$AWAY_SCORE_CT == df$HOME_SCORE_CT & df$BASE1_RUN_ID == "" & df$BASE2_RUN_ID == "" & df$BASE3_RUN_ID == "",]
df_t9_r3_0o <- df[df$INN_CT == 9 & df$OUTS_CT == 0 & df$BAT_HOME_ID == 0 & df$AWAY_SCORE_CT == df$HOME_SCORE_CT & df$BASE1_RUN_ID == "" & df$BASE2_RUN_ID == "" & df$BASE3_RUN_ID != "",]
df_b9_r2_02 <- df[df$INN_CT == 9 & df$OUTS_CT == 2 & df$BAT_HOME_ID == 1 & df$AWAY_SCORE_CT - df$HOME_SCORE_CT == 1 & df$BASE1_RUN_ID == "" & df$BASE2_RUN_ID != "" & df$BASE3_RUN_ID == "",]
df_b9_r2_01 <- df[df$INN_CT == 9 & df$OUTS_CT == 1 & df$BAT_HOME_ID == 1 & df$AWAY_SCORE_CT - df$HOME_SCORE_CT == 1 & df$BASE1_RUN_ID == "" & df$BASE2_RUN_ID != "" & df$BASE3_RUN_ID == "",]
df_b9_r12_01 <- df[df$INN_CT == 9 & df$OUTS_CT == 1 & df$BAT_HOME_ID == 1 & df$AWAY_SCORE_CT - df$HOME_SCORE_CT == 2 & df$BASE1_RUN_ID != "" & df$BASE2_RUN_ID != "" & df$BASE3_RUN_ID == "",]

df_t9_r0_0o$AwayWin <- rep(1)
substr(df_t9_r0_0o[3, 1], nchar(df_t9_r0_0o[3, 1]), nchar(df_t9_r0_0o[3, 1]))

for (i in 1:nrow(df_t9_r0_0o)) {
  awayteam <- df_t9_r0_0o[i, 2]
  dh <- substr(df_t9_r0_0o[i, 1], nchar(df_t9_r0_0o[i, 1]), nchar(df_t9_r0_0o[i, 1]))
  print(dh)
  print(awayteam)
  date <- df_t9_r0_0o[i, 98]
  print(date)
  selected_row <- game_outcome[game_outcome$Date == date & game_outcome$VisTm == awayteam, ]
  if (dh != 0) {
    selected_row <- selected_row[2, ]
    print("########################")
  }
  win <- selected_row[10] > selected_row[11]
  print(win)
  df_t9_r0_0o[i, 99] <- win
}

# 
# df_t9_r0_0o[df_t9_r0_0o$GAME_ID == "BOS202306032", 99] <- 1
# df_t9_r0_0o[df_t9_r0_0o$GAME_ID == "PIT202308132", 99] <- 0
# df_t9_r0_0o[df_t9_r0_0o$GAME_ID == "NYN202309272", 99] <- 1
# df_t9_r0_0o[df_t9_r0_0o$GAME_ID == "PHI202308082", 99] <- 1

# Create a pie chart
sum(df_t9_r0_0o$AwayWin)
unique_count <- length(unique(df_t9_r0_0o$GAME_ID))
unique_count
# Initialize counts for each category
counts <- c(W = 0, S = 0, D = 0, T = 0, HR = 0, E = 0, FC = 0,HP = 0, K = 0, Out = 0)

events <- df_t9_r0_0o$EVENT_TX

# Iterate through the rows of df
for (i in 1:length(events)) {
  # Get the first 1 or 2 letters/digits of the value in "EVENT_TX"
  prefix <- substr(events[i], 1, 2)
  print(prefix)
  # Update counts based on the prefix
  if (grepl("^W", prefix)) {
    counts["W"] <- counts["W"] + 1
  } else if (grepl("^HW", prefix)) {
    counts["HW"] <- counts["HW"] + 1
  } else if (grepl("^IW", prefix)) {
    counts["IW"] <- counts["IW"] + 1
  } else if (grepl("^S", prefix)) {
    counts["S"] <- counts["S"] + 1
  } else if (grepl("^D", prefix)) {
    counts["D"] <- counts["D"] + 1
  } else if (grepl("^T", prefix)) {
    counts["T"] <- counts["T"] + 1
  } else if (grepl("^E", prefix)) {
    counts["E"] <- counts["E"] + 1
  } else if (grepl("^FC", prefix)) {
    counts["FC"] <- counts["FC"] + 1
  } else if (grepl("^HP", prefix)) {
    counts["HP"] <- counts["HP"] + 1
  } else if (grepl("^K", prefix)) {
    counts["K"] <- counts["K"] + 1
  } else if (grepl("^\\d", prefix)) {  # Check if it starts with a digit
    counts["Out"] <- counts["Out"] + 1
  } else if (grepl("^HR", prefix)) {
    counts["HR"] <- counts["HR"] + 1
  }
}

# Print the counts
print(counts)
sum(counts)

#pie(counts, labels = names(counts), main = "Distribution of Counts")

# Convert counts and categories into a data frame
counts_df <- data.frame(Category = names(counts), Count = counts)

# Reorder levels for increasing order
counts_df$Category <- factor(counts_df$Category, levels = counts_df$Category[order(counts_df$Count)])

# Create a pie chart using ggplot2 with a different color palette
ggplot(counts_df, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3") +  # Choose a different palette here
  theme_minimal() +
  ggtitle("Distribution of Outcomes")


# Given statistics
walks <- 18
hit_by_pitch <- 1
plate_appearances <- 235 - 1
singles <- 35
doubles <- 12
triples <- 0
home_runs <- 9
at_bats <- 235 - 1 - 18 - 1
hits <- singles + doubles + triples
# Calculate OBP
obp <- (hits + walks + hit_by_pitch) / plate_appearances

# Calculate SLG
total_bases <- singles + 2 * doubles + 3 * triples + 4 * home_runs
slg <- total_bases / at_bats

# Calculate OPS
ops <- obp + slg

# Print OPS
print(ops)


# Initialize counts for each category
counts <- c(W = 0, S = 0, D = 0, T = 0, HR = 0, E = 0, FC = 0, HP = 0, K = 0, Out = 0)

events <- df_t9_r0_0o$EVENT_TX

# Create a new column in df_t9_r0_0o for categories
df_t9_r0_0o$Category <- NA

# Iterate through the rows of df_t9_r0_0o
for (i in 1:length(events)) {
  # Get the first 1 or 2 letters/digits of the value in "EVENT_TX"
  prefix <- substr(events[i], 1, 2)
  
  # Update counts based on the prefix
  if (grepl("^W", prefix)) {
    counts["W"] <- counts["W"] + 1
    df_t9_r0_0o$Category[i] <- "W"
  } else if (grepl("^HW", prefix)) {
    counts["HW"] <- counts["HW"] + 1
    df_t9_r0_0o$Category[i] <- "HW"
  } else if (grepl("^IW", prefix)) {
    counts["IW"] <- counts["IW"] + 1
    df_t9_r0_0o$Category[i] <- "IW"
  } else if (grepl("^S", prefix)) {
    counts["S"] <- counts["S"] + 1
    df_t9_r0_0o$Category[i] <- "S"
  } else if (grepl("^D", prefix)) {
    counts["D"] <- counts["D"] + 1
    df_t9_r0_0o$Category[i] <- "D"
  } else if (grepl("^T", prefix)) {
    counts["T"] <- counts["T"] + 1
    df_t9_r0_0o$Category[i] <- "T"
  } else if (grepl("^E", prefix)) {
    counts["E"] <- counts["E"] + 1
    df_t9_r0_0o$Category[i] <- "E"
  } else if (grepl("^FC", prefix)) {
    counts["FC"] <- counts["FC"] + 1
    df_t9_r0_0o$Category[i] <- "FC"
  } else if (grepl("^HP", prefix)) {
    counts["HP"] <- counts["HP"] + 1
    df_t9_r0_0o$Category[i] <- "HP"
  } else if (grepl("^K", prefix)) {
    counts["K"] <- counts["K"] + 1
    df_t9_r0_0o$Category[i] <- "K"
  } else if (grepl("^\\d", prefix)) {  # Check if it starts with a digit
    counts["Out"] <- counts["Out"] + 1
    df_t9_r0_0o$Category[i] <- "Out"
  } else if (grepl("^HR", prefix)) {
    counts["HR"] <- counts["HR"] + 1
    df_t9_r0_0o$Category[i] <- "HR"
  }
}

# Calculate the average of "AwayWin" for each "Category"
result <- df_t9_r0_0o %>%
  group_by(Category) %>%
  summarize(Average_AwayWin = mean(AwayWin, na.rm = TRUE))

# Print the result
print(result)


