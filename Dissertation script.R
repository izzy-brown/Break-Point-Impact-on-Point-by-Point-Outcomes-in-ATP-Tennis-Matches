# ----------------------------------------------
# Clear environment
# ----------------------------------------------
rm(list=ls())


# ----------------------------------------------
# Load libraries 
# ----------------------------------------------

library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(readr)
library(readxl)
library(writexl)
library(fuzzyjoin)
library(flextable)
library(lme4)
library(pROC)
library(caret)
library(corrplot)
library(randomForest)
library(ggeffects)


# ----------------------------------------------
# Load data 
# ----------------------------------------------

Fullset <- read.csv("~/Documents/MSc Sport Data Analytics/B1706 - Dissertation in Sport Data Analytics/pbp_matches_atp_main_current.csv")


# ----------------------------------------------
# Inspect data 
# ----------------------------------------------

str(Fullset)
head(Fullset)
length(unique(Fullset$pbp_id)) # Number of unique matches 
any(is.na(Fullset)) # Check for missing values 
duplicated <- duplicated(Fullset) # Check for duplicates
sum(duplicated(Fullset))
Fullset <- Fullset %>% distinct() # Remove duplicate
sum(is.na(Fullset)) # Check missing values 

Fullset$date <- dmy(Fullset$date) # Change date to date


# ----------------------------------------------
# Gathering list of unique tournament names 
# ----------------------------------------------

# Unique tournaments
unique_tournaments <- data.frame(tournament_name = unique(Fullset$tny_name))

# Saving unique tournaments
write.csv(unique_tournaments, file = "tournaments.csv", row.names = FALSE) # save list 

# Removing Davis Cup and Hopman Cup games as individuals are representing their country in group stages, not reflecting individual effects and tournament phases
Fullset <- Fullset %>%
  filter(!tny_name %in% c("DavisCup", "HopmanCupMens", "DavisCup-GroupsIIIandIV"))


# ----------------------------------------------
# Add tournament round to dataset 
# ----------------------------------------------

# Load data with tournament round
TournamentData1 <- read_excel("TournamentData1.xlsx", 
                              col_types = c("text", "text", "date"))

# Renaming tournament name so column names match 
TournamentData1 <- TournamentData1 %>%
  rename(tny_name = `Tournament name`) %>%
  rename(date = Date)

# Merge data
Fullset2 <- left_join(Fullset, TournamentData1, by = c("tny_name", "date"))

# Check to see if there are missing rounds that have not been added 
missing <- sum(is.na(Fullset2$Round))
missing
# There are so need to manually add these in as I know why they were missed
# Saving data so I can add missing rounds manually 
write_xlsx(Fullset2, "Fullset2.xlsx")
# Load in edited data 
Fullset2 <- read_excel("EditedData.xlsx")
# Double check missing values 
missing <- sum(is.na(Fullset2$Round))
missing


# ----------------------------------------------
# Add court surface to dataset
# ----------------------------------------------

# Load data with court surface
tournaments_WSurface <- read_csv("tournaments_WSurface.csv")

# Renaming tournament name so column names match 
tournaments_WSurface <- tournaments_WSurface %>%
  rename(tny_name = tournament_name)

# Merge data
Fullset2 <- merge(Fullset2, tournaments_WSurface, by = "tny_name", all.x = TRUE)

# Check that all rows have a surface
missing <- sum(is.na(Fullset2$Surface))
missing

# Removing columns I don't need 
Fullset2 <- Fullset2 %>%
  select(-tour, -draw, -adf_flag, -wh_minutes)

# Check for duplicates
duplicated <- duplicated(Fullset2)
sum(duplicated(Fullset2))


# ----------------------------------------------
# Add player ranking to dataset
# ----------------------------------------------

# Retrieving the first and last date to retrieve corresponding player rankings 
min_date <- min(Fullset2$date, na.rm = TRUE)
max_date <- max(Fullset2$date, na.rm = TRUE)
print(min_date)
print(max_date)

# Load player rankings 
Player_Rankings <- read_excel("Player Rankings1.xlsx", 
                               sheet = "All Dates")

# Join datasets

# For server1
Fullset2$ranking_server1 <- sapply(1:nrow(Fullset2), function(i) {
  date_i <- Fullset2$date[i]
  player_i <- Fullset2$server1[i]
  
  matching_row <- Player_Rankings[
    Player_Rankings$`Player name` == player_i & 
      Player_Rankings$`First Date` <= date_i & 
      Player_Rankings$`Last Date` >= date_i, ]
  
  if (nrow(matching_row) > 0) matching_row$Rank[1] else NA
})

# For server2
Fullset2$ranking_server2 <- sapply(1:nrow(Fullset2), function(i) {
  date_i <- Fullset2$date[i]
  player_i <- Fullset2$server2[i]
  
  matching_row <- Player_Rankings[
    Player_Rankings$`Player name` == player_i & 
      Player_Rankings$`First Date` <= date_i & 
      Player_Rankings$`Last Date` >= date_i, ]
  
  if (nrow(matching_row) > 0) matching_row$Rank[1] else NA
})

# Check that all rows have a ranking
missing1 <- sum(is.na(Fullset2$ranking_server1))
missing1
missing2 <- sum(is.na(Fullset2$ranking_server2))
missing2
# Approx 20 values missing for each server so saving data and adding missing rankings manually 
write_xlsx(Fullset2, "Fullset3.xlsx")
# Load in edited data 
Fullset2 <- read_excel("FinalData.xlsx")
# Double check that all rows now have a ranking
missing3 <- sum(is.na(Fullset2$ranking_server1))
missing3
missing4 <- sum(is.na(Fullset2$ranking_server2))
missing4

# Categorise rankings 
Fullset2 <- Fullset2 %>%
  mutate(across(
    c(ranking_server1, ranking_server2),
    ~ cut(.x,
          breaks = c(-Inf, 10, 30, 50, 100, 200, Inf),
          labels = c("Top 10", "11–30", "31–50", "51–100", "101-200", "200+"),
          right = TRUE),
    .names = "category_{.col}"
  ))

# Clean environment
rm(Player_Rankings, TournamentData1, tournaments_WSurface, unique_tournaments, Fullset)


# ----------------------------------------------
# Separate rows and split points 
# ----------------------------------------------

FullsetLong <- Fullset2 %>%
  separate_rows(pbp, sep = ";")             # separate rows 
FullsetLong <- FullsetLong %>%
  mutate(points = strsplit(pbp, ""))%>%     # split points 
  unnest(points)


# ----------------------------------------------
# Function to Convert Points to Tennis Scores
# ----------------------------------------------

convert_score <- function(points) {
  score_lookup <- c("0", "15", "30", "40")
  if (points > 3) return("Ad")
  return(score_lookup[points + 1])
}

# Initialise Variables
server_points <- 0
returner_points <- 0
scoreline <- c()

# Loop to Calculate Scoreline
for (i in 1:nrow(FullsetLong)) {
  point <- FullsetLong$points[i]
  
  # Update Scores Based on Point Outcome
  if (point %in% c("S", "A")) {
    server_points <- server_points + 1
  } else if (point %in% c("R", "D")) {
    returner_points <- returner_points + 1
  }
  
  # Handle Immediate Game Win: 40-0, 40-15, 40-30
  if (server_points == 4 && returner_points < 3) {
    scoreline[i] <- "Game S1"
    # Reset scores for next game
    server_points <- 0
    returner_points <- 0
  } else if (returner_points == 4 && server_points < 3) {
    scoreline[i] <- "Game S2"
    # Reset scores for next game
    server_points <- 0
    returner_points <- 0
  }
  
  # Handle Deuce and Advantage Scenarios
  else if (server_points >= 3 && returner_points >= 3) {
    if (server_points == returner_points) {
      scoreline[i] <- "Deuce"
    } else if (server_points == returner_points + 1) {
      scoreline[i] <- "Ad-40"
    } else if (returner_points == server_points + 1) {
      scoreline[i] <- "40-Ad"
    } else if (server_points >= returner_points + 2) {
      # Server wins the game
      scoreline[i] <- "Game S1"
      server_points <- 0
      returner_points <- 0
    } else if (returner_points >= server_points + 2) {
      # Returner wins the game
      scoreline[i] <- "Game S2"
      server_points <- 0
      returner_points <- 0
    }
  } else {
    # Regular Scoreline
    scoreline[i] <- paste(convert_score(server_points), "-", convert_score(returner_points))
  }
}

# Add Scoreline Column
FullsetLong <- FullsetLong %>%
  mutate(scoreline = scoreline)


# ----------------------------------------------
# Filtering for matches containing tiebreaks
# ----------------------------------------------

unique_pbp_id <- FullsetLong %>%
  filter(grepl("/", pbp)) %>%        # filter rows where pbp contains "/"
  distinct(pbp_id)                   # select unique pbp_id values 
print(unique_pbp_id)

# create new dataset excluding these matches
df_filtered <- FullsetLong[!FullsetLong$pbp_id %in% unique_pbp_id$pbp_id, ] 

# count the number of unique pbp_id values to check matches have been removed 
num_unique_ids <- df_filtered %>%
  summarise(unique_count = n_distinct(pbp_id)) 
print(num_unique_ids)


# ----------------------------------------------
# Adding both a number of games column and a set column
# ----------------------------------------------

# Initialise the game number column
df_filtered$game_number <- 1
df_filtered$set_number <- 1

# Variables to keep track of previous servers
previous_server1 <- df_filtered$server1[1]
previous_server2 <- df_filtered$server2[1]
previous_point <- df_filtered$points[1]

# Initialise the first game number value
game_number <- 1
set_number <- 1

# Loop through the rows of the dataframe
for (i in 2:nrow(df_filtered)) {
  
  # Condition 1: Reset game number if a dot is found in 'points'
  if (df_filtered$points[i] == '.') {
    game_number <- 1
    set_number <- set_number + 1    # Increment set number when a dot is found
  }
  # Condition 2: Reset game and set number if server names change
  else if (df_filtered$server1[i] != previous_server1 | df_filtered$server2[i] != previous_server2) {
    game_number <- 1
    set_number <- 1    
  }
  # Condition 3: Increment game number after 'Game S1' or 'Game S2' appears
  else if (df_filtered$scoreline[i-1] %in% c("Game S1", "Game S2")) {
    game_number <- game_number + 1
  }
  
  # Update the game_number column in the dataframe
  df_filtered$game_number[i] <- game_number
  df_filtered$set_number[i] <- set_number
  
  # Update previous server values for the next iteration
  previous_server1 <- df_filtered$server1[i]
  previous_server2 <- df_filtered$server2[i]
  previous_point <- df_filtered$points[i]
}


# ----------------------------------------------
# Splitting data into odd and even sets
# ----------------------------------------------

df_odd <- df_filtered %>% filter(set_number %% 2 != 0) # split into odd sets 
df_even <- df_filtered %>% filter(set_number %% 2 == 0) # split into even sets


# ----------------------------------------------
# Converting incorrect sets 
# ----------------------------------------------

# Create a new column with replacements
df_odd <- df_odd %>%
  mutate(point_winner = case_when(
    points == "S" ~ "S1",
    points == "R" ~ "S2",
    points == "D" ~ "S2",
    points == "A" ~ "S1",
    TRUE ~ NA_character_  # Default value for any other cases
  ))

# Create a new column with replacements
df_even <- df_even %>%
  mutate(point_winner = case_when(
    points == "S" ~ "S1",
    points == "R" ~ "S2",
    points == "D" ~ "S2",
    points == "A" ~ "S1",
    TRUE ~ NA_character_  # Default value for any other cases
  ))

# Swap where game_number is even
df_odd <- df_odd %>%
  mutate(point_winner = case_when(
    game_number %% 2 == 0 & point_winner == "S1" ~ "S2",      # S1 -> S2 when game_number is even
    game_number %% 2 == 0 & point_winner == "S2" ~ "S1",      # S2 -> S1 when game_number is even
    TRUE ~ point_winner                                       # Keep original value if no condition is met
  ))

# Change who won the game to match with the new correct column
df_odd$scoreline <- ifelse(
  # If scoreline contains "game s1" and point_winner is not "s1"
  grepl("Game S1", df_odd$scoreline) & df_odd$point_winner != "S1",
  # Replace with the correct format
  paste("Game", df_odd$point_winner),
  ifelse(
    # If scoreline contains "game s2" and point_winner is not "s2"
    grepl("Game S2", df_odd$scoreline) & df_odd$point_winner != "S2",
    # Replace with the correct format
    paste("Game", df_odd$point_winner),
    # Otherwise keep the original value
    df_odd$scoreline
  )
)

# Swap where game_number is odd
df_even <- df_even %>%
  mutate(point_winner = case_when(
    game_number %% 2 != 0 & point_winner == "S1" ~ "S2",      # S1 -> S2 when game_number is even
    game_number %% 2 != 0 & point_winner == "S2" ~ "S1",      # S2 -> S1 when game_number is even
    TRUE ~ point_winner                                       # Keep original value if no condition is met
  ))

# Change who won the game to match with the new correct column
df_even$scoreline <- ifelse(
  # If scoreline contains "game s1" and point_winner is not "s1"
  grepl("Game S1", df_even$scoreline) & df_even$point_winner != "S1",
  # Replace with the correct format
  paste("Game", df_even$point_winner),
  ifelse(
    # If scoreline contains "game s2" and point_winner is not "s2"
    grepl("Game S2", df_even$scoreline) & df_even$point_winner != "S2",
    # Replace with the correct format
    paste("Game", df_even$point_winner),
    # Otherwise keep the original value
    df_even$scoreline
  )
)


# ----------------------------------------------
# Merge odd and even sets back together 
# ----------------------------------------------

# Merge sets
corrected_sets <- rbind(df_odd, df_even) %>%
  arrange(pbp_id)

# Tidy environment 
rm(df_even, df_odd, FullsetLong, num_unique_ids, unique_pbp_id, df_filtered, Fullset2)


# ----------------------------------------------
# Add column to track game score 
# ----------------------------------------------

# Create variables to track scores
score_tracker <- function(corrected_sets) {
  S1_score <- 0
  S2_score <- 0
  current_server1 <- corrected_sets$server1[1]
  current_server2 <- corrected_sets$server2[2]
  scores <- rep(NA, nrow(corrected_sets))
  
  for (i in 1:nrow(corrected_sets)) {
    # Check if servers changed or points has a dot - reset scores if true
    if ((i > 1 && (corrected_sets$server1[i] != current_server1 || corrected_sets$server2[i] != current_server2)) || 
        grepl("\\.", corrected_sets$points[i])) {
      S1_score <- 0
      S2_score <- 0
      current_server1 <- corrected_sets$server1[i]
      current_server2 <- corrected_sets$server2[i]
    }
    
    # Update scores based on scoreline
    if (grepl("Game S1", corrected_sets$scoreline[i], ignore.case = TRUE)) {
      S1_score <- S1_score + 1
      scores[i] <- paste(S1_score, "-", S2_score)
    } else if (grepl("Game S2", corrected_sets$scoreline[i], ignore.case = TRUE)) {
      S2_score <- S2_score + 1
      scores[i] <- paste(S1_score, "-", S2_score)
    }
  }
  
  return(scores)
}

# Apply the scoring function
corrected_sets$game_score <- score_tracker(corrected_sets)


# ----------------------------------------------
# Add a score for pressure points and shift W/L outcome up a row
# ----------------------------------------------

# Shift up a row
corrected_sets$W_L_shifted_up <- lead(corrected_sets$points, n = 1)

# Convert letters to 0/1
corrected_sets <- corrected_sets %>%
  mutate(
    W_L_shifted_up = case_when(
      W_L_shifted_up %in% c("R", "D") ~ 1,  
      W_L_shifted_up %in% c("S", "A") ~ 0,
    )
  )

# Add 'break_points' column (1 if X-40 or 40-Ad, else 0)
corrected_sets <- corrected_sets %>%
  mutate(
    break_points = case_when(
      endsWith(scoreline, " - 40") ~ 1,  # Any X-40
      scoreline == "40-Ad" ~ 1,         # Exactly 40-Ad
      TRUE ~ 0                          # All other cases
    )
  )

# Save finalised data 
write.csv(corrected_sets, file = "Dissertation Data.csv", row.names = FALSE)


# ----------------------------------------------
# Load full data set
# ----------------------------------------------

data <- read.csv("Dissertation Data.csv")


# ----------------------------------------------
# Calculate ranking differences for players 
# ----------------------------------------------

# Initialise vector to hold server indicator (TRUE for S1 serving, FALSE for S2)
server <- logical(nrow(data))
current_server <- TRUE  # Focus on server ranking for break points 

# Loop through each row
for (i in seq_len(nrow(data))) {
  if (grepl("Game S[12]", data$scoreline[i])) {
    current_server <- !current_server  # Switch server
  }
  server[i] <- current_server
}

# Add the server flag to the data
data$S1_serving <- server

# Compute ranking difference
data$Rank_Diff <- ifelse(data$S1_serving,
                         data$ranking_server1 - data$ranking_server2,
                         data$ranking_server2 - data$ranking_server1)


# Creating a column that has ranking of server alone 
# Create a vector to store the category ranking of the server
category_rank <- numeric(nrow(data))

# Set the starting server — TRUE if S1 is serving, FALSE if S2
current_server <- TRUE

for (i in seq_len(nrow(data))) {
  # Switch server when "Game S1" or "Game S2" appears
  if (grepl("Game S[12]", data$scoreline[i])) {
    current_server <- !current_server
  }
  
  # Assign category rank based on current server
  category_rank[i] <- if (current_server) {
    data$category_ranking_server1[i]
  } else {
    data$category_ranking_server2[i]
  }
}

# Add it to the dataframe
data$Server_Category_Rank <- category_rank


# Create column of server names 
server_names <- character(nrow(data))          # Create a vector to store the server names
current_server <- TRUE                         # Set the starting server — TRUE if S1 is serving, FALSE if S2
for (i in seq_len(nrow(data))) {
  # Switch server when "Game S1" or "Game S2" appears
  if (grepl("Game S[12]", data$scoreline[i])) {
    current_server <- !current_server
  }
  
  # Assign category rank based on current server
  server_names[i] <- if (current_server) {
    data$server1[i]
  } else {
    data$server2[i]
  }
}
# Add it to the dataframe
data$Server_Name <- server_names


# ----------------------------------------------
# Inspect data 
# ----------------------------------------------

# Check missing data for new columns
colSums(is.na(data))
sum(data$points == ".") # missing point_winner occurs where there is '.' to denote end of set 
                        # missing W_L_shifted_up has same missing as directly related to point_winner, plus one extra due to lag
                        # missing game_score as only denoted at the end of each game

# Check structure of data
str(data)
# Change variable types 
data <- data %>%
  mutate(tny_name = as.factor(tny_name),
         pbp_id = as.numeric(pbp_id),
         date = as.Date(date),
         server1 = as.factor(server1),
         server2 = as.factor(server2),
         winner = as.numeric(winner),
         score = as.factor(score),
         Round = as.factor(Round),
         Surface = as.factor(Surface),
         ranking_server1 = as.numeric(ranking_server1),
         ranking_server2 = as.numeric(ranking_server2),
         category_ranking_server1 = as.factor(category_ranking_server1), 
         category_ranking_server2 = as.factor(category_ranking_server2),
         points = as.factor(points),
         scoreline = as.factor(scoreline),
         game_number = as.numeric(game_number),
         set_number = as.numeric(set_number),
         point_winner = as.factor(point_winner),
         game_score = as.factor(game_score),
         W_L_shifted_up = as.numeric(W_L_shifted_up),
         break_points = as.numeric(break_points),
         Rank_Diff = as.numeric(Rank_Diff),
         Server_Category_Rank = as.factor(Server_Category_Rank))


# Vector of column names to check for outliers
columns_to_check <- c("game_number", "set_number", "break_points", "ranking_server1", "ranking_server2", "W_L_shifted_up")
# Loop over each column and display boxplot + outliers
for (col in columns_to_check) {
  boxplot(data[[col]], main = paste("Boxplot of", col))
  outliers <- boxplot.stats(data[[col]])$out
  cat("\nOutliers in", col, ":\n")
  print(outliers)
}
# Outliers in ranking mitigated by ranking categories 
# Higher (outliers) game numbers when more and/or closer sets have been played 
# Higher (outliers) set numbers when more sets played in Grand Slams
table(data$break_points) 
prop.table(table(data$break_points)) * 100 # 1 appears as outlier in break points as it is rare


# ----------------------------------------------
# Descriptive Statistics 
# ----------------------------------------------

# Count the number points in the final dataset (those with '.' denote end of set so not actual points)
sum(!grepl("\\.", data$points))

# Number of unique matches 
length(unique(data$pbp_id)) 

# Number of unique tournaments 
length(unique(data$tny_name)) 

# Number of unique players 
length(unique(c(data$server1, data$server2)))

# Range of rankings
range(c(data$ranking_server1, data$ranking_server2), na.rm = TRUE)


# Defining round order for reporting
round_order <- c(
  "Final", 
  "Semi Finals", 
  "Quarter Finals", 
  "Round of 16", 
  "Round of 32", 
  "Round of 64", 
  "Round of 128"
)

# Defining rank order for reporting
rank_order <- c(
  "Top 10", 
  "11–30", 
  "31–50", 
  "51–100", 
  "101-200", 
  "200+"
)


# Proportion of matches on each surface 
surface_summary <- data %>%
  select(pbp_id, Surface) %>%
  distinct() %>%
  count(Surface) %>%
  mutate(proportion = round((n / sum(n)) * 100, 2)) # Convert to %
# Surface summary table
Table1<-flextable(surface_summary)
Table1 <- set_header_labels(Table1, 
                            Statistic="", 
                            Surface = "Surface",
                            n = "No of Matches",
                            proportion = "Proportion (%)")
Table1<-set_table_properties(Table1, layout = "autofit")
Table1 <- theme_vanilla(Table1)
Table1 <- align(Table1, j = c(1:3), align = "center", part = "all")
# Change font of the table
Table1 <- font(Table1, font = "Times New Roman", part = "all")
Table1

# Proportion of matches in each round
round_summary <- data %>%
  select(pbp_id, Round) %>%
  distinct() %>%
  count(Round) %>%
  mutate(proportion = n / sum(n)) %>%
  mutate(Round = factor(Round, levels = round_order)) %>%
  mutate(proportion = round((n / sum(n)) * 100, 2)) %>% # Convert to %
  arrange(Round) 
# Round summary table
Table2<-flextable(round_summary)
Table2 <- set_header_labels(Table2, 
                            Statistic="", 
                            Round = "Round",
                            n = "No of Matches",
                            proportion = "Proportion (%)")
Table2<-set_table_properties(Table2, layout = "autofit")
Table2 <- theme_vanilla(Table2)
Table2 <- align(Table2, j = c(1:3), align = "center", part = "all")
# Change font of the table
Table2 <- font(Table2, font = "Times New Roman", part = "all")
Table2

# Proportion of matches by ranking
rank_summary <- data %>%
  select(pbp_id, Server_Category_Rank) %>%
  distinct() %>%
  count(Server_Category_Rank) %>%
  mutate(Server_Category_Rank = factor(Server_Category_Rank, levels = rank_order)) %>%
  mutate(proportion = round((n / sum(n)) * 100, 2)) %>% # Convert to %
  arrange(Server_Category_Rank) 
# Ranking summary table
Table3<-flextable(rank_summary)
Table3 <- set_header_labels(Table3, 
                            Statistic="", 
                            Server_Category_Rank = "Ranking Category",
                            n = "No of Matches",
                            proportion = "Proportion (%)")
Table3<-set_table_properties(Table3, layout = "autofit")
Table3 <- theme_vanilla(Table3)
Table3 <- align(Table3, j = c(1:3), align = "center", part = "all")
# Change font of the table
Table3 <- font(Table3, font = "Times New Roman", part = "all")
Table3


# Break point win rate by surface 
bp_surface <- data %>%
  filter(break_points == 1) %>%
  group_by(Surface) %>%
  summarise(
    total_bp = n(),
    bp_won = sum(W_L_shifted_up, na.rm = TRUE),
    win_rate = bp_won / total_bp
  ) %>%
  mutate(win_rate = round(win_rate, 3)) %>%
  arrange(desc(win_rate))
# Table (for appendix)
Table4<-flextable(bp_surface)
Table4 <- set_header_labels(Table4, 
                            Statistic="", 
                            Surface = "Surface",
                            total_bp = "Total Break Points",
                            bp_won = "Break Points Won",
                            win_rate = "Win Rate")
Table4<-set_table_properties(Table4, layout = "autofit")
Table4 <- theme_vanilla(Table4)
Table4 <- align(Table4, j = c(1:3), align = "center", part = "all")
# Change font of the table
Table4 <- font(Table4, font = "Times New Roman", part = "all")
Table4

# Break point win rate by round 
bp_round <- data %>%
  filter(break_points == 1) %>%
  group_by(Round) %>%
  summarise(
    total_bp = n(),
    bp_won = sum(W_L_shifted_up, na.rm = TRUE),
    win_rate = bp_won / total_bp
  ) %>%
  mutate(win_rate = round(win_rate, 3)) %>%
  mutate(Round = factor(Round, levels = round_order)) %>%  
  arrange(desc(Round))  
# Table (for appendix)
Table5<-flextable(bp_round)
Table5 <- set_header_labels(Table5, 
                            Statistic="", 
                            Round = "Round",
                            total_bp = "Total Break Points",
                            bp_won = "Break Points Won",
                            win_rate = "Win Rate")
Table5<-set_table_properties(Table5, layout = "autofit")
Table5 <- theme_vanilla(Table5)
Table5 <- align(Table5, j = c(1:3), align = "center", part = "all")
# Change font of the table
Table5 <- font(Table5, font = "Times New Roman", part = "all")
Table5

# Break point win rate by ranking 
bp_ranking <- data %>%
  filter(break_points == 1) %>%
  group_by(Server_Category_Rank) %>%
  summarise(
    total_bp = n(),
    bp_won = sum(W_L_shifted_up, na.rm = TRUE),
    win_rate = bp_won / total_bp
  ) %>%
  mutate(win_rate = round(win_rate, 3)) %>%
  arrange(desc(win_rate))
# Table (for appendix)
Table6<-flextable(bp_ranking)
Table6 <- set_header_labels(Table6, 
                            Statistic="", 
                            Server_Category_Rank = "Ranking Category",
                            total_bp = "Total Break Points",
                            bp_won = "Break Points Won",
                            win_rate = "Win Rate")
Table6<-set_table_properties(Table6, layout = "autofit")
Table6 <- theme_vanilla(Table6)
Table6 <- align(Table6, j = c(1:3), align = "center", part = "all")
# Change font of the table
Table6 <- font(Table6, font = "Times New Roman", part = "all")
Table6

# Break points won by surface and round 
pressure_table <- data %>%
  filter(break_points == 1) %>%
  group_by(Surface, Round) %>%
  summarise(
    total_bp = n(),
    bp_won = sum(W_L_shifted_up, na.rm = TRUE),
    win_rate = bp_won / total_bp *100
  )
pressure_table$Round <- factor(pressure_table$Round, levels = round_order, ordered = TRUE) # Order by chronological rounds
# Heatmap of pressure point success by surface and round 
ggplot(pressure_table, aes(x = Round, y = Surface, fill = win_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "skyblue1", high = "sienna1", name = "Win %") +
  theme_minimal() +
  theme(axis.text = element_text(size = 20, family = "Times New Roman"),     
        axis.title = element_text(size = 22, family = "Times New Roman"), 
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.text = element_text(size = 18, family = "Times New Roman")) +    
  labs(title = "Pressure Point Win Rate by Surface and Round")


# More detailed breakdown of total matches
demographics <- data %>%
  select(pbp_id, Surface, Round) %>%
  mutate(Round = factor(Round, levels = round_order, ordered = TRUE)) %>%
  distinct() %>%
  group_by(Surface, Round) %>%
  summarise(n_matches = n_distinct(pbp_id), .groups = "drop") %>%
  pivot_wider(
    names_from = Round,
    values_from = n_matches
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
  select(Surface, Total, everything())
# Table (for appendix)
Table7<-flextable(demographics)
Table7 <- set_header_labels(Table7, 
                            Statistic="", 
                            Surface = "Surface",
                            Round = "Round",
                            n_matches = "No of Matches")
Table7<-set_table_properties(Table7, layout = "autofit")
Table7 <- theme_vanilla(Table7)
Table7 <- align(Table7, j = c(1:3), align = "center", part = "all")
# Change font of the table
Table7 <- font(Table7, font = "Times New Roman", part = "all")
Table7

# Clean environment
rm(bp_ranking, bp_round, bp_surface, demographics, pressure_table, rank_summary, round_summary, surface_summary, Table1, Table2, Table3, Table4, Table5, Table6, Table7)


# ----------------------------------------------
# Chi Square / Correlations
# ----------------------------------------------

# Remove rows with na
data <- data %>%
  filter(!is.na(W_L_shifted_up))  

# Chi square tests for categorical variables 
chisq.test(table(data$W_L_shifted_up, data$break_points))
chisq.test(table(data$W_L_shifted_up, data$Surface))
chisq.test(table(data$W_L_shifted_up, data$Round))

# Correlation for numerical variable 
cor.test(data$Rank_Diff, as.numeric(data$W_L_shifted_up), method = "pearson")


# Create summary table
results_table <- data.frame(
  Variable_Compared = c(
    "W_L_shifted_up vs break_points",
    "W_L_shifted_up vs Surface",
    "W_L_shifted_up vs Round",
    "W_L_shifted_up vs Rank_Diff"
  ),
  Test = c(
    "Chi-squared",
    "Chi-squared",
    "Chi-squared",
    "Pearson correlation"
  ),
  Test_Statistic = c(
    "X² = 97.03",
    "X² = 62.04",
    "X² = 16.50",
    "t = -0.04"
  ),
  df = c("1", "2", "6", "178440"),
  p_value = c(
    "< 0.001***",
    "< 0.001***",
    "0.01*",
    "0.97"
  )
)

# Table for reporting
Table8<-flextable(results_table)
Table8 <- set_header_labels(Table8, 
                            Statistic="", 
                            Variable_Compared = "Variable Compared",
                            Test = "Test",
                            Test_Statistic = " Test Statistic",
                            df = "df",
                            p_value = "p")
Table8<-set_table_properties(Table8, layout = "autofit")
Table8 <- theme_vanilla(Table8)
Table8 <- align(Table8, j = c(1:3), align = "center", part = "all")
# Change font of the table
Table8 <- font(Table8, font = "Times New Roman", part = "all")
Table8


# ----------------------------------------------
# GLMM Logistic Regression
# ----------------------------------------------

# Check correlation between variables 
cor_matrix <- cor(select(data, where(is.numeric)), use = "complete.obs")
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black")

# Make W_L_shifted_up factor
data$W_L_shifted_up <- factor(data$W_L_shifted_up, levels = c(0, 1), labels = c("Server Wins", "Server Loses"))

# Scale numerical data 
data$Rank_Diff_scaled <- scale(data$Rank_Diff)

# Split data 
set.seed(123)  # for reproducibility
train_index <- createDataPartition(data$W_L_shifted_up, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# First basic model with only pressure as predictor 
model <- glmer(W_L_shifted_up ~ break_points + 
                 (1 | pbp_id) + (1 | Server_Name),
               data = train_data, family = binomial)
summary(model)

# Table of estimates and odd ratios with 95% CI
se <- sqrt(diag(vcov(model)))    # Calculate standard error 
(tab <- cbind(Est = fixef(model), OR = exp(fixef(model)), LL = exp(fixef(model)) - 1.96 * se, UL = exp(fixef(model)) + 1.96 *
                se))

# Predicted Probabilities
test_data$predicted_prob <- predict(model, newdata = test_data, type = "response")

# ROC curve
roc_curve <- roc(test_data$W_L_shifted_up, test_data$predicted_prob)
plot(roc_curve, main="ROC Curve")
# AUC
auc1 <- auc(roc_curve) 
# Find best threshold 
coords(roc_curve, "best", ret = "threshold")

# Adjust predicted_class to match the factor levels of pass_fail
predicted_class <- ifelse(test_data$predicted_prob > 0.37, "Server Loses", "Server Wins")
# Convert to factors with the same levels
predicted_class <- factor(predicted_class, levels = c("Server Wins", "Server Loses"))
# Confusion Matrix
confusionMatrix1 <- confusionMatrix(predicted_class,test_data$W_L_shifted_up)
# Extract precision, recall, and F1 score
precision <- confusionMatrix1$byClass["Pos Pred Value"]
recall <- confusionMatrix1$byClass["Sensitivity"]
f1_score <- 2 * ((precision * recall) / (precision + recall))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("F1 Score:", round(f1_score, 4)))

# Visualise confusion matrix for appendix 
# Extract the confusion matrix table
cm_table <- as.table(confusionMatrix1$table)
# Convert to data frame for ggplot
cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Predicted", "Actual", "Freq")
# Plot 
ggplot(data = cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1, size = 8, family = "Times New Roman") +
  scale_fill_gradient(low = "skyblue1", high = "dodgerblue4") +
  theme_minimal() +
  theme(axis.text = element_text(size = 20, family = "Times New Roman"),     
        axis.title = element_text(size = 22, family = "Times New Roman"), 
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.text = element_text(size = 18, family = "Times New Roman")) +
  labs(title = "Confusion Matrix", x = "Actual Label", y = "Predicted Label")



# Model 2 with all predictors 
model2 <- glmer(W_L_shifted_up ~ break_points + Surface + Round + Rank_Diff_scaled +
                  (1 | pbp_id) + (1 | Server_Name),
                data = train_data, family = binomial,
                nAGQ = 1,
                control = glmerControl(optimizer = "bobyqa"))
summary(model2)

# Table of estimates with 95% CI
se <- sqrt(diag(vcov(model2)))
(tab <- cbind(Est = fixef(model2), OR = exp(fixef(model2)), LL = exp(fixef(model2)) - 1.96 * se, UL = exp(fixef(model2)) + 1.96 *
                se))

# Predicted Probabilities
test_data$predicted_prob <- predict(model2, newdata = test_data, type = "response")

# ROC curve
roc_curve <- roc(test_data$W_L_shifted_up, test_data$predicted_prob)
plot(roc_curve, main="ROC Curve")
# AUC
auc2 <- auc(roc_curve)
# Find best threshold 
coords(roc_curve, "best", ret = "threshold")

# Adjust predicted_class to match the factor levels of pass_fail
predicted_class <- ifelse(test_data$predicted_prob > 0.38, "Server Loses", "Server Wins")
# Convert to factors with the same levels
predicted_class <- factor(predicted_class, levels = c("Server Wins", "Server Loses"))
# Confusion Matrix
confusionMatrix2 <- confusionMatrix(predicted_class,test_data$W_L_shifted_up)
# Extract precision, recall, and F1 score
precision <- confusionMatrix2$byClass["Pos Pred Value"]
recall <- confusionMatrix2$byClass["Sensitivity"]
f1_score <- 2 * ((precision * recall) / (precision + recall))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("F1 Score:", round(f1_score, 4)))

# Visualise confusion matrix for appendix 
# Extract the confusion matrix table
cm_table <- as.table(confusionMatrix2$table)
# Convert to data frame for ggplot
cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Predicted", "Actual", "Freq")
# Plot 
ggplot(data = cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1, size = 8, family = "Times New Roman") +
  scale_fill_gradient(low = "skyblue1", high = "dodgerblue4") +
  theme_minimal() +
  theme(axis.text = element_text(size = 20, family = "Times New Roman"),     
        axis.title = element_text(size = 22, family = "Times New Roman"), 
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.text = element_text(size = 18, family = "Times New Roman")) +
  labs(title = "Confusion Matrix", x = "Actual Label", y = "Predicted Label")

# Create summary table
results_table2 <- data.frame(
  Predictor = c(
    "(Intercept)",
    "Break Points",
    "Grass Courts",
    "Hard Courts",
    "Quarter Finals",
    "Round of 128",
    "Round of 16",
    "Round of 32",
    "Round of 64",
    "Semi Finals",
    "Rank Difference"
  ),
  Estimate = c(
    "-0.55",
    "0.15",
    "-0.12",
    "-0.05",
    "0.05",
    "0.10",
    "0.05",
    "0.07",
    "0.09",
    "0.04",
    "0.001"
  ),
  OR = c(
    "0.58",
    "1.16",
    "0.89",
    "0.96",
    "1.05",
    "1.11",
    "1.05",
    "1.08",
    "1.09",
    "1.04",
    "1.00"
  ),
  CI_OR = c(
    "[0.50, 0.65]",
    "[1.13, 1.20]",
    "[0.85, 0.93]",
    "[0.92, 0.98]",
    "[0.97, 1.33]",
    "[1.03, 1.19]",
    "[0.97, 1.13]",
    "[1.00, 1.15]",
    "[1.02, 1.17]",
    "[0.95, 1.13]",
    "[0.99, 1.01]"
  ),
  p_value = c(
    "< 0.001***",
    "< 0.001***",
    "< 0.001***",
    "< 0.001***",
    "0.23",
    "0.012*",
    "0.24",
    "0.06",
    "0.02*",
    "0.41",
    "0.85"
  )
)

# Table for reporting 
Table9<-flextable(results_table2)
Table9 <- set_header_labels(Table9, 
                            Statistic="", 
                            Estimate = "Estimate",
                            OR = "OR",
                            CI_OR = "95% CI for OR",
                            p_value = "p")
Table9<-set_table_properties(Table9, layout = "autofit")
Table9 <- theme_vanilla(Table9)
Table9 <- align(Table9, j = c(1:3), align = "center", part = "all")
# Change font of the table
Table9 <- font(Table9, font = "Times New Roman", part = "all")
Table9



# Model 3 with interactions 
model3 <- glmer(W_L_shifted_up ~ (break_points * Surface) + (break_points * Round) + (break_points * Rank_Diff_scaled) +
                  (1 | pbp_id) + (1 | Server_Name),
                data = train_data, family = binomial,
                nAGQ = 1,
                control = glmerControl(optimizer = "bobyqa"))
summary(model3)

# Table of estimates with 95% CI
se <- sqrt(diag(vcov(model3)))
(tab <- cbind(Est = fixef(model3), OR = exp(fixef(model3)), LL = exp(fixef(model3)) - 1.96 * se, UL = exp(fixef(model3)) + 1.96 *
                se))

# Predicted Probabilities
test_data$predicted_prob <- predict(model3, newdata = test_data, type = "response")

# ROC curve
roc_curve <- roc(test_data$W_L_shifted_up, test_data$predicted_prob)
plot(roc_curve, main="ROC Curve")
# AUC
auc3 <- auc(roc_curve) 
# Find best threshold 
coords(roc_curve, "best", ret = "threshold")

# Adjust predicted_class to match the factor levels of pass_fail
predicted_class <- ifelse(test_data$predicted_prob > 0.38, "Server Loses", "Server Wins")
# Convert to factors with the same levels
predicted_class <- factor(predicted_class, levels = c("Server Wins", "Server Loses"))
# Confusion Matrix
confusionMatrix3 <- confusionMatrix(predicted_class,test_data$W_L_shifted_up)
# Extract precision, recall, and F1 score
precision <- confusionMatrix3$byClass["Pos Pred Value"]
recall <- confusionMatrix3$byClass["Sensitivity"]
f1_score <- 2 * ((precision * recall) / (precision + recall))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("F1 Score:", round(f1_score, 4)))

# Visualise confusion matrix for appendix 
# Extract the confusion matrix table
cm_table <- as.table(confusionMatrix3$table)
# Convert to data frame for ggplot
cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Predicted", "Actual", "Freq")
# Plot 
ggplot(data = cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1, size = 8, family = "Times New Roman") +
  scale_fill_gradient(low = "skyblue1", high = "dodgerblue4") +
  theme_minimal() +
  theme(axis.text = element_text(size = 20, family = "Times New Roman"),     
        axis.title = element_text(size = 22, family = "Times New Roman"), 
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.text = element_text(size = 18, family = "Times New Roman")) +
  labs(title = "Confusion Matrix", x = "Actual Label", y = "Predicted Label")

# Plot marginal effects
me <- ggpredict(model3, terms = c("break_points", "Surface"))
plot(me) + 
  scale_color_manual(values = c("Clay" = "sienna1", "Grass" = "green2", "Hard" = "skyblue1")) + 
  theme(axis.text = element_text(size = 20, family = "Times New Roman"),     
        axis.title = element_text(size = 22, family = "Times New Roman"), 
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.text = element_text(size = 18, family = "Times New Roman")) +
  labs(x = "Break Points (0=Non-Break Point, 1=Break Point)",
       y = "Predicted Probability of Server Losing")


# ----------------------------------------------
# Random Forest 
# ----------------------------------------------

# Basic random forest model
rf_model <- randomForest(W_L_shifted_up ~ break_points, 
                         data = train_data, 
                         ntree = 500,       # number of trees
                         mtry = 1)          # number of variables to try at each split 

# Evaluate
rf_preds_prob <- predict(rf_model, newdata = test_data, type = "prob")[, "Server Loses"]
rf_preds_class <- predict(rf_model, newdata = test_data)

confusionMatrix4 <- confusionMatrix(rf_preds_class, test_data$W_L_shifted_up)
# Extract precision, recall, and F1 score
precision <- confusionMatrix4$byClass["Pos Pred Value"]
recall <- confusionMatrix4$byClass["Sensitivity"]
f1_score <- 2 * ((precision * recall) / (precision + recall))
print(paste("Precision:", round(precision, 3)))
print(paste("Recall:", round(recall, 3)))
print(paste("F1 Score:", round(f1_score, 3)))

# ROC curve and AUC
rf_roc <- roc(test_data$W_L_shifted_up, rf_preds_prob)
plot(rf_roc, main = "Random Forest ROC Curve")
auc4 <- auc(rf_roc)

# Visualise confusion matrix for appendix 
# Extract the confusion matrix table
cm_table <- as.table(confusionMatrix4$table)
# Convert to data frame for ggplot
cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Predicted", "Actual", "Freq")
# Plot 
ggplot(data = cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1, size = 8, family = "Times New Roman") +
  scale_fill_gradient(low = "skyblue1", high = "dodgerblue4") +
  theme_minimal() +
  theme(axis.text = element_text(size = 20, family = "Times New Roman"),     
        axis.title = element_text(size = 22, family = "Times New Roman"), 
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.text = element_text(size = 18, family = "Times New Roman")) +
  labs(title = "Confusion Matrix", x = "Actual Label", y = "Predicted Label")



# Random forest model with all predictors
rf_model2 <- randomForest(W_L_shifted_up ~ break_points + Surface + Round + Rank_Diff_scaled, 
                         data = train_data, 
                         ntree = 500, 
                         importance = TRUE) # to get variable importance

# Evaluate
rf_preds_prob <- predict(rf_model2, newdata = test_data, type = "prob")[, "Server Loses"]
rf_preds_class <- predict(rf_model2, newdata = test_data)

confusionMatrix5 <- confusionMatrix(rf_preds_class, test_data$W_L_shifted_up)
# Extract precision, recall, and F1 score
precision <- confusionMatrix5$byClass["Pos Pred Value"]
recall <- confusionMatrix5$byClass["Sensitivity"]
f1_score <- 2 * ((precision * recall) / (precision + recall))
print(paste("Precision:", round(precision, 3)))
print(paste("Recall:", round(recall, 3)))
print(paste("F1 Score:", round(f1_score, 3)))

# ROC curve and AUC
rf_roc <- roc(test_data$W_L_shifted_up, rf_preds_prob)
plot(rf_roc, main = "Random Forest ROC Curve")
auc5 <- auc(rf_roc)

# Visualise confusion matrix for appendix 
# Extract the confusion matrix table
cm_table <- as.table(confusionMatrix5$table)
# Convert to data frame for ggplot
cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Predicted", "Actual", "Freq")
# Plot 
ggplot(data = cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1, size = 8, family = "Times New Roman") +
  scale_fill_gradient(low = "skyblue1", high = "dodgerblue4") +
  theme_minimal() +
  theme(axis.text = element_text(size = 20, family = "Times New Roman"),     
        axis.title = element_text(size = 22, family = "Times New Roman"), 
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.text = element_text(size = 18, family = "Times New Roman")) +
  labs(title = "Confusion Matrix", x = "Actual Label", y = "Predicted Label")


# ----------------------------------------------
# Model Comparison 
# ----------------------------------------------

# Create comparison table
comparison <- data.frame(
  Model = c("Model 1: GLMM Basic", "Model 2: GLMM All Predictors", "Model 3: GLMM Interactions", "Model 4: RF Basic", "Model 5: RF All Predictors"),
  AUC = round(c(auc1, auc2, auc3, auc4, auc5), 2),
  Accuracy = round(c(confusionMatrix1$overall["Accuracy"], confusionMatrix2$overall["Accuracy"], confusionMatrix3$overall["Accuracy"], confusionMatrix4$overall["Accuracy"], confusionMatrix5$overall["Accuracy"]), 2),
  Sensitivity = round(c(confusionMatrix1$byClass["Sensitivity"], confusionMatrix2$byClass["Sensitivity"], confusionMatrix3$byClass["Sensitivity"], confusionMatrix4$byClass["Sensitivity"], confusionMatrix5$byClass["Sensitivity"]), 2),
  Specificity = round(c(confusionMatrix1$byClass["Specificity"], confusionMatrix2$byClass["Specificity"], confusionMatrix3$byClass["Specificity"], confusionMatrix4$byClass["Specificity"], confusionMatrix5$byClass["Specificity"]), 2)
)
print(comparison)

# Table for reporting
Table10<-flextable(comparison)
Table10 <- set_header_labels(Table10, 
                            Statistic="", 
                            Model = "Model",
                            AUC = "AUC",
                            Accuracy = " Accuracy",
                            Sensitivity = "Sensitivity",
                            Specificity = "Specificity")
Table10<-set_table_properties(Table10, layout = "autofit")
Table10 <- theme_vanilla(Table10)
Table10 <- align(Table10, j = c(1:3), align = "center", part = "all")
# Change font of the table
Table10 <- font(Table10, font = "Times New Roman", part = "all")
Table10


# ----------------------------------------------
# Filtering for just break points 
# ----------------------------------------------

# Filter for break points
data_break_points <- data %>%
  filter(break_points == 1)

# Scale numerical data
data_break_points$Rank_Diff_scaled <- scale(data_break_points$Rank_Diff)

# Split data 
set.seed(123)  # for reproducibility
train_index <- createDataPartition(data_break_points$W_L_shifted_up, p = 0.8, list = FALSE)
train_data <- data_break_points[train_index, ]
test_data <- data_break_points[-train_index, ]


# Model with all predictors 
model2 <- glmer(W_L_shifted_up ~ Surface + Round + Rank_Diff_scaled +
                  (1 | pbp_id) + (1 | Server_Name),
                data = train_data, family = binomial,
                nAGQ = 1)
summary(model2)

# Table of estimates with 95% CI
se <- sqrt(diag(vcov(model2)))
(tab <- cbind(Est = fixef(model2), LL = fixef(model2) - 1.96 * se, UL = fixef(model2) + 1.96 *
                se))

# Predicted Probabilities
test_data$predicted_prob <- predict(model2, newdata = test_data, type = "response", allow.new.levels = TRUE)

# ROC curve
roc_curve <- roc(test_data$W_L_shifted_up, test_data$predicted_prob)
plot(roc_curve, main="ROC Curve")
# AUC
auc(roc_curve) 
# Find best threshold 
coords(roc_curve, "best", ret = "threshold")

# Adjust predicted_class to match the factor levels of pass_fail
predicted_class <- ifelse(test_data$predicted_prob > 0.43, "Server Loses", "Server Wins")
# Convert to factors with the same levels
predicted_class <- factor(predicted_class, levels = c("Server Wins", "Server Loses"))
# Confusion Matrix
confusionMatrixA <- confusionMatrix(predicted_class,test_data$W_L_shifted_up)

# Visualise confusion matrix for appendix 
# Extract the confusion matrix table
cm_table <- as.table(confusionMatrixA$table)
# Convert to data frame for ggplot
cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Predicted", "Actual", "Freq")
# Plot 
ggplot(data = cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1, size = 8, family = "Times New Roman") +
  scale_fill_gradient(low = "skyblue1", high = "dodgerblue4") +
  theme_minimal() +
  theme(axis.text = element_text(size = 20, family = "Times New Roman"),     
        axis.title = element_text(size = 22, family = "Times New Roman"), 
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.text = element_text(size = 18, family = "Times New Roman")) +
  labs(title = "Confusion Matrix", x = "Actual Label", y = "Predicted Label")



# Random forest model with all predictors
rf_model2 <- randomForest(W_L_shifted_up ~ Surface + Round + Rank_Diff_scaled, 
                          data = train_data, 
                          ntree = 500, 
                          importance = TRUE)

# Evaluate
rf_preds_prob <- predict(rf_model2, newdata = test_data, type = "prob")[, "Server Loses"]
rf_preds_class <- predict(rf_model2, newdata = test_data)

confusionMatrixB <- confusionMatrix(rf_preds_class, test_data$W_L_shifted_up)

rf_roc <- roc(test_data$W_L_shifted_up, rf_preds_prob)
plot(rf_roc, main = "Random Forest ROC Curve")
auc(rf_roc)

# Visualise confusion matrix for appendix 
# Extract the confusion matrix table
cm_table <- as.table(confusionMatrixB$table)
# Convert to data frame for ggplot
cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Predicted", "Actual", "Freq")
# Plot 
ggplot(data = cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1, size = 8, family = "Times New Roman") +
  scale_fill_gradient(low = "skyblue1", high = "dodgerblue4") +
  theme_minimal() +
  theme(axis.text = element_text(size = 20, family = "Times New Roman"),     
        axis.title = element_text(size = 22, family = "Times New Roman"), 
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.text = element_text(size = 18, family = "Times New Roman")) +
  labs(title = "Confusion Matrix", x = "Actual Label", y = "Predicted Label")

