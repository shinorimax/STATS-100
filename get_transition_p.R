library(ggplot2)
library(dplyr)

##############
#get probability of runner scoring from 2nd on a single
df <- read.csv("2023events.csv")
df$Date <- rep(as.Date("2023-01-01"), nrow(df))
df_r2_single <- df[df$BASE2_RUN_ID != "" & substr(df$EVENT_TX, 1, 1) == "S", ]
single_score <- 0
single_stop <- 0
single_killed_at_h <- 0
sb <- 0
for (i in 1:nrow(df_r2_single)) {
  if (grepl("2-H", df_r2_single[i, "EVENT_TX"])) {
    single_score <- single_score + 1
  } else if (grepl("2-3", df_r2_single[i, "EVENT_TX"])){
    single_stop <- single_stop + 1
  } else if (grepl("2XH", df_r2_single[i, "EVENT_TX"])){
    single_killed_at_h <- single_killed_at_h + 1
    #print(df_r2_single[i, "EVENT_TX"])
  } else if (grepl("SB", df_r2_single[i, "EVENT_TX"])){
    sb <- sb + 1
  } else {
    #print(df_r2_single[i, "EVENT_TX"])
  }
}
nrow(df_r2_single)
single_score
single_stop
single_killed_at_h
sb
nrow(df_r2_single) - single_score - single_stop - single_killed_at_h - sb

df_r1_double <- df[df$BASE1_RUN_ID != "" & substr(df$EVENT_TX, 1, 1) == "D", ]
double_score <- 0
double_stop <- 0
double_killed_at_h <- 0
sb <- 0
di <- 0
for (i in 1:nrow(df_r1_double)) {
  if (grepl("1-H", df_r1_double[i, "EVENT_TX"])) {
    double_score <- double_score + 1
  } else if (grepl("1-3", df_r1_double[i, "EVENT_TX"])){
    double_stop <- double_stop + 1
  } else if (grepl("1XH", df_r1_double[i, "EVENT_TX"])){
    double_killed_at_h <- double_killed_at_h + 1
    #print(df_r2_single[i, "EVENT_TX"])
  } else if (grepl("SB", df_r1_double[i, "EVENT_TX"])){
    sb <- sb + 1
  } else if (grepl("DI", df_r1_double[i, "EVENT_TX"])){
    di <- di + 1
  } else {
    print(df_r1_double[i, "EVENT_TX"])
  }
}

nrow(df_r1_double)
double_score
double_stop
double_killed_at_h 
sb
di


df_r1_single <- df[df$BASE1_RUN_ID != "" & substr(df$EVENT_TX, 1, 1) == "S", ]
to3rd <- 0
to2nd <- 0
killed_at_3rd <- 0
sb <- 0
di <- 0
for (i in 1:nrow(df_r1_single)) {
  if (grepl("1-3", df_r1_single[i, "EVENT_TX"])) {
    to3rd <- to3rd + 1
  } else if (grepl("1-2", df_r1_single[i, "EVENT_TX"])){
    to2nd <- to2nd + 1
  } else if (grepl("1X3", df_r1_single[i, "EVENT_TX"])){
    killed_at_3rd <- killed_at_3rd + 1
    #print(df_r2_single[i, "EVENT_TX"])
  } else if (grepl("SB", df_r1_single[i, "EVENT_TX"])){
    sb <- sb + 1
  } else if (grepl("DI", df_r1_single[i, "EVENT_TX"])){
    di <- di + 1
  } else {
    #print(df_r1_single[i, "EVENT_TX"])
  }
}

nrow(df_r1_single)
to3rd 
to2nd 
killed_at_3rd 
sb 
di
nrow(df_r1_single)-to3rd -to2nd -killed_at_3rd -sb -di

3115/(3115+1816)
1043/(1043+1474)
5583/(2734+5583)


###############
#A function that creates a markov chain with certain inputs and run simulation
#inputs: Player Name and Team 

make_markov_from_player <- function(player_name, team) {
  df_player_stats <- read.csv("player_stats_2023.csv")
  df_player_stats$Name <- gsub("\\*", "", df_player_stats$Name)
  df_player_stats$Name <- gsub("\\#", "", df_player_stats$Name)
  name_parts <- strsplit(player_name, " ")[[1]]
  first_name <- name_parts[1]
  partial_matches <- df_player_stats[grepl(first_name, df_player_stats$Name) & df_player_stats$Tm == team, ]
  s <- partial_matches$SingleP
  d <- partial_matches$DoubleP
  t <- partial_matches$TripleP
  hr <- partial_matches$HRP
  w <- partial_matches$BB.HBP.IBBP
  o <- partial_matches$OutP
    
  transition_matrix <- matrix(
    c(hr, s+w, d, t, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, 0, d*(0.4143822), t, w+s*(0.6712757), w+s*(1-0.6712757), d*(1-0.4143822), 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, s*(0.6317177), d, t, w, s*(1-0.6317177), 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, s, d, t, 0, 0, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, 0, d*(0.4143822), t, s*(0.6317177)*(0.6712757), s*(0.6317177)*(1-0.6712757), d*(1-0.4143822), w+s*(1-0.6317177), 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, 0, d*(0.4143822), t, s*(0.6712757), s*(1-0.6712757), d*(1-0.4143822), w, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, s*(0.6317177), d, t, 0, s*(1-0.6317177), 0, w, 0, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, 0, d*(0.4143822), t, s*(0.6317177)*(0.6712757), s*(0.6317177)*(1-0.6712757), d*(1-0.4143822), s*(1-0.6317177), 0, 0, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, s+w, d, t, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, w+s*(0.6712757), w+s*(1-0.6712757), d*(1-0.4143822), 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, s*(0.6317177), d, t, w, s*(1-0.6317177), 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, s, d, t, 0, 0, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, s*(0.6317177)*(0.6712757), s*(0.6317177)*(1-0.6712757), d*(1-0.4143822), w+s*(1-0.6317177), 0, 0, 0, 0, o, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, s*(0.6712757), s*(1-0.6712757), d*(1-0.4143822), w, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, s*(0.6317177), d, t, 0, s*(1-0.6317177), 0, w, 0, 0, 0, 0, 0, 0, o, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, s*(0.6317177)*(0.6712757), s*(0.6317177)*(1-0.6712757), d*(1-0.4143822), s*(1-0.6317177), 0, 0, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, s+w, d, t, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, w+s*(0.6712757), w+s*(1-0.6712757), d*(1-0.4143822), 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, s*(0.6317177), d, t, w, s*(1-0.6317177), 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, s, d, t, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, s*(0.6317177)*(0.6712757), s*(0.6317177)*(1-0.6712757), d*(1-0.4143822), w+s*(1-0.6317177), o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, s*(0.6712757), s*(1-0.6712757), d*(1-0.4143822), w, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, s*(0.6317177), d, t, 0, s*(1-0.6317177), 0, w, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, s*(0.6317177)*(0.6712757), s*(0.6317177)*(1-0.6712757), d*(1-0.4143822), s*(1-0.6317177), o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    nrow = 25, byrow = TRUE,
    dimnames = list(c("o0ro", "o0r1", "o0r2", "o0r3", "o0r12", "o0r13", "o0r23", "o0r123", "o1ro", "o1r1", "o1r2", "o1r3", "o1r12", "o1r13", "o1r23", "o1r123", "o2ro", "o2r1", "o2r2", "o2r3", "o2r12", "o2r13", "o2r23", "o2r123", "o3"), c("o0ro", "o0r1", "o0r2", "o0r3", "o0r12", "o0r13", "o0r23", "o0r123", "o1ro", "o1r1", "o1r2", "o1r3", "o1r12", "o1r13", "o1r23", "o1r123", "o2ro", "o2r1", "o2r2", "o2r3", "o2r12", "o2r13", "o2r23", "o2r123", "o3"))
  )
  return (transition_matrix)
}

P <- make_markov_from_player("Shohei Ohtani", "LAA")
View(P)

simulateMarkovChain <- function(transitionMatrix, specialMatrix, initialState) {
  # Initialize the transition count matrix with zeros
  transitionCountMatrix <- matrix(0, nrow = 25, ncol = 25)
  
  # Convert initial state to a probability vector if it's not
  if(length(initialState) == 1) {
    currentState <- rep(0, 25)
    currentState[initialState] <- 1
  } else {
    currentState <- initialState
  }
  
  # The current state index
  currentStateIndex <- which(currentState == max(currentState))
  
  # Flag to indicate if the first transition has occurred
  firstTransitionDone <- FALSE
  
  # Run the simulation until it reaches the absorbing state (25)
  while(currentStateIndex != 25) {
    # Determine the next state based on the transition matrix
    #print(currentStateIndex)
    if(!firstTransitionDone) {
      nextStateIndex <- sample(1:25, 1, prob = specialMatrix[currentStateIndex, ])
      firstTransitionDone <- TRUE
    } else {
      nextStateIndex <- sample(1:25, 1, prob = transitionMatrix[currentStateIndex, ])
    }
    
    # Update the transition count matrix
    transitionCountMatrix[currentStateIndex, nextStateIndex] <- transitionCountMatrix[currentStateIndex, nextStateIndex] + 1
    
    # Update the current state
    currentStateIndex <- nextStateIndex
  }
  
  score = transitionCountMatrix[1, 1] + 2 * transitionCountMatrix[2, 1] + transitionCountMatrix[2, 3] + transitionCountMatrix[2, 4] + 2*transitionCountMatrix[3, 1] + transitionCountMatrix[3, 2] + transitionCountMatrix[3, 3] + transitionCountMatrix[3, 4] +2*transitionCountMatrix[4, 1] + transitionCountMatrix[4, 2] + transitionCountMatrix[4, 3] + transitionCountMatrix[4, 4] + 3*transitionCountMatrix[5, 1] + 2*transitionCountMatrix[5, 3] + 2*transitionCountMatrix[5, 4] + transitionCountMatrix[5, 5] + transitionCountMatrix[5, 6]+transitionCountMatrix[5, 7] + 3*transitionCountMatrix[6, 1] + 2*transitionCountMatrix[6, 3] + 2*transitionCountMatrix[6, 4] + transitionCountMatrix[6, 5] + transitionCountMatrix[6, 6]+transitionCountMatrix[6, 7] + 3*transitionCountMatrix[7, 1] + 2*transitionCountMatrix[7, 2]+ 2*transitionCountMatrix[7, 3] + 2*transitionCountMatrix[7, 4] + transitionCountMatrix[7, 5] + transitionCountMatrix[7, 6]+transitionCountMatrix[7, 7] + 4*transitionCountMatrix[8, 1] + 3*transitionCountMatrix[8, 3] + 3*transitionCountMatrix[8, 4] + 2*transitionCountMatrix[8, 5] + 2*transitionCountMatrix[8, 6] + 2*transitionCountMatrix[8, 7] + transitionCountMatrix[8, 8] + transitionCountMatrix[9, 9] + 2 * transitionCountMatrix[10, 9] + transitionCountMatrix[10, 11] + transitionCountMatrix[10, 12] + 2*transitionCountMatrix[11, 9] + transitionCountMatrix[11, 10] + transitionCountMatrix[11, 11] + transitionCountMatrix[11, 12] +2*transitionCountMatrix[12, 9] + transitionCountMatrix[12, 10] + transitionCountMatrix[12, 11] + transitionCountMatrix[12, 12] + 3*transitionCountMatrix[13, 9] + 2*transitionCountMatrix[13, 11] + 2*transitionCountMatrix[13, 12] + transitionCountMatrix[13, 13] + transitionCountMatrix[13, 14]+transitionCountMatrix[13, 15] + 3*transitionCountMatrix[14, 9] + 2*transitionCountMatrix[14, 11] + 2*transitionCountMatrix[14, 12] + transitionCountMatrix[14, 13] + transitionCountMatrix[14, 14]+transitionCountMatrix[14, 15] + 3*transitionCountMatrix[15, 9] + 2*transitionCountMatrix[15, 10]+ 2*transitionCountMatrix[15, 11] + 2*transitionCountMatrix[15, 12] + transitionCountMatrix[15, 13] + transitionCountMatrix[15, 14]+transitionCountMatrix[15, 15] + 4*transitionCountMatrix[16, 9] + 3*transitionCountMatrix[16, 11] + 3*transitionCountMatrix[16, 12] + 2*transitionCountMatrix[16, 13] + 2*transitionCountMatrix[16, 14] + 2*transitionCountMatrix[16, 15] + transitionCountMatrix[16, 16] + transitionCountMatrix[17, 17] + 2 * transitionCountMatrix[18, 17] + transitionCountMatrix[18, 19] + transitionCountMatrix[18, 20] + 2*transitionCountMatrix[19, 17] + transitionCountMatrix[19, 18] + transitionCountMatrix[19, 19] + transitionCountMatrix[19, 20] +2*transitionCountMatrix[20, 17] + transitionCountMatrix[20, 18] + transitionCountMatrix[20, 19] + transitionCountMatrix[20, 20] + 3*transitionCountMatrix[21, 17] + 2*transitionCountMatrix[21, 19] + 2*transitionCountMatrix[21, 20] + transitionCountMatrix[21, 21] + transitionCountMatrix[21, 22]+transitionCountMatrix[21, 23] + 3*transitionCountMatrix[22, 17] + 2*transitionCountMatrix[22, 19] + 2*transitionCountMatrix[22, 20] + transitionCountMatrix[22, 21] + transitionCountMatrix[22, 22]+transitionCountMatrix[22, 23] + 3*transitionCountMatrix[23, 17] + 2*transitionCountMatrix[23, 18]+ 2*transitionCountMatrix[23, 19] + 2*transitionCountMatrix[23, 20] + transitionCountMatrix[23, 21] + transitionCountMatrix[23, 22]+transitionCountMatrix[23, 23] + 4*transitionCountMatrix[24, 17] + 3*transitionCountMatrix[24, 19] + 3*transitionCountMatrix[24, 20] + 2*transitionCountMatrix[24, 21] + 2*transitionCountMatrix[24, 22] + 2*transitionCountMatrix[24, 23] + transitionCountMatrix[24, 24]
  return(score)
}


I <- 1
simulateMarkovChain(P, P, I)


##################
#Analysis with arraez vs luis robert jr

score_counter_arraez <- c()
score_counter_robertjr <- c()
P_arraez <- make_markov_from_player("Luis Arraez", "MIA")
P_robertjr <- make_markov_from_player("Luis Robert Jr", "CHW")
P <- make_markov_from_player("Average", "Average")

for (i in c(1:10000)) {
  score <- simulateMarkovChain(P, P_arraez, 19)
  score_counter_arraez <- c(score_counter_arraez, score)
}

for (i in c(1:10000)) {
  score <- simulateMarkovChain(P, P_robertjr, 19)
  score_counter_robertjr <- c(score_counter_robertjr, score)
}

# Step 2: Calculate probabilities
data_frame <- as.data.frame(table(score_counter_arraez)) %>% 
  mutate(probability = Freq / sum(Freq))

# Step 3: Calculate 95% confidence intervals
data_frame <- data_frame %>%
  mutate(lower_ci = qbinom(0.025, sum(Freq), probability) / sum(Freq),
         upper_ci = qbinom(0.975, sum(Freq), probability) / sum(Freq))

# Step 4: Plot with ggplot2
ggplot(data_frame, aes(x = as.factor(score_counter_arraez), y = probability)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .2, color = "black") +
  labs(x = "Value", y = "Probability") +
  ggtitle("Distribution of Scores with 95% Confidence Intervals for Luis Arraez")


mean(score_counter_arraez)
mean(score_counter_arraez>0)
mean(score_counter_arraez>1)
mean(score_counter_arraez>2)
var(score_counter_arraez)
sqrt(var(score_counter_arraez))

# Step 2: Calculate probabilities
data_frame <- as.data.frame(table(score_counter_robertjr)) %>% 
  mutate(probability = Freq / sum(Freq))

# Step 3: Calculate 95% confidence intervals
data_frame <- data_frame %>%
  mutate(lower_ci = qbinom(0.025, sum(Freq), probability) / sum(Freq),
         upper_ci = qbinom(0.975, sum(Freq), probability) / sum(Freq))

# Step 4: Plot with ggplot2
ggplot(data_frame, aes(x = as.factor(score_counter_robertjr), y = probability)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .2, color = "grey") +
  labs(x = "Value", y = "Probability") +
  ggtitle("Distribution of Scores with 95% Confidence Intervals for Luis Robert Jr")

mean(score_counter_robertjr)
mean(score_counter_robertjr>0)
mean(score_counter_robertjr>1)
mean(score_counter_robertjr>2)
var(score_counter_robertjr)
sqrt(var(score_counter_robertjr))


##################
#Analysis with Bo Bichette vs Kyle Schwarber

score_counter_arraez <- c()
score_counter_robertjr <- c()
P_arraez <- make_markov_from_player("Bo Bichette", "TOR")
P_robertjr <- make_markov_from_player("Kyle Schwarber", "PHI")
P <- make_markov_from_player("Average", "Average")

for (i in c(1:10000)) {
  score <- simulateMarkovChain(P, P_arraez, 15)
  score_counter_arraez <- c(score_counter_arraez, score)
}

for (i in c(1:10000)) {
  score <- simulateMarkovChain(P, P_robertjr, 15)
  score_counter_robertjr <- c(score_counter_robertjr, score)
}

# Step 2: Calculate probabilities
data_frame <- as.data.frame(table(score_counter_arraez)) %>% 
  mutate(probability = Freq / sum(Freq))

# Step 3: Calculate 95% confidence intervals
data_frame <- data_frame %>%
  mutate(lower_ci = qbinom(0.025, sum(Freq), probability) / sum(Freq),
         upper_ci = qbinom(0.975, sum(Freq), probability) / sum(Freq))

# Step 4: Plot with ggplot2
ggplot(data_frame, aes(x = as.factor(score_counter_arraez), y = probability)) +
  geom_bar(stat = "identity", fill = "#134A8E") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .2, color = "#E8291C") +
  labs(x = "Value", y = "Probability") +
  ggtitle("Distribution of Scores with 95% Confidence Intervals for Bo Bichette")


mean(score_counter_arraez)
mean(score_counter_arraez>0)
mean(score_counter_arraez>1)
mean(score_counter_arraez>2)
var(score_counter_arraez)
sqrt(var(score_counter_arraez))

# Step 2: Calculate probabilities
data_frame <- as.data.frame(table(score_counter_robertjr)) %>% 
  mutate(probability = Freq / sum(Freq))

# Step 3: Calculate 95% confidence intervals
data_frame <- data_frame %>%
  mutate(lower_ci = qbinom(0.025, sum(Freq), probability) / sum(Freq),
         upper_ci = qbinom(0.975, sum(Freq), probability) / sum(Freq))

# Step 4: Plot with ggplot2
ggplot(data_frame, aes(x = as.factor(score_counter_robertjr), y = probability)) +
  geom_bar(stat = "identity", fill = "#E81828") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .2, color = "#FFE6D0") +
  labs(x = "Value", y = "Probability") +
  ggtitle("Distribution of Scores with 95% Confidence Intervals for Kyle Schwarber")

mean(score_counter_robertjr)
mean(score_counter_robertjr>0)
mean(score_counter_robertjr>1)
mean(score_counter_robertjr>2)
var(score_counter_robertjr)
sqrt(var(score_counter_robertjr))

##################
#Impact of each variable
make_markov_from_values <- function(s, d, t, hr, w, o) {
  transition_matrix <- matrix(
    c(hr, s+w, d, t, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, 0, d*(0.4143822), t, w+s*(0.6712757), w+s*(1-0.6712757), d*(1-0.4143822), 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, s*(0.6317177), d, t, w, s*(1-0.6317177), 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, s, d, t, 0, 0, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, 0, d*(0.4143822), t, s*(0.6317177)*(0.6712757), s*(0.6317177)*(1-0.6712757), d*(1-0.4143822), w+s*(1-0.6317177), 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, 0, d*(0.4143822), t, s*(0.6712757), s*(1-0.6712757), d*(1-0.4143822), w, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, s*(0.6317177), d, t, 0, s*(1-0.6317177), 0, w, 0, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      hr, 0, d*(0.4143822), t, s*(0.6317177)*(0.6712757), s*(0.6317177)*(1-0.6712757), d*(1-0.4143822), s*(1-0.6317177), 0, 0, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, s+w, d, t, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, w+s*(0.6712757), w+s*(1-0.6712757), d*(1-0.4143822), 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, s*(0.6317177), d, t, w, s*(1-0.6317177), 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, s, d, t, 0, 0, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, s*(0.6317177)*(0.6712757), s*(0.6317177)*(1-0.6712757), d*(1-0.4143822), w+s*(1-0.6317177), 0, 0, 0, 0, o, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, s*(0.6712757), s*(1-0.6712757), d*(1-0.4143822), w, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, s*(0.6317177), d, t, 0, s*(1-0.6317177), 0, w, 0, 0, 0, 0, 0, 0, o, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, s*(0.6317177)*(0.6712757), s*(0.6317177)*(1-0.6712757), d*(1-0.4143822), s*(1-0.6317177), 0, 0, 0, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, s+w, d, t, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, w+s*(0.6712757), w+s*(1-0.6712757), d*(1-0.4143822), 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, s*(0.6317177), d, t, w, s*(1-0.6317177), 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, s, d, t, 0, 0, 0, 0, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, s*(0.6317177)*(0.6712757), s*(0.6317177)*(1-0.6712757), d*(1-0.4143822), w+s*(1-0.6317177), o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, s*(0.6712757), s*(1-0.6712757), d*(1-0.4143822), w, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, s*(0.6317177), d, t, 0, s*(1-0.6317177), 0, w, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, hr, 0, d*(0.4143822), t, s*(0.6317177)*(0.6712757), s*(0.6317177)*(1-0.6712757), d*(1-0.4143822), s*(1-0.6317177), o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    nrow = 25, byrow = TRUE,
    dimnames = list(c("o0ro", "o0r1", "o0r2", "o0r3", "o0r12", "o0r13", "o0r23", "o0r123", "o1ro", "o1r1", "o1r2", "o1r3", "o1r12", "o1r13", "o1r23", "o1r123", "o2ro", "o2r1", "o2r2", "o2r3", "o2r12", "o2r13", "o2r23", "o2r123", "o3"), c("o0ro", "o0r1", "o0r2", "o0r3", "o0r12", "o0r13", "o0r23", "o0r123", "o1ro", "o1r1", "o1r2", "o1r3", "o1r12", "o1r13", "o1r23", "o1r123", "o2ro", "o2r1", "o2r2", "o2r3", "o2r12", "o2r13", "o2r23", "o2r123", "o3"))
  )
  return (transition_matrix)
}

#leadoff
#Single
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
singlep_league_leader <- 0.25931929 #Arraez
singlep_league_average <- 0.14139291
increment <- (singlep_league_leader - singlep_league_average)/20
score_effect_single_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  s <- singlep_league_average + (j - 1) * increment  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(s, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 1)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_single_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_s <- data.frame(j_index = 1:21,
                   average_score = sapply(score_effect_single_1, mean))
 
# # Plot using ggplot
# ggplot(df_j_s, aes(x = j_index, y = average_score)) +
#   geom_line() +
#   labs(x = "Index (j)", y = "Average Score", title = "Average Score vs. Index (j)")

#double
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
doublep_league_leader <- 0.08082192 #Freeman
doublep_league_average <- 0.04469213
increment_d <- (doublep_league_leader - doublep_league_average)/20
score_effect_double_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  d <- doublep_league_average + (j - 1) * increment_d  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, d, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 1)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_double_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_d <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_double_1, mean))
#triple
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
triplep_league_leader <- 0.015850144 #whitt jr
triplep_league_average <- 0.003867379
increment_t <- (triplep_league_leader - triplep_league_average)/20
score_effect_triple_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  t <- triplep_league_average + (j - 1) * increment_t  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, 0.04469213, t, 0.03187329, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 1)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_triple_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_t <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_triple_1, mean))

#hr
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
hrp_league_leader <- 0.07500000 #Olson
hrp_league_average <- 0.03187329
increment_hr <- (hrp_league_leader - hrp_league_average)/20
score_effect_hr_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  hr <- hrp_league_average + (j - 1) * increment_hr  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, hr, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 1)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_hr_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_hr <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_hr_1, mean))

#walks
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
wp_league_leader <- 0.20480226 #Soto
wp_league_average <- 0.09997067
increment_w <- (wp_league_leader - wp_league_average)/20
score_effect_w_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  w <- wp_league_average + (j - 1) * increment_w  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, w, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 1)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_w_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_w <- data.frame(j_index = 1:21,
                      average_score = sapply(score_effect_w_1, mean))

# # Plot using ggplot
# ggplot(df_j_d, aes(x = j_index, y = average_score)) +
#   geom_line() +
#   labs(x = "Index (j)", y = "Average Score", title = "Average Score vs. Index (j)")

# Add an identifier column to each data frame
df_j_s$type <- 'Singles'
df_j_d$type <- 'Doubles'
df_j_t$type <- 'Triples'
df_j_hr$type <- 'HRs'
df_j_w$type <- 'Walks'

# Combine the data frames
df_combined <- rbind(df_j_s, df_j_d, df_j_t, df_j_hr, df_j_w)

# Assuming df_combined is your dataframe and 'type' is the column indicating Singles, Doubles, etc.
df_combined$type <- factor(df_combined$type, levels = c("Singles", "Doubles", "Triples", "HRs", "Walks"))

# Now, plot with ggplot using the modified dataframe
ggplot(df_combined, aes(x = j_index, y = average_score, color = type)) +
  geom_line() +
  labs(x = "Index (j)", y = "Average Score", title = "Average Score vs. Index (j), Leading-off") +
  scale_color_manual(values = c("Singles" = "blue", "Doubles" = "red", "Triples" = "green", "HRs" = "orange", "Walks" = "pink")) +
  theme_minimal() +
  guides(color = guide_legend(title = "Type", order = 1)) # This may not be necessary since the factor level adjustment should already control the legend order


#df_var <- data.frame(s = singlep_league_average + (1:20 - 1) * increment,
#                 score_var = sapply(score_effect_single_1, var))

# Plot using ggplot
#ggplot(df_var, aes(x = s, y = score_var)) +
#  geom_line() +
#  labs(x = "single probability", y = "Score Variance", title = "Score Variance vs. single probability")

#11
#Single
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
singlep_league_leader <- 0.25931929 #Arraez
singlep_league_average <- 0.14139291
increment <- (singlep_league_leader - singlep_league_average)/20
score_effect_single_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  s <- singlep_league_average + (j - 1) * increment  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(s, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 11)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_single_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_s <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_single_1, mean))

# # Plot using ggplot
# ggplot(df_j_s, aes(x = j_index, y = average_score)) +
#   geom_line() +
#   labs(x = "Index (j)", y = "Average Score", title = "Average Score vs. Index (j)")

#double
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
doublep_league_leader <- 0.08082192 #Freeman
doublep_league_average <- 0.04469213
increment_d <- (doublep_league_leader - doublep_league_average)/20
score_effect_double_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  d <- doublep_league_average + (j - 1) * increment_d  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, d, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 11)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_double_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_d <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_double_1, mean))
#triple
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
triplep_league_leader <- 0.015850144 #whitt jr
triplep_league_average <- 0.003867379
increment_t <- (triplep_league_leader - triplep_league_average)/20
score_effect_triple_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  t <- triplep_league_average + (j - 1) * increment_t  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, 0.04469213, t, 0.03187329, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 11)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_triple_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_t <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_triple_1, mean))

#hr
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
hrp_league_leader <- 0.07500000 #Olson
hrp_league_average <- 0.03187329
increment_hr <- (hrp_league_leader - hrp_league_average)/20
score_effect_hr_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  hr <- hrp_league_average + (j - 1) * increment_hr  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, hr, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 11)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_hr_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_hr <- data.frame(j_index = 1:21,
                      average_score = sapply(score_effect_hr_1, mean))

#walks
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
wp_league_leader <- 0.20480226 #Soto
wp_league_average <- 0.09997067
increment_w <- (wp_league_leader - wp_league_average)/20
score_effect_w_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  w <- wp_league_average + (j - 1) * increment_w  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, w, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 11)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_w_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_w <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_w_1, mean))

# # Plot using ggplot
# ggplot(df_j_d, aes(x = j_index, y = average_score)) +
#   geom_line() +
#   labs(x = "Index (j)", y = "Average Score", title = "Average Score vs. Index (j)")

# Add an identifier column to each data frame
df_j_s$type <- 'Singles'
df_j_d$type <- 'Doubles'
df_j_t$type <- 'Triples'
df_j_hr$type <- 'HRs'
df_j_w$type <- 'Walks'

# Combine the data frames
df_combined <- rbind(df_j_s, df_j_d, df_j_t, df_j_hr, df_j_w)

# Assuming df_combined is your dataframe and 'type' is the column indicating Singles, Doubles, etc.
df_combined$type <- factor(df_combined$type, levels = c("Singles", "Doubles", "Triples", "HRs", "Walks"))

# Now, plot with ggplot using the modified dataframe
ggplot(df_combined, aes(x = j_index, y = average_score, color = type)) +
  geom_line() +
  labs(x = "Index (j)", y = "Average Score", title = "Average Score vs. Index (j), 1 out runner on 2nd") +
  scale_color_manual(values = c("Singles" = "blue", "Doubles" = "red", "Triples" = "green", "HRs" = "orange", "Walks" = "pink")) +
  theme_minimal() +
  guides(color = guide_legend(title = "Type", order = 1)) # This may not be necessary since the factor level adjustment should already control the legend order


#19
#Single
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
singlep_league_leader <- 0.25931929 #Arraez
singlep_league_average <- 0.14139291
increment <- (singlep_league_leader - singlep_league_average)/20
score_effect_single_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  s <- singlep_league_average + (j - 1) * increment  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(s, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 8)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_single_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_s <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_single_1, mean))

# # Plot using ggplot
# ggplot(df_j_s, aes(x = j_index, y = average_score)) +
#   geom_line() +
#   labs(x = "Index (j)", y = "Average Score", title = "Average Score vs. Index (j)")

#double
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
doublep_league_leader <- 0.08082192 #Freeman
doublep_league_average <- 0.04469213
increment_d <- (doublep_league_leader - doublep_league_average)/20
score_effect_double_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  d <- doublep_league_average + (j - 1) * increment_d  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, d, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 8)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_double_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_d <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_double_1, mean))
#triple
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
triplep_league_leader <- 0.015850144 #whitt jr
triplep_league_average <- 0.003867379
increment_t <- (triplep_league_leader - triplep_league_average)/20
score_effect_triple_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  t <- triplep_league_average + (j - 1) * increment_t  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, 0.04469213, t, 0.03187329, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 8)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_triple_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_t <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_triple_1, mean))

#hr
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
hrp_league_leader <- 0.07500000 #Olson
hrp_league_average <- 0.03187329
increment_hr <- (hrp_league_leader - hrp_league_average)/20
score_effect_hr_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  hr <- hrp_league_average + (j - 1) * increment_hr  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, hr, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 8)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_hr_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_hr <- data.frame(j_index = 1:21,
                      average_score = sapply(score_effect_hr_1, mean))

#walks
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
wp_league_leader <- 0.20480226 #Soto
wp_league_average <- 0.09997067
increment_w <- (wp_league_leader - wp_league_average)/20
score_effect_w_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  w <- wp_league_average + (j - 1) * increment_w  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, w, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 8)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_w_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_w <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_w_1, mean))

# # Plot using ggplot
# ggplot(df_j_d, aes(x = j_index, y = average_score)) +
#   geom_line() +
#   labs(x = "Index (j)", y = "Average Score", title = "Average Score vs. Index (j)")

# Add an identifier column to each data frame
df_j_s$type <- 'Singles'
df_j_d$type <- 'Doubles'
df_j_t$type <- 'Triples'
df_j_hr$type <- 'HRs'
df_j_w$type <- 'Walks'

# Combine the data frames
df_combined <- rbind(df_j_s, df_j_d, df_j_t, df_j_hr, df_j_w)

# Assuming df_combined is your dataframe and 'type' is the column indicating Singles, Doubles, etc.
df_combined$type <- factor(df_combined$type, levels = c("Singles", "Doubles", "Triples", "HRs", "Walks"))

# Now, plot with ggplot using the modified dataframe
ggplot(df_combined, aes(x = j_index, y = average_score, color = type)) +
  geom_line() +
  labs(x = "Index (j)", y = "Average Score", title = "Average Score vs. Index (j), 0 out bases loaded") +
  scale_color_manual(values = c("Singles" = "blue", "Doubles" = "red", "Triples" = "green", "HRs" = "orange", "Walks" = "pink")) +
  theme_minimal() +
  guides(color = guide_legend(title = "Type", order = 1)) # This may not be necessary since the factor level adjustment should already control the legend order



#####
#variance
#Single
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
singlep_league_leader <- 0.25931929 #Arraez
singlep_league_average <- 0.14139291
increment <- (singlep_league_leader - singlep_league_average)/20
score_effect_single_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  s <- singlep_league_average + (j - 1) * increment  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(s, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 1)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_single_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_s <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_single_1, var))

# # Plot using ggplot
# ggplot(df_j_s, aes(x = j_index, y = average_score)) +
#   geom_line() +
#   labs(x = "Index (j)", y = "Average Score", title = "Average Score vs. Index (j)")

#double
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
doublep_league_leader <- 0.08082192 #Freeman
doublep_league_average <- 0.04469213
increment_d <- (doublep_league_leader - doublep_league_average)/20
score_effect_double_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  d <- doublep_league_average + (j - 1) * increment_d  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, d, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 1)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_double_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_d <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_double_1, var))
#triple
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
triplep_league_leader <- 0.015850144 #whitt jr
triplep_league_average <- 0.003867379
increment_t <- (triplep_league_leader - triplep_league_average)/20
score_effect_triple_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  t <- triplep_league_average + (j - 1) * increment_t  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, 0.04469213, t, 0.03187329, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 1)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_triple_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_t <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_triple_1, var))

#hr
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
hrp_league_leader <- 0.07500000 #Olson
hrp_league_average <- 0.03187329
increment_hr <- (hrp_league_leader - hrp_league_average)/20
score_effect_hr_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  hr <- hrp_league_average + (j - 1) * increment_hr  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, hr, 0.09997067, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 1)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_hr_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_hr <- data.frame(j_index = 1:21,
                      average_score = sapply(score_effect_hr_1, var))

#walks
P_average <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, 0.09997067, 0.6782036)
wp_league_leader <- 0.20480226 #Soto
wp_league_average <- 0.09997067
increment_w <- (wp_league_leader - wp_league_average)/20
score_effect_w_1 <- list()  # Initialize a list to store the arrays for each value of s
for (j in 1:21) {
  w <- wp_league_average + (j - 1) * increment_w  # Update s based on the loop index j
  P_in_question <- make_markov_from_values(0.14139291, 0.04469213, 0.003867379, 0.03187329, w, 0.6782036)
  scores <- numeric(0)  # Initialize a vector to store scores for the current value of s
  for (i in 1:20000) {
    score <- simulateMarkovChain(P, P_in_question, 1)
    scores <- c(scores, score)  # Store the score for the current iteration
  }
  score_effect_w_1[[j]] <- scores  # Store scores for current s in the list
}

# Create a data frame with j values (1 to 20) and corresponding average scores
df_j_w <- data.frame(j_index = 1:21,
                     average_score = sapply(score_effect_w_1, var))

# # Plot using ggplot
# ggplot(df_j_d, aes(x = j_index, y = average_score)) +
#   geom_line() +
#   labs(x = "Index (j)", y = "Average Score", title = "Average Score vs. Index (j)")

# Add an identifier column to each data frame
df_j_s$type <- 'Singles'
df_j_d$type <- 'Doubles'
df_j_t$type <- 'Triples'
df_j_hr$type <- 'HRs'
df_j_w$type <- 'Walks'

# Combine the data frames
df_combined <- rbind(df_j_s, df_j_d, df_j_t, df_j_hr, df_j_w)

# Assuming df_combined is your dataframe and 'type' is the column indicating Singles, Doubles, etc.
df_combined$type <- factor(df_combined$type, levels = c("Singles", "Doubles", "Triples", "HRs", "Walks"))

# Now, plot with ggplot using the modified dataframe
ggplot(df_combined, aes(x = j_index, y = average_score, color = type)) +
  geom_line() +
  labs(x = "Index (j)", y = "Score Variance", title = "Score Variance vs. Index (j), Leading-off") +
  scale_color_manual(values = c("Singles" = "blue", "Doubles" = "red", "Triples" = "green", "HRs" = "orange", "Walks" = "pink")) +
  theme_minimal() +
  guides(color = guide_legend(title = "Type", order = 1)) # This may not be necessary since the factor level adjustment should already control the legend order





##################
#Optimal Hitting Order

simulateMarkovChain_order <- function(transitionMatrix, list_of_hitters, initialState) {
  # Initialize the transition count matrix with zeros
  transitionCountMatrix <- matrix(0, nrow = 25, ncol = 25)
  
  # Convert initial state to a probability vector if it's not
  if(length(initialState) == 1) {
    currentState <- rep(0, 25)
    currentState[initialState] <- 1
  } else {
    currentState <- initialState
  }
  
  # The current state index
  currentStateIndex <- which(currentState == max(currentState))
  
  # Flag to indicate if the first transition has occurred
  Tcount <- 1
  
  # Run the simulation until it reaches the absorbing state (25)
  while(currentStateIndex != 25) {
    # Determine the next state based on the transition matrix
    #print(currentStateIndex)
    #print(Tcount)
    if (Tcount %% 9 == 1) {
      nextStateIndex <- sample(1:25, 1, prob = list_of_hitters[[1]][currentStateIndex, ])
      Tcount <- Tcount + 1
    } else if (Tcount %% 9 == 2){
      nextStateIndex <- sample(1:25, 1, prob = list_of_hitters[[2]][currentStateIndex, ])
      Tcount <- Tcount + 1
    } else if (Tcount %% 9 == 3) {
      nextStateIndex <- sample(1:25, 1, prob = list_of_hitters[[3]][currentStateIndex, ])
      Tcount <- Tcount + 1
    } else if (Tcount %% 9 == 4) {
      nextStateIndex <- sample(1:25, 1, prob = list_of_hitters[[4]][currentStateIndex, ])
      Tcount <- Tcount + 1
    } else {
      nextStateIndex <- sample(1:25, 1, prob = transitionMatrix[currentStateIndex, ])
      Tcount <- Tcount + 1
    }
    
    # Update the transition count matrix
    transitionCountMatrix[currentStateIndex, nextStateIndex] <- transitionCountMatrix[currentStateIndex, nextStateIndex] + 1
    
    # Update the current state
    currentStateIndex <- nextStateIndex
  }
  
  score = transitionCountMatrix[1, 1] + 2 * transitionCountMatrix[2, 1] + transitionCountMatrix[2, 3] + transitionCountMatrix[2, 4] + 2*transitionCountMatrix[3, 1] + transitionCountMatrix[3, 2] + transitionCountMatrix[3, 3] + transitionCountMatrix[3, 4] +2*transitionCountMatrix[4, 1] + transitionCountMatrix[4, 2] + transitionCountMatrix[4, 3] + transitionCountMatrix[4, 4] + 3*transitionCountMatrix[5, 1] + 2*transitionCountMatrix[5, 3] + 2*transitionCountMatrix[5, 4] + transitionCountMatrix[5, 5] + transitionCountMatrix[5, 6]+transitionCountMatrix[5, 7] + 3*transitionCountMatrix[6, 1] + 2*transitionCountMatrix[6, 3] + 2*transitionCountMatrix[6, 4] + transitionCountMatrix[6, 5] + transitionCountMatrix[6, 6]+transitionCountMatrix[6, 7] + 3*transitionCountMatrix[7, 1] + 2*transitionCountMatrix[7, 2]+ 2*transitionCountMatrix[7, 3] + 2*transitionCountMatrix[7, 4] + transitionCountMatrix[7, 5] + transitionCountMatrix[7, 6]+transitionCountMatrix[7, 7] + 4*transitionCountMatrix[8, 1] + 3*transitionCountMatrix[8, 3] + 3*transitionCountMatrix[8, 4] + 2*transitionCountMatrix[8, 5] + 2*transitionCountMatrix[8, 6] + 2*transitionCountMatrix[8, 7] + transitionCountMatrix[8, 8] + transitionCountMatrix[9, 9] + 2 * transitionCountMatrix[10, 9] + transitionCountMatrix[10, 11] + transitionCountMatrix[10, 12] + 2*transitionCountMatrix[11, 9] + transitionCountMatrix[11, 10] + transitionCountMatrix[11, 11] + transitionCountMatrix[11, 12] +2*transitionCountMatrix[12, 9] + transitionCountMatrix[12, 10] + transitionCountMatrix[12, 11] + transitionCountMatrix[12, 12] + 3*transitionCountMatrix[13, 9] + 2*transitionCountMatrix[13, 11] + 2*transitionCountMatrix[13, 12] + transitionCountMatrix[13, 13] + transitionCountMatrix[13, 14]+transitionCountMatrix[13, 15] + 3*transitionCountMatrix[14, 9] + 2*transitionCountMatrix[14, 11] + 2*transitionCountMatrix[14, 12] + transitionCountMatrix[14, 13] + transitionCountMatrix[14, 14]+transitionCountMatrix[14, 15] + 3*transitionCountMatrix[15, 9] + 2*transitionCountMatrix[15, 10]+ 2*transitionCountMatrix[15, 11] + 2*transitionCountMatrix[15, 12] + transitionCountMatrix[15, 13] + transitionCountMatrix[15, 14]+transitionCountMatrix[15, 15] + 4*transitionCountMatrix[16, 9] + 3*transitionCountMatrix[16, 11] + 3*transitionCountMatrix[16, 12] + 2*transitionCountMatrix[16, 13] + 2*transitionCountMatrix[16, 14] + 2*transitionCountMatrix[16, 15] + transitionCountMatrix[16, 16] + transitionCountMatrix[17, 17] + 2 * transitionCountMatrix[18, 17] + transitionCountMatrix[18, 19] + transitionCountMatrix[18, 20] + 2*transitionCountMatrix[19, 17] + transitionCountMatrix[19, 18] + transitionCountMatrix[19, 19] + transitionCountMatrix[19, 20] +2*transitionCountMatrix[20, 17] + transitionCountMatrix[20, 18] + transitionCountMatrix[20, 19] + transitionCountMatrix[20, 20] + 3*transitionCountMatrix[21, 17] + 2*transitionCountMatrix[21, 19] + 2*transitionCountMatrix[21, 20] + transitionCountMatrix[21, 21] + transitionCountMatrix[21, 22]+transitionCountMatrix[21, 23] + 3*transitionCountMatrix[22, 17] + 2*transitionCountMatrix[22, 19] + 2*transitionCountMatrix[22, 20] + transitionCountMatrix[22, 21] + transitionCountMatrix[22, 22]+transitionCountMatrix[22, 23] + 3*transitionCountMatrix[23, 17] + 2*transitionCountMatrix[23, 18]+ 2*transitionCountMatrix[23, 19] + 2*transitionCountMatrix[23, 20] + transitionCountMatrix[23, 21] + transitionCountMatrix[23, 22]+transitionCountMatrix[23, 23] + 4*transitionCountMatrix[24, 17] + 3*transitionCountMatrix[24, 19] + 3*transitionCountMatrix[24, 20] + 2*transitionCountMatrix[24, 21] + 2*transitionCountMatrix[24, 22] + 2*transitionCountMatrix[24, 23] + transitionCountMatrix[24, 24]
  return(score)
}

#Test simulateMarkovChain_order
#P_arraez <- make_markov_from_player("Luis Arraez", "MIA")
#P_robertjr <- make_markov_from_player("Luis Robert Jr", "CHW")
P_bichette <- make_markov_from_player("Bo Bichette", "TOR")
P_schwarber <- make_markov_from_player("Kyle Schwarber", "PHI")
P_yelich <- make_markov_from_player("Christian Yelich", "MIL")
P <- make_markov_from_player("Average", "Average")
list_of_matrices <- list()
list_of_matrices[[1]] <- P_bichette
list_of_matrices[[2]] <- P_schwarber
list_of_matrices[[3]] <- P_yelich
list_of_matrices[[4]] <- P
simulateMarkovChain_order(P, list_of_matrices, 1)

# Generate all possible lineups (4^4 = 256 combinations)
all_lineups <- expand.grid(1:4, 1:4, 1:4, 1:4)

# Initialize a list to store the simulation results for each lineup
simulation_results <- vector("list", length = nrow(all_lineups))


# Loop through each lineup combination
for (i in 1:nrow(all_lineups)) {
  lineup_indices <- unlist(all_lineups[i, ], use.names = FALSE)
  
  # Subset list_of_matrices using the numeric vector of indices
  lineup_matrices <- list_of_matrices[lineup_indices]
  
  # Initialize a numeric vector to store the results of 10,000 simulations for this lineup
  lineup_results <- numeric(10000)
  
  # Run 10,000 simulations for the current lineup
  for (j in 1:10000) {
    # Assuming simulateMarkovChain_order returns a single numeric value
    lineup_results[j] <- simulateMarkovChain_order(P, lineup_matrices, 1)
  }
  
  # Store the simulation results for this lineup
  simulation_results[[i]] <- lineup_results
}

# At this point, simulation_results contains the results of 10,000 simulations for each of the 256 lineups
# Assuming you have the simulation_results list and the all_lineups dataframe from before

# Step 1: Calculate the average runs scored for each lineup
average_runs <- sapply(simulation_results, mean)

# Create a dataframe that combines lineup configurations with their average runs
lineup_scores_df <- data.frame(lineup_configuration = apply(all_lineups, 1, paste, collapse = "-"), 
                               average_runs = average_runs)

# Step 2: Sort the dataframe by average runs scored, in descending order
sorted_lineup_scores_df <- lineup_scores_df[order(-lineup_scores_df$average_runs), ]

# Step 3: Select the top 10 lineups
top_10_lineups_df <- head(sorted_lineup_scores_df, 10)
# Get the least 10 lineups
least_10_lineups <- tail(sorted_lineup_scores_df, 10)

# Print the top 10 lineups and their average runs
print(top_10_lineups_df)
print(least_10_lineups)

# Assuming simulation_results is already filled with 10,000 simulations for each of the 256 lineups

# Calculate mean and 95% confidence interval for each lineup
simulation_summary <- lapply(simulation_results, function(scores) {
  mean_score <- mean(scores)
  sd_score <- sd(scores)
  error_margin <- qt(0.975, length(scores)-1) * sd_score / sqrt(length(scores))
  return(c(mean_score, mean_score - error_margin, mean_score + error_margin))
})

# Convert simulation_summary to a data frame
simulation_summary_df <- do.call(rbind, simulation_summary)
colnames(simulation_summary_df) <- c("Mean", "Lower_CI", "Upper_CI")

# Add lineup configuration and average runs to the dataframe
simulation_summary_df <- cbind(apply(all_lineups, 1, paste, collapse = "-"), simulation_summary_df)
colnames(simulation_summary_df)[1] <- "Lineup_Configuration"

# Convert to data frame and change columns to appropriate data types
simulation_summary_df <- as.data.frame(simulation_summary_df)
simulation_summary_df$Mean <- as.numeric(as.character(simulation_summary_df$Mean))
simulation_summary_df$Lower_CI <- as.numeric(as.character(simulation_summary_df$Lower_CI))
simulation_summary_df$Upper_CI <- as.numeric(as.character(simulation_summary_df$Upper_CI))

# Order the data frame by Mean in descending order to get top 10 lineups
simulation_summary_df <- simulation_summary_df[order(-simulation_summary_df$Mean), ]

# Select the top 10 lineups
top_10_lineups_df <- head(simulation_summary_df, 10)

# Assuming top_10_lineups_df is already prepared and contains the top 10 lineups

# Convert Lineup_Configuration to a factor and order it based on Mean scores
top_10_lineups_df$Lineup_Configuration <- factor(top_10_lineups_df$Lineup_Configuration, levels = top_10_lineups_df$Lineup_Configuration[order(-top_10_lineups_df$Mean)])

# Now plot using ggplot2, where the bars will be sorted in descending order of their mean scores
ggplot(top_10_lineups_df, aes(x = Lineup_Configuration, y = Mean)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  labs(title = "Top 10 Lineups with 95% Confidence Interval", x = "Lineup Configuration", y = "Mean Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
