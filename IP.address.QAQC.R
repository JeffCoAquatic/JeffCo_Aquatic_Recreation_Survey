library(dplyr)
library(stringr)
library(lubridate)

# Load data
df <- read.csv("Resident_Survey_Responses_3.31.2025.csv", stringsAsFactors = FALSE)

# Define metadata vs. survey columns
meta_cols <- c("IP.address", "Start.time", "Time.taken", "User.agent", "City.area", "Age", "JeffCo.resident")
survey_cols <- setdiff(names(df), c(meta_cols, "TimeTakenSec"))

# Convert time
parse_time <- function(x) {
  min <- as.numeric(str_extract(x, "\\d+(?=\\s+min)"))
  sec <- as.numeric(str_extract(x, "\\d+(?=\\s+sec)"))
  min[is.na(min)] <- 0
  sec[is.na(sec)] <- 0
  return(min * 60 + sec)
}
df$TimeTakenSec <- parse_time(df$Time.taken)

# Response fingerprint
df$response_fingerprint <- apply(df[, survey_cols], 1, function(x) paste(x, collapse = " | "))

# Count non-null survey responses
df$NumQuestionsAnswered <- rowSums(!is.na(df[, survey_cols]))

# Identify repeated IPs
repeat_ips <- df %>%
  count(IP.address) %>%
  filter(n > 1) %>%
  pull(IP.address)

df <- df %>%
  mutate(Start.time.parsed = parse_date_time(Start.time, orders = c("mdy HM", "ymd HMS", "ymd HM", "mdy HMS")))

summary_table <- df %>%
  filter(IP.address %in% repeat_ips) %>%
  arrange(IP.address, Start.time.parsed) %>%
  group_by(IP.address) %>%
  summarise(
    Number_of_Responses = n(),
    Entries_Under_10min_Apart = sum(difftime(Start.time.parsed, lag(Start.time.parsed)) < 600, na.rm = TRUE),
    Unique_User_Agents = n_distinct(User.agent),
    Mean_Time_Taken_Seconds = mean(TimeTakenSec, na.rm = TRUE),
    Time_Taken_Range_Seconds = paste0(min(TimeTakenSec, na.rm = TRUE), "-", max(TimeTakenSec, na.rm = TRUE)),
    Avg_Questions_Answered = mean(NumQuestionsAnswered, na.rm = TRUE),
    Prop_Duplicate_Responses = mean(duplicated(response_fingerprint)),
    Unique_Response_Ratio = n_distinct(response_fingerprint) / n(),
    Hour_of_Day_STD = sd(hour(Start.time.parsed), na.rm = TRUE),
    All_Mobile_User_Agents = all(str_detect(User.agent, "Android|iPhone|Mobile"))
  )


############################################################################################
# Visualize Data

library(ggplot2)

# Plot duplicate response ratio
ggplot(summary_table, aes(x = reorder(IP.address, -Prop_Duplicate_Responses), y = Prop_Duplicate_Responses)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(title = "Proportion of Duplicate Responses by IP", x = "IP Address", y = "Proportion") +
  coord_flip()

# Plot time taken per IP
ggplot(summary_table, aes(x = reorder(IP.address, -Mean_Time_Taken_Seconds), y = Mean_Time_Taken_Seconds)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Time Taken per IP", x = "IP Address", y = "Seconds") +
  coord_flip()
