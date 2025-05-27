# Load necessary libraries
library(tidyverse)
library(lubridate)

# Read the data
df <- read_csv("ViewingActivity.csv")
head(df)

# Convert Duration to period object
df <- df %>%
  mutate(Duration = hms(Duration))


# Group and summarize
profile_summary <- df %>%
  group_by(`Profile Name`) %>%
  summarise(TotalTime = sum(Duration))

print(profile_summary)

# Convert to seconds for plotting
df <- df %>%
  mutate(Duration_minutes = as.numeric(Duration, units = "mins"))


# Plot
df %>%
  group_by(`Profile Name`) %>%
  summarise(TotalMinutes = sum(Duration_minutes, na.rm = TRUE))


ggplot(df) + 
  geom_boxplot(aes(x = `Profile Name`, y = Duration_minutes))


# Most Watched Titles
df %>%
  count(Title, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(Title, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Top 10 Most Watched Titles",
       x = "Title", y = "Watch Count") +
  theme_minimal()


# Most Watched Devices
unique(df$`Device Type`)
ggplot(df) + geom_bar(aes(x=`Device Type`))


unique(df$Country)
