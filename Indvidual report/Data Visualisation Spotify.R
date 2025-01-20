
# Load necessary libraries
library(ggplot2)
library(dplyr)
install.packages("ggcorrplot")
library(ggcorrplot)
library(tidyverse)
library(VIM)
spotify_data <- read.csv("D:\\Introduction to Data Science\\assessment\\dataset.csv")

# Step 2: Initial exploration
summary(spotify_data)  # Summary statistics
str(spotify_data)      # Structure of the dataset

# Step 3: Check for missing or invalid values
colSums(is.na(spotify_data))  # Check for missing values
spotify_data %>%
  summarise_all(~ sum(. == 0))  # Check for zero values in numeric columns

# Step 4: Handle missing or invalid values
# Replace 0 in `duration_ms` with its median
spotify_data$duration_ms <- ifelse(
  spotify_data$duration_ms == 0,
  median(spotify_data$duration_ms[spotify_data$duration_ms > 0], na.rm = TRUE),
  spotify_data$duration_ms
)

# Replace invalid `time_signature` values (outside 3 to 7) with the mode
spotify_data$time_signature <- ifelse(
  spotify_data$time_signature < 3 | spotify_data$time_signature > 7,
  NA,
  spotify_data$time_signature
)

# Perform KNN imputation for `time_signature`
spotify_data <- kNN(spotify_data, variable = "time_signature", k = 5)

# Step 5: Handle duplicates
sum(duplicated(spotify_data)) 
# Prepare data for box plot
box_data <- spotify_data %>% 
  select(explicit, danceability, energy, valence) %>% 
  pivot_longer(cols = c(danceability, energy, valence), names_to = "feature", values_to = "value")
# Step 6: Handle outliers
# Function to detect and cap outliers
cap_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column <- ifelse(column < lower_bound, lower_bound, column)
  column <- ifelse(column > upper_bound, upper_bound, column)
  return(column)
}
# Apply outlier treatment
spotify_data$loudness <- cap_outliers(spotify_data$loudness)
spotify_data$tempo <- cap_outliers(spotify_data$tempo)
spotify_data$duration_ms <- cap_outliers(spotify_data$duration_ms)

# Step 7: Normalize/Standardize numerical columns
spotify_data <- spotify_data %>%
  mutate(across(
    c(popularity, duration_ms, tempo, loudness),
    ~ scale(.)  # Standardize using z-score normalization
  ))

# Step 8: Convert categorical variables
spotify_data$explicit <- as.factor(spotify_data$explicit)
spotify_data$track_genre <- as.factor(spotify_data$track_genre)

# 1. How do audio features like danceability, energy, and valence differ between explicit and non-explicit tracks, and how might these differences influence popularity?
# Create box plot to compare explicit vs non-explicit tracks for danceability, energy, and valence
ggplot(box_data, aes(x = explicit, y = value, fill = explicit)) +
  geom_boxplot() +
  facet_wrap(~feature, scales = "free_y") +
  scale_fill_manual(values = c("skyblue", "salmon")) + # Distinct fill colors
  labs(title = "Comparison of Audio Features by Explicitness",
       subtitle = "Danceability, Energy, and Valence",
       x = "Explicitness (True/False)",
       y = "Feature Value",
       caption = "Data Source: Spotify Tracks") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Calculate the distribution of explicit vs non-explicit tracks
explicit_distribution <- spotify_data %>%
  group_by(explicit) %>%
  summarise(count = n())

# Plot the bar chart with counts displayed on top of the bars
ggplot(explicit_distribution, aes(x = explicit, y = count, fill = explicit)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.3, size = 5, color = "black") +  # Display count on top of bars
  labs(title = "Distribution of Explicit vs Non-Explicit Tracks",
       x = "Explicitness",
       y = "Count of Tracks") +
  scale_fill_manual(values = c("blue", "red")) +  # Custom colors
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.title = element_text(size = 12),     # Make axis titles bigger
        axis.text = element_text(size = 10))      # Make axis text bigger

# Create violin plot to compare danceability, energy, and valence between explicit and non-explicit tracks
ggplot(box_data, aes(x = explicit, y = value, fill = explicit)) +
  geom_violin() +
  facet_wrap(~ feature, scales = "free_y") +
  scale_fill_manual(values = c("skyblue", "salmon")) +  # Distinct fill colors
  labs(title = "Distribution of Audio Features by Explicitness",
       subtitle = "Danceability, Energy, and Valence",
       x = "Explicitness (True/False)",
       y = "Feature Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Select numerical columns relevant to the analysis
num_data <- spotify_data %>% 
  select(popularity, danceability, energy, valence, loudness, tempo, duration_ms)

# Calculate the correlation matrix
cor_matrix <- cor(num_data, use = "complete.obs")

# Create a heatmap using ggcorrplot
ggcorrplot(cor_matrix, 
           lab = TRUE,          # Display correlation coefficients
           lab_size = 3,        # Size of the labels
           title = "Correlation Heatmap of Audio Features and Popularity",
           colors = c("red", "white", "blue"), # Red to blue color scale
           ggtheme = theme_minimal())  # Use a minimal theme

# Create scatter plot for energy vs danceability, with popularity as color
ggplot(spotify_data, aes(x = energy, y = danceability, color = popularity)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Energy vs Danceability with Popularity as Color",
       x = "Energy",
       y = "Danceability",
       color = "Popularity") +
  theme_minimal()


