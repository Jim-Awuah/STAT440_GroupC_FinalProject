# Load libraries
library(readxl)
library(ggplot2)
library(tidyr)
library(flextable)

# Import data
International_Labour <- read_excel("/Users/mac/Desktop/STAT 440 PROJECT/International Labour.xlsx")

# Reshape data
labor_long <- pivot_longer(International_Labour, cols = -Year, 
                           names_to = "Country", values_to = "Value")

# Summary statistics
round(sapply(International_Labour[,-1], function(x) c(
  Min = min(x),
  Q1 = quantile(x, 0.25),
  Median = median(x),
  Mean = mean(x),
  Q3 = quantile(x, 0.75),
  Max = max(x)
)), 2)

# Individual line plots
ggplot(labor_long, aes(x = Year, y = Value)) +
  geom_line(color = "black") +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "International Labour Data by Country",
       x = "Time Period", y = "Value") +
  theme_minimal()

# Box and whisker plot
ggplot(labor_long, aes(x = Country, y = Value, fill = Country)) +
  geom_boxplot() +
  labs(title = "Distribution of International Labour Data by Country",
       x = "Country", y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")

# Correlation matrix
cor_matrix <- round(cor(International_Labour[,-1]), 2)
print(cor_matrix)

# Correlation heatmap
cor_long <- as.data.frame(as.table(cor_matrix))
names(cor_long) <- c("Country1", "Country2", "Correlation")
ggplot(cor_long, aes(x = Country1, y = Country2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Correlation), size = 3) +
  scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue", midpoint = 0.5) +
  labs(title = "Correlation Heatmap of International Labour Data",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

