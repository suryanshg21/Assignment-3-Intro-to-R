#Question 1
# Load the required packages
library(ggplot2)

# Load the iris dataset
data(iris)

# Create the scatterplot
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  labs(x = "Sepal Length", y = "Petal Length", color = "Species") +
  ggtitle("Scatterplot of Sepal Length and Petal Length") +
  theme_minimal()
#By observing the scatterplot, we can draw conclusions about the relationship between Sepal.Length and Petal.Length for different species of iris flowers. We can analyze the clusters or patterns formed by the points to identify any correlations or trends between these variables.

#Question 2
library(ggplot2)

# Load the txhousing dataset
data(txhousing)

# View the structure and summary of the dataset
str(txhousing)
summary(txhousing)

# Check for missing values
missing_rows <- sum(!complete.cases(txhousing))
cat("Number of missing rows:", missing_rows, "\n")

# Scatterplot of sales price vs. year
ggplot(txhousing, aes(x = year, y = sales)) +
  geom_point() +
  labs(x = "Year", y = "Sales Price") +
  ggtitle("Scatterplot of Sales Price vs. Year") +
  theme_minimal()

# Boxplot of sales price by city
ggplot(txhousing, aes(x = city, y = sales)) +
  geom_boxplot() +
  labs(x = "City", y = "Sales Price") +
  ggtitle("Boxplot of Sales Price by City") +
  theme_minimal()

# Histogram of sales price
ggplot(txhousing, aes(x = sales)) +
  geom_histogram(binwidth = 100000) +
  labs(x = "Sales Price", y = "Count") +
  ggtitle("Histogram of Sales Price") +
  theme_minimal()

# Bar plot of the number of houses by year
ggplot(txhousing, aes(x = factor(year))) +
  geom_bar() +
  labs(x = "Year", y = "Number of Houses") +
  ggtitle("Bar Plot of Number of Houses by Year") +
  theme_minimal()

# Correlation matrix
cor_matrix <- cor(txhousing[, c("sales", "volume", "listings")], use = "complete.obs")

# Convert the correlation matrix to data frame
cor_df <- as.data.frame(cor_matrix)
cor_df$Var1 <- rownames(cor_matrix)
cor_df <- tidyr::pivot_longer(cor_df, cols = -Var1, names_to = "Var2", values_to = "value")

# Heatmap of the correlation matrix
ggplot(data = cor_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Correlation Matrix",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))




#Question 3
# Load the required packages
library(ggplot2)
library(readr)
library(dplyr)

# Read the Titanic dataset
titanic <- read.csv("C:/Users/surya/Downloads/titanic.csv")

# Filter the required columns
filtered_data <- titanic %>%
  select(PassengerId, Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked)

# Create a new factor variable combining "Survived" and "Died" with modified order of levels
filtered_data$Survival <- factor(ifelse(filtered_data$Survived == 1, "Survived", "Died"), levels = c("Died", "Survived"))

# Create the plot with color aesthetics and separate male and female within categories
finalP <- ggplot(data = filtered_data, aes(x = Fare, y = factor(Survival, levels = c("Died", "Survived")), fill = Sex)) +
  geom_boxplot(position = "dodge")+
  labs(x = "Fare", y = "Survival") +
  scale_fill_manual(values = c("cyan", "pink")) +
  theme_minimal()

# Save the plot in the object finalP
finalP
