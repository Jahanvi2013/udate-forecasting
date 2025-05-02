getwd()
setwd("/Users/jahanvi/Desktop/R")
store_df<- read.table(file = 'store.csv', header = TRUE, sep= ",")
test_df<- read.table(file = 'test(1).csv', header = TRUE, sep= ",")
train_df<- read.table(file = 'train(2).csv', header = TRUE, sep= ",")
df<- read.table(file = 'df.csv', header = TRUE, sep= ",")

str(store_df)
unique(store_df)
summary(store_df)
table(train_df$DayOfWeek)
str(train_df)

#bar chart for number of stores by store type (a,b,c,d)
library(ggplot2)
ggplot(store_df, aes(x = StoreType)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of Stores by Type", x = "Store Type", y = "Count") +
  theme_minimal()

#bar chart for average sales by day of week
ggplot(train_df, aes(x = DayOfWeek, y = Sales)) +
  stat_summary(fun = mean, geom = "bar", fill = "steelblue") +
  labs(title = "Average Sales by Day of the Week", x = "Day of Week", y = "Average Sales") +
  theme_minimal()

#VISUALISATION 1
#scatterplot with no. of customers and sales
ggplot(train_df, aes(x = Customers, y = Sales)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Sales vs. Customers", x = "Customers", y = "Sales") +
  theme_minimal()

#boxplot of sales with and wihtout promo
ggplot(train_df, aes(x = factor(Promo), y = Sales)) +
  geom_boxplot(fill = c("lightblue", "lightcoral")) +
  labs(title = "Sales with and without Promotion", x = "Promo (0 = No, 1 = Yes)", y = "Sales") +
  theme_minimal()

#histogram of competition open sice year 
ggplot(store_df, aes(x = CompetitionOpenSinceYear)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Competition Opening Timeline", x = "Year", y = "Number of Competitors") +
  theme_minimal()

library(dplyr)

# Merge datasets on Store column
merged_data <- train_df %>%
  inner_join(store_df, by = "Store")

#VISAUL 2
# Scatter plot of Sales vs. CompetitionDistance
ggplot(merged_data, aes(x = CompetitionDistance, y = Sales)) +
  geom_point(alpha = 0.5, color = "goldenrod") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Sales vs. Competition Distance",
       x = "Competition Distance (m)",
       y = "Sales") +
  theme_minimal()

train_df$Promo <- as.factor(train_df$Promo)

#VISUAL 3
# violin plot for sales and promo days 
ggplot(train_df, aes(x = Promo, y = Sales, fill = Promo)) +
  geom_violin(trim = TRUE, scale = "width", alpha = 0.7) +
  stat_summary(fun = median, geom = "point", color = "black", size = 2, shape = 16) +
  labs(title = "Sales Distribution: Promo vs. Non-Promo Days",
       x = "Promo (0 = No, 1 = Yes)",
       y = "Sales",
       fill = "Promo Status") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "salmon"))

#VISUAL 4
#heatmap for sales, competition open year, and store type
heatmap_data <- merged_data %>%
  group_by(CompetitionOpenSinceYear, StoreType) %>%
  summarise(AverageSales = mean(Sales, na.rm = TRUE))

ggplot(heatmap_data, aes(x = as.factor(CompetitionOpenSinceYear), y = StoreType, fill = AverageSales)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Average Sales by Competition Open Year and Store Type",
       x = "Competition Open Year",
       y = "Store Type",
       fill = "Avg Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

str(merged_data)

install.packages('randomforest')
library(randomForest)
set.seed(4543)
data(df)
rf.fit <- randomForest(Sales ~ ., data=df, ntree=1000,
                       keep.forest=FALSE, importance=TRUE, na.action = na.omit)
rf.fit

str(df$Sales)
summary(df$Sales)

mod_rf <-
  randomForest(Sales ~ .,
        data=df, # Training data 
        method = "ranger", # random forest (ranger is much faster than rf)
        metric = "ROC", # area under the curve
        trControl = control_conditions,
        tuneGrid = tune_mtry,
        na.action = na.omit
  )
mod_rf














