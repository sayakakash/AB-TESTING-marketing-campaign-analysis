library(readr)

A_B_testing_control <- read_csv("D:\\data ANALYTICS AND SCIENCE\\COACH X ASSIGN\\DATA\\A and B_Testing\\control_group.csv")
A_B_testing_test <- read_csv("D:\\data ANALYTICS AND SCIENCE\\COACH X ASSIGN\\DATA\\A and B_Testing\\test_group.csv")

# Show FIRST 5 rows of control campaign dataset
head(A_B_testing_control, 5)

# Show LAST 5 rows of control campaign dataset
tail(A_B_testing_control, 5)

# Check for null values in control campaign dataset
sum(is.na(A_B_testing_control))
colSums(is.na(A_B_testing_control))
#COLUMN NAMES 
colnames(A_B_testing_control)
colnames(A_B_testing_test)



# Get information about A_B_testing_control dataframe
print(dim(A_B_testing_control))   # Print number of rows and columns
print(str(A_B_testing_control))   # Print structure of the dataframe
print(str(A_B_testing_test))



# filling the null values with mean 
A_B_testing_control$`of Impressions`[is.na(A_B_testing_control$`of Impressions`)] <- mean(A_B_testing_control$`of Impressions`, na.rm = TRUE)
A_B_testing_control$`Reach`[is.na(A_B_testing_control$`Reach`)] <- mean(A_B_testing_control$`Reach`, na.rm = TRUE)
A_B_testing_control$`of Website Clicks`[is.na(A_B_testing_control$`of Website Clicks`)] <- mean(A_B_testing_control$`of Website Clicks`, na.rm = TRUE)
A_B_testing_control$`of View Content`[is.na(A_B_testing_control$`of View Content`)] <- mean(A_B_testing_control$`of View Content`, na.rm = TRUE)
A_B_testing_control$`of Searches`[is.na(A_B_testing_control$`of Searches`)] <- mean(A_B_testing_control$`of Searches`, na.rm = TRUE)
A_B_testing_control$`of Add to Cart`[is.na(A_B_testing_control$`of Add to Cart`)] <- mean(A_B_testing_control$`of Add to Cart`, na.rm = TRUE)
A_B_testing_control$`of Purchase`[is.na(A_B_testing_control$`of Purchase`)] <- mean(A_B_testing_control$`of Purchase`, na.rm = TRUE)





# Check for null values in control campaign dataset
colSums(is.na(A_B_testing_control))

# Check for null values in test campaign dataset
colSums(is.na(A_B_testing_test))


#chceking statistics 
print(summary(A_B_testing_control))   # Print summary statistics of the dataframe


#merging pdf
merged_df_1 <- merge(A_B_testing_control, A_B_testing_test, by = c('Campaign Name', 'Date', 'Spend [USD]', 
                                                                   'of Impressions', 'Reach', 'of Website Clicks', 
                                                                   'of Searches', 'of View Content', 'of Add to Cart',
                                                                   'of Purchase'), all = TRUE)
View(merged_df_1)






#  Z TEST
# Z-test to determine if the sales (specified as 2500) in the merged_df_1 dataset is significantly different from the mean of the combined data.
# Load required libraries
library(tidyverse)
library(stats)
# Combine the data into a vector
combined_data <- merged_df_1$`Spend [USD]`
# Specify the value for the average sales
specified_value <- 2500
# Calculate the mean and standard deviation of the combined dataset
mean <- mean(combined_data)
std_dev <- sd(combined_data)
# Calculate the Z-score
n <- length(combined_data)
z_score <- (mean - specified_value) / (std_dev / sqrt(n))
z_score
# Define the significance level
significance_level <- 0.05
# Calculate the critical Z-value
critical_value <- qnorm(1 - significance_level / 2)
# Compare the Z-score with the critical value
if (abs(z_score) > critical_value) {
  print("Reject the null hypothesis")
}else {
  print("Fail to reject the null hypothesis")
}





# ANOVA ANALYSIS
# Load required libraries
library(car)
# Create a sample data frame
data <- merged_df_1[, c('Campaign Name', 'Spend [USD]')]
# Rename the columns to remove spaces
colnames(data) <- c('Campaign_Name', 'Spend_USD')
# Fit the ANOVA model
model <- aov(Spend_USD ~ Campaign_Name, data = data)
# Perform ANOVA analysis
anova_table <- Anova(model)
# Print the ANOVA table
print(anova_table)

"The F value is 8.82085. A larger F value suggests a larger difference between the means
of the groups being compared. p-value is 0.004326, which is less than 0.05(ignificance level). 
The null hypothesis in this case state that there are no significant differences between the means of the two groups Control Campaign and Test Campaign But we see p-value is 0.004326, which is less than 0.05.Therefore null hypothesis gets rejected and alternate hypothesis gets accepted.This means that there are significant differences between the means of the groups being compared. If the F-value is large and the p-value is small (typically less than a predetermined significance level, such as 0.05), it indicates that there is a significant difference between the means of the two groups.
The ANOVA results suggest that there are significant differences between the Control Campaign
and Test Campaign groups. This implies that the campaign type has an impact on sales performance"





# Gaussian distribution to check outliers
library(MASS)
library(ggplot2)
# Fit a Gaussian distribution to the "Sales USD" data
fit <- fitdistr(merged_df_1$`Spend [USD]`, "normal")
fit
mu <- fit$estimate[1]
sigma <- fit$estimate[2]
mu
sigma

# Define a threshold for anomaly detection (e.g., 3 standard deviations from the mean)
threshold <- mu - 1.5 * sigma
threshold

# Identify outliers as data points with PDF values below the threshold
outliers <- merged_df_1[merged_df_1$`Spend [USD]` < threshold, ]
outliers




# scatter plot for Spend [USD] vs of Impressions

library(ggplot2)

# Extract the relevant columns
impressions <- merged_df_1$`of Impressions`
amount_spent <- merged_df_1$`Spend [USD]`
campaign_name <- merged_df_1$`Campaign Name`

# Filter data for Test Campaign
test_campaign_data <- merged_df_1[merged_df_1$`Campaign Name` == "Test Campaign", ]
test_impressions <- test_campaign_data$`of Impressions`
test_amount_spent <- test_campaign_data$`Spend [USD]`

# Filter data for Control Campaign
control_campaign_data <- merged_df_1[merged_df_1$`Campaign Name` == "Control Campaign", ]
control_impressions <- control_campaign_data$`of Impressions`
control_amount_spent <- control_campaign_data$`Spend [USD]`

# Create a scatter plot for Test Campaign and Control Campaign
library(ggplot2)

# Create a scatter plot for Test Campaign and Control Campaign
ggplot() +
  geom_point(data = test_campaign_data, aes(x = test_impressions, y = test_amount_spent, color = "Test Campaign"), shape = 16) +
  geom_point(data = control_campaign_data, aes(x = control_impressions, y = control_amount_spent, color = "Control Campaign"), shape = 16) +
  labs(x = "Number of Impressions", y = "Amount Spent", title = "Relationship between Impressions and Amount Spent") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("Test Campaign", "Control Campaign")) +
  # Add legend
  guides(color = guide_legend(title = "Campaign")) 




# Pie chart to show number of searches performed on the website from both campaigns

library(ggplot2)

# Filter data for Test Campaign
test_campaign_data <- merged_df_1[merged_df_1$`Campaign Name` == "Test Campaign", ]
test_searches <- sum(test_campaign_data$`of Searches`)

# Filter data for Control Campaign
control_campaign_data <- merged_df_1[merged_df_1$`Campaign Name` == "Control Campaign", ]
control_searches <- sum(control_campaign_data$`of Searches`)

# Create a data frame for the pie chart
pie_data <- data.frame(
  Campaign = c("Test Campaign", "Control Campaign"),
  Searches = c(test_searches, control_searches)
)

# Create a pie chart
ggplot(pie_data, aes(x = "", y = Searches, fill = Campaign)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Number of Searches by Campaign") +
  scale_fill_manual(values = c("red", "orange")) +
  theme_void() +
  geom_text(aes(label = paste0(Searches, " (", round(Searches/sum(Searches)*100, 1), "%)")), 
            position = position_stack(vjust = 0.5))

























































