df_user <- readRDS("anon_subject_user.rdata")
df_consumption <- readRDS("anon_consumption_user.rdata")

library(tidyr)
library(dplyr)
library(magrittr)
library(sf)
library(ggplot2)
library(stringr)

# A 24-hour dietary recall interview and a seven-day food frequency questionnaire 
# interviews were conducted for each participant

############### DATA CLEANING #####################
colnames(df_user)
colnames(df_consumption)

# Check ID duplicates
n_distinct(df_consumption$SUBJECT) #543 subjects - NO duplicates
n_distinct(df_user) #543 subjects - NO duplicates

# Add NAs for obs with null valyes 
df_consumption <- df_consumption %>%
  mutate_all(~ifelse(. == " ", NA, .))

# Check columns with missing values
columns_with_missing_values <- colSums(is.na(df_consumption)) > 0
cols_missing <- names(columns_with_missing_values[columns_with_missing_values])
cols_missing # 33 columns with all missing values

# Remove the columns with all Nas
df_consumption <- df_consumption %>%
  select(-where(~all(is.na(.)))) # now df has 45 columns

# Extract rows with null values 
rows_with_missing_values <- df_consumption %>%
  filter(rowSums(is.na(.)) > 0)
rows_with_missing_values 
# there are 2811 rows with missing values
# recipe_code and amount_recipe are these columns responsible for the rows with missing values
# since it is not the purpose of our analysis to identify recipes or codes, we will keep them. 

################ DESCRIPTIVE STATISTICS ############
# Check total food amount reported by each user
tot_food_amount_per_subject <-
df_consumption %>%
  group_by(SUBJECT) %>%
  summarise(total_food_amount = sum(FOOD_AMOUNT_REPORTED))

#####  Common measures of central tendency ##### 
summary(tot_food_amount_per_subject$total_food_amount)

# Min = 168.6
# 1st Qu. = 949.1
# Median = 1451.9
# Mean = 2055.7
# 3rd Qu = 2421.1
# Max = 10743.2 

# A quantile-quantile plot compares the quantiles of your data to the quantiles 
# of a theoretical distribution (e.g., normal distribution). It helps assess the normality of your data.
qqnorm(tot_food_amount_per_subject$total_food_amount)
qqline(tot_food_amount_per_subject$total_food_amount)

##### Dispersion Statistics - Variability ##### 
range_value <- range(tot_food_amount_per_subject$total_food_amount)
range_value #  168.60 - 10743.21

# Interquartile Range (IQR):
# The IQR is the range covered by the middle 50% of the data, i.e., the 
# difference between the third quartile (Q3) and the first quartile (Q1). 
IQR_value <- IQR(tot_food_amount_per_subject$total_food_amount)
IQR_value # 1471.95

# Variance: average squared deviation of each data point from the mean
variance_value <- var(tot_food_amount_per_subject$total_food_amount)
variance_value # 2951962

# Standard Deviation: square root of the variance and provides a measure of the 
# average distance between each data point and the mean.
sd_value <- sd(tot_food_amount_per_subject$total_food_amount)
sd_value # 1718.127

# Coefficient of Variation (CV): ratio of the standard deviation to the mean
mean_value <- mean(tot_food_amount_per_subject$total_food_amount)
cv_value <- sd_value / mean_value * 100
cv_value # 83.57851

# Mean Absolute Deviation (MAD):  average absolute difference between each data point and the mean.
mad_value <- mean(abs(tot_food_amount_per_subject$total_food_amount - mean_value))
mad_value #  1247.22

# Detecting outliers according to the total food amount by each subject

# Z-Score Method: Z-scores measure how many standard deviations a data point is from the mean. 
# Commonly, values with a z-score greater than 3 or less than -3 are considered potential outliers.
z_score_threshold <- 3
upper_threshold <- mean_value + z_score_threshold * sd_value # 7210.087
lower_threshold <- mean_value - z_score_threshold * sd_value # -3098.678

Q1 <- 949.1
Q3 <- 2421.1
IQR <- Q3 - Q1

# Modified Z-Score:
median_value <- 1451.9
median_absolute_deviation <- median(abs(tot_food_amount_per_subject$total_food_amount - median_value))
modified_z_scores <- 0.6745 * (tot_food_amount_per_subject$total_food_amount - median_value) / median_absolute_deviation
outliers <- abs(modified_z_scores) > 3.5
outlier_values <- tot_food_amount_per_subject$total_food_amount[outliers]

# Interquartile Range (IQR) Method:
lower_threshold_2 <- Q1 - 1.5 * IQR # -1258.9
upper_threshold_2 <- Q3 + 1.5 * IQR # 4629.1

# A boxplot (or box-and-whisker plot) - five-number summary and helps identify outliers.
boxplot(tot_food_amount_per_subject$total_food_amount, main="Boxplot")

# Histogram - distribution of your data, showing the frequency of values within different bins
hist(tot_food_amount_per_subject$total_food_amount, main="Histogram")

# Similar to a histogram, a density plot shows the distribution of data but in a smoothed manner.
ggplot(tot_food_amount_per_subject, aes(x="", y=total_food_amount)) +
  geom_violin() +
  ggtitle("Violin Plot")

# I decided to use the upper threshold of z_score method
df_consumption_clean <- df_consumption %>%
  group_by(SUBJECT) %>%
  filter(sum(FOOD_AMOUNT_REPORTED) <= 7210.087)

# Now there are 18945 obs
n_distinct(df_consumption_clean$SUBJECT) # 533 subjectes, 10 were removed.
colnames(df_consumption_clean)

# Check the food amount reported by subject in the different days
food_subject_period_clean <-
  df_consumption_clean %>%
  group_by(SUBJECT, CONSUMPTION_YEAR, WEEK_DAY) %>%
  summarise(total_food_amount = sum(FOOD_AMOUNT_REPORTED)) %>%
  group_by(SUBJECT)

# Some subjects reported food consumption in one single day while others reported 
# in more days. We will need to go back to the previous dataset 

unique_days_filter <- c(1, 2, 3, 4)  

result_list <- list()

for (days in unique_days_filter) {
  result <- df_consumption %>%
    group_by(SUBJECT) %>%
    summarise(unique_days = n_distinct(CONSUMPTION_DAY)) %>%
    filter(unique_days == days)
  
  result_data <- semi_join(df_consumption, result, by = "SUBJECT")
  result_list[[as.character(days)]] <- result_data
}

# Access the data frames for each unique days group
df_1 <- result_list[["1"]]
df_2 <- result_list[["2"]]
df_3 <- result_list[["3"]]
df_4 <- result_list[["4"]]

# Now we have four different datasets with different subjects that were interviewed
# in one or more days
# Lets check distinct subjects for each df
result_list_2 <- list(df_1, df_2, df_3, df_4)
distinct_subjects <- lapply(result_list_2, function(df) n_distinct(df$SUBJECT))
distinct_subjects
# df_1 = 434; df_2=13; df_3=12, df_4=84
total_distinct_subjects <- sum(sapply(result_list_2, function(df) n_distinct(df$SUBJECT)))
# 543

# 80% of the sample is in the df_1 (which means, people who reported the amount
# of food in one single day) so let's start by this df
df_1_distinct <-
df_1 %>%
  group_by(SUBJECT) %>%
  summarise(tot_food_amount = sum(FOOD_AMOUNT_REPORTED), tot_kcal = sum(ENERGY_kcal), tot_proteins = sum(PROTEIN_g))

# Detect outliers
boxplot(df_1_distinct$tot_kcal)

df_1_distinct %<>%
  filter(!(tot_kcal > 3000)) # there are 16 subjects with kcal intake > 3000
# we removed them

summary(df_1_distinct$tot_kcal) # again

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 229.0   931.8  1311.0  1396.7  1781.2  2989.0 

# Lets match the new dataset with df_user so we will have the cities
df_1_distinct_regions <- 
  left_join(df_1_distinct, df_user %>% select(SUBJECT, ADM2_NAME, AGE_YEAR, WEIGHT, HEIGHT, PREG_LACT), by = "SUBJECT")

summary_stats <- sapply(df_1_distinct_regions, summary)

# AGE
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.00   15.00   16.00   16.11   17.00   19.00 

# WEIGHT
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  29.60   43.30   48.00   48.09   52.10   75.40       1 

# HEIGHT
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 136.7   150.3   154.1   153.7   157.3   171.0       1 

df_1_distinct_regions %>% count(PREG_LACT)
# 48 girls pregnant, 39 girls lactating and 4 pregnant and lactating (7=NA)

summary_by_region <- df_1_distinct_regions %>%
  group_by(ADM2_NAME) %>%
  summarise(max_kcal = max(tot_kcal),
            min_kcal = min(tot_kcal))

# The maximum kcal intake is the same in the 3 cities, while the min differs.
# Cidade de Quelimane min = 408 kcal
# Maganja da Costa min = 289 kcal
# Morrumbala = 229 kcal

# Lets have a look in the other variables as well
summary_all <- 
  df_1_distinct_regions %>%
  select(-SUBJECT, -PREG_LACT)%>%
  group_by(ADM2_NAME) %>%
  summarise_all(list(max = ~max(., na.rm = TRUE), 
                     min = ~min(., na.rm = TRUE),
                     mean = ~mean(., na.rm = TRUE),
                     median = ~median(., na.rm = TRUE),
                     sd = ~sd(., na.rm = TRUE)))
 
summary_all

# In terms of dispersion, the city "Maganja da Costa" has the lowest sd for 
# food variables. It has the lowest max food amount reported and the lowest minimum
# food amount reported. Also lowest min. weight. Lowest mean and median for food amount.

summary_food <- 
  df_1_distinct_regions %>%
  select(ADM2_NAME, tot_food_amount, tot_kcal) %>%
  group_by(ADM2_NAME) %>%
  summarise(across(c(tot_food_amount, tot_kcal), 
                   list(median = median, max = max, min = min), 
                   na.rm = TRUE))

# Reshape the data for plotting
summary_food_long <- tidyr::gather(summary_food, key = "Statistic", value = "Value", -ADM2_NAME)

# Plotting
ggplot(summary_food_long, aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3, color = "white") +
  geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.5), vjust = -0.5, size =2) +  # Add text labels
  facet_grid(ADM2_NAME ~ ., scales = "free_y", space = "free_y") +  # Arrange facets vertically and set larger size
  labs(title = "Summary Statistics for Food Variables by City",
       x = "Statistic",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 8),
        legend.position = "none",  # Remove legend
        panel.grid = element_blank(),  # Remove grid lines
        panel.background = element_rect(fill = "lightgray", color = "lightgray"),  # Use empty background
        plot.title = element_text(size = 10), # Adjust size title
        axis.title.x = element_text(size = 8),  # Adjust size parameter for x-axis title
        axis.title.y = element_text(size = 8))  # Adjust size parameter for y-axis title)  # Adjust size parameter for the title

# It looks like Morrumbala has lower kcal while the food amount is higher
# While cidade de Quelimane has lower food amount but higher kcal

######## INFERENTIAL STATISTICS #############

# HYPOTHESIS TEST #

# Null hypothesis: The means of "tot_food_amount" are equal across different cities.
# Alternative hypothesis: The means of "tot_food_amount" are not equal across different cities.

# compare means between cities, you could use ANOVA or t-tests.
anova_result <- aov(tot_food_amount ~ ADM2_NAME, data = df_1_distinct_regions)
summary(anova_result)

# there is a significant difference in the means of "tot_food_amount" across different cities 
# (ADM2_NAME).  "Pr(>F)" represents the p-value for the overall F-statistic. 
# In your output, the p-value is very small (3.16e-05), which is less than the typical significance 
# level of 0.05.

# Small p-value (e.g., less than 0.05): You reject the null hypothesis.

# CORRELACTION ANALYSIS #

# Check correlation between food amount and kcal intake
cor_result <- cor.test(df_1_distinct_regions$tot_food_amount, df_1_distinct_regions$tot_kcal)

# The positive correlation coefficient (0.7977972) indicates a strong positive linear
# relationship between the two variables. As one variable increases, the other tends 
# to increase as well.
# The confidence interval suggests that you can be 95% confident that the true correlation 
# lies between 0.7600411 and 0.8301858.

# Check correlation between food amount and proteins intake
cor_result_2 <- cor.test(df_1_distinct_regions$tot_food_amount, df_1_distinct_regions$tot_proteins)

# The positive correlation coefficient (0.6055049) indicates a moderate positive linear relationship 
# between the two variables. As one variable increases, the other tends to increase as well.
# The confidence interval suggests that you can be 95% confident that the true correlation lies 
# between 0.5410101 and 0.6629195.

# Check correlation between kcal intake and proteins intake
cor_result_3 <- cor.test(df_1_distinct_regions$tot_kcal, df_1_distinct_regions$tot_proteins)

# The positive correlation coefficient (0.7963956) indicates a strong positive linear relationship between the two variables. 
# As one variable increases, the other tends to increase as well.
# The confidence interval suggests that you can be 95% confident that the true correlation 
# lies between 0.7584129 and 0.8289873.

# REGRESSION ANALYSIS #
lm_result <- lm(tot_food_amount ~ ADM2_NAME, data = df_1_distinct_regions)
summary(lm_result)

# Maganja da Costa: The coefficient for this region is -338.90. 
# It represents the estimated difference in "tot_food_amount" between the baseline category 
# and Maganja da Costa. The negative sign indicates that, on average, "tot_food_amount" is lower
# in Maganja da Costa compared to the baseline.
# Morrumbala: The coefficient for this region is -99.52. 
# The negative sign indicates that, on average, "tot_food_amount" is lower in Morrumbala 
# compared to the baseline.

# For Maganja da Costa: p-value < 0.001 (highly significant).
# For Morrumbala: p-value = 0.185 (not statistically significant at the conventional 0.05 significance level).

# In this model, the Multiple R-squared is 0.04871, indicating that the model explains a small 
# proportion of the variability in "tot_food_amount."

# The overall model is statistically significant (p-value: 3.159e-05).
# The specific regions, Maganja da Costa and Morrumbala, have different effects on "tot_food_amount," 
# but only the effect of Maganja da Costa is statistically significant.

