library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# get next holidays worldwide to test API
next_holidays <- httr::GET("https://date.nager.at/api/v3/NextPublicHolidaysWorldwide")
str(next_holidays)  
next_holidayscontent <- content(next_holidays, as = "text")
str(next_holidayscontent)
next_holidayscontentjson <- jsonlite::fromJSON(next_holidayscontent)
View(next_holidayscontentjson)

# Test API for one single country
holidays <- httr::GET("https://date.nager.at/api/v3/publicholidays/2023/BR")
holidayscontent <- content(holidays, as = "text")
holidaysJson <- jsonlite::fromJSON(holidayscontent)


# I need all holidays worldwide of 2023
# First I will get the available countries and create a df with countries name and code
countries <- httr::GET("https://date.nager.at/api/v3/AvailableCountries")
countriescontent <- content(countries, as = "text")
countriescontentjson <- jsonlite::fromJSON(countriescontent)


# Iterate through each country and fetch public holidays

all_holidays_df <- data.frame() # create empty df

for (country_code in countriescontentjson$countryCode) {
  holidays_url <- paste0("https://date.nager.at/api/v3/publicholidays/2023/", country_code)
  holidays_request <- httr::GET(holidays_url)
  holidays_content <- content(holidays_request, as = "text")
  holidays_json <- jsonlite::fromJSON(holidays_content)
  # Example: Create a dataframe for the current country's holidays
  country_holidays_df <- as.data.frame(holidays_json)
  
  # Add a column for the country code
  country_holidays_df$country_code <- country_code
  
  # Combine the data for the current country with the overall dataframe
  all_holidays_df <- bind_rows(all_holidays_df, country_holidays_df)
}

# Count holidays that are present in most of the countries
holiday_counts <- all_holidays_df %>%
  group_by(name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Find the holidays with the highest count
top_holidays <- holiday_counts %>%
  top_n(10, wt = Count) %>%
  arrange(desc(Count))


# Plot the results using ggplot2 d
ggplot(top_holidays, aes(x = reorder(name, Count), y = Count)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.5), vjust = 0.2) +
  labs(title = "Holidays with the Highest Count",
       x = "Holiday",
       y = "Count worldwide") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Count holidays by each country
holiday_each_country <- all_holidays_df %>%
  group_by(country_code) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Merge the df "holiday_each_country" with "countriescontentjson" since the last one
# has the names.
colnames(countriescontentjson)[1] <- "country_code"
top_10_countries <- merge(holiday_each_country, countriescontentjson, by = "country_code", all.x = TRUE) %>%
  select(country_code, name, Count) %>%
  rename(country_code = country_code, country = name, number_of_holidays = Count) %>%
  arrange(desc(number_of_holidays)) %>%
  slice_head(n = 10)

# Create a bar plot
ggplot(top_10_countries, aes(x = reorder(country, number_of_holidays), y = number_of_holidays)) +
  geom_bar(stat = "identity", fill = "salmon") +
  geom_text(aes(label = number_of_holidays), position = position_dodge(width = 0.5), vjust = 0.2) +
  labs(title = "Top 10 Countries with the Highest Number of Holidays",
       x = "Country",
       y = "Number of Holidays") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
