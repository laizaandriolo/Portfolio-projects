library(tidyverse)
library(shiny)
library(ggplot2)

df <- read.csv("FAOSTAT_data_en_1-23-2024.csv")
unique(df$Item)
df$Year <- as.Date(paste0(df$Year, "-01-01"))
df$Year <- floor_date(df$Year, unit = "years")
class(df$Year)
head(df)


# EDA
filteredData <- df[df$Item == "2.1.1 Number of undernourished people" & df$Year == "2021-01-01", ] # filter indicator and year
filteredData$Value <- na_if(filteredData$Value, "") #replace empty values by NAs
sum(is.na(filteredData$Value)) # there are 56 NaS
area_with_highest_value <- filteredData$Area[which.max(filteredData$Value)] # country with highest value *India*
class(filteredData$Value) # character

# Replace "<1" with 0 and convert "Value" to integer
filteredData <- filteredData %>%
  mutate(Value = as.integer(gsub("<1", "0", Value)))

# Top 10 countries with highest Number of undernourished people
top_10_df <- filteredData %>%
  na.omit() %>%
  arrange(desc(Value)) %>%
  head(10)

# Extract only the countries
countries <- top_10_df$Area

# Lets look at the time series
df_2.1.1 <- df[df$Item == "2.1.1 Number of undernourished people",] # filter indicator
df_2.1.1$Value <- na_if(df_2.1.1$Value, "") #replace empty values by NAs
sum(is.na(df_2.1.1$Value)) # there are 1224 NaS
df_2.1.1 <- df_2.1.1 %>%
  mutate(Value = as.integer(gsub("<1", "0", Value)))
head(df_2.1.1)

# Top 10 countries with highest Number of undernourished people
top_10_df_2.1.1 <- df_2.1.1 %>%
  filter(Area %in% countries)

# Create line chart
ggplot(top_10_df_2.1.1, aes(x = Year, y = Value, color = Area)) +
  geom_line() +
  labs(title = "2.1.1 Number of undernourished people",
       x = "Year",
       y = "Value")

#######################

#1) filter by item - 14 items
#2) replace empty values by NAs
#3) replace "<1" with 0 and convert "Value" to integer
#3) filter by Year 2021 and select the 10 countries with the highest Value - top_10_countriees_2021
#4) Create a new df filtering only the countries present in the df top_10_countries_2021
#5) Create a line plot and save the graph - 14 graphs

# List of unique items
unique_items <- unique(df$Item)

# Create an empty list to store the top 10 data frames for each item
top_10_list <- list()

# Loop through each unique item
for (item in unique_items) {
  
  # Step 1: Filter by item
  df_filtered <- df[df$Item == item, ]
  
  # Step 2: Replace empty values by NAs
  df_filtered$Value <- na_if(df_filtered$Value, "")
  
  # Step 3: Replace "<1" with 0 and convert "Value" to integer
  df_filtered$Value <- as.integer(gsub("<1", "0", df_filtered$Value))
  
  # Step 4: Filter by Year 2021 and select the top 10 countries with the highest Value
  top_10_countries_2021 <- df_filtered %>%
    filter(Year == "2021-01-01") %>%
    arrange(desc(Value)) %>%
    head(10)
  
  # Step 5: Create a new df filtering only the countries present in the df top_10_countries_2021
  df_filtered_top_10 <- df_filtered %>%
    filter(Area %in% top_10_countries_2021$Area)
  
  # Append the result to the list
  top_10_list[[item]] <- df_filtered_top_10
  
  # Step 6: Create a line plot
  gg <- ggplot(df_filtered_top_10, aes(x = Year, y = Value, color = Area, label = Label)) +
    geom_line() +
    labs(title = paste("SDG Indicator Over Time -", item),
         x = "Year",
         y = "Value") +
    theme(plot.title = element_text(size = 8),  # Adjust the size as needed
          legend.text = element_text(size = 5),  # Adjust the size of the legend text
          axis.title.x = element_text(size = 7),  # Adjust the size of the x-axis label
          axis.title.y = element_text(size = 7), # Adjust the size of the y-axis label)
          legend.title = element_text(size = 7),  # Adjust the size of the legend title
          axis.text.x = element_text(size = 6),   # Adjust the size of the x-axis text
          axis.text.y = element_text(size = 6))   # Adjust the size of the y-axis text
  
  
  # Save the graph
  ggsave(paste("line_plot_", gsub(" ", "_", item), ".png", sep = ""), plot = gg, width = 12, height = 5)
}


############################
# Define the Shiny UI
ui <- fluidPage(
  titlePanel("SDG Indicator Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("area", "Choose Area:", choices = unique(df$Area)),
      selectInput("item", "Choose Item:", choices = unique(df$Item))
    ),
    
    mainPanel(
      plotOutput("line_plot")
    )
  )
)

# Define the Shiny server
server <- function(input, output) {
  
  # Filter the data based on user inputs
  filtered_data <- reactive({
    df %>%
      filter(Area == input$area, Item == input$item) %>%
      mutate(Value = as.integer(gsub("<1", "0", Value)))
  })
  
  # Create the line plot
  output$line_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = Value, color = Area, group = Area)) +
      geom_line() +
      labs(title = paste("SDG Indicator Over Time -", input$item),
           x = "Year",
           y = "Value",
           color = "Area") +
      theme_minimal()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


