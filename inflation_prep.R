## -----------------------------------------------------------------------------
#| label: library
#| include: false
#| authors: Loh Xian Ming Jeremiah, Wong Zhen Line, Veleon Lim Ming Zhe, Farah Binte Mohamed Tajudeen, Lim Xiang Wei Alex, Javier Ng Wei Cheng

library(tidyverse)
library(knitr)
library(readxl)
library(purrr)
library(ggplot2)
library(scales)
library(stringr)
library(grid)
library(dplyr)



## -----------------------------------------------------------------------------
#| label: fig-inflation
#| echo: false
#| fig.cap: "Visualising inflation in the USA from May 2023 to Apr 2024
#|  by @Joella_Carman and @Nigel_Chiwaya."

include_graphics("images/inflation-original-big.png")


## -----------------------------------------------------------------------------
#| label: merge-files
#| echo: false
#| include: false

# Get a list of all Consumer Price Index(CPI) XLSX file paths
xlsx_files <- list.files(path = "data/raw/cpi", pattern = "*.xlsx", full.names = TRUE)

# Filter out temporary files
xlsx_files <- xlsx_files[!grepl("~\\$", xlsx_files)]

# Read and combine the XLSX files, selecting the desired columns and skipping rows
# We also add columns for year and month of data
inflation_data <- map_dfr(xlsx_files,function(file) { 
  read_excel(file, skip = 6) |>
    # Select the relevant columns and clean up the data
    select(expenditure_category = 2,
           percent_change = 4)|>
      mutate(year = str_extract(basename(file), "\\d{4}"),  # Extract year from file name
      month = str_extract(basename(file), "\\d{2}(?!\\d)"),  # Extract month from file name
      expenditure_category = str_replace(expenditure_category, "\\(.*", ""))|>  # Remove parentheses and text inside to simplify category names
      filter(!is.na(expenditure_category))  # Remove rows with NA in expenditure_category
})

# Keep only the categories we are interested in
keep_categories <- c("All items", "Airline fares", "New and used motor vehicles", "Apparel", "Shelter", "Meats, poultry, fish, and eggs", "Gasoline ")
filtered_data <- inflation_data %>%
  filter(expenditure_category %in% keep_categories)


## -----------------------------------------------------------------------------
#| label: merge-electricity-data
#| include: false
# Get a list of all the energy data XLSX file paths
xlsx_files <- list.files(path = "data/raw/energy", pattern = "*.xlsx", full.names = TRUE)

# Read and combine the XLSX files, calculate percentage change of energy
energy_data <- map_dfr(xlsx_files, ~{
  read_excel(.x, skip = 64, n_max = 1) %>%
    select(current_price = 2, previous_price = 3) %>%
    transmute(
      expenditure_category = "Energy",
      percent_change = round((current_price - previous_price) / previous_price * 100, 1),
      # Add year and month of each data point
      year = str_extract(basename(.x), "\\d{4}"),
      month = str_extract(basename(.x), "\\d{2}(?!\\d)")
    )
})

# Merge filtered CPI data and energy data
merged_data <- bind_rows(filtered_data, energy_data)



## -----------------------------------------------------------------------------
#| label: finalise-long-data
#| echo: false

# 
long_data <- merged_data %>%
  unite(date, year, month, sep = "/") %>%
  rename(inflation_rate = percent_change)

# Wrap long expenditure category labels
long_data$expenditure_category <- str_wrap(long_data$expenditure_category, width = 20)
view(long_data)


## -----------------------------------------------------------------------------
#| label: merge-covid-data
#| include: false

covid_data <- read_csv("data/raw/covid/WHO-COVID-19-global-data.csv")

# Prepare the COVID-19 data
covid_data <- covid_data |>
  filter(Country == "United States of America") |> # Filter data for the United States of America
  select(Date_reported, New_cases) |>
  mutate(Date_reported = as.Date(Date_reported)) |> # Convert to Date format
  group_by(month = floor_date(Date_reported, "month")) |> # Group by month
  summarise(total_cases = sum(New_cases, na.rm = TRUE)) |> # Calculate the total cases for each month
  
  mutate(month = format(month, "%Y/%m")) |> # Format the month to match the inflation data
  mutate(month = as.factor(month)) |> # Convert to a factor for proper alignment with non-date format
  # Normalize the total cases
  mutate(total_cases = (total_cases - min(total_cases, na.rm = TRUE)) / (max(total_cases, na.rm = TRUE) - min(total_cases, na.rm =TRUE))) |>
  slice(1:(n() - 5)) # Remove the 5 months data to only include up to Jan 2024

View(covid_data)


## -----------------------------------------------------------------------------
#| label: improved-visualisation
#| fig.width: 11
#| fig.height: 5

# Create a mapping for expenditure categories to continuous y-values
unique_categories <- unique(long_data$expenditure_category)
num_categories <- length(unique_categories)
category_mapping <- data.frame(
  expenditure_category = unique_categories,
  y_value = seq(1, 8, length.out = num_categories)
)

# Merge the mapping with the long_data
long_data <- merge(long_data, category_mapping, by = "expenditure_category")

# Calculate the scale factor for the COVID-19 cases to fit the 0.00 to 1.00 range
scale_factor <- max(covid_data$total_cases) / num_categories

# Plot the inflation rates over time with heatmap
ggplot() +
  # Inflation change heatmap
  geom_tile(data = long_data, aes(x = date, y = y_value, fill = inflation_rate), color = "grey85") +
  coord_fixed(ratio = 2.5) +
  theme_minimal() +
  scale_fill_distiller(palette = "RdYlBu") +
  scale_y_continuous(breaks = category_mapping$y_value, labels = category_mapping$expenditure_category, 
                     sec.axis = sec_axis(~.*max(covid_data$total_cases), name = "Total Covid Cases (10M)")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Covid cases line graph
  geom_line(data = covid_data, aes(x = month, y = total_cases * 8 + 0.5, group = 1, color = "Number of COVID-19 cases")) +
  # X-axis labels and scaling representing Time
  scale_x_discrete(breaks = levels(covid_data$month)[seq(1, length(levels(covid_data$month)), by = 3)]) +
  scale_color_manual(values = c("Number of COVID-19 cases" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        legend.position = "right",
        axis.title.x = element_text(vjust = -1, margin = margin(t = 10)),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 2)) +
  labs(title = "Impact of COVID-19 on Inflation Rates for Selected Categories Over Time",
       x = "Time",
       y = "Expenditure Category",
       fill = "Year-over-Year Price Change (%)",
       color = "Data Type") +
  guides(color = guide_legend(title = "COVID-19 Label", override.aes = list(linetype = c("solid"))))

