## -----------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)
library(stringr)


## -----------------------------------------------------------------------------

# Read in the wide data
wide_data <- read_csv("data/merged/filtered_wide_inflation_data.csv")
wide_data


## -----------------------------------------------------------------------------
# GGPlot implementatioin

# Making long
long_data <- wide_data %>%
  pivot_longer(cols = -expenditure_category, names_to = "date", values_to = "inflation_rate")

print(long_data)

covid_data <- read_csv("data/raw/covid/WHO-COVID-19-global-data.csv")
# Summarize to a monthly level


covid_data <- covid_data |>
  filter(Country == "United States of America") |>
  select(Date_reported, New_cases) |>
  mutate(Date_reported = as.Date(Date_reported)) |>
  group_by(month = floor_date(Date_reported, "month")) |>
  summarise(total_cases = sum(New_cases, na.rm = TRUE)) |>
  mutate(prev_year_cases = lag(total_cases, n = 12),   # Get the total cases from the previous year  
         pct_change = (total_cases - prev_year_cases) / prev_year_cases * 100) |> # Calculate the percent change
  mutate(month = format(month, "%Y/%m")) |> 
  mutate(month = as.factor(month)) |> # Convert to a factor for proper alignment with non-date format
  # Normalize the total cases
  mutate(total_cases = (total_cases - min(total_cases, na.rm = TRUE)) / (max(total_cases, na.rm = TRUE) - min(total_cases, na.rm =TRUE))) 

print(long_data)
print(covid_data)


## -----------------------------------------------------------------------------
#| label: plot-with-white-lines
#| fig.width: 8
#| fig.height: 4

# Wrap long expenditure category labels
long_data$expenditure_category <- str_wrap(long_data$expenditure_category, width = 20)

# Plot the inflation rates over time with heatmap
ggplot() +
  geom_tile(data=long_data, aes(x = date, y = expenditure_category, fill = inflation_rate), color = "white") +
  theme_minimal() +
  scale_fill_distiller(palette = "RdYlBu") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.x = element_blank())+
  geom_line(data = covid_data, aes(x = month, y = total_cases * 8 + 0.5, group = 1, color = "Number of COVID-19 cases")) +
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





## -----------------------------------------------------------------------------
#| label: plot-with-grey-lines
#| fig.width: 8
#| fig.height: 4

# Wrap long expenditure category labels
long_data$expenditure_category <- str_wrap(long_data$expenditure_category, width = 20)

# Plot the inflation rates over time with heatmap
ggplot() +
  geom_tile(data=long_data, aes(x = date, y = expenditure_category, fill = inflation_rate), color = "grey90", size = 0.1) +
  theme_minimal() +
  scale_fill_distiller(palette = "RdYlBu") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.x = element_blank())+
  geom_line(data = covid_data, aes(x = month, y = total_cases * 8 + 0.5, group = 1, color = "Number of COVID-19 cases")) +
  geom_rug(data = long_data, aes(x = date), sides = "b", color = "black", size = 0.5) +
  scale_x_discrete(breaks = levels(covid_data$month)[seq(1, length(levels(covid_data$month)), by = 3)]) +
  scale_color_manual(values = c("Number of COVID-19 cases" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5),
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
  guides(color = guide_legend(title = "COVID-19 Label", override.aes = list(linetype = c("solid")))) +
  coord_cartesian(xlim = c(0, 51))




