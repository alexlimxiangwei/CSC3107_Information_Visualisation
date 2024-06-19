## -----------------------------------------------------------------------------
#| label: library
#| message: false
#| authors: Loh Xian Ming Jeremiah, Wong Zhen Line, Veleon Lim Ming Zhe, Farah Binte Mohamed Tajudeen, Lim Xiang Wei Alex, Javier Ng Wei Cheng

library(tidyverse)
library(knitr)
library(readxl)
library(purrr)
library(pheatmap)



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

# Get a list of all the XLSX file paths
xlsx_files <- list.files(path = "data/raw/cpi", pattern = "*.xlsx", full.names = TRUE)
#print("1st")
#head(xlsx_files)

# Filter out temporary files
xlsx_files <- xlsx_files[!grepl("~\\$", xlsx_files)]

#print("2nd")
#head(xlsx_files)

# Read and combine the XLSX files, selecting the desired columns and skipping rows
# We also add columns for year and month of data
inflation_data <- map_dfr(xlsx_files,function(file) { 
  read_excel(file, skip = 6) |>
    # Select the relevant columns and clean up the data
    select(expenditure_category = 2,
           percent_change = 4)|>
      mutate(year = str_extract(basename(file), "\\d{4}"),  # Extract year from file name
      month = str_extract(basename(file), "\\d{2}(?!\\d)"),  # Extract month from file name
      expenditure_category = str_replace(expenditure_category, "\\(.*", ""))|>  # Remove parentheses and text inside
      filter(!is.na(expenditure_category))  # Remove rows with NA in expenditure_category
})

#print("inflation data:")
#head(inflation_data)


## -----------------------------------------------------------------------------

# Keep only the categories we are interested in
keep_categories <- c("All items", "Airline fares", "New and used motor vehicles", "Apparel", "Shelter", "Meats, poultry, fish, and eggs", "Gasoline ")
filtered_data <- inflation_data %>%
  filter(expenditure_category %in% keep_categories)
print("filtered data:")
View(filtered_data)


## -----------------------------------------------------------------------------
#| label: merge-electricity
#| include: false
#|
# Get a list of all the energy XLSX file paths
xlsx_files <- list.files(path = "data/raw/energy", pattern = "*.xlsx", full.names = TRUE)

energy_data <- map_dfr(xlsx_files,function(file) { 
  #only read row 66, rest are irrelevant
  read_excel(file, skip = 64, n_max = 1) |>
    # Select the relevant columns
    select(current_price = 2,
           previous_price = 3) |>
          mutate(percent_change = round((current_price - previous_price) / previous_price * 100, 1),
                year = str_extract(basename(file), "\\d{4}"),  # Extract year from file name
                month = str_extract(basename(file), "\\d{2}(?!\\d)"),
                expenditure_category = "Energy")  # Extract month from file name |>
})
# drop the current_price and previous_price columns
energy_data <- energy_data |>
  select(expenditure_category, percent_change, year, month)

# merge the data
merged_data <- bind_rows(filtered_data, energy_data)



## -----------------------------------------------------------------------------
#| label: pivot-data
#| echo: false

# Pivot the data to wide format
wide_data <- merged_data %>%
  unite(year_month, year, month, sep = "/") %>%
  pivot_wider(names_from = year_month, 
              values_from = c(percent_change), 
              names_sep = "/")
write_csv(wide_data, "data/merged/filtered_wide_inflation_data.csv")

