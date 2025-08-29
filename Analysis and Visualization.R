S---
title: "Netflix Titles (2007-2021)"
author: "Saksham Tejpal (100874871)"
date: "2025-03-24"
output: html_document
---

# Import libraries
library(tidyverse)
library(ggridges)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(scales)
library(countrycode)

# Import dataset
netflix <- read_csv("netflix_titles.csv")

# Format and extract dates
netflix <- netflix |> 
  mutate(date_added = mdy(date_added),
         year_added = year(date_added)) |> 
  filter(!is.na(year_added))

# ScatterPlot Data
netflix |> 
  count(year_added) |> 
  ggplot(aes(x = year_added, y = n)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_point(color = "steelblue", size = 2) +
  labs(x = "Year Added", y = "Number of Titles", 
       title = "Titles Added to Netflix (2007–2021)") +
  theme_minimal()

# Compute lag between release year and the year added
netflix <- netflix |> 
  mutate(lag_years = year_added - release_year)

# Summarize average lag
avg_lag <- netflix |> 
  summarize(avg_lag = mean(lag_years, na.rm = TRUE))

# Histogram of lag years, with 1 to 40 years of lag
netflix |> 
  ggplot(aes(x = lag_years)) +
  geom_histogram(binwidth = 1, fill = "bisque3", color = "white") +
  coord_cartesian(xlim = c(0, 40)) +
  labs(x = "Years Between Release and Addition", 
       y = "Count", 
       title = "Distribution of Lag Between Release Year and Netflix Addition") +
  annotate("text", x = Inf, y = Inf, 
           label = paste("Avg. lag:", round(avg_lag, 1), "years"), 
           hjust = 2, vjust = 7, size = 4, color = "black") +
  theme_minimal()

# Compute the average lag per year
avg_lag_year <- netflix |>  
  group_by(year_added) |> 
  summarize(avg_lag = mean(lag_years, na.rm = TRUE),
            count = n(), .groups = "drop")

# ScatterPlot to Show the change
ggplot(avg_lag_year, aes(x = year_added, y = avg_lag)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "black", size = 2) +
  labs(x = "Year Added", y = "Average Lag (Years)",
       title = "Average Lag Between Release and Addition Over Time") +
  theme_minimal()

# Separate genres
genres <- netflix |>  
  separate_rows(listed_in, sep = ",\\s*")

# Count frequency of each genre
genre_counts <- genres |> 
  count(listed_in, sort = TRUE)

# Select the top 10 genres
top10_genres <- genre_counts |>  
  slice(1:10)

# Piechart to display split of top 10 genres
ggplot(top10_genres, aes(x = "", y = n, fill = listed_in)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 0.1) +
  coord_polar(theta = "y") +
  labs(title = "Share of Top 10 Genres on Netflix",
       fill = "Genres") +
  theme_void()

# Filter the top 10 genres and lag_years > 1
top10_genres_1 <- genres |> 
  filter(listed_in %in% top10_genres$listed_in, lag_years > 1)

# Ridgeline Plot for Top 10 Genre
ggplot(top10_genres_1, aes(x = lag_years, 
             y = fct_reorder(listed_in, lag_years, .fun = median), 
             fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Lag (years)", option = "C") +
  labs(x = "Lag (Years)", 
       y = "Genre", 
       title = "Distribution of Lag by Genre (Top 10, lag > 1 year)") +
  theme_minimal()

# Separate the country column
countries <- netflix |> 
  separate_rows(country, sep = ",\\s*") |> 
  filter(!is.na(country) & country != "")

# Adjust country names 
countries <- countries |> 
  count(country, sort = TRUE) |> 
  mutate(country_join = case_when(
    country == "United States" ~ "United States of America",
    country == "UK" ~ "United Kingdom",
    TRUE ~ country
  ))

# World Map Data in sf format
world <- ne_countries(scale = "medium", returnclass = "sf")

# Join the Netflix counts with the world data
world_netflix <- left_join(world, countries, by = c("name" = "country_join"))

# Custom color transformation 
power_trans_075 <- trans_new("trans_0.25",
                             transform = function(x) x^0.25,
                             inverse = function(x) x^(1/0.75),
                             format = label_number())

# World Map Plot for Different Countries
ggplot(world_netflix) +
  geom_sf(aes(fill = n), color = "white") +
  scale_fill_viridis_c(
    option = "plasma", 
    trans = power_trans_075, 
    na.value = "grey90",
    breaks = c(10, 50, 200, 400, 1000, 2000, 3500),
    labels = c("10", "50", "200", "400", "1000", "2000", "3500")
  ) +
  labs(title = "Number of Netflix Titles by Country",
       fill = "Count") +
  theme_minimal()

# Summarize average lag per country
avg_lag_country <- netflix |> 
  filter(!is.na(country) & country != "") |> 
  group_by(country) |> 
  summarize(avg_lag = mean(lag_years, na.rm = TRUE),
            count = n(), .groups = "drop")

# Top 10 Countries
top10_countries <- avg_lag_country |>  
  slice_max(count, n = 10)

# Plot average lag for the top 10 countries by count
ggplot(top10_countries, aes(x = fct_reorder(country, avg_lag), y = avg_lag)) +
  geom_col(fill = "azure3") +
  coord_flip() +
  labs(x = "Country", y = "Average Lag (Years)",
       title = "Average Lag for Top 10 Countries") +
  theme_minimal()

# Separate the country column
netflix_country_genre <- netflix |> 
  separate_rows(country, sep = ",\\s*") |>  
  separate_rows(listed_in, sep = ",\\s*") |>  
  filter(!is.na(country) & country != "", 
         !is.na(listed_in) & listed_in != "")

# Select the top 5 genres
top5_genres <- genre_counts |>  
  slice(1:5) |>   
  pull(listed_in)

# Select the top 10 Countries
top10_countries <- netflix_country_genre %>% 
  count(country, sort = TRUE) %>% 
  slice_max(n, n = 10) %>% 
  pull(country)

# Filter dataset for top countries and genres
stack_data <- netflix_country_genre |> 
  filter(country %in% top10_countries,
         listed_in %in% top5_genres) |>  
  count(country, listed_in)

# Stacked bar chart
ggplot(stack_data, aes(x = fct_reorder(country, n, sum), y = n, fill = listed_in)) +
  geom_bar(stat = "identity",color = "darkgrey", size = 0.01) +
  coord_flip() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Country", y = "Count of Titles", 
       title = "Stacked Bar Chart: Top 10 Countries by Top 5 Genres",
       fill = "Genre") +
  theme_minimal()

# Compute top 10 genres from the same dataset
top10_genres <- netflix_country_genre |>  
  count(listed_in, sort = TRUE) |>  
  slice_max(n, n = 10) |> 
  pull(listed_in)

# Aggregate average lag and count for each country and genre
lag_country_genre <- netflix_country_genre |> 
  filter(country %in% top10_countries,
         listed_in %in% top10_genres) |>  
  group_by(country, listed_in) |>  
  summarize(avg_lag = mean(lag_years, na.rm = TRUE),
            count = n(), .groups = "drop") |> 
  # Convert country names to ISO2 codes
  mutate(country_abbr = countrycode(country, origin = "country.name", destination = "iso2c"))

# Create a heatmap using the aggregated data
ggplot(lag_country_genre, aes(x = fct_reorder(country_abbr, count, sum), y = listed_in, fill = avg_lag)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(avg_lag, 1)), size = 3, color = "black") +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = "Country (ISO2 codes)", y = "Genre", 
       title = "Heatmap: Average Lag by Country and Genre",
       fill = "Avg Lag (Years)") +
  theme_minimal()


