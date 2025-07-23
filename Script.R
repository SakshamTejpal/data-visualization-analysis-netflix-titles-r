### Netflix Titles Data Visualization and Analysis (2007–2021)

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

netflix <- read\_csv("netflix\_titles.csv")

# Format and extract dates

netflix <- netflix |>
  mutate(date\_added = mdy(date\_added),
         year\_added = year(date\_added)) |>
  filter(!is.na(year\_added))

# ScatterPlot - Titles Added Over Time

netflix |>
  count(year\_added) |>
  ggplot(aes(x = year\_added, y = n)) +
  scale\_x\_continuous(breaks = scales::pretty\_breaks(n = 10))+
  geom\_point(color = "steelblue", size = 2) +
  labs(x = "Year Added", y = "Number of Titles",
       title = "Titles Added to Netflix (2007–2021)") +
  theme\_minimal()

# Compute lag between release year and the year added

netflix <- netflix |>
  mutate(lag\_years = year\_added - release\_year)

# Summarize average lag

avg\_lag <- netflix |>
  summarize(avg\_lag = mean(lag\_years, na.rm = TRUE))

# Histogram of lag years

netflix |>
  ggplot(aes(x = lag\_years)) +
  geom\_histogram(binwidth = 1, fill = "bisque3", color = "white") +
  coord\_cartesian(xlim = c(0, 40)) +
  labs(x = "Years Between Release and Addition",
       y = "Count",
       title = "Distribution of Lag Between Release Year and Netflix Addition") +
  annotate("text", x = Inf, y = Inf,
           label = paste("Avg. lag:", round(avg\_lag, 1), "years"),
           hjust = 2, vjust = 7, size = 4, color = "black") +
  theme\_minimal()

# Compute average lag per year

avg\_lag\_year <- netflix |>
  group\_by(year\_added) |>
  summarize(avg\_lag = mean(lag\_years, na.rm = TRUE),
            count = n(), .groups = "drop")

# ScatterPlot - Change in Average Lag Over Time

ggplot(avg\_lag\_year, aes(x = year\_added, y = avg\_lag)) +
  geom\_line(color = "steelblue", size = 1) +
  geom\_point(color = "black", size = 2) +
  labs(x = "Year Added", y = "Average Lag (Years)",
       title = "Average Lag Between Release and Addition Over Time") +
  theme\_minimal()

# Separate genres

genres <- netflix |>
  separate\_rows(listed\_in, sep = ",\s\*")

# Count and select top 10 genres

genre\_counts <- genres |>
  count(listed\_in, sort = TRUE)
top10\_genres <- genre\_counts |> slice(1:10)

# Pie chart - Top 10 Genres

ggplot(top10\_genres, aes(x = "", y = n, fill = listed\_in)) +
  geom\_bar(stat = "identity", width = 1, color = "white", size = 0.1) +
  coord\_polar(theta = "y") +
  labs(title = "Share of Top 10 Genres on Netflix", fill = "Genres") +
  theme\_void()

# Ridgeline Plot - Lag per Genre (lag > 1 year)

top10\_genres\_1 <- genres |>
  filter(listed\_in %in% top10\_genres\$listed\_in, lag\_years > 1)

ggplot(top10\_genres\_1, aes(x = lag\_years,
                             y = fct\_reorder(listed\_in, lag\_years, .fun = median),
                             fill = ..x..)) +
  geom\_density\_ridges\_gradient(scale = 1.5, rel\_min\_height = 0.01) +
  scale\_fill\_viridis\_c(name = "Lag (years)", option = "C") +
  labs(x = "Lag (Years)", y = "Genre",
       title = "Distribution of Lag by Genre (Top 10, lag > 1 year)") +
  theme\_minimal()

# Choropleth Map - Titles by Country

countries <- netflix |>
  separate\_rows(country, sep = ",\s\*") |>
  filter(!is.na(country) & country != "") |>
  count(country, sort = TRUE) |>
  mutate(country\_join = case\_when(
    country == "United States" \~ "United States of America",
    country == "UK" \~ "United Kingdom",
    TRUE \~ country
  ))

world <- ne\_countries(scale = "medium", returnclass = "sf")
world\_netflix <- left\_join(world, countries, by = c("name" = "country\_join"))
power\_trans\_075 <- trans\_new("trans\_0.25",
                                transform = function(x) x^0.25,
                                inverse = function(x) x^(1/0.75),
                                format = label\_number())

ggplot(world\_netflix) +
  geom\_sf(aes(fill = n), color = "white") +
  scale\_fill\_viridis\_c(option = "plasma", trans = power\_trans\_075,
                          na.value = "grey90",
                          breaks = c(10, 50, 200, 400, 1000, 2000, 3500),
                          labels = c("10", "50", "200", "400", "1000", "2000", "3500")) +
  labs(title = "Number of Netflix Titles by Country", fill = "Count") +
  theme\_minimal()

# Bar Chart - Average Lag per Country

top10\_countries <- netflix |>
  filter(!is.na(country) & country != "") |>
  group\_by(country) |>
  summarize(avg\_lag = mean(lag\_years, na.rm = TRUE), count = n(), .groups = "drop") |>
  slice\_max(count, n = 10)

ggplot(top10\_countries, aes(x = fct\_reorder(country, avg\_lag), y = avg\_lag)) +
  geom\_col(fill = "azure3") +
  coord\_flip() +
  labs(x = "Country", y = "Average Lag (Years)",
       title = "Average Lag for Top 10 Countries") +
  theme\_minimal()

# Stacked Bar Chart - Genre Composition by Country

netflix\_country\_genre <- netflix |>
  separate\_rows(country, sep = ",\s\*") |>
  separate\_rows(listed\_in, sep = ",\s\*") |>
  filter(!is.na(country) & country != "",
         !is.na(listed\_in) & listed\_in != "")

top5\_genres <- genre\_counts |> slice(1:5) |> pull(listed\_in)
top10\_countries <- netflix\_country\_genre |> count(country, sort = TRUE) |> slice\_max(n, n = 10) |> pull(country)

stack\_data <- netflix\_country\_genre |>
  filter(country %in% top10\_countries, listed\_in %in% top5\_genres) |>
  count(country, listed\_in)

ggplot(stack\_data, aes(x = fct\_reorder(country, n, sum), y = n, fill = listed\_in)) +
  geom\_bar(stat = "identity", color = "darkgrey", size = 0.01) +
  coord\_flip() +
  scale\_y\_continuous(breaks = scales::pretty\_breaks(n = 5)) +
  scale\_fill\_brewer(palette = "Set3") +
  labs(x = "Country", y = "Count of Titles",
       title = "Stacked Bar Chart: Top 10 Countries by Top 5 Genres",
       fill = "Genre") +
  theme\_minimal()

# Heatmap - Average Lag by Country and Genre

top10\_genres <- netflix\_country\_genre |>
  count(listed\_in, sort = TRUE) |>
  slice\_max(n, n = 10) |>
  pull(listed\_in)

lag\_country\_genre <- netflix\_country\_genre |>
  filter(country %in% top10\_countries, listed\_in %in% top10\_genres) |>
  group\_by(country, listed\_in) |>
  summarize(avg\_lag = mean(lag\_years, na.rm = TRUE),
            count = n(), .groups = "drop") |>
  mutate(country\_abbr = countrycode(country, origin = "country.name", destination = "iso2c"))

ggplot(lag\_country\_genre, aes(x = fct\_reorder(country\_abbr, count, sum), y = listed\_in, fill = avg\_lag)) +
  geom\_tile(color = "white") +
  geom\_text(aes(label = round(avg\_lag, 1)), size = 3, color = "black") +
  scale\_fill\_viridis\_c(option = "plasma") +
  labs(x = "Country (ISO2 codes)", y = "Genre",
       title = "Heatmap: Average Lag by Country and Genre",
       fill = "Avg Lag (Years)") +
  theme\_minimal()
