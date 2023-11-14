---
title: "REC Eligibility Script"
author: "Andrew Spangler"
format:
  pdf:
    toc: true
    number-sections: true
    colorlinks: true
# server: shiny
editor: visual
---

![](REC%20Search.png){width="356"}

## Eligibility Script

### Setup

**Before starting, make sure the "REC Eligibility Program" folder is fully extracted onto your computer. Additionally, make sure the directory is set correctly by** **going into (Session - Set Working Directory - To Source File Location) in the RStudio top left menu. For each code chunk below, press the "Run Current Chunk" button in the top left corner of the code. Make sure to pay attention to the other bold text in the section descriptions because they are particularly important.**

Package Installation

```{r 1}
packages <-
  c("tidyverse","shiny","geosphere","googleway","ggmap","maps","mapdata","rmarkdown")

pInstall <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
  }
}

pInstall(packages)
```

Call Packages

```{r 2}
pLoad <- function(packages) {
  for (package in packages) {
    library(package, character.only = TRUE)
  }
}

pLoad(packages)
```

API Key Registration

```{r}
gmaps_key = "AIzaSyBs1eOoZohkWEK9ymYWznwZ-ltxJQAvnYE" 

register_google(gmaps_key)
```

### Eligibility Function

MSA Data-frame Establishment

```{r 3}
expc <- read.csv(file = 'MSA (Formatted).csv')
```

**(Don't Run This Segment Unless Using MSA Source File)** Will Not Work With Rest of Code Unless Lon-Lat Values are Reformatted

```{r 4}
# expc <- read.csv(file = '2020 MSA_ITS Tool & Graphs (Source).csv')
# 
# expc <- expc %>%
#   select(MSA.Full.Name, Rank..20) %>%
#   filter(MSA.Full.Name != "Z-Other Metropolitan Areas") %>%
#   filter(Rank..20 <= 101) %>%
#   mutate(coords = geocode(location = MSA.Full.Name, output = "latlon"))
# 
# colnames(expc)[0:3] <- c("MSA", "Export.Volume", "Coords")
```

Address Data-frame Establishment

1.  **File must be a ".csv" file type named "Input File.csv"**

2.  **The file must be formatted with the Company Name in the 1st column and** **the Headquarters Location in the 2nd. The column names don't matter, only their position.**

3.  If you have a spreadsheet with address fields in different columns, use the following excel formula, adjust the columns, and drag: `=TEXTJOIN(", ", TRUE, A2, B2, C2, D2)`

```{r 5}
df <- read.csv(file = 'Input File.csv')

colnames(df)[0:2] <- c("Company.Name", "Headquarters.Location")

df <- df %>% 
  select(`Company.Name`, `Headquarters.Location`) %>%
  mutate(coords = geocode(location = Headquarters.Location, output = "latlon")) %>%
  filter(Company.Name!="")
```

Distance Checking & Eligibility

```{r 6}
options(scipen = 999)

results <- data.frame(Company.Name = character(),
                      Headquarters.Location = character(),
                      Closest.City = character(),
                      Distance = numeric(),
                      City.Longitude = numeric(),
                      City.Latitude = numeric())

for (i in 1:nrow(df)) {

  distances <- distm(data.frame(lon = expc$Longitude, lat = expc$Latitude),
                     data.frame(lon = df$coords$lon[i], lat = df$coords$lat[i]))
  
  min_non_NaN <- function(x) {
  min(x[!is.na(x)])
}
  
  min_distance <- min_non_NaN(distances)
  closest_city <- expc$MSA[which.min(distances)]
  
    results <- rbind(results, data.frame(Company.Name = df$Company.Name[i],
                                    Headquarters.Location = df$Headquarters.Location[i],
                                    Closest.City = closest_city,
                                    Distance = round(min_distance/1609.344, 2),
                                    City.Longitude = df$coords$lon[i],
                                    City.Latitude = df$coords$lat[i]
                                    ))
}

results <- mutate(results, Eligible = ifelse(Distance>=50, "True", "False"))

```

Save File to Directory **(Will Save as "Output File.csv to Directory Folder)**

```{r}
write.csv(results, file = "Output File.csv", row.names = FALSE)
```

### Data Visualization

Map Creation

```{r 7}
us_map <- get_stamenmap(bbox = c(left = min(results$City.Longitude-1), bottom = min(results$City.Latitude-1),
                                  right = max(results$City.Longitude+1), top = max(results$City.Latitude+1)),
                        zoom = 5, maptype = "toner-background")
```

Scatter-plot w/ Eligibility

```{r}
ggmap(us_map) +
  geom_point(data = results, aes(x = City.Longitude, y = City.Latitude, color = Eligible)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude", title = "Company Locations & Eligibility", color = "Eligibility")
```

Density Heat-map

```{r}
ggmap(us_map) +
  stat_density_2d(data = results, aes(x = City.Longitude, y = City.Latitude, fill = after_stat(level), alpha = after_stat(level)),
                  geom = "polygon", bins = 100) +
  scale_fill_gradient(low = "darkslategray1", high = "blue4") +
  scale_alpha_continuous(range = c(0, 0.3)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Longitude", y = "Latitude", title = "Density Heatmap")
```

