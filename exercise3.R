################
## Exercise 3 ##
################

## Preparation
## Load the necessary libraries
library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times
library(zoo)          # To smoothen using moving window functions

## Import the Wildschwein Data
wildschwein_BE = read_delim("wildschwein_BE_2056.csv",",")

wildschwein_BE = st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

## Select the wild boar "Sabi"
sabi = wildschwein_BE[wildschwein_BE$TierName == "Sabi",]

## Create columns for the offsets
sabi = sabi %>%
  mutate(
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   # distance to pos -30 minutes
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -15 minutes
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +15 mintues
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2)  # distance to pos +30 minutes
  )

## Calculate the mean distance to the other points in the
## same window
sabi = sabi %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus2, nMinus1,nPlus1,nPlus2))
  ) %>%
  ungroup()

## Calculate static points (Threshold = mean distance)
sabi = sabi %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

## Filter the static points out
sabi_filter = sabi %>%
  filter(!static)

## Plot the non-static trajectories
sabi_filter%>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

## Task 1
## 
