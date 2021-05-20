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
library(SimilarityMeasures)   # Similarity measures

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
## Import the Caro Data
caro = read_delim("caro60.csv",",")

## Calculate the offset distances between the points
caro = caro %>%
  mutate(
    nMinus3 = sqrt((lag(E,3)-E)^2+(lag(N,3)-N)^2),   # distance to pos -3 minutes
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   # distance to pos -2 minutes
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -1 minutes
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +1 minutes
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2),# distance to pos +2 minutes
    nPlus3  = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2)  # distance to pos +3 minutes
  )

## Calculate the mean distance to the other points in the
## same window
caro = caro %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3,nMinus2,nMinus1,
                      nPlus1,nPlus2,nPlus3))
  ) %>%
  ungroup()

## Task 2
## Select a reasonable distance threshold
boxplot(caro$stepMean)
hist(caro$stepMean)

# Threshold = mean distance
caro = caro %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

## Task 3
## plot the segmented "stop" and "move" trajectories
caro %>%
  ggplot(aes(E, N, colour = static))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "right")

## Task 4
## Function for the assignment of a unique Identifier
rle_id = function(vec){
  x = rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

## Assign unique identifier to all segments
caro = caro %>%
  mutate(segment_id = rle_id(static))

## Filter the static points out
caro = caro %>%
  filter(!static)

## Plot the uncleaned trajectories
caro %>%
  ggplot(aes(E, N, colour = segment_id))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "right")

## Clean out trajectories of length <5 minutes
table(caro$segment_id) < 5
names(which(table(caro$segment_id) < 5))
caro_cleaned=caro[!caro$segment_id %in% names(which(table(caro$segment_id) < 5)), ]
# There is probably an easier way of doing this? If yes, I
# would be happy for Feedback

## Plot the cleaned trajectories
caro_cleaned %>%
  ggplot(aes(E, N, colour = segment_id))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "right")

## Task 5
## Import the pedestrian data
ped = read_delim("pedestrian.csv",",")
ped$TrajID_char = as.character(ped$TrajID)

## Plot the different trajectories
ped %>%
  ggplot(aes(E, N,colour=TrajID_char)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  facet_wrap("TrajID") +
  theme(legend.position = "right")

## Task 6
## Define the matrices with the different trajectories
traj1 = as.matrix(ped[ped$TrajID==1,c(2,3)])
traj2 = as.matrix(ped[ped$TrajID==2,c(2,3)])
traj3 = as.matrix(ped[ped$TrajID==3,c(2,3)])
traj4 = as.matrix(ped[ped$TrajID==4,c(2,3)])
traj5 = as.matrix(ped[ped$TrajID==5,c(2,3)])
traj6 = as.matrix(ped[ped$TrajID==6,c(2,3)])

## DTW
dtw = rep(0,5)
dtw[1] = DTW(traj1,traj2)
dtw[2] = DTW(traj1,traj3)
dtw[3] = DTW(traj1,traj4)
dtw[4] = DTW(traj1,traj5)
dtw[5] = DTW(traj1,traj6)

## Frechet
frechet = rep(0,5)
frechet[1] = Frechet(traj1,traj2)
frechet[2] = Frechet(traj1,traj3)
frechet[3] = Frechet(traj1,traj4)
frechet[4] = Frechet(traj1,traj5)
frechet[5] = Frechet(traj1,traj6)

## EditDist
editdist = rep(0,5)
editdist[1] = EditDist(traj1,traj2)
editdist[2] = EditDist(traj1,traj3)
editdist[3] = EditDist(traj1,traj4)
editdist[4] = EditDist(traj1,traj5)
editdist[5] = EditDist(traj1,traj6)

## LCSS (Do not run, if you have little time!)
lcss = rep(0,5)
lcss[1] = LCSS(traj1,traj2)
lcss[2] = LCSS(traj1,traj3)
lcss[3] = LCSS(traj1,traj4)
lcss[4] = LCSS(traj1,traj5)
lcss[5] = LCSS(traj1,traj6)

## Plot and compare the similarity measures
barplot(dtw, ylab="DTW")
barplot(frechet, ylab="Frechet")
barplot(editdist, ylab="EditDist")
#barplot(lcss, ylab="LCSS")
