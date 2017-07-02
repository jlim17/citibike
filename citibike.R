citi2017March <- read.csv("citi201703.csv")

# Age distribution of Mar 2017 riders
hist(2017-citi2017March$Birth.Year, xlab="Approx Age", main="Distribution of Citibike Rider Age", breaks=68)

# Identify missing values
attach(citi2017March)
missing_values <- function(data, variable, new_variable) {
  data$new_variable <- ifelse(is.na(variable),1,0)
  return(data$new_variable)
}
missingBirth <- missing_values(citi2017March, Birth.Year, Birth_New)

# Number of riders Mar 2017, who are 18/19/20 year old registrants
sum(citi2017March$Birth.Year==1999, na.rm=TRUE) # 1 18year 
sum(citi2017March$Birth.Year==1998, na.rm=TRUE) # 8 19year
sum(citi2017March$Birth.Year==1997, na.rm=TRUE) # 24 20year olds
sum(citi2017March$Birth.Year==1996, na.rm=TRUE) # 39 21year olds


# Find distances between start and end coordinates
library(gmapsdistance)
set.api.key("xxxxxx")



# c(Start.Station.Latitude, Start.Station.Longitude), c(End.Station.Latitude, End.Station.Longitude)
sample_start <- c("40.71836+-74.03891")
sample_end <- c("40.73760+-74.05248")
gmapsdistance(sample_start, sample_end, mode="bicycling")

start_coord <- paste(Start.Station.Latitude, Start.Station.Longitude, sep="+")
end_coord <- paste(End.Station.Latitude, End.Station.Longitude, sep="+")

# Save results of gmapsdistance to Data Frame mar17dist
mar17dist <- data.frame(matrix(ncol=3, nrow=nrow(citi2017March)))
mar17dist[1,] <- gmapsdistance(start_coord[1], end_coord[1], mode="bicycling")

marchLength <- length(citi2017March$Start.Station.Latitude)
# doesn't take into account, for example, people dropping off their bike at the same stop
# distance shouldn't be 0 in that case but... it is how it is.

for(i in 1:length(citi2017March$Start.Station.Latitude)) {
  mar17dist[i,] <- gmapsdistance(start_coord[i], end_coord[i], mode="bicycling")
}



penn <- "Pennsylvania+Station"
newark <- "Newark+Airport"
dist1 <- gmapsdistance(penn, newark, mode="driving")
dist1$Time
dist1$miles <- dist1$Distance/1609.344 # convert meters to miles
dist1$Status



#using geosphere package
library(geosphere)
for(i in 1:length(citi2017March$Start.Station.Latitude)) {
  mar17dist[i,1] <- distm(c(citi2017March$Start.Station.Latitude[i], citi2017March$Start.Station.Longitude[i]),
                         c(citi2017March$End.Station.Latitude[i], citi2017March$End.Station.Longitude[i]), fun=distHaversine)
}

distm(c(40.71836, -74.03891), c(40.73760, -74.05248), fun=distHaversine)


# UNIQUE
uniqueID <- data.frame(matrix(ncol=2, nrow=29296-15084))

for(i in 15084:29296) {
  #uniqueID[i-15083,][1] = i
}

bikeIDGroup <- group_by(citi2017March, Bike.ID)
bikeIDGroupMut <- mutate(bikeIDGroup, count=n())

bikeIDGroupMutFilt <- subset(bikeIDGroupMut, !duplicated(bikeIDGroupMut$Bike.ID))

uniqueBikeID <- bikeIDGroupMutFilt[c(12,16)]
plot(uniqueBikeID$count, main="Number of rides on a Unique Bike")


# Ok now gonna do it on the big set
citi201703full <- read.csv("citi201703full.csv")

bikeIDGroup1 <- group_by(citi201703full, Bike.ID)
bikeIDGroupMut1 <- mutate(bikeIDGroup1, count=n())
bikeIDGroupMutFilt1 <- subset(bikeIDGroupMut1, !duplicated(bikeIDGroupMut1$Bike.ID))
uniqueBikeID1 <- bikeIDGroupMutFilt1[c(12,16)]

plot(density(uniqueBikeID1$count))
density(uniqueBikeID1$count)



