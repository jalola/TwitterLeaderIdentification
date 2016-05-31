source("setup.environment.R")
source("twitter.authentication.R")
source("utils.R")
source("download.R")
source("algorithms.R")


keywords <- "Forex OR #FX"
#keywords <- "Apple iPhone"
query <- "Forex FX investment trading technical analysis yield risk profit lost currency market"

geos <- '52.015462,4.455864,1500km'

#geos <- NULL

downloadData(keywords, geos)

#start algorithm
cleanScreenNames(keywords)

calculateSumofRetweet(keywords)

gatherTOPSISAndSave(keywords)

filterUsersByNumberOfRTs(keywords)

computeAllSimsWithQuery(keywords, query)

rankAndSaveSimilarity(keywords)

simThres <- similarityThreshold(keywords, query)

#-------------------TFIDF-------------------
sim <- read.csv(keywordTFIDFFilePath(keywords))

#-------------------INFLUENCE-------------------
#load TOPSIS for followers retweets and followings
topsis <- read.csv(keywordTOPSISFilePath(keywords))

#-------------------TOPSIS-------------------
#combine two dataframe
#View(merge(sim,topsis))

bothdf <- merge(sim,topsis)

filterSim <- bothdf[,"score"] >= simThres
filterRTs <- bothdf[,"averageRTs"] >= 1

filterBoth <- filterSim&filterRTs

#View(bothdf[filterSim, ])

#filter noise here
filteredUsers <- bothdf[filterBoth,]
#filteredUsers <- bothdf[filterSim,]
#-----------------
keep <- c("score", "retweetCount", "followersCount", "friendsCount")

#x <- as.matrix(bothdf[keep])
x <- as.matrix(filteredUsers[keep])
w <- c(5, 3, 1, 1)
i <- c("+", "+", "+", "-")
result <- topsis(x, w, i)

result.order <- result[order(result$rank),]
#View(bothdf[result.order[1:40,"alt.row"],])
View(filteredUsers[result.order[,"alt.row"],])

