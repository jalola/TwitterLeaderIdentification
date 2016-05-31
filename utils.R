##################DATABASE PATH#######################
#./database/keywords/tweets
#                   /screennames
#                   /result
#          /users/first character in name/second character in name/user-screenname/timeline
#                                /followers
#                /configuration/rate_limit

mainDir             <- "./database"
keywordDir          <- paste0(mainDir, "/keywords/")
userDir             <- paste0(mainDir, "/users/")
configurationDir    <- paste0(mainDir, "/configuration/")
rateLimitFilePath   <- paste0(configurationDir, "rate_limit.csv")



############CONSTANTS############
RATE_LIMIT_STATUS   <- "/application/rate_limit_status"
SEARCH_TWEET        <- "/search/tweets" 
FOLLOWERS_IDS       <- "/followers/ids" 
USER_TIMELINE       <- "/statuses/user_timeline"
USER_INFO           <- "/users/show/:id"
USER_LOOKUP         <- "/users/lookup"
RETURN_OK           <- "ok"
RETURN_LIMITED      <- "limited"
maximumDownloadable <- 400


############FUNCTIONS############
#check dir exists
checkDirExists <- function(directory){
    if(dir.exists(file.path(directory))){
        TRUE
    }
    else{
        FALSE
    } 
}

#create a folder
createDir <- function(directory){
    if(!checkDirExists(directory)){
        dir.create(file.path(directory))
    }
}

createDir(mainDir)
createDir(keywordDir)
createDir(userDir)
createDir(configurationDir)

#generate folder name for a keyword
cleanStringToFolderName <- function(aString){
    cleanString <-  gsub("#","",aString) #remove hash tag
    return(cleanString)
}

#generate keyword folder for a keyword
keywordFolderPath <- function(aKeyword){
    path <- paste0(keywordDir, cleanStringToFolderName(aKeyword), "/")
    createDir(path)
    return(path)
}

#tf_idf keyword file path
keywordTFIDFFilePath <- function(aKeyword){
    paste0(keywordFolderPath(keywords), "tf_idf.csv")
}

#topsis keyword file path
keywordTOPSISFilePath <- function(aKeyword){
    paste0(keywordFolderPath(keywords), "topsis.csv")
}

#generate tweets file path for a keyword
keywordTweetFilePath <- function(aKeyword){
    tweetFile <- paste0(keywordFolderPath(aKeyword), "tweets.csv")
    return(tweetFile)
}

keywordNumberedTweetFilePath <- function(aKeyword, number){
    tweetFile <- paste0(keywordFolderPath(aKeyword), "tweets_", number, ".csv")
    return(tweetFile)
}

#generate screennames file path for a keyword
keywordScreenNameFilePath <- function(aKeyword){
    screenNameFile <- paste0(keywordFolderPath(aKeyword), "screennames.csv")
    return(screenNameFile)
}

#clean screennames after filtering for a keyword
keywordCleanScreenNameFilePath <- function(aKeyword, withRT){
    screenNameFile <- NULL
    if(withRT){
        screenNameFile <- paste0(keywordFolderPath(aKeyword), "RT_" ,"cleanscreennames.csv")
    }
    else{
        screenNameFile <- paste0(keywordFolderPath(aKeyword), "cleanscreennames.csv")
    }
    return(screenNameFile)
}

#new user folder path due to too many users in a folder, we need to split them into numerous folders
newUserFolderPath <- function(aUserString){
    path1 <- paste0(userDir, substr(aUserString, 1, 1), "/")
    createDir(path1)
    
    path2 <- paste0(path1, substr(aUserString, 2, 2),"/")
    createDir(path2)
    
    path3 <- paste0(path2, aUserString,"/")
    createDir(path3)
    
    return(path3)
}

#user folder path for a user string
userFolderPath <- function(aUserString){
    return(newUserFolderPath(aUserString))
}

#user timeline file
userTimelineFilePath <- function(aUserString){
    userTimelineFile <- paste0(userFolderPath(aUserString), "timeline.csv")
    return(userTimelineFile)
}

#user profile file
userProfileFilePath <- function(aUserString){
    userProfileFile <- paste0(userFolderPath(aUserString), "profile.csv")
    return(userProfileFile)
}

#user similarity file
userSimFilePath <- function(aUser, aKeyword){
    aKeyword <- cleanStringToFolderName(aKeyword)
    result <- paste0(userFolderPath(aUser),"sim_",aKeyword,".csv")
}

userTopsisFilePath <- function(aUser){
    result <- paste0(userFolderPath(aUser),"topsis",".csv")
}

#get similarity of aUser with keywords
getSim <- function(aUser, aKeyword){
    result <- read.csv(userSimFilePath(aUser, aKeyword), stringsAsFactors=FALSE, fileEncoding="latin1")
    result
}

#get rate limit
getRateLimit <- function(string){
    if(file.exists(rateLimitFilePath)){
        rateLimitInfo <- read.csv(rateLimitFilePath, stringsAsFactors=FALSE, fileEncoding="latin1")
        t <- rateLimitInfo[which(rateLimitInfo$resource==string),]
        if(toString(t$reset) > as.POSIXlt(Sys.time(), "GMT")){
            return(c(as.numeric(t$remaining),toString(t$reset)))
        }
    }
    
    rateLimitInfo <- getCurRateLimitInfo()
    write.csv(rateLimitInfo, rateLimitFilePath, row.names=F)
    t <- rateLimitInfo[which(rateLimitInfo$resource==string),]
    return(c(as.numeric(t$remaining),toString(t$reset)))
}

#get tweets
getTweets <- function(aKeyword, index){
    if(!file.exists(keywordNumberedTweetFilePath(aKeyword, index))){
        NULL
    }
    else{
        tweets <- read.csv(keywordNumberedTweetFilePath(aKeyword, index), stringsAsFactors=FALSE, fileEncoding="latin1")
        tweets
    }    
}

#get user timeline
getUserTimeline <- function(aUserString){
    if(!file.exists(userTimelineFilePath(aUserString))){
        NULL
    }
    else{
        timeline <- read.csv(userTimelineFilePath(aUserString), stringsAsFactors=FALSE, fileEncoding="latin1", nrows=maximumDownloadable)
        timeline
    }  
}

#get screennames
getScreenNames <- function(aKeyword){
    if(!file.exists(keywordScreenNameFilePath(aKeyword))){
        NULL
    }
    else{
        screennames <- read.csv(keywordScreenNameFilePath(aKeyword), stringsAsFactors=FALSE, fileEncoding="latin1")
        screennames
    }    
}

#get screennames
getCleanScreenNames <- function(aKeyword, withRT){
    if(!file.exists(keywordCleanScreenNameFilePath(aKeyword, withRT))){
        NULL
    }
    else{
        screennames <- read.csv(keywordCleanScreenNameFilePath(aKeyword, withRT), stringsAsFactors=FALSE, fileEncoding="latin1")
        screennames <- apply(screennames, MARGIN = 1, FUN = as.character)
        screennames
    }    
}

#download tweets of a search string
downloadTweets <- function(aKeyword, geos){
    index <- 1
    while(T){
        if(file.exists(keywordNumberedTweetFilePath(aKeyword, index))){
            index <- index + 1
        }
        else
            break
    }
    
    ##################     GET TWEET FROM DATABASE    ##################
    tweetsFromDatabase <- getTweets(aKeyword, index-1)
    #tweetsFromDatabase <- getTweets(aKeyword, 1)
    
    sID <- NULL
    mID <- NULL
    
    needToBreak <- FALSE
    
    #if a week passed
    #if(!is.null(tweetsFromDatabase) && nrow(tweetsFromDatabase) > 0){
    #    sID <- tweetsFromDatabase[1, "id"]
    #}
    
    #else
    if(!is.null(tweetsFromDatabase) && nrow(tweetsFromDatabase) > 0){
        mID <- tweetsFromDatabase[nrow(tweetsFromDatabase), "id"]
    }
   
    downloadedTweets <- NULL
    ##################     GET RATE LIMIT    ##################
    searchTweetRateLimit <- getRateLimit(SEARCH_TWEET)
    remaining <- as.numeric(searchTweetRateLimit[1])
    resetTime <- searchTweetRateLimit[2]
    rateLimitInfo <- read.csv(rateLimitFilePath, stringsAsFactors=FALSE, fileEncoding="latin1")
    averageNumberOfTweets <- 1000
    
    ##################     SEARCH TWEET UNTIL OUT OF LIMIT    ##################
    if(remaining > 10){
        while(remaining > 10){
            tweets <- NULL
            
            result <- tryCatch({
                #download tweets from a keyword
                print(paste0("keywords: ", aKeyword, " | n: ", averageNumberOfTweets, " | maxID: ", mID, " | sinceID: ", sID, " | geocode:", geos))
                tweets <- searchTwitter(aKeyword, n = averageNumberOfTweets, lang="en", maxID = mID, sinceID = sID, geocode = geos)
            }, warning = function(w) {
                print("-------------maybe end of tweets-------------")
                print(paste0("length tweets: ", length(tweets)))
                if(!is.null(tweets) && length(tweets) > 0){
                    print(paste0("length tweets to database: ", length(tweets)))
                    tweetsDF <- twListToDF(tweets)
                    downloadedTweets <- rbind(downloadedTweets, tweetsDF)
                }
                needToBreak <- TRUE
            })
            
            if(needToBreak){
                break
            }
            
            if(is.null(tweets))
                break
            if(length(tweets) == 0){
                remaining <- remaining - 1
                break
            }
            
            tweetsDF <- twListToDF(tweets)
            downloadedTweets <- rbind(downloadedTweets, tweetsDF)
            
            print(paste0("downloading: ", nrow(downloadedTweets), " of tweets"))
            if(nrow(tweetsDF) > 0){
                mID <- as.numeric(tweetsDF[nrow(tweetsDF), "id"])
            }
            
            rateLimitInfoTmp <- getCurRateLimitInfo()
            write.csv(rateLimitInfo, rateLimitFilePath, row.names=F)
            remaining <- as.numeric(rateLimitInfoTmp[which(rateLimitInfo$resource==SEARCH_TWEET),"remaining"])
            print(paste0("remaining times: ", remaining))
            
            #something wrong with rate limit - always minus 2
            #remaining <- remaining - 1
            
            if(nrow(tweetsDF) < averageNumberOfTweets){
                break
            }
            
            if(remaining < 10){
                ##################     UPDATE NEW RATE LIMIT IF POSSIBLE    ##################
                if(resetTime <= as.POSIXlt(Sys.time(), "GMT")){
                    searchTweetRateLimit <- getRateLimit(SEARCH_TWEET)
                    remaining <- as.numeric(searchTweetRateLimit[1])
                    resetTime <- searchTweetRateLimit[2]
                }
                ##################     STOP SEARCHING DUE TO OUT OF RATE LIMIT    ##################
                else{
                    break
                }   
            }
        }
        
        ##################     UPDATE TWEET DATABASE    ##################
        if(!is.null(downloadedTweets)){
            #update tweets file
            fileName <- keywordNumberedTweetFilePath(aKeyword, index)
            print(paste0("tweet in databse: ", nrow(tweetsFromDatabase)))
            
            downloadedTweets$created <- strftime(downloadedTweets$created, '%Y-%m-%d %H:%M:%S')
            tweetsFromDatabase <- rbind(downloadedTweets, NULL)
            write.csv(tweetsFromDatabase, fileName, row.names=F)
            print(paste0("tweet(updated) in databse: ", nrow(tweetsFromDatabase)), ", to file: ", fileName)
                  
            #update screenname file
            screenNames <- getScreenNames(aKeyword)
            
            newScreenNames <- as.data.frame(tweetsFromDatabase[,"screenName"])
            if(!is.null(screenNames) && nrow(screenNames) > 0){
                names(newScreenNames) <- names(screenNames)
            }
            screenNames <- rbind(newScreenNames, screenNames)
            screenNames <- subset(screenNames, !duplicated(screenNames))
            write.csv(screenNames, keywordScreenNameFilePath(aKeyword), row.names=F)
            
            #update remaining
            print(paste0("remaining search: ", remaining))
            rateLimitInfo[which(rateLimitInfo$resource==SEARCH_TWEET), "remaining"] <- remaining
            write.csv(rateLimitInfo, rateLimitFilePath, row.names=F)
        } 
    }
}


checkUserExist <- function(aUser){
    file.exists(userProfileFilePath(aUser))
}
writeUser <- function(aUser){
    write.csv(aUser, userProfileFilePath(aUser$screenName), row.names=F)
}
#download user profile
downloadUserProfile <- function(aKeyword){
    print("---------------------------DOWNLOADING PROFILES------------------------")
    screenNames <- getScreenNames(aKeyword)
    screenNames <- apply(screenNames, MARGIN = 1, FUN = as.character)
    
    profileRateLimit <- getRateLimit(USER_LOOKUP)
    remaining <- as.numeric(profileRateLimit[1])
    resetTime <- profileRateLimit[2]
    rateLimitInfo <- read.csv(rateLimitFilePath, stringsAsFactors=FALSE, fileEncoding="latin1")
    
    total <- length(screenNames)
    count <- 1
    averagePerOneRQ <- 100
    if(remaining > 0){
        while(count <= total && remaining > 0){
            sub100screenNames <- screenNames[count:min((count+averagePerOneRQ-1), total)]
            profileExist <- sapply(sub100screenNames, checkUserExist)
            sub100screenNames <- sub100screenNames[!profileExist]
            if(length(sub100screenNames) < 5){
                count <- count + averagePerOneRQ
                next
            }
            print(paste0("downloading: ", length(sub100screenNames), " user profiles"))
            
            #sub100screenNames <- c("MuscleNerd", "iphone_dev", "saurik")
            users <- lookupUsers(sub100screenNames)
            if(length(users) == 0){
                remaining <- remaining - 1
                count <- count + averagePerOneRQ
                next
            }
            
            usersdf <- twListToDF(users)
            usersdf[,"timelimeDownloaded"] <- 0
            usersdf["timelimeDownloaded400"] <- 0
            usersdf[,"timelimeProcessed"] <- 0
            temp <- by(usersdf, 1:nrow(usersdf), function(row) writeUser(row))
            count <- count + averagePerOneRQ
            remaining <- remaining - 1
            
            if(remaining <= 0){
                ##################     UPDATE NEW RATE LIMIT IF POSSIBLE    ##################
                if(resetTime <= as.POSIXlt(Sys.time(), "GMT")){
                    searchTweetRateLimit <- getRateLimit(USER_LOOKUP)
                    remaining <- as.numeric(searchTweetRateLimit[1])
                    resetTime <- searchTweetRateLimit[2]
                }
                ##################     STOP SEARCHING DUE TO OUT OF RATE LIMIT    ##################
                else{
                    break
                }
            }
        }
    }
}

#download user timeline of a search string
downloadUserTimeline <- function(aUser){
    ##################     GET TIMELINE FROM DATABASE    ##################
    print("--------------------------------------------------------------")
    print(paste0("timeline: ", aUser))
    if(file.exists(userProfileFilePath(aUser))){
        userData <- read.csv(userProfileFilePath(aUser), stringsAsFactors=FALSE, fileEncoding="latin1")
        print(paste0("has: ", userData$statusesCount, " statuses"))
    }
    
    timelineFromDatabase <- getUserTimeline(aUser)
    
    #for timeline we don't need to use sinceid but maxid since we assume that we'll ignore few recent tweets
    sID <- NULL
    #if(!is.null(timelineFromDatabase) && nrow(timelineFromDatabase) > 0){
    #    sID <- timelineFromDatabase[1, "id"]
    #}
    
    returnCode <- RETURN_OK
    mID <- NULL
    if(!is.null(timelineFromDatabase) && nrow(timelineFromDatabase) > 0){
        mID <- as.numeric(timelineFromDatabase[nrow(timelineFromDatabase), "id"]) - 1
    }
    
    downloadedTweets <- NULL
    ##################     GET RATE LIMIT    ##################
    searchTweetRateLimit <- getRateLimit(USER_TIMELINE)
    remaining <- as.numeric(searchTweetRateLimit[1])
    resetTime <- searchTweetRateLimit[2]
    rateLimitInfo <- read.csv(rateLimitFilePath, stringsAsFactors=FALSE, fileEncoding="latin1")
    averageNumberOfTweets <- 100
    #maximumDownloadable <- 400 approximately 3 tweets per day and during 6 months if that user is active
    nDownload <- min(maximumDownloadable, remaining*averageNumberOfTweets, userData$statusesCount)
    if(remaining*averageNumberOfTweets < maximumDownloadable && remaining*averageNumberOfTweets < userData$statusesCount){
        returnCode <- RETURN_LIMITED
        return(returnCode)
    }
         
    
    ##################     SEARCH TWEET UNTIL OUT OF LIMIT    ##################
    if(remaining > 0){
        tweets <- userTimeline(aUser, n = nDownload, maxID = mID, sinceID = sID, excludeReplies = FALSE, includeRts = TRUE)
        if(is.null(tweets))
            break
        if(length(tweets) == 0){
            remaining <- remaining - ceiling(nDownload/averageNumberOfTweets)
        }
        else{
            tweetsDF <- twListToDF(tweets)
            downloadedTweets <- rbind(downloadedTweets, tweetsDF)
            
            print(paste0("downloading: ", nrow(downloadedTweets), " of tweets"))
            
            remaining <- ceiling(remaining - nDownload/averageNumberOfTweets)
            
            if(remaining < maximumDownloadable/averageNumberOfTweets){
                ##################     UPDATE NEW RATE LIMIT IF POSSIBLE    ##################
                if(resetTime <= as.POSIXlt(Sys.time(), "GMT")){
                    searchTweetRateLimit <- getRateLimit(USER_TIMELINE)
                    remaining <- as.numeric(searchTweetRateLimit[1])
                    resetTime <- searchTweetRateLimit[2]
                }
                ##################     STOP SEARCHING DUE TO OUT OF RATE LIMIT    ##################
                else{
                    returnCode <- RETURN_LIMITED
                }
            }
        }
    }
    
    ##################     UPDATE TWEET DATABASE    ##################
    if(!is.null(downloadedTweets)){
        #update tweets file
        print(paste0("tweet in databse: ", nrow(timelineFromDatabase)))
        downloadedTweets$created <- strftime(downloadedTweets$created, '%Y-%m-%d %H:%M:%S')
        timelineFromDatabase <- rbind(timelineFromDatabase, downloadedTweets)
        write.csv(timelineFromDatabase, userTimelineFilePath(aUser), row.names=F)
        print(paste0("tweet(updated) in databse: ", nrow(timelineFromDatabase)))
    }
    
    #update remaining
    print(paste0("remaining search: ", remaining))
    rateLimitInfo[which(rateLimitInfo$resource==USER_TIMELINE), "remaining"] <- remaining
    write.csv(rateLimitInfo, rateLimitFilePath, row.names=F)
    
    #update profile processed
    if(returnCode == RETURN_OK){
        if(file.exists(userProfileFilePath(aUser))){
            if(nDownload == maximumDownloadable){
                userData["timelimeDownloaded"] <- 0
                userData["timelimeDownloaded400"] <- 1
            }
            else if(nDownload == userData$statusesCount){
                userData["timelimeDownloaded"] <- 1
                userData["timelimeDownloaded400"] <- 0
            }
            else{
                userData["timelimeDownloaded"] <- 0
                userData["timelimeDownloaded400"] <- 0
            }
            
            write.csv(userData, userProfileFilePath(aUser), row.names=F)
        }    
    }
    returnCode
}