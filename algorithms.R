cleanScreenNames <- function(keywords){
    screenNames <- getScreenNames(keywords)
    sc <- apply(screenNames, MARGIN = 1, FUN = as.character)
    filterScreenNames <- array(dim = length(sc))
    
    count <- 1
    countProtected <- 0
    countMissingTimeline <- 0
    countLackOfFollowers <- 0
    countLackOfTweets <- 0
    countTooManyFollowings <- 0
    
    countMissingProfile <- 0
    
    for(aUser in sc){
        if(file.exists(userProfileFilePath(aUser))){
            userData <- read.csv(userProfileFilePath(aUser))
            
            if(userData[1, "timelimeDownloaded"] != 1 && userData[1, "timelimeDownloaded400"] != 1 && userData[1, "protected"] == TRUE){
                countProtected <- countProtected+1
                filterScreenNames[count] <- FALSE
            }
            else if(userData[1, "followersCount"] <= 30){
                #print(paste0(userData[1, "screenName"], " has less than 30 followers: ", userData[1, "followersCount"]))
                countLackOfFollowers <- countLackOfFollowers+1
                filterScreenNames[count] <- FALSE
            }
            else if(userData[1, "statusesCount"] <= 50){
                #print(paste0(userData[1, "screenName"], " has less than 50 tweets: ", userData[1, "statusesCount"]))
                countLackOfTweets <- countLackOfTweets+1
                filterScreenNames[count] <- FALSE
            }
            else if((userData[1, "followersCount"]*2 < userData[1, "friendsCount"])){
                #print(paste0(userData[1, "screenName"], " has friends more than followers: ", 
                #            userData[1, "followersCount"], ", ", userData[1, "friendsCount"]))
                countTooManyFollowings <- countTooManyFollowings+1
                filterScreenNames[count] <- FALSE
            }
            else if(!file.exists(userTimelineFilePath(aUser))){
                print(paste0(userData[1, "screenName"], " timeline is missing"))
                countMissingTimeline <- countMissingTimeline+1
                filterScreenNames[count] <- FALSE
            }
            else{
                filterScreenNames[count] <- TRUE
            }
        }
        else{
            countMissingProfile <- countMissingProfile+1
            print(paste0("missing name: "))
            filterScreenNames[count] <- FALSE
        }
        count <- count+1
        print(paste0("count: ", count))
    }
    
    print(paste0("countProtected: ", countProtected))
    print(paste0("countMissingTimeline: ", countMissingTimeline))
    print(paste0("countLackOfFollowers: ", countLackOfFollowers))
    print(paste0("countLackOfTweets: ", countLackOfTweets))
    print(paste0("countTooManyFollowings: ", countTooManyFollowings))
    print(paste0("countMissingProfile: ", countMissingProfile))
    
    cleanScreenNames <- sc[filterScreenNames]
    
    #allScreenNames <- getCleanScreenNames(keywords, F)
    #allScreenNames[640:642] <- c("MuscleNerd", "iphone_dev", "saurik")
    write.csv(cleanScreenNames, keywordCleanScreenNameFilePath(keywords, withRT = F), row.names=F)
}

filterUsersByNumberOfRTs <- function(keywords){
    topsis <- read.csv(keywordTOPSISFilePath(keywords))
    filterRTs <- topsis[,"averageRTs"] >= 1
    remainingUsers <- topsis[filterRTs, "screenName"]
    write.csv(remainingUsers, keywordCleanScreenNameFilePath(keywords, withRT = T), row.names=F)
}


similarityThreshold <- function(keywords, query){
    numberOfWordsInQuery <- sapply(gregexpr("\\W+", query), length) + 1
    #averageNumberOfWordsInTweets <- averageNumberOfWords(keywords)
    averageNumberOfWordsInTweets <- 11.58508
    expectedLeastTime <- 3
    threshold <- 1/ sqrt(numberOfWordsInQuery * averageNumberOfWordsInTweets) / expectedLeastTime
}

getTermTF_IDF <- function(aVec, n){
    #calculate df
    df <- sum(aVec[1:n] > 0)
    weight <- rep(0, n)
    #calculate tfidf for a term: using (1+log2(tf))*log2(n/df)
    weight[aVec > 0] <- (1 + log2(aVec[aVec > 0]))*log2(n/df)
    weight
}

getTermLogTF <- function(aVec, n){
    weight <- rep(0, n)
    #calculate tf weight for a term: using (1+log2(tf))
    weight[aVec > 0] <- (1 + log2(aVec[aVec > 0]))
    weight
}

getTermTF <- function(aVec, n){
    weight <- rep(0, n)
    #calculate tf weight for a term: using (1+log2(tf))
    weight[aVec > 0] <- aVec[aVec > 0]
    weight
}

#clean Tweet Text
cleanTweetText <- function(text){
    ############################
    text <- sapply(text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    #convert all text to lower case
    text <- tolower(text)
    # Remove links
    text <- gsub("https://[^ ]*", " ", text)
    # Replace @UserName
    text <- gsub("@\\w+", "", text)
    # Remove punctuation
    text <- gsub("[[:punct:]]", " ", text)
    # Remove tabs
    text <- gsub("[ |\t]{2,}", " ", text)
    # Remove blank spaces at the beginning
    text <- gsub("^ ", "", text)
    # Remove blank spaces at the end
    text <- gsub(" $", "", text)
    text <- gsub("[\r\n]", " ", text)
    text
}

#compute similarity with a query of a user
simUser <- function(aUser, query, aKeyword){
    print(paste0("processing user: ", aUser))
    if(file.exists(userSimFilePath(aUser,aKeyword))){
        print("already proceeded")
        return(NULL)
    }
    aTimeline <- getUserTimeline(aUser)
    
    listTweets <- cleanTweetText(aTimeline[,"text"])
    #listTweets <- as.character(listTweets)
    
    count <- length(listTweets)
    names(listTweets) <- paste0("tweet", c(1:count))
    documents <- VectorSource(c(listTweets, query))
    documents$names <- c(names(listTweets), "query")
    count <- count+1
    
    corpus <- Corpus(documents)
    
    corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
    corpus <- tm_map(corpus, stemDocument, language = "english")
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, stripWhitespace)
    
    #build tf for all documents
    docs.matrix.stm <- TermDocumentMatrix(corpus)
    docs.matrix <- as.matrix(docs.matrix.stm)
    
    #using TF IDF as a weight
    #tfidf.matrix <- t(apply(docs.matrix, c(1), FUN = function(x) getTermTF_IDF(x, count)))
    
    #using just TF as a weight
    #tfidf.matrix <- t(apply(docs.matrix, c(1), FUN = function(x) getTermTF(x, count)))
    
    #using just (1 + ln(TF)) as a weight
    tfidf.matrix <- t(apply(docs.matrix, c(1), FUN = function(x) getTermLogTF(x, count)))
    
    colnames(tfidf.matrix) <- colnames(docs.matrix)
    
    #normalize the matrix
    tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))
    
    query.vector <- tfidf.matrix[, count]
    count <- count-1
    tfidf.matrix <- tfidf.matrix[, 1:count]
    
    doc.scores <- t(query.vector) %*% tfidf.matrix
    
    #remove NA
    doc.scores[is.na(doc.scores)] <- 0
    #results.df <- data.frame(doc = names(listTweets), score = t(doc.scores), text = unlist(listTweets))
    
    #add score to tweets
    #aTimeline["score"] <- t(doc.scores)
    write.csv(t(doc.scores), userSimFilePath(aUser,aKeyword), row.names=F)
    
    #results.df <- results.df[order(results.df$score, decreasing = TRUE), ]
    
    #options(width = 5000)
    #print(results.df[1:3,], row.names = FALSE, right = FALSE, digits = 2)
}

#compute all similarity of all user timeline with query
computeAllSimsWithQuery <- function(keywords, queryStr){
    allScreenNames <- getCleanScreenNames(keywords, T)
    r <- lapply(allScreenNames, FUN = function(x) simUser(x, queryStr, keywords))
}


#rank similarity
rankAndSaveSimilarity <- function(keywords){
    allScreenNames <- getCleanScreenNames(keywords, T)
    simAverages <- sapply(allScreenNames, FUN = function(x) { scores <- read.csv(userSimFilePath(x,keywords)); scores[is.na(scores)] <- 0; mean(scores[,1], na.rm=T) })
    result <- data.frame(screenName = allScreenNames, score = simAverages)
    result.order_score <- result[order(result$score, decreasing = TRUE), ]
    write.csv(result, keywordTFIDFFilePath(keywords), row.names=F)
    #View(result.order_score)
}

#topsis algorithm
computeAllTopsisFactors <- function(aUser){
    print(paste0("processing user: ", aUser))
    if(file.exists(userTopsisFilePath(aUser))){
        print("already processed")
        return(NULL)
    }
    aTimeline <- getUserTimeline(aUser)
    userProfile <- read.csv(userProfileFilePath(aUser))
    numberOfRetweets <- 0
    averageRetweets <- 0
    if(!is.null(aTimeline)){
        numberOfRetweets <- sum(aTimeline[which(aTimeline[,"isRetweet"] == FALSE),"retweetCount"])
        if(nrow(aTimeline) > 0)
            averageRetweets = numberOfRetweets/nrow(aTimeline)
    }
    df <- data.frame(retweetCount = numberOfRetweets, followersCount = userProfile[1,"followersCount"], friendsCount = userProfile[1,"friendsCount"], averageRTs = averageRetweets)
    write.csv(df, userTopsisFilePath(aUser), row.names=F)
}

calculateSumofRetweet <- function(keywords){
    allScreenNames <- getCleanScreenNames(keywords, F)
    r <- lapply(allScreenNames, FUN = function(x) computeAllTopsisFactors(x))
}

gatherTOPSISAndSave <- function(keywords){
    allScreenNames <- getCleanScreenNames(keywords, F)
    r <- sapply(allScreenNames, FUN = function(x) read.csv(userTopsisFilePath(x)))
    x <- cbind(allScreenNames,t(r))
    colnames(x)[1] <- "screenName"
    write.csv(x, keywordTOPSISFilePath(keywords), row.names=F)
}


#calculate average number of words in a tweet for all users
averageNumberOfWords <- function(keywords){
    allScreenNames <- getCleanScreenNames(keywords, F)
    r <- sapply(allScreenNames, FUN = function(x) averageNumberOfWordsInTweetOfUser(x))
    mean(r)
}

averageNumberOfWordsInTweetOfUser <- function(aUser){
    print(paste0("processing user: ", aUser))
    #if(file.exists(userSimFilePath(aUser,aKeyword))){
    #    print("already proceeded")
    #    return(NULL)
    #}
    aTimeline <- getUserTimeline(aUser)
    listTweets <- cleanTweetText(aTimeline[,"text"])
    listLength <- sapply(gregexpr("\\W+", listTweets), length) + 1
    mean(listLength)
}

