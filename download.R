
downloadMultipleUserTimeline <- function(keywords){
    returnCode <- RETURN_OK
    screenNames <- getScreenNames(keywords)
    sc <- sapply(screenNames, MARGIN = 1, FUN = as.character)
    for(aUser in sc){
        if(file.exists(userProfileFilePath(aUser))){
            #print("user exists")
            userData <- read.csv(userProfileFilePath(aUser))
            #if user Profile is not fully downloaded, then we continue to download it
            if(userData[1, "timelimeDownloaded"] != 1 && userData["timelimeDownloaded400"] != 1 && 
                   userData[1, "followersCount"] > 30 && userData[1, "statusesCount"] > 50 && 
                   userData[1, "protected"] == FALSE){
                result <- tryCatch({
                    returnCode <- downloadUserTimeline(aUser)
                    #c("MuscleNerd", "iphone_dev", "saurik")
                    #downloadUserTimeline("saurik")
                }, warning = function(w) {
                    print("-------------warning in download A timeline---------------")
                }, error = function(e) {
                    print("-------------error in download A timeline---------------")
                    print(paste("MY_ERROR:  ",e$message))
                    
                    if(grepl("OAuth authentication error", e$message) || grepl("Not Found", e$message)){
                        print("mark this user as protected or not found")
                        userData[1, "protected"] = TRUE
                        write.csv(userData, userProfileFilePath(userData$screenName), row.names=F)
                    }
                }, finally = {
                    print("-------------finally in download A timeline---------------")
                })
                
                if(returnCode == RETURN_LIMITED)
                    break
            }
        }
    }
    returnCode
}

downloadData <- function(keywords, geos){
    rateLimitInfo <- getRateLimit(RATE_LIMIT_STATUS)
    
    returnCode <- RETURN_OK
    
    while(TRUE){
        result <- tryCatch({
            #download tweets from a keyword
            downloadTweets(keywords, geos)
        }, warning = function(w) {
            print("-------------warning in download tweets---------------")
        }, error = function(e) {
            print("-------------error in download tweets---------------")
            print(paste("MY_ERROR:  ",e))
        }, finally = {
            print("-------------finally in download tweets---------------")
        })
        
        result <- tryCatch({
            #download userprofiles who mentioned about that keyword
            downloadUserProfile(keywords)
        }, warning = function(w) {
            print("-------------warning in download profile---------------")
        }, error = function(e) {
            print("-------------error in download profile---------------")
            print(paste("MY_ERROR:  ",e))
        }, finally = {
            print("-------------finally in download profile---------------")
        })
        
        result <- tryCatch({
            #download usertimeline for tf-idf calculation
            returnCode <- downloadMultipleUserTimeline(keywords)
        }, warning = function(w) {
            print("-------------warning in download timeline---------------")
        }, error = function(e) {
            print("-------------error in download timeline---------------")
            print(paste("MY_ERROR:  ",e))
            returnCode <- RETURN_LIMITED
            print(paste0("return code", returnCode))
        }, finally = {
            print("-------------finally in download timeline---------------")
        })
        
        rateLimitSearchTweet <- getRateLimit(SEARCH_TWEET)
        profileRateLimit <- getRateLimit(USER_LOOKUP)
        rateLimitUserTimeline <- getRateLimit(USER_TIMELINE)
        
        ######## DONE SEARCHING ########
        if(rateLimitSearchTweet[1] > 0 && profileRateLimit[1] > 0 && returnCode == RETURN_OK)
            break
        
        ######## SEARCHING IS BEING LIMITED  ########
        else {
            minTime <- ""
            limitCategory <- ""
            ######## TWEET SEARCHING IS LIMITED  ########
            if(rateLimitSearchTweet[1] <= 0){
                minTime <- rateLimitSearchTweet[2]
                limitCategory <- SEARCH_TWEET
            }
            ######## USERTIMELINE SEARCHING IS LIMITED  ########
            else if(profileRateLimit[1] <= 0){
                minTime <- profileRateLimit[2]
                limitCategory <- USER_LOOKUP
            }
            else{
                minTime <- rateLimitUserTimeline[2]
                limitCategory <- USER_TIMELINE
            }
            
            # Sleep until the next rate limit will reset
            print("==============WAITING FOR RESET TIME=================")
            print(paste0("limit search: ", limitCategory))
            print(paste0("reset time: ", minTime))
            waitingTime <- as.numeric(ceiling(abs(difftime(as.POSIXlt(minTime, "GMT"), as.POSIXlt(Sys.time(), "GMT"), units = "secs"))))
            print(paste0("waiting time(in second): ", waitingTime))
            Sys.sleep(min(waitingTime, 15*60))
        }
    }
}
