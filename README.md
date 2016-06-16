# Instructions

## Installation

* Create a new application on Twitter Apps portal https://apps.twitter.com/

* Replace 4 following variables with new application's keys and access tokens:
```
	access_token <- "Access Token"
	access_secret <- "Access Token Secret"
	apiKey <-  "Consumer Key (API Key)"
	apiSecret <- "Consumer Secret (API Secret)" 
```
* Now we can start the program by loading and running twitter.demo.R module:

```	
	source("twitter.demo.R")
```

## Structure 

* twitter.demo.R: starting the program, calling all general functions of downloading data, processing data, running algorithm, and returning the final result.

* setup.environment.R: installing and adding libraries.

* twitter.authentication.R: setting Twitter authentication for calling APIs.

* download.R: responsible for downloading data from Twitter, including recent Tweets, Users' Profiles and Users' Timeline, as well as handling waiting time due to rate limit.

* algorithms.R: implementing main algorithms regarding similarity and topsis.

* utils.R: mainly responsible for database, actual calling Twitter APIs, reading and writing data. 
