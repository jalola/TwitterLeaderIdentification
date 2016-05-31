# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

access_token <- "--------------------------------------------"
access_secret <- "--------------------------------------------"
apiKey <-  "--------------------------------------------"
apiSecret <- "--------------------------------------------"

setup_twitter_oauth(apiKey, apiSecret, access_token, access_secret)
c