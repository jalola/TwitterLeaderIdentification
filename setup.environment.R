getPckg <- function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")

pckg = try(require(twitteR))
if(!pckg) {
    cat("Installing 'twitteR' from CRAN\n")
    getPckg("twitteR")
    require("twitteR")
}

pckg = try(require(R.utils))
if(!pckg) {
    cat("Installing 'R.utils' from CRAN\n")
    getPckg("R.utils")
    require("R.utils")
}

pckg = try(require(topsis))
if(!pckg) {
    cat("Installing 'topsis' from CRAN\n")
    getPckg("topsis")
    require("topsis")
}

pckg = try(require(ROCR))
if(!pckg) {
    cat("Installing 'ROCR' from CRAN\n")
    getPckg("ROCR")
    require("ROCR")
}

pckg = try(require(RCurl))
if(!pckg) {
    cat("Installing 'RCurl' from CRAN\n")
    getPckg("RCurl")
    require("RCurl")
}

pckg = try(require(SnowballC))
if(!pckg) {
    cat("Installing 'SnowballC' from CRAN\n")
    getPckg("SnowballC")
    require("SnowballC")
}

pckg = try(require(tm))
if(!pckg) {
    cat("Installing 'tm' from CRAN\n")
    getPckg("tm")
    require("tm")
}

pckg = try(require(wordcloud))
if(!pckg) {
    cat("Installing 'wordcloud' from CRAN\n")
    getPckg("wordcloud")
    require("wordcloud")
}

pckg = try(require(RColorBrewer))
if(!pckg) {
    cat("Installing 'RColorBrewer' from CRAN\n")
    getPckg("RColorBrewer")
    require("RColorBrewer")
}

pckg = try(require(base64enc))
if(!pckg) {
    cat("Installing 'base64enc' from CRAN\n")
    getPckg("base64enc")
    require("base64enc")
}

pckg = try(require(Ckmeans.1d.dp))
if(!pckg) {
    cat("Installing 'Ckmeans.1d.dp' from CRAN\n")
    getPckg("Ckmeans.1d.dp")
    require("Ckmeans.1d.dp")
}

pckg = try(require(sqldf))
if(!pckg) {
    cat("Installing 'sqldf' from CRAN\n")
    getPckg("sqldf")
    require("sqldf")
}

pckg = try(require(data.table))
if(!pckg) {
    cat("Installing 'data.table' from CRAN\n")
    getPckg("data.table")
    require("data.table")
}