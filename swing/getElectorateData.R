source("globals.R")

urls <- str_c("http://electionresults.org.nz/electionresults_2014/electorate-", 1:71, ".html")

library(httr) # for GET
library(stringr) # for fixing up [1]'s etc
library(plyr) # of course
library(XML) # for web scraping

url <- urls[1]

getElectorateData <- function(url) {
    message(url)
    
    # read the table
    h <- rawToChar(GET(url)$content)
#    h <- htmlParse(GET(url))

    tabs <- readHTMLTable(h, stringsAsFactors=FALSE)

    # electorate details
    elect_name <- gsub("(.*) -- (.*)", "\\2", names(tabs[[2]]))
    is_prelim  <- gsub("(.*) -- (.*)", "\\1", names(tabs[[2]])) != "Official Count Results"

    # electorate votes
    skip_rows <- ifelse(is_prelim, 8, 7)
    X <- tabs[[3]]
    X <- X[-(1:skip_rows),]
    X <- X[-nrow(X),]

    # pull out the party vote for this electorate
    parties <- X[,1]
    votes   <- as.numeric(gsub(",", "", X[,2]))
    names(votes) <- parties
    votes <- votes[c(order(names(votes)[-length(votes)]),length(votes))]
    votes <- votes[!is.na(votes)]
    votes <- data.frame(t(votes), check.names=F)
    row.names(votes) <- elect_name
#   names(votes) <- elect_name
    return(votes)
}

# candidate party to party name map
party_map <- read.table(
    header = TRUE,
    row.names = "CandParty",
    stringsAsFactors = FALSE,
    comment = "",
    text = "

    Party           CandParty
    National        NAT
    Labour          LAB
    Green           GP
    'NZ First'      NZF
    Conservative    CNSP
    Māori           MAOR
    ACT             ACT
    'United Future' UFNZ
    MANA            MANA
")

getCandidateData <- function(url) {
    message(url)
    
    # read the table
    h <- rawToChar(GET(url)$content)
#    h <- htmlParse(GET(url))

    tabs <- readHTMLTable(h, stringsAsFactors=FALSE)

    # electorate details
    elect_name <- gsub("(.*) -- (.*)", "\\2", names(tabs[[2]]))
    is_prelim  <- gsub("(.*) -- (.*)", "\\1", names(tabs[[2]])) != "Official Count Results"

    # electorate votes
    skip_rows <- ifelse(is_prelim, 8, 7)
    X <- tabs[[3]]
    X <- X[-(1:skip_rows),]
    X <- X[-nrow(X),]

    # pull out the party vote for this electorate
    parties <- X[,5]
    votes   <- as.numeric(gsub(",", "", X[,6]))
    names(votes) <- parties
    names(votes)[length(votes)] <- "TOTAL"

    # map the names to party names
    v <- lapply(row.names(party_map), function(x) { as.integer(votes[x]) })
    names(v) <- party_map[,1]
    v["TOTAL"] <- votes["TOTAL"]

    votes <- data.frame(v, check.names=F)
    row.names(votes) <- elect_name
    return(votes)
}

urls <- str_c("http://electionresults.org.nz/electionresults_2011/electorate-", 1:70, ".html")

dat <- NULL
for (i in 1:length(urls))
  dat <- rbind(dat, getCandidateData(urls[i]))


urls <- str_c("http://electionresults.org.nz/electionresults_2014/electorate-", 1:71, ".html")

dat <- NULL
for (i in 1:length(urls))
  dat <- rbind(dat, getCandidateData(urls[i]))
