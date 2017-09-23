# grab the results
library(httr)
library(rvest)
library(dplyr)
library(tidyr)
library(ggplot2)

electorates <- 1:71
parties <- c("New Zealand First Party", "National Party", "Green Party", "Labour Party")

to_numeric <- function(x) {
  as.numeric(gsub("%", "", gsub(",", "", x)))
}

# now grab 2014 results
get_electorate_2014 <- function(elect) {
  base_url <- "http://archive.electionresults.govt.nz/electionresults_2014/electorate-"
  url <- sprintf("%s%d.html", base_url, elect)
  u <- read_html(url)
  elect_name <- u %>% html_nodes("h3") %>% html_text %>% strsplit(" -- ") %>% unlist
  h <- u %>% html_nodes("table") %>% html_table(fill=TRUE)
  d <- h[[5]][-(1:6),1:2]
  names(d) <- c("Party", "Votes")
  elec <- d %>% filter(Party %in% c(parties, "TOTAL")) %>%
    mutate(Votes = to_numeric(Votes))
  elec$ElectName <- elect_name[2]
  elec$Electorate <- elect
  elec
}

all2014 <- lapply(electorates, get_electorate_2014)
pv2014 <- do.call(rbind, all2014) %>%
  spread(Party, Votes)
pv2014[,3:6] <- pv2014[,3:6] / pv2014$TOTAL
pv2014 <- pv2014 %>% select(-TOTAL) %>% gather(Party, Votes, -Electorate, -ElectName)


get_electorate_2017 <- function(elect) {
  base_url <- "http://www.electionresults.govt.nz/electorate-details-"
  url <- sprintf("%s%02d.html", base_url, elect)
  h <- read_html(url) %>% html_nodes("table") %>% html_table(fill=TRUE)
  elec <- h[[3]] %>% separate(Candidates, into=c("Candidate", "CandVotes"), "\n") %>%
    separate(Party, into=c("Party", "PartyVotes"), "\n") %>% select(Party, Votes=PartyVotes) %>%
    mutate(Votes = to_numeric(Votes)) %>%
    filter(Party %in% c(parties, "TOTAL:"))
  elec$Electorate <- elect
  elec
}

all2017 <- lapply(electorates, get_electorate_2017)
pv2017 <- do.call(rbind, all2017) %>%
  mutate(Party = ifelse(Party == "TOTAL:", "TOTAL", Party)) %>%
  spread(Party, Votes)

pv2017[,2:5] <- pv2017[,2:5] / pv2017$TOTAL
pv2017 <- pv2017 %>% select(-TOTAL) %>% gather(Party, Votes, -Electorate)

u <- read_html("http://www.electionresults.govt.nz/index.html")
count <- u %>% html_nodes("h3") %>% html_text
curr <- sub(".*?([0-9\\.]+)%.*", "\\1", count[2])

library(forcats)

compare <- pv2017 %>%
  left_join(pv2014, by=c("Party", "Electorate")) %>%
  mutate(Swing = 100*(Votes.x - Votes.y)) %>%
  mutate(Party=fct_recode(Party, W="New Zealand First Party",
             N="National Party",
             L="Labour Party",
             G="Green Party"))

# now plot
ggplot(compare) +
  geom_col(aes(x=Party, y=Swing, fill=Party)) +
  facet_wrap(~ElectName) +
  scale_fill_manual(values=c("green3", "red", "blue", "grey30")) +
  guides(fill=FALSE) +
  ylab("") +
  xlab("") +
  ggtitle(paste0("Party vote 2017 swing compared to 2014 (", curr, "% counted)")) +
  theme_bw(base_size = 9) +
  theme(strip.text = element_text(size=7))

ggsave(paste0("swing_", curr, ".png"), width=10, height=10, dpi=120)

