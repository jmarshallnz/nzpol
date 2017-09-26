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
  d <- h[[5]][-(1:6),c(1:2, 4,6)]
  names(d) <- c("Party", "PartyVotes", "Candidate", "CandVotes")
  elec <- d %>%
    mutate(CandVotes = to_numeric(CandVotes),
           PartyVotes = to_numeric(PartyVotes))
  elec$Electorate <- elect_name[2]
  elec
}

all2014 <- lapply(electorates, get_electorate_2014)
pv2014 <- do.call(rbind, all2014)

pv_totals <- pv2014 %>% filter(Party == "TOTAL") %>%
  select(CandTotal=CandVotes, PartyTotal=PartyVotes, Electorate)

pv2014 <- pv2014 %>% filter(Party != "TOTAL") %>% left_join(pv_totals) %>%
  mutate(PercPartyVotes = PartyVotes / PartyTotal * 100) %>%
  mutate(PercCandVotes = CandVotes / CandTotal * 100)


get_electorate_2017 <- function(elect) {
  base_url <- "http://www.electionresults.govt.nz/electorate-details-"
  url <- sprintf("%s%02d.html", base_url, elect)
  u <- read_html(url)
  elect_name <- u %>% html_nodes("h2") %>% html_text() %>% strsplit("  - ") %>% unlist
  h <- read_html(url) %>% html_nodes("table") %>% html_table(fill=TRUE)
  elec <- h[[3]] %>% separate(Candidates, into=c("Candidate", "CandVotes"), "\n") %>%
    separate(Party, into=c("Party", "PartyVotes"), "\n") %>%
    mutate(PartyVotes = to_numeric(PartyVotes)) %>%
    mutate(CandVotes = to_numeric(CandVotes)) %>%
#    filter(Party %in% c(parties, "TOTAL:")) %>%
    mutate(Party = ifelse(Party == "TOTAL:", "TOTAL", Party))
  elec$Electorate <- elect_name[3]
  elec
}

all2017 <- lapply(electorates, get_electorate_2017)
pv2017 <- do.call(rbind, all2017)

pv_totals <- pv2017 %>% filter(Party == "TOTAL") %>%
  select(CandTotal=CandVotes, PartyTotal=PartyVotes, Electorate)

pv2017 <- pv2017 %>% filter(Party != "TOTAL") %>% left_join(pv_totals) %>%
  mutate(PercPartyVotes = PartyVotes / PartyTotal * 100) %>%
  mutate(PercCandVotes = CandVotes / CandTotal * 100)
#write.csv(pv2017, "dat2017.csv", row.names=FALSE)

# Some stuff to have a look at some parties
pv2017 %>% filter(Party == "Green Party") %>% arrange(desc(CandVotes))
pv2017 %>% filter(Party == "Green Party") %>% arrange(desc(PartyVotes))

pv2017 %>% filter(Party == "New Zealand First Party") %>% arrange(desc(PercPartyVotes))

pv2017 %>% filter(Party == "National Party") %>% arrange(desc(PercPartyVotes))

pv2017 %>% filter(Electorate == "Nelson") %>% arrange(desc(PercPartyVotes))

# dataset for checking candidate-effects (if they exist)
pv2017 %>% mutate(Combine = fct_collapse(Party,
  `Labour+Greens` = c("Labour Party", "Green Party"),
  `NZ First` = "New Zealand First Party",
  National = "National Party")) %>% group_by(Electorate, Combine) %>%
  summarize(PartyVotes = ifelse(sum(is.na(PartyVotes)) == length(PartyVotes), NA, sum(PartyVotes, na.rm=TRUE)),
            CandVotes=ifelse(sum(is.na(CandVotes)) == length(CandVotes), NA, sum(CandVotes, na.rm=TRUE))) -> comb

# combine 2017 and 2014 datasets
compare <- rbind(pv2017 %>% filter(Party %in% parties) %>% mutate(Election=2017),
                 pv2014 %>% filter(Party %in% parties) %>% mutate(Election=2014)) %>%
  group_by(Party) %>%
  select(Electorate, Election, PercPartyVotes) %>%
  spread(Election, PercPartyVotes) %>%
#  mutate(`2017` = `2017`/exp(mean(log(`2017`))), `2014`=`2014`/exp(mean(log(`2014`))))
  mutate(`2017` = `2017`- mean(`2017`), `2014`=`2014` - mean(`2014`),
         diff = `2017` - `2014`)

# Do a plot on the log scale. This doesn't account for the maxing out of
# party vote. e.g. in Manakau East, the PV is 65% or so in both elections.
# it's unlikely that they can improve it all that much from there.

trans <- scales::trans_new("From100", function(x) { x }, function(x) { x },
                           format=scales::percent)
ggplot(compare) +
  geom_col(aes(x=Electorate, y=`2017`/`2014`-1, fill=Party)) +
  facet_wrap(~Party,ncol=4) +
  coord_flip() +
  scale_fill_manual(values=c("green3", "red", "blue", "grey30")) +
  scale_y_continuous(trans = trans) +
  guides(fill=FALSE) +
  ylab("") +
  xlab("") +
  ggtitle("Improvement in ratio of Electorate party vote to National party vote from 2014 to 2017") +
  theme_bw(base_size = 9) +
  theme(strip.text = element_text(size=7))
ggsave("elect_performance_percent.png", width=10, height=10, dpi=80)

  


  mutate(Swing = 100*(Votes.x - Votes.y)) %>%
  mutate(Party=fct_recode(Party, W="New Zealand First Party",
                          N="National Party",
                          L="Labour Party",
                          G="Green Party"))
