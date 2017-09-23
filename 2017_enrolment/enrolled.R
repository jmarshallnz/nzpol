# download all the info from stats nz

library(httr)
library(rvest)
library(dplyr)
library(ggplot2)

base_url <- "http://www.elections.org.nz/research-statistics/enrolment-statistics-electorate"
h <- read_html(base_url)

electorates <- h %>% html_nodes("select") %>% html_children %>% html_text
electorates <- electorates[-1]

get_electorate <- function(electorate, base_url) {
  u <- parse_url(base_url)
  h <- read_html(modify_url(u, query=list(name=electorate)))
  df <- h %>% html_nodes("table") %>% html_table
  df <- df[[1]]
  df$Electorate <- electorate
  df
}
to_numeric <- function(x) {
  as.numeric(gsub("%", "", gsub(",", "", x)))
}

all <- lapply(electorates, get_electorate, base_url=base_url)
ec <- do.call(rbind, all) %>%
  mutate_at(vars(`Est Eligible Population`:`% Enrolled`), to_numeric) %>%
  filter(Age != "Total") %>%
  mutate(Age = as.character(as.numeric(gsub("[0-9]+ - ([0-9]+)", "\\1", Age))+1))

ggplot(ec) +
  geom_col(aes(x=Age, y=`Total Enrolled`), position=position_nudge(x=-0.5), fill="steelblue") +
  scale_x_discrete(labels=c(seq(25,70,by=5),""), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,1000)) +
  facet_wrap(~Electorate) + theme_bw() +
  theme(axis.text.x = element_text(size=5))

write.csv(ec, "enrollment_20170922_1600.csv", row.names=FALSE)
old <- read.csv("enrollment_20170922_1600.csv", stringsAsFactors = FALSE, check.names = FALSE)
old$Age <- as.character(old$Age)

both <- ec %>% left_join(old, by=c('Age', 'Electorate')) %>%
  mutate(Difference = `Total Enrolled.x` - `Total Enrolled.y`)

ggplot(both) +
  geom_col(aes(x=Age, y=Difference), position=position_nudge(x=-0.5), fill="steelblue") +
  scale_x_discrete(labels=c(seq(25,70,by=5),""), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,1)) +
  facet_wrap(~Electorate) + theme_bw() +
  theme(axis.text.x = element_text(size=5))
ggsave("new_enrolments.png", width = 10, height = 8, dpi=72)
