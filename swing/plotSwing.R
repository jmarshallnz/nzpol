# read in party vote info from 2011 and 2014
p2011 <- read.csv("party2011.csv", row.names=1, check.names=F)
p2014 <- read.csv("party2014.csv", row.names=1, check.names=F)

#p2011 <- read.csv("elect2011.csv", row.names=1, check.names=F)
#p2014 <- read.csv("elect2014.csv", row.names=1, check.names=F)

row.names(p2011)[row.names(p2011) == "Ōhariu"] <- "Ōhāriu"

# drop 'Party' from the party names
names(p2011) <- gsub(" Party", "", names(p2011))
names(p2014) <- gsub(" Party", "", names(p2014))

# rename 'New Zealand First' to 'NZ First' and 'ACT New Zealand' to 'ACT'
names(p2011) <- gsub("New Zealand", "NZ", names(p2011))
names(p2014) <- gsub("New Zealand", "NZ", names(p2014))
names(p2011) <- gsub("ACT NZ", "ACT", names(p2011))
names(p2014) <- gsub("ACT NZ", "ACT", names(p2014))


# find common electorates so we can work out the swings
elect <- intersect(row.names(p2011), row.names(p2014))

# parties we want to analyse
party_ctl <- read.table(

    header = TRUE,
    row.names = "Party",
    stringsAsFactors = FALSE,
    comment = "",
    text = "

    Party           Colour   Contrast Include
    National        #00529F  #CCDDFF    Y
    Labour          #FF0000  #FFBAA8    Y
    Green           #098137  #B3FFB3    Y
    'NZ First'      #000000  #CCCCCC    Y
    Conservative    #00AEEF  #33CCFF    Y
    Māori           #EF4A42  #FFCC80    Y
    ACT             #FFE401  #FFFF80    Y
    'United Future' #501557  #DD99DD    Y
    MANA            #770808  #FF6E6E    -
")

# reverse order so the barplots do what we want
party <- rev(row.names(party_ctl)[party_ctl$Include == "Y"])

e <- elect[1]
p <- party[1]

# divide by totals
p2011 <- p2011 / p2011[,"TOTAL"]
p2014 <- p2014 / p2014[,"TOTAL"]

p2011[is.na(p2011)] <- 0
p2014[is.na(p2014)] <- 0

diff <- 100*t(p2014[elect, party] - p2011[elect, party])
max_diff <- max(abs(diff), na.rm=T)
diff[diff == 0] <- NA

# tornado plot
tornado <- function(electorate) {
    barplot(diff[,electorate], col=party_ctl[party, "Contrast"], border=party_ctl[party, "Colour"], beside=T, horiz=T, xlim=c(-max_diff, max_diff), las=1, main=electorate, yaxt="n")
}

#pdf("out.pdf", width=11, height=8)
#png("cand_swing.png", width=1200, height=1350)
png("party_swing.png", width=1200, height=1350)
par(mfrow=c(9,8), mai=c(0.5,0,0.5,0))
lapply(elect[1:5], tornado)
par(mfg=c(1,7), xpd=NA, mai=c(0,0,0,0))
plot(NULL, xlim=c(-1,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
text(0, 0.75, "Party vote swings", cex=5)
text(0, 0.50, "2011-2014", cex=4)
legend(0, 0.35, legend=rev(party), fill=party_ctl[rev(party), "Contrast"], border=party_ctl[rev(party), "Colour"], bty="n", ncol=4, xjust=0.5, cex=1.2)
par(mfg=c(2,1), mai=c(0.5,0,0.5,0), xpd=F)
lapply(elect[-(1:5)], tornado)
mtext("@jmarshallnz", side=1, line=2.5, at=max_diff, adj=c(1.1), cex=0.8)
dev.off()

