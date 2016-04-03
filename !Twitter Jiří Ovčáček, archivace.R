# Archivace účtu Jiřího Ovčáčka na Twitteru (@PREZIDENTmluvci)
# ------------------------------------------------------------

# Skript R pro archivaci zvoleného účtu na Twitteru
# Jednorázově načte posledních 3200 záznamů (dalo by se jít víc do minulosti, ale bylo by nutné stránkovat - možná TODO)
# Při dalších spuštěních načítá jen nové tweety a přidává je k původní stažené databázi

# save.image("Twitter Jiří Ovčáček, archivace.RData")

library(twitteR)
library(data.table)

account <- "PREZIDENTmluvci"


# Twitter, autentizace ----------------------------------------------------

source("Auth.R")  # načte autentizační řetězce pro OAuth autentizaci
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
rm(consumer_key, consumer_secret, access_token, access_secret)


# První stažení ---------------------------------------------------------------

if(exists("tweets")) {
  d <- userTimeline(user = account, n = 100, includeRts = TRUE, sinceID = tweets[, max(id)])
} else {
  d <- userTimeline(user = account, n = 3200, includeRts = TRUE)
}

d <- twListToDF(d)
d <- as.data.table(d)

d[, text := gsub("\n", " ", text, fixed = TRUE)]
d[, text := gsub("  ", " ", text, fixed = TRUE)]

d[, created:= as.numeric(created)]
d[, created:= as.POSIXct(created, origin = "1970-01-01", tz = "CET")]

d[, reply := TRUE]
d[is.na(replyToSN), reply := FALSE]

d[, date := as.Date(created)]
d[, year := as.integer(format(date, "%Y"))]
d[, month := paste0(format(date, "%Y"), " ", format(date, "%m"))]
d[, q := paste0(format(created, "%Y"), " Q", ceiling(as.numeric(format(created, "%m")) / 3))]
d[, hour := as.integer(format(created, "%H"))]
d[, week := paste0(format(created, "%Y")," ", format(created, "%W"))]
d[, day.of.week := c("(7) neděle", "(1) pondělí", "(2) úterý", "(3) středa", "(4) čtvrtek", "(5) pátek", "(6) sobota")[as.POSIXlt(created)$wday + 1]]

hour <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
part.of.day <- c("(7) noc", "(7) noc", "(7) noc", "(7) noc", "(7) noc", "(7) noc", "(1) ráno", "(1) ráno", "(1) ráno", "(2) dopoledne",
                 "(2) dopoledne", "(2) dopoledne", "(3) brzké odpoledne", "(3) brzké odpoledne", "(3) brzké odpoledne",
                 "(4) pozdní odpoledne", "(4) pozdní odpoledne", "(4) pozdní odpoledne", "(5) večer", "(5) večer", "(5) večer",
                 "(6) pozdní večer", "(6) pozdní večer", "(6) pozdní večer")
part.of.day.temp <- data.frame(hour, part.of.day)
d <- merge(d, part.of.day.temp, by = "hour", all.x = TRUE)
rm(hour, part.of.day, part.of.day.temp)

d[, part.of.week := c("(2) víkend", "(1) všední den", "(1) všední den", "(1) všední den", "(1) všední den", "(1) všední den",
                      "(2) víkend")[as.POSIXlt(created)$wday + 1]]

d[, source := sub("<.+?>(.+?)<\\/a>", "\\1", statusSource)]
  
d[, tweet.url := paste0("https://twitter.com/", account, "/status/", id)]

d <- d[, c(2, 6, 14, 4, 13, 18, 28, 9, 27, 20, 22, 21, 23, 26, 24, 19, 25, 1), with = FALSE]

colnames(d) <- c("tweet", "created", "is.retweet", "favorited", "retweeted", "is.reply", "url", "id", "source", "year", "q", "month",
                 "week", "part.of.week", "day.of.week", "date", "part.of.day", "hour")

d <- d[order(-created)]

if(exists("tweets")) {
  tweets <- rbindlist(list(d, tweets))
} else {
  tweets <- d
}

rm(d)



# Zapsání do tabulky Google Drive -----------------------------------------

write.csv(tweets, "tweets, data to import.csv", row.names = FALSE)
# Importovat do tabulky přes Import; vybrat A2; zvolit Replace data starting at selected cell; odstranit řádek 2

save.image("Twitter Jiří Ovčáček, archivace.RData")