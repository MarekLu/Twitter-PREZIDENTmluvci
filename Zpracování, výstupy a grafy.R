# Účet na Twitteru: Zpracování, výstupy a grafy

# Pracovní zápisky

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)

load("Twitter Jiří Ovčáček, archivace.RData")

# Zdroje (klienty)
round(prop.table(sort(-table(tweets$source))), 2) * 100


# Hodiny
t.hours <- tweets %>% 
  group_by(hour) %>% 
  summarise(count = n()) %>%
  arrange(hour)

ggplot(t.hours, aes(x = hour, y = count)) +
  geom_bar(stat = "identity", fill = "orange2") +
  ggtitle("Kdy Jiří Ovčáček tweetuje\n") +
  xlab("") +
  ylab("") +
  theme_bw()


# Dny
t.daysofweek <- tweets %>% 
  group_by(day.of.week) %>% 
  summarise(count = n()) %>%
  arrange(day.of.week)

ggplot(t.daysofweek, aes(x = day.of.week, y = count)) +
  geom_bar(stat = "identity", fill = "orange2") +
  ggtitle("Kdy Jiří Ovčáček tweetuje\n") +
  xlab("") +
  ylab("") +
  theme_bw()

# Nejoblíbenější (favorite)
t.favorited <- tweets %>% 
  arrange(desc(favorited)) %>% 
  top_n(15, favorited) %>% 
  select(tweet, date, favorited)

kable(t.favorited, format = "pandoc")


# Nejretweetovanější
t.retweeted <- tweets %>% 
  filter(is.retweet == FALSE) %>% 
  arrange(desc(retweeted)) %>% 
  top_n(15, retweeted) %>% 
  select(tweet, date, retweeted)

kable(t.retweeted, format = "pandoc")