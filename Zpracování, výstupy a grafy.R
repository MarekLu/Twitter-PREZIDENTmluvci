# Účet na Twitteru: Zpracování, výstupy a grafy

# Pracovní zápisky

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(Cairo)
library(gridExtra)

load("Twitter Jiří Ovčáček, archivace.RData")


# Zdroje (klienty) --------------------------------------------------------

round(prop.table(sort(-table(tweets$source))), 2) * 100


# Hodiny ------------------------------------------------------------------

t.hours <- tweets %>% 
  group_by(hour) %>% 
  summarise(count = n()) %>%
  arrange(hour)

ggplot(t.hours, aes(x = hour, y = count)) +
  geom_bar(stat = "identity", fill = "orange2") +
  ggtitle("\nKdy Jiří Ovčáček tweetuje, pracuje a spí\n") +
  xlab("Hodina dne") +
  ylab("Počet tweetů") +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  theme_bw()

ggsave("Výstupy/1 Kdy Jiří Ovčáček tweetuje, pracuje a spí.png", width = 10, height = 5, dpi = 90, type = "cairo-png")


# Dny ---------------------------------------------------------------------

t.daysofweek <- tweets %>% 
  group_by(day.of.week) %>% 
  summarise(count = n()) %>%
  arrange(day.of.week)

ggplot(t.daysofweek, aes(x = day.of.week, y = count)) +
  geom_bar(stat = "identity", fill = "orange2") +
  ggtitle("\nKdy Jiří Ovčáček tweetuje, pracuje a spí\n") +
  xlab("Den týdne") +
  ylab("Počet tweetů") +
  scale_x_discrete(labels = sub("\\(\\d\\).(.*)", "\\1", sort(unique(t.daysofweek$day.of.week)))) +
  theme_bw()

ggsave("Výstupy/2 Kdy Jiří Ovčáček tweetuje, pracuje a spí.png", width = 10, height = 5, dpi = 90, type = "cairo-png")


# Heatmapa: Hodiny a dny v týdnu ----------------------------------------------------

t.hours.days <- tweets %>% 
  group_by(hour, day.of.week) %>% 
  summarise(count = n()) %>%
  arrange(hour)

ggplot(t.hours.days, aes(x = hour, y = day.of.week)) +
  geom_tile(aes(fill = count)) +
  ggtitle("\nKdy Jiří Ovčáček tweetuje, pracuje a spí\n") +
  xlab("Hodina dne") +
  ylab("Den týdne") +
  scale_x_continuous(breaks = seq(0, 24, 1)) +
  # scale_y_discrete(labels = sub("\\(\\d\\).(.*)", "\\1", sort(unique(t.hours.days$day.of.week)))) +
  scale_y_discrete(c("neděle", "sobota", "pátek", "čtvrtek", "středa", "úterý", "pondělí"), 
                   labels = c("neděle", "sobota", "pátek", "čtvrtek", "středa", "úterý", "pondělí"), 
                   limits = sort(unique(t.daysofweek$day.of.week), decreasing = TRUE)) +
  scale_fill_distiller("Tweetů", palette = "Spectral") +
  theme_bw()

ggsave("Výstupy/3 Kdy Jiří Ovčáček tweetuje, pracuje a spí.png", width = 10, height = 5, dpi = 90, type = "cairo-png")


# Po měsících ---------------------------------------------------------------------

t.monthly <- tweets %>% 
  group_by(month) %>% 
  summarise(count = n()) %>%
  arrange(month)

ggplot(t.monthly, aes(x = month, y = count)) +
  geom_bar(stat = "identity", fill = "orange2") +
  ggtitle("\nMěříme výkon Jiřího Ovčáčka: počet tweetů za měsíc\n") +
  xlab("Měsíc") +
  ylab("Počet tweetů") +
  theme_bw() +
  theme(
    axis.ticks = element_blank()
  )

ggsave("Výstupy/4 Měříme výkon Jiřího Ovčáčka, po měsících.png", width = 12, height = 5, dpi = 90, type = "cairo-png")


# Počet tweetů za den, po měsících ---------------------------------------------------------------------

t.daily <- tweets %>% 
  group_by(date, month) %>% 
  summarise(count = n()) %>%
  arrange(date)

ggplot(t.daily, aes(x = month, y = count)) +
  geom_boxplot(fill = "orange2") +
  ggtitle("\nMěříme výkon Jiřího Ovčáčka: počet tweetů za den\n") +
  xlab("Měsíc") +
  ylab("Počet tweetů") +
  theme_bw() +
  coord_cartesian(ylim = c(0, 20)) +
  theme(
    axis.ticks = element_blank()
  )

ggsave("Výstupy/6 Měříme výkon Jiřího Ovčáčka, po dnech.png", width = 12, height = 5, dpi = 90, type = "cairo-png")


# Heatmapa se sloupci ----------------------------------------------------

t.hours <- tweets %>% 
  group_by(hour) %>% 
  summarise(count = n()) %>%
  arrange(hour)

t.daysofweek <- tweets %>% 
  group_by(day.of.week) %>% 
  summarise(count = n()) %>%
  arrange(day.of.week)

t.hours.days <- tweets %>% 
  group_by(hour, day.of.week) %>% 
  summarise(count = n()) %>%
  arrange(hour)

bottom <- ggplot(t.hours, aes(x = hour, y = count)) +
  geom_bar(stat = "identity", fill = "deepskyblue4") +
  xlab("Hodina dne") +
  ylab("") +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(colour = "grey90")
  )

left <- ggplot(t.daysofweek, aes(x = day.of.week, y = count)) +
  geom_bar(stat = "identity", fill = "deepskyblue4") +
  xlab("Den týdne\n") +
  ylab("") +
  coord_flip() +
  # scale_x_discrete(labels = sub("\\(\\d\\).(.*)", "\\1", limits = sort(unique(t.daysofweek$day.of.week), decreasing = TRUE))) +
  scale_x_discrete("Den týdne", labels = c("neděle", "sobota", "pátek", "čtvrtek", "středa", "úterý", "pondělí"), 
                   limits = sort(unique(t.daysofweek$day.of.week), decreasing = TRUE)) +
  ggtitle("\n \n") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey90"),
    plot.title = element_text(hjust = 0, size = 19)
  )

main <- ggplot(t.hours.days, aes(x = hour, y = day.of.week)) +
  geom_tile(aes(fill = count)) +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  scale_y_discrete("", labels = c("neděle", "sobota", "pátek", "čtvrtek", "středa", "úterý", "pondělí"), 
                   limits = sort(unique(t.daysofweek$day.of.week), decreasing = TRUE)) +
  # scale_y_discrete(limits = sort(unique(t.daysofweek$day.of.week), decreasing = TRUE)) +
  scale_fill_distiller("Tweetů", palette = "Spectral") +
  ggtitle("\nKdy Jiří Ovčáček tweetuje, pracuje a spí\n") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey90"),
    plot.title = element_text(hjust = 0, size = 19),
    panel.grid.major = element_blank()
  )

empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())


grid.arrange(left, main, empty, bottom, ncol=2, nrow=2, widths=c(1.2, 4), heights=c(4, 1.3))



# Počet tweetů za den -----------------------------------------------------

ggplot(tweets[, .(pocet = .N), by = date], aes(x = date, y = pocet)) +
  geom_bar(stat = "identity", fill = "orange2") +
  theme_bw() +
  ggtitle("\nPočet tweetů Jiřího Ovčáčka za den\n") +
  scale_x_date("", date_breaks = "1 month", date_labels = "%Y %m") +
  scale_y_continuous("") +
  expand_limits(y = 0)

ggsave("Výstupy/6 Měříme výkon Jiřího Ovčáčka, po dnech 2.png", width = 17, height = 7, dpi = 90)


# Nejoblíbenější (favorite) -----------------------------------------------

t.favorited <- tweets %>% 
  arrange(desc(favorited)) %>% 
  top_n(15, favorited) %>% 
  select(tweet, date, favorited)

kable(t.favorited, format = "pandoc")


# Nejretweetovanější ------------------------------------------------------

t.retweeted <- tweets %>% 
  filter(is.retweet == FALSE) %>% 
  arrange(desc(retweeted)) %>% 
  top_n(15, retweeted) %>% 
  select(tweet, date, retweeted)

kable(t.retweeted, format = "pandoc")