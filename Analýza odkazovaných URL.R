# Analýza odkazovaných URL

save.image("Analýza odkazovaných URL.RData")

library(data.table)
library(urltools)
library(ggplot2)


# Funkce ------------------------------------------------------------------

unshorten_url <- function(uri){  # zdroj http://stackoverflow.com/a/34383991/4880707
  require(RCurl)
  if(RCurl::url.exists(uri)){
    # listCurlOptions()
    opts <- list(
      followlocation = TRUE,  # resolve redirects
      ssl.verifyhost = FALSE, # suppress certain SSL errors
      ssl.verifypeer = FALSE, 
      nobody = TRUE, # perform HEAD request
      verbose = FALSE
    );
    curlhandle = getCurlHandle(.opts = opts)
    getURL(uri, curl = curlhandle)
    info <- getCurlInfo(curlhandle)
    rm(curlhandle)  # release the curlhandle!
    info$effective.url
  } else {
    # just return the url as-is
    uri
  }
}


# Vytažení skutečných URL ---------------------------------------------------

load("Twitter Jiří Ovčáček, archivace.RData")

d <- copy(tweets)
rm(tweets)

d <- d[grepl("http", tweet),]
d[, link := sub(".*(http.+)", "\\1", tweet)]
d[, link := sub("(http.+)\\s.*", "\\1", link)]

d <- d[!grepl("…", link)]
d[, link.true := as.character(NA)]


# Jde to hodně pomalu. U erroru stačí pokračovat od řádku s chybou
for (n in 1:nrow(d)) {
# for (n in 1:37) {
  print(paste0(n, "/", nrow(d)))
  d[n, link.true := unshorten_url(d$link[n])] 
}
rm(n)

d.zaloha <- d

# Úpravy ------------------------------------------------------------------

d <- d[!grepl("twitter.com", link.true)]  # zůstalo 530 s odkazem
d <- d[!grepl("\\/t.co\\/", link.true)]
d <- d[!is.na(link.true)]

d[, domain := url_parse(link.true)$domain]
d[, domain.simple := sub("([^.]*\\.[^.]{2,3})(?:\\.[^.]{2,4})?$", "\\2", domain)]
d[, domain.simple := nchar(domain.simple)]
d[, domain.simple := substr(domain, domain.simple + 1, 1000)]

# Prvních deset
result <- d[, .(count = .N), by = domain.simple][order(-count)][1:10]  # zůstalo 507, 96 % URL
result <- as.data.frame(result)
colnames(result) <- c("domain", "count")
result$domain <- as.factor(result$domain)
result$domain <- reorder(result$domain, result$count)


# Graf --------------------------------------------------------------------

ggplot(result, aes(x = domain, y = count)) +
  geom_bar(stat = "identity", fill = "orangeRed3") +
  coord_flip() +
  theme_bw() +
  scale_x_discrete("") +
  scale_y_continuous("Počet odkazů") +
  ggtitle("\nKam vedou odkazy z tweetů Jiřího Ovčáčka (top 10)\n") +
  theme(
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank()
  )
