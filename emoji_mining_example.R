set.seed(20170202)
ht <- '#nobannowall'

tweets.raw <- searchTwitter(ht, n = 1000, lang = 'en', since = '2017-01-29', until = '2017-01-29');
df <- twListToDF(strip_retweets(tweets.raw, strip_manual = TRUE, strip_mt = TRUE))
df$hashtag <- ht
df$created <- as.POSIXlt(df$created)
df$text <- iconv(df$text, 'latin1', 'ASCII', 'byte')
df$url <- paste0('https://twitter.com/', df$screenName, '/status/', df$id)
df <- rename(df, c(retweetCount = 'retweets'));
df.a <- subset(df, select = c(text, created, url, latitude, longitude, retweets, hashtag));
nrow(df.a)
head(df.a);
setwd('.../PRISMOJI/tutorial')
write.csv(df.a, paste0('tweets.cleaned_', format(min(df.a$created), '%m%d'), '-', format(max(df.a$created), '%m%d'), '_', ht, '_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '_n', nrow(df.a), '.csv'), row.names = FALSE);
tweets <- df
tweets$z <- 1
tweets$created <- as.POSIXlt(tweets$created)
nrow(tweets)
min(tweets$created)
max(tweets$created)
median(tweets$created);
system.time(df.s <- sapply(emojis$rencoding, regexpr, tweets$text, ignore.case = T, useBytes = T));
rownames(df.s) <- 1:nrow(df.s)

colnames(df.s) <- 1:ncol(df.s)

df.t <- data.frame(df.s)
df.t$tweetid <- tweets$tweetid;
# merge in hashtag data from original tweets dataset
df.a <- subset(tweets, select = c(tweetid, hashtag))
df.u <- merge(df.t, df.a, by = 'tweetid')
df.u$z <- 1
df.u <- arrange(df.u, tweetid)

tweets.emojis.matrix <- df.u;
## create emoji count dataset
df <- subset(tweets.emojis.matrix)[, c(2:843)]
count  -1);
emojis.m <- cbind(count, emojis)
emojis.m <- arrange(emojis.m, desc(count));
emojis.count  1)
emojis.count$dens <- round(1000 * (emojis.count$count / nrow(tweets)), 1)
emojis.count$dens.sm <- (emojis.count$count + 1) / (nrow(tweets) + 1);
emojis.count$rank <- as.numeric(row.names(emojis.count));
emojis.count.p <- subset(emojis.count, select = c(name, dens, count, rank));
# print summary stats
subset(emojis.count.p, rank <= 10);
num.tweets <- nrow(tweets)
df.t  -1)
num.tweets.with.emojis  0])
num.emojis <- sum(emojis.count$count);
min(tweets$created)
max(tweets$created)
median(tweets$created);
num.tweets
num.tweets.with.emojis
round(100 * (num.tweets.with.emojis / num.tweets), 1)
num.emojis
nrow(emojis.count);
df.plot <- subset(emojis.count.p, rank <= 10)
xlab <- 'Rank'



ylab <- 'Overall Frequency (per 1,000 Tweets)';
setwd('.../PRISMOJI/tutorial/ios_9_3_emoji_files')
df.plot <- arrange(df.plot, name);
imgs <- lapply(paste0(df.plot$name, '.png'), png::readPNG);
g <- lapply(imgs, grid::rasterGrob);
k <- 0.20 * (10/nrow(df.plot)) * max(df.plot$dens)
df.plot$xsize <- k
df.plot$ysize <- k
df.plot$ysize <- k * (df.plot$dens / max(df.plot$dens));
df.plot <- arrange(df.plot, name);
g1 <- ggplot(data = df.plot, aes(x = rank, y = dens)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue4') +
  xlab(xlab) + ylab(ylab) +
  mapply(function(x, y, i) {
    annotation_custom(g[[i]], xmin = x-0.5*df.plot$xsize[i], xmax = x+0.5*df.plot$xsize[i], 
                      ymin = y-0.5*df.plot$ysize[i], ymax = y+0.5*df.plot$ysize[i])},
    df.plot$rank, df.plot$dens, seq_len(nrow(df.plot))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(df.plot), 1), labels = seq(1, nrow(df.plot), 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(df.plot$dens))) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
g1;
setwd('.../PRISMOJI/tutorial')
png(paste0('emoji_barchart_', as.Date(min(tweets$created)), '_', as.Date(max(tweets$created)), '_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '_n', nrow(tweets), '.png'), 
    width = 6600, height = 4000, units = 'px', res = 1000);
g1
dev.off();