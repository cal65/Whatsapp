setwd('C:/Users/212590921/Documents/Personal_Projects/Whatsapp')
Sys.setlocale(category='LC_ALL')
library(plyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(ggrepel)
options(stringsAsFactors = F)
groupchat <- read.csv('Party_in_my_pants_text.csv', encoding = 'utf-8')
groupchat$date <- strptime(groupchat$date, format = '%m/%d/%y, %I:%M:%S %p') + 60*60*12
groupchat$day <- format(groupchat$date, '%Y-%m-%d')

day_agg <- ddply(groupchat, .(day), summarize, n = length(message))
day_agg$day <- as.Date(day_agg$day)
day_agg <- day_agg[-which(is.na(day_agg$day)),]
day_agg$day_month <- as.Date(format(day_agg$day, '2016-%m-%d'))
day_agg$weekday <- factor(weekdays(day_agg$day), 
                          levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
events <- read.csv('events.csv')
events$day <- as.Date(events$day, format= '%m/%d/%Y')
events <- merge(events, day_agg, by ='day', all.x=T)
events$day_month<- as.Date(format(events$day, '2016-%m-%d'))
ggplot(day_agg) + geom_line(aes(x=day_month, y=n)) + 
  facet_grid(format(day, '%Y') ~ ., scales='free') +
  geom_point(aes(x=day_month, y=n, color=weekday)) +
  scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("4 weeks")) +
  geom_label_repel(data=events, aes(x=day_month, y=ifelse(is.na(n), 100, n), label=Event), 
                   size=2, alpha=0.8, point.padding=unit(.3,'lines')) +
  scale_color_manual('Day of the Week', values=brewer.pal(7, 'Dark2')) + xlab('Date') +
  ggtitle('Party in my Pants Historical Volume')+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8), 
        plot.title = element_text(hjust=0.5), legend.position = 'bottom')
#ggsave('PIMP Timeline.jpeg', width=11, height=8, dpi=500)
subset(day_agg, n>120)
subset(groupchat, day =='2013-08-01') -> XX2

subset(groupchat, day =='2016-02-26') -> XX3

sender_agg <-  ddply(groupchat, .(sender), summarize, n = length(message))
groupchat$sender <- factor(groupchat$sender, levels = sender_agg$sender[order(-sender_agg$n)])

ggplot(subset(groupchat, sender %in% subset(sender_agg, n>100)$sender & !is.na(date))) +
  geom_bar(aes(sender, fill=format(date, '%Y'))) + 
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust=0.5), panel.background = element_blank()) +
  scale_fill_manual('Year', values=brewer.pal(6, 'Greens')) + ggtitle('PIMP texters') +
  coord_flip()
#ggsave('Whatsapp_Senders.jpeg', height=8, width=9.5, dpi=500)
#who said lol the most
table(groupchat$sender[grep('lol', tolower(groupchat$message))])

#isolate times of LKF text, then convert to a date on an arbitrary day for plotting purposes.
#retain as datetime object
LKF_times <- as.POSIXct(paste0("2017-01-01 ", format(groupchat[grep('lkf', 
                tolower(groupchat$message)),]$date, '%H:%M'), format ="%Y-%m-%d %H:%M"))
#add 24 hours to texts between midnight and 6am, so that texts from 11pm and midnight are shown adjacently
LKF_times[LKF_times < "2017-01-01 06:00:00"] <- LKF_times[LKF_times < "2017-01-01 06:00:00"]+ 24*60*60
ggplot() + geom_histogram(aes(x=LKF_times), fill='dark blue') +
  scale_x_datetime(date_label = "%H:%M %p", date_breaks='2 hours') +
  ggtitle('When do we text "LKF"?') + xlab('Time of text') +
  theme(panel.background = element_blank(), plot.title=element_text(hjust=0.5))

sexplicit <- groupchat[grep('sex', tolower(groupchat$message)),c('sender', 'message')]
sex_message <- ddply(sexplicit, .(sender), summarize, n=length(message), text=message[sample(1:n,1)])
sexplicit$sender <- factor(sexplicit$sender, levels = sex_message$sender[order(sex_message$n)])
ggplot(sexplicit) + geom_bar(aes(sender), fill='palevioletred') + 
  geom_text(data=sex_message, aes(x=sender, y=0, label=text), color='dark green', hjust=0) +
  coord_flip() + ggtitle('Sex Mentions by Sender') + ylab('Number of Texts') +
  theme(panel.background = element_blank())
ggsave('Chart1.jpeg', width=10, height=8, dpi=760)
fbomb <- groupchat[grep('fuck', tolower(groupchat$message)),c('sender', 'message')]
f_message <- ddply(fbomb, .(sender), summarize, n=length(message), text=message[sample(1:n,1)])
fbomb$sender <- factor(fbomb$sender, levels = f_message$sender[order(f_message$n)])
ggplot(fbomb) + geom_bar(aes(sender), fill='dark red') + 
  geom_text(data=f_message, aes(x=sender, y=0, label=text), color='green', size=4, hjust=0) +
  coord_flip() + theme(panel.background = element_blank()) +
  ggtitle('Fbombs by Sender') + ylab('Number of Texts')
ggsave('Chart2.jpeg', width=10, height=8, dpi=760)
x <- 'drunk'
drunk <- groupchat[grep(x, tolower(groupchat$message)),c('sender', 'message')]
d_message <- ddply(drunk, .(sender), summarize, n=length(message), text=message[sample(1:n,1)])
drunk$sender <- factor(drunk$sender, levels = d_message$sender[order(d_message$n)])
ggplot(drunk) + geom_bar(aes(sender), fill='indianred2') + 
  geom_text(data=d_message, aes(x=sender, y=0, label=text), color='dark blue', size=3, hjust=0) +
  coord_flip() + theme(panel.background = element_blank()) + ggtitle(x)
###text mining
library(tm)
library(data.table)
text_mined <- tolower(groupchat$message)
text_mined <- gsub('(?!.)[[:punct:]]', ' ', text_mined, perl=T) #this should be changed
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
text_mined = stringr::str_replace_all(text_mined, stopwords_regex, '')
#corpus <- tm_map(Corpus(VectorSource(text_mined)), stemDocument)
corpus <- Corpus(VectorSource(text_mined))
dtm = DocumentTermMatrix(corpus)
rm(text_mined)
#labeledTerms = as.data.frame(as.matrix(dtm))
#labeledTerms$sender <- groupchat$sender
words <- dtm$dimnames$Terms
word_count <- apply(dtm, 2, sum) 
singles <- which(word_count==1)
#convert raw count to frequency
average_freq <- word_count/ length(dtm$i)
#this step is super memory intensive
n_words <- 13
group_words <- as.data.frame(matrix(nrow=length(unique(groupchat$sender)), ncol=n_words+1))
names(group_words) <- c('sender', paste0('word',1:n_words))
group_words$sender <- unique(groupchat$sender)
for (i in 1:length(unique(groupchat$sender))){
  #calculate  dictionary word_count for individual sender
  sender_corp <- apply(dtm[which(groupchat$sender == unique(groupchat$sender)[i]),], 2, sum)
  #convert raw sum to frequency
  sender_corp <- sender_corp/sum(sender_corp)
  #weight user's vocab compared to average vocab. 
  #50 is abitrary but chosen because it leads to interesting results
  evaluate <- (sender_corp - average_freq)*50 * (sender_corp/average_freq)
  top_words <- sort(evaluate[-singles], decreasing = T)[1:n_words]
  #loop through user's vocab, do not save the word if it is never actually used
  for (j in 1:n_words){
    group_words[i,j+1] <- ifelse(top_words[j] > 0, names(top_words[j]), '')
  }
}

words.m <- melt(group_words[1:38,], id.var='sender')
words.m$variable <- as.numeric(gsub('word', '', words.m$variable))
words.m <- as.data.frame(words.m)
ggplot(words.m) + geom_text(aes(x=variable, y= sender, label=value)) +
  theme(panel.background = element_blank(), plot.title = element_text(color='orange', hjust=0.5)) +
  ggtitle('Most Distinctive Words by Sender')

#this will get you the speakers who have said the word mexican
groupchat$sender[dtm$i[which(dtm$j== which(words=='mexican'))]]

neil <- apply(dtm[which(groupchat$sender == 'Neil Gysel'),], 2, sum)
neil <- neil/sum(neil)
names(sort(neil - average_freq, decreasing=T))[1:10]

#
emoji_dict <- read.csv('emoji_unicode.csv')
emoji_dict$Code <- tolower(gsub('U\\+', 'U000', emoji_dict$Code))
emoji_dict <- emoji_dict[which(emoji_dict$Number != "?"),]
emoji_words <- character()
#increment a count if a certain word matches with the emoji_dict code
words.m$emoji_count <- rep(NA, nrow(words.m))
for (i in 1:nrow(words.m)){
  if (words.m$value[i] %in% emoji_dict$Code){
    emoji_words <- append(emoji_words, emoji_dict$CLDR.Short.Name[match(words.m$value[i], emoji_dict$Code)])
    words.m$emoji_count[i] <- length(emoji_words)
  }
}
setwd('ios_9_3_emoji_files/')
#connect our emoji words with actual png files that we can plot
match_png <- which(emoji_words %in% gsub('.png', '', dir()))
#convert these matches into their proper file name
imgs <- lapply(paste0(emoji_words[match_png], '.png'), png::readPNG)
#convert these matches into raster
g <- lapply(imgs, grid::rasterGrob)

words.m$sender <- factor(words.m$sender, levels=unique(words.m$sender))
words.m$text <- words.m$value
words.m$text[which(!is.na(words.m$emoji_count))] <- ''
wordplot <- ggplot(words.m) + geom_text(aes(x=variable, y= sender, label=text)) +
  theme(panel.background = element_blank(), plot.title = element_text(color='orange', hjust=0.5)) + 
  ggtitle('Most Distinctive Words by Sender') + scale_x_discrete(breaks=c(1:13))
for (k in which(!is.na(words.m$emoji_count))){
  raster <- match(words.m$emoji_count[k], match_png)
  if(!is.na(raster)){
    wordplot <- wordplot + annotation_custom(g[[raster]], 
                xmin=as.numeric(words.m$variable[k])-0.5,
                xmax=as.numeric(words.m$variable[k])+0.5,
                ymin=as.numeric(match(words.m$sender[k], unique(words.m$sender)))-0.5,
                ymax=as.numeric(match(words.m$sender[k], unique(words.m$sender)))+0.5)
  }
  
}
wordplot
subset(words.m, sender=='Clay Carol')
setdiff(emoji_words, emoji_words[match_png])
setwd('..')
ggsave('distinctive_words.jpeg', width=10.5, height=8, dpi=750)

second_names <-setdiff(names(sort(table(groupchat$sender), decreasing = T)), 
                       group_words$sender[1:38])[1:25]
words.m2 <- melt(group_words[which(group_words$sender %in% second_names),1:11], id.var='sender')
words.m2$variable <- as.numeric(gsub('word', '', words.m2$variable))
words.m2 <- as.data.frame(words.m2)
emoji_words2 <- character()
words.m2$emoji_count <- rep(NA, nrow(words.m2))
for (i in 1:nrow(words.m2)){
  if (words.m2$value[i] %in% emoji_dict$Code){
    emoji_words2 <- append(emoji_words2, emoji_dict$CLDR.Short.Name[match(words.m2$value[i], emoji_dict$Code)])
    words.m2$emoji_count[i] <- length(emoji_words2)
  }
}
setwd('ios_9_3_emoji_files/')
match_png2 <- which(emoji_words2 %in% gsub('.png', '', dir()))
#convert these matches into their proper file name
imgs2 <- lapply(paste0(emoji_words2[match_png2], '.png'), png::readPNG)
#convert these matches into raster
g2 <- lapply(imgs2, grid::rasterGrob)
#this line is important for getting the emojis to display on the correct line
words.m2$sender <- factor(words.m2$sender, levels=unique(words.m2$sender))
#transcribe the non emojis text
words.m2$text <- words.m2$value
words.m2$text[which(!is.na(words.m2$emoji_count))] <- ''
wordplot2 <- ggplot(words.m2) + geom_text(aes(x=variable, y= sender, label=text)) +
  theme(panel.background = element_blank(), plot.title = element_text(color='orange', hjust=0.5)) + 
  ggtitle('Most Distinctive Words by Sender') + scale_x_discrete(breaks=c(1:13))

for (k in which(!is.na(words.m2$emoji_count))){
  raster <- match(words.m2$emoji_count[k], match_png2)
  if(!is.na(raster)){
    wordplot2 <- wordplot2 + annotation_custom(g2[[raster]], 
                                             xmin=as.numeric(words.m2$variable[k])-0.5,
                                             xmax=as.numeric(words.m2$variable[k])+0.5,
                                             ymin=as.numeric(match(words.m2$sender[k], unique(words.m2$sender)))-0.5,
                                             ymax=as.numeric(match(words.m2$sender[k], unique(words.m2$sender)))+0.5)
  }
  
}
wordplot2
setwd('..')
ggsave('distinctive_words2.jpeg', width=11.5, height=8, dpi=750)
