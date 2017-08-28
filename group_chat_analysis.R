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
                   alpha=0.8, point.padding=unit(.2,'lines')) +
  scale_color_manual('Day of the Week', values=brewer.pal(7, 'Dark2')) +
  ggtitle('Party in my Pants Historical Volume')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8), 
        plot.title = element_text(hjust=0.5), legend.position = 'bottom')
  

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
#ggsave('Whatsapp_Senders.jpeg', height=8, width=9, dpi=500)
#who said lol the most
table(groupchat$sender[grep('lol', tolower(groupchat$message))])
table(groupchat$sender[grep('cum', tolower(groupchat$message))])
table(groupchat$sender[grep('sex', tolower(groupchat$message))])

sexplicit <- groupchat[grep('sex', tolower(groupchat$message)),c('sender', 'message')]
sex_message <- ddply(sexplicit, .(sender), summarize, n=length(message), text=message[sample(1:n,1)])
sexplicit$sender <- factor(sexplicit$sender, levels = sex_message$sender[order(sex_message$n)])
ggplot(sexplicit) + geom_bar(aes(sender)) + 
  geom_text(data=sex_message, aes(x=sender, y=0, label=text), color='red', hjust=0) +
  coord_flip() + ggtitle('Sex Mentions by Sender') + ylab('Number of Texts')
#ggsave('Chart1.jpeg', width=10, height=8, dpi=760)
fbomb <- groupchat[grep('fuck', tolower(groupchat$message)),c('sender', 'message')]
f_message <- ddply(fbomb, .(sender), summarize, n=length(message), text=message[sample(1:n,1)])
fbomb$sender <- factor(fbomb$sender, levels = f_message$sender[order(f_message$n)])
ggplot(fbomb) + geom_bar(aes(sender), fill='dark red') + 
  geom_text(data=f_message, aes(x=sender, y=0, label=text), color='green', size=4, hjust=0) +
  coord_flip() + theme(panel.background = element_blank()) +
  ggtitle('Fbombs by Sender') + ylab('Number of Texts')
ggsave('Chart2.jpeg', width=10, height=8, dpi=760)
x <- 'china'
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
average_freq <- word_count/ length(dtm$i)
#this step is super memory intensive
#sender_terms <- aggregate(.~sender, labeledTerms, sum)
n_words <- 13
group_words <- as.data.frame(matrix(nrow=length(unique(groupchat$sender)), ncol=n_words+1))
names(group_words) <- c('sender', paste0('word',1:n_words))
group_words$sender <- unique(groupchat$sender)
for (i in 1:length(unique(groupchat$sender))){
  sender_corp <- apply(dtm[which(groupchat$sender == unique(groupchat$sender)[i]),], 2, sum)
  sender_corp <- sender_corp/sum(sender_corp)
  evaluate <- (sender_corp - average_freq)*50 * (sender_corp/average_freq)
  top_words <- sort(evaluate[-singles], decreasing = T)[1:n_words]
  for (j in 1:n_words){
    group_words[i,j+1] <- ifelse(top_words[j] > 0, names(top_words[j]), '')
  }
}

words.m <- melt(group_words[1:38,1:11], id.var='sender')
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
emoji_words <- character()
words.m$emoji_count <- rep(NA, nrow(words.m))
for (i in 1:nrow(words.m)){
  if (words.m$value[i] %in% emoji_dict$Code){
    emoji_words <- append(emoji_words, emoji_dict$CLDR.Short.Name[match(words.m$value[i], emoji_dict$Code)])
    words.m$emoji_count[i] <- length(emoji_words)
  }
}
setwd('ios_9_3_emoji_files/')
match_png <- which(emoji_words %in% gsub('.png', '', dir()))
imgs <- lapply(paste0(emoji_words[match_png], '.png'), png::readPNG)
g <- lapply(imgs, grid::rasterGrob)
ggplot(words.m) + geom_text(aes(x=variable, y= sender, label=value)) +
  theme(panel.background = element_blank(), plot.title = element_text(color='orange', hjust=0.5)) +
  annotation_custom(g[[emoji_count]], xmin=as.numeric(variable)-0.5, xmax=as.numeric(variable)+0.5,
                    ymin=as.numeric(sender)-0.5, ymax=as.numeric(sender)+0.5) +
  ggtitle('Most Distinctive Words by Sender')
