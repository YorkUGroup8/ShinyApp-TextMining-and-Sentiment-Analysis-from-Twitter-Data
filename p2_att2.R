library(rtweet)
library(lubridate) #ymd_hms
#install.packages("magrittr") # package installations are only needed the first time you use it
#detach("package:dplyr")  
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%  And glimpse



library(kableExtra)  # kable
library(knitr)
library(dplyr) # %>% and unnest_tokens
library(ggplot2)
 

#install.packages("stringr")   # Install stringr R package For Str_c
library("stringr")             # Load stringr R package
#install.packages("tibble")
library(tibble)
#install.packages("tidyverse")

library(tidyr)

library("tm")  # For DataframeSource
library(tidytext)  # For tidy

#install.packages("wordcloud")
library(wordcloud) # For wordcloud
#install.packages("wordcloud2") # For wordcloud2
library(gridExtra) # For grid.arrange

library(recommenderlab) 
library(dplyr)  # for spread


#install.packages("tidyverse") #Try replacing library(dplyr) with library(tidyverse). The spread function now lives in the tidyr package which is part of the tidyverse along with dplyr.
library(tidyverse) # for spread
library(textdata) # For afinn

library(bit64) # For Integer64
#install.packages("knitr")
#install.packages("rmarkdown")
library(rmarkdown)

#-----------------
#remove.packages('lubridate')
#install.packages('Rcpp', dependencies = TRUE)
#install.packages('lubridate', dependencies = TRUE)
library(lubridate)



#Loading the data

covid <- as_tibble(data.table::fread(str_c("d:/", "COVID.csv"), encoding= "UTF-8"))

str(covid)
glimpse(covid)

names(covid)


names(covid)[names(covid) == "Screen Name"] <- "Screen_Name"
names(covid)[names(covid) == "Tweet Language"] <- "Tweet_Language"
names(covid)[names(covid) == "Tweet Content"] <- "Tweet_Content"
names(covid)[names(covid) == "Tweet Id"] <- "doc_id"
names(covid)[names(covid) == "Tweet Type"] <- "Tweet_Type"

names(covid)[names(covid) == "Tweet Posted Time (UTC)"] <- "time"

# Exploratory Data Analysis (EDA)



# Do covid tweet in Languages other than English?



dt <- covid %>% group_by(Tweet_Language) %>% count()   # %>% rename("Tweet_Language"= "lang", n= "Number of Tweets")
names(dt)
kable(dt) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "float_left")
#Some tweets are in languages other than English. The “Undefined” ones are generally very short tweets

#Which people were retweeted?





p1 <- covid[c("Name")]

p1 <- p1 %>% filter( Name != "" ) %>% group_by (Name) %>% count()   %>% filter(n>=40) %>% arrange(desc(n)) %>% ungroup()
p1

ggplot(p1, aes(x=reorder(Name, n), y=n)) +
  geom_bar(stat="identity", fill="darkgreen") + coord_flip() +
  labs(x="", y="number of tweets retweeted ") +
  theme(legend.position = "none")


#Text mining  
#The tweet texts

kable(head(covid %>% select(Name, Tweet_Language, Tweet_Content), 20), format = "html") %>%
  kable_styling() %>%
  column_spec(1, bold = T, width = "2cm", border_right = T) %>%
  column_spec(2, bold = T, width = "2cm", border_right = T) %>%
  column_spec(3, width = "19cm")
# So text variable contains some Regex, such as \n for a new line and also \".
covid$Tweet_Content[c(1,1)]
#removing the \n as removePunctuation ,  URLs  ,the & sign + converting the tweet text into ascii to remove emoji’s


covid$Tweet_Content <- gsub('\"', " ", covid$Tweet_Content, fixed = TRUE)
covid$Tweet_Content <- gsub('#', " ", covid$Tweet_Content, fixed = TRUE)

covid$Tweet_Content <- str_replace_all(covid$Tweet_Content, "[\n]" , "") #remove new lines
covid$Tweet_Content <- str_replace_all(covid$Tweet_Content, "&amp", "") # rm ampersand

#URLs are always at the end and did not counts towards the 140 characters limit
covid$Tweet_Content <- str_replace_all(covid$Tweet_Content, "http.*" , "")

covid$Tweet_Content <- iconv(covid$Tweet_Content, "latin1", "ASCII", sub="")

#All Express News


names(covid)



#One column needs to have a unique document id (and must be named doc_id), one column must be named ‘text’,


all_Tweets <- covid %>% filter(Tweet_Type=="Tweet" )
names(all_Tweets)[names(all_Tweets) == "Tweet_Content"] <- "text"
all_Tweets$doc_id <- gsub('"', "", all_Tweets$doc_id, fixed = TRUE)

all_Tweets$doc_id <- as.integer64(as.character(all_Tweets$doc_id))

all_Tweets$date <- all_Tweets$time





all_Tweets$date <- parse_date_time(all_Tweets$date,  '%d/%b/%Y %H:%M:%S')
all_Tweets$date <- as.Date(all_Tweets$date, format = "%Y.%m.%d")




all_Corpus <- DataframeSource(all_Tweets)
all_Corpus <- VCorpus(all_Corpus)

all_Corpus
content(all_Corpus[[1]])
# remove the English stopwords

print(sort(stopwords("en")))
#
#convert all characters into lower characters (no more capitals)
#remove numbers
#remove all English stopwords.
#remove punctuation
#strip whitespaces

CleanCorpus <- function(x){
  x <- tm_map(x, content_transformer(tolower))
  x <- tm_map(x, removeNumbers) #remove numbers before removing words. Otherwise "trump2016" leaves "trump"
  x <- tm_map(x, removeWords, tidytext::stop_words$word)
  x <- tm_map(x, removePunctuation)
  x <- tm_map(x, stripWhitespace)
  return(x)
}


# Extra words removed  
RemoveNames <- function(x) {
#   x <- str_replace_all(x, "coronavirus" , "")
  x <- tm_map(x, removeWords, c("coronavirus"))
  return(x)
}


# TermDocumentMatrix, which has all (remaining) terms as rows and all Documents (tweets) as columns

CreateTermsMatrix <- function(x) {
  x <- TermDocumentMatrix(x)
  x <- as.matrix(x)
  y <- rowSums(x)
  y <- sort(y, decreasing=TRUE)
  return(y)
}

all_Corpus <- CleanCorpus(all_Corpus)
All_tweeter_Freq <- CreateTermsMatrix(all_Corpus)

content(all_Corpus[[2]])
#The words that Twitter  used the most

all_Word_used <- data.frame(word=names(All_tweeter_Freq), count=All_tweeter_Freq)

all_Word_used[1:30,] %>%
  ggplot(aes(x=(reorder(word, count)), y=count)) +
  geom_bar(stat='identity', fill="blue") + coord_flip() + theme(legend.position = "none") +
  labs(x="")


#As the names (especially ‘Coronavirus’ ) became very overwhelming in the wordcloud, 
# I removed the names (with the RemoveName function) from the corpus and created the Term Frequency Matrix again first.

all_Corpus <- RemoveNames(all_Corpus)
All_tweeter_Freq <- CreateTermsMatrix(all_Corpus)
all_Word_used <- data.frame(word=names(All_tweeter_Freq), count=All_tweeter_Freq)

all_Word_used[1:30,] %>%
  ggplot(aes(x=(reorder(word, count)), y=count)) +
  geom_bar(stat='identity', fill="blue") + coord_flip() + theme(legend.position = "none") +
  labs(x="")


# “standard” wordcloud,



set.seed(2018)
wordcloud(all_Word_used$word, all_Word_used$count, max.words = 100, scale=c(2.5,.5), random.color = TRUE, colors=brewer.pal(9,"Set1"))
#This wordcloud is interactive and I really started to actually like wordclouds

wordcloud2::wordcloud2(all_Word_used[1:100,], color = "random-light", backgroundColor = "grey", shuffle=FALSE, size=0.4)


#Comparison cloud


#Bigrams; continuing with tidytext



# 6.1 Covid  bigrams



bigram_all_Tidy <- tidy(all_Corpus)

plotBigrams <- function(tibble, topN=20, title="", color="#FF1493"){
  x <- tibble %>% select(text) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  y <- x %>% count(bigram, sort = TRUE) %>% top_n(topN, wt=n) %>%
    ggplot(aes(x=reorder(bigram, n), y=n)) +
    geom_bar(stat='identity', fill=color) + coord_flip() +
    theme(legend.position="none") + labs(x="", title=title)
}

bi_all <- plotBigrams(bigram_all_Tidy, title="Covid  bigrams", color="blue")
bi_all



#Sentiment analysis




# --------------  1        The Bing lexicon (positive/negative, binary)

get_sentiments("bing")


# Positive and negative words used most frequently

DocMeta_all_twittes <- meta(all_Corpus)  

DocMeta_all_twittes$date <- date(DocMeta_all_twittes$date)

bigram_all_Tidy$date <- DocMeta_all_twittes$date # NO WOrk




Words_covid <- bigram_all_Tidy %>% unnest_tokens(word, text)

Bing_covid <- Words_covid %>% inner_join(get_sentiments("bing"), by="word")



b1 <- Bing_covid   %>% count(word, sentiment, sort=TRUE) %>%
  group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:20) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(x="", y="number of times used", title="most used words") +
  scale_fill_manual(values = c("positive"="green", "negative"="red"))
b1

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#Time series of sentiment



time_all  <- Bing_covid %>% group_by(date) %>% count(sentiment) %>%
  spread(sentiment, n) %>% mutate(score=positive-negative) %>%
  ggplot(aes(x=date, y=score)) +
  scale_x_date(limits=c(as.Date("2019-12-01"), as.Date("2020-02-28")), date_breaks = "1 week", date_labels = "%b") +
  geom_line(stat="identity", col="blue") + geom_smooth(col="red") + labs(title="Sentiment All Twitters")



time_all


#7.2 The AFFIN lexicon (positive/negative, with scores)
get_sentiments("afinn")


Afinn_covid <- Words_covid %>% inner_join(get_sentiments("afinn"), by="word")


t1 <- Afinn_covid %>% select(id, date, word, value) %>% filter(date=="2020-02-28") 

kable(t1)






names(Afinn_covid)[names(Afinn_covid) == "value"] <- "score"
a11 <- Afinn_covid   %>% group_by(date) %>% summarise(score=sum(score))

a1 <- Afinn_covid  %>% group_by(date) %>% summarise(score=sum(score)) %>%
  ggplot(aes(x=date, y=score)) +
  scale_x_date(limits=c(as.Date("2019-12-01"), as.Date("2020-02-287")), date_breaks = "1 week", date_labels = "%b") +
  geom_line(stat="identity", col="blue") + geom_smooth(col="red") + labs(title="Sentiment All Twitter")

a1

# The nrc lexicon (2 sentiment categories, and 8 basic emotions)

get_sentiments("nrc")

Nrc_covid <- Words_covid %>% inner_join(get_sentiments("nrc"), by="word")

n1 <- Nrc_covid  %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment, y=2500)) +
  labs(x="", y="", title="Covid 19 with nrc lexicon")
n1




