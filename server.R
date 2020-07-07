#--- SERVER ---#
library(rtweet)
library(tidyr)
library(tidytext)
library(magrittr)
library(purrr)
library(qdapRegex)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(stopwords)
library(wordcloud2)
library(wordcloud)
library(radarchart)
library(shinycssloaders)
library(tm)
library(data.table)
library(memoise)
library(RColorBrewer)
library(proxy)
library(lubridate)
library(dplyr)
library(kableExtra)
library(knitr)
library(ggplot2)
library(stringr)
library(tibble)
library(gridExtra)
library(tidyverse)
library(textdata)
library(bit64)


tweets_Data <- read.csv("COVIDClean.csv", sep= ",", stringsAsFactors = FALSE)

shinyServer <- function(input, output, session) 
{
  #- WORD CLOUD
    # a reactive expression for the document term matrix
    word_cloud <- reactive({
    # change when "update" button is click  
    input$update
    
    isolate({
      
      withProgress({
        setProgress(message = "Processing Corpus take time, Please wait for latest [06] mintues....")
        
        
        # convert into dataframe and VCorpus
        texttweets <- DataframeSource(tweets_Data)
        texttweetsCorpus <- VCorpus(texttweets)
      
        CleanCorpus <- function(x){
          x <- tm_map(x, content_transformer(tolower))
          x <- tm_map(x, removeNumbers) #remove numbers before removing words. Otherwise "trump2016" leaves "trump"
          myStopwords <- stopwords("english")
          x <- tm_map(x, removeWords, myStopwords)
          x <- tm_map(x, removePunctuation)
          x <- tm_map(x, stripWhitespace)
          return(x)
        }
        # Extra words removed  
        RemoveNames <- function(x) {
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
        
        # start cleaning data and build term matrix
        texttweetsCorpus <- CleanCorpus(texttweetsCorpus)
        texttweetsCorpus <- RemoveNames(texttweetsCorpus)
        texttweets_Freq <- CreateTermsMatrix(texttweetsCorpus)
        data.frame(word=names(texttweets_Freq), count=texttweets_Freq)  
        
        #getTermMatrix(tweets_Data)
      
        })
      })
    })
  
# make wordcloud predicable during the a session
wordcloud_rep <- repeatable(wordcloud)    
wordcloud2_rep <- repeatable(wordcloud2)

output$plotwcloud <- renderPlot({
    withProgress({
      setProgress(message = "Creating Word Cloud within short time.....")
      d <- word_cloud()
      
      wc_color = brewer.pal(8,"Set2")
      
      if (input$color == "Accent"){
        wc_color = brewer.pal(8,"Accent")
      }
      else{
        wc_color = brewer.pal(8,"Dark2")
      }
      
      set.seed(2018)
      wordcloud_rep(words=d$word, d$count, min.freq = input$wordfreq, max.words = input$maxword,
                    scale=c(4,0.5), colors = wc_color, vfont=c("sans serif","plain"),
                    random.order = input$random, rot.per = .30)
    })
  })

output$plotwcloud2 <- renderWordcloud2({
  withProgress({
    setProgress(message = "Creating Word Cloud2 within short time.....")
    d <- word_cloud()
    
    #This wordcloud is interactive and I really started to actually like wordclouds
    wordcloud2_rep(d[1:input$maxword,], minSize = input$wordfreq, color = "random-light",
                           backgroundColor = "grey", shuffle=FALSE, size=0.4)
  })
})


output$Bing_lexicon <- renderPlot({
  withProgress({
    setProgress(message = "Creating Bing_Lexicon graph within short time.....")
  get_sentiments("bing")
  texttweets <- DataframeSource(tweets_Data)
  texttweetsCorpus <- VCorpus(texttweets)
  
  CleanCorpus <- function(x){
    x <- tm_map(x, content_transformer(tolower))
    x <- tm_map(x, removeNumbers) #remove numbers before removing words. Otherwise "trump2016" leaves "trump"
    myStopwords <- stopwords("english")
    x <- tm_map(x, removeWords, myStopwords)
    x <- tm_map(x, removePunctuation)
    x <- tm_map(x, stripWhitespace)
    return(x)
  }
  # Extra words removed  
  RemoveNames <- function(x) {
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
  
  # start cleaning data and build term matrix
  texttweetsCorpus <- CleanCorpus(texttweetsCorpus)
  texttweetsCorpus <- RemoveNames(texttweetsCorpus)
  
  DocMeta_all_twittes <- meta(texttweetsCorpus)  
  DocMeta_all_twittes$date <- date(DocMeta_all_twittes$date)
  
  bigram_all_Tidy <- tidy(texttweetsCorpus)
  
  bigram_all_Tidy$date <- DocMeta_all_twittes$date
  
  Words_covid <- bigram_all_Tidy %>% unnest_tokens(word, text)
  Bing_covid <- Words_covid %>% inner_join(get_sentiments("bing"), by="word")
  
  
  Bing_covid   %>% count(word, sentiment, sort=TRUE) %>%
    group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:20) %>%
    ggplot(aes(x=reorder(word, n), y=n)) +
    geom_col(aes(fill=sentiment), show.legend=FALSE) +
    coord_flip() +
    facet_wrap(~sentiment, scales="free_y") +
    labs(x="", y="number of times used", title="The Bing Lexicon (most used words") +
    scale_fill_manual(values = c("positive"="green", "negative"="red"))
  })
})

output$nrc_lexicon <- renderPlot({
  withProgress({
    setProgress(message = "Creating NRC_Lexicon graph within short time.....")
  get_sentiments("nrc")
  texttweets <- DataframeSource(tweets_Data)
  texttweetsCorpus <- VCorpus(texttweets)
  
  CleanCorpus <- function(x){
    x <- tm_map(x, content_transformer(tolower))
    x <- tm_map(x, removeNumbers) #remove numbers before removing words. Otherwise "trump2016" leaves "trump"
    myStopwords <- stopwords("english")
    x <- tm_map(x, removeWords, myStopwords)
    x <- tm_map(x, removePunctuation)
    x <- tm_map(x, stripWhitespace)
    return(x)
  }
  # Extra words removed  
  RemoveNames <- function(x) {
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
  
  # start cleaning data and build term matrix
  texttweetsCorpus <- CleanCorpus(texttweetsCorpus)
  texttweetsCorpus <- RemoveNames(texttweetsCorpus)
  
  DocMeta_all_twittes <- meta(texttweetsCorpus)  
  DocMeta_all_twittes$date <- date(DocMeta_all_twittes$date)
  
  bigram_all_Tidy <- tidy(texttweetsCorpus)
  
  bigram_all_Tidy$date <- DocMeta_all_twittes$date
  
  Words_covid <- bigram_all_Tidy %>% unnest_tokens(word, text)
  
  Nrc_covid <- Words_covid %>% inner_join(get_sentiments("nrc"), by="word")
  
  Nrc_covid  %>% count(sentiment) %>%
    ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
    geom_bar(stat="identity") + coord_polar() +
    theme(legend.position = "left", axis.text.x = element_blank()) +
    geom_text(aes(label=n, y=n + 1000)) +
    labs(x="", y="", title="Covid 19 with nrc lexicon")
  })
})

output$nrc_lexicon_scores <- renderPlot({
  withProgress({
    setProgress(message = "Creating NRC_Lexicon_Scores graph within short time.....")
  get_sentiments("nrc")
  texttweets <- DataframeSource(tweets_Data)
  texttweetsCorpus <- VCorpus(texttweets)
  
  CleanCorpus <- function(x){
    x <- tm_map(x, content_transformer(tolower))
    x <- tm_map(x, removeNumbers) #remove numbers before removing words. Otherwise "trump2016" leaves "trump"
    myStopwords <- stopwords("english")
    x <- tm_map(x, removeWords, myStopwords)
    x <- tm_map(x, removePunctuation)
    x <- tm_map(x, stripWhitespace)
    return(x)
  }
  # Extra words removed  
  RemoveNames <- function(x) {
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
  
  # start cleaning data and build term matrix
  texttweetsCorpus <- CleanCorpus(texttweetsCorpus)
  texttweetsCorpus <- RemoveNames(texttweetsCorpus)
  
  DocMeta_all_twittes <- meta(texttweetsCorpus)  
  DocMeta_all_twittes$date <- date(DocMeta_all_twittes$date)
  
  bigram_all_Tidy <- tidy(texttweetsCorpus)
  
  bigram_all_Tidy$date <- DocMeta_all_twittes$date
  
  Words_covid <- bigram_all_Tidy %>% unnest_tokens(word, text)
  
  Nrc_covid <- Words_covid %>% inner_join(get_sentiments("nrc"), by="word")
  
  sentimentscores <- Nrc_covid  %>% count(sentiment)
  
  ggplot(data=sentimentscores,aes(x=sentiment,y=n))+
    geom_bar(aes(fill=sentiment),stat = "identity")+
    theme(legend.position="none")+
    xlab("Sentiments")+ylab("Scores")+
    ggtitle("Total sentiment based on scores")+
    theme_minimal()
  })
})

}

