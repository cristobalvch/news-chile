#import libraries
library(ggplot2)
library(lubridate)
library(tidyr)
library(tm)
library(dplyr)
library(corrplot)
library(caret)
library(stringr)
library(purrr)
library(syuzhet)
library(tidyverse)
library(scales)

#open data
data <- read.csv('news.csv',stringsAsFactors = FALSE, fileEncoding = 'utf-8')

#select columns to use
data <- select(data,year,month,day,name_month,summary)

#create a function to clean the data and normalize the words
cleandata <- function(text,token=TRUE,stopword=TRUE){
  
  news <- gsub("[[:punct:]]","",text)
  news <- gsub("\\w*[0-9]+\\w*\\s*", "",news)
  news <- stringr::str_replace_all(news, "\\p{quotation mark}", "")
  news <- gsub("\\n", " ",news)
  news <- stringr::str_replace_all(news,"[\\s]+", " ")
  news <- stringr::str_replace_all(news," $", "")
  news <- chartr('??????','aeioun',news)
  news <- tolower(news)
  
  if(stopword){
    news <- tm::removeWords(news, 
                            iconv(tm::stopwords("spanish"), 
                                  from = "", 
                                  to = "UTF-8",
                                  sub = "byte"))
  }
  if(token){
    news <- stringr::str_split(news, " ")[[1]]
    news <- purrr::keep(.x = news,
                          .p = function(x){stringr::str_length(x) > 1})
    
    
  }
}

#clean the data with the function created above
clean_data <- data %>% 
             mutate(token_text = map(.x = summary,.f=cleandata))

#sort the data and sepate words of  news summary on a column called token
sort_data <- clean_data %>%  select(-summary) %>%  unnest()
sort_data <- sort_data %>%  rename(token=token_text)


#function to calculate the emotion of each word in token column
sentdata <- function(text,onlyresult=FALSE,summary=TRUE){
  
  if(summary){text <- as.vector(text)}
  
  emotion.df <- get_nrc_sentiment(char_v=text,language="spanish")
  
  colnames(emotion.df)[1] <- "ira"
  colnames(emotion.df)[2] <- "anticipacion"
  colnames(emotion.df)[3] <- "aversion"
  colnames(emotion.df)[4] <- "miedo"
  colnames(emotion.df)[5] <- "alegria"
  colnames(emotion.df)[6] <- "tristeza"
  colnames(emotion.df)[7] <- "sorpresa"
  colnames(emotion.df)[8] <- "confianza"
  colnames(emotion.df)[9] <- "negativo"
  colnames(emotion.df)[10] <- "positivo"
  
  if(summary){
    emotion.df3 <- data.frame(t(emotion.df))
    emotion.df3 <- data.frame(rowSums(emotion.df3))
    names(emotion.df3[1]) <- "cuenta"
    emotion.df3 <- cbind("sentimiento" = rownames(emotion.df3),emotion.df3)
    rownames(emotion.df3) <- NULL
    if(onlyresult==TRUE){emotion.df4 <- emotion.df3[,2]}
    if(onlyresult==FALSE){emotion.df4 <- emotion.df3}
  }
  if(!summary){
    emotion.df <-data.frame(texto=text,emotion.df,stringsAsFactors = FALSE)
    if(onlyresult==TRUE){emotion.df4 <- emotion.df[,-1]}
    if(onlyresult==FALSE){emotion.df4 <- emotion.df}
    
  }
  emotion.df4

}

#function to calculate the emotion of each word in token column
calcSent <- function(x){
  x2 <- x[,6:13]
  y <- x
  # x2 <- x2[rowSums(x2)>0,]
  l=1
  for (k in 1:dim(x)[1]) {
    a <- which.max(x2[k,])
    mat <- x2[k, a] == x2[k,]
    mat <- names(x2[k,])[mat]
    if(sum(x2[k,])==0){
      y[l,1:dim(x)[2]] <- x[k,]
      y[l,dim(x)[2]+1] <- ""
      l <- l + 1
    }
    if(sum(x2[k,])>0){
      for (i in 1:length(mat)) {
        y[l,1:dim(x)[2]] <- x[k,]
        y[l,dim(x)[2]+1] <- mat[i]
        l <- l + 1
      }
    }
  }
  names(y)[dim(y)[2]] <- "emocion"
  y
}

#calculate the emotions for each word
sent <- sentdata(sort_data$token,onlyresult = FALSE,summary = FALSE)

#save the positive, neutral and negative types into a new column called tipo
news_sent <- cbind(sort_data,sent[,-1],tipo= 
                     ifelse(sent$positivo>sent$negativo,"positivo",
                            ifelse(sent$positivo<sent$negativo,
                            "negativo","neutro")))

# save the other emotions into a new collum called emocion
news_sent2 <- calcSent(news_sent)


#plot the type column by year
news_sent[sent$positivo!=0|sent$negativo!=0,] %>%
        count(year,tipo) %>%
        group_by(year) %>%
        mutate(Proporcion = n / sum(n)) %>%
        ggplot() +
        aes(year,Proporcion,fill=tipo) +
        geom_col() +
        scale_y_continuous(labels=percent_format()) + 
        theme(legend.position = "top")

  

#plot the emotion column by year
news_sent2[news_sent2$emocion!="",] %>%
          count(year,emocion) %>%
          group_by(year) %>%
          mutate(Proporcion = n / sum(n)) %>%
          ggplot() +
          aes(year,Proporcion,fill=emocion) +
          geom_col() +
          scale_y_continuous(labels=percent_format()) +
          theme(legend.position = "top")


#save the dataframe of words on a new file called words.csv      
news_sent3 <- select(news_sent2,year,month,day,name_month,token,tipo,emocion)     
write.csv(news_sent3,"words.csv")




      
  