library(downloader)
library(stringr)
library(tidytext)
library(wordcloud)
library(reshape2)
library(xml2)
library(rvest)

readtext <- function(url,url1,head,tail) {
  html <- read_html(url)
  webtext <- html %>% html_nodes("p") %>% html_text()
  text_df <- tibble(lines=head:(length(webtext)-tail),text=webtext[head:(length(webtext)-tail)])
  html1 <- read_html(url1)
  webtext1 <- html1 %>% html_nodes("p") %>% html_text()
  text_df1 <- tibble(lines=head:(length(webtext1)-tail),text=webtext1[head:(length(webtext1)-tail)])
  textdf <- rbind(text_df,text_df1) %>% unnest_tokens(word,text) %>% anti_join(stop_words)
  return(textdf)}

freqwords <- function(textdf, limit){
  textdf %>%
    count(word,sort=T)%>%
    filter(n>limit) %>%
    mutate(word=reorder(word,n))%>%
    ggplot(aes(word,n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()}

sentplot <- function(textdf,top){
  sentiment1<- textdf %>%
    inner_join(get_sentiments("bing")) %>%
    count(word,sentiment,sort=T) %>%
    ungroup()
  sentiment1 %>% 
    group_by(sentiment) %>% 
    top_n(top) %>% 
    ungroup() %>%
    mutate(word=reorder(word,n)) %>%
    ggplot(aes(word,n,fill=sentiment)) +
    geom_col(show.legend=F) +
    facet_wrap(~sentiment,scales="free_y") +
    labs(y="Contribution to sentiment", x=NULL) + coord_flip()}

sentcloud <- function(textdf)  {
  textdf %>% 
    inner_join(get_sentiments("bing")) %>%
    count(word,sentiment,sort=T) %>%
    acast(word ~ sentiment, value.var="n",fill=0) %>%
    comparison.cloud(colors=c("gray20","gray80"))}

sentorder <- function(textdf) {
  snrc <- get_sentiments("nrc")
  sent <- inner_join(textdf, snrc)
sent %>% group_by(sentiment) %>% summarize(n=n()) %>%
  ggplot(aes(x=reorder(sentiment,n),y=n,fill=sentiment)) + geom_bar(stat="identity") + coord_flip() + 
  ylab("Number of words") + xlab("") + theme(legend.position = "none") + theme_bw()}


