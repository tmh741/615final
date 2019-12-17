library(downloader)
library(stringr)
library(tidytext)
library(wordcloud)
library(reshape2)

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
  xlab(NULL) + ylab("Number of occurrences") +
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

sentorder <- function(textdf) {sent <- inner_join(textdf, snrc)
sent %>% group_by(sentiment) %>% summarize(n=n()) %>%
  ggplot(aes(x=reorder(sentiment,n),y=n,fill=sentiment)) + geom_bar(stat="identity") + coord_flip() + 
  ylab("Number of words") + xlab("") + theme(legend.position = NULL) + theme_bw()}

textdf <- readtext("https://www.trackingsharks.com/mans-inflatable-paddleboard-sunk-by-10ft-tiger-shark-shark-hits-kid/",
                   "https://www.trackingsharks.com/great-white-bites-kayak-in-california-leaves-2-massive-teeth/",3,3)
freqwords(textdf,5)
sentplot(textdf,4)
sentcloud(textdf)



textdf1 <- readtext("https://www.trackingsharks.com/shark-bite-snorkelers-arm-down-to-bone/",
                    "https://www.trackingsharks.com/shark-bites-surfer-near-cocoa-beach-pier/",
                    3,3)
freqwords(textdf1,3)
sentplot(textdf1,5)
sentcloud(textdf1)

textdf2 <- readtext("https://www.hawaiinewsnow.com/2019/06/15/teen-loses-leg-fingers-shark-attack/",
                    "https://www.hawaiinewsnow.com/2019/08/20/shark-attack-reported-napoopoo-beach-park-off-hawaii-island/",
                    1,2)
freqwords(textdf2,3)
sentplot(textdf2,4)
sentcloud(textdf2)

textdf3 <- readtext("https://www.hawaiinewsnow.com/story/20010243/shark-attack-survivor-recounts-frightening-ordeal/",
                    "https://www.hawaiinewsnow.com/story/20230172/beaches-closed-after-man-by-shark-on-maui/",
                    1,2)
freqwords(textdf3,3)
sentplot(textdf3,4)
sentcloud(textdf3)

snrc <- get_sentiments("nrc")
sentorder(textdf2)

