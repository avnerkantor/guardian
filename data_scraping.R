# Let start by using the official Guaridan API: https://open-platform.theguardian.com/access/
# http://discussion.theguardian.com/discussion-api/
# We will scrape datablog articles.

library(jsonlite)
library(lubridate)
library(dplyr)
library(stringr)
library(readr)

api_key<-"***"

fromDate<-seq(ymd('2012-09-01'),ymd('2016-09-01'), by = '3 month')
toDate<-seq(ymd('2012-12-01'),ymd('2016-12-01'), by = '3 month')
dates<-data.frame(fromDate=fromDate, toDate=toDate)

datablog_search<-do.call(rbind,
                         apply(dates, 2, function(x){
                           url<-paste0("https://content.guardianapis.com/search?q=datablog&page-size=200&from-date=", dates$fromDate, "&to-date=", dates$toDate, "&order-by=newest&show-fields=all&api-key=", api_key)
                           guardianArticles<-data.frame(url=url, stringsAsFactors = FALSE)
                         })
)

# interactive_tag<-do.call(rbind,
#                      apply(dates, 2, function(x){
#                        url<-paste0("https://content.guardianapis.com/search?tag=type/interactive&page-size=200&from-date=", dates$fromDate, "&to-date=", dates$toDate, "&order-by=newest&show-fields=all&api-key=", api_key)
#                        guardianArticles<-data.frame(url=url, stringsAsFactors = FALSE)
#                      })
# )
# 
datablog_tag<-do.call(rbind,
                      apply(dates, 2, function(x){
                        url<-paste0("https://content.guardianapis.com/search?tag=type/datablog&page-size=50&from-date=", dates$fromDate, "&to-date=", dates$toDate, "&order-by=newest&show-fields=all&api-key=", api_key)
                        guardianArticles<-data.frame(url=url, stringsAsFactors = FALSE)
                      })
)

news_section<-do.call(rbind,
                      apply(dates, 2, function(x){
                        url<-paste0("https://content.guardianapis.com/search?q=uk-news&page-size=200&from-date=", dates$fromDate, "&to-date=", dates$toDate, "&order-by=newest&show-fields=all&api-key=", api_key)
                        guardianArticles<-data.frame(url=url, stringsAsFactors = FALSE)
                      })
)

# After we have urls let collect some information with the same API:
guardianArticlesData <- do.call("rbind", lapply(news_section$url,  function(url) {
  # url2<-paste0("https://content.guardianapis.com", str_remove(url, "https://www.theguardian.com"), "?show-fields=all&show-tags=contributor&api-key=test")
  tryCatch({
    document <- fromJSON(url)
    
    # author=paste(document$response$content$tags$firstName, document$response$content$tags$lastName)
    webUrl=document$response$results$webUrl
    wordcount=document$response$results$fields$wordcount
    commentable=document$response$results$fields$commentable
    commentCloseDate=document$response$results$fields$commentCloseDate
    shortUrl=document$response$results$fields$shortUrl
    # print(shortUrl)
    sectionName=document$response$results$sectionName
    date=document$response$results$webPublicationDate
    
    guardianArticlesData<-data.frame(webUrl=webUrl, wordcount=wordcount, shortUrl=shortUrl, sectionName=sectionName, date=date,
                                     commentable=commentable, commentCloseDate=commentCloseDate)
    
    write.table(guardianArticlesData, file="./news_section.csv", append = T, sep=',', row.names=F, col.names=F )},
    
    error=function(err) {})
}))

data_tag <- read_csv("data_tag.csv")
news_section <- read_csv("news_section.csv")

colnames(news_section)<-c("webUrl", "wordcount", "shortUrl", "sectionName", "date", "commentable", "commentCloseDate")

g<-data_tag%>%
  unique()%>%
  filter(commentable=="TRUE")

news<-news_section%>%
  mutate(year=year(date),
         sectionName=tolower(sectionName),
         sectionName=case_when(
           #sectionName=="politics"~"news",
           sectionName=="world news"~"news",
           #sectionName=="uk news"~"news",
           #sectionName=="us news"~"news",
           TRUE~sectionName
         ))%>%
  filter(commentable=="TRUE",sectionName=="news")
  
  
news%>%group_by(year)%>%summarise(n())


# The official API doesn't give comments/replies/sharess. Fortunately for us the guardain allow ask to hacking their database by unofficial API.
# http://discussion.theguardian.com/discussion-api/
# http://discussion.theguardian.com/discussion-api/getCommentCounts?short-urls=/p/3htd7,/p/27y27

gComments<-do.call(rbind, lapply(news$shortUrl, function(url){  
  tryCatch({
    comments<-""
    sUrl<-str_extract(url, '(/p/)(.*?)([a-z, 0-9]{5})')
    b<-paste0('https://api.nextgen.guardianapps.co.uk/discussion/comment-counts.json?shortUrls=', sUrl)
    comments<-fromJSON(b)[[1]][[2]]
    guardianComments<-data.frame(comments=comments, url=url, stringsAsFactors = FALSE)
    write.table(guardianComments, file="./gNewsComments.csv", append = T, sep=',', row.names=F, col.names=F )},
    error=function(err) {})
}))


# Guardian replies
gReplies<-do.call(rbind, lapply(news$shortUrl, function(url){
  tryCatch({
    sUrl<-str_extract(url, '(/p/)(.*?)([a-z, 0-9]{5})')
    b<-paste0('https://api.nextgen.guardianapps.co.uk/discussion/', sUrl, '.json?orderBy=oldest&pageSize=100&displayThreaded=true&commentsClosed=true&maxResponses=100')
    document <- fromJSON(b)
    a<-document$commentsHtml%>%str_extract_all("(data-comment-replies=)(.*)")%>%str_extract_all("[0-9]{1}")
    replies<-sum(as.numeric(unlist(a)))
    guardianReplies<-data.frame(replies=replies, url=url, stringsAsFactors = FALSE)
    
    write.table(guardianReplies, file="./gNewsReplies.csv", append = T, sep=',', row.names=F, col.names=F )},
    error=function(err) {})
}))


# Now let combine everything 
gComments <- read_csv("gComments.csv")
colnames(gComments)<-c("comments", "shortUrl")

gReplies <- read_csv("gReplies.csv")
colnames(gReplies)<-c("replies", "shortUrl")

gArticles1<-g%>%
  left_join(gComments, by="shortUrl")%>%
  left_join(gReplies, by="shortUrl")

# comments open dates

# Adding some metadata: datablog, video, table, gallery, interactive, query type, 
gArticles2<-gArticles1%>%
  filter(commentable==TRUE, comments>0)%>%
  mutate(n_comments=comments-replies,
         replies_ratio=replies/n_comments,
         # datesDiff=commentCloseDate-date,
         datablog=ifelse(str_detect(webUrl, "/datablog/"), TRUE, FALSE),
         interactive=ifelse(str_detect(webUrl, paste(c("/ng-interactive/", "/interactive/"), collapse='|')), TRUE, FALSE)
  )

save(gArticles2, file="gArticles2.rda")
#######################

gComments <- read_csv("gNewsComments.csv")
colnames(gComments)<-c("comments", "shortUrl")

gReplies <- read_csv("gNewsReplies.csv")
colnames(gReplies)<-c("replies", "shortUrl")

gArticles3<-news%>%
  left_join(gComments, by="shortUrl")%>%
  left_join(gReplies, by="shortUrl")%>%
  unique()

# comments open dates

# Adding some metadata: datablog, video, table, gallery, interactive, query type, 
gArticles4<-gArticles3%>%
  filter(commentable==TRUE, comments>0)%>%
  mutate(n_comments=comments-replies,
         replies_ratio=replies/n_comments,
         # datesDiff=commentCloseDate-date,
         datablog=ifelse(str_detect(webUrl, "/datablog/"), TRUE, FALSE),
         interactive=ifelse(str_detect(webUrl, paste(c("/ng-interactive/", "/interactive/"), collapse='|')), TRUE, FALSE)
  )

save(gArticles4, file="gArticles4.rda")
