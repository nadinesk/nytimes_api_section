library(httr)
library(dplyr)
library(plyr)
library(reshape2)


api_key = Sys.getenv("api_key")

getArchive <- function(year, month, key) {
  
  create_url <- paste("http://api.nytimes.com/svc/archive/v1/", year,"/",month,".json?api-key=",key, sep="")
  
  g1 <- GET(create_url)
  
  g2 <- content(g1, 'parsed')
  
  g3 <- g2$response
  
  g4 <- g3$docs
  
  g5 <- data.frame(do.call(rbind, g4), stringsAsFactors=FALSE)
  
  g6 <- lapply(g5, function(x) ifelse(x=="NULL", NA, x))
  g7 <- lapply(g6, function(x) as.character((unlist(x))))
  g8 <- as.data.frame(do.call(cbind, g7))
  
  return(g8)
  
  
}

getArchiveSectionName <- function(year, month, key) {
  
  create_url <- paste("http://api.nytimes.com/svc/archive/v1/", year,"/",month,".json?api-key=",key, sep="")
  
  g1 <- GET(create_url)
  
  g2 <- content(g1, 'parsed')
  
  g3 <- g2$response
  
  g4 <- g3$docs
  
  g5 <- data.frame(do.call(rbind, g4), stringsAsFactors=FALSE)
  
  g6 <- lapply(g5, function(x) ifelse(x=="NULL", NA, x))
  g7 <- lapply(g6, function(x) as.character((unlist(x))))
  g8 <- as.data.frame(do.call(cbind, g7))
  
  g9 <- as.data.frame(table(g8$section_name, useNA="always"))
  
  return(g9)
  
  
}

getMonths <- function(year, key) {
  df_total = NULL
  
  for (i in 1:12) {
    
    i_string <- toString(i)
    assign(paste('sub',i,sep=''), getArchiveSectionName(year,i_string,key))
    
  }
  cyear <- rbind(sub1, sub2, sub3, sub4, sub5, sub6, sub7, sub8, sub9, sub10, sub11, sub12) %>%
    group_by(Var1) %>%
    summarise_all(funs(sum)) %>%
    arrange(desc(Freq))
  
  return(cyear)
  
  
}

t16 <- getMonths('2016', api_key)
t15 <- getMonths('2015', api_key)
t14 <- getMonths('2014', api_key)
t13 <- getMonths('2013', api_key)
t12 <- getMonths('2012', api_key)
t11 <- getMonths('2011', api_key)



create_url <- paste("http://api.nytimes.com/svc/archive/v1/", '2013',"/",'1',".json?api-key=",api_key, sep="")

g1 <- GET(create_url)

g2 <- content(g1, 'parsed')

g3 <- g2$response

g4 <- g3$docs

g5 <- data.frame(do.call(rbind, g4), stringsAsFactors=FALSE)

str(g5)

g6 <- g5$section_name

g7 <- lapply(g6, function(x) ifelse(x=="NULL", NA, x))
g8 <- lapply(g7, function(x) as.character((unlist(x))))
g9 <- as.data.frame(do.call(cbind, g8))

g9$ID <- seq.int(nrow(g9))

g10 <- as.data.frame(t(g9))

table(g10$V1)

str(g10)

return(g8)

create_url <- paste("http://api.nytimes.com/svc/archive/v1/", '2013',"/",'1',".json?api-key=",api_key, sep="")

g1 <- GET(create_url)

g2 <- content(g1, 'parsed')

g3 <- g2$response

g4 <- g3$docs

g5 <- data.frame(do.call(rbind, g4), stringsAsFactors=FALSE)

g6 <- g5$section_name

g7 <- lapply(g6, function(x) ifelse(x=="NULL", NA, x))
g8 <- lapply(g7, function(x) as.character((unlist(x))))
g9 <- as.data.frame(do.call(cbind, g8))

g9$ID <- seq.int(nrow(g9))

g10 <- as.data.frame(t(g9))

g11 <- as.data.frame(table(g10$V1))

##

acreate_url <- paste("http://api.nytimes.com/svc/archive/v1/", '2013',"/",'3',".json?api-key=",api_key, sep="")

ag1 <- GET(acreate_url)

ag2 <- content(ag1, 'parsed')

ag3 <- ag2$response

ag4 <- ag3$docs

ag5 <- data.frame(do.call(rbind, ag4), stringsAsFactors=FALSE)

ag6 <- ag5$section_name

ag7 <- lapply(ag6, function(x) ifelse(x=="NULL", NA, x))
ag8 <- lapply(ag7, function(x) as.character((unlist(x))))
ag9 <- as.data.frame(do.call(cbind, ag8))

ag9$ID <- seq.int(nrow(ag9))

ag10 <- as.data.frame(t(ag9))

ag11 <- as.data.frame(table(ag10$V1))

library(tidyr)

str(ag11)
ag12 <- cSplit(ag11, 'Var1', ";") %>%
            select(Var1_1, Freq) %>%
            group_by(Var1_1) %>%
            summarise_all(funs(sum))


sum(ag12$Freq)
sum(g11$Freq)

sum(ag12$Freq)


t1 <- ag5 %>%
          filter(X_id == '51409d26cf28d02e3d00012e')

