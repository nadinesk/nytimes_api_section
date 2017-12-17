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
  
  freq_name <- paste('Freq', year, sep="-")
  
  cyear <- rbind(sub1, sub2, sub3, sub4, sub5, sub6, sub7, sub8, sub9, sub10, sub11, sub12) %>%
    group_by(Var1) %>%
    summarise_all(funs(sum)) %>%
    arrange(desc(Freq)) %>%
    rename(c("Freq" = freq_name))
  
  
  return(cyear)
  
  
}

newgetArchiveSectionName <- function(year, month, key) {
  
  create_url <- paste("http://api.nytimes.com/svc/archive/v1/", year,"/",month,".json?api-key=",key, sep="")
  
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
  
  g11 <- as.data.frame(table(g10$V1, useNA="always"))
  
  g12 <- cSplit(g11, 'Var1', ";")
  
  names(g12)[2] <- 'Var1'
  
  g13 <- g12 %>%
          select(Var1, Freq) %>%
          group_by(Var1) %>%
          summarise_all(funs(sum))
  
  return(g13)
  
}

newgetMonths <- function(year, key) {
  df_total = NULL
  
  for (i in 1:12) {
    i_string <- toString(i)
    assign(paste('sub',i,sep=''), getArchiveSectionName(year,i_string,key))
    name <- paste('sub', i, sep="")
    
  }
  
  freq_name <- paste('Freq', year, sep="-")
  
  cyear <- rbind(sub1, sub2, sub3, sub4, sub5, sub6, sub7, sub8, sub9, sub10, sub11, sub12) %>%
            group_by(Var1) %>%
            summarise_all(funs(sum)) %>%
            arrange(desc(Freq))
  
  g12 <- cSplit(cyear, 'Var1', ";")
  
  names(g12)[2] <- 'Var1'
  
  g13 <- g12 %>%
    select(Var1, Freq) %>%
    group_by(Var1) %>%
    summarise_all(funs(sum)) %>%
    rename(c("Freq" = freq_name))
  
  return(g13)
  

}


t16 <- getMonths('2016', api_key)

c17 <- newgetMonths('2017', api_key)
c16 <- newgetMonths('2016', api_key)
c15 <- newgetMonths('2015', api_key)
c14 <- newgetMonths('2014', api_key)
c13 <- newgetMonths('2013', api_key)
c12 <- newgetMonths('2012', api_key)
c11 <- newgetMonths('2011', api_key)
c10 <- newgetMonths('2010', api_key)
c09 <- newgetMonths('2009', api_key)
c08 <- newgetMonths('2008', api_key)
c07 <- newgetMonths('2007', api_key)
c06 <- newgetMonths('2006', api_key)


c11_16 <- c11 %>%
            left_join(c12, by="Var1_1") %>%
            left_join(c13, by="Var1_1") %>%
            left_join(c14, by="Var1_1") %>%
            left_join(c15, by="Var1_1") %>%
            left_join(c16, by="Var1_1")
      


sum(c16$`Freq-2016`)
sum(c15$`Freq-2015`)
sum(c14$`Freq-2014`)
sum(c13$`Freq-2013`)
sum(c12$`Freq-2012`)
sum(c11$`Freq-2011`)
sum(c10$`Freq-2010`)
sum(c09$`Freq-2009`)
sum(c08$`Freq-2008`)
sum(c07$`Freq-2007`)
sum(c06$`Freq-2006`)


n13_1 <- getArchiveSectionName('2013','1',api_key)
n13_2 <- getArchiveSectionName('2013','2',api_key)
n13_3 <- getArchiveSectionName('2013','3',api_key)
n13_4 <- getArchiveSectionName('2013','4',api_key)
n13_5 <- getArchiveSectionName('2013','5',api_key)
n13_6 <- getArchiveSectionName('2013','6',api_key)
n13_7 <- getArchiveSectionName('2013','7',api_key)
n13_8 <- getArchiveSectionName('2013','8',api_key)
n13_9 <- getArchiveSectionName('2013','9',api_key)
n13_10 <- getArchiveSectionName('2013','10',api_key)
n13_11 <- getArchiveSectionName('2013','11',api_key)
n13_12 <- getArchiveSectionName('2013','12',api_key)


cyear1 <- rbind(n13_1, n13_2, n13_3, n13_4, n13_5, n13_6, n13_7, n13_8, n13_9, n13_10, n13_11, n13_12) %>%
  group_by(Var1) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(Freq))

sum(cyear$Freq)

ng12 <- cSplit(cyear1, 'Var1', ";")

names(ng12)[2] <- 'Var1'

ng13 <- ng12 %>%
  select(Var1, Freq) %>%
  group_by(Var1) %>%
  summarise_all(funs(sum))

sum(ng13$Freq)


a13_1 <- newgetArchiveSectionName('2013','1',api_key)
a13_2 <- newgetArchiveSectionName('2013','2',api_key)
a13_3 <- newgetArchiveSectionName('2013','3',api_key)
a13_4 <- newgetArchiveSectionName('2013','4',api_key)
a13_5 <- newgetArchiveSectionName('2013','5',api_key)
a13_6 <- newgetArchiveSectionName('2013','6',api_key)
a13_7 <- newgetArchiveSectionName('2013','7',api_key)
a13_8 <- newgetArchiveSectionName('2013','8',api_key)
a13_9 <- newgetArchiveSectionName('2013','9',api_key)
a13_10 <- newgetArchiveSectionName('2013','10',api_key)
a13_11 <- newgetArchiveSectionName('2013','11',api_key)
a13_12 <- newgetArchiveSectionName('2013','12',api_key)

cyear <- rbind(a13_1, a13_2, a13_3, a13_4, a13_5, a13_6, a13_7, a13_8, a13_9, a13_10, a13_11, a13_12) %>%
  group_by(Var1) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(Freq))

sum(cyear$Freq)


g12 <- cSplit(n13_3, 'Var1', ";") %>%
  select(Var1_1, Freq) %>%
  group_by(Var1_1) %>%
  summarise_all(funs(sum))

# create_url <- paste("http://api.nytimes.com/svc/archive/v1/", '2013',"/",'1',".json?api-key=",api_key, sep="")
# 
# g1 <- GET(create_url)
# 
# g2 <- content(g1, 'parsed')
# 
# g3 <- g2$response
# 
# g4 <- g3$docs
# 
# g5 <- data.frame(do.call(rbind, g4), stringsAsFactors=FALSE)
# 
# str(g5)
# 
# g6 <- g5$section_name
# 
# g7 <- lapply(g6, function(x) ifelse(x=="NULL", NA, x))
# g8 <- lapply(g7, function(x) as.character((unlist(x))))
# g9 <- as.data.frame(do.call(cbind, g8))
# 
# g9$ID <- seq.int(nrow(g9))
# 
# g10 <- as.data.frame(t(g9))
# 
# table(g10$V1)
# 
# str(g10)
# 
# return(g8)
# 
# create_url <- paste("http://api.nytimes.com/svc/archive/v1/", '2013',"/",'1',".json?api-key=",api_key, sep="")
# 
# g1 <- GET(create_url)
# 
# g2 <- content(g1, 'parsed')
# 
# g3 <- g2$response
# 
# g4 <- g3$docs
# 
# g5 <- data.frame(do.call(rbind, g4), stringsAsFactors=FALSE)
# 
# g6 <- g5$section_name
# 
# g7 <- lapply(g6, function(x) ifelse(x=="NULL", NA, x))
# g8 <- lapply(g7, function(x) as.character((unlist(x))))
# g9 <- as.data.frame(do.call(cbind, g8))
# 
# g9$ID <- seq.int(nrow(g9))
# 
# g10 <- as.data.frame(t(g9))
# 
# g11 <- as.data.frame(table(g10$V1))

##

acreate_url <- paste("http://api.nytimes.com/svc/archive/v1/", '2013',"/",'1',".json?api-key=",api_key, sep="")

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

ag12 <- cSplit(ag11, 'Var1', ";") %>%
            select(Var1_1, Freq) %>%
            group_by(Var1_1) %>%
            summarise_all(funs(sum))


sum(ag12$Freq)
sum(g11$Freq)

sum(ag12$Freq)


t1 <- ag5 %>%
          filter(X_id == '51409d26cf28d02e3d00012e')


#######





