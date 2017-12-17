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


test1 <- getMonths('2016', api_key)
t2016 <- test1

test2015 <- getMonths('2015', api_key)

sum(test1$Freq)

arch_1_16 <- getArchive('2016','1',api_key)
arch_1_16_table <- getArchiveSectionName('2016','1',api_key)
arch_2_16_table <- getArchiveSectionName('2016','2',api_key)
arch_3_16_table <- getArchiveSectionName('2016','3',api_key)
arch_4_16_table <- getArchiveSectionName('2016','4',api_key)
arch_5_16_table <- getArchiveSectionName('2016','5',api_key)
arch_6_16_table <- getArchiveSectionName('2016','6',api_key)
arch_7_16_table <- getArchiveSectionName('2016','7',api_key)
arch_8_16_table <- getArchiveSectionName('2016','8',api_key)
arch_9_16_table <- getArchiveSectionName('2016','9',api_key)
arch_10_16_table <- getArchiveSectionName('2016','10',api_key)
arch_11_16_table <- getArchiveSectionName('2016','11',api_key)
arch_12_16_table <- getArchiveSectionName('2016','12',api_key)



c2016 <- rbind(arch_1_16_table,arch_2_16_table,arch_3_16_table,arch_4_16_table,arch_5_16_table,arch_6_16_table,arch_7_16_table,arch_8_16_table,
               arch_9_16_table,arch_10_16_table,arch_11_16_table,arch_12_16_table) %>%
  group_by(Var1) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(Freq))


arch_1_17 <- getArchive('2017','1',api_key)
arch_2_17 <- getArchive('2017','2',api_key)
arch_1_17_table <- getArchiveSectionName('2017','1',api_key)
arch_2_17_table <- getArchiveSectionName('2017','2',api_key)
arch_3_17_table <- getArchiveSectionName('2017','3',api_key)
arch_4_17_table <- getArchiveSectionName('2017','4',api_key)
arch_5_17_table <- getArchiveSectionName('2017','5',api_key)
arch_6_17_table <- getArchiveSectionName('2017','6',api_key)
arch_7_17_table <- getArchiveSectionName('2017','7',api_key)
arch_8_17_table <- getArchiveSectionName('2017','8',api_key)
arch_9_17_table <- getArchiveSectionName('2017','9',api_key)
arch_10_17_table <- getArchiveSectionName('2017','10',api_key)
arch_11_17_table <- getArchiveSectionName('2017','11',api_key)
arch_12_17_table <- getArchiveSectionName('2017','12',api_key)

c2017 <- rbind(arch_1_17_table,arch_3_17_table,arch_4_17_table,arch_5_17_table,arch_6_17_table,arch_7_17_table) %>%
  group_by(Var1) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(Freq))


arch_1_15_table <- getArchiveSectionName('2015','1',api_key)
arch_2_15_table <- getArchiveSectionName('2015','2',api_key)
arch_3_15_table <- getArchiveSectionName('2015','3',api_key)
arch_4_15_table <- getArchiveSectionName('2015','4',api_key)
arch_5_15_table <- getArchiveSectionName('2015','5',api_key)
arch_6_15_table <- getArchiveSectionName('2015','6',api_key)
arch_7_15_table <- getArchiveSectionName('2015','7',api_key)
arch_8_15_table <- getArchiveSectionName('2015','8',api_key)
arch_9_15_table <- getArchiveSectionName('2015','9',api_key)
arch_10_15_table <- getArchiveSectionName('2015','10',api_key)
arch_11_15_table <- getArchiveSectionName('2015','11',api_key)
arch_12_15_table <- getArchiveSectionName('2015','12',api_key)

c2015 <- rbind(arch_1_15_table,arch_2_15_table,arch_3_15_table,arch_4_15_table,arch_5_15_table,arch_6_15_table,arch_7_15_table,arch_8_15_table,
               arch_9_15_table,arch_10_15_table,arch_11_15_table,arch_12_15_table) %>%
  group_by(Var1) %>%
  summarise_all(funs(sum)) %>%
  arrange(desc(Freq))


# t1 <- as.data.frame(table(arch_1_16$subsection_name, useNA="always"))
# 
# sum(c1$Freq)
# 
# sum(arch_2_16_table$Freq)
# 
# 
# 
# sum(t1$Freq)
# 
# 
# 
# table(arch_1_16$section_name)
# 
# member_list <- GET("http://api.nytimes.com/svc/archive/v1/2016/2.json?api-key=ce83775169cb4f22bee33c9953893d7f")
# 
# str(member_list)
# 
# ml_1 <- content(member_list, 'parsed')
# 
# ml_2 <- ml_1$response
# 
# str(ml_2)
# 
# t1 <- ml_2$docs
# 
# t2 <- t1[[2]]
# 
# 
# ml_5 <- data.frame(do.call(rbind, t1), stringsAsFactors=FALSE)
# 
# t3 <- table(ml_5$section_name)
# 
# 
# x3 <- lapply(ml_5, function(x) ifelse(x=="NULL", NA, x))
# x4 <- lapply(x3, function(x) as.character((unlist(x))))
# x5 <- as.data.frame(do.call(cbind, x4))
# str(x5)
# 
# x6 <- as.data.frame(table(x5$section_name)) %>%
#           arrange(desc(Freq))
# 
# 
# sum(x6$Freq)


acreate_url <- paste("http://api.nytimes.com/svc/archive/v1/", '2017',"/",'2',".json?api-key=",api_key, sep="")

ag1 <- GET(acreate_url)

ag2 <- content(ag1, 'parsed')

ag3 <- ag2$response

ag4 <- ag3$docs

ag5 <- data.frame(do.call(rbind, ag4), stringsAsFactors=FALSE)

ag6 <- lapply(ag5, function(x) ifelse(x=="NULL", NA, x))
ag7 <- lapply(ag6, function(x) as.character((unlist(x))))
ag8 <- as.data.frame(do.call(cbind, ag7))

return(g8)