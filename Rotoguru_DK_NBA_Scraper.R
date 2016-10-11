library(XML)
library(RCurl)
library(plyr)
library(stringr)

#BBall Ref Scraper--------------------
# create the days
x <- seq(as.Date("2015-10-27"), as.Date("2016-01-31"), by = "day")

twentyfifteen <- x[ -which(x %in% as.Date(c("2015-11-26","2015-12-24")))]
twentyfourteen <- x[-which(x %in% as.Date(c("2014-11-27","2014-12-24","2015-02-13","2015-02-14","2015-02-15","2015-02-16","2015-02-17","2015-02-18")))]

x <- twentyfifteen

# create a url template for sprintf()
utmp <- "http://www.basketball-reference.com/friv/dailyleaders.cgi?month=%d&day=%d&year=%d"
# convert to numeric matrix after splitting for year, month, day
m <- do.call(rbind, lapply(strsplit(as.character(x), "-"), type.convert))
# create the list to hold the tables
tables <- vector("list", length(m))
# get the tables
for(i in seq_len(nrow(m))) {
  # create the url for the day and if it exists, read it - if not, NULL
  tables[[i]] <- if(url.exists(u <- sprintf(utmp, m[i, 2], m[i, 3], m[i, 1])))
    readHTMLTable(u, stringsAsFactors = FALSE)
  else NULL
  Sys.sleep(3)
}

# create the list to hold the dates
dates <- vector("list", length(tables))

# get the dates
for(i in seq_len(nrow(m))) {
  dates[[i]] <- if(url.exists(u <- sprintf(utmp, m[i,2],m[i,3],m[i,1])))
    x[i]
  else NULL
}

#put the data together
data <- do.call(rbind,Map(data.frame,A=tables, B=dates))

data <- data[,c(2:25,27)]
colnames(data) <- c("Player","Team","Away","Opponent","Win/Loss","MP","FG","FGA","FG%","3P","3PA","3P%","FT","FTA","FT%","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","Date")

#Clean Data
data <- data[!data$Player %in% c("Player"),]
cols <- c(7:24)
data[,cols] <- apply(data[,cols], 2, function(x) as.numeric(as.character(x)))

write.csv(data,"BBall Ref 2015-16 Game Log.csv")


#FDPs
data$FDPs <- apply(data,1,function(x) data$PTS+(data$TRB*1.2)+(data$AST*1.5)+(data$STL*2)+(data$BLK*2)-data$TOV)

write.csv(data, "NBA15-16.csv")

#RotoGuru Scraper----------------------
library(XML)
library(RCurl)
library(plyr)
library(stringr)

#Scraper (if RotoGuru doesn't have data, tables will be NULL, date will Work)
# create the days
x <- seq(as.Date("2014-10-28"), as.Date("2015-04-15"), by = "day")
twentyfifteen <- x[ -which(x %in% as.Date(c("2015-11-26","2015-12-24")))]
twentyfourteen <- x[-which(x %in% as.Date(c("2014-11-27","2014-12-24","2015-02-13","2015-02-14","2015-02-15","2015-02-16","2015-02-17","2015-02-18")))]

x <- twentyfourteen
# create a url template for sprintf()
utmp <- "http://rotoguru1.com/cgi-bin/hyday.pl?game=dk&mon=%d&day=%d&year=%d"

# convert to numeric matrix after splitting for year, month, day
m <- do.call(rbind, lapply(strsplit(as.character(x), "-"), type.convert))
# create the list to hold the tables
tables <- vector("list", length(m))
# get the tables
for(i in seq_len(nrow(m))) {
  # create the url for the day and if it exists, read it - if not, NULL
  tables[[i]] <- if(url.exists(u <- sprintf(utmp, m[i, 2], m[i, 3], m[i, 1])))
    readHTMLTable(u, stringsAsFactors = FALSE,which=8,as.data.frame=TRUE)
  else NULL
  Sys.sleep(3)
}

# create the list to hold the dates
dates <- vector("list", length(tables))

# get the dates
for(i in seq_len(nrow(m))) {
  dates[[i]] <- if(url.exists(u <- sprintf(utmp, m[i,2],m[i,3],m[i,1])))
    x[i]
  else NULL
}

#put the data together

data <- do.call(rbind,Map(data.frame,A=tables, B=dates))
#data <- rbind(data,data2,data3,data4)

#Clean data
data <- data[!data$A.V1 %in% c("Unlisted","Guards","Centers","Forwards"),]
colnames(data) <- c("Position", "Player", "Points","Salary","Team","Opp","Score","Minutes","Stats","Date")
data <- data[complete.cases(data),]
data$Started <- ifelse(grepl("\\^",data$Player),"Yes","No")
data$Player <- gsub("\\^","",data$Player)
data$Score <- gsub("Â","",data$Score)
data$Stats <- gsub("Â","",data$Stats)
data$Points <- round(as.numeric(data$Points),1)
data$Home.Away <- ifelse(grepl("v ",data$Opp),"Home","Away")
data$Opp <- gsub("v ","",data$Opp)
data$Opp <- gsub("@ ","",data$Opp)
data$TeamScore <- as.numeric(sapply(strsplit(data$Score,"\\-"),'[[',1))
data$OppScore <- as.numeric(sapply(strsplit(data$Score,"\\-"),'[[',2))
data$Win.Loss <- ifelse(data$TeamScore > data$OppScore,"Win","Loss")
data <- data[,-which(names(data) %in% c("Score"))]
data$Position[data$Position=="NA"] <- NA
data$Salary[data$Salary=="N/A"] <- NA
data <- data[complete.cases(data),]
data$Salary <- gsub("\\$","",data$Salary)
data$Salary <- gsub("\\,","",data$Salary)
data$Salary <- as.numeric(data$Salary)
data$Minutes <- as.numeric(sapply(strsplit(data$Minutes,"\\:"),'[[',1))
data$Minutes <- as.numeric(data$Minutes)
data$Team <- as.factor(data$Team)
data$Opp <- as.factor(data$Opp)
data$Started <- as.factor(data$Started)
data$Home.Away <- as.factor(data$Home.Away)
data$Win.Loss <- as.factor(data$Win.Loss)
data$Position <- as.factor(data$Position)
data <- data[complete.cases(data),]

#str(data)
#split Name into two columns
splits <- str_split_fixed(data$Player, ", ", 2)
#now merge these two columns the other way round
data$Player <- paste(splits[,2], splits[,1], sep = ' ')

#SportsDatabase Scraper-----------
# create the days
x <- seq(as.Date("2016-01-31"), as.Date("2016-02-10"), by = "day")

twentyfifteen <- x[ -which(x %in% as.Date(c("2015-11-26","2015-12-24")))]
twentyfourteen <- x[-which(x %in% as.Date(c("2014-11-27","2014-12-24","2015-02-13","2015-02-14","2015-02-15","2015-02-16","2015-02-17","2015-02-18")))]

x <- twentyfifteen
y <- x
x <- gsub("\\-","",x)
x <- paste("%3D",x,sep="")

# create a url template for sprintf()
utmp <- paste("http://sportsdatabase.com/nba/query?sdql=date")
m <- data.frame(x)
# create the list to hold the tables
tables <- vector("list", length(seq_len(nrow(m))))
# get the tables
for(i in seq_len(nrow(m))) {
  # create the url for the day and if it exists, read it - if not, NULL
  tables[[i]] <- readHTMLTable(u <- paste(utmp, m[i,1],sep=""),which=3,as.data.frame=T,stringsAsFactors=F)
  Sys.sleep(3)
}

# create the list to hold the dates
dates <- vector("list", length(tables))

# get the dates
for(i in seq_len(nrow(m))) {
  dates[[i]] <- y[i]
}

#put the data together

data <- do.call(rbind,Map(data.frame,A=tables, B=dates))

#Clean data
data <- data[!data$A.V1 %in% c("Date"),]
data <- data[complete.cases(data),]
colnames(data) <- c("Day Total","Link","Day","Season","Team","Opp","Site","Final","Rest","Line","Total","SUm","ATSm","OUm","DPS","DPA","SUr","ATSr","OUr","ot","Date")
data$TeamScore <- as.numeric(sapply(strsplit(data$Final,"\\-"),'[[',1))
data$OppScore <- as.numeric(sapply(strsplit(data$Final,"\\-"),'[[',2))
data$Win.Loss <- ifelse(data$TeamScore > data$OppScore,"Win","Loss")
data$Team <- as.factor(data$Team)
data$Opp <- as.factor(data$Opp)
data$Site <- as.factor(data$Site)
data$Win.Loss <- as.factor(data$Win.Loss)
