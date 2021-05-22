library(httr)
library(jsonlite)
library(data.table)
library(plyr)
library(dplyr)
library(rjson)
#Don't know if you need all these libraries I changed a lot of stuff in the making.




apiKey = "your api key"
accId = "your account id"


#Warning! Spaghetti code ahead ...

all_matches <- function(x,y){
  noc <- x:y #Number of Champions (by championId)
  champ = paste0("champion=", noc, collapse = "&")
  
  res = GET(paste("https://euw1.api.riotgames.com/lol/match/v4/matchlists/by-account/",accId,"?",champ,"&queue=450&api_key=",apiKey, sep = ""))
  data = fromJSON(rawToChar(res$content))
  
  
  matches <- NULL
  mmm <- NULL
  for (i in 1:length(data$matches)){
    matches[i+length(mmm)] <- data$matches[[i]]$gameId
  }
  mmm = matches
  return(matches)
}
matches <- NULL
matches <- c(matches,all_matches(1,10)) 
#to get all the matches I was adding more champions(depending on your total games), be careful about the api limit!
# -----20 requests every second and 100 requests every 2 minutes!!!
#It would be something like this
#matches <- c(matches,all_matches(1,10)) this basically means - get all matches with championId = 1 to championId = 10
#matches <- c(matches,all_matches(10,15))  
#matches <- c(matches,all_matches(15,20)) etc.



#Now we have all the matches Ids
#Raw data to list
myList <- list()

for (i in 1:20){#20 requests every second and 100 requests every 2 minutes!!!
  stats <- GET(paste("https://euw1.api.riotgames.com/lol/match/v4/matches/",toString(matches[i]),"?api_key=",apiKey, sep=""))
  sdata = fromJSON(rawToChar(stats$content))
  myList[[i]] = sdata
}


#Matches
matches_db <- c(sapply(myList, function(v) v[1]), sapply(myList, function(v) v[3]), sapply(myList, function(v) v[4]), sapply(myList, function(v) v[8])) 
matches_db <- matrix(unlist(matches_db), ncol = 4)
colnames(matches_db) <- c(names(myList[[1]][1]), names(myList[[1]][3]), names(myList[[1]][4]), names(myList[[1]][8]))

#ParticipantsIds
player <- sapply(myList, function(v) v[13])
player <- unlist(unlist(player, recursive = F),recursive = F) 
player <- player[c(F, T)] 
pIds_db <- c(rep(sapply(myList, function(v) v[1]),each = 10), sapply(player, function(v) v[2]), sapply(player, function(v) v[3])) 
pIds_db <- matrix(unlist(pIds_db), ncol = 3)
pIds_db <- cbind(pIds_db[,1], rep(1:10,length(matches)), pIds_db[,2:3]) 
colnames(pIds_db) <- c("gameId", "participantId","accountId", "summonerName")

#Participants
participants <- sapply(myList, function(v) v[12])
participants <- unlist(participants, recursive = F)
participants_db <- c(rep(sapply(myList, function(v) v[1]), each = 10), sapply(participants, function(v) v[1]), sapply(participants, function(v) v[2]),
                     sapply(participants, function(v) v[3]), sapply(participants, function(v) v[4]), sapply(participants, function(v) v[5]))
participants_db <- matrix(unlist(participants_db), ncol = 6)
colnames(participants_db) <- c("gameId", "participantId", names(participants[[1]][2]), names(participants[[1]][3]),
                               names(participants[[1]][4]), names(participants[[1]][5]))

#Teams
teams <- sapply(myList, function(v) v[11])
teams <- unlist(teams, recursive = F)
teams_db <- c(rep(sapply(myList, function(v) v[1]),each = 2),sapply(teams, function(v) v[1]),sapply(teams, function(v) v[2]),
              sapply(teams, function(v) v[3]),sapply(teams, function(v) v[4]),sapply(teams, function(v) v[5]),
              sapply(teams, function(v) v[9]),sapply(teams, function(v) v[10]))
teams_db <- matrix(unlist(teams_db), ncol = 8)
colnames(teams_db) <- c("gameId", names(teams[[1]][1]), names(teams[[1]][2]), names(teams[[1]][3]), names(teams[[1]][4]),
                        names(teams[[1]][5]), names(teams[[1]][9]), names(teams[[1]][10]))

#Stats
stats <- sapply(participants, function(v) v[["stats"]])
stats <- data.table::rbindlist(stats, idcol = TRUE, fill = TRUE)
stats <- stats[,-1]
stats_db <- cbind(rep(sapply(myList, function(v) v[1]), each = 10), stats)
colnames(stats_db)[1] <- "gameId"


write.csv(as.matrix(stats_db), 'stats_db.csv')
write.csv(matches_db, 'matches_db.csv')
write.csv(participants_db, 'participants_db.csv')
write.csv(pIds_db, 'participantIdentities_db.csv')
write.csv(teams_db, 'teams_db.csv')


