update_eloRating=function(playerARating, PlayerBRating, k=1) {
  #https://gist.github.com/i000313/90b1f3c556b1b1ad8278
  
  # Expected score for player A and for player B.
  EA <- (1 / (1 + 10^((PlayerBRating - playerARating)/400)))
  EB <- (1 / (1 + 10^((playerARating - PlayerBRating)/400)))
  
  # RAn = RA + K * (SA - EA)
  newRatingPlyAWins  <- playerARating + k * (1 - EA)
  newRatingPlyADraws <- playerARating + k * (0.5 - EA)
  newRatingPlyADefeated  <- playerARating + k * (0 - EA)
  
  # RBn = RB + K * (SB - EB)
  newRatingPlyBWins  <- PlayerBRating + k * (1 - EB)
  newRatingPlyBDraws <- PlayerBRating + k * (0.5 - EB)
  newRatingPlyBDefeated  <- PlayerBRating + k * (0 - EB)
  
  chanceToWin <- round(data.frame(chanceToWin=c(EA, EB)) * 100, digits=0)
  playerAWins  <- round(data.frame(playerAWins=c(newRatingPlyAWins, newRatingPlyBDefeated)), digits=0)
  playerDraw  <- round(data.frame(draw=c(newRatingPlyADraws, newRatingPlyBDraws)), digits=0)
  playerBWins  <- round(data.frame(playerBWins=c(newRatingPlyADefeated, newRatingPlyBWins)), digits=0)
  
  df <- cbind(chanceToWin, playerAWins, playerDraw, playerBWins)
  rownames(df) <- c('playerA', 'playerB')
  return(df)
}



#######################################################################
playersToElo <- new.env(hash=TRUE)
matchesCount <- new.env(hash=TRUE)
matches <- train_data_ordered
firstDate <- min(train_data$date)

# Run computeElo for elo results in an environment indexed by player names
computeElo <- function() {
  apply(matches,1,updateMatchesCountByRow)
  apply(matches,1,computeEloByRow)
  
  return(playersToElo)
}

# Gives the highest elo ratings
summaryPlayers <- function() {
  playersToMax <- data.frame(ranking=1500,meanr=1500,medianr=1500,name="Nobody")
  for (pl in ls(playersToElo)) {
    player <- playersToElo[[pl]]
    ## player <- player[order(player$date,player$num,decreasing=TRUE),]
    ## player <- player[!duplicated(player$date),]
    ## player <- player[order(player$date,player$num,decreasing=FALSE),]
    
    newRow <- data.frame(ranking=max(player$ranking),meanr=mean(player$ranking),medianr=median(player$ranking),name=pl)
    playersToMax <- rbind(playersToMax,newRow)
  }
  
  playersToMax <- playersToMax[order(playersToMax$ranking,decreasing=TRUE),]
  return(playersToMax)
}

### Peaks

getYear <- function(year) {
  return(as.Date(paste(year,"-01-01",sep="")))
}

getYearMonth <- function(year,month) {
  return(as.Date(paste(year,"-",month,"-01-",sep="")))
}

betweenDates <- function(date1,date2) {
  playersToMax <- data.frame(ranking=1500,meanr=1500,medianr=1500,name="Nobody")
  for (pl in ls(playersToElo)) {
    player <- playersToElo[[pl]]
    ## player <- player[order(player$date,player$num,decreasing=TRUE),]
    ## player <- player[!duplicated(player$date),]
    ## player <- player[order(player$date,player$num,decreasing=FALSE),]
    player <- player[which(player$date>=date1),]
    player <- player[which(player$date <= date2),]
    newRow <- data.frame(ranking=max(player$ranking),meanr=mean(player$ranking),medianr=median(player$ranking),name=pl)
    playersToMax <- rbind(playersToMax,newRow)
  }
  playersToMax <- playersToMax[order(playersToMax$ranking,decreasing=TRUE),]
  return(playersToMax)
}


### Elo computation details

computeEloByRow <- function(row) {
  updateElo(playersToElo, row[1], row[2], row[1], row[3],row[4],row[5])
  return(0)
}

updateMatchesCountByRow <- function(row) {
  updateMatchesCount(row[1],row[2])
  return(0)
}

updateMatchesCount <- function (playerA, playerB) {
  if(is.null(matchesCount[[playerA]])) { matchesCount[[playerA]] <- 0 }
  if(is.null(matchesCount[[playerB]])) { matchesCount[[playerB]] <- 0 }
  matchesCount[[playerA]] <- matchesCount[[playerA]]+1
  matchesCount[[playerB]] <- matchesCount[[playerB]]+1
}

updateElo <- function (plToElo, playerA, playerB, winner, level, matchDate,matchNum) {
  rA <- tail(plToElo[[playerA]]$ranking,n=1)
  rB <- tail(plToElo[[playerB]]$ranking,n=1)
  
  if(is.null(rA)) {
    plToElo[[playerA]] <- data.frame(ranking=1500, date=firstDate, num=0)
    rA <- 1500
  }
  if(is.null(rB)) {
    plToElo[[playerB]] <- data.frame(ranking=1500, date=firstDate, num=0)
    rB <- 1500
  }
  
  eA <- 1 / (1 + 10 ^ ((rB - rA)/400))
  eB <- 1 / (1 + 10 ^ ((rA - rB)/400))
  
  if (winner==playerA) {
    sA <- 1
    sB <- 0
  } else {
    sA <- 0
    sB <- 1
  }
  
  kA <- 250/((matchesCount[[playerA]]+5)^0.4)
  kB <- 250/((matchesCount[[playerB]]+5)^0.4)
  k <- ifelse(level == "G", 1.1, 1)
  
  rA_new <- rA + (k*kA) * (sA-eA)
  rB_new <- rB + (k*kB) * (sB-eB)
  
  plToElo[[playerA]] <- rbind(plToElo[[playerA]],data.frame(ranking=rA_new, date=matchDate, num=matchNum))
  plToElo[[playerB]] <- rbind(plToElo[[playerB]],data.frame(ranking=rB_new, date=matchDate, num=matchNum))
}



## Some Plotting 

greaterEqualYear <- function(pl, year) {
  return(pl[pl$date>=getYear(year),])
}



