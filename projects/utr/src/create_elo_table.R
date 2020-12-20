initial_rank = 1200
all_ids <- unique(c(train_data$winnerid, train_data$loserid))
plToElo <- data.frame(player_id = all_ids, 
                      date = rep(firstDate -1, times = length(all_ids)),
                      elo_rank = rep(initial_rank, times = length(all_ids)),
                      num_matches = rep(0, length(all_ids)),
                      resultid = rep(0, length(all_ids))
                                                      )


updateElo <- function (plToElo, playerA, playerB, winner, matchDate) {
  rA <- tail(plToElo %>% 
               filter(player_id == playerA),n=1)$elo_rank
  
  rB <- tail(plToElo %>% 
               filter(player_id == playerB),n=1)$elo_rank
  
  if(length(rA)==0) {
    plToElo <- rbind(plToElo, data.frame(player_id = playerA, 
                                         date = firstDate,
                                         elo_rank = 1500,
                                         num_matches =0))
    rA <- 1500
  }
  if(length(rB)==0) {
    plToElo <- rbind(plToElo, data.frame(player_id = playerB, 
                                         date = firstDate,
                                         elo_rank = 1500,
                                         num_matches =0))
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
  
  num_matches_playerA = tail(plToElo %>% 
                               filter(player_id == playerA),n=1)$num_matches
  num_matches_playerB = tail(plToElo %>% 
                               filter(player_id == playerB),n=1)$num_matches
  
  # kA <- 250/((matchesCount[[playerA]]+5)^0.4)
  # kB <- 250/((matchesCount[[playerB]]+5)^0.4)
  kA <- 250/((num_matches_playerA+5)^0.4)
  kB <- 250/((num_matches_playerB+5)^0.4)
  k <- 1
  
  rA_new <- rA + (k*kA) * (sA-eA)
  rB_new <- rB + (k*kB) * (sB-eB)
  
  plToElo <- rbind(plToElo, data.frame(player_id = playerA, 
                                       date = matchDate,
                                       elo_rank = rA_new,
                                       num_matches = num_matches_playerA + 1))
  
  plToElo <- rbind(plToElo, data.frame(player_id = playerB, 
                                       date = matchDate,
                                       elo_rank = rB_new,
                                       num_matches = num_matches_playerB + 1))
  return(c(rA_new, rB_new))
}

computeEloByRow <- function(row) {
  updateElo(plToElo,row[1], row[2], row[1], row[3])
  return(0)
}





prematch_ra <- vector()
prematch_rb <- vector()
### Good old for loop...
for(index in 1:nrow(matches)){
  dat = matches[index,]
  
  playerA = dat$winnerid
  playerB = dat$loserid
  winner = dat$winnerid
  matchDate = dat$date
  resultid <- dat$resultid
  
  rA <- tail(plToElo %>% 
               filter(player_id == playerA),n=1)$elo_rank
  
  rB <- tail(plToElo %>% 
               filter(player_id == playerB),n=1)$elo_rank
  
  if(length(rA)==0) {
    plToElo <- rbind(plToElo, data.frame(player_id = playerA, 
                                         date = firstDate,
                                         elo_rank = 1500,
                                         num_matches =0,
                                         resultsid = 0))
    rA <- 1500
  }
  if(length(rB)==0) {
    plToElo <- rbind(plToElo, data.frame(player_id = playerB, 
                                         date = firstDate,
                                         elo_rank = 1500,
                                         num_matches =0,
                                         resultid = 0))
    rB <- 1500
  }
  
  
  prematch_ra[index] <- rA
  prematch_rb[index] <- rB
  eA <- 1 / (1 + 10 ^ ((rB - rA)/400))
  eB <- 1 / (1 + 10 ^ ((rA - rB)/400))
  
  if (winner==playerA) {
    sA <- 1
    sB <- 0
  } else {
    sA <- 0
    sB <- 1
  }
  
  num_matches_playerA = tail(plToElo %>% 
                               filter(player_id == playerA),n=1)$num_matches
  num_matches_playerB = tail(plToElo %>% 
                               filter(player_id == playerB),n=1)$num_matches
  
  # kA <- 250/((matchesCount[[playerA]]+5)^0.4)
  # kB <- 250/((matchesCount[[playerB]]+5)^0.4)
  kA <- 250/((num_matches_playerA+5)^0.4)
  kB <- 250/((num_matches_playerB+5)^0.4)
  k <- 2
  
  rA_new <- rA + (k*kA) * (sA-eA)
  rB_new <- rB + (k*kB) * (sB-eB)
  
  plToElo <- rbind(plToElo, data.frame(player_id = playerA, 
                                       date = matchDate,
                                       elo_rank = rA_new,
                                       num_matches = num_matches_playerA + 1,
                                       resultid = resultid))
  
  plToElo <- rbind(plToElo, data.frame(player_id = playerB, 
                                       date = matchDate,
                                       elo_rank = rB_new,
                                       num_matches = num_matches_playerB + 1,
                                       resultid = resultid))
  
}
