# -- Create Match Number table

all_ids <- unique(c(train_data$winnerid, train_data$loserid))
pl_num_matches <- data.frame(player_id = all_ids, 
                             num_matches = rep(0, length(all_ids)),
                             match_id = rep(NA, length(all_ids)))


### Good old for loop...
for(index in 1:nrow(matches)){
  dat = matches[index,]
  
  playerA = dat$winnerid
  playerB = dat$loserid
  matchDate = dat$date
  
  num_matches_playerA <- tail(pl_num_matches %>% 
               filter(player_id == playerA),n=1)$num_matches
  
  num_matches_playerB <- tail(pl_num_matches %>% 
                              filter(player_id == playerB),n=1)$num_matches
  
  if(num_matches_playerA==0) {
    pl_num_matches[pl_num_matches$player_id == playerA, 'match_id'] = dat$resultid
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
  
}
