
rows <- c(3,5,7)

create_row <- function(num,longest) {
  return(c(rep(1,num),rep(0,longest-num)))
}

setup_game <- function(rows) {
  if (length(rows)==1) {return(rep(1,rows))}
  game <- c()
  rows <- rows[order(-rows)]
  for (i in 1:length(rows)) {
    game <- cbind(game,create_row(rows[i],max(rows)))
  }
  return(game)
}

get_possible_decisions <- function(current_game) {
  rows <- c()
  nums <- c()
  if(is.null(ncol(current_game))) {
    num_left <- sum(current_game)-1
    if (num_left > 0) {
      rows <- c(rows,rep(1,num_left))
      nums <- c(nums,1:num_left)
    }
  }
  else {
    for (i in 1:ncol(current_game)) {
      num_left <- length(which(current_game[,i]==1))
      rows <- c(rows,rep(i,num_left))
      nums <- c(nums,1:num_left)
    }
  }

  return(cbind(rows,nums))
}

make_decision <- function(current_game,decisions,updated_decision_criteria,explore) {
  rand <- runif(1,0,1)
  state <- updated_decision_criteria[updated_decision_criteria$state == convert_to_factor(decisions),]
  if (!is.null(updated_decision_criteria) & rand >= explore) {
    if (nrow(state) > 0) {
      choice <- state[which.max(state$winning_prob),'decision']
      choice <- c("rows"=as.integer(strsplit(choice,",")[[1]][1]),
                  "nums"=as.integer(strsplit(choice,",")[[1]][2]))
      return(choice)
    }
  }
  choice <- decisions[sample(1:nrow(decisions), 1),]
  return(choice)
}

record_decision <- function(possible_decisions,recorded_decisions,turn,decision) {
  new_decision_state <- cbind(possible_decisions,
                              "choice" = ifelse(possible_decisions[,1] == decision[1] & 
                                                  possible_decisions[,2] == decision[2], 1, 0)
  )
  new_decision_state <- cbind(new_decision_state,"player"=rep(turn%%2+1,nrow(new_decision_state)))
  new_decision_state <- cbind(new_decision_state,"turn"=rep(turn,nrow(new_decision_state)))
  return(rbind(recorded_decisions,new_decision_state))
}

update_game <- function(current_game, decision) {
  if (is.null(ncol(current_game))) {
    num <- max(which(current_game == 1))
    current_game[num:(num-decision[2]+1)] <- 0
    return(current_game)
  }
  num <- max(which(current_game[,decision[1]]==1))
  current_game[num:(num-decision[2]+1),decision[1]] <- 0
  current_game <- current_game[, order(colSums(-current_game))]
  if (is.null(nrow(current_game))) {
    return(current_game[current_game>0])
  }
  return(current_game[, colSums(current_game != 0) > 0])
}

game_complete <- function(current_game,turn) {
  if (sum(current_game) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

save_results <- function(results,turn,i){
  return(rbind(results,data.frame("game"=i,"winner"=turn%%2+1)))
}

value_recorded_decisions <- function(recorded_decisions,turn) {
  valued_decisions <- recorded_decisions[which(recorded_decisions[,3] == 1 & 
                                                 recorded_decisions[,4] == turn%%2+1),]
  valued_decisions <- cbind(valued_decisions,"values"=1)
  all_decisions <- merge(recorded_decisions,valued_decisions,all.x=TRUE)
  all_decisions[is.na(all_decisions)] <- 0
  return(all_decisions)
}

convert_to_factor <- function(possible_decisions) {
  if (length(unique(possible_decisions[,1])) == 1) {
    return(paste(max(possible_decisions[,2])+1,collapse=""))
  }
  else {
    return(paste(aggregate(possible_decisions[,2],by=list(possible_decisions[,1]),FUN=max)[[2]], 
                 collapse = "")
    )
  }
}

update_decision_criteria <- function(updated_decision_criteria,valued_decisions) {
  for (t in unique(valued_decisions[,5])) {
    temp <- valued_decisions[which(valued_decisions[,5] == t),]
    state <- convert_to_factor(temp[,c(1,2)])
    decision <- paste(temp[which(temp[,3] == 1),c(1,2)],collapse = ",")
    correct <- temp[which(temp[,3] == 1),6]
    count <- 1
    updated_decision_criteria <- rbind(updated_decision_criteria,
                                       data.frame("state"=state,"decision"=decision,"correct"=correct,"count"=count,"winning_prob"=0,
                                                  stringsAsFactors = FALSE)
    )
  }
  updated_decision_criteria <- aggregate(updated_decision_criteria[,c('correct','count')],updated_decision_criteria[,c('state','decision')],FUN=sum)
  colnames(updated_decision_criteria) <- c("state","decision","correct","count")
  updated_decision_criteria$winning_prob <- round(updated_decision_criteria$correct/updated_decision_criteria$count,2)
  return(updated_decision_criteria)
}

results <- c()
updated_decision_criteria <- c()
total_games <- 10000
print(plot(c(0,1)~c(0,total_games/100),xlab="iteration",ylab="win percent"))
for (i in 1:total_games) {
  explore <- 1 - i/total_games
  recorded_decisions <- c()
  current_game <- setup_game(rows)
  turn <- 1
  while(!game_complete(current_game,turn)) {
    turn <- turn+1
    possible_decisions <- get_possible_decisions(current_game)
    decision <- make_decision(current_game,possible_decisions,updated_decision_criteria,explore)
    recorded_decisions <- record_decision(possible_decisions,recorded_decisions,turn,decision)
    current_game <- update_game(current_game,decision)
  }
  results <- save_results(results,turn,i)
  valued_decisions <- value_recorded_decisions(recorded_decisions,turn)
  updated_decision_criteria <- update_decision_criteria(updated_decision_criteria,valued_decisions)
  if (i %% 100 == 0) {
    print(points(i/100,length(which(results[(i-99):i,2]==1))/100))
  }
  print(paste("Winner: Player",results[i,2]))
}


# write.csv(updated_decision_criteria,"Documents/other/decision_criteria.csv",row.names = FALSE)


