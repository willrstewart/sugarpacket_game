


agg_states <- aggregate(updated_decision_criteria[,c("correct","count")],by=list(updated_decision_criteria$state),FUN=sum)
colnames(agg_states) <- c('state','correct','count')
agg_states$win_prob <- agg_states$correct/agg_states$count





all_states <- agg_states$state
optimal_decision <- c()
test_results <- c()
for (i in 1:length(all_states)) {
  rows <- as.numeric(strsplit(all_states[i], "")[[1]])
  explore <- 0
  recorded_decisions <- c()
  current_game <- setup_game(rows)
  turn <- 1
  while(!game_complete(current_game,turn)) {
    current_game
    turn <- turn+1
    possible_decisions <- get_possible_decisions(current_game)
    decision <- make_decision(current_game,possible_decisions,updated_decision_criteria,explore)
    if(turn == 2) {
      optimal_decision <- rbind(optimal_decision,decision)
    }
    #recorded_decisions <- record_decision(possible_decisions,recorded_decisions,turn,decision)
    current_game <- update_game(current_game,decision)
  }
  test_results <- save_results(test_results,turn,i)
}
starts <- possible_decisions
test_starts <- cbind(test_results,starts)

test_states <- cbind("state"=all_states,test_results)
test_states <- cbind(test_states,optimal_decision)



