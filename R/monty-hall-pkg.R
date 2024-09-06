#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title Select a random door in the Monty Hall game. 
#' 
#' @description `select_door()` randomly selects one of the three doors in the Monty Hall problem. 
#' 
#' @details In the Monty Hall game, a contestant initially selects one of the three doors. This function simulates the contestant's first random selection of either goat or a car. 
#' 
#' @param ... no arguments are used by the function. 
#' 
#' @return The function returns a single integer between 1 and 3, representing the door selected by the contestant. 
#' 
#' @examples `select_door()`
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title Host opens goat door 
#' 
#' @description `open_goat_door()` simulates the host opening one of the remaining doors to reveal a goat after the contestant has made their initial selection. 
#' 
#' @details After the contestant makes an initial door selection, the host opens one of the other two doors to reveal a goat. If the contestant's initial pick is the car, the host randomly selects one of the two doors with a goat behind it. If the initial pick is a goat, the host opens the other door that also has a goat behind it. 
#' 
#' @param `game` A character vector of length 3, representing the positions of the car and the goats. The vector consists of two "goat" values and one "car" value. 
#' 
#' @param `a.pick` An integer (1, 2, or 3) representing the door initially selected by the contestant. 
#' 
#' @return The function returns an integer (1, 2, or 3) representing the door that is opened by the host to reveal a goat. 
#' 
#' @examples `open_goat_door()`
#' 
#' @export 
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title Final door selection 
#' 
#' @description `change_door()` determines the contestant's final door selection based on whether they choose to stay with their original pick or switch to the other unopened door. 
#' 
#' @details After the host opens a door to reveal a goat, the contestant has the option to either stay with their inital door selection or switch to the other unopened door. This function updates the contestant's final pick based on their decision. 
#' 
#' @param `stay` A logical value (T or F) indicating whether the contestant chooses to stay with their original door selection `TRUE` or switch to the other unopened door `FALSE`
#' 
#' @param `opened.door` An integer (1, 2, or 3) representing the door that was opened by the host to reveal a goat.
#' 
#' @param `a.pick` An integer (1, 2, or 3) representing the door initially selected by the contestant. 
#' 
#' @return The function returns an integer (1, 2, or 3) representing the contestant's final door selection. 
#' 
#' @examples `change_door()`
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title Determine the outcome of the Monty Hall game. 
#' 
#' @description `determine_winner()` checks the contestant's final door selection to determine if they have won the car or received a goat. 
#' 
#' @details After the contestant makes their final door selection, this function compares the selection to the game setup to determine whether the contestant wins (by selecting the door with the car) or loses (by selecting a door with a goat). 
#' 
#' @param `final.pick` An integer (1, 2, or 3) representing the contestant's final door selection. 
#' 
#' @param game A character vector of length 3, representing the positions of the car and the goats. The vector consists of two "goat" values and one "car" value. 
#' 
#' @return The function returns a character string: `WIN` if the contestant selected the door with the car, or `LOSE` if they selected a door with a goat. 
#' 
#' @examples `determine_winner()`
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title Simulate the Monty Hall game. 
#' @description `play_game()` simulates a single round of the Monty Hall game, including selecting an initial door, opening a door with a goat, and determining the outcome for both staying with the initial pick or switching to the other door. 
#' 
#' @details This function orchestrates a full Monty Hall game round which includes: creating a new game with a random number distribution of the car and goats, having the contestant make an initial door selection, having the host open one of the other doors to reveal a goat. Then, the function determines the outcome for both strategies: staying with the initial selection or switching to the other unopened door. It then return a data frame showing the outcomes for both strategies. 
#'  
#' @param ... No arguments are used by the function. 
#' 
#' @return A data frame with two columns (stategy and outcome)
#' 
#' @examples `play_game()`
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title Simulate multiple Monty Hall games.
#' 
#' @description play_n_games()` runs a specified number of Monty Hall game simulations and calculates the proportion of wins and losses for both staying with the initial pick and switching to the other door. 
#' 
#' @details This function simulates a given number of Monty Hall games using `play_game()`. It then collects the results of each game and combines all results into a single data frame. Afterwards, it computes and prints the proportion of wins and losses for each strategy. Finally, it returns a data frame with the combined results of all simulations. 
#' 
#' @param `n` An integer specifying the number of games to simulate. Default is 100. 
#' 
#' @return A data frame with the results of each game, including the strategy used and the outcome for that strategy. 
#' 
#' @examples `play_n_games()`
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}


