
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('MontyHallSimulation.R')

reset <- function(){
  saveRDS(c(0,0,0), "lastButton.rds")
  saveRDS("started", "state.rds")
  saveRDS(sim.choose.door(1), "prize.rds")
  saveRDS(0, "won.rds")
}

reset()

wonGame <- function(door, prize.door){
  state <- readRDS('state.rds')
  prize <- readRDS('prize.rds')
  
  if(state == 'chose.sec.door'){
    saveRDS("ended", "state.rds")
    return(ifelse(door == prize.door, 1, 0))
  }
  else {
    return(0)
  }
}

openDoor <- function(door, prize.door){
  if(door != 0){
    if(door == prize.door){
      open <- ifelse(runif(1) <= 1/2, as.numeric(prize.door)+1, as.numeric(prize.door)+2)
      open <- ifelse(open > 3, open-3, open)
    }
    else {
      open <- setdiff(c(1,2,3), c(door, prize.door))
    }
  }
  else {
    return(0)
  }
}

choose_door <- function(door1, door2, door3){
  last <- readRDS("lastButton.rds")
  last.door1 <- as.numeric(last[1])
  last.door2 <- as.numeric(last[2])
  last.door3 <- as.numeric(last[3])
  
  saveRDS(c(door1(), door2(), door3()), "lastButton.rds")
  
  state <- readRDS('state.rds')
  if(state == 'started' & (door1() > 0 | door2() > 0 | door3() > 0)){
    saveRDS("chose.first.door", "state.rds")
  }
  if(state == 'chose.first.door'){
    saveRDS("chose.sec.door", "state.rds")
  }
  
  if(door1() == last.door1 + 1){
    cat("Escolhida 1\n")
    return(1)
  }
  else if(door2() == last.door2 + 1){
    cat("Escolhida 2\n")
    return(2)
  }
  else if(door3() == last.door3 + 1){
    cat("Escolhida 3\n")
    return(3)
  }
  else {
    cat("Escolhida nenhuma\n")
    return(0)
  }
}

getPrizeDoor <- function(){
  state <- readRDS('state.rds')
  prize <- readRDS('prize.rds')
  
  cat("Prize is on door ", prize, "\n")
  
  if(state != 'ended'){
    return(prize)
  }
  else {
    new_door <- sim.choose.door(1)
    saveRDS("started", "state.rds")
    saveRDS(new_door, "prize.rds")
    return(new_door)
  }
}


getHostMsg <- function(door, prize.door, open.door, won){
  state <- readRDS('state.rds')
  if(state == "started"){
    msg <- "> Welcome! Choose your first door to start playing!"
  }
  else if(state == "chose.first.door"){
    other.door <- setdiff(c(1,2,3), c(door, open.door))
    msg1 <- paste0("> You chose Door ", door, "!<br/>")
    msg2 <- paste0("> My turn..<br/>> Now I opened Door ", open.door, " and it does NOT contain the prize!<br/>")
    msg3 <- paste0("> Would you like to <span style='color:green'>stay with Door ", door, "</span> or <span style='color:red'>switch to Door ", other.door, "</span> ?<br/>")
    msg4 <- paste0("> Observe the probabilities below to think about it..")
    
    msg <- paste(msg1, msg2, msg3, msg4, sep="<br/>")
  }
  else if(state == "ended"){
    msg1 <- paste0("> You chose Door ", door, "!")
    if(won == 1){
      msg2 <- paste0("> Congratulations!! <br/>> Door ", door, " has the prize! You won!")
      won <- readRDS('won.rds') + 1
      saveRDS(won, "won.rds")
    }
    else {
      msg2 <- paste0("> Unfortunately, Door ", door, " does NOT have the prize! You lost..")
    }
    msg3 <- "<hr/>"
    msg4 <- paste0("> If you want to play again you can start chosing your first door again now..")
    msg <- paste(msg1, msg2, msg3, msg4, sep="<br/>")
  }
  return(msg)
}

plotDistrib <- function(){
  state <- readRDS('state.rds')
  if(state == "chose.first.door"){
    plot.probs(dist.game())
  }
  else {
    NULL
  }
}

shinyServer(
  function(input, output) {
    door1 <- renderText({input$buttonDoor1 
      isolate(input$buttonDoor1)})
    door2 <- renderText({input$buttonDoor2 
      isolate(input$buttonDoor2)})
    door3 <- renderText({input$buttonDoor3 
      isolate(input$buttonDoor3)})
    
    hostmsg <- reactive({
      prize.door <- getPrizeDoor()
      door <- choose_door(door1, door2, door3)
      open.door <- openDoor(door, prize.door)
      won <- wonGame(door, prize.door)
      
      getHostMsg(door, prize.door, open.door, won)
    })
    
    probPlot <- renderPlot({plotDistrib()})
    
    # To conditionally load main panel at ui.R
    output$myoutput <- renderUI({
      tagList(
        HTML("<br/><br/>"),
        
        HTML("<br/>How to play:<br/>1) Observe the instructions on the right given by the Host<br/> 2) Observe the probabilities chart to decide if your going to switch your first door choice or not.<br/>
           3) For more info on the game, google Monty Hall."),
        HTML("<br/><br/><b>Notes:</b><br/>Probability chart is created with simulation by repeating 300 times the calculation of the probability of winning (by switching or not switching door) using 100 tries."),
        
        HTML("<br/><br/><br/><b>About the problem:</b><br/><li><a target='blank' href='https://en.wikipedia.org/wiki/Monty_Hall_problem'>Understand the Monty Hall problem!</a></li>"),
        HTML("<li><a target='blank' href='https://ricardosc.shinyapps.io/MontyHallSim/'>Run different Monty Hall simulations!</a></li><br/>"),
        
        HTML("<br/><b>Credits:</b> <a target='blank' href='http://ricardoscr.github.io/'>Ricardo Carvalho</a> (ricardosc at gmail.com)<br/>"),
        
        HTML("<li>Complete code for this shiny app<a target='blank' href='https://github.com/ricardoscr/monty-hall-game'> available on Github.</a><br/>")
        )
      
    })
    
    # To conditionally load main panel at ui.R
    output$mainpanel <- renderUI({
      tagList(
        h3('[HOST]'),
        HTML(paste0("<h4>", hostmsg(), "</h4>")),
        HTML("<hr/>"),
        probPlot
      )
    })
    
  }
)