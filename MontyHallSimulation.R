
sim.won <- function(prize.door, first.door, switching){
  as.numeric(ifelse(first.door == prize.door, !switching, switching))
}

sim.choose.door <- function(n){
  door.prob <- runif(n)
  ifelse(door.prob <= 1/3, 1, ifelse(door.prob <= 2/3, 2, 3))
}

sim.choose.switching <- function(n){
  switching.prob <- runif(n)
  ifelse(switching.prob <= 1/2, TRUE, FALSE)
}

sim.game <- function(n=1000){
  prize.door <- sim.choose.door(n)
  first.door <- sim.choose.door(n)
  switching <- sim.choose.switching(n)
  won <- sim.won(prize.door, first.door, switching)
  
  df <- data.frame(won = won, switching = switching)
  
  prob.win.switching <- table(df$won, df$switching)[2,2]/sum(table(df$won, df$switching)[,2])
  prob.win.not.switching <- table(df$won, df$switching)[1,2]/sum(table(df$won, df$switching)[,1])
  
  return(c(prob.win.switching, prob.win.not.switching))
}

plot.probs <- function(df, bins = 50){
  require(ggplot2)
  require(gridExtra)
  
  bw <- (max(df$prob.win.switching) - min(df$prob.win.switching))/(bins - 1)
  
  h1 <- ggplot(df, aes(prob.win.switching)) + geom_histogram(binwidth = bw) + 
    ggtitle('Switching') + xlab('Probability') + 
    geom_vline(xintercept = mean(df$prob.win.switching), colour='red') + 
    scale_x_continuous(breaks = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90, round(mean(df$prob.win.switching),2)), limits=c(0.10, 0.90))
  
  bw <- (max(df$prob.win.not.switching) - min(df$prob.win.not.switching))/(bins - 1)
  h2 <- ggplot(df, aes(prob.win.not.switching)) + geom_histogram(binwidth = bw) + 
    ggtitle('NOT Switching') + xlab('Probability') + 
    geom_vline(xintercept = mean(df$prob.win.not.switching), colour='red') +
    scale_x_continuous(breaks = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90, round(mean(df$prob.win.not.switching),2)), limits=c(0.10, 0.90))
  
  grid.arrange(h1, h2, nrow = 1, top="Distributions of Probability of Winning")
}

dist.game <- function(reps=300, n=100){
  if(n < 100){
    n <- 100
  }
  if(reps < 300){
    reps <- 300
  }
  
  dist <- data.frame(prob.win.switching = rep(0, times = reps),
                     prob.win.not.switching = rep(0, times = reps))
  
  for(i in 1:reps){
    dist[i, ] <- sim.game(n)
  }
  
  dist
}

###########################################################################################

## Run a single simulation
#dist.game()

## Detailed view of simulation timings
# library(microbenchmark)
# microbenchmark(
#   dist.game(),
#   times = 20
# )

