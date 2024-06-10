library(GA)

# Two-point crossover function
twoPointCrossover <- function(parents, ...) {
  n <- nrow(parents)
  children <- matrix(0, nrow = n, ncol = ncol(parents))
  for (i in seq(1, n, by = 2)) {
    points <- sort(sample(1:(ncol(parents) - 1), 2))
    children[i, ] <- c(parents[i, 1:points[1]], parents[i + 1, (points[1] + 1):points[2]], parents[i + 1, (points[2] + 1):ncol(parents)])
    children[i + 1, ] <- c(parents[i + 1, 1:points[1]], parents[i, (points[1] + 1):points[2]], parents[i, (points[2] + 1):ncol(parents)])
  }
  return(children)
}



# This function is used by the GA to compute or report the statistics of your interest after every generation.
# This function overrides the default functionality provided by gaMonitor().
monitor <- function(obj){
  iter <- obj@iter
  if (iter <= maxGenerations){
    fitness <- obj@fitness
    thisRunResults[iter,1] <<- max(fitness)
    thisRunResults[iter,2] <<- mean(fitness)    
    thisRunResults[iter,3] <<- median(fitness)
    cat(paste("\rGA | generation =", obj@iter, "Mean =", thisRunResults[iter,2], "| Best =", thisRunResults[iter,1], "\n"))
    flush.console()
  }  
  else {
    cat("ERROR: iter = ", iter, "exceeds maxGenerations = ", maxGenerations, ".\n")
    cat("Ensure maxGenerations == nrow(thisRunResults)")
  }
}



runGA <- function(noRuns = 30){
  maxGenerations <<- 20
  popSize = 40
  pcrossover = 0.8
  pmutation = 0.1
  type = "binary"
  crossover = gabin_spCrossover    #gabin_spCrossover, gabin_uCrossover #twoPointCrossover
  data <- getData()
  xx <- data[,-ncol(data)]
  yy <- data[,ncol(data)]
  fitness <- featureFitness
  
  # Set up what stats you wish to note.    
  statnames <- c("best", "mean", "median")
  thisRunResults <<- matrix(nrow=maxGenerations, ncol = length(statnames)) # Stats of a single run
  resultsMatrix <- matrix(1:maxGenerations, ncol = 1)  # Stats of all the runs
  
  resultNames <- character(length(statnames)*noRuns)
  resultNames[1] <- "Generation"
  
  bestFitness <<- -Inf
  bestSolution <<- NULL
  
  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    GA <- ga(type=type, fitness = fitness, xx=xx, yy=yy, nBits = ncol(xx), 
             names = colnames(xx), seed=i, popSize = popSize, 
             pcrossover = pcrossover, pmutation = pmutation, 
             maxiter = maxGenerations, monitor= monitor, crossover = crossover)
    
    resultsMatrix <- cbind(resultsMatrix, thisRunResults)
    
    if (GA@fitnessValue > bestFitness){
      bestFitness <<- GA@fitnessValue
      bestSolution <<- GA@solution
    }
    # Create column names for the resultsMatrix
    for (j in 1:length(statnames)) resultNames[1+(i-1)*length(statnames)+j] <- paste(statnames[j],i)
  }
  colnames(resultsMatrix) <- resultNames
  return (resultsMatrix)
}

getBestFitness <- function(){
  return(bestFitness)
}

getBestSolution <- function(){
  return(bestSolution)
}
