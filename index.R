#library(TTR)
#install.packages("tidyverse")
library(tidyverse)
#x <- stockSymbols()
#view(x)
library(quantmod)
library(GA)
#choosing assets manually
chosenAssets <- c ("AAPL","QCOM","MAR","PEP","COST","PYPL","WBD","GOOGL","SBUX","VRTX")
getSymbols(chosenAssets, src="yahoo", from="2020-01-01", to="2021-01-01")
head(QCOM)
chartSeries(QCOM)
myRetData <- data.frame(merge(dailyReturn(AAPL),dailyReturn(QCOM),dailyReturn(MAR)
                              ,dailyReturn(PEP),dailyReturn(COST),dailyReturn(PYPL)
                              ,dailyReturn(WBD),dailyReturn(GOOGL),dailyReturn(SBUX)
                              ,dailyReturn(VRTX)))

colnames(myRetData) <- chosenAssets
head(myRetData)
#Mean Daily Returns for each asset
meanDailyReturns <-   apply(myRetData,2,mean)
meanDailyReturns

#generating weights
chromosome <- runif(n = 10)
weight <-  chromosome/sum(chromosome)
weight
sum(weight)

#Defining population size and number of assets
popSize = 100
num_assets=10


# for (r in 1:popSize) {
#   chromosome <- runif(n = num_assets)
#   weight <-  chromosome/sum(chromosome)
#   if (r == 1) {
#     n <- rbind(weight)
#   }
#   else
#   {
#     n <- rbind(n,weight)
#   }
# }

#Funcion for creating initial population 
#This function will generate 10 weights
#corresponding to 10 assets for each individual and the sum of
#weights is equal to 1
initial_population <- function(object) {
  for (r in 1:popSize) {
    chromosome <- runif(n = 10) #generate random chromosome (weights)
    weight <-  chromosome/sum(chromosome) #weights = gene/sum(chromosome weights)
    if (r == 1) {
      initialPop <- rbind(weight)
    }
    else
    {
      initialPop <- rbind(initialPop,weight) #append each row to the matrix
    }
  }
  return(initialPop) #return matrix of population to the GA function
}
  
# dim(n)
# n[1,]
# initChromosomes <- 
# initialPop <- matrix(runif(popSize * num_assets), ncol = num_assets)
# initialPop
# initial_population <- function(object) {
#   initialPop <- matrix(runif(popSize * num_assets), ncol = num_assets)
#   return(initialPop)
# }
# initialPop[1,]

#Annualized portfolio returns for single portfolio
normal_returns <- sum(weight * meanDailyReturns) 
normal_returns
annual_returns <- (sum(weight * meanDailyReturns) + 1)^252 - 1
annual_returns

#Covariance
head (myRetData)
cov_assets = cov(data.frame(myRetData))
cov_assets
risk = t(weight) %*% (cov_assets %*% weight)
risk
ind_asset=normal_returns/risk
ind_asset
#Check the default values for Selection, Crossover, Mutation
#We can see that if we use default value of gareal_laCrossover (Local Arithmetic cross)
#or gareal_raMutation (Uniform random mutation), it will break the restriction that
#sum of weights should always be 1
defaultControl <- gaControl()
print(defaultControl)

#Hence setting the new values for selection, crossover and mutation as per the
#paper by Chi-Ming Lin and Mitsuo Gen
gaControl("real-valued" = list(selection = "gareal_rwSelection", #Roulette wheel Selection
                               crossover = "gareal_spCrossover", #Single Point Crossover
                               mutation= "gaperm_ismMutation"))  #Inversion Mutation

# evalFunc <- function(weights,meanDailyRet,cov_assets) {
#   #print(weights)
#   returns <- sum(weights * meanDailyRet)
#   risk <- t(weights) %*% (cov_assets %*% weights)
#   eval = returns/risk
#     return(eval)
# }
evalFunc <- function(weights,meanDailyRet,cov_assets) {
  #print(weights)
  returns <- sum(weights * meanDailyRet)
  risk <- t(weights) %*% (cov_assets %*% weights)
  eval = returns/risk
  return(eval)
}

n <- initial_population()
sum(n[1,])

GA <- ga(type = "real-valued", fitness = function(x) evalFunc(x, meanDailyReturns, cov_assets), 
             population = initial_population,
             popSize = popSize, maxiter = 100,
             pcrossover = 0.8,
             lower = c(0,0,0,0,0,0,0,0,0,0),
             upper = c(1,1,1,1,1,1,1,1,1,1),
             pmutation = 0.2,monitor = TRUE
)
print(GA@solution)
optimal_weights <- GA@solution
final_return1 <- sum(optimal_weights * meanDailyReturns)
final_return1
pie(optimal_weights, labels = chosenAssets, main="Manually Generated Portfolio 2021")
summary(GA)
plot(GA)

# Done with getting weights for manually generated GA
#Fetch data for 2022
getSymbols(chosenAssets, src="yahoo", from="2021-01-01", to="2022-01-01")
head(QCOM)
chartSeries(QCOM)
myRetData2 <- data.frame(merge(dailyReturn(AAPL),dailyReturn(QCOM),dailyReturn(MAR)
                              ,dailyReturn(PEP),dailyReturn(COST),dailyReturn(PYPL)
                              ,dailyReturn(WBD),dailyReturn(GOOGL),dailyReturn(SBUX)
                              ,dailyReturn(VRTX)))

colnames(myRetData2) <- chosenAssets
head(myRetData2)
meanDailyReturns2 <-   apply(myRetData2,2,mean)
meanDailyReturns2
final_return2 <- sum(optimal_weights * meanDailyReturns2)
final_return2

#evenly weighted portfolio
even_weights <- rep(0.1,10)
sum(even_weights)

#Randomly generated weights
for (r in 1:20) {
  chromosome <- runif(n = 10) #generate random chromosome (weights)
  weight <-  chromosome/sum(chromosome) #weights = gene/sum(chromosome weights)
  if (r == 1) {
    randompop <- rbind(weight)
  }
  else
  {
    randompop <- rbind(randompop,weight) #append each row to the matrix
  }
}
