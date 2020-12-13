lol <- read.csv("2020_LoL_esports_match_data_from_OraclesElixir_20201204.csv", header = T)
head(lol)##to see the original data variables
##create a set to include all the variables that I choose to research on for this project
selectvariables <- c("side", "gamelength", "result", "teamkills", "teamdeaths",
                     "dpm","earned.gpm", "golddiffat10", "golddiffat15")
lol <- lol[selectvariables]##subsetting
head(lol)##check the new data

##data transformation
##the gamelength is in second, plan to convert to mins for convinence
lol$gamelength <- lol$gamelength/60
##convert the result to charactor variable of "win" and "loss"
lol$result[lol$result == "0"] <- "loss"
lol$result[lol$result == "1"] <- "win"


##create a new variable to characterize different length of games
length <- c()##new variable
length[lol$gamelength < 25] <- "Short"
length[lol$gamelength >= 25 & lol$gamelength < 38] <- "Regular"
length[lol$gamelength >= 38] <- "Long"
##define gamelength to "short" "regular" and "long" games
lol <- cbind(lol, length)##bind with data set


##3D Pie Chart
library(plotrix)
lengthtab <- table(lol$length)##create counts of different length of games
lengthtab
##Long Regular   Short 
##12600   59376    5568 
lengthlab <- paste(names(lengthtab), " game")##names of label
pie3D(lengthtab,labels = lengthlab, col = c(rgb(0,0.2,0,0.5),rgb(0.1,0.4,0.8,0.7), rgb(0.7,0.6,0,0.5)), explode = 0.2,
      labelcex = 2, radius = 1.5, main = "Game length pie", start = 2)


##mosaic plot
require(stats)
mosaicplot(~ length[lol$result == "win"] + side[lol$result == "win"], data = lol, 
           main = "win rate(/side /game length)",  xlab = "game legnth", ylab = "side",
           color = c(rgb(0,0,1,0.5),rgb(1,0,0,0.5)), las = 1)
##create mosaic plot to see win rate of each side when game length changes.
abline(h = 0.48, col = rgb(0,0,0,0.4), lty = 2, lwd = 2)##add reference line

##Waffle plot
library(ggplot2)
library(waffle)
winside <- table(lol$side[lol$result == "win"])##get the counts of the win games from each side
winside
##Blue   Red 
##20658 18114 
waffle(winside,rows = 100, size = 0.1,  col = c(rgb(0.1,0.4,0.8,0.5), rgb(0.7,0.2,0.2,0.5)))


##histogram of game length
hist(lol$gamelength,col=rgb(0.1,0.4,0.8,0.5), main = "gamelength", freq=F, ylim = c(0,0.12), xlim = c(10,70),
     breaks=seq(15,65,2), xlab = "gamelength(min)")
##histogram of gamelength showing how long the league of legends games usually are
denswin1<-density(lol$gamelength[lol$result == "win"],bw=1)## add a density line
lines(denswin1,lwd=2,col=rgb(0,0.4,0,0.5))

##prediction algorithm
resultb <- c()
##changing the dependent variables to numeric
resultb[lol$result == "win"] <- 1
resultb[lol$result == "loss"] <- 0
lol <- cbind(lol,resultb)


# Split data set into training set and test set
lol_noNA <- na.omit(lol)
n <- nrow(lol_noNA)  # Number of observations = 98
ntrain <- round(n*0.6)    # 60% for training set
set.seed(314)             # Set seed for reproducible results
tindex <- sample(n, ntrain) # Create an index
trainlol <- lol_noNA[tindex,]  # Create training set
testlol <- lol_noNA[-tindex,]  # Create test set

lmwin <- glm(resultb~ side+gamelength+teamkills+teamdeaths+dpm+earned.gpm+
               golddiffat10+golddiffat15, data=trainlol, family = "binomial")
summary(lmwin)

##Call:
##  glm(formula = resultb ~ side + gamelength + teamkills + teamdeaths + 
##        dpm + earned.gpm + golddiffat10 + golddiffat15, family = "binomial", 
##      data = trainlol)

##Deviance Residuals: 
##  Min       1Q   Median       3Q      Max  
##-4.5634  -0.0743   0.0007   0.0739   4.4519  

##Coefficients:
##  Estimate Std. Error z value Pr(>|z|)    
##(Intercept)  -1.043e+00  2.168e-01  -4.811  1.5e-06 ***
##  sideRed      -7.997e-02  6.022e-02  -1.328   0.1841    
##gamelength    4.205e-03  5.375e-03   0.782   0.4340    
##teamkills     5.768e-01  8.628e-03  66.857  < 2e-16 ***
##  teamdeaths   -5.784e-01  8.687e-03 -66.582  < 2e-16 ***
##  dpm          -1.029e-03  2.186e-04  -4.708  2.5e-06 ***
##  earned.gpm    6.235e-03  6.112e-04  10.201  < 2e-16 ***
##  golddiffat10 -1.980e-04  9.746e-05  -2.032   0.0422 *  
##  golddiffat15  5.704e-05  5.584e-05   1.022   0.3070    
##---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##(Dispersion parameter for binomial family taken to be 1)

##Null deviance: 53325  on 38465  degrees of freedom
##Residual deviance:  7766  on 38457  degrees of freedom
##AIC: 7784

##Number of Fisher Scoring iterations: 8


predict <- predict(lmwin, newdata=testlol, type="response") ##predict the data from testlol
# Check correlation between predicted and actual test set response values 
summary(predict)
predict[predict > 0.5] <- 1
predict[predict <= 0.5] <- 0
cor(predict, testlol$resultb)
##[1] 0.9224758
##pretty high correlation. great model
