

# load packages
# library(car)
# library(rTPC) #remotes::instany_github('padpadpadpad/rTPC')
library(tidyverse)
library(dplyr)
library(boot)
library(nlstools)
library(nls.multstart)
library(broom)
library(tidyverse)
library(patchwork)
library(minpack.lm)
library(writexl)
library(xlsx)
library(dplyr)
tizenBugs <-
  readxl::read_xlsx(
    "countedSRs.xlsx",
    col_names = c("x", "y"),
    col_types = c("text", "numeric")
  )
x <- tizenBugs['x']
y <- tizenBugs['y']

df <- data.frame(x, y)
df$x <- as.Date(df$x)
df$date <- as.POSIXct(df$x,
                      format = "%Y-%m-%d",
                      tz = "UTC",)
df$days <- as.numeric(df$x - df[1, ]$x)
df$months <- round(as.numeric(df$x - df[1, ]$x) / 30)

valuesOfB <- c(0.001 , 0.01, 10 , 100)
valuesOfC <- c(0.001 , 0.01 , 10, 100)

valuesOfA <- c(500 , 2000 , 6000)
bothdfs <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(bothdfs) <- c('Value of A','Value of b','Value of c','Name of Formula')

wb = createWorkbook()
sheet = createSheet(wb, "sheet1")
startRow = 1


nValue = 100
for (aValue in valuesOfA)
  {
    for (cValue in valuesOfC) {
      for (bValue in valuesOfB) {
        DF2<- data.frame(c(str_interp("number of iterations = $[f]{nValue}")),c(str_interp("value of A = $[f]{aValue}")),c(str_interp("Value of B = $[f]{bValue}")),c(str_interp("Value of C = $[f]{cValue}")))
        colnames(DF2) <- colnames(bothdfs) 
        bothdfs <- rbind(bothdfs, DF2)
        positiveResults <- data.frame(matrix(ncol = 4, nrow = 0))
        colnames(positiveResults) <- c('Value of A','Value of b','Value of c','Name of Formula')
        foundNegative = FALSE
        ## Goel-Okumoto (GO)
        tryCatch({
          goDays <- minpack.lm::nlsLM(
            formula = y ~ a * (1 - exp(-b * days)),
            ## model
            data = df,
            ## dataset
            start = list(a = aValue, b = bValue),
            ## starting values
          )
          # bootstrapping the SRGMs
          goBootDays <-  nlsBoot(goDays, niter = nValue)
          result <- data.frame(goBootDays[["bootCI"]], 'goBootDays')
          if(any(goBootDays[["bootCI"]])<0){
            foundNegative = TRUE
            print(goBootDays[["bootCI"]])

            
          }
          colnames(result) <- colnames(positiveResults) 
          positiveResults <- rbind(positiveResults, result)
          
          
        },
        error = function(cond) {
          message(cond)
        })
       
        
        tryCatch({
          ## GO S-Shaped (GoS)
          goSDays <- minpack.lm::nlsLM(
            formula = y ~ a * (1 - (1 + b * days) * exp(-b * days)),
            ## model
            data = df,
            ## dataset
            start = c(a = aValue, b = bValue, c = cValue),
            ## starting values
          )
          goSBootDays <- nlsBoot(goSDays, niter = nValue)
          result <- data.frame(goSBootDays[["bootCI"]], 'goSBootDays')
          if(any(goSBootDays[["bootCI"]])<0){
            print(goSBootDays[["bootCI"]])
            
            foundNegative = TRUE
            
          }
          colnames(result) <- colnames(positiveResults) 
          positiveResults <- rbind(positiveResults, result)
        },
        error = function(cond) {
          message(cond)
        })
      
        tryCatch({
          ## Logistics (L)
          lDays <- minpack.lm::nlsLM(
            formula = y ~ a / (1 + b * exp(-c * days)),
            ## model
            data = df,
            ## dataset
            start = c(a = aValue, b = bValue, c = cValue),
            ## starting values
          )
          
          lBootDays <- nlsBoot(lDays, niter = nValue)
          result <- data.frame(lBootDays[["bootCI"]], 'lBootDays')
          if(any(lBootDays[["bootCI"]])<0){
            print(lBootDays[["bootCI"]])
            foundNegative = TRUE
            
          }
          colnames(result) <- colnames(positiveResults) 
          positiveResults <- rbind(positiveResults, result)
          
        },
        error = function(cond) {
          message(cond)
        })
        
        tryCatch({
          ## Hossain-Dahiya (HD)
          hdDays <- minpack.lm::nlsLM(
            formula = y ~ a * ((1 - exp(-b * days)) / (1 + c * exp(-b * c))),
            ## model
            data = df,
            ## dataset
            start = c(a = aValue, b = bValue, c = cValue),
            ## starting values
          )
          
          hdBootDays <- nlsBoot(hdDays, niter = nValue)
          if(any(hdBootDays[["bootCI"]])<0){
            print(hdBootDays[["bootCI"]])
            
            foundNegative = TRUE
            
          }
          result <- data.frame(hdBootDays[["bootCI"]], 'hdBootDays')
          colnames(result) <- colnames(positiveResults) 
          positiveResults <- rbind(positiveResults, result)
          
        },
        error = function(cond) {
          message(cond)
        })
        
        tryCatch({
          ## Weibull (W)
          wDays <- minpack.lm::nlsLM(
            formula = y ~ a * (1 - exp(-b * (days ** c))),
            ## model
            data = df,
            ## dataset
            start = c(a = aValue, b = bValue, c = cValue),
            ## starting values
          )
          
          wBootDays <- nlsBoot(wDays, niter = nValue)
          if(any(wBootDays[["bootCI"]])<0){
            print(wBootDays[["bootCI"]])
            
            foundNegative = TRUE
            
          }
          result <- data.frame(wBootDays[["bootCI"]], 'wBootDays')
          colnames(result) <- colnames(positiveResults) 
          positiveResults <- rbind(positiveResults, result)
         
        },
        error = function(cond) {
          message(cond)
        })
       
        tryCatch({
          ## W more S-Shaped (WS)
          wsDays <- minpack.lm::nlsLM( 
            formula = y ~ a * (1 - (1 + b * (days ** c)) * exp(-b * (days ** c))),
            ## model
            data = df,
            ## dataset
            start = c(a = aValue, b = bValue, c = cValue),
            ## starting values
          )
          
          wsBootDays <- nlsBoot(wsDays, niter = nValue)
          if(any(wsBootDays[["bootCI"]])<0){
            print(wsBootDays[["bootCI"]])
            
            foundNegative = TRUE
            
          }
          result <- data.frame(wsBootDays[["bootCI"]], 'wsBootDays')
          colnames(result) <- colnames(positiveResults) 
          positiveResults <- rbind(positiveResults, result)

        },
        error = function(cond) {
          message(cond)
        })
       
        tryCatch({
          ## Yamada Exp. (YE)
          yeDays <- minpack.lm::nlsLM(
            formula = y ~ a * (1 - exp(-b * (
              1 - exp(c * days)
            ))),
            ## model
            data = df,
            ## dataset
            start = c(a = aValue, b = bValue, c = cValue),
            ## starting values
          )
          
          yeBootDays <- nlsBoot(yeDays, niter = nValue)
          if(any(yeBootDays[["bootCI"]])<0){
            print(yeBootDays[["bootCI"]])
            
            foundNegative = TRUE
            
          }
          result <- data.frame(yeBootDays[["bootCI"]], 'yeBootDays')
          colnames(result) <- colnames(positiveResults) 
          positiveResults <- rbind(positiveResults, result)
          
          
        },
        error = function(cond) {
          message(cond)
        })
        tryCatch({
          ## Yamada Raleigh (YR)
          yrDays <- minpack.lm::nlsLM(
            formula = y ~ a * (1 - exp(-b * (
              1 - exp(-c * (days ** 2 / 2))
            ))),
            ## model
            data = df,
            ## dataset
            start = c(a = aValue, b = bValue, c = cValue),
            ## starting values
          )
          
          yrBootDays <- nlsBoot(yrDays, niter = nValue)
          if(any(yrBootDays[["bootCI"]])<0){
            print(yrBootDays[["bootCI"]])
            foundNegative = TRUE
            
          }
          result <- data.frame(yrBootDays[["bootCI"]], 'yrBootDays')
          colnames(result) <- colnames(positiveResults) 
          positiveResults <- rbind(positiveResults, result)
        },
        error = function(cond) {
          message(cond)
        })
        
        ## merge positive only if there's no negative in any
        ## assuming that, it's better to get rid of any negative in any results to accept the result of any
        ## if and only if any results are positive, I will accept results of the any equations
        colnames(positiveResults) <- colnames(bothdfs) 
        bothdfs <- rbind(bothdfs, positiveResults)
      
    }
  }
}
addDataFrame(
  bothdfs,
  sheet = sheet,
  startRow = 1
)
saveWorkbook(wb, "resultTizen.xlsx")



# tizenBugLong <- gather(tizenBugs, key = rep_site, value = SR, -Quadrat_Area)

# separate(rep_site, into = c("repo", "site"))

# ggplot(sp_long, aes(x = Quadrat_Area, y = SR)) +
#   geom_point(aes(color = site))

# sal <- nls(SR ~ c * Quadrant_Area ^ z, data = tizenBugLong, start = c(c = 10, z = 0.27))
# fit using Levenberg-Marquardt algorithm
