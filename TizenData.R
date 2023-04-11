

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
    "countedSRsCyanogenmodBugs.xlsx",
    col_names = c("t", "bugs"),
    col_types = c("numeric", "numeric")
  )
print(tizenBugs)
df <- data.frame(tizenBugs['t'], tizenBugs['bugs'])


valuesOfB <- c(0.1 , 0.5, 1 )
valuesOfC <- c(0.1 , 0.5 , 1)
valuesOfA <- c(10 , 50 , 100)

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
      DF2<- data.frame(c(str_interp("value of A = $[f]{aValue}")),c(str_interp("Value of B = $[f]{bValue}")),c(str_interp("Value of C = $[f]{cValue}")))
      colnames(DF2) <- colnames(bothdfs) 
      bothdfs <- rbind(bothdfs, DF2)
      positiveResults <- data.frame(matrix(ncol = 4, nrow = 0))
      colnames(positiveResults) <- c('Value of A','Value of b','Value of c','Name of Formula')
      foundNegative = FALSE
      ## Goel-Okumoto (GO)
      tryCatch({
        goDays <- minpack.lm::nlsLM(
          formula = bugs ~ a * (1 - exp(-b * t)),
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
          formula = bugs ~ a * (1 - (1 + b * t) * exp(-b * t)),
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
          formula = bugs ~ a / (1 + b * exp(-c * t)),
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
          formula = bugs ~ a * ((1 - exp(-b * t)) / (1 + c * exp(-b * c))),
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
          formula = bugs ~ a * (1 - exp(-b * (t ** c))),
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
          formula = bugs ~ a * (1 - (1 + b * (t ** c)) * exp(-b * (t ** c))),
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
          formula = bugs ~ a * (1 - exp(-b * (
            1 - exp(c * t)
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
          formula = bugs ~ a * (1 - exp(-b * (
            1 - exp(-c * (t ** 2 / 2))
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
saveWorkbook(wb, "resultCyanogenmod.xlsx")



# tizenBugLong <- gather(tizenBugs, key = rep_site, value = SR, -Quadrat_Area)

# separate(rep_site, into = c("repo", "site"))

# ggplot(sp_long, aes(x = Quadrat_Area, y = SR)) +
#   geom_point(aes(color = site))

# sal <- nls(SR ~ c * Quadrant_Area ^ z, data = tizenBugLong, start = c(c = 10, z = 0.27))
# fit using Levenberg-Marquardt algorithm
