library(minpack.lm)
library(xlsx)
library(stringr)
library(nlstools)

dbNames <- list('Tizen', 'Cyanogenmod','Nemo', 'Mer')

for(dbName in dbNames){
  arrivalBugs <-
    readxl::read_xlsx(
      str_glue("./results/countedSRs{dbName}Bugs.xlsx"),
      col_names = c("t", "y"),
      col_types = c("numeric", "numeric")
    )
  t <- arrivalBugs['t']
  y <- arrivalBugs['y']
  columnNames <- c('median','2.5','97.5', 'SRGM Name')
  df <- data.frame(t, y)
  valuesOfB <- c(0.01 , 0.1, 1 , 4)
  valuesOfC <- c(0.01 , 0.1 , 1, 4)
  
  valuesOfA <- c(100 , 150 , 200)
  bothdfs <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(bothdfs) <- columnNames
  
  wb = createWorkbook()
  sheet = createSheet(wb, "sheet1")
  startRow = 1
  
  
  nValue = 100
  for (aValue in valuesOfA)
    {
      for (bValue in valuesOfB) {
        for (cValue in valuesOfC) {
          DF2<- data.frame(c(str_interp("number of iterations = $[f]{nValue}")),c(str_interp("value of A = $[f]{aValue}")),c(str_interp("Value of B = $[f]{bValue}")),c(str_interp("Value of C = $[f]{cValue}")))
          colnames(DF2) <- colnames(bothdfs) 
          bothdfs <- rbind(bothdfs, DF2)
          result <- data.frame(matrix(ncol = 4, nrow = 0))
          colnames(result) <- columnNames
          positiveResults <- data.frame(matrix(ncol = 4, nrow = 0))
          colnames(positiveResults) <- columnNames
          
          ## Goel-Okumoto (GO)
          tryCatch({
            go <- minpack.lm::nlsLM(
              formula = y ~ a * (1 - exp(-b * t)),
              ## model
              data = df,
              ## dataset
              start = list(a = aValue, b = bValue),
              ## starting values
            )
            # bootstrapping the SRGMs
            goBoot <-  nlsBoot(go, niter = nValue)
            result <- data.frame(goBoot[["bootCI"]], 'goBoot')
            
            colnames(result) <- colnames(positiveResults) 
            positiveResults <- rbind(positiveResults, result)
          },
          error = function(cond) {
            message(cond)
          })
         
        
          tryCatch({
            ## Logistics (L)
            l <- minpack.lm::nlsLM(
              formula = y ~ a / (1 + b * exp(-c * t)),
              ## model
              data = df,
              ## dataset
              start = c(a = aValue, b = bValue, c = cValue),
              ## starting values
            )
            
            lBoot <- nlsBoot(l, niter = nValue)
            

            
            result <- data.frame(lBoot[["bootCI"]], 'lBoot')
            
            colnames(result) <- colnames(positiveResults) 
            positiveResults <- rbind(positiveResults, result)
            
            plot(lBoot)
            plot(lBoot, type = "boxplot", ask = FALSE)
            summary(lBoot)
          },
          error = function(cond) {
            message(cond)
          })
          
          
          tryCatch({
            ## Weibull (W)
            w <- minpack.lm::nlsLM(
              formula = y ~ a * (1 - exp(-b * (t ** c))),
              ## model
              data = df,
              ## dataset
              start = c(a = aValue, b = bValue, c = cValue),
              ## starting values
            )
            
            wBoot <- nlsBoot(w, niter = nValue)
            

            
            result <- data.frame(wBoot[["bootCI"]], 'wBoot')
            colnames(result) <- colnames(positiveResults) 
            positiveResults <- rbind(positiveResults, result)
            
            plot(wBoot)
            plot(wBoot, type = "boxplot", ask = FALSE)
            summary(wBoot)
          },
          error = function(cond) {
            message(cond)
          })
         
          tryCatch({
            ## W more S-Shaped (WS)
            ws <- minpack.lm::nlsLM( 
              formula = y ~ a * (1 - (1 + b * ( t** c)) * exp(-b * (t ** c))),
              ## model
              data = df,
              ## dataset
              start = c(a = aValue, b = bValue, c = cValue),
              ## starting values
            )
            
            wsBoot <- nlsBoot(ws, niter = nValue)

            
            result <- data.frame(wsBoot[["bootCI"]], 'wsBoot')
            colnames(result) <- colnames(positiveResults) 
            positiveResults <- rbind(positiveResults, result)
            
            plot(wsBoot)
            plot(wsBoot, type = "boxplot", ask = FALSE)
            summary(wsBoot)
          },
          error = function(cond) {
            message(cond)
          })
         

          tryCatch({
            ## Yamada Raleigh (YR)
            yr <- minpack.lm::nlsLM(
              formula = y ~ a * (1 - exp(-b * (
                1 - exp(-c * (t ** 2 / 2))
              ))),
              ## model
              data = df,
              ## dataset
              start = c(a = aValue, b = bValue, c = cValue),
              ## starting values
            )
            
            yrBoot <- nlsBoot(yr, niter = nValue)
    
            
            result <- data.frame(yrBoot[["bootCI"]], 'yrBoot')
            colnames(result) <- colnames(positiveResults) 
            positiveResults <- rbind(positiveResults, result)
            plot(yrBoot)
            plot(yrBoot, type = "boxplot", ask = FALSE)
            summary(yrBoot)
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
  saveWorkbook(wb, str_glue("./results/resultEdited{dbName}.xlsx"))
  
} 