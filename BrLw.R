BrLw <-function(numInClass,odds){

  days=1:365
  varstor=numeric()
  brownlow = 0
  
  for (i in 1:1000) {
    sam=sample(days,size=numInClass,replace = T)
    y=duplicated(sam)
    if (sum(y)>0) brownlow=brownlow+1
    #else brownlow = brownlow - odds
  }
  pWin = brownlow/1000
  
  bWins = numInClass/2 * pWin * 1
  bLoss = numInClass/2 * (1 - pWin)*odds
  
 return(bWins-bLoss)
}

