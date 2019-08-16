gamestrategy <- function(goal){
  sN = goal+6
  V <<- array(NA, c(sN, sN, sN))
  U <<- array(NA, c(sN, sN, sN))
  for (i in (0:(sN-1))){
    for (j in (0:(sN-1))){
      for(k in (0:(sN-1))){
        pWin(i,j,k,goal)
      }
    }
  }
  save(list = c('V', 'U'), file = 'VUfile.Rdata')
}

pWin <- function(i,j,k,goal){
  if ((i+k) >= goal){
    V[i+1,j+1,k+1] <<- 1
    U[i+1,j+1,k+1] <<- 2
    return(1)
  }
  if (j >= goal){
    V[i+1,j+1,k+1] <<- 0
    U[i+1,j+1,k+1] <<- 2
    return(0)
  }
  if (!is.na(V[i+1,j+1,k+1])){
    return(V[i+1,j+1,k+1])
  }
  pRoll <- 1 - pWin(j, (i+1), 0, goal)
  for (roll in 2:6){
    pRoll = pRoll + pWin(i, j, (k+roll), goal)
  }
  pRoll = pRoll/6
  
  if (k==0){
    pHold = 1 - pWin(j, (i+1), 0, goal)
  } else {
    pHold = 1 - pWin(j, (i+k), 0, goal)
  }
  if ((k==0) & (pHold > pRoll)) {
  }
  V[i+1,j+1,k+1] <<- max(pHold, pRoll)
  U[i+1,j+1,k+1] <<- which.max(c(pRoll, pHold))
  
  return(V[i+1,j+1,k+1])
}

gamestrategy(100)
