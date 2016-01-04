#Name Zheng Chang
# Function1
readData = function(fileName, dateFormat = "%m/%d/%y"){
  
  # Read in the data and keep strings
  stockData = read.csv(fileName, stringsAsFactors = FALSE)
  
  # Convert Date to an object of Date class,
  stockData$Date = as.Date(stockData$Date, format = dateFormat)
  
  # Arrange the rows of stockData by increasing date
  stockData = stockData[order(stockData$Date), ]
  
  return(stockData[ , c("Date", "Adj.Close") ])
}
# Function2
combine2stocks = function(stockA, stockB) {
  
  commonDays = intersect(stockA$Date, stockB$Date)
  stockA = stockA[ stockA$Date %in% commonDays, ]
  stockB = stockB[ stockB$Date %in% commonDays, ]
  ratio = stockA$Adj.Close / stockB$Adj.Close
  
  return(data.frame(Date = stockA$Date, 
                    Adj.Close.A = stockA$Adj.Close,
                    Adj.Close.B = stockB$Adj.Close,
                    ratio = ratio))
}

# Function3
plotRatio = function(ratio, k = 1, 
                     date = seq(along = ratio), ...) {
  
  plot(ratio ~ date, type = "l", ...)
  
  meanRatio = mean(ratio)
  sdRatio = sd(ratio)
  abline(h = meanRatio, lty = "dashed", col = "darkgreen")
  abline( h = meanRatio + k * sdRatio, 
          lty = "dashed", col = "red")
  abline( h = meanRatio - k * sdRatio, 
          lty = "dashed", col = "red")
  
}
# Function4 
showPosition = function(pos, ratios, 
                        col = c("darkgreen", "red"),
                        radius = 100) {
  
  symbols(x = pos, y = ratios, fg = col,
          circles = rep(radius, 2), add = TRUE,
          inches = FALSE)  
}

if (FALSE) {
  ## This is code for testing my functions
  ## I include it in a code block that will not execute 
  ## because the if condition is always FALSE.
  ## This way, I can source in the functions and not have 
  ## the test code run. 
  
fn = "http://www.stat.berkeley.edu/users/nolan/data/stocks/f.csv"
gn = "http://www.stat.berkeley.edu/users/nolan/data/stocks/gm.csv"

sA = readData(fn)
sB = readData(gn)

stockB = sB[1000: 1036, ]
stockA = sA[3710:3770, ]
ratDF = combine2stocks(stockA, stockB)

dim(ratDF)
dim(stockB)
head(ratDF)
tail(ratDF)

plotRatio(ratDF2$ratio)
plotRatio(ratDF2$ratio, date = ratDF2$Date)
plotRatio(ratDF2$ratio, date = ratDF2$Date, 
          main = "Pairs Trading")

plotRatio(ratDF2$ratio, date = ratDF2$Date, 
          k = c(0.5, 1, 1.5), main = "Pairs Trading")

showPosition(pos = ratDF2$Date[c(5, 20)], 
             ratios = ratDF2$ratio[c(5, 20)], 
             radius = 2, col = c("blue", "orange"))

}




#Function 5
findNextPosition = function(ratio, startDay = 1, k = 1, m = mean(ratio), s = sd(ratio))
{
  redlineUp = m + k *s
  redlineDown = m - k *s
  
  if(startDay > 1){
    ratio = ratio[ - (1:(startDay-1)) ]}
  # It excludes the ratio that before the startday
  
  open_closeArea = (ratio >= redlineUp | ratio <= redlineDown)
  
  w = which(open_closeArea)
  
  if(length(w) == 0){
    return(integer())}
  #If no open position is found, then the function returns an empty vector.
  
  open = w[1]
  close = if(ratio[open] > redlineUp){
    ratio[ - (1:open) ] <= m}
  else{
    ratio[ - (1:open) ] >= m}
  # Find the close point.
  
  close = if(any(close)){
    which(close)[1] + open}
    else{
      length(ratio)
    } 
  return(c(open, close) + startDay - 1) 
}


#Function6
getPositions =
  function(ratio, k = 1, m = mean(ratio), s = sd(ratio))
  {
    position = list()
    i = 1
    startDay = 1
    
    tmp = findNextPosition(ratio, startDay = startDay, k, m, s)
    
    if (length(tmp) == 0 ){
      return(position)
    }   
    position[[i]] = tmp
    startDay = tmp[2]
    
    while(startDay < length(ratio)) {
     
      currentPosition = findNextPosition(ratio, startDay = startDay, k, m, s)
      
      if (length(currentPosition) == 0) {
        return(position)
      }
      
      i = i + 1
      position[[i]] = currentPosition
      startDay = currentPosition[2] 
    }

    return(position)
  }


#Function7
positionProfit = function(pos, stockPriceA, stockPriceB, 
                          m = mean(stockPriceA/stockPriceB) ){
  
  stockRatio = stockPriceA[pos[1]]/stockPriceB[pos[1]]
   
  profit = if(stockRatio >= m) {
    (stockPriceB[pos[2]]/stockPriceA[pos[1]] - stockPriceA[pos[2]]/stockPriceB[pos[1]])
    
   }else {
    (stockPriceA[pos[2]]/stockPriceB[pos[1]] - stockPriceB[pos[2]]/stockPriceA[pos[1]])
  }
    return(profit)
}
    
    

# Function 8
getProfit.K = function (x, y, m = mean (x/y), s = sd (x/y), k = 1) {
  positions = getPositions(x/y, k = k, m = m, s = s)
  if (length (positions) == 0) {return (NA)}
  profit = 0
  for (i in 1:length(positions)) {
    profit = profit + positionProfit(positions[[i]], x, y, m = m)
  }
  return(profit)
}

  


# Function 9

getBest.K = 
  function(x, y, k.max = max((max(x/y)-m)/s,(m-min(x/y)/s)), 
                 k.min = 0.1, m = mean(x/y), s = sd(x/y), numK= 20){
    if(k.min < 0.1) k.min = 0.1
      ratio = x/y
      
     # test = (k.max - k.min)/(numK-1)
      real.k.max = max((max(x/y)-m)/s, (m-min(x/y))/s)
    
     ks = seq(k.min, real.k.max, length = numK )
     allprofit = vector("numeric", length = length(ks))
    
    for (i in 1:length(ks)) {
        allprofit[i] = getProfit.K(x, y, k = ks[i], m =m ,s=s)
    }
     
      index = which(allprofit == max(allprofit))
      return(median(ks[index]))
  }
    
  

# function 10
stockSim = function (n = 4000, rho = 0.99, psi =0, sigma = rep (1, 2),
                     beta0 = rep (100, 2), beta1 = rep (0, 2),
                     epsilon = matrix (rnorm (2*n, sd = sigma),
                                       nrow = n, byrow = TRUE)) {
  a1 = numeric (length = n)
  a2 = numeric (length = n)
  a0 = rnorm (n = 1, sd = sigma, mean = 0)
  a1[1] = rho * a0 + psi * (1-rho) * a0 + epsilon[,1][1]
  a2[1] = rho * a0 + psi * (1-rho) * a0 + epsilon[,2][1]
  
  for (t in 2:n){
    a1[t] = rho * a1[t-1] + psi * (1-rho) * a2[t-1] + epsilon[,1][t]
    a2[t] = rho * a2[t-1] + psi * (1-rho) * a1[t-1] + epsilon[,2][t]
  }
  
  b1 = numeric (length = n)
  b2 = numeric (length = n)
  i = (1:n)
  b1 [i] = beta0 + beta1 * i +a1[i]
  b2 [i] = beta0 + beta1 * i +a2[i]
  return (matrix (c(b1, b2), nrow = length (b1), ncol =2, byrow = FALSE))
}


##function 11
runSim = function(rho, psi, beta0 = c(100, 100), beta1 = c(.0003, .0003),
                  sigma = c(1, 1), n = 4000) {
  stock = stockSim(n = n, rho = rho, psi = psi, beta0 = beta0, beta1 = beta1, sigma = sigma)
  stockA = stock[1:(n/2),1]
  stockB = stock[1:(n/2),2]
  test = stock[(n/2 + 1):n, ]
  k.max = max(abs(stockA/stockB -mean(stockA/stockB)))/sd(stockA/stockB)
  k.min = min(abs(stockA/stockB -mean(stockA/stockB)))/sd(stockA/stockB)#Get k.min and k.max
  k.min = max(k.min, 0.1)
  m1 = mean(stockA/stockB)
  s1 = sd(stockA/stockB)
  best.k = getBest.K(x = stockA, y = stockB, k.min = k.min, k.max =k.max, numK = 30, m = m1 ,s = s1)
  profit = getProfit.K(test[,1], test[,2], k = best.k)
  return(profit)
}




##function 12
simProfitDist = function (..., B = 50) sapply (1:B, function (i, ...)
  runSim (...), ...)

