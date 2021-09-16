library("quantmod") 
sp500  <-  new.env()
getSymbols("^GSPC",  env  =  sp500,  src  =  "yahoo", from  =  as.Date("1960-01-04"),  to  =  as.Date("2009-01-01"))

GSPC <- sp500$GSPC
GSPC1 <- get("GSPC", envir = sp500)
GSPC2 <- with(sp500, GSPC)

rm(GSPC1) 
rm(GSPC2)

head(GSPC)
class(GSPC)
dim(GSPC)
head(GSPC$GSPC.Volume)
GSPC["1970-03"]
GSPC["/1960-01-06"]
GSPC["2008-12-25/"]
head(Cl(GSPC))
head(OpCl(GSPC))

head(ClCl(GSPC))

nextfri <- function(x) 7 * ceiling(as.numeric(x - 5 + 4)/7) + as.Date(5 - 4)
SP.we <- aggregate(GSPC, nextfri, tail, 1)
SP.we <- xts(aggregate(GSPC, nextfri, tail, 1))
SPC.we <- Cl(SP.we)
plot(SPC.we)

lr <- diff(log(SPC.we))
plot(lr)

# Accessing http://www.nasdaq.com/quotes/nasdaq-100-stocks.aspx?render=download allows to
# download a .csv le including company symbol and name (note that there are more than 100 entries, as
#                                                       some companies appear with 2 symbols):

nasdaq100 <- read.csv("nasdaq100list.csv", stringsAsFactors = FALSE, strip.white = TRUE)
dim(nasdaq100)
names(nasdaq100)
nasdaq100$Name[duplicated(nasdaq100$Name)]
nasdaq <- new.env()
for(i in nasdaq100$Symbol[startsWith(nasdaq100$Symbol, "A")]) {
  cat("Downloading time series for symbol '", i, "' ...\n", sep = "")
  status <- tryCatch(getSymbols(i, env = nasdaq, src = "yahoo", from = as.Date("2000-01-01")), error = identity)
  if(inherits(status, "error"))
    cat("Symbol '", i, "' not downloadable!\n", sep = "")
}

with(nasdaq, head(AAPL))
chartSeries(nasdaq$AAPL)

# Look at the quantmod homepage for further examples 
# http://www.quantmod.com/examples/intro/.

# Backup data files
# att <- read.table(file="ATT.CSV", header=TRUE, sep=",")
# verizon <- read.table(file="VERIZON.CSV", header=TRUE, sep=",")

getSymbols("T",src="yahoo", from  =  as.Date("2002-01-01"))
getSymbols("VZ",src="yahoo", from  =  as.Date("2001-01-01"))

S1 = T
S2 = VZ

# getSymbols("MCD",src="yahoo", from  =  as.Date("2002-01-01"))
# getSymbols("DNKN",src="yahoo", from  =  as.Date("2001-01-01"))
# S1 = MCD
# S2 = DNKN

head(S1)
head(S2)
summary(S1)
summary(S2)

stock1 = as.data.frame(S1)
stock1=cbind(datestamp=rownames(stock1),stock1)
rownames(stock1)=c()
head(stock1)
colnames(stock1) <- c("Date","Open","High","Low","Close","Volume","Adj.Close")
summary(stock1)

stock2 = as.data.frame(S2)
stock2=cbind(datestamp=rownames(stock2),stock2)
rownames(stock2)=c()
head(stock2)
colnames(stock2) <- c("Date","Open","High","Low","Close","Volume","Adj.Close")
summary(stock2)

summary(stock1)
stock1$Date = as.Date(stock1$Date)
summary(stock1)
stock2$Date = as.Date(stock2$Date)
summary(stock2)

combine2Stocks = function(a, b, stockNames = c(deparse(substitute(a)), 
                                deparse(substitute(b)))) 
  { 
    rr = intersect(a$Date, b$Date)
    a.sub=a[which(a$Date %in% rr),]
    b.sub=b[which(b$Date %in% rr),]
    structure(data.frame(as.Date(a.sub$Date), 
                         a.sub$Adj.Close, 
                         b.sub$Adj.Close),
              names = c("Date", stockNames)) 
  }

overlap = combine2Stocks(stock1, stock2)

head(overlap)
names(overlap)

overlap=overlap[order(overlap$Date),]
head(overlap)



range(overlap$Date)


range(stock1$Date)

range(stock2$Date)


r = overlap$stock1/overlap$stock2

plotRatio = function(r, k = 1, date = seq(along = r), ...)
  {
    plot(date, r, type = "l", ...)
    abline(h = c(mean(r), 
                 mean(r) + k * sd(r), 
                 mean(r) - k * sd(r)), 
           col = c("darkgreen", rep("red", 2*length(k))), 
           lty = "dashed")
  }


plotRatio(r, k=0.85, overlap$Date, col = "lightgray", xlab = "Date", ylab = "Ratio")

# e.g.,  findNextPosition(r)
#        findNextPosition(r, 1174)
# Check they are increasing and correctly offset
findNextPosition = function(ratio, startDay = 1, k = 1, 
           m = mean(ratio), s = sd(ratio))
  {
    up = m + k *s
    down = m - k *s
    
    if(startDay > 1)
      ratio = ratio[ - (1:(startDay-1)) ]
    
    isExtreme = ratio >= up | ratio <= down
    
    if(!any(isExtreme))
      return(integer())
    
    start = which(isExtreme)[1]
    backToNormal = if(ratio[start] > up)
      ratio[ - (1:start) ] <= m
    else
      ratio[ - (1:start) ] >= m 
    
    # return either the end of the position or the index 
    # of the end of the vector.
    # Could return NA for not ended, i.e. which(backToNormal)[1]
    # for both cases. But then the caller has to interpret that.
    
    end = if(any(backToNormal))
      which(backToNormal)[1] + start
    else
      length(ratio)
    
    c(start, end) + startDay - 1 
  }

k = .85
a = findNextPosition(r, k = k)
a

b = findNextPosition(r, a[2], k = k)
b

c = findNextPosition(r, b[2], k = k)
c

symbols(overlap$Date[a[1]],r[a[1]],circles = 60,fg = "darkgreen", add=TRUE, inches = FALSE)
symbols(overlap$Date[a[2]],r[a[2]],circles = 60,fg = "red", add=TRUE, inches = FALSE)

showPosition = function(days, ratios, radius = 100)
  {
    symbols(days, ratios, circles = rep(radius, 2), 
            fg = c("darkgreen", "red"), add = TRUE, inches = FALSE)
  }

showPosition(overlap$Date[a],r[a])
showPosition(overlap$Date[b],r[b])
showPosition(overlap$Date[c],r[c])

getPositions = function(ratio, k = 1, m = mean(ratio), s = sd(ratio))
  {
    when = list()
    cur = 1
    
    while(cur < length(ratio)) {
      tmp = findNextPosition(ratio, cur, k, m, s)
      if(length(tmp) == 0)  # done
        break
      when[[length(when) + 1]] = tmp
      if(is.na(tmp[2]) || tmp[2] == length(ratio))
        break
      cur = tmp[2]
    }
    
    when
  }

pos=getPositions(r,k)
plotRatio(r, k, overlap$Date, xlab = "Date", ylab = "Ratio")
invisible(lapply(pos, function(p) showPosition(overlap$Date[p],r[p])))

showPosition = function(days, ratio, radius = 30)
  {
    if(is.list(days))
      days = unlist(days)
    
    symbols(days, ratio[days], 
            circles = rep(radius, length(days)), 
            fg = c("darkgreen", "red"),
            add = TRUE, inches = FALSE)
  }

k = 0.85
pos=getPositions(r,k)
plotRatio(r, k, col = "lightgray", xlab = "Date", ylab = "Ratio")
showPosition(pos,r)

#  r = overlap$att/overlap$verizon
#  k = 1.7
#  pos = getPositions(r, k)
#  positionProfit(pos[[1]], overlap$att, overlap$verizon)
positionProfit = function(pos, stockPriceA, stockPriceB, 
           ratioMean = mean(stockPriceA/stockPriceB), 
           p = .001, byStock = FALSE)
  {
    if(is.list(pos)) {
      ans = sapply(pos, positionProfit, 
                   stockPriceA, stockPriceB, ratioMean, p, byStock)
      if(byStock)
        rownames(ans) = c("A", "B", "commission")
      return(ans)
    }
    # prices at the start and end of the positions
    priceA = stockPriceA[pos]
    priceB = stockPriceB[pos]
    
    # how many units can we by of A and B with $1
    unitsOfA = 1/priceA[1]
    unitsOfB = 1/priceB[1]
    
    # The dollar amount of how many units we would buy of A and B
    # at the cost at the end of the position of each.
    amt = c(unitsOfA * priceA[2], unitsOfB * priceB[2])
    
    # Which stock are we selling
    sellWhat = if(priceA[1]/priceB[1] > ratioMean) "A" else "B"
    
    profit = if(sellWhat == "A") 
      c((1 - amt[1]),  (amt[2] - 1), - p * sum(amt))
    else 
      c( (1 - amt[2]),  (amt[1] - 1),  - p * sum(amt))
    
    if(byStock)
      profit
    else
      sum(profit)
  }

pf = positionProfit(c(1, 2), c(3838.48, 8712.87), 
                    c(459.11, 1100.65), p = 0)
pf
round(pf,2)

tail(overlap)

prof = positionProfit(pos, overlap$stock1, overlap$stock2, mean(r))
length(pos)
prof

summary(prof)


i = 1:floor(nrow(overlap)*2/3)
train = overlap[i, ]
test = overlap[ - i, ]

r.train = train$stock1/train$stock2
r.test = test$stock1/test$stock2

k.max = max((r.train - mean(r.train))/sd(r.train)) #+5

k.min = min((abs(r.train - mean(r.train))/sd(r.train)))

ks = seq(k.min, k.max, length = 100)
m  = mean(r.train)

profits = sapply(ks,function(k) {
           pos = getPositions(r.train, k)
           if(length(pos)>0){
             sum(positionProfit(pos, train$stock1, train$stock2, 
                                mean(r.train)))
           }
           else{
             return(0)
             
           }
         })

plot(ks, profits, type = "l", xlab = "k", ylab = "Profit")

ks[  profits == max(profits) ]  


tmp.k = ks[  profits == max(profits) ]  
pos = getPositions(r.train, tmp.k[1])
all(sapply(tmp.k[-1], function(k) identical(pos, getPositions(r.train, k))))

k.star = mean(ks[  profits == max(profits) ]  )

pos = getPositions(r.test, k.star, mean(r.train), sd(r.train))
testProfit = sum(positionProfit(pos, test$stock1, test$stock2)) 
testProfit

#Simulation Study
stockSim = function(n = 4000, rho = 0.99, psi = 0, sigma = rep(1, 2),
           beta0 = rep(100, 2), beta1 = rep(0, 2),
           epsilon = matrix(rnorm(2*n, sd = sigma),
                            nrow = n, byrow = TRUE))
  {
    X = matrix(0, nrow = n, ncol = 2)
    X[1,] = epsilon[1,]
    
    A = matrix(c(rho, psi*(1-rho), psi*(1-rho), rho), nrow = 2)
    for(i in 2:n)
      X[i,] = A %*% X[i-1,] + epsilon[i,]
    
    # Add in the trends, in place
    X[,1] = beta0[1] + beta1[1] * (1:n) + X[,1]
    X[,2] = beta0[2] + beta1[2] * (1:n) + X[,2]
    
    X
  }

set.seed(12312)

a = stockSim(n=4000,rho = .99, psi = 0)

matplot(a,type="l",xlab="Day",ylab="X,Y",col=c("black","grey"),lty="solid")

a = stockSim(beta1 = c(.05, .1))

matplot(a,type="l",xlab="Day",ylab="X,Y",col=c("black","grey"),lty="solid")

runSim = function(rho, psi, beta0 = c(100, 100), beta1 = c(0, 0),
           sigma = c(1, 1), n = 4000)
  {
    X = stockSim(n, rho, psi, sigma, beta = beta0, beta1 = beta1)
    train = X[ 1:floor(n/2), ]
    test = X[ (floor(n/2)+1):n, ]
    m = mean(train[, 1]/train[, 2])
    s = sd(train[, 1]/train[, 2])
    k.star = getBestK(train[, 1], train[, 2], m = m, s = s)
    getProfit.K(k.star, test[, 1], test[, 2], m, s)
  }

getProfit.K = function(k, x, y, m = mean(x/y), s = sd(x/y))
  {
    pos = getPositions(x/y, k, m = m, s = s)
    if(length(pos) == 0)  
      0
    else
      sum(positionProfit(pos, x, y, m))
  }

getBestK = function(x, y, ks = seq(0.1, max.k, length = N), N = 100, 
           max.k = NA, m = mean(x/y), s = sd(x/y))
  {
    if(is.na(max.k)) {
      r = x/y
      max.k = max(r/sd(r))
    }
    
    pr.k = sapply(ks, getProfit.K, x, y, m = m, s = s)
    median(ks[ pr.k == max(pr.k) ])
  }

simProfitDist = function(..., B = 999) sapply(1:B,  function(i, ...) runSim(...), ...)

set.seed(1223)

system.time({ x = simProfitDist( .99, .9, c(0, 0)) })

summary(x)

hist(x)

sum(x < 0)/length(x)

g = expand.grid(psi = seq(.8, .99, length = 20),  
                beta1 = seq(-.01, .01, length = 20),  
                beta2 = seq(-.01, .01, length = 20))
g

Rprof("sim.prof")
system.time({x = simProfitDist( .99, .9, c(0, 0))})
Rprof(NULL)
head(summaryRprof("sim.prof")$by.self)





