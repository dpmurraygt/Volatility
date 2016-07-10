library(tseries)

#Download the Data

MyStockQuote<-get.hist.quote("HD",quote = "Close")

#Calculate log return

MyReturn<-log(lag(MyStockQuote)-log(MyStockQuote))

#Calculate volatility

MyVolatility<-sd(MyReturn)*sqrt(250)*100

#Calculate volatility with 3 different decay measures

Vol<- function(d,logrets) {
  var = 0
  lam = 0
  varlist <- c()
  for (r in logrets) {
    lam = lam*(1-1/d) + 1
    var = (1-1/lam)*var+(1/lam)*r^2
    varlist<-c(varlist, var)
  }
  sqrt(varlist)
}

volest1<-Vol(10,MyReturn)
volest2<-Vol(30,MyReturn)
volest3<-Vol(100,MyReturn)

#Plot the results
plot(volest1, type = "l")
lines(volest2, type = "l", col = "red")
lines(volest3, type = "l", col = "blue")

