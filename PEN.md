# Volatility of PEN
Dennis Murray  
July 14, 2016  



This document will calculate and review the volatility of the stock for PEN, Penumbra Inc., a company in the "design, development, manufacture and marketing of innovative medical devices" (Marketwatch).  It is headquartered in Alameda, CA.


```r
library(tseries)
MyStockQuote<-get.hist.quote("PEN",quote = "Close")
```

```
## Warning in download.file(url, destfile, method = method, quiet = quiet):
## downloaded length 12495 != reported length 200
```

```
## time series starts 2015-09-21
```


```r
plot(MyStockQuote)
```

![](PEN_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

A basic plot of the quote shows a substantial range in price, dropping as low $18.00 and peaking at $137.51.  However, history is very short - with the first date in the time series occurring in late 2015.

We can calculate Log Returns, and Volatility for the stock over time.


```r
MyReturn<-log(lag(MyStockQuote))-log(MyStockQuote)
MyVolatility<-sd(MyReturn)*sqrt(250)*100
plot(MyReturn)
```

![](PEN_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
MyVolatility
```

```
## [1] 48.51677
```

On an annual basis, Volatility for Penumbra is 48.5 Daily log returns range from (-0.077, 0.212).  There are several significant days of negative returns of beyond -0.05, as well as more than 0.20.

#Calculate volatility with 3 different decay measures


```r
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
```


```r
#Plot the results
plot(volest1, type = "l")
lines(volest2, type = "l", col = "red")
lines(volest3, type = "l", col = "blue")
```

![](PEN_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The longest decay, 100 days, is shown in blue. 
The shortest decay, 10 days, is shown in black.
The middle decay, 30 days, is shown in red.

The stock's initial period of availability shows the three decays synchronized, until enough back history was created to allow for floating of each measure.  The Short decay, 10 days, shows an immediate jump when the stock sees it's large jump.  Both of the other time measures followed, but neither to the degree.

Overall, the short decay shows more sensitivity to change.  The longest decay measure shows a general trend while the short decay show individual daily changes.
