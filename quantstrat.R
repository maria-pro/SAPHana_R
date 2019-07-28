require (quantstrat)
#Step 1. Initialize currency and instruments, and load historic data
#Initialize a currency and a stock instrument
currency("AUD")
stock("BHP.AX",currency="AUD",multiplier=1)
ls(envir=FinancialInstrument:::.instrument)
ls(all=T)

# system settings
initDate <- '2012-12-31'
startDate <- '2013-01-01'
endDate <- '2016-06-30'
initEq <- 1e6

Sys.setenv(TZ="Australia/Melbourne") #Must set timezone
getSymbols('BHP.AX', from=startDate, to=endDate, index.class="POSIXct", adjust=T) #Must use a POSIX time-date index class
BHP.AX=to.monthly(BHP.AX, indexAt='endof', drop.time=FALSE) 
BHP.AX$SMA10m <- SMA(Cl(BHP.AX), 10) #In to.monthly, you must use 'endof' and you must set drop.time=FALSE

#Step 2. Initialize portfolio, account, orders, strategy 
#inz portfolio, account
qs.strategy <- "qsTest"
rm.strat(qs.strategy) # remove strategy etc. if this is a re-run 
initPortf(qs.strategy,'BHP.AX', initDate=initDate)
initAcct(qs.strategy,portfolios=qs.strategy, initDate=initDate, initEq=initEq)

# initialize orders container
args(initOrders)
initOrders(portfolio=qs.strategy,initDate=initDate)

# instantiate a new strategy object
args(strategy)
strategy(qs.strategy,store=TRUE)

ls(all=T)
ls(.blotter)
ls(.strategy)

args(getStrategy)
strat <-getStrategy(qs.strategy)
class(strat)
summary(strat)

#Step 3. Define strategy

args(add.indicator)
# Main arguments: strategy - strategy object, 
#name - name of the indicator (must be an R function), 
#arguments arguments to be passed to the indicator function, label name to reference the indicator
add.indicator(strategy = qs.strategy, name = "BHP.AX",
              arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")
summary(getStrategy(qs.strategy))

#add.signals function
args(add.signal)
#strategy - strategy object,  name - name of the signal, must correspond to an R function
#arguments arguments to be passed to the indicator function


#Add signal for crossing above SMA
add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","SMA10"),relationship="gt"),
           label="Cl.gt.SMA")
#gt, lt, lte and gte stand for greater than, less than, less than or equal to and greater than or equal to
#Add signal for crossing below SMA
add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","SMA10"),relationship="lt"),
           label="Cl.lt.SMA")

summary(getStrategy(qs.strategy))

#The function add.rule adds a rule to a strategy
args(add.rule)

#strategy: strategy object
#name: name of the rule (typically ruleSignal)
#arguments: arguments to be passed to the rule function
#type: type of rule ("risk","order","rebalance","exit","enter")

#ruleSignal is the default rule to generate a trade order on a signal
args(ruleSignal)

#sigcol: column name to check for signal
#sigval: signal value to match
#orderqty: quantity for order or 'all', modified by osFUN
#ordertype: "market","limit","stoplimit","stoptrailing","iceberg"
#orderside: "long", "short", or NULL
#osFUN: function or name of order sizing function (default is osNoOp)


#Add rule to enter when Cl.gt.SMA is true
#Add rule to exit when Cl.lt.SMA is true

# go long when close > MA
add.rule(qs.strategy, name='ruleSignal',
         arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=900,
                          ordertype='market', orderside='long'),
         type='enter')
# exit when close < MA
add.rule(qs.strategy, name='ruleSignal',
         arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all',
                          ordertype='market', orderside='long'),
         type='exit')
summary(getStrategy(qs.strategy))

#Step 4 - processing
args(applyStrategy)
#strategy an object of type 'strategy'
#portfolios a list of portfolios to apply the strategy to
#parameters named list of parameters to be applied during evaluation of the strategy

#Calling applyStrategy generates transactions in the specified portfolio.

applyStrategy(strategy=qs.strategy , portfolios=qs.strategy)
getTxns(Portfolio=qs.strategy, Symbol="BHP.AX")

#mktdata is a special variable constructed during the execution of applyStrategy. 
#It is a time series object which contains the historic price data as well as the calculated indicators, signals, and rules:
  
mktdata["2014"] # inspect the data that was generated

# Update portfolio, account, and equity - Functions must be called in order
updatePortf(qs.strategy)
updateAcct(qs.strategy)
updateEndEq(qs.strategy)

#Data integrity check
checkBlotterUpdate <- function(port.st,account.st,verbose=TRUE)
{
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(sapply(syms,FUN = function(x) eval(parse(
    text=paste("sum(p$symbols",x,"posPL.USD$Net.Trading.PL)",sep="$")))))
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  if( !isTRUE(all.equal(port.tot,port.sum.tot)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match sum of symbols P&L")
  }
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  if( !isTRUE(all.equal(port.tot,endEq-initEq)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match account P&L")
  }
  if( sum(duplicated(index(p$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in portfolio summary")
  }
  if( sum(duplicated(index(a$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in account summary")
  }
  return(ok)
}


checkBlotterUpdate(qs.strategy,qs.strategy)

#Step 6: Reporting

#Plot performance
# create custom theme
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
# plot performance
chart.Posn(qs.strategy, Symbol = 'BHP.AX', Dates = '1998::',theme=myTheme,
           TA='add_SMA(n=10,col=4, on=1, lwd=2)')


#Trade statistics
tstats <- t(tradeStats(qs.strategy))

#The order_book object
#The function getOrderBook can be used to retrieve the order_book object
ob <- getOrderBook(qs.strategy)
class(ob)
## [1] "order_book"
names(ob)
## [1] "qsFaber"
names(ob$qsFaber)
## [1] "SPY"
names(ob$qsFaber$BHP.AX)

#--------------------------------------------------------
#Example with portfolio of several variables

symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"),
           from=startDate, to=endDate, adjust=T)
for(symbol in symbols)
{
  stock(symbol, currency="USD",multiplier=1)
  x<-get(symbol)
  x<-to.monthly(x,indexAt='endof',drop.time=FALSE)
  indexFormat(x)<-'%Y-%m-%d'
  colnames(x)<-gsub("x",symbol,colnames(x))
  assign(symbol,x)
}

multi.asset <- "multiAsset"
rm.strat(multi.asset) # remove strategy etc. if this is a re-run
initPortf(multi.asset,symbols=symbols, initDate=initDate)
initAcct(multi.asset,portfolios=multi.asset, initDate=initDate,
         initEq=initEq)
initOrders(portfolio=multi.asset,initDate=initDate)

#step 3
add.indicator(strategy = qs.strategy, name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")

add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","SMA10"),relationship="gt"),
           label="Cl.gt.SMA")
add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","SMA10"),relationship="lt"),
           label="Cl.lt.SMA")

# go long when close > MA
add.rule(qs.strategy, name='ruleSignal',
         arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=900,
                          ordertype='market', orderside='long'),
         type='enter')
# exit when close < MA
add.rule(qs.strategy, name='ruleSignal',
         arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all',
                          ordertype='market', orderside='long'),
         type='exit')

#step 4

applyStrategy(strategy=qs.strategy , portfolios=multi.asset)
updatePortf(multi.asset)
updateAcct(multi.asset)

a <- getAccount(multi.asset)
p <- getPortfolio(multi.asset)
names(p$symbols)


par(mfrow=c(3,3))
for(symbol in symbols)
{
  chart.Posn(Portfolio=multi.asset,Symbol=symbol,theme=myTheme,
             TA="add_SMA(n=10,col='blue')")
}
par(mfrow=c(1,1))
updateEndEq(multi.asset)
checkBlotterUpdate(multi.asset,multi.asset)

