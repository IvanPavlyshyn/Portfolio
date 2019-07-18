library(timeSeries)
library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(plyr)
library(PerformanceAnalytics)
library(ggplot2)
library(reshape)
 


######################STEP ONE: Create Returns Time Series#########################################
options(stringsAsFactors = F)
spy_list<-read.csv('spy_list.csv')
spy_list<-subset(spy_list,! Ticker  %in% c('RDS-A','СCI'))
#Calculate Returns: Daily
portfolioPrices <- NULL
for (Ticker in spy_list$Ticker)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(Ticker, from="2010-01-01",to="2018-01-01", auto.assign=FALSE)[,4])




colnames(portfolioPrices)<-spy_list$Ticker

simul<-as.data.frame(matrix(NA, nrow = 30000, ncol=19))
names(simul)<-c('as1','as2','as3','as4','as5','as6','as7','as8','as9','as10','as11','as12','as13','as14','as15','MVreturn','MVrisk','TGreturn','TGrisk')

t1<-Sys.time()
set.seed(77)
for( i in 1:30000)
{
  
  
 
  ticker_list<-spy_list[sample(seq(1,25,by=1),size=sample(seq(7,15,1),size=1)),'Ticker']
  simul[i,c(1:length(ticker_list))]<-ticker_list
  
  portfolioPrices_n<-portfolioPrices[,ticker_list]
  portfolioPrices_n<-na.omit(portfolioPrices_n)
  
  #Calculate Monthly or Weekly Returns
  Stock_Data <- portfolioPrices_n %>% lapply(function(x) monthlyReturn(x))
  
  portfolioReturns <- do.call(merge, Stock_Data)
  # keep only the dates that have closing prices for all tickers
  portfolioReturns <- portfolioReturns[apply(portfolioReturns,1,function(x) all(!is.na(x))),]
  colnames(portfolioReturns) <- ticker_list
  portfolioReturns <- as.timeSeries(portfolioReturns)
  
  scenarios <-dim(portfolioReturns)[1]
  assets <- dim(portfolioReturns)[2]
  
  spec <- portfolioSpec()
  setSolver(spec) <- "solveRquadprog"
  setNFrontierPoints(spec) <-dim(portfolioReturns)[2]
  constraints <- c('minW[1:assets]=0', 'maxW[1:assets]=0.3')
  portfolioConstraints(portfolioReturns, spec, constraints)
  
  # calculate the efficient frontier
  effFrontier <- portfolioFrontier(portfolioReturns, constraints = constraints)
  
  # plot frontier
  #'Options
  #'1: Plot Efficient Frontier
  #'2: Plot Minimum Variance Portfolio
  #'3: Plot Tangency Portfolio
  #'4: Plot Risk Returns of Each Asset
  #'5: Plot Equal Weights Portfolio
  #'6: Plot Two Asset Frontiers (Long)
  #'7: Plot Monte Carlo Portfolios
  #'8: Plot Sharpe Ratio
  #plot(effFrontier,c(1,2,3,4))
  
  #Plot Frontier Weights (Can Adjust Number of Points)
  frontierWeights <-getWeights(effFrontier) # get allocations for each instrument for each point on the efficient frontier
  colnames(frontierWeights) <-ticker_list 
  risk_return <- frontierPoints(effFrontier)
  
 
  
  #Get Minimum Variance Port, Tangency Port, etc.
  mvp <- minvariancePortfolio(portfolioReturns, spec=portfolioSpec(), constraints=constraints)
  
  simul[i,"MVreturn"]<-mvp@portfolio@portfolio$targetReturn[[1]]
  simul[i,"MVrisk"]<-mvp@portfolio@portfolio$targetRisk[[1]]
  
  tangencyPort <- tangencyPortfolio(portfolioReturns, spec=portfolioSpec(), constraints=constraints)
  
  simul[i,"TGreturn"]<-tangencyPort@portfolio@portfolio$targetReturn[[1]]
  simul[i,"TGrisk"]<-tangencyPort@portfolio@portfolio$targetRisk[[1]]
 
  
}

t2<-Sys.time()
t2-t1


performance<-function(simul_data) {
  
  simul<-simul_data
  simul$check<-ifelse(simul$TGreturn>simul$TGrisk,1,0)
  simul$MVreturn<-as.numeric(simul$MVreturn)
  simul$MVrisk<-as.numeric(simul$MVrisk)
  simul$TGreturn<-as.numeric(simul$TGreturn)
  simul$R_P_tg<-simul$TGreturn/simul$TGrisk
  simul$R_P_mv<-simul$MVreturn/simul$MVrisk
  
  simul<-simul[order(simul$R_P_tg,decreasing = T),]
  ncol<-sum(head(!is.na(simul[1:15]),1))
  best_portf_tg<- unlist(simul[1,1:ncol])
  
  simul<-simul[order(simul$R_P_mv,decreasing = T),]
  ncol<-sum(head(!is.na(simul[1:15]),1))
  best_portf_mv<- unlist(simul[1,1:ncol])
  
  
  portfolioPrices_n<-portfolioPrices[,best_portf_tg]
  portfolioPrices_n<-na.omit(portfolioPrices_n)
  
  Stock_Data <- portfolioPrices_n %>% lapply(function(x) monthlyReturn(x))
  portfolioReturns <- do.call(merge, Stock_Data)
  portfolioReturns <- portfolioReturns[apply(portfolioReturns,1,function(x) all(!is.na(x))),]
  colnames(portfolioReturns) <- best_portf_tg
  portfolioReturns <- as.timeSeries(portfolioReturns)
  scenarios <-dim(portfolioReturns)[1]
  assets <- dim(portfolioReturns)[2]
  effFrontier <- portfolioFrontier(portfolioReturns, constraints = constraints)
  frontierWeights_tg <- getWeights(effFrontier)  
  colnames(frontierWeights_tg) <- best_portf_tg
  risk_return <- frontierPoints(effFrontier)
  tangencyPort <- tangencyPortfolio(portfolioReturns, spec=spec, constraints=constraints)
  
  tailoredFrontierPlot(object=effFrontier)
  
  tg_weigth<-as.data.frame(getWeights(tangencyPort))
  tg_weigth$ticker<-rownames(tg_weigth)
  names(tg_weigth)[1]<-'w_tg'
  
  
  portfolioPrices_n<-portfolioPrices[,best_portf_mv]
  portfolioPrices_n<-na.omit(portfolioPrices_n)
  
  
  Stock_Data <- portfolioPrices_n %>% lapply(function(x) monthlyReturn(x))
  portfolioReturns <- do.call(merge, Stock_Data)
  portfolioReturns <- portfolioReturns[apply(portfolioReturns,1,function(x) all(!is.na(x))),]
  colnames(portfolioReturns) <- best_portf_mv
  portfolioReturns <- as.timeSeries(portfolioReturns)
  scenarios <-dim(portfolioReturns)[1]
  assets <- dim(portfolioReturns)[2]
  effFrontier <- portfolioFrontier(portfolioReturns, constraints = constraints)
  frontierWeights_mv <- getWeights(effFrontier)  
  colnames(frontierWeights_mv) <- best_portf_mv
  risk_return <- frontierPoints(effFrontier)
  mvp <- minvariancePortfolio(portfolioReturns, spec=portfolioSpec(), constraints=constraints)
  
  tailoredFrontierPlot(object=effFrontier)
  
  mv_weigth<-as.data.frame(getWeights(mvp))
  mv_weigth$ticker<-rownames(mv_weigth)
  names(mv_weigth)[1]<-'w_mv'
  
  backtest_prices<-NULL
  for (Ticker in c(spy_list$Ticker,'SPY'))
    backtest_prices <- cbind(backtest_prices,
                             getSymbols(Ticker, from="2018-01-01",to="2019-07-01", auto.assign=FALSE)[,4])
  
  
  Stock_Data_test<- backtest_prices %>% lapply(function(x) weeklyReturn(x))
  backtestReturns <- do.call(merge, Stock_Data_test)
  colnames(backtestReturns) <-  c(spy_list$Ticker,'SPY')
  backtestReturns<-as.data.frame(backtestReturns)
  backtestReturns$date_time<-rownames(backtestReturns)
  
  
  
  backtestReturns<-melt(backtestReturns,id='date_time')
  names(backtestReturns)[2]<-'ticker'
  market<-subset(backtestReturns, ticker=='SPY')
  names(market)[3]<-'spy_return'
  backtestReturns<-subset(backtestReturns,! ticker=='SPY')
  backtestReturns2<-backtestReturns
  backtestReturns<-join(backtestReturns,tg_weigth)
  backtestReturns<-na.omit(backtestReturns)
  backtestReturns$return_tg<-backtestReturns$w_tg*backtestReturns$value
  backtestReturns_port_tg<-ddply(backtestReturns, 'date_time',function(mod)  sum(mod$return_tg))
  names(backtestReturns_port_tg)[2]<-'TG_portf_return'
  
  backtestReturns2<-join(backtestReturns2,mv_weigth)
  backtestReturns2<-na.omit(backtestReturns2)
  backtestReturns2$return_mv<-backtestReturns2$w_mv*backtestReturns2$value
  backtestReturns_port_mv<-ddply(backtestReturns2, 'date_time',function(mod)  sum(mod$return_mv))
  names(backtestReturns_port_mv)[2]<-'MV_portf_return'
  
  market<-join(market,backtestReturns_port_tg)
  market<-join(market,backtestReturns_port_mv)
  
  market$date_time<-as.Date(market$date_time)
  market$spy_return<-cumsum(market$spy_return)
  market$TG_portf_return<-cumsum(market$TG_portf_return)
  market$MV_portf_return<-cumsum(market$MV_portf_return)
  
  results<-as.data.frame(matrix(NA,4,4))
  names(results)<-c('System','SPY','TG','MV') 
  results$System<-c('Return','Sharpe','MaxDD','Volatility')
  results[1,2:4]<-tail(market[,3:5],1)
  
  results[2,2:4]<-cbind(SharpeRatio(xts(market$spy_return,market$date_time)),SharpeRatio(xts(market$TG_portf_return,market$date_time)),SharpeRatio(xts(market$MV_portf_return,market$date_time)))
  results[3,2:4]<-cbind(maxDrawdown(xts(market$spy_return,market$date_time)),maxDrawdown(xts(market$TG_portf_return,market$date_time)),maxDrawdown(xts(market$MV_portf_return,market$date_time)))
  results[4,2:4]<-cbind(sd(xts(market$spy_return,market$date_time)),sd(xts(market$TG_portf_return,market$date_time)),sd(xts(market$MV_portf_return,market$date_time)))
  
  print(results)
  
  
  
  
  ggplot(market, aes(date_time)) + 
    geom_line(aes(y = spy_return, colour = "spy_return")) + 
    geom_line(aes(y = TG_portf_return, colour = "TG_portf_return"))+
    geom_line(aes(y = MV_portf_return, colour = "MV_portf_return"))+
    ggtitle("SPY VS MV/TG portfolio returns")+
    labs(y="Return", x = "Timeline")
  
}

performance(simul)
 

#ggplot MVP Weights
df <- data.frame(getWeights(mvp))
assets <- colnames(frontierWeights_mv)
ggplot(data=df, aes(x=assets, y=getWeights(mvp), fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",getWeights(mvp)*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Minimum Variance Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")


dft <- data.frame(getWeights(tangencyPort))
assets <- colnames(frontierWeights_tg)
ggplot(data=dft, aes(x=assets, y=getWeights(tangencyPort), fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",getWeights(tangencyPort)*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Tangency Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")



