
library(timeSeries)
library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)


options(digits = 5)
######################STEP ONE: Create Returns Time Series#########################################
 
#Create Vector of Tickers
tickers <- c("OLED", "AMD", "TEAM", "ETSY", "ATVI",'TTWO','NVDA')

#Calculate Returns: Daily
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(Ticker, from="2017-01-01",to="2018-08-07", auto.assign=FALSE)[,4])
 
#Delete all dates with no prices
portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(portfolioPrices) <- tickers

#Calculate Returns: Daily RoC
portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
portfolioReturns <- as.timeSeries(portfolioReturns)

#Calculate Monthly or Weekly Returns
Stock_Data <- tickers %>% lapply(function(x) getSymbols(x, from="2017-01-01",to="2018-08-07", auto.assign=FALSE)[,4]) %>%
  lapply(function(x) monthlyReturn(x))

portfolioReturns <- do.call(merge, Stock_Data)
# keep only the dates that have closing prices for all tickers
portfolioReturns <- portfolioReturns[apply(portfolioReturns,1,function(x) all(!is.na(x))),]
colnames(portfolioReturns) <- tickers
portfolioReturns <- as.timeSeries(portfolioReturns)


#################STEP TWO: Calculate and Plot Frontier and Efficient Portfolios##############
# calculate the efficient frontier
effFrontier <- portfolioFrontier(portfolioReturns, constraints = "LongOnly")
 
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

plot(effFrontier,c(1,2,3,4))

#Plot Frontier Weights (Can Adjust Number of Points)
frontierWeights <- getWeights(effFrontier) # get allocations for each instrument for each point on the efficient frontier
colnames(frontierWeights) <- tickers
risk_return <- frontierPoints(effFrontier)
write.csv(risk_return, "risk_return.csv")

#Output Correlation
cor_matrix <- cor(portfolioReturns)
cov_matrix <- cov(portfolioReturns)
write.csv(cov_matrix, "covmatrix.csv")

cov_matrix
#Annualize Data
riskReturnPoints <- frontierPoints(effFrontier) # get risk and return values for points on the efficient frontier
annualizedPoints <- data.frame(targetRisk=riskReturnPoints[, "targetRisk"] * sqrt(252),
                               targetReturn=riskReturnPoints[,"targetReturn"] * 252)
plot(annualizedPoints)

# plot Sharpe ratios for each point on the efficient frontier
riskFreeRate <- 0
plot((annualizedPoints[,"targetReturn"]-riskFreeRate) / annualizedPoints[,"targetRisk"], xlab="point on efficient frontier", ylab="Sharpe ratio")

#Plot Frontier Weights (Need to transpose matrix first)
barplot(t(frontierWeights), main="Frontier Weights", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(frontierWeights))


#Get Minimum Variance Port, Tangency Port, etc.
mvp <- minvariancePortfolio(portfolioReturns, spec=portfolioSpec(), constraints="LongOnly")
mvp
tangencyPort <- tangencyPortfolio(portfolioReturns, spec=portfolioSpec(), constraints="LongOnly")
tangencyPort

mvpweights <- getWeights(mvp)
tangencyweights <- getWeights(tangencyPort)

#Extract value at risk
covRisk(portfolioReturns, mvpweights)
varRisk(portfolioReturns, mvpweights, alpha = 0.05)
cvarRisk(portfolioReturns, mvpweights, alpha = 0.05)

#Plot MVP Weights: Basic Graphs
barplot(mvpweights, main="Minimum Variance Portfolio Weights", xlab="Assset", ylab="Weight In Portfolio (%)", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(weights))
pie(mvpweights, col=cm.colors(ncol(frontierWeights)+2))

#ggplot MVP Weights
df <- data.frame(mvpweights)
assets <- colnames(frontierWeights)
ggplot(data=df, aes(x=assets, y=mvpweights, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",mvpweights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Minimum Variance Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")

dft <- data.frame(tangencyweights)
assets <- colnames(frontierWeights)
ggplot(data=dft, aes(x=assets, y=tangencyweights, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",tangencyweights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Tangency Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")

 
