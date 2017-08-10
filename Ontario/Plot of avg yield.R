setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis")

Yield <- readRDS("Data needed to complete empirical work/Yield Data/Ontario/Yield_Ontario")

yield <- Yield[,3]

time <- c(1950:2013)

avg_yield <- NULL
for(i in 1:length(time))
{avg_yield[i] <- mean(Yield[Yield$year %in% time[i],3])}


SO <- c("brant","chatham-kent","elgin","essex","haldimand-norfolk","lambton","middlesex","oxford")

Yield_SO <- Yield[Yield$county %in% SO,]

yield_SO <- Yield_SO[,3]

avg_yield_SO <- NULL
for(i in 1:length(time))
{avg_yield_SO[i] <- mean(Yield_SO[Yield_SO$year %in% time[i],3])}


ON <- c("brant","bruce","chatham-kent","elgin","essex","haldimand-norfolk","huron","lambton","middlesex","ottawa","oxford","peel","perth","prescott-russell","simcoe","stormont-dundas-glengarry","waterloo","wellington","york")

Yield_ON <- Yield[Yield$county %in% ON,]

yield_ON <- Yield_ON[,3]

avg_yield_ON <- NULL
for(i in 1:length(time))
{avg_yield_ON[i] <- mean(Yield_ON[Yield_ON$year %in% time[i],3])}


WS <- c("huron","middlesex","ottawa","peel","perth","prescott-russell","stormont-dundas-glengarry","york")

Yield_WS <- Yield[Yield$county %in% WS,]

yield_WS <- Yield_WS[,3]

avg_yield_WS <- NULL
for(i in 1:length(time))
{avg_yield_WS[i] <- mean(Yield_WS[Yield_WS$year %in% time[i],3])}


LOW <- c("dufferin","grey","halton","hamilton","hastings","kawartha lakes","lanark","leeds-grenville","lennox-addington","niagara","northumberland","prince edward","renfrew")

Yield_LOW <- Yield[Yield$county %in% LOW,]

yield_low <- Yield_LOW[,3]

avg_yield_LOW <- NULL
for(i in 1:length(time))
{avg_yield_LOW[i] <- mean(Yield_LOW[Yield_LOW$year %in% time[i],3])}

install.packages("colorspace")
library(colorspace)


plot(time,avg_yield,col="blue",type="o", xlab="Year",ylab= "Avg. County Level Yield",main="Average Yield for Subsets of Ontario")
lines(time,avg_yield_ON,col="red")
lines(time, avg_yield_LOW,col="cyan")
lines(time, avg_yield_SO,col="green3")
leg <-c("All Counties","Corn Producers","Low Producers","Southern Ontario")
col <- c("blue","red","cyan","green3")
legend("topleft",legend=leg, fill = col) 

X <- cbind(avg_yield,avg_yield_ON,avg_yield_LOW,avg_yield_SO)
write.table(X,"Code/Ontario/Yields")

X <- data.frame(X)

X <- data.frame(avg_yield,avg_yield_ON,avg_yield_LOW,avg_yield_SO)
