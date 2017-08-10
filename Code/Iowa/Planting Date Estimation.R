setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis")

weather <- readRDS("Data needed to complete empirical work/Daily weather data/Iowa/daily_weather_Iowa")

planting <- read.delim(file="Data needed to complete empirical work/Planting Date data/Iowa/county_planting_dates_iowa.txt",header=T,sep="\t")
silking <- read.delim(file="Data needed to complete empirical work/Planting Date data/Iowa/county_silking_dates_iowa.txt",header=T,sep="\t")

predict <- unique(planting$year)
weather_predict <-  weather[weather$year %in% predict,]

adjust <-seq(0,nrow(weather_predict)-365,365)

planting_row <- planting[,3]+adjust
silking_row <- silking[,3]+adjust

planting_day <- planting[,3]
silking_day <- silking[,3]

gdd_ag_accum <- NULL
for (i in 1:length(planting_row))
{gdd_ag_accum[i]<-sum(weather_predict$gdd_ag[planting_row[i]:silking_row[i]])}

median_gdd_accum <- summary(gdd_ag_accum)[3]

#####Modelling Planting Dates#####

###based on weather in the month of April###

April1 <- seq(91,nrow(weather_predict),365)
April30 <- seq(120,nrow(weather_predict),365)

mean_temp_daily <- (weather_predict$min+weather_predict$max)/2

mean_temp_April <- NULL
for(i in 1:length(April1))
{mean_temp_April[i]<-mean(mean_temp_daily[April1[i]:April30[i]])}

pcp_April <- NULL
for(i in 1:length(April1))
{pcp_April[i]<-mean(weather_predict$pcp[April1[i]:April30[i]])}

vpd_April <- NULL
for(i in 1:length(April1))
{vpd_April[i]<-mean(weather_predict$vpd[April1[i]:April30[i]])}

#we will add in the trend as it would be if all years were included#
#1974 - 20, 1986 - 32, 1988 - 34, 1991 - 37, 1993 - 39, 2012 - 58)

trend <- c(20:32,34:37,39:58)
Time <- matrix(0,99,length(trend))
for (i in 1:length(trend))
{Time[,i] <- rep(trend[i],99)}
trend <- as.vector(Time)

mod <- lm(planting_day ~ mean_temp_April+vpd_April+pcp_April+trend)

Beta_planting <- summary(mod)$coeff[,1]

#formatting table for latex using stargazer package#

install.packages("stargazer")
library(stargazer)
stargazer(mod, title="Planting Date Model Results", align=TRUE)

#####Creating the estimated planting dates from our model#####

###Creating the April weather data for all years in study###

April1 <- seq(91,nrow(weather),365)
April30 <- seq(120,nrow(weather),365)

mean_temp_daily <- (weather$min+weather$max)/2

mean_temp_April <- NULL
for(i in 1:length(April1))
{mean_temp_April[i]<-mean(mean_temp_daily[April1[i]:April30[i]])}

pcp_April <- NULL
for(i in 1:length(April1))
{pcp_April[i]<-mean(weather$pcp[April1[i]:April30[i]])}

vpd_April <- NULL
for(i in 1:length(April1))
{vpd_April[i]<-mean(weather$vpd[April1[i]:April30[i]])}

trend <- c(1:58)
Time <- matrix(0,99,length(trend))
for(i in 1:length(trend))
{Time[,i] <- rep(trend[i],99)}
trend <- as.vector(Time)

const <- rep(1,length(trend))

X <- cbind(const,mean_temp_April,vpd_April,pcp_April,trend)

planting_day_estimate <- X%*%Beta_planting

adjust <- seq(0,nrow(weather)-365,365)

planting_row_estimate <- planting_day_estimate+adjust

county <- rep(unique(weather$county),58)

year <- c(1955:2012)
Year <- matrix(0,99,length(year))
for(i in 1:length(year))
{Year[,i]<-rep(year[i],99)}
year <- as.vector(Year)

planting_day_estimate <- data.frame(cbind(year,county,planting_day_estimate))

###creating the estimated silking dates based on the planting dates###

#the silking dates will be the point at which the median gdd_ag accumulation occurs after planting date#

silking_row_estimate <- NULL

Aug30 <- seq(242,nrow(weather),365)
for(i in 1:length(planting_row_estimate))
{accum_gdd_ag <- 0
for(j in planting_row_estimate[i]:Aug30[i])
{accum_gdd_ag<- weather$gdd_ag[j]+accum_gdd_ag
if(accum_gdd_ag>=median_gdd_accum)
{silking_row_estimate[i] <- j
break}}
}

silking_day_estimate <- silking_row_estimate-adjust

silking_row_estimate <- data.frame(year,county,silking_row_estimate)
silking_day_estimate <- data.frame(year,county,silking_day_estimate)

#testing the level of error in the silking date estimates using known values

silk_test <- silking_day_estimate[silking_day_estimate$year %in% predict,]

silk_predict <- silk_test[,3]

mod <- lm(silking_day ~ 0+silk_predict)

#Residual standard error of 5.412 when using the predicted planting days to guess the known silking days#

###now we want to replace the known years with the values we have instead of keeping them as the predicted values###

new <- planting_day_estimate[planting_day_estimate$year %in% predict,]

rownumberknown <- row.names(new)

for (i in 1:length(rownumberknown))
{planting_day_estimate[rownumberknown[i],] <- planting[i,]}

for(i in 1:length(rownumberknown))
{silking_day_estimate[rownumberknown[i],] <- silking[i,]}

adjust <- seq(0,nrow(weather)-365,365)
planting_row_estimate <- planting_day_estimate[,3]+adjust
planting_row_estimate <- data.frame(year,county,planting_row_estimate)

silking_row_estimate <- silking_day_estimate[,3]+adjust
silking_row_estimate <- data.frame(year,county,silking_row_estimate)

saveRDS(planting_row_estimate,"Data needed to complete empirical work/Planting Date data/Iowa/planting_row_estimate")
saveRDS(silking_row_estimate,"Data needed to complete empirical work/Planting Date data/Iowa/silking_row_estimate")

