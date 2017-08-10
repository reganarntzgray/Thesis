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
#we will use the planting date model we came up with to get planting date futured on simulated data, given year 2006#
###futured on weather in the month of April###

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

####Now introducing the simulated data####

setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Iowa")

weather <- read.table("daily_future_sim_weather_full")


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

#we will add in the trend as if all years are 2090 since data is to represent 2080-2099#
#2090 is trend year 136#

const <- rep(1,length(April1))
trend <- rep(136,length(April1))

X <- cbind(const,mean_temp_April,vpd_April,pcp_April,trend)

planting_day <- X%*%Beta_planting

adjust <- seq(0,nrow(weather)-365,365)

planting_row <- planting_day+adjust

silking_row <- NULL

Aug30 <- seq(242,nrow(weather),365)
for(i in 1:length(planting_row))
{accum_gdd_ag <- 0
for(j in planting_row[i]:Aug30[i])
{accum_gdd_ag<- weather$gdd_ag[j]+accum_gdd_ag
if(accum_gdd_ag>=median_gdd_accum)
{silking_row[i] <- j
break}}
}

T <- 100
N <- 99

year <- c(1:T)
Year <- matrix(0,N,length(year))
for(i in 1:length(year))
{Year[,i]<-rep(year[i],N)}
year <- as.vector(Year)

county <- rep(unique(weather$county),T)

plant_silk <- data.frame(year,county,planting_row,silking_row)

write.table(plant_silk,"future_sim_planting_silking")
