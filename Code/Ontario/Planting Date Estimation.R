setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis")

weather <- readRDS("Data needed to complete empirical work/Daily weather data/Ontario/daily_weather_ontario")

counties <- c("brant","bruce","chatham-kent","dufferin","elgin","essex","grey","haldimand-norfolk",
		"halton","hamilton","hastings","huron","kawartha lakes","lambton","lanark",
		"leeds-grenville","lennox-addington","middlesex","niagara","northumberland",
		"ottawa","oxford","peel","perth","prescott-russell","prince edward","renfrew",
		"simcoe","stormont-dundas-glengarry","waterloo","wellington","york")


weather <- weather[weather$county %in% counties,]

attach(weather)

#calculating planting date as the day after 3 consecutive days with temp over 12.8 degrees celcius

April1 <-  seq(91,nrow(weather),365)
June1 <- seq(152,nrow(weather),365)
plant <- NULL
for(i in 1:length(April1))
{for(j in April1[i]:June1[i])
{if(((weather$max[j]+weather$min[j])/2)>=12.8 && ((weather$max[j+1]+weather$min[j+1])/2)>=12.8  && ((weather$max[j+2]+weather$min[j+2])/2)>=12.8)
{plant[i]<- (j+3)
break}
else{plant[i] <- (June1[i]+3)}}}

silk <- NULL

accum_gdd_ag <- 0
Aug30 <- seq(242,nrow(weather),365)
for(i in 1:length(plant))
{accum_gdd_ag <- 0
for(j in plant[i]:Aug30[i])
{accum_gdd_ag<- weather$gdd_ag[j]+accum_gdd_ag
if(accum_gdd_ag>=1400)
{silk[i] <- j
break}
else{silk[i] <- Aug30[i]}}}

year <- unique(year)
Year <- matrix(0,length(unique(county)),length(year))
for (i in 1:length(year))
{Year[,i]<-rep(year[i],length(unique(county)))}
year <- as.vector(Year)

county <- rep(unique(county),length(unique(year)))

planting_row_estimate <- plant
planting_row_estimate <- data.frame(year,county,planting_row_estimate)

silking_row_estimate <- silk
silking_row_estimate <- data.frame(year,county,silking_row_estimate)

saveRDS(planting_row_estimate,"Data needed to complete empirical work/Planting Date data/Ontario/planting_row_estimate")
saveRDS(silking_row_estimate,"Data needed to complete empirical work/Planting Date data/Ontario/silking_row_estimate")

