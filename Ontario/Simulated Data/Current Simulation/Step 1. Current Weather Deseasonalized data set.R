setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis")

N <- 38
T <- 64

weather <- readRDS("Data needed to complete empirical work/Daily weather data/Ontario/daily_weather_ontario")
attach(weather)

weather <- data.frame(county,year,days,min,max,pcp)
weather <- weather[order(county),]
attach(weather)

county_num <- seq(1,(nrow(weather)+1),365*T)

vec<-matrix(0,99,3)
for (i in 1:(length(county_num)-1))
{
  nam_max <- paste("max_", county[county_num[i]], sep = "")
  assign(nam_max, max[county_num[i]:(county_num[i+1]-1)])
  
  nam_min <- paste("min_", county[county_num[i]], sep = "")
  assign(nam_min, min[county_num[i]:(county_num[i+1]-1)])
  
  nam_pcp <- paste("pcp_", county[county_num[i]], sep = "")
  assign(nam_pcp, pcp[county_num[i]:(county_num[i+1]-1)])
 
}

year <- year[county_num[1]:(county_num[2]-1)]
day <- days[county_num[1]:(county_num[2]-1)]
weather_daily <- data.frame(year,day,min_brant,max_brant,pcp_brant,min_bruce,max_bruce,pcp_bruce,`min_chatham-kent`,`max_chatham-kent`,`pcp_chatham-kent`,
				    min_dufferin,max_dufferin,pcp_dufferin,min_durham,max_durham,pcp_durham,min_elgin,max_elgin,pcp_elgin,
                            min_essex,max_essex,pcp_essex,min_frontenac,max_frontenac,pcp_frontenac,min_grey,max_grey,pcp_grey,
                            `min_haldimand-norfolk`,`max_haldimand-norfolk`,`pcp_haldimand-norfolk`,min_haliburton,max_haliburton,pcp_haliburton,min_halton,max_halton,pcp_halton,
                            min_hamilton,max_hamilton,pcp_hamilton,min_hastings,max_hastings,pcp_hastings,min_huron,max_huron,pcp_huron,
                            `min_kawartha lakes`,`max_kawartha lakes`,`pcp_kawartha lakes`,min_lambton,max_lambton,pcp_lambton,min_lanark,max_lanark,pcp_lanark,
				                    `min_leeds-grenville`,`max_leeds-grenville`,`pcp_leeds-grenville`,`min_lennox-addington`,`max_lennox-addington`,`pcp_lennox-addington`,
				                    min_middlesex,max_middlesex,pcp_middlesex,min_muskoka,max_muskoka,pcp_muskoka,min_niagara,max_niagara,pcp_niagara,min_northumberland,max_northumberland,pcp_northumberland,
				                    min_ottawa,max_ottawa,pcp_ottawa,min_oxford,max_oxford,pcp_oxford,`min_parry sound`,`max_parry sound`,`pcp_parry sound`,
				                    min_peel,max_peel,pcp_peel,min_perth,max_perth,pcp_perth,min_peterborough,max_peterborough,pcp_peterborough,
				                    `min_prescott-russell`,`max_prescott-russell`,`pcp_prescott-russell`,`min_prince edward`,`max_prince edward`,`pcp_prince edward`,min_renfrew,max_renfrew,pcp_renfrew,
				                    min_simcoe,max_simcoe,pcp_simcoe,`min_stormont-dundas-glengarry`,`max_stormont-dundas-glengarry`,`pcp_stormont-dundas-glengarry`,min_waterloo,max_waterloo,pcp_waterloo,
				                   min_wellington,max_wellington,pcp_wellington,min_york,max_york,pcp_york)

write.table(weather_daily,"Data needed to complete empirical work/simulated weather/Ontario/daily_allcounty_allyears_weather")

weather_daily<-weather_daily[weather_daily$year %in% c(2000:2013),]

T <- 14
#de-seasonalizing weather data#

day_year <- seq(1,nrow(weather_daily),365)

mean_day <- matrix(0,365,ncol(weather_daily))
for(i in 1:365)
  for(j in 3:ncol(weather_daily))
  {mean_day[i,j] <- mean(weather_daily[(day_year+(i-1)),j])}

write.table(mean_day,"Data needed to complete empirical work/simulated weather/Ontario/current_mean_day")

mean_day <- do.call(rbind, replicate(T, mean_day, simplify=FALSE))

weather_demeaned <- weather_daily-mean_day

#standardizing weather data#

sd_day <- matrix(1,365,ncol(weather_daily))
for(i in 1:365)
  for(j in 3:ncol(weather_daily))
  {sd_dayi <- sd(weather_daily[(day_year+(i-1)),j])
  sd_day_before <- sd(weather_daily[(day_year+(i-2)),j])
  sd_day_after <- sd(weather_daily[(day_year+i),j])
  sd_mean <- (sd_day_before+sd_day_after)/2
  
    sd_day[i,j] <- ifelse(sd_dayi!=0,sd_dayi,sd_mean)}

write.table(sd_day,"Data needed to complete empirical work/simulated weather/Ontario/current_sd_day")

sd_day <- do.call(rbind, replicate(T, sd_day, simplify=FALSE))

weather_deseasonalized <- weather_demeaned/sd_day

write.table(weather_deseasonalized,"Data needed to complete empirical work/simulated weather/Ontario/weather_current_deseasonalized_Ontario")

