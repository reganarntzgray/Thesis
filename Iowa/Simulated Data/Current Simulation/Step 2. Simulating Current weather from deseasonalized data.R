set.seed(10301992)

setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Iowa")

N <- 99
T <- 13

weather_deseasonalized <- read.table("weather_current_deseasonalized_Iowa")

W <- weather_deseasonalized[,3:ncol(weather_deseasonalized)]

#k-NN sampling to generate weather#

sigma <- cov(W)
sigma_inv <- solve(sigma)

distance <- function(w1,w2)
{distance<-t(w1-w2)%*%sigma_inv%*%(w1-w2)
return(sqrt(distance))}

#choosing the starting vector#
#k small due to very large d - meant to be prop. to n^4/(d+4)=1.117871 - d=297

k <- 10

for(j in 48:100)
{seed <- j*1000
  set.seed(seed)

w1 <- c(rnorm(297))
w1 <- as.vector(w1)

Sample_weather <- matrix(0,365,ncol(W))

Prob_num <- 1/c(1:k)
denom <- sum(Prob_num)
Prob <- Prob_num/denom

for(n in 1:365)
{
  Distance <- rep(0,nrow(W))
  
  for (i in 1:nrow(W))
  {w2 <- t(W[i,])
  Distance[i] <- distance(w1,w2)}
  ord <- c(1:nrow(W))
  Distance1 <- data.frame(ord,Distance)
  Distance_ord <- Distance1[order(Distance),]
  
  Neighbours <- Distance_ord[1:k,1]
  
  successor <- sample(x=Neighbours,1, prob=Prob)+1
  
  w1 <- t(W[successor,]) 
  
  Sample_weather[n,] <- w1
  print(n)}
print(j)
write.table(Sample_weather,paste("weather_current_simulated_year",j,sep=""))
}
##above code will ggenerate one year of sample weather data##

write.table(Sample_weather,"weather_current_simulated_year1")

#we can create say 100 simulated years of data - then use those 100 years to create weather vars as in our yield model- then sub in the vars to get yield##

#for year 2 we will set a new seed and simulate again starting from choosing a random std normal vector to start

set.seed(07231991)
write.table(Sample_weather,"weather_current_simulated_year2")

set.seed(3000)
write.table(Sample_weather,"weather_current_simulated_year3")

set.seed(4000)
write.table(Sample_weather,"weather_current_simulated_year4")

set.seed(7000)
write.table(Sample_weather,"weather_current_simulated_year7")


set.seed(9000)
write.table(Sample_weather,"weather_current_simulated_year9")

set.seed(11000)
write.table(Sample_weather,"weather_current_simulated_year11")

set.seed(13000)
write.table(Sample_weather,"weather_current_simulated_year13")

set.seed(14000)
write.table(Sample_weather,"weather_current_simulated_year14")

set.seed(17000)
write.table(Sample_weather,"weather_current_simulated_year17")

set.seed(19000)
write.table(Sample_weather,"weather_current_simulated_year19")

set.seed(20000)
write.table(Sample_weather,"weather_current_simulated_year20")

set.seed(22000)
write.table(Sample_weather,"weather_current_simulated_year22")

set.seed(27000)
write.table(Sample_weather,"weather_current_simulated_year27")

set.seed(28000)
write.table(Sample_weather,"weather_current_simulated_year28")

set.seed(29000)
write.table(Sample_weather,"weather_current_simulated_year29")

set.seed(30000)
write.table(Sample_weather,"weather_current_simulated_year30")

set.seed(31000)
write.table(Sample_weather,"weather_current_simulated_year31")

set.seed(33000)
write.table(Sample_weather,"weather_current_simulated_year33")

set.seed(36000)
write.table(Sample_weather,"weather_current_simulated_year36")

set.seed(37000)
write.table(Sample_weather,"weather_current_simulated_year37")

set.seed(41000)
write.table(Sample_weather,"weather_current_simulated_year41")

set.seed(43000)
write.table(Sample_weather,"weather_current_simulated_year43")

set.seed(45000)
write.table(Sample_weather,"weather_current_simulated_year45")

set.seed(46000)
write.table(Sample_weather,"weather_current_simulated_year46")

set.seed(47000)
write.table(Sample_weather,"weather_current_simulated_year47")

 ##putting all created years together and re-seasonalizing

#year1 <- read.table("weather_current_simulated_year1")
#year2 <- read.table("weather_current_simulated_year2")

#do for other years

weather_current_sim <- rbind(year1,year2, etc)

#seasonalizing resultant data

#first need to call on the daily weather

setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis")

N <- 99
T <- 58

weather <- readRDS("Data needed to complete empirical work/Daily weather data/Iowa/daily_weather_iowa")
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
weather_daily <- data.frame(year,day,min_adair,max_adair,pcp_adair,min_adams,max_adams,pcp_adams,min_allamakee,max_allamakee,
                            pcp_allamakee,min_appanoose,max_appanoose,pcp_appanoose,min_audubon,max_audubon,pcp_audubon,
                            min_benton,max_benton,pcp_benton,`min_black hawk`,`max_black hawk`,`pcp_black hawk`,min_boone,max_boone,pcp_boone,
                            min_bremer,max_bremer,pcp_bremer,min_buchanan,max_buchanan,pcp_buchanan,`min_buena vista`,`max_buena vista`,`pcp_buena vista`,
                            min_butler,max_butler,pcp_butler,min_calhoun,max_calhoun,pcp_calhoun,min_carroll,max_carroll,pcp_carroll,min_cass,max_cass,
                            pcp_cass,min_cedar,max_cedar,pcp_cedar,`min_cerro gordo`,`max_cerro gordo`,`pcp_cerro gordo`,min_cherokee,max_cherokee,pcp_cherokee,
                            min_chickasaw,max_chickasaw,pcp_chickasaw,min_clarke,max_clarke,pcp_clarke,min_clay,max_clay,pcp_clay,min_clayton,max_clayton,pcp_clayton,
                            min_clinton,max_clinton,pcp_clinton,min_crawford,max_crawford,pcp_crawford,min_dallas,max_dallas,pcp_dallas,
                            min_davis,max_davis,pcp_davis,min_decatur,max_decatur,pcp_decatur,min_delaware,max_delaware,pcp_delaware,`min_des moines`,`max_des moines`,`pcp_des moines`,
                            min_dickinson,max_dickinson,pcp_dickinson,min_dubuque,max_dubuque,pcp_dubuque,min_emmet,max_emmet,pcp_emmet,min_fayette,max_fayette,pcp_fayette,
                            min_floyd,max_floyd,pcp_floyd,min_franklin,max_franklin,pcp_franklin,min_fremont,max_fremont,pcp_fremont,
                            min_greene,max_greene,pcp_greene,min_grundy,max_grundy,pcp_grundy,min_guthrie,max_guthrie,pcp_guthrie,min_hamilton,max_hamilton,pcp_hamilton,
                            min_hancock,max_hancock,pcp_hancock,min_hardin,max_hardin,pcp_hardin,min_harrison,max_harrison,pcp_harrison,min_henry,max_henry,pcp_henry,
                            min_howard,max_howard,pcp_howard,min_humboldt,max_humboldt,pcp_humboldt,min_ida,max_ida,pcp_ida,min_iowa,max_iowa,pcp_iowa,
                            min_jackson,max_jackson,pcp_jackson,min_jasper,max_jasper,pcp_jasper,min_jefferson,max_jefferson,pcp_jefferson,min_johnson,max_johnson,pcp_johnson,
                            min_jones,max_jones,pcp_jones,min_keokuk,max_keokuk,pcp_keokuk,min_kossuth,max_kossuth,pcp_kossuth,min_lee,max_lee,pcp_lee,
                            min_linn,max_linn,pcp_linn,min_louisa,max_louisa,pcp_louisa,min_lucas,max_lucas,pcp_lucas,min_lyon,max_lyon,pcp_lyon,min_madison,max_madison,pcp_madison,
                            min_mahaska,max_mahaska,pcp_mahaska,min_marion,max_marion,pcp_marion,min_marshall,max_marshall,pcp_marshall,min_mills,max_mills,pcp_mills,
                            min_mitchell,max_mitchell,pcp_mitchell,min_monona,max_monona,pcp_monona,min_monroe,max_monroe,pcp_monroe,min_montgomery,max_montgomery,pcp_montgomery,
                            min_muscatine,max_muscatine,pcp_muscatine,`min_o brien`,`max_o brien`,`pcp_o brien`,min_osceola,max_osceola,pcp_osceola,
                            min_page,max_page,pcp_page,`min_palo alto`,`max_palo alto`,`pcp_palo alto`,min_plymouth,max_plymouth,pcp_plymouth,min_pocahontas,max_pocahontas,pcp_pocahontas,
                            min_polk,max_polk,pcp_polk,min_pottawattamie,max_pottawattamie,pcp_pottawattamie,min_poweshiek,max_poweshiek,pcp_poweshiek,
                            min_ringgold,max_ringgold,pcp_ringgold,min_sac,max_sac,pcp_sac,min_scott,max_scott,pcp_scott,min_shelby,max_shelby,pcp_shelby,
                            min_sioux,max_sioux,pcp_sioux,min_story,max_story,pcp_story,min_tama,max_tama,pcp_tama,min_taylor,max_taylor,pcp_taylor,
                            min_union,max_union,pcp_union,`min_van buren`,`max_van buren`,`pcp_van buren`,min_wapello,max_wapello,pcp_wapello,min_warren,max_warren,pcp_warren,
                            min_washington,max_washington,pcp_washington,min_wayne,max_wayne,pcp_wayne,min_webster,max_webster,pcp_webster,min_winnebago,max_winnebago,pcp_winnebago,
                            min_winneshiek,max_winneshiek,pcp_winneshiek,min_woodbury,max_woodbury,pcp_woodbury,min_worth,max_worth,pcp_worth,min_wright,max_wright,pcp_wright)


weather_daily<-weather_daily[weather_daily$year %in% c(2000:2012),]

T <- 13

t <- 100 #number of years of created data
#de-seasonalizing weather data#

day_year <- seq(1,nrow(weather_daily),365)

mean_day <- matrix(0,365,ncol(weather_daily))
for(i in 1:365)
  for(j in 3:ncol(weather_daily))
  {mean_day[i,j] <- mean(weather_daily[(day_year+(i-1)),j])}

mean_day <- do.call(rbind, replicate(t, mean_day, simplify=FALSE))

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

sd_day <- do.call(rbind, replicate(t, sd_day, simplify=FALSE))


#then we will make a data frame which has all  the created years of data - will want to stack it again in the normal way