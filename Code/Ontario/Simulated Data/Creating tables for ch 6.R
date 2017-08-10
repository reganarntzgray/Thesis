setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario")

Y_current <- read.table("Yield_current_sim_Ontario")

Y_future_nochange <- read.table("Yield_future_nochange_sim_Ontario")

Y_future_change <- read.table("Yield_future_change_sim_Ontario")

Y_future <- read.table("Yield_future_sim_Ontario")


Y_cur_dyn <- Y_current[,3]
Y_cur_stat <- Y_current[,4]
Y_fut_dyn <- Y_future[,3]
Y_fut_stat <- Y_future[,4]
Y_fut_nc <- Y_future_nochange[,3]
Y_fut_cc <- Y_future_change[,3]

premiums_cur <- read.table("Premiums_Ontario_current")
premiums_fut <- read.table("Premiums_Ontario_future")


dyn_pre_cur <- premiums_cur[,2]
stat_pre_cur <- premiums_cur[,3]
ratio_cur <- premiums_cur[,4]

dyn_pre_fut <- premiums_fut[,2]
stat_pre_fut <- premiums_fut[,3]
ratio_fut <- premiums_fut[,4]

#creating the current table#

N <- 32
T <- 100

#are the county mean values different under the static and dynamic models?#

install.packages("car")
library(car)

county_num <- seq(1,nrow(Y_current),N)

Current_Yield_Summary <- matrix(0,N,9)
mean_pval <- NULL
var_pval <- NULL
for (i in 1:N)
{yield_i_dyn <- Y_cur_dyn[county_num+(i-1)]
 yield_i_stat <- Y_cur_stat[county_num+(i-1)]
 
 
 mean_pval[i] <- t.test(yield_i_stat,yield_i_dyn)$p.value
 
 yi <- c(yield_i_stat,yield_i_dyn)
 groupi<-  c(rep(1,length(yield_i_stat)),rep(2,length(yield_i_dyn)))
 groupi <- as.factor(groupi)
 
 var_pval[i] <- leveneTest(yi,groupi)$'Pr(>F)'[1]
 
 Current_Yield_Summary[i,]<-c(mean(yield_i_dyn),sd(yield_i_dyn),dyn_pre_cur[i],mean(yield_i_stat),sd(yield_i_stat),stat_pre_cur[i],ratio_cur[i],mean_pval[i],var_pval[i])
 }


Future_Yield_Summary <- matrix(0,N,9)
mean_pval <- NULL
var_pval <- NULL
for (i in 1:N)
{yield_i_dyn <- Y_fut_dyn[county_num+(i-1)]
yield_i_stat <- Y_fut_stat[county_num+(i-1)]


mean_pval[i] <- t.test(yield_i_stat,yield_i_dyn)$p.value

yi <- c(yield_i_stat,yield_i_dyn)
groupi<-  c(rep(1,length(yield_i_stat)),rep(2,length(yield_i_dyn)))
groupi <- as.factor(groupi)

var_pval[i] <- leveneTest(yi,groupi)$'Pr(>F)'[1]

Future_Yield_Summary[i,]<-c(mean(yield_i_dyn),sd(yield_i_dyn),dyn_pre_fut[i],mean(yield_i_stat),sd(yield_i_stat),stat_pre_fut[i],ratio_fut[i],mean_pval[i],var_pval[i])
}

county <- unique(Y_current$county)

Current_Yield_Summary<- data.frame(county,Current_Yield_Summary)
colnames(Current_Yield_Summary)<-c("county","$mu_d$","$hat(sigma_d)$","$r_d$","$mu_s$","$hat(sigma_s)$","$r_s$","ratio","$pval_{mu}$","$pval_{sigma}$")

write.table(Current_Yield_Summary,"Current_Yield_Summary_Ontario")

Future_Yield_Summary<- data.frame(county,Future_Yield_Summary)
colnames(Future_Yield_Summary)<-c("county","$mu_d$","$hat(sigma_d)$","$r_d$","$mu_s$","$hat(sigma_s)$","$r_s$","ratio","$pval_{mu}$","$pval_{sigma}$")

write.table(Future_Yield_Summary,"Future_Yield_Summary_Ontario")


county_num <- seq(1,nrow(Y_future),N)

Expected_Yield_Summary <- matrix(0,N,6)
mean_pval <- NULL
var_pval <- NULL
for (i in 1:N)
{yield_i_cc <- Y_fut_cc[county_num+(i-1)]
yield_i_nc <- Y_fut_nc[county_num+(i-1)]
mean_pval[i] <- t.test(yield_i_cc,yield_i_nc)$p.value

yi <- c(yield_i_cc,yield_i_nc)
groupi<-  c(rep(1,length(yield_i_cc)),rep(2,length(yield_i_nc)))
groupi <- as.factor(groupi)

var_pval[i] <- leveneTest(yi,groupi)$'Pr(>F)'[1]

Expected_Yield_Summary[i,]<-c(mean(yield_i_cc),sd(yield_i_cc),mean(yield_i_nc),sd(yield_i_nc),mean_pval[i],var_pval[i])
}


county <- unique(Y_future$county)

Expected_Yield_Summary<- data.frame(county,Expected_Yield_Summary)
colnames(Expected_Yield_Summary)<-c("county","$mu_{cc}$","$hat(sigma_{cc})$","$mu_{nc}$","$hat(sigma_{nc})$","$pval_{mu}$","$pval_{sigma}$")

write.table(Expected_Yield_Summary,"Expected_Yield_Summary_Ontario")

###lets get a premia table - analyze whether the premia ratio is different from 1 within the state

