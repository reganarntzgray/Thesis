setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/Data sets for model testing/Iowa")

yield_data <- readRDS("Data_d_iv_iowa")

P <- yield_data$pcp_pol_i

n <- length(P)
Dynamic_Kern_Estimate <- function(p)
{
  sigma_y<- sd(P)
  
  Q3 <- quantile(P)[4]
  Q1 <- quantile(P)[2]
  range <- (Q3-Q1)/1.34
  A <- min(sigma_y^2,range)
  h<- 0.9*A*(n^(-1/5))
  
  prob <-(1/(n*h))*sum(dnorm((p-P)/h))
  return(prob)}

by <- 400/999

grid <- seq(0,400,by)
pdf_pcp_iowa <- NULL

for (i in 1:length(grid))
{pdf_pcp_iowa[i]<-Dynamic_Kern_Estimate(grid[i])}

setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Iowa")


write.table(pdf_pcp_iowa,"pdf_pcp_iowa")


###Ontario
rm(list = ls())

setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/Data sets for model testing/Ontario")

yield_data <- readRDS("Data_d_iii_ont")

P <- yield_data$mean_pcp_july

n <- length(P)
Dynamic_Kern_Estimate <- function(p)
{
  sigma_y<- sd(P)
  
  Q3 <- quantile(P)[4]
  Q1 <- quantile(P)[2]
  range <- (Q3-Q1)/1.34
  A <- min(sigma_y^2,range)
  h<- 0.9*A*(n^(-1/5))
  
  prob <-(1/(n*h))*sum(dnorm((p-P)/h))
  return(prob)}

by <- 300/999

grid <- seq(0,300,by)
pdf_pcp_ont <- NULL

for (i in 1:length(grid))
{pdf_pcp_ont[i]<-Dynamic_Kern_Estimate(grid[i])}

setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario")


write.table(pdf_pcp_ont,"pdf_pcp_ont")
