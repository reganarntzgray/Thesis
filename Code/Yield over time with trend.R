Yield <- readRDS("Data needed to complete empirical work/Yield Data/Iowa/Yield_Iowa")

yield <- Yield[,3]

year <- unique(Yield$year)

yield_iowa <- NULL
for(i in 1:length(year))
{yield_iowa[i] <- mean(Yield[Yield$year %in% year[i],3])}

time <- year-1954

mod <- lm(yield_iowa~year)

plot(year, yield_iowa, col = "blue", main = "Average County Level Yield Over Time: Iowa", xlab = "Year", ylab = "Average County Level Yield")
abline(mod)

Yield <- readRDS("Data needed to complete empirical work/Yield Data/Ontario/Yield_Ontario")

yield <- Yield[,3]

year <- unique(Yield$year)

yield_ont <- NULL
for(i in 1:length(year))
{yield_ont[i] <- mean(Yield[Yield$year %in% year[i],3])}

install.packages("segmented")
library(segmented)

time <- year
trend <- (time-2000)*(time>2000)

mod <- lm(yield_ont~year)
segment <- segmented(mod, seg.Z = ~year, psi=2000)
plot(segment,add=T)

plot(year, yield_ont, col = "blue", main = "Average County Level Yield Over Time: Ontario", xlab = "Year", ylab = "Average County Level Yield")
abline(segment)

mod2 <- lm(yield_ont~time+trend)
