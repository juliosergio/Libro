##########################################################
## Forecasting reservoir inflows in Mexico using lagged ## 
## streamflow and principle components of Kaplan SSTs   ##
##########################################################

## Code by Floris van Ogtrop September 2014

## Load Streamflow data
## setwd("C:\\Users\\fvanogtrop\\Dropbox\\PROJECTS\\MEXICO\\Data")
flow <- read.csv("8SAN_59_07.csv")

## We are going to look at monthly flows for the month of December 
## This was suggested by Waldo as a critical period for reservoir operation
month.flow <- flow[flow$Month == 12 & flow$Year <=2007,]

## Inspect flow data
plot(month.flow[,1], month.flow[,3], ylab = "Inflow (Hm^3)", xlab = "Year", type = "l")
qqnorm(month.flow$flow.Hm3)
plot(density(month.flow$flow.Hm3)) ## Data is skewed
acf(month.flow$flow.Hm3) ## independant in time



##################################
## Principal component analysis ##
##################################
 
require(RNetCDF)
file <- "ExtendedKaplanSSTv2_Colombia.cdf"
nc <- open.nc(file)

# following four lines to find out which variables are contained in 'nc'
var.inq.nc(nc, 0)
var.inq.nc(nc, 1)
var.inq.nc(nc, 2)
var.inq.nc(nc, 3)

# extracting data. Note that longitude goes from -332.5 to 22.5 - changing so it goes from -177.5 to 177.5 to be consistent with mapping software
time <- var.get.nc(nc, 1)
year <- as.numeric(floor(time/12+1960)) # conversion to years and months.
month <- round(0.5+12*(time/12+1960-year))

lat <- var.get.nc(nc, 0)
lon <- var.get.nc(nc, 2)+155

ssta <- var.get.nc(nc, 3)

close.nc(nc)

## Converting X from a 3 dimensional array to a two dimensional matrix
X.lat <- NULL
X.lon <- NULL
k = 1
X <- matrix(NA, nrow=dim(ssta)[3], ncol=dim(ssta)[1]*dim(ssta)[2])
for (i in 1:dim(ssta)[1]) {
   for (j in 1:dim(ssta)[2]) {
       X.lon[k] <- lon[i]
       X.lat[k] <- lat[j]
       X[,k] <- ssta[i,j,]
       k = k + 1
   }
}
X.lat <- X.lat[!is.na(X[1,])]
X.lon <- X.lon[!is.na(X[1,])]
X <- X[,!is.na(X[1,])] # removing columns with 'NA'

# Plot of unweighted average SST. What is meant by 'unweighted' and why does it matter?? How would you weight the dataset? (hint - need to
# adjust by a factor of cos(latitude).
plot((year+(month-1)/12), rowMeans(X), type="l", xlab="Time", ylab="SST anomaly", main="Unweighted time series of global SSTs")

# Detrending - using a linear model.
#for (i in 1:dim(X)[2])
#X[,i] <- X[,i] - predict(lm(X[,i]~I(1:dim(X)[1])))

X2 <- cbind(year,month,X)

## We calculate PCs based on expanding windows starting with 30 years of data, then 31, then 32, ...

start.year <- 1959
end.year <- 2007

wndw <- 30 ## Set window width
a <- start.year:end.year
b <- list()
for(g in 1:(length(a)-wndw)){
	b[[g]] <- a[1:(g+wndw)]
}
	
X3 <- list()
for(k in 1:length(b)){
	X3[[k]] <- X2[X2[,1] %in% b[[k]],]
}

require(psych)
## use "prcomp" for standard pca or 
## use "principal" from the psych package for varimax rotated pc's

pc.fun <- function(Y) {
	model1 <- prcomp(Y[,3:NCOL(Y)]) 
	PCs <- predict(model1, data = Y[,3:NCOL(Y)])
	PC.df <- data.frame(year = Y[,1], month = Y[,2], PC1 = PCs[,1], PC2 = PCs[,2], PC3 = PCs[,3]
	, PC4 = PCs[,4], PC5 = PCs[,5], PC6 = PCs[,6], PC7 = PCs[,7], PC8 = PCs[,8], PC9 = PCs[,9]
	, PC10 = PCs[,10])
}
		
X4 <-  lapply(X3, pc.fun)

## Add flow (and rainfall if data available) as lagged covariate(s)
for(i in 1:length(X4)){
	X4[[i]]$lagflow <- flow[flow$Year<=(start.year+(wndw-1)+i),]$flow.Hm3 
}

## Save having to calculate each time
#save(X4, file="X4")
#load("X4")

## We like to explore the relationaship between different single or multiple (averaged)  
## lagged covariates and streamflow
## Note that July seemed to be a good start for this model (4 month lag to December)

ave.fun <- function(z) {
	Y <- c(7) ## Single month or months you wish to average 1:Jan, 2:Feb,..., 12:Dec
	z1 <- z[z[,2]%in%Y,] ## Subset the months you wish average
	z2 <- do.call(rbind, by(z1, z1$year, colMeans)) ## Calculate means
}

covariates  <- lapply(X4, ave.fun)

## Plot relatioship between streamflow and PCs for all year

par(mfrow = c(4,3))
plot(month.flow[,3], covariates[[19]][,3], xlab = "Streamflow Hm3", ylab = "PC1")
plot(month.flow[,3], covariates[[19]][,4], xlab = "Streamflow Hm3", ylab = "PC2")
plot(month.flow[,3], covariates[[19]][,5], xlab = "Streamflow Hm3", ylab = "PC3")
plot(month.flow[,3], covariates[[19]][,6], xlab = "Streamflow Hm3", ylab = "PC4")
plot(month.flow[,3], covariates[[19]][,7], xlab = "Streamflow Hm3", ylab = "PC5")
plot(month.flow[,3], covariates[[19]][,8], xlab = "Streamflow Hm3", ylab = "PC6")
plot(month.flow[,3], covariates[[19]][,9], xlab = "Streamflow Hm3", ylab = "PC7")
plot(month.flow[,3], covariates[[19]][,10], xlab = "Streamflow Hm3", ylab = "PC8")
plot(month.flow[,3], covariates[[19]][,11], xlab = "Streamflow Hm3", ylab = "PC9")
plot(month.flow[,3], covariates[[19]][,12], xlab = "Streamflow Hm3", ylab = "PC10")
plot(month.flow[,3], covariates[[19]][,13], xlab = "Streamflow Hm3", ylab = "Lag flow Hm3")

## Is there any year to year correlation?
acf(month.flow[,3]) ## No

#######################
## Expanding windows ##
#######################

## Because we use calculated PCs (principal components) I use what I call an "expanding windows" approach.
## This ensures that we are going to forcast ahead given no knowledge of the future system

## It can be demonstrated why this is important using the following plots
## The plots compare PCs calculated using data from 1961-1989 vs PCs calculated using data from 1961 to 2007
## Notice there is some variation between using the full dataset to part of the dataset

par(mfrow = c(4,3)) 
plot(X4[[1]][1:372,3],X4[[19]][1:372,3]) ##PC1
plot(X4[[1]][1:372,4],X4[[19]][1:372,4]) ##PC2
plot(X4[[1]][1:372,5],X4[[19]][1:372,5]) ##PC3
plot(X4[[1]][1:372,6],X4[[19]][1:372,6]) ##PC4
plot(X4[[1]][1:372,7],X4[[19]][1:372,7]) ##PC5
plot(X4[[1]][1:372,8],X4[[19]][1:372,8]) ##PC6
plot(X4[[1]][1:372,9],X4[[19]][1:372,9]) ##PC7
plot(X4[[1]][1:372,10],X4[[19]][1:372,10]) ##PC8
plot(X4[[1]][1:372,11],X4[[19]][1:372,11]) ##PC9
plot(X4[[1]][1:372,12],X4[[19]][1:372,12]) ##PC10

###############
## Forecasts ##
###############

## For the following analysis I will only use PC1, PC2, PC7 and lagflow, these can be easily changed or we can do some
## form of model selection. Alternatively we could use SOI, Nino 3.4, AO, PDO...

## Split validation
## first do a split validation using PCs calculated on data from 1961 - 1995 (at least 30 years of data)

covariates1a <- data.frame(covariates[[8]])
flow1a <- month.flow[month.flow$Year <= tail(covariates1a$year, 1),]
mod.data1a <- data.frame(flow = flow1a[,3], covariates1a)
mod.data1a <- mod.data1a[mod.data1a$flow > 0,] 
mod.cal1a <- glm(flow ~ PC1+PC2+PC7+lagflow, data = mod.data1a, family = Gamma(link = "log"))
summary(mod.cal1a)
par(mfrow = c(2,2))
plot(mod.cal1a)

covariates1b <- data.frame(covariates[[19]])

predict.val1a <- predict(mod.cal1a, newdata=covariates1b, type = "response", se.fit=TRUE)
predict.val1a$uci <- as.numeric(predict.val1a$fit)+1.96*as.numeric(predict.val1a$se.fit)
predict.val1a$lci <- as.numeric(predict.val1a$fit)-1.96*as.numeric(predict.val1a$se.fit)
predict.val1a$observed <- month.flow[,3]
predict.val1a$year <- month.flow[,1] 

par(mfrow = c(1,1))
plot(predict.val1a$year,predict.val1a$observed, type = "l", ylim = c(min(predict.val1a$lci),max(predict.val1a$uci))
, xlab = "Inflow (Hm^3)", main = "Forecast Inflows Presa Sanalona")
lines(predict.val1a$year,as.numeric(predict.val1a$fit), col = "red")
lines(predict.val1a$year,predict.val1a$uci, lty = 2)
lines(predict.val1a$year,predict.val1a$lci, lty = 2)
legend("topright",1,c("Observed","Forecast", "Confidence intervals"),lty=c(1, 1, 2)
, col = c("black", "red", "black"))

## look at r-squared (not the best metric but it is sufficient for now
cor(predict.val1a$observed, as.numeric(predict.val1a$fit))^2

## One step ahead forecast 

## The problem with the split approach is that we are not retraining the model with new data as it is measured each year
## Therefore we forecast one step ahead, then retrain the model with the new measurments
## So we use PCAs calcualted on SSTs up to 1989 and flow up to 1989 to train model to forecast flow for 1990.

mod.cal1 <- list()
mod.summary1 <- list()
n <- length(X4)

for(i in 1:n){
	covariates1 <- data.frame(covariates[[i]])
	flow <- month.flow[month.flow$Year <= tail(covariates1$year, 1),]
	mod.data1 <- data.frame(flow = flow[,3], covariates1)
	mod.data1 <- mod.data1[mod.data1$flow > 0,] 
	mod.cal1[[i]] <- glm(flow ~ PC1+PC2+PC7+lagflow, data = mod.data1, family = Gamma(link = "log"))
	mod.summary1[[i]] <- summary(mod.cal1[[i]])
}

## now we use each model to predict the next year streamflow

predict.val1 <- list()
for(i in 1:(n-1)){
	Data.val <- data.frame(tail(covariates[[i+1]], 1))
	predict.val1[[i]] <- predict(mod.cal1[[i]], newdata=Data.val, type = "response", se.fit=TRUE)
} 

predict.val2 <- data.frame(do.call(rbind, predict.val1))
predict.val2$uci <- as.numeric(predict.val2$fit)+1.96*as.numeric(predict.val2$se.fit)
predict.val2$lci <- as.numeric(predict.val2$fit)-1.96*as.numeric(predict.val2$se.fit)
predict.val2$observed <- month.flow[month.flow$Year >=1990,3]
predict.val2$year <- month.flow[month.flow$Year >=1990,1] 

par(mfrow = c(1,1))
plot(predict.val2$year,predict.val2$observed, type = "l", ylim = c(min(predict.val2$lci),max(predict.val2$uci))
, xlab = "Inflow (Hm^3)", main = "Forecast Inflows Presa Sanalona")
lines(predict.val2$year,as.numeric(predict.val2$fit), col = "red")
lines(predict.val2$year,predict.val2$uci, lty = 2)
lines(predict.val2$year,predict.val2$lci, lty = 2)
legend("topright",1,c("Observed","Forecast", "Confidence intervals"),lty=c(1, 1, 2)
, col = c("black", "red", "black"))

## Use simple metric r2 to test model performance
cor(predict.val2$observed,as.numeric(predict.val2$fit))^2 

