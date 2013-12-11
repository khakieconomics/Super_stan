# little demonstration of rstan for Boothy

# Clear your workspace
#rm(list = ls())

# Load the libararies. To install rstan, you'll need to do it via github. See https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
library(ggplot2); library(plyr); library(Hmisc); library(RSQLite); library(rstan); library(parallel); library(reshape2)
# Set default text size
theme_set(theme_gray(base_size = 18))

# You will need to have a working directory on your local machine (not a network or dropbox)
setwd("C:/stantest")

# Download my database of APRA data
download.file("https://dl.dropboxusercontent.com/u/63100926/Super_analysis.RData", destfile="Super_analysis.RData")

load("Super_analysis.RData")

# The basic idea here is to use a simple varying intercept, varying slope model with year returns defining the groups. The dependent variable is time t fee-adjusted returns. The independent variables are lagged fee-adjusted returns and lagged costs. So the model asks "if all you know about the fund is its historical returns and costs, what should you expect next year's returns to be?
# This is a fun opportunity to use STAN!!

simple.mod <- 'data {
// The below are data you will pass to the model. The integers are the length of the data series
int<lower=0> T; // Number of years in the sample
int<lower=0> J; // Number of fund types (not used below)
int<lower=0> N; // Number of observations

// The data series you will pass in, and how long they are
vector[N] costs_lag;
vector[N] ROR;
vector[N] ROR_lag;

// The index series. These are data series also (one unit per observation), converted to integers. There are J unique values in fund_type and T unique values in year.
int<lower=0> fund_type[N];
int<lower=0> year[N];

}
parameters {

// Define the regression-level parameters. The time-varying parameters are indexed.
real B0T[T];
real B1T[T];
real B2;

// And define the hyperparameters (parameters which describe the distribution of the regression-level parameters)

real B0That;
real B1That;
real<lower=0> sigma_b0t;
real<lower=0> sigma_b1t;
real<lower=0> sigma_ror;

}
model {
// Now we just write out the model. 

// The time-varying parameters (varying intercept, varying slope) are normally distributed around *hat
B0T ~ normal(B0That, sigma_b0t);
B1T ~ normal(B1That, sigma_b1t);

// Specify a prior for the effect of historical returns on future returns. We assume efficient market.
B2 ~ normal(0,0.03);

// Write out the regression model. Note that we use the index variables to index the 
for(n in 1:N)
ROR[n] ~ normal(B0T[year[n]] + B1T[year[n]] * costs_lag[n] + B2 * ROR_lag[n], sigma_ror);
}'





# Data construction
# We need fund name, year, ROR, costs

# Define a cost series
apra.ds$Costs <- ifelse(apra.ds$Net_assets_at_end_of_period_>0 & apra.ds$Total_operating_expenses_>0, ((apra.ds$Total_operating_expenses_+apra.ds$Total_investment_expenses_)/apra.ds$Net_assets_at_end_of_period_)*100,NA)

# Grab the columns I want
mcmcdat <- data.frame(fund = apra.ds$Fund_name, year = apra.ds$Year, ROR = apra.ds$Rate_of_return, costs = apra.ds$Costs, fund_type = apra.ds$Fund_type)

# Get complete cases
mcmcdat <- mcmcdat[complete.cases(mcmcdat),]

# Function to create model matrix using ddply

mm.maker <- function(x){
	x <- x[order(x$year),]
	year <- x$year - 2004
	ROR <- x$ROR
	costs <- x$costs
	ROR_lag <- ifelse(rep(length(ROR),length(ROR)) >1,c(NA, ROR[-length(ROR)]), NA)
	costs_lag <- ifelse(rep(length(costs),length(costs))>1,c(NA, costs[-length(costs)]),NA)
	fund_type <- x$fund_type
	
	out <- data.frame(year, ROR, ROR_lag, costs_lag, fund_type)
	out$delete <- ifelse(nrow(out)<3, FALSE, TRUE)
	out$delete <- ifelse(out$delete==FALSE, FALSE, ifelse(rep(max(year) - min(year)+1)<length(year), FALSE, TRUE))
	out
}

mm <- ddply(mcmcdat, "fund", mm.maker)
mm <- mm[complete.cases(mm),]
mm <- mm[mm$delete,]


# Create a data list containing all the data specified in the model above
simple_model_data <- list(N = nrow(mm),
									T = max(mm$year),
									J = length(unique(mm$fund_type)),				
									year = mm$year,
									ROR = mm$ROR,
									ROR_lag = mm$ROR_lag,
									costs_lag = mm$costs_lag,
									fund_type = as.numeric(as.factor(mm$fund_type))
									)


# Run a single iteration of the model (just to compile the C++ code)
simple.returns.fit <- stan(model_code = simple.mod, data = simple_model_data, iter = 1, chains = 1)

# Set the number of cores you have here. 
options(mc.cores=4)

# If you're running this baby in parallel, you can do one chain per core quickly.
chains <- 4

# Apply the model compiled above to the number of chains you have, executed on each core
posterior <- mclapply(1:chains, FUN = function(chain){
	stan(model_code = simple.mod, fit = simple.returns.fit, data = simple_model_data, chains = 1, chain_id = chain, refresh = -1)
})

# The 'posterior' object above is a list. We want it to be a stanfit S4 object. 
posterior <- sflist2stanfit(posterior)

# Print the output. When Rhat approaches 1, we have convergence. 
print(posterior)

# Turn the output into something easily analysable
returns.fit.ex <- extract(posterior, permuted = TRUE)

# Let's make a density plot of the varying intercepts.
B0T <- returns.fit.ex$B0T
B0T.m <- melt(B0T)
B0T.m$Var2 <- as.factor(B0T.m$Var2+2004)
names(B0T.m) <- c("iterations", "Year", "Return")

ggplot(B0T.m, aes(x = Return, group = Year, fill = Year)) + geom_density(alpha = 0.4) + ggtitle("Average returns by year controlling for costs and historical returns") + xlab("Per cent") + ylab("Posterior density")

# Now let's make a plot of the varying slopes
B1T <- returns.fit.ex$B1T
head(B1T)
B1T.m <- melt(B1T)
B1T.m$Var2 <- as.factor(B1T.m$Var2)
names(B1T.m) <- c("iterations", "Year", "Fee_effect")
B1T.m$Year <- as.factor(as.numeric(B1T.m$Year) + 2004)
ggplot(B1T.m, aes(x = Fee_effect, group = Year, fill = Year)) + geom_density(alpha = 0.4) + xlab("Expected difference in after-fee annual % returns\n of being in a fund 1% more expensive, all Australian super funds") + ylab("Posterior density") + ggtitle("Expensive funds rarely outperform")


# How much correlation in returns is there across time?
ts.effect <- returns.fit.ex$B2
ts.effect.m <- melt(ts.effect)
head(ts.effect.m)
ggplot(ts.effect.m, aes(x = value)) + geom_density() + xlab("Whole percentage points (1 = 1%)") + ggtitle("Impact of a 1% higher return last year on\n this year's returns") + ylab("Posterior density, prior ~Normal(0,0.03)")

