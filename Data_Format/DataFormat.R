# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()


# Load and install packages if necessary
ipak <- function(package) {
    new.package <- package[!(package %in% installed.packages()[, "Package"])]
    if (length(new.package)) {
        install.packages(new.package, dependencies = TRUE)
    }
    sapply(package, require, character.only = TRUE)
}
package.list = c("dplyr", "xts", "smooth", "abind", "zoo", "parallel", "doParallel", "rnn", "RSNNS")
ipak(package.list)
remove(package.list)
remove(ipak)

# Format of the market data
variables = list.files(path = paste(getwd(), "/Data_Format/Other/", sep = ""))
market = lapply(variables, function(i) {
    i <- paste(paste(getwd(), "/Data_Format/Other/", sep = ""), i, sep = "")
    read.csv2(i, header = TRUE, sep = ";", dec = ".")
})
names(market) <- gsub(".csv", "", variables)
for (var in names(market)) {
    market[[var]] = market[[var]][nrow(market[[var]]):1, ]
}
for (var in names(market)) {
    market[[var]]$Date = as.character(market[[var]]$Date)
}
for (var in names(market)) {
    for (rep in list(c("janv", "01"), c("Feb", "02"), c("mars", "03"), c("Apr", "04"), c("May", "05"), c("Jun", "06"), c("Jul", "07"), c("Aug", 
                                                                                                                                         "08"), c("sept", "09"), c("oct", "10"), c("nov", "11"), c("Dec", "12"))) {
        market[[var]]$Date[grep(rep[1], as.character(market[[var]]$Date))] = gsub(rep[1], rep[2], as.character(market[[var]]$Date))[grep(rep[1], 
                                                                                                                                         as.character(market[[var]]$Date))]
    }
}
for (var in names(market)) {
    market[[var]]$Date = as.Date(format(as.Date(market[[var]]$Date, "%d-%m-%Y"), "20%y-%m-%d"))
}

marketData = data.frame(Date = seq(as.Date("2014-07-30"), as.Date("2017-07-17"), by = 1)) %>% left_join(left_join(market$Euribor_1Y, left_join(market$Euribor_3M, 
                                                                                                                                               left_join(market$Euribor_6M, left_join(market$Euro_UK_rate, left_join(market$Euro_US_rate, market$US_JPY_rate, by = "Date"), by = "Date"), 
                                                                                                                                                         by = "Date"), by = "Date"), by = "Date"), by = "Date")
marketData = xts(marketData[, 2:ncol(marketData)], order.by = marketData$Date)
marketData = na.locf(marketData)  # We impute the missing values (weekends, holidays) with the last value observed
print(apply(marketData, 2, function(x) sum(is.na(x))))  # No NA
remove(market)

# Format of the Crix data
load(paste(getwd(), "/Data_Format/CompleteData.RData", sep = ""))
# Missing value test
print(apply(crix, 2, function(x) sum(is.na(x))))
# 34% of missing values for eth eth has a lot of NAs, for now we delete it

# Endogene variables which are modelized one by one:
endogene = c("btc", "dash", "xrp", "xmr", "ltc", "doge", "nxt", "nmc")
crypto   = price[, endogene]
print(apply(crypto, 2, function(x) sum(is.na(x))))

# We keep the shared dates
prices = cbind(crix, cbind(crypto[time(crypto)[time(crypto) %in% time(marketData)]], marketData[time(marketData)[time(marketData) %in% time(crypto)]]))
colnames(prices)[1] = "crix"

# Bollinger intervals
ma = apply(prices, 2, function(x) sma(x, order = 30, silent = "graph"))  #moving average
xl = do.call(mapply, c("abind", ma, rev.along = 0))
sd = rollapply(prices, 30, stats::sd)
print(apply(sd, 2, function(x) sum(is.na(x))))

# We have NAs on the first rolling window, we replace it by the sd on the 29 first observations
for (var in colnames(sd)) {
    sd[is.na(sd[, var]), var] <- apply(prices[1:29, ], 2, stats::sd)[var]
}
middleBand  = xts(xl$fitted[, 1, ], order.by = time(prices))
upperBand   = middleBand + sd * 2
lowerBand   = middleBand - sd * 2

dimnames(upperBand)[[2]]   = paste("upperBand_", dimnames(upperBand)[[2]], sep = "")
dimnames(lowerBand)[[2]]   = paste("lowerBand_", dimnames(lowerBand)[[2]], sep = "")
dimnames(middleBand)[[2]]  = paste("middleBand_", dimnames(middleBand)[[2]], sep = "")
prices                     = cbind(prices, lowerBand, middleBand, upperBand)

print("Missing values test in \"prices\" table")
print(apply(prices, 2, function(x) sum(is.na(x))))
print("################ NO MISSING VALUE IN THE FINAL TABLE ################")
write.zoo(prices, file = paste(getwd(), "//CompleteData.csv", sep = ""), sep = ",")
rm(list = setdiff(ls(), "prices"))