# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Load Packages and install packages if necessary
if (!require("xts")) install.packages("xts")
library("xts")
if (!require("doParallel")) install.packages("doParallel")
library("doParallel")
if (!require("RSNNS")) install.packages("RSNNS")
library("RSNNS")

# Normalization + formatting
dataNormalization = function(data, type) {
  norm_prices = normalizeData(data, type)
  colnames(norm_prices) = colnames(data)
  norm_prices = as.xts(norm_prices, order.by = time(data))
  return(norm_prices)
}

# Training function:
crixModel = function(var, model_type, size, maxit = 1000) {
  # Index for data partition for train and test sets: We want to test the data on a whole year
  test_time   = seq(end(prices) - 364, end(prices), by = 1)
  train_time  = time(prices)[!time(prices) %in% test_time]
  
  # Train and test sets:
  intvar = colnames(prices)[grepl(var, colnames(prices))]  #Response variable and Bollinger bands
  
  # Normalization
  train_ = dataNormalization(prices[train_time, c(intvar, crix, exogene)], "0_1")
  test_  = dataNormalization(prices[test_time, c(intvar, crix, exogene)], getNormParameters(train_))
  
  # train the model
  model  = model_type(train_, train_[, var], size = size, maxit = maxit, shufflePatterns = FALSE, linOut = TRUE)
  
  # We obtain the normalized price
  normprice_pred = predict(model, test_)
  plot(as.numeric(test_[, var]), type = "l", ylab = "Normalized Prices", xaxt = "n", main = paste("Normalized prices and Normalized prices prediction for", 
                                                                                                  var, sep = " "))
  lines(normprice_pred, col = "red")
  lablist = time(normprice_pred)[seq(0, 364, by = 50) + 1]
  axis(1, at = seq(0, 364, by = 50), labels = lablist)
  legend("topleft", legend = c("Real", "Predicted"), pch = 18, col = c("black", "red"), bty = "n")
  
  # Final prices
  price_pred = denormalizeData(normprice_pred, getNormParameters(train_))
  price_pred = as.xts(price_pred, order.by = time(test_))
  
  # Plot
  plot(as.numeric(prices[time(price_pred), var]), type = "l", ylim = c(min(as.numeric(prices[time(price_pred), var]), as.numeric(price_pred)), 
                                                                       max(as.numeric(prices[time(price_pred), var]), as.numeric(price_pred))), ylab = "Prices", xaxt = "n", xlab = "Dates", main = paste("Prices and prices prediction for", 
                                                                                                                                                                                                          var, sep = " "))
  lines(as.numeric(price_pred), col = "red")
  lablist = time(price_pred)[seq(0, 364, by = 50) + 1]
  axis(1, at = seq(0, 364, by = 50), labels = lablist)
  legend("topleft", legend = c("Real", "Predicted"), pch = 18, col = c("black", "red"), bty = "n")
  
  # We compute the RMSE for the model selection
  RMSE = sqrt(sum((price_pred - prices[time(test_), var])^2)/nrow(price_pred))
  
  return(list(model = model, RMSE = RMSE, prediction = price_pred))
}
# Parameters tuning : Tuning of the number of neurons
jordanTuning = function(neurons, iteration) {
  nrOfCores = detectCores() - 1
  registerDoParallel(cores = nrOfCores)
  message(paste("\n Registered number of cores:\n", nrOfCores, "\n"))
  
  startf = Sys.time()
  set.seed(825)
  model.jordan = list()
  finalmodel.jordan = list()
  
  for (var in endogene) {
    start = Sys.time()
    set.seed(825)
    model.jordan[[var]] = data.frame(Size = 1:neurons, RMSE = 1:neurons)
    for (size in 1:neurons) {
      model.jordan[[var]][size, 2] = crixModel(var, jordan, size, iteration)$RMSE
      print(paste(var, ": Size = ", size, sep = ""))
    }
    print(model.jordan[[var]][model.jordan[[var]][, 2] == min(model.jordan[[var]][, 2]), ])
    finalmodel.jordan[[var]] = crixModel(var, jordan, c(model.jordan[[var]][model.jordan[[var]][, 2] == min(model.jordan[[var]][, 2]), 
                                                                            1]), iteration)
    end = Sys.time()
    print(paste("Time for ", var, "= ", end - start, sep = ""))
  }
  save(finalmodel.jordan, file = paste(getwd(), "/JordanNetwork.RData", sep = ""))
  endf = Sys.time()
  print(paste("Total time = ", endf - startf, sep = ""))
  
}
# Load Data and variables' definition
prices    = read.csv2(paste(getwd(), "/Data.csv", sep = ""), sep = ",", dec = ".")
prices    = xts(prices[, -1], order.by = as.Date(prices[, 1]))
endogene  = c("btc", "dash", "xrp", "xmr", "ltc", "doge", "nxt", "nmc")
crix      = colnames(prices)[grepl("crix", colnames(prices))]
exogene   = c(colnames(prices)[grepl("Euribor", colnames(prices))], colnames(prices)[grepl("EUR", colnames(prices))])

# Missing value test prices is a clean data set without any missing values
print("Missing values test in \"prices\" table")
test = apply(prices, 2, function(x) sum(is.na(x)))
if (unique(test) != 0) {
  print(paste("Missing value in ", names(test[test != 0]), sep = ""))
} else {
  print("NO MISSING VALUE")
}

# Model tuning and training:
maxit   = 10000 # The bigger is the number of iteration, the more stable the model is
layer1  = 40    # c(layer1, layer2, layer3) = gridsearch for parameters tuning
layer2  = 20
layer3  = 20

# Elman network tuning
elmanTuning(layer1, layer2, layer3, maxit)
# Jordan network tuning: only one layer
jordanTuning(layer1, maxit)
