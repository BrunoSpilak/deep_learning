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
elmanTuning = function(neuron1, neuron2, neuron3, iteration) {
  nrOfCores = detectCores() - 1
  registerDoParallel(cores = nrOfCores)
  message(paste("\n Registered number of cores:\n", nrOfCores, "\n"))
  
  startf = Sys.time()
  set.seed(825)
  model1.elman = list()
  finalmodel1.elman = list()
  model2.elman = list()
  finalmodel2.elman = list()
  model3.elman = list()
  finalmodel3.elman = list()
  
  for (var in endogene) {
    start = Sys.time()
    set.seed(825)
    model1.elman[[var]] = data.frame(Layer = 1:neuron1, RMSE = 1:neuron1)
    for (layer1 in 1:neuron1) {
      model1.elman[[var]][layer1, 2] = crixModel(var, elman, c(layer1), iteration)$RMSE
      print(paste(var, ": Layer1 = ", layer1, sep = ""))
    }
    print(model1.elman[[var]][model1.elman[[var]][, 2] == min(model1.elman[[var]][, 2]), ])
    finalmodel1.elman[[var]] = crixModel(var, elman, c(model1.elman[[var]][model1.elman[[var]][, 2] == min(model1.elman[[var]][, 2]), 1]), 
                                         10000)
    end = Sys.time()
    print(paste("Time for ", var, " layer1 = ", end - start, sep = ""))
    
    start = Sys.time()
    set.seed(825)
    
    model2.elman[[var]] = data.frame(Layer = 1:neuron2, RMSE = 1:neuron2)
    for (layer2 in 1:neuron2) {
      model2.elman[[var]][layer2, 2] = crixModel(var, elman, c(finalmodel1.elman[["btc"]]$model$archParams$size, layer2), 10000)$RMSE
      print(paste(var, ": Layer2 = ", layer2, sep = ""))
    }
    print(model2.elman[[var]][model2.elman[[var]][, 2] == min(model2.elman[[var]][, 2]), ])
    finalmodel2.elman[[var]] = crixModel(var, elman, c(finalmodel1.elman[["btc"]]$model$archParams$size, model2.elman[[var]][model2.elman[[var]][, 
                                                                                                                                                 2] == min(model2.elman[[var]][, 2]), 1]), iteration)
    end = Sys.time()
    print(paste("Time for ", var, " layer2 = ", end - start, sep = ""))
    
    start = Sys.time()
    set.seed(825)
    
    model3.elman[[var]] = data.frame(Layer = 1:neuron3, RMSE = 1:neuron3)
    for (layer3 in 1:neuron3) {
      model3.elman[[var]][layer3, 2] = crixModel(var, elman, c(finalmodel2.elman[["btc"]]$model$archParams$size, layer3), 10000)$RMSE
      print(paste(var, ": Layer3 = ", layer3, sep = ""))
    }
    print(model3.elman[[var]][model3.elman[[var]][, 2] == min(model3.elman[[var]][, 2]), ])
    finalmodel3.elman[[var]] = crixModel(var, elman, c(finalmodel2.elman[["btc"]]$model$archParams$size, model3.elman[[var]][model3.elman[[var]][, 
                                                                                                                                                 2] == min(model3.elman[[var]][, 2]), 1]), iteration)
    end = Sys.time()
    print(paste("Time for ", var, " layer3 = ", end - start, sep = ""))
  }
  save(finalmodel3.elman, file = paste(getwd(), "/ElmanNetwork.RData", sep = ""))
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
