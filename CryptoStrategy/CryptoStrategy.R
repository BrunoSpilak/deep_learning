# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Load and install packages if necessary
if (!require("xts")) install.packages("xts")
library("xts")

# Graphics
final.graph = function(data, var, model_type) {
    datepred = as.Date(time(data[[var]]$prediction))
    plot(as.numeric(prices[datepred, var]), type = "l", ylim = c(min(as.numeric(prices[datepred, var]), as.numeric(data[[var]]$prediction)), 
                                                                 max(as.numeric(prices[datepred, var]), as.numeric(data[[var]]$prediction))), ylab = "Prices", xaxt = "n", xlab = "Dates", main = paste("Prices and prices prediction for", 
                                                                                                                                                                                                        var, paste("with ", model_type, " Network", sep = ""), sep = " "))
    lines(as.numeric(data[[var]]$prediction), col = "red")
    lablist = datepred[c(seq(0, 364, by = 90) + 1, length(datepred))]
    # axis(1, at=c(seq(0,200,by=50),length(datepred)), labels = lablist)
    text(c(seq(0, 364, by = 90) + 1, length(datepred)), par("usr")[3] - 0.25, srt = 60, adj = 1, xpd = TRUE, labels = lablist, cex = 0.8)
    legend("topleft", legend = c("Real", "Predicted"), pch = 18, col = c("black", "red"), bty = "n")
}

#### Networks' performance
performance = function() {
    print("#Networks' performance")
    RMSE_elman_jordan = matrix(nrow = length(endogene), ncol = 3)
    colnames(RMSE_elman_jordan) = c("RMSE_Elman", "RMSE_Jordan", "Best_Model")
    rownames(RMSE_elman_jordan) = endogene
    for (var in endogene) {
        RMSE_elman_jordan[var, "RMSE_Jordan"] = finalmodel.jordan[[var]]$RMSE
        RMSE_elman_jordan[var, "RMSE_Elman"] = finalmodel.elman[[var]]$RMSE
    }
    RMSE_elman_jordan = as.data.frame(RMSE_elman_jordan)
    RMSE_elman_jordan[RMSE_elman_jordan$RMSE_Elman > RMSE_elman_jordan$RMSE_Jordan, "Best_Model"] = "Jordan"
    RMSE_elman_jordan[RMSE_elman_jordan$RMSE_Elman <= RMSE_elman_jordan$RMSE_Jordan, "Best_Model"] = "Elman"
    
    return(RMSE_elman_jordan)
}

#### Strategy
strategy = function() {
    quarterDate = rev(time(prices)[seq(from = length(time(prices)), to = length(time(prices)) - 3 * 90, by = -90)])
    comPrices = list()
    for (var in endogene) {
        comPrices[[var]] = as.data.frame(cbind(prices[quarterDate, var], finalmodel.jordan[[var]]$prediction[quarterDate[2:4]], finalmodel.elman[[var]]$prediction[quarterDate[2:4]]))
        colnames(comPrices[[var]]) = c(var, "jordan", "elman")
        comPrices[[var]]$jordanPortfolio[2:4] = (comPrices[[var]]$jordan[2:4] - as.numeric(comPrices[[var]][, var][1:3]))/as.numeric(comPrices[[var]][, 
                                                                                                                                                      var][1:3]) > 0.05
        comPrices[[var]]$elmanPortfolio[2:4] = (comPrices[[var]]$elman[2:4] - as.numeric(comPrices[[var]][, var][1:3]))/as.numeric(comPrices[[var]][, 
                                                                                                                                                    var][1:3]) > 0.05
    }
    portfolioJordan = list()
    for (var in endogene) {
        for (Q in rownames(comPrices[[var]])[which(comPrices[[var]]$jordanPortfolio)]) {
            portfolioJordan[[Q]] = c(portfolioJordan[[Q]], var)
        }
    }
    portfolioElman = list()
    for (var in endogene) {
        for (Q in rownames(comPrices[[var]])[which(comPrices[[var]]$elmanPortfolio)]) {
            portfolioElman[[Q]] = c(portfolioElman[[Q]], var)
        }
    }
    
    # Portfolio based on Jordan network's prediction
    JordanPortfolio = data.frame(matrix(ncol = length(quarterDate), nrow = length(c(endogene, "crix", "portfolio", "nn_portfolio_return", "Index_portfolio_return"))), 
                                 row.names = c(endogene, "crix", "portfolio", "nn_portfolio_return", "Index_portfolio_return"))
    colnames(JordanPortfolio) = quarterDate
    for (Q in colnames(JordanPortfolio)[2:4]) {
        for (var in portfolioJordan[[Q]]) {
            JordanPortfolio[var, Q] = comPrices[[var]][Q, var]
        }
    }
    JordanPortfolio["crix", ] = prices[colnames(JordanPortfolio), "crix"]
    JordanPortfolio[c(endogene, "crix"), 1] = t(prices[colnames(JordanPortfolio)[1], c(endogene, "crix")])
    JordanPortfolio["portfolio", ] = colSums(JordanPortfolio, na.rm = T)
    JordanPortfolio["nn_portfolio_return", 2:4] = (JordanPortfolio["portfolio", 2:4] - JordanPortfolio["portfolio", 1:3]) * 100/JordanPortfolio["portfolio", 
                                                                                                                                                1:3]
    JordanPortfolio["Index_portfolio_return", 2:4] = (JordanPortfolio["crix", 2:4] - JordanPortfolio["crix", 1:3]) * 100/JordanPortfolio["crix", 
                                                                                                                                         1:3]
    
    # Portfolio based on Elman network's prediction
    ElmanPortfolio = JordanPortfolio
    for (Q in colnames(ElmanPortfolio)[2:4]) {
        for (var in portfolioElman[[Q]]) {
            ElmanPortfolio[var, Q] = comPrices[[var]][Q, var]
        }
    }
    ElmanPortfolio["crix", ] = prices[colnames(ElmanPortfolio), "crix"]
    ElmanPortfolio[c(endogene, "crix"), 1] = t(prices[colnames(ElmanPortfolio)[1], c(endogene, "crix")])
    ElmanPortfolio["portfolio", ] = colSums(ElmanPortfolio, na.rm = T)
    ElmanPortfolio["nn_portfolio_return", 2:4] = (ElmanPortfolio["portfolio", 2:4] - ElmanPortfolio["portfolio", 1:3]) * 100/ElmanPortfolio["portfolio", 
                                                                                                                                            1:3]
    ElmanPortfolio["Index_portfolio_return", 2:4] = (ElmanPortfolio["crix", 2:4] - ElmanPortfolio["crix", 1:3]) * 100/ElmanPortfolio["crix", 
                                                                                                                                     1:3]
    portfolio = list(Elman = ElmanPortfolio, Jordan = JordanPortfolio)
    print("# Strategy with the two neural networks models #")
    print("# Nas mean the crypto is not included in the portfolio for this quarter #")
    print(portfolio)
    
    # Network structure
    structure = data.frame(Jordan = 1:length(endogene), Elman1 = 1:length(endogene), Elman2 = 1:length(endogene), Elman3 = 1:length(endogene), 
                           row.names = endogene)
    for (var in endogene) {
        structure[var, "Jordan"] = finalmodel.jordan[[var]]$model$archParams
        structure[var, "Elman1"] = finalmodel.elman[[var]]$model$archParams[[1]][1]
        structure[var, "Elman2"] = finalmodel.elman[[var]]$model$archParams[[1]][2]
        structure[var, "Elman3"] = finalmodel.elman[[var]]$model$archParams[[1]][3]
    }
    print("#Networks' structure #")
    print(structure)
    
    # Quarterly returns of a network portfolio and the index portfolio
    return.portfolio = list()
    return.portfolio$Jordan = portfolio$Jordan[c("nn_portfolio_return", "Index_portfolio_return"), 2:4]
    return.portfolio$Elman = portfolio$Elman[c("nn_portfolio_return", "Index_portfolio_return"), 2:4]
    print(" # Quarterly returns of jordan network portfolio and the index portfolio")
    print(return.portfolio$Jordan)
    print(" # Quaterly returns of Elman network portfolio and the index portfolio")
    print(return.portfolio$Elman)
    
    return(list(structure = structure, portfolio = portfolio, returns = return.portfolio))
}

# Load Data Load the final models
dir = getwd()
setwd("../")
load(paste(getwd(), "/RNNtraining/JordanNetwork/JordanNetwork.RData", sep = ""))
load(paste(getwd(), "/RNNtraining/ElmanNetwork/ElmanNetwork.RData", sep = ""))
setwd(dir)

# Load Data
prices    = read.csv2(paste(getwd(), "/Data.csv", sep = ""), sep = ",", dec = ".")
prices    = xts(prices[, -1], order.by = as.Date(prices[, 1]))
endogene  = c("btc", "dash", "xrp", "xmr", "ltc", "doge", "nxt", "nmc")
crix      = colnames(prices)[grepl("crix", colnames(prices))]
exogene   = c(colnames(prices)[grepl("Euribor", colnames(prices))], colnames(prices)[grepl("EUR", colnames(prices))])

# Graphs
for (var in endogene) {
    final.graph(finalmodel.elman, var, "Elman")
    final.graph(finalmodel.jordan, var, "Jordan")
}
performance()
strategy()