library(moments)
library(ggplot2)

# daily return data
daily1.1 <- read.csv("TRD_Dalyr1.1.csv")
daily1.2 <- read.csv("TRD_Dalyr1.2.csv")
daily2.1 <- read.csv("TRD_Dalyr2.1.csv")
daily2.2 <- read.csv("TRD_Dalyr2.2.csv")
daily2.3 <- read.csv("TRD_Dalyr2.3.csv")
daily3.1 <- read.csv("TRD_Dalyr3.1.csv")
daily3.2 <- read.csv("TRD_Dalyr3.2.csv")
daily <- rbind(daily1.1, daily1.2, daily2.1, daily2.2, 
               daily2.3, daily3.1, daily3.2)
colnames(daily) <- c("Index", "Date", "Return")
daily$Date <- as.Date(daily$Date, format = "%Y-%m-%d")
daily$Month <- format(daily$Date, format = "%Y-%m")

#rf data
rf <- read.csv("MBK_ShiborM.csv")
colnames(rf) <- c("Date", "Term", "Rf")
rf <- subset(rf, Term == "1 day") # extracting only the overnight
rf$Rf <- rf$Rf/12
rf$Date <- as.Date(rf$Date, format = "%Y-%m-%d")
rf$Month <- format(rf$Date, "%Y-%m")
rf_month <- unique(rf$Month)
# calculating rf
monthly_rf <- c()
for(i in rf_month){
  index <- which(rf$Month == i)
  monthly_rf <- append(monthly_rf, mean(rf$Rf[index]))
}
rf <- data.frame(Month = rf_month, Rf = monthly_rf)

#ff data
ff <- read.csv("STK_MKT_ThrfacDay.csv")[-c(1,2),2:5]
colnames(ff) <- c("TradingDate", "RmRf", "SMB", "HML")
ff$TradingDate <- as.Date(ff$TradingDate, format = "%Y-%m-%d")
ff$Month <- format(ff$TradingDate, "%Y-%m")
ff_month <- unique(ff$Month)
monthly_rmrf <- c()
monthly_smb <- c()
monthly_hml <- c()
for(i in ff_month){
  index <- which(ff$Month == i)
  monthly_rmrf <- append(monthly_rmrf, mean(ff$RmRf[index]))
  monthly_smb <- append(monthly_smb, mean(ff$SMB[index]))
  monthly_hml <- append(monthly_hml, mean(ff$HML[index]))
}
ff <- data.frame(Month = rf_month, RmRf = monthly_rmrf, SMB = monthly_smb, HML = monthly_hml)

month <- unique(daily$Month)
portfolio_skew <- data.frame()
portfolio_kurt <- data.frame()
for(i in 4:length(month)){
  past_months <- daily[which(daily$Month %in% month[(i-3):(i-1)]),]
  current_month <- daily[which(daily$Month == month[i]),]
  
  month_data <- data.frame(Index = integer(), Skewness = numeric(), 
                           Kurtosis = numeric(), Return = numeric())
  
  company <- unique(daily$Index)
  for(j in company){
    company_past <- past_months[which(past_months$Index == j),]
    company_current <- current_month[which(current_month$Index == j),]
    # skewness for 4 months
    skewness <- skewness(company_past$Return)
    kurtosis <- kurtosis(company_past$Return)
    
    avg_ret <- mean(company_current$Return)
    
    company_j <- c(j, skewness, kurtosis, avg_ret)
    month_data <- rbind(month_data, company_j)
  }
  month_data <- na.omit(month_data)
  colnames(month_data) <- c("Index", "Skewness", "Kurtosis", "Return")
  skew_rank <- month_data[order(month_data$Skewness, decreasing = FALSE), ]
  kurt_rank <- month_data[order(month_data$Kurtosis, decreasing = FALSE), ]
  
  comp_per_portfolio <- ceiling(nrow(month_data)/10)
  sequence <- seq(1, nrow(month_data), comp_per_portfolio)
  monthly_skew_portfolio <- c(month[i])
  monthly_kurt_portfolio <- c(month[i])
  for (k in 1:10) {
    if (k == 10) {
      skew_portfolio <- skew_rank[sequence[k]:nrow(skew_rank), ]
      kurt_portfolio <- kurt_rank[sequence[k]:nrow(kurt_rank), ]
    }
    else {
      skew_portfolio <- skew_rank[sequence[k]:sequence[k]+comp_per_portfolio-1, ]
      kurt_portfolio <- kurt_rank[sequence[k]:sequence[k]+comp_per_portfolio-1, ]
    }
    monthly_skew_portfolio <- append(monthly_skew_portfolio, mean(skew_portfolio$Return))
    monthly_kurt_portfolio <- append(monthly_kurt_portfolio, mean(kurt_portfolio$Return))
  }
  portfolio_skew <- rbind(portfolio_skew, monthly_skew_portfolio)
  portfolio_kurt <- rbind(portfolio_kurt, monthly_kurt_portfolio)
}
colnames(portfolio_skew) <- c("Month", "P1", "P2", "P3", "P4", "P5",
                              "P6", "P7", "P8", "P9", "P10")
portfolio_skew["P1"] <- as.numeric(unlist(portfolio_skew["P1"]))
portfolio_skew["P2"] <- as.numeric(unlist(portfolio_skew["P2"]))
portfolio_skew["P3"] <- as.numeric(unlist(portfolio_skew["P3"]))
portfolio_skew["P4"] <- as.numeric(unlist(portfolio_skew["P4"]))
portfolio_skew["P5"] <- as.numeric(unlist(portfolio_skew["P5"]))
portfolio_skew["P6"] <- as.numeric(unlist(portfolio_skew["P6"]))
portfolio_skew["P7"] <- as.numeric(unlist(portfolio_skew["P7"]))
portfolio_skew["P8"] <- as.numeric(unlist(portfolio_skew["P8"]))
portfolio_skew["P9"] <- as.numeric(unlist(portfolio_skew["P9"]))
portfolio_skew["P10"] <- as.numeric(unlist(portfolio_skew["P10"]))

colnames(portfolio_kurt) <- c("Month", "P1", "P2", "P3", "P4", "P5",
                              "P6", "P7", "P8", "P9", "P10")
portfolio_kurt["P1"] <- as.numeric(unlist(portfolio_kurt["P1"]))
portfolio_kurt["P2"] <- as.numeric(unlist(portfolio_kurt["P2"]))
portfolio_kurt["P3"] <- as.numeric(unlist(portfolio_kurt["P3"]))
portfolio_kurt["P4"] <- as.numeric(unlist(portfolio_kurt["P4"]))
portfolio_kurt["P5"] <- as.numeric(unlist(portfolio_kurt["P5"]))
portfolio_kurt["P6"] <- as.numeric(unlist(portfolio_kurt["P6"]))
portfolio_kurt["P7"] <- as.numeric(unlist(portfolio_kurt["P7"]))
portfolio_kurt["P8"] <- as.numeric(unlist(portfolio_kurt["P8"]))
portfolio_kurt["P9"] <- as.numeric(unlist(portfolio_kurt["P9"]))
portfolio_kurt["P10"] <- as.numeric(unlist(portfolio_kurt["P10"]))

#long short portfolio
portfolio_skew$P11 <- 1.3 * portfolio_skew[,11] - 0.3 * portfolio_skew[,2]
portfolio_kurt$P11 <- 1.3 * portfolio_kurt[,11] - 0.3 * portfolio_kurt[,2]

portfolio_skew_month <- unique(portfolio_skew$Month)
alpha_portfolio_skew <- c()
for(i in 2:ncol(portfolio_skew)){
  rm_rf <- c()
  smb <- c()
  hml <- c()
  for(j in 1:length(portfolio_skew_month)){
    ri_rf <- append(rm_rf, (portfolio_skew[j, i] - rf$Rf[which(rf$Month == portfolio_skew_month[j])]))
    rm_rf <- append(rm_rf, ff$RmRf[which(ff$Month == portfolio_skew_month[j])])
    smb <- append(smb, ff$SMB[which(ff$Month == portfolio_skew_month[j])])
    hml <- append(hml, ff$HML[which(ff$Month == portfolio_skew_month[j])])
  }
  regression <- lm(ri_rf ~ rm_rf + smb + hml)
  alpha <- coef(regression)[1]
  alpha_portfolio_skew <- append(alpha_portfolio_skew, alpha)
}
print(alpha_portfolio_skew)

portfolio_kurt_month <- unique(portfolio_kurt$Month)
alpha_portfolio_kurt <- c()
for(i in 2:ncol(portfolio_kurt)){
  rm_rf <- c()
  smb <- c()
  hml <- c()
  for(j in 1:length(portfolio_kurt_month)){
    ri_rf <- append(rm_rf, (portfolio_kurt[j, i] - rf$Rf[which(rf$Month == portfolio_kurt_month[j])]))
    rm_rf <- append(rm_rf, ff$RmRf[which(ff$Month == portfolio_kurt_month[j])])
    smb <- append(smb, ff$SMB[which(ff$Month == portfolio_kurt_month[j])])
    hml <- append(hml, ff$HML[which(ff$Month == portfolio_kurt_month[j])])
  }
  regression <- lm(ri_rf ~ rm_rf + smb + hml)
  alpha <- coef(regression)[1]
  alpha_portfolio_kurt <- append(alpha_portfolio_kurt, alpha)
}
print(alpha_portfolio_kurt)

#finding cumulative average monthly return for each portfolio
cum_portfolio_skew <- data.frame()
for(i in 1:nrow(portfolio_skew)){
  if(i == 1){
    cum_portfolio_skew <- portfolio_skew[i,2:12]
  }else{
    cumulative <- cum_portfolio_skew[i-1,] + portfolio_skew[i,2:12]
    cum_portfolio_skew <- rbind(cum_portfolio_skew, cumulative)
  }
}
rownames(cum_portfolio_skew) <- c(1:nrow(cum_portfolio_skew))
cum_portfolio_skew <- cbind(cum_portfolio_skew, portfolio_skew["Month"])

cum_portfolio_kurt <- data.frame()
for(i in 1:nrow(portfolio_kurt)){
  if(i == 1){
    cum_portfolio_kurt <- portfolio_kurt[i,2:12]
  }else{
    cumulative <- cum_portfolio_kurt[i-1,] + portfolio_kurt[i,2:12]
    cum_portfolio_kurt <- rbind(cum_portfolio_kurt, cumulative)
  }
}
rownames(cum_portfolio_kurt) <- c(1:nrow(cum_portfolio_kurt))
cum_portfolio_kurt <- cbind(cum_portfolio_kurt, portfolio_kurt["Month"])

# plotting the cumulative return 
plot_return <- ggplot() + geom_line(aes(x = Month, y = P1, 
                                        colour = "Portfolio 1"), data = cum_portfolio_skew, group = 1) + 
  geom_point() + geom_line(aes(x = Month, y = P2, colour = "Portfolio 2"), 
                           data = cum_portfolio_skew, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P3, colour = "Portfolio 3"), 
                           data = cum_portfolio_skew, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P4, colour = "Portfolio 4"), 
                           data = cum_portfolio_skew, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P5, colour = "Portfolio 5"), 
                           data = cum_portfolio_skew, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P6, colour = "Portfolio 6"), 
                           data = cum_portfolio_skew, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P7, colour = "Portfolio 7"), 
                           data = cum_portfolio_skew, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P8, colour = "Portfolio 8"), 
                           data = cum_portfolio_skew, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P9, colour = "Portfolio 9"), 
                           data = cum_portfolio_skew, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P10, colour = "Portfolio 10"), 
                           data = cum_portfolio_skew, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P11, colour = "Portfolio 11"), 
                           data = cum_portfolio_skew, group = 1) +
  scale_colour_manual("", breaks = c("Portfolio 1", "Portfolio 2", "Portfolio 3",
                                     "Portfolio 4", "Portfolio 5", "Portfolio 6",
                                     "Portfolio 7", "Portfolio 8", "Portfolio 9",
                                     "Portfolio 10", "Portfolio 11"), 
                      values = c("#FF0000", "#0089FF", "#1B9E77", "#D95F02",
                                 "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
                                 "#A6761D", "#666666", "#00AFBB")) +
  labs(title = "Cumulative Monthly Average Return", y = "Return", x = "Year - Month") +
  theme(plot.title = element_text(hjust = 0.5, ), 
        axis.text.x = element_text(color = "black", size = 6, angle = 90, 
                                   vjust = 0.8, hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 10))) + theme(
          legend.position = "right",
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(0.1, 0.1, 0.1 , 0.1))
plot_return

plot_return <- ggplot() + geom_line(aes(x = Month, y = P1, 
                                        colour = "Portfolio 1"), data = cum_portfolio_kurt, group = 1) + 
  geom_point() + geom_line(aes(x = Month, y = P2, colour = "Portfolio 2"), 
                           data = cum_portfolio_kurt, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P3, colour = "Portfolio 3"), 
                           data = cum_portfolio_kurt, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P4, colour = "Portfolio 4"), 
                           data = cum_portfolio_kurt, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P5, colour = "Portfolio 5"), 
                           data = cum_portfolio_kurt, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P6, colour = "Portfolio 6"), 
                           data = cum_portfolio_kurt, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P7, colour = "Portfolio 7"), 
                           data = cum_portfolio_kurt, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P8, colour = "Portfolio 8"), 
                           data = cum_portfolio_kurt, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P9, colour = "Portfolio 9"), 
                           data = cum_portfolio_kurt, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P10, colour = "Portfolio 10"), 
                           data = cum_portfolio_kurt, group = 1) +
  geom_point() + geom_line(aes(x = Month, y = P11, colour = "Portfolio 11"), 
                           data = cum_portfolio_kurt, group = 1) +
  scale_colour_manual("", breaks = c("Portfolio 1", "Portfolio 2", "Portfolio 3",
                                     "Portfolio 4", "Portfolio 5", "Portfolio 6",
                                     "Portfolio 7", "Portfolio 8", "Portfolio 9",
                                     "Portfolio 10", "Portfolio 11"), 
                      values = c("#FF0000", "#0089FF", "#1B9E77", "#D95F02",
                                 "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
                                 "#A6761D", "#666666", "#00AFBB")) +
  labs(title = "Cumulative Monthly Average Return", y = "Return", x = "Year - Month") +
  theme(plot.title = element_text(hjust = 0.5, ), 
        axis.text.x = element_text(color = "black", size = 6, angle = 90, 
                                   vjust = 0.8, hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 10))) + theme(
          legend.position = "right",
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(0.1, 0.1, 0.1 , 0.1))
plot_return

# plotting alpha for each portfolio
plot(x = c(1:11), y = unique(alpha_portfolio_skew), pch=21, bg = "#FF0000",
     xlab = "Portfolio Number Ranked by Skewness", ylab = "Abnormal Returns",
     main = "Abnormal Returns Ranked with Fama-French Three Factors Model", 
     cex.main = 0.8, ylim = c(-0.0008,-0.0008))
text(x = c(1:11), y = unique(alpha_portfolio_skew), 
     round(unique(alpha_portfolio_skew),6), cex=0.45, pos = c(1,1.1))

plot(x = c(1:11), y = unique(alpha_portfolio_kurt), pch=21, bg = "#FF0000",
     xlab = "Portfolio Number Ranked by Kurtosis", ylab = "Abnormal Returns",
     main = "Abnormal Returns Ranked with Fama-French Three Factors Model", 
     cex.main = 0.8, ylim = c(-0.0015,-0.00009))
text(x = c(1:11), y = unique(alpha_portfolio_kurt), 
     round(unique(alpha_portfolio_kurt),6), cex=0.45,pos = c(1,1.1))
