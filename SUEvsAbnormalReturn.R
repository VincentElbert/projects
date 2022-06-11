library(ggplot2)

setwd("C:/Users/vince/Desktop/task/FIN3080/project 3/number 2 data")

data <- read.csv("IAR_Rept.csv")
colnames(data) <- c("StockCode", "Company", "ReportType", "AccPer", "AnnDate", "AdjEarnings")

daily_return1.1 <- read.csv("TRD_Dalyr1.1.csv")
daily_return1.2 <- read.csv("TRD_Dalyr1.2.csv")
daily_return1.3 <- read.csv("TRD_Dalyr1.3.csv")
daily_return2.1 <- read.csv("TRD_Dalyr2.1.csv")
daily_return2.2 <- read.csv("TRD_Dalyr2.2.csv")
daily_return <- rbind(daily_return1.1, daily_return1.2, daily_return1.3,
                      daily_return2.1, daily_return2.2)
colnames(daily_return) <- c("StockCode", "TradingDate", "Return", "MarketType")

market_return <- read.csv("TRD_Cndalym.csv")
colnames(market_return) <- c("MarketType", "TradingDate", "Return")

data <- subset(data, ReportType %in% c(2, 4))
data$AccPer <- as.character(data$AccPer)
data$AnnDate <- as.character(data$AnnDate)

n_company <- unique(data$StockCode)
accounting_period <- unique(data$AccPer)
total_company_data <- data.frame()

# Step 2 and 3 calculation UE and SUE
for(i in n_company){
  company <- data[which(data$StockCode == i), ]
  data_per_company <- data.frame()
  
  #Calculate Unexpected Earnings
  for(j in 3:length(accounting_period)){
    subset_by_date_j <- subset(company, AccPer %in% accounting_period[c((j-2), j)])
    unexpected_earnings <- subset_by_date_j$AdjEarnings[2] - subset_by_date_j$AdjEarnings[1]
    company_j <- c(i, company$ReportType[j], company$AccPer[j], 
                   company$AnnDate[j], unexpected_earnings)
    data_per_company <- rbind(data_per_company, company_j)
  }  
  colnames(data_per_company) <- c("StockCode", "ReportType", "AccountingPeriod",
                                  "AnnouncementDate", "UnexpectedEarnings")
  na.omit(data_per_company)
  
  accounting_period_new <- unique(data_per_company$AccountingPeriod)
  #Calculate Standardized
  for(k in 4:nrow(data_per_company)){
    subset_by_date_k <- subset(data_per_company, AccountingPeriod %in% accounting_period_new[(k-3):k])
    if (nrow(subset_by_date_k) == 4){
      data_per_company$UnexpectedEarnings <- as.numeric(data_per_company$UnexpectedEarnings)
      standard_dev_earnings <- sd(data_per_company$UnexpectedEarnings[(k-3):k])
      standardized_earnings <- data_per_company$UnexpectedEarnings[k]/standard_dev_earnings
      company_k <- c(i, company$ReportType[k+2], company$AccPer[k+2], 
                     company$AnnDate[k+2], standardized_earnings)
      total_company_data <- rbind(total_company_data, company_k)
    }
  }
}
colnames(total_company_data) <- c("StockCode", "ReportType", "AccountingPeriod",
                                  "AnnouncementDate", "StandardizedEarnings")

total_company_data <- na.omit(total_company_data)
total_company_data$StandardizedEarnings <- as.numeric(total_company_data$StandardizedEarnings)

## Step 6a calculating abnormal return
abnormal_return_list <- c()
for(i in 1:nrow(daily_return)){
  print(i)
  index <- which(market_return$TradingDate == daily_return$TradingDate[i])
  abnormalreturn <- daily_return$Return[i] - market_return$Return[index]
  abnormal_return_list <- append(abnormal_return_list, abnormalreturn)
}
daily_return$AbnormalReturn <- abnormal_return_list

date <- unique(total_company_data$AccountingPeriod)

halfyear_timeseries <- data.frame()

for (i in date){
  subset_by_period <- total_company_data[which(total_company_data$AccountingPeriod == i), ]
  subset_by_period <- subset_by_period[order(subset_by_period$StandardizedEarnings, decreasing = TRUE),]
  
  comp_per_portfolio <- ceiling(nrow(subset_by_period)/10)
  sequence <- seq(1, nrow(subset_by_period), comp_per_portfolio)
  
  portfolio_avg_cum_ab_ret <- c()
  for (j in 1:10) {
    print(j)
    company_cum_ab_ret <- data.frame()
    if(j == 10){
      portfolio_i <- subset_by_period[sequence[j]:(nrow(subset_by_period)), ]
    }
    else{
      portfolio_i <- subset_by_period[sequence[j]:(sequence[j]+comp_per_portfolio-1), ]
    }
    for(k in 1:nrow(portfolio_i)){
      index <- which(daily_return$TradingDate == portfolio_i$AnnouncementDate[k]
                     & daily_return$StockCode == as.integer(portfolio_i$StockCode[k]))
      if(length(index) == 0){
        cum_ab_ret <- rep(NA, 241)
      }else{
        cum_ab_ret <- c(0)
        ret <- 0
        for(l in -120:120){
          ret <- ret + daily_return$AbnormalReturn[(index+l)]
          cum_ab_ret <- append(cum_ab_ret, ret)
        }
        company_cum_ab_ret <- rbind(company_cum_ab_ret, cum_ab_ret)
      }
    }
    portfolio_avg_cum_ab_ret <- colMeans(company_cum_ab_ret, na.rm = TRUE)
    halfyear_timeseries <- rbind(halfyear_timeseries, portfolio_avg_cum_ab_ret)
  } 
}

final_timeseries <- data.frame()
sequence <- seq(1, 101, 10)
for(i in 0:9){
  halfyear_i <- halfyear_timeseries[(sequence+i),]
  final_timeseries <- rbind(final_timeseries, colMeans(halfyear_i))
}

final_timeseries <- t(final_timeseries)
final_timeseries <- as.data.frame(final_timeseries)

colnames(final_timeseries) <- c("P1", "P2", "P3", "P4", "P5",
                                "P6", "P7", "P8", "P9", "P10")
rownames(final_timeseries) <- c(1:242)

plot_return <- ggplot() + geom_line(aes(x = c(1:242), y = P1, 
                                        colour = "Portfolio 1"), data = final_timeseries, group = 1) + 
  geom_point() + geom_line(aes(x = c(1:242), y = P2, colour = "Portfolio 2"), 
                           data = final_timeseries, group = 1) +
  geom_point() + geom_line(aes(x = c(1:242), y = P3, colour = "Portfolio 3"), 
                           data = final_timeseries, group = 1) +
  geom_point() + geom_line(aes(x = c(1:242), y = P4, colour = "Portfolio 4"), 
                           data = final_timeseries, group = 1) +
  geom_point() + geom_line(aes(x = c(1:242), y = P5, colour = "Portfolio 5"), 
                           data = final_timeseries, group = 1) +
  geom_point() + geom_line(aes(x = c(1:242), y = P6, colour = "Portfolio 6"), 
                           data = final_timeseries, group = 1) +
  geom_point() + geom_line(aes(x = c(1:242), y = P7, colour = "Portfolio 7"), 
                           data = final_timeseries, group = 1) +
  geom_point() + geom_line(aes(x = c(1:242), y = P8, colour = "Portfolio 8"), 
                           data = final_timeseries, group = 1) +
  geom_point() + geom_line(aes(x = c(1:242), y = P9, colour = "Portfolio 9"), 
                           data = final_timeseries, group = 1) +
  geom_point() + geom_line(aes(x = c(1:242), y = P10, colour = "Portfolio 10"), 
                           data = final_timeseries, group = 1) +
  scale_colour_manual("", breaks = c("Portfolio 1", "Portfolio 2", "Portfolio 3",
                                     "Portfolio 4", "Portfolio 5", "Portfolio 6",
                                     "Portfolio 7", "Portfolio 8", "Portfolio 9",
                                     "Portfolio 10"), 
                      values = c("#FF0000", "#0089FF", "#1B9E77", "#D95F02",
                                 "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
                                 "#A6761D", "#666666")) +
  labs(title = "Cumulative Abnormal Return \n Time Series Based on SUE Rank", y = "Cumulative Abnormal Return",
       x = "Time Series") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(color = "black", size = 9, angle = 50, 
                                   vjust = 0.8, hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 10))) + theme(
          legend.position = "right",
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(0.1, 0.1, 0.1 , 0.1))
plot_return
