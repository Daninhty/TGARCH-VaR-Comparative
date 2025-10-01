library(fGarch)
library(lmtest)

sigma <- garch_fit1@fit$sigma
return_data <- panel_a$Gold
quantile_1 <- qged(0.01, nu = coef(garch_fit1)[["shape"]])
quantile_5 <- qged(0.05, nu = coef(garch_fit1)[["shape"]])
mu_t <-  garch_fit1@fit$fitted.values 
VaR_11 <-  -mu_t -(sigma * quantile_1)
VaR_51 <-  -mu_t -(sigma * quantile_5)
VaR_data1 <- data.frame(
  Date = garch_fit1@model$modeldata$index,
  Return = return_data,
  VaR_1 = VaR_11,
  VaR_5 = VaR_51
)

sigma <- garch_fit2@fit$sigma
return_data <- panel_a$IHSG
quantile_1 <- qged(0.01, nu = coef(garch_fit2)[["shape"]])
quantile_5 <- qged(0.05, nu = coef(garch_fit2)[["shape"]])
mu_t <-  garch_fit2@fit$fitted.values 
VaR_12 <-  -mu_t -(sigma * quantile_1)
VaR_52 <-  -mu_t -(sigma * quantile_5)
VaR_data2 <- data.frame(
  Date = garch_fit2@model$modeldata$index,
  Return = return_data,
  VaR_1 = VaR_12,
  VaR_5 = VaR_52
)

sigma <- garch_fit3@fit$sigma
return_data <- panel_b$Gold
quantile_1 <- qged(0.01, nu = coef(garch_fit3)[["shape"]])
quantile_5 <- qged(0.05, nu = coef(garch_fit3)[["shape"]])
mu_t <-  garch_fit3@fit$fitted.values 
VaR_13 <-  -mu_t -(sigma * quantile_1)
VaR_53 <-  -mu_t -(sigma * quantile_5)
VaR_data3 <- data.frame(
  Date = garch_fit3@model$modeldata$index,
  Return = return_data,
  VaR_1 = VaR_13,
  VaR_5 = VaR_53
)

sigma <- garch_fit4@fit$sigma
return_data <- panel_b$IHSG
quantile_1 <- qged(0.01, nu = coef(garch_fit4)[["shape"]])
quantile_5 <- qged(0.05, nu = coef(garch_fit4)[["shape"]])
mu_t <-  garch_fit4@fit$fitted.values 
VaR_14 <-  -mu_t -(sigma * quantile_1)
VaR_54 <-  -mu_t -(sigma * quantile_5)
VaR_data4 <- data.frame(
  Date = garch_fit4@model$modeldata$index,
  Return = return_data,
  VaR_1 = VaR_14,
  VaR_5 = VaR_54
)

sigma <- garch_fit5@fit$sigma
return_data <- panel_c$Gold
quantile_1 <- qged(0.01, nu = coef(garch_fit5)[["shape"]])
quantile_5 <- qged(0.05, nu = coef(garch_fit5)[["shape"]])
mu_t <-  garch_fit5@fit$fitted.values 
VaR_15 <-  -mu_t -(sigma * quantile_1)
VaR_55 <-  -mu_t -(sigma * quantile_5)
VaR_data5 <- data.frame(
  Date = garch_fit5@model$modeldata$index,
  Return = return_data,
  VaR_1 = VaR_15,
  VaR_5 = VaR_55
)

sigma <- garch_fit6@fit$sigma
return_data <- panel_c$IHSG
quantile_1 <- qged(0.01, nu = coef(garch_fit6)[["shape"]])
quantile_5 <- qged(0.05, nu = coef(garch_fit6)[["shape"]])
mu_t <-  garch_fit6@fit$fitted.values 
VaR_16 <-  -mu_t -(sigma * quantile_1)
VaR_56 <-  -mu_t -(sigma * quantile_5)
VaR_data6 <- data.frame(
  Date = garch_fit6@model$modeldata$index,
  Return = return_data,
  VaR_1 = VaR_16,
  VaR_5 = VaR_56
)

sigma <- garch_fit7@fit$sigma
return_data <- panel_d$Gold
quantile_1 <- qged(0.01, nu = coef(garch_fit7)[["shape"]])
quantile_5 <- qged(0.05, nu = coef(garch_fit7)[["shape"]])
mu_t <-  garch_fit7@fit$fitted.values 
VaR_17 <-  -mu_t -(sigma * quantile_1)
VaR_57 <-  -mu_t -(sigma * quantile_5)
VaR_data7 <- data.frame(
  Date = garch_fit7@model$modeldata$index,
  Return = return_data,
  VaR_1 = VaR_17,
  VaR_5 = VaR_57
)

sigma <- garch_fit8@fit$sigma
return_data <- panel_d$IHSG
quantile_1 <- qged(0.01, nu = coef(garch_fit8)[["shape"]])
quantile_5 <- qged(0.05, nu = coef(garch_fit8)[["shape"]])
mu_t <-  garch_fit8@fit$fitted.values 
VaR_18 <-  -mu_t -(sigma * quantile_1)
VaR_58 <-  -mu_t -(sigma * quantile_5)
VaR_data8 <- data.frame(
  Date = garch_fit8@model$modeldata$index,
  Return = return_data,
  VaR_1 = VaR_18,
  VaR_5 = VaR_58
)

library(GAS)

total_downside <- nrow(VaR_data1) ####
VaR_data1$Violation_99 <- ifelse(VaR_data1$Return < -VaR_data1$VaR_1, 1, 0)
total_violations99 <- sum(VaR_data1$Violation_99)
total_violations99
failure_rate_downside99 <- (total_violations99 / total_downside)
failure_rate_downside99
VaR_data1$Violation_95 <- ifelse(VaR_data1$Return < -VaR_data1$VaR_5, 1, 0)
total_violations95 <- sum(VaR_data1$Violation_95)
total_violations95
failure_rate_downside95 <- (total_violations95 / total_downside)
failure_rate_downside95

bt_down_99 <- BacktestVaR(VaR_data1$Return, -VaR_data1$VaR_1, 0.01,20) ##
bt_down_95 <- BacktestVaR(VaR_data1$Return, -VaR_data1$VaR_5, 0.05,20) ##
bt_down_99[1:3]
bt_down_95[1:3]

total_downside <- nrow(VaR_data2)
VaR_data2$Violation_99 <- ifelse(VaR_data2$Return < -VaR_data2$VaR_1, 1, 0)
total_violations99 <- sum(VaR_data2$Violation_99)
total_violations99
failure_rate_downside99 <- (total_violations99 / total_downside)
failure_rate_downside99
VaR_data2$Violation_95 <- ifelse(VaR_data2$Return < -VaR_data2$VaR_5, 1, 0)
total_violations95 <- sum(VaR_data2$Violation_95)
total_violations95
failure_rate_downside95 <- (total_violations95 / total_downside)
failure_rate_downside95

bt_down_99 <- BacktestVaR(VaR_data2$Return, -VaR_data2$VaR_1, 0.01,20)
bt_down_95 <- BacktestVaR(VaR_data2$Return, -VaR_data2$VaR_5, 0.05,20)
bt_down_99[1:3]
bt_down_95[1:3]

total_downside <- nrow(VaR_data3)
VaR_data3$Violation_99 <- ifelse(VaR_data3$Return < -VaR_data3$VaR_1, 1, 0)
total_violations99 <- sum(VaR_data3$Violation_99)
total_violations99
failure_rate_downside99 <- (total_violations99 / total_downside)
failure_rate_downside99
VaR_data3$Violation_95 <- ifelse(VaR_data3$Return < -VaR_data3$VaR_5, 1, 0)
total_violations95 <- sum(VaR_data3$Violation_95)
total_violations95
failure_rate_downside95 <- (total_violations95 / total_downside)
failure_rate_downside95

bt_down_99 <- BacktestVaR(VaR_data3$Return, -VaR_data3$VaR_1, 0.01,20)
bt_down_95 <- BacktestVaR(VaR_data3$Return, -VaR_data3$VaR_5, 0.05,20)
bt_down_99[1:3]
bt_down_95[1:3]

total_downside <- nrow(VaR_data4)
VaR_data4$Violation_99 <- ifelse(VaR_data4$Return < -VaR_data4$VaR_1, 1, 0)
total_violations99 <- sum(VaR_data4$Violation_99)
total_violations99
failure_rate_downside99 <- (total_violations99 / total_downside)
failure_rate_downside99
VaR_data4$Violation_95 <- ifelse(VaR_data4$Return < -VaR_data4$VaR_5, 1, 0)
total_violations95 <- sum(VaR_data4$Violation_95)
total_violations95
failure_rate_downside95 <- (total_violations95 / total_downside)
failure_rate_downside95

bt_down_99 <- BacktestVaR(VaR_data4$Return, -VaR_data4$VaR_1, 0.01,20)
bt_down_95 <- BacktestVaR(VaR_data4$Return, -VaR_data4$VaR_5, 0.05,20)
bt_down_99[1:3]
bt_down_95[1:3]


total_downside <- nrow(VaR_data5)
VaR_data5$Violation_99 <- ifelse(VaR_data5$Return < -VaR_data5$VaR_1, 1, 0)
total_violations99 <- sum(VaR_data5$Violation_99)
total_violations99
failure_rate_downside99 <- (total_violations99 / total_downside)
failure_rate_downside99
VaR_data5$Violation_95 <- ifelse(VaR_data5$Return < -VaR_data5$VaR_5, 1, 0)
total_violations95 <- sum(VaR_data5$Violation_95)
total_violations95
failure_rate_downside95 <- (total_violations95 / total_downside)
failure_rate_downside95

bt_down_99 <- BacktestVaR(VaR_data5$Return, -VaR_data5$VaR_1, 0.01,20)
bt_down_95 <- BacktestVaR(VaR_data5$Return, -VaR_data5$VaR_5, 0.05,20)
bt_down_99[1:3]
bt_down_95[1:3]

total_downside <- nrow(VaR_data6)
VaR_data6$Violation_99 <- ifelse(VaR_data6$Return < -VaR_data6$VaR_1, 1, 0)
total_violations99 <- sum(VaR_data6$Violation_99)
total_violations99
failure_rate_downside99 <- (total_violations99 / total_downside)
failure_rate_downside99
VaR_data6$Violation_95 <- ifelse(VaR_data6$Return < -VaR_data6$VaR_5, 1, 0)
total_violations95 <- sum(VaR_data6$Violation_95)
total_violations95
failure_rate_downside95 <- (total_violations95 / total_downside)
failure_rate_downside95

bt_down_99 <- BacktestVaR(VaR_data6$Return, -VaR_data6$VaR_1, 0.01,20)
bt_down_95 <- BacktestVaR(VaR_data6$Return, -VaR_data6$VaR_5, 0.05,20)
bt_down_99[1:3]
bt_down_95[1:3]

total_downside <- nrow(VaR_data7)
VaR_data7$Violation_99 <- ifelse(VaR_data7$Return < -VaR_data7$VaR_1, 1, 0)
total_violations99 <- sum(VaR_data7$Violation_99)
total_violations99
failure_rate_downside99 <- (total_violations99 / total_downside)
failure_rate_downside99
VaR_data7$Violation_95 <- ifelse(VaR_data7$Return < -VaR_data7$VaR_5, 1, 0)
total_violations95 <- sum(VaR_data7$Violation_95)
total_violations95
failure_rate_downside95 <- (total_violations95 / total_downside)
failure_rate_downside95

bt_down_99 <- BacktestVaR(VaR_data7$Return, -VaR_data7$VaR_1, 0.01,20)
bt_down_95 <- BacktestVaR(VaR_data7$Return, -VaR_data7$VaR_5, 0.05,20)
bt_down_99[1:3]
bt_down_95[1:3]

total_downside <- nrow(VaR_data8)
VaR_data8$Violation_99 <- ifelse(VaR_data8$Return < -VaR_data8$VaR_1, 1, 0)
total_violations99 <- sum(VaR_data8$Violation_99)
total_violations99
failure_rate_downside99 <- (total_violations99 / total_downside)
failure_rate_downside99
VaR_data8$Violation_95 <- ifelse(VaR_data8$Return < -VaR_data8$VaR_5, 1, 0)
total_violations95 <- sum(VaR_data8$Violation_95)
total_violations95
failure_rate_downside95 <- (total_violations95 / total_downside)
failure_rate_downside95

bt_down_99 <- BacktestVaR(VaR_data8$Return, -VaR_data8$VaR_1, 0.01,20)
bt_down_95 <- BacktestVaR(VaR_data8$Return, -VaR_data8$VaR_5, 0.05,20)
bt_down_99[1:3]
bt_down_95[1:3]

#GRANGER

a <- VaR_11
b <- VaR_12
y <- b
x <- a
m5 <- grangertest(y ~ x, order = 5)
m10 <- grangertest(y ~ x, order = 10)
m20 <- grangertest(y ~ x, order = 20)
m5
m10
m20

var_list <- list(
  VaR_11 = VaR_11, VaR_12 = VaR_12, VaR_13 = VaR_13, VaR_14 = VaR_14,
  VaR_15 = VaR_15, VaR_16 = VaR_16, VaR_17 = VaR_17, VaR_18 = VaR_18,
  VaR_51 = VaR_51, VaR_52 = VaR_52, VaR_53 = VaR_53, VaR_54 = VaR_54,
  VaR_55 = VaR_55, VaR_56 = VaR_56, VaR_57 = VaR_57, VaR_58 = VaR_58
)

pairs <- list(
  c("VaR_11", "VaR_12"),
  c("VaR_13", "VaR_14"),
  c("VaR_15", "VaR_16"),
  c("VaR_17", "VaR_18"),
  c("VaR_51", "VaR_52"),
  c("VaR_53", "VaR_54"),
  c("VaR_55", "VaR_56"),
  c("VaR_57", "VaR_58")
)

run_granger_summary <- function(x, y, x_name, y_name, lag) {
  result1 <- grangertest(y ~ x, order = lag)
  result2 <- grangertest(x ~ y, order = lag)
  
  data.frame(
    COMEX = x_name,
    IHSG = y_name,
    Lag = lag,
    Direction = c(paste(x_name, "→", y_name), paste(y_name, "→", x_name)),
    F_statistic = c(result1$F[2], result2$F[2]),
    p_value = c(result1$`Pr(>F)`[2], result2$`Pr(>F)`[2])
  )
}

granger <- do.call(rbind, lapply(pairs, function(pair) {
  x_name <- pair[1]
  y_name <- pair[2]
  x <- var_list[[x_name]]
  y <- var_list[[y_name]]
  do.call(rbind, lapply(c(5, 10, 20), function(lag) {
    run_granger_summary(x, y, x_name, y_name, lag)
  }))
}))

granger
