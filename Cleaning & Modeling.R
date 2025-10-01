library(quantmod)
library(ggplot2)
library(dplyr)
library(tseries)
library(fBasics)
library(lmtest)
library(rugarch)
library(rmgarch)
library(FinTS)
library(tstests)
library(fGarch)
library(GAS)

getSymbols(c("GC=F", "^JKSE"), from = "2001-01-01", to = "2025-1-1")

datatgold <- na.omit(Cl(`GC=F`))
datatgold_df <- data.frame(Date = index(datatgold), coredata(datatgold))
datatgold_df <- datatgold_df %>% mutate(Year = as.numeric(format(Date, "%Y")))
datatihsg <- na.omit(Cl(JKSE))
datatihsg_df <- data.frame(Date = index(datatihsg), coredata(datatihsg))
datatihsg_df <- datatihsg_df %>% mutate(Year = as.numeric(format(Date, "%Y")))

ggplot(data = datatgold_df, aes(x = Date, y = GC.F.Close)) +
  geom_line(color = "#2464b8") +
  labs(
    title = "COMEX",           
    x = "Year",                     
    y = "USD per Troy Ounce"        
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0, max(datatgold_df$GC.F.Close, na.rm = TRUE), 500)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
  )

ggplot(data = datatihsg_df, aes(x = Date, y = JKSE.Close)) +
  geom_line(color = "#2464b8") +
  labs(
    title = "IHSG",           
    x = "Year",                     
    y = "Point"        
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0, max(datatihsg_df$JKSE.Close, na.rm = TRUE), 1500)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
  )

gcf_return <- diff(log(Cl(`GC=F`)))*100
gcf_return <- na.omit(gcf_return)
ihsg_return <- diff(log(Cl(JKSE)))*100
ihsg_return <- na.omit(ihsg_return)

data <- na.omit(merge(gcf_return, ihsg_return))
colnames(data) <- c("Gold", "IHSG")
data_df <- data.frame(Date = index(data), coredata(data))
data_df <- data_df %>% mutate(Year = as.numeric(format(Date, "%Y")))

panel_a <- data_df
panel_b <- data_df %>% filter(Date >= "2001-01-01" & Date <= "2007-06-29")
panel_c <- data_df %>% filter(Date >= "2007-07-02" & Date <= "2019-12-12")
panel_d <- data_df %>% filter(Date >= "2019-12-13" & Date <= "2025-1-1")

ggplot(data = panel_a, aes(x = Date, y = Gold)) +
  geom_line(color = "#2464b8") +
  labs(
    title = "Return COMEX",           
    x = "Year",                     
    y = "Return (%)"        
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = c(0, 0)) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
  )

ggplot(data = panel_a, aes(x = Date, y = IHSG)) +
  geom_line(color = "#2464b8") +
  labs(
    title = "Return IHSG",           
    x = "Tahun",                     
    y = "Return (%)"        
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = c(0, 0)) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
  )

calc_stats <- function(data) {
  mean_val <- mean(data, na.rm = TRUE)
  sd_val <- sd(data, na.rm = TRUE)
  skewness_val <- skewness(data, na.rm = TRUE)
  kurtosis_val <- kurtosis(data, na.rm = TRUE)
  
  stats <- data.frame(
    Mean = mean_val,
    Std_Dev = sd_val,
    Skewness = skewness_val,
    Kurtosis = kurtosis_val
  )
  
  return(stats)
}

#a
stats_a_gold <- calc_stats(panel_a$Gold)
stats_a_gold
jarque.bera.test(panel_a$Gold)
lbq20_a_gold <- Box.test(panel_a$Gold, lag = 20, type = "Ljung-Box")
lbq20_a_gold
archtest_a_gold <- ArchTest(panel_a$Gold, lag = 20)
archtest_a_gold
adf_a_gold <- adf.test(panel_a$Gold)
adf_a_gold

stats_a_ihsg <- calc_stats(panel_a$IHSG)
stats_a_ihsg
jarque.bera.test(panel_a$IHSG)
lbq20_a_ihsg <- Box.test(panel_a$IHSG, lag = 20, type = "Ljung-Box")
lbq20_a_ihsg
archtest_a_ihsg <- ArchTest(panel_a$IHSG, lag = 20)
archtest_a_ihsg
adf_a_ihsg <- adf.test(panel_a$IHSG)
adf_a_ihsg

#b
stats_b_gold <- calc_stats(panel_b$Gold)
stats_b_gold
jarque.bera.test(panel_b$Gold)
lbq20_b_gold <- Box.test(panel_b$Gold, lag = 20, type = "Ljung-Box")
lbq20_a_gold
archtest_b_gold <- ArchTest(panel_b$Gold, lag = 20)
archtest_b_gold
adf_b_gold <- adf.test(panel_b$Gold)
adf_b_gold

stats_b_ihsg <- calc_stats(panel_b$IHSG)
stats_b_ihsg
jarque.bera.test(panel_b$IHSG)
lbq20_b_ihsg <- Box.test(panel_b$IHSG, lag = 20, type = "Ljung-Box")
lbq20_b_ihsg
archtest_b_ihsg <- ArchTest(panel_b$IHSG, lag = 20)
archtest_b_ihsg
adf_b_ihsg <- adf.test(panel_b$IHSG)
adf_b_ihsg

#c
stats_c_gold <- calc_stats(panel_c$Gold)
stats_c_gold
jarque.bera.test(panel_c$Gold)
lbq20_c_gold <- Box.test(panel_c$Gold, lag = 20, type = "Ljung-Box")
lbq20_c_gold
archtest_c_gold <- ArchTest(panel_c$Gold, lag = 20)
archtest_c_gold
adf_c_gold <- adf.test(panel_c$Gold)
adf_c_gold

stats_c_ihsg <- calc_stats(panel_c$IHSG)
stats_c_ihsg
jarque.bera.test(panel_c$IHSG)
lbq20_c_ihsg <- Box.test(panel_c$IHSG, lag = 20, type = "Ljung-Box")
lbq20_c_ihsg
archtest_c_ihsg <- ArchTest(panel_c$IHSG, lag = 20)
archtest_c_ihsg
adf_c_ihsg <- adf.test(panel_c$IHSG)
adf_c_ihsg

#d
stats_d_gold <- calc_stats(panel_d$Gold)
stats_d_gold
jarque.bera.test(panel_d$Gold)
lbq20_d_gold <- Box.test(panel_d$Gold, lag = 20, type = "Ljung-Box")
lbq20_d_gold
archtest_d_gold <- ArchTest(panel_d$Gold, lag = 20)
archtest_d_gold
adf_d_gold <- adf.test(panel_d$Gold)
adf_d_gold

stats_d_ihsg <- calc_stats(panel_d$IHSG)
stats_d_ihsg
jarque.bera.test(panel_d$IHSG)
lbq20_d_ihsg <- Box.test(panel_d$IHSG, lag = 20, type = "Ljung-Box")
lbq20_d_ihsg
archtest_d_ihsg <- ArchTest(panel_d$IHSG, lag = 20)
archtest_d_ihsg
adf_d_ihsg <- adf.test(panel_d$IHSG)
adf_d_ihsg

#Seluruh Periode
acf(panel_a$Gold, 
    main = "ACF COMEX Seluruh Periode", 
    col = "#2464b8",        
    lwd = 2,                
    ci.col = "black",     
    cex.axis = 1.2,         
    cex.main = 1.5)      

pacf(panel_a$Gold, 
     main = "PACF COMEX Seluruh Periode", 
     col = "#2464b8",        
     lwd = 2,             
     ci.col = "black",       
     cex.axis = 1.2,          
     cex.main = 1.5)          

acf(panel_a$IHSG, 
    main = "ACF IHSG Seluruh Periode", 
    col = "#2464b8",        
    lwd = 2,                
    ci.col = "black",     
    cex.axis = 1.2,         
    cex.main = 1.5)      

pacf(panel_a$IHSG, 
     main = "PACF IHSG Seluruh Periode", 
     col = "#2464b8",        
     lwd = 2,             
     ci.col = "black",       
     cex.axis = 1.2,          
     cex.main = 1.5)  

#Subperiode 1
acf(panel_b$Gold, 
    main = "ACF COMEX Subperiode I", 
    col = "#2464b8",        
    lwd = 2,                
    ci.col = "black",     
    cex.axis = 1.2,         
    cex.main = 1.5)      

pacf(panel_b$Gold, 
     main = "PACF COMEX Subperiode I", 
     col = "#2464b8",        
     lwd = 2,             
     ci.col = "black",       
     cex.axis = 1.2,          
     cex.main = 1.5)          

acf(panel_b$IHSG, 
    main = "ACF IHSG Subperiode I", 
    col = "#2464b8",        
    lwd = 2,                
    ci.col = "black",     
    cex.axis = 1.2,         
    cex.main = 1.5)      

pacf(panel_b$IHSG, 
     main = "PACF IHSG Subperiode I", 
     col = "#2464b8",        
     lwd = 2,             
     ci.col = "black",       
     cex.axis = 1.2,          
     cex.main = 1.5)  

#Subperiode II
acf(panel_c$Gold, 
    main = "ACF COMEX Subperiode II", 
    col = "#2464b8",        
    lwd = 2,                
    ci.col = "black",     
    cex.axis = 1.2,         
    cex.main = 1.5)      

pacf(panel_c$Gold, 
     main = "PACF COMEX Subperiode II", 
     col = "#2464b8",        
     lwd = 2,             
     ci.col = "black",       
     cex.axis = 1.2,          
     cex.main = 1.5)          

acf(panel_c$IHSG, 
    main = "ACF IHSG Subperiode II", 
    col = "#2464b8",        
    lwd = 2,                
    ci.col = "black",     
    cex.axis = 1.2,         
    cex.main = 1.5)      

pacf(panel_c$IHSG, 
     main = "PACF IHSG Subperiode II", 
     col = "#2464b8",        
     lwd = 2,             
     ci.col = "black",       
     cex.axis = 1.2,          
     cex.main = 1.5)  

#Subperiode III
acf(panel_d$Gold, 
    main = "ACF COMEX Subperiode III", 
    col = "#2464b8",        
    lwd = 2,                
    ci.col = "black",     
    cex.axis = 1.2,         
    cex.main = 1.5)      

pacf(panel_d$Gold, 
     main = "PACF COMEX Subperiode III", 
     col = "#2464b8",        
     lwd = 2,             
     ci.col = "black",       
     cex.axis = 1.2,          
     cex.main = 1.5)          

acf(panel_d$IHSG, 
    main = "ACF IHSG Subperiode III", 
    col = "#2464b8",        
    lwd = 2,                
    ci.col = "black",     
    cex.axis = 1.2,         
    cex.main = 1.5)      

pacf(panel_d$IHSG, 
     main = "", 
     col = "#2464b8",        
     lwd = 2,             
     ci.col = "black",       
     cex.axis = 1.2,          
     cex.main = 1.5)  

p_values <- c(0,1, 2, 3)
q_values <- c(0,1, 2, 3)

arma <- function(x, p_values, q_values) {
  results <- expand.grid(p = p_values, q = q_values)
  results$AIC <- NA
  for (i in seq_len(nrow(results))) {
    p <- results$p[i]
    q <- results$q[i]
    model <- arima(x, order = c(p, 0, q), include.mean = TRUE)
    results$AIC[i] <- AIC(model)
  }
  return(results)
}

#Seluruh periode emas
x <- xts(panel_a$Gold, order.by = panel_a$Date)
results <- arma(x, p_values, q_values)
results
best_model <- results[order(results$AIC)[1], ]
best_model
p <- best_model$p
q <- best_model$q

arma_manual1 <- arima(x, order = c(p, 0, q), include.mean = TRUE)
jarque.bera.test(arma_manual1$residuals)
Box.test(arma_manual1$residuals, lag = 20, type = "Ljung-Box")
ArchTest(arma_manual1$residuals^2, lag = 20)
garch_spec1 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
tgarch_spec1 <- ugarchspec(
  variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
garch_fit1 <- ugarchfit(spec = tgarch_spec1, data = x)
garch_fit1
garch_fit1@fit$matcoef
garch_fit1@fit$LLH
infocriteria(garch_fit1)
stand_resid1 <- garch_fit1@fit$z
Box.test(stand_resid1, lag = 20, type = "Ljung-Box")
ArchTest(stand_resid1^2, lag = 20)

cond_var <- sigma(garch_fit1)^2
cond_var_df <- data.frame(
  Time = as.Date(time(cond_var)),
  ConditionalVariance = as.numeric(cond_var)
)
ggplot(data = cond_var_df, aes(x = Time, y = ConditionalVariance)) +
  geom_line(color = "#2464b8") +
  labs(
    title = "Conditional Variance Emas", 
    x = "Tahun", 
    y = "Conditional Variance"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") + 
  scale_y_continuous(
    breaks = seq(0, max(cond_var, na.rm = TRUE), length.out = 10),
    limits = c(0, max(cond_var, na.rm = TRUE))
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 14), 
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 6),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

x <- xts(panel_a$IHSG, order.by = panel_a$Date)
results <- arma(x, p_values, q_values)
results
best_model <- results[order(results$AIC)[2], ] #1 NaNs #2 stabil ovefit #3 stabil kurang signifikan #5 stabil
best_model
p <- best_model$p
q <- best_model$q

arma_manual2 <- arima(x, order = c(p, 0, q), include.mean = TRUE)
jarque.bera.test(arma_manual2$residuals)
Box.test(arma_manual2$residuals, lag = 20, type = "Ljung-Box")
ArchTest(arma_manual2$residuals^2, lag = 20)
garch_spec2 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
tgarch_spec2 <- ugarchspec(
  variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
garch_fit2 <- ugarchfit(spec = garch_spec2, data = x)
garch_fit2 <- ugarchfit(spec = tgarch_spec2, data = x)
garch_fit2
garch_fit2@fit$matcoef
garch_fit2@fit$LLH
infocriteria(garch_fit2)
stand_resid2 <- garch_fit2@fit$z
Box.test(stand_resid2, lag = 20, type = "Ljung-Box")
ArchTest(stand_resid2^2, lag = 20)

cond_var2 <- sigma(garch_fit2)^2
cond_var_df2 <- data.frame(
  Time = as.Date(time(cond_var2)),
  ConditionalVariance = as.numeric(cond_var2)
)
ggplot(data = cond_var_df2, aes(x = Time, y = ConditionalVariance)) +
  geom_line(color = "#2464b8") +
  labs(
    title = "Conditional Variance IHSG", 
    x = "Tahun", 
    y = "Conditional Variance"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") + 
  scale_y_continuous(
    breaks = seq(0, max(cond_var2, na.rm = TRUE), length.out = 10),
    limits = c(0, max(cond_var2, na.rm = TRUE))
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 14), 
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 6),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

#subperiode 1 emas
x <- xts(panel_b$Gold, order.by = panel_b$Date)
results <- arma(x, p_values, q_values)
results
best_model <- results[order(results$AIC)[2], ]
best_model
p <- best_model$p
q <- best_model$q

arma_manual3 <- arima(x, order = c(p, 0, q), include.mean = TRUE)
jarque.bera.test(arma_manual3$residuals)
Box.test(arma_manual3$residuals, lag = 20, type = "Ljung-Box")
ArchTest(arma_manual3$residuals^2, lag = 20)
garch_spec3 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
tgarch_spec3 <- ugarchspec(
  variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
garch_fit3 <- ugarchfit(spec = tgarch_spec3, data = x)
garch_fit3
garch_fit3@fit$matcoef
garch_fit3@fit$LLH
infocriteria(garch_fit3)
stand_resid3 <- garch_fit3@fit$z
Box.test(stand_resid3, lag = 20, type = "Ljung-Box")
ArchTest(stand_resid3^2, lag = 20)

#subperiode 1 ihsg
x <- xts(panel_b$IHSG, order.by = panel_b$Date)
results <- arma(x, p_values, q_values)
results
best_model <- results[order(results$AIC)[1], ]
best_model
p <- best_model$p
q <- best_model$q

arma_manual4 <- arima(x, order = c(p, 0, q), include.mean = TRUE)
jarque.bera.test(arma_manual4$residuals)
Box.test(arma_manual4$residuals, lag = 20, type = "Ljung-Box")
ArchTest(arma_manual4$residuals^2, lag = 20)
garch_spec4 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
tgarch_spec4 <- ugarchspec(
  variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
garch_fit4 <- ugarchfit(spec = tgarch_spec4, data = x)
garch_fit4
garch_fit4@fit$matcoef
garch_fit4@fit$LLH
infocriteria(garch_fit4)
stand_resid4 <- garch_fit4@fit$z
Box.test(stand_resid4, lag = 20, type = "Ljung-Box")
ArchTest(stand_resid4^2, lag = 20)

#subperiode 2 emas
x <- xts(panel_c$Gold, order.by = panel_c$Date)
results <- arma(x, p_values, q_values)
results
best_model <- results[order(results$AIC)[1], ]
best_model
p <- best_model$p
q <- best_model$q

arma_manual5 <- arima(x, order = c(p, 0, q), include.mean = TRUE)
jarque.bera.test(arma_manual5$residuals)
Box.test(arma_manual5$residuals, lag = 20, type = "Ljung-Box")
ArchTest(arma_manual5$residuals^2, lag = 20)
garch_spec5 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
tgarch_spec5 <- ugarchspec(
  variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
garch_fit5 <- ugarchfit(spec = tgarch_spec5, data = x)
garch_fit5
garch_fit5@fit$matcoef
garch_fit5@fit$LLH
infocriteria(garch_fit5)
stand_resid5 <- garch_fit5@fit$z
Box.test(stand_resid5, lag = 20, type = "Ljung-Box")
ArchTest(stand_resid5^2, lag = 20)

#subperiode 2 ihsg
x <- xts(panel_c$IHSG, order.by = panel_c$Date)
results <- arma(x, p_values, q_values)
results
best_model <- results[order(results$AIC)[11], ] #3 tidak stabil #11
best_model
p <- best_model$p
q <- best_model$q
arma_manual6 <- arima(x, order = c(p, 0, q), include.mean = TRUE)
jarque.bera.test(arma_manual6$residuals)
Box.test(arma_manual6$residuals, lag = 20, type = "Ljung-Box")
ArchTest(arma_manual6$residuals^2, lag = 20)
garch_spec6 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
tgarch_spec6 <- ugarchspec(
  variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
garch_fit6 <- ugarchfit(spec = garch_spec6, data = x)
garch_fit6
garch_fit6 <- ugarchfit(spec = tgarch_spec6, data = x)
garch_fit6
garch_fit6@fit$matcoef
garch_fit6@fit$LLH
infocriteria(garch_fit6)
stand_resid6 <- garch_fit6@fit$z
Box.test(stand_resid6, lag = 20, type = "Ljung-Box")
ArchTest(stand_resid6^2, lag = 20)

#subperiode 3 emas
x <- xts(panel_d$Gold, order.by = panel_d$Date)
results <- arma(x, p_values, q_values)
results
best_model <- results[order(results$AIC)[1], ]
best_model
p <- best_model$p
q <- best_model$q

arma_manual7 <- arima(x, order = c(p, 0, q), include.mean = TRUE)
jarque.bera.test(arma_manual7$residuals)
Box.test(arma_manual7$residuals, lag = 20, type = "Ljung-Box")
ArchTest(arma_manual7$residuals^2, lag = 20)
garch_spec7 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
tgarch_spec7 <- ugarchspec(
  variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
garch_fit7 <- ugarchfit(spec = tgarch_spec7, data = x)
garch_fit7
garch_fit7@fit$matcoef
garch_fit7@fit$LLH
infocriteria(garch_fit7)
stand_resid7 <- garch_fit7@fit$z
Box.test(stand_resid7, lag = 20, type = "Ljung-Box")
ArchTest(stand_resid7^2, lag = 20)

#subperiode 3 ihsg
x <- xts(panel_d$IHSG, order.by = panel_d$Date)
results <- arma(x, p_values, q_values)
results
best_model <- results[order(results$AIC)[4], ] #4
best_model
p <- best_model$p
q <- best_model$q
arma_manual8 <- arima(x, order = c(p, 0, q), include.mean = TRUE)
jarque.bera.test(arma_manual8$residuals)
Box.test(arma_manual8$residuals, lag = 20, type = "Ljung-Box")
ArchTest(arma_manual8$residuals^2, lag = 20)
garch_spec8 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
tgarch_spec8 <- ugarchspec(
  variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
  distribution.model = "ged"
)
garch_fit8 <- ugarchfit(spec = garch_spec8, data = x)
garch_fit8
garch_fit8 <- ugarchfit(spec = tgarch_spec8, data = x)
garch_fit8
garch_fit8@fit$matcoef
garch_fit8@fit$LLH
infocriteria(garch_fit8)
stand_resid8 <- garch_fit8@fit$z
Box.test(stand_resid8, lag = 20, type = "Ljung-Box")
ArchTest(stand_resid8^2, lag = 20)
