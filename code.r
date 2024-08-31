# data formating
library(readxl)
airpass <- read_excel("air_passenger.xlsx")
airpass <- airpass[19:1,]
library(knitr)
library(kableExtra)
library(xtable)
library(htmlTable)
library(gridExtra)
# Basic table using kable
my_table <- xtable(airpass)
# Add title and caption
# Save LaTeX code to a file
attr(my_table, "caption") <- "This is a stylish table with a title and caption"
attr(my_table, "label") <- "tab:mytable"

# Print the table
print(my_table, scalebox = 0.8,  include.rownames = FALSE)


library(reshape2)
library(segmented)
library(tseries)
library(ggplot2)
library(seastests)
library(forecast)
airdat <- melt(airpass, variable.name = "Month", value.name = "Passengers", id.vars = "Year")
airdat <- airdat[order(airdat$Year),]
airts <- ts(airdat$Passengers, start = 2001, frequency = 12)

#plot
tt <- seq.default(from = 2001, by = 1/12, length.out = 228)
p1 <- ggplot() + geom_line(aes(x =tt, y = airdat$Passengers/1e6), col = 'darkred', linewidth = 0.8) +theme_light()+
    labs(x = 'Time (Years)', y = 'Passengers (millions)', title = 'Monthly US Passenger Booking ( 2001 - 2019 )')

ggsave(filename = "MothlyPass.png" ,plot =  p1)


# Not homoscadastic:  https://en.wikipedia.org/wiki/Variance-stabilizing_transformation
# examine the relationship between anuall variance and annuall mean
yearlymean <- rowMeans(airpass[,2:13])
yearlyvar <- apply(data.frame(airpass[, 2:13]), 1, var)
p2 <- ggplot() + geom_line(aes(x =2001:2019, y = yearlyvar), col = 'darkblue', linewidth = 0.8) +theme_light()+
    labs(x = 'Time (Years)', y = 'Variace', title = 'Annual Variance US Passenger Booking ( 2001 - 2019 )')
p2
ggsave(filename = "Varyearly.png" ,plot =  p2)

p3 <- ggplot() + geom_point(aes(x =yearlymean, y = yearlyvar), col = 'darkred') +theme_light()+
    labs(x = 'Mean', y = 'Variance', title = 'Anuall Variance - Mean Relationship ( 2001 - 2019 )')
p3
ggsave(filename = "var_mean.png" ,plot =  p3)



plot(yearlymean, yearlyvar, type = 'l',lwd = 2.5 ,xlab = "Yearly Mean", ylab = "Yearly Variance", main = "Variance-Mean Plot")
# fit linear model
lmm <- lm(yearlyvar~yearlymean)
summary(lmm) # adjRsq =
#fit sq model
ymeansq <- yearlymean^2
mm <- lm(yearlyvar~ymeansq)
summary(mm)# adjRsq =

#fit cubic model
ymeancube <- yearlymean^3
cm <- lm(yearlyvar~ymeancube)
summary(cm)# adjRsq =

# Plot Fitted Values
lines(yearlymean, lmm$fitted.values, col = "darkred", lwd = 2.5)
p3.2  <- p3 + geom_abline(slope = lmm$coefficients[2], intercept = lmm$coefficients[1], col = 'brown', linewidth = 1)
ggsave(filename = "var_mean_fit.png", plot = p3.2)
# var proportional to mean sq so log transform
logy <- sqrt(airdat$Passengers)




p4 <- ggplot() + geom_line(aes(x =tt, y = logy), col = 'darkred', linewidth = 0.8) +theme_light()+
    labs(x = 'Time (Years)', y = 'Sqrt(Passengers)', title = 'Transformed Time Series ( 2001 - 2019 )')
p4
ggsave(filename = "transformedTS.png" ,plot =  p4)


new_var <- apply(data.frame(log(airpass[, 2:13])), 1, var)

p5 <- ggplot() + geom_line(aes(x =2002:2019, y = new_var[-1]), col = 'darkred', linewidth = 0.8) +theme_light()+
    labs(x = 'Time (Years)', y = 'Variance', title = 'New Variance ( 2001 - 2019 )')+
    lims(y  = c(0, 0.07))
p5
ggsave(filename = "new_var.png" ,plot =  p5)

# # estimation of seasonal components
# ## fast trend method
# movingwindow = c(0.5, rep(1,23), 0.5)
# est_mt <- numeric(length = 228-24)
# for(i in 13:(228-12)){
#     est_mt[i-12] <- sum(movingwindow*logy[(i-12):(i+12)]/24)
# }
# yt_mt = matrix(logy[13:(228-12)] - est_mt, ncol  =12, nrow = 17, byrow =T)
# yt_mt
# par(mfrow = c(1,2))
# w_k <- colMeans(yt_mt)
# s_k <- w_k - mean(w_k)
# plot(s_k, type = 'l', main = "Fast Trend Method")
# seasonal_comp <- rep (s_k, 12)
#
# deseasonalised <- logy - seasonal_comp
#
#
# ## slow trend method
# log_dat_mat <- matrix(logy, nrow = 19, ncol=12, byrow = T)
# m_t2 <- rowMeans(log_dat_mat)
# s_k2 = colMeans(log_dat_mat - matrix(rep(m_t2, 12), nrow = 19, ncol = 12, byrow = F))
# plot(s_k2, type = 'l', main = "Slow Trend Method")
#
# seasonal_comp2 <- rep (s_k2, 19)
# deseasonalised2 <-  logy - seasonal_comp2
# par(mfrow = c(1,1))
#
# plot(deseasonalised, type = 'l',  main = "FTM")
# plot(deseasonalised2, type = 'l', main = "STM" ) # seasnality not captured properly by stm
# fried(deseasonalised, freq = 12)
# fried(deseasonalised2, freq = 12)
# # trend removal
# ######################################
#

t <- 1:length(logy)

# Fit a piecewise linear model
trend_model_linear <- segmented(lm(logy ~ t), seg.Z = ~t, psi = c(85,97 , 109,121))
summary(trend_model_linear)
ggplot() + geom_line(aes(x = t, y = logy))+theme_light()+
    geom_line(aes(x= t, y = trend_model_linear$fitted.values, col = "darkred"), show.legend = F)




tsq <- t^2

trend_model_sq2 <- segmented(lm(logy ~ t+ tsq), seg.Z = ~t, psi = c(85,97 , 109,121))
# coeff of t not signifiacant
summary(trend_model_sq2)
ggplot() + geom_line(aes(x = t, y = logy))+theme_light()+
    geom_line(aes(x= t, y = trend_model_sq2$fitted.values, col = "darkred"), show.legend = F)


trend_model_sq1 <- segmented(lm(logy ~ tsq), seg.Z = ~t, psi = c(85,95 , 109,121)**2)
# coeff of t not signifiacant
summary(trend_model_sq1)
p6 <- ggplot() + geom_line(aes(x = tt, y = logy))+theme_light()+
    geom_line(aes(x= tt, y = trend_model_sq1$fitted.values, col = "darkred"),linewidth = 0.8, show.legend = F)+
    labs(x = 'Time (Years)', y = "Sqrt(Y)", title = 'Trend Component')
p6
ggsave(filename = "Trend_fit.png", plot= p6)




detrended <- logy -  trend_model_sq1$fitted.values
relativeOrderTest  <- function(x, level_of_sig) {
    n <- length(x)
    Q <- 0
    for(j in 2:n){
        for( i in 1:(j-1)){
            if(x[i] > x[j]){
                Q <-  Q + 1
            }
        }
    }
    tao <- 1 - 4*Q/(n*(n-1))
    var_tao <- 2*(2*n + 5)/(9*n*(n-1))
    z <- tao/sqrt(var_tao)
    alpha = 1-level_of_sig
    critical.val = qnorm(1-alpha/2)
    rule = ifelse(abs(z)>critical.val,'trend is present','trend is not present')
    print(c('test statistic'= z,rule))
}

relativeOrderTest(detrended, 0.95)

p7 <- ggplot() + geom_line(aes(x =tt, y = detrended), col = 'darkred', linewidth = 0.8) +theme_light()+
    labs(x = 'Time (Years)', y = '', title = 'Detrended Series ( 2001 - 2019 )')
p7
ggsave(filename = "Detrended.png" ,plot =  p7)
fried(detrended, freq =12)








# estimation of seasonal components
## fast trend method
movingwindow = c(0.5, rep(1,23), 0.5)
est_mt <- numeric(length = 228-24)
for(i in 13:(228-12)){
    est_mt[i-12] <- sum(movingwindow*detrended[(i-12):(i+12)]/24)
}
yt_mt = matrix(detrended[13:(228-12)] - est_mt, ncol  =12, nrow = 17, byrow =T)
yt_mt
par(mfrow = c(1,2))
w_k <- colMeans(yt_mt)
s_k <- w_k - mean(w_k)
plot(s_k, type = 'l', main = "Fast Trend Method")
seasonal_comp <- rep (s_k, 19)

deseasonalised <- detrended - seasonal_comp


## slow trend method
log_dat_mat <- matrix(detrended, nrow = 19, ncol=12, byrow = T)
m_t2 <- rowMeans(log_dat_mat)
s_k2 = colMeans(log_dat_mat - matrix(rep(m_t2, 12), nrow = 19, ncol = 12, byrow = F))
plot(s_k2, type = 'l', main = "Slow Trend Method")

seasonal_comp2 <- rep (s_k2, 19)
deseasonalised2 <-  detrended - seasonal_comp2

p8.1 <- ggplot() + geom_line(aes(x =1:12, y = s_k), col = 'darkred', linewidth = 0.8) +theme_light()+
    labs(x = 'Month', y = '', title = 'Fast Trend Method')
p8.1


p8.2 <- ggplot() + geom_line(aes(x =1:12, y = s_k2), col = 'darkred', linewidth = 0.8) +theme_light()+
    labs(x = 'Month', y = '', title = 'Slow Trend Method')
p8.2
p8 <- grid.arrange(p8.1, p8.2 )


ggsave(filename = "seasonal.png" ,plot =  p8)



par(mfrow = c(1,1))

plot(deseasonalised, type = 'l',  main = "FTM")
plot(deseasonalised2, type = 'l', main = "STM" ) # seasnality not captured properly by stm

fried(deseasonalised, freq = 12)
fried(deseasonalised2, freq = 12)
# trend removal
######################################

p9 <- ggplot() + geom_line(aes(x =tt, y = deseasonalised), col = 'darkred', linewidth = 0.8) +theme_light()+
    labs(x = 'Time (Years)', y = '', title = 'Deseasonalised Series')
p9
ggsave(filename = "Deseasonalised.png" ,plot =  p9)





residue <- deseasonalised



relativeOrderTest(residue, 0.95)

# new series does not have a trend

adf.test(residue)# series is stationary at 0.95 level of sig
fried(residue, freq = 12)#https://search.r-project.org/CRAN/refmans/seastests/html/fried.html
# no seasonality
r_pacf <- pacf(residue)# suggests ar 1
r_acf <- acf(residue)# suggests ma 2
par(mfrow=c(1, 2))
plot(r_pacf, main = "Deseasonalised Series PACF")
plot(r_acf, main = "Deseasonalised Series ACF")




potential_models <- data.frame('ar' = numeric(24), 'ma' = numeric(24), 'aic' = numeric(24), 'bic' = numeric(24))
J <- 1
for(i in 0:4){
    for(j in 0:4){
        if(i == 0 & j == 0){
            next
        }
        potential_models[J,1] <- i
        potential_models[J,2] <- j
        mod <- arima(residue, order = c(i, 0, j))
        potential_models[J,3] <- AIC(mod)
        potential_models[J,4] <- BIC(mod)
        J<-J+1
    }
}


my_table2 <- xtable(potential_models)
# Add title and caption
# Save LaTeX code to a file
attr(my_table2, "caption") <- "This is a stylish table with a title and caption"
attr(my_table2, "label") <- "tab:mytable"

# Print the table
print(my_table2, scalebox = 0.8,  include.rownames = FALSE)


# best model from BIC
parbic <- potential_models[which.min(potential_models$bic), ]
paraic <- potential_models[which.min(potential_models$aic), ]


bic_model <- arima(residue, order = c(parbic[1, 1], 0, parbic[1,2]))# ar1
aic_model <- arima(residue, order = c(paraic[1, 1], 0, paraic[1,2]))# ma3
summary(bic_model)
summary(aic_model)


plot(t, residue, type = "l", lwd = 2)
lines(t, fitted.values( bic_model), col = "darkred", lwd = 2)
plot(t, residue, type = "l", lwd = 2)
lines(t, fitted.values( aic_model), col = "darkgreen", lwd = 2)
# choose AR1 as it is a simpler model with same predictive power


seasonal_component = seasonal_comp
trend_component = fitted.values(trend_model_sq1)
stochastic_component = fitted(bic_model)
noise_component =  residue - fitted(bic_model)

ggplot() + geom_line(aes(x = t, y = seasonal_component)) + theme_light()
ggplot() + geom_line(aes(x = t, y = trend_component)) + theme_light()
ggplot() + geom_line(aes(x = t, y = stochastic_component), col = 'darkred', linewidth = 0.6) + theme_light()
ggplot() + geom_line(aes(x = t, y = noise_component)) + theme_light()

acf(noise_component, lag.max = 40)#  not white noise
pacf(noise_component)

m_pacf <- pacf(noise_component)# suggests ar 1
m_acf <- acf(noise_component)# suggests ma 2
par(mfrow=c(1, 2))
plot(m_pacf, main = "Noise Series PACF")
plot(m_acf, main = "Noise Series ACF")

# informed model
best_model <- arima(residue, order = c(1,0,0), seasonal = list(order = c(1, 0, 0), period =12))

summary(best_model)
BIC(best_model)



seasonal_component = seasonal_comp
trend_component = fitted.values(trend_model_sq1)
stochastic_component = fitted(best_model)
noise_component =  residue - fitted(best_model)

p10.1 <- ggplot() + geom_line(aes(x = tt, y = seasonal_component), col = 'darkred', linewidth = 0.6) + theme_light()+
    labs(x = "Time (Years)", y = '', title = 'Seasonal Component')
p10.2 <- ggplot() + geom_line(aes(x = tt, y = trend_component), col = 'darkred', linewidth = 0.6) + theme_light()+
    labs(x = "Time (Years)", y = '', title = 'Trend Component')
p10.3 <- ggplot() + geom_line(aes(x = tt, y = stochastic_component), col = 'darkred', linewidth = 0.6) + theme_light()+
    labs(x = "Time (Years)", y = '', title = 'Predictive Component')
p10.4 <- ggplot() + geom_line(aes(x = tt, y = noise_component), col = 'darkred', linewidth = 0.6) + theme_light()+
    labs(x = "Time (Years)", y = '', title = 'Noise Component')

p10 <- grid.arrange(p10.1, p10.2, p10.3, p10.4, top = 'Time Series Decomposition')
p10
ggsave(filename = 'decomposition.png', plot= p10)

m_pacf <- pacf(noise_component)# suggests ar 1
m_acf <- acf(noise_component)# suggests ma 2
par(mfrow=c(1, 2))
plot(m_pacf, main = "Noise Series PACF")
plot(m_acf, main = "Noise Series ACF")


# reverse the transformations
fitted_model <- trend_component + seasonal_component + stochastic_component
p11 <- ggplot() + geom_line(aes(x = tt, y = c((fitted_model)^2)/1e6, color = "Predicted"), linewidth = 0.6, alpha = 0.8) + theme_light()+
    geom_line(aes(x = tt, y = (logy**2)/1e6, color = "Actual"), linewidth = 0.6, alpha = 0.8)+
    guides(color = guide_legend(title = "Bookings"))+
    labs(x = "Time (Years)", y = 'Passengers (millions)', title = "Final Fitted Model Performance")
p11
ggsave("final.png", plot = p11)
stochastic_forecast <- forecast(best_model, h = 12)
future_tsq <- data.frame(tsq = c((229:240)^2))
trend_forecast <- predict(trend_model_sq1, newdata = future_tsq )

next_year_forecast <- (trend_forecast + s_k + stochastic_forecast$mean)
plot(1:240, c((fitted_model)**2, next_year_forecast**2), type = 'l')
next_year_forecast**2

