################
# read data set
################

QLD_fuel = read.csv("daily_fuel_mix_QLD.csv")

QLD_fuel_proportion = QLD_fuel[,12:20]
QLD_fuel_proportion_zero_fill = matrix(NA, nrow(QLD_fuel_proportion), ncol(QLD_fuel_proportion))
for(iw in 1:ncol(QLD_fuel_proportion))
{
    QLD_fuel_proportion_zero_fill[,iw] = replace(QLD_fuel_proportion[,iw], which(is.na(QLD_fuel_proportion[,iw])), 0)
    print(iw); rm(iw)
}
fuel_type = c("Battery.Storage", "Black.coal", "Methane", "Diesel.oil", "Hydro", "Kerosene",
              "Natural Gas", "Solar", "Wind")
n_fuel_type = length(fuel_type)

# historical data reconstruction

all(round(rowSums(QLD_fuel_proportion_zero_fill),4) == 1) # TRUE

# set up training and testing data

n_days = nrow(QLD_fuel)
n_test = ceiling(n_days * 0.25)
n_train = n_days - n_test

###########################
# centered log ratio (clr)
###########################

# EVR

QLD_fuel_clr_inverse = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    QLD_fuel_clr = clr(QLD_fuel_proportion_zero_fill[1:(n_train + iwk - 1),])
    ncomp_EVR = select_K(tau = 0.001, eigenvalue = svd(QLD_fuel_clr)$d^2)
    
    ftsm_obj = ftsm(fts(1:n_fuel_type, t(QLD_fuel_clr)), order = ncomp_EVR)
    QLD_fuel_clr_fore = as.numeric(forecast(ftsm_obj, h = 1)$mean$y)
    
    QLD_fuel_clr_inverse[,iwk] = clrInv(QLD_fuel_clr_fore)
    print(iwk); rm(iwk)
}
rownames(QLD_fuel_clr_inverse) = fuel_type
colnames(QLD_fuel_clr_inverse) = 1:n_test

# K = 6

QLD_fuel_clr_inverse_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    QLD_fuel_clr = clr(QLD_fuel_proportion_zero_fill[1:(n_train + iwk - 1),])
    
    ftsm_obj = ftsm(fts(1:n_fuel_type, t(QLD_fuel_clr)), order = 6)
    QLD_fuel_clr_fore = as.numeric(forecast(ftsm_obj, h = 1)$mean$y)
    
    QLD_fuel_clr_inverse_K6[,iwk] = clrInv(QLD_fuel_clr_fore)
    print(iwk); rm(iwk)
}
rownames(QLD_fuel_clr_inverse_K6) = fuel_type
colnames(QLD_fuel_clr_inverse_K6) = 1:n_test

# forecast total supply

QLD_fuel_sum_fore_arima = QLD_fuel_sum_fore_ets = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    QLD_fuel_sum_fore_arima[iwk] = exp(forecast(auto.arima(log(QLD_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    QLD_fuel_sum_fore_ets[iwk]   = exp(forecast(ets(log(QLD_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    print(iwk); rm(iwk)
}

# distribute total to fuel type mix

QLD_fuel_type_fore_mat_arima = QLD_fuel_type_fore_mat_ets = 
QLD_fuel_type_fore_mat_arima_K6 = QLD_fuel_type_fore_mat_ets_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    QLD_fuel_type_fore_mat_arima[,iwk] = QLD_fuel_clr_inverse[,iwk] * QLD_fuel_sum_fore_arima[iwk]
    QLD_fuel_type_fore_mat_ets[,iwk]   = QLD_fuel_clr_inverse[,iwk] * QLD_fuel_sum_fore_ets[iwk]

    QLD_fuel_type_fore_mat_arima_K6[,iwk] = QLD_fuel_clr_inverse_K6[,iwk] * QLD_fuel_sum_fore_arima[iwk]
    QLD_fuel_type_fore_mat_ets_K6[,iwk]   = QLD_fuel_clr_inverse_K6[,iwk] * QLD_fuel_sum_fore_ets[iwk]
    print(iwk); rm(iwk)
}
rownames(QLD_fuel_type_fore_mat_arima) = rownames(QLD_fuel_type_fore_mat_ets) = 
rownames(QLD_fuel_type_fore_mat_arima_K6) = rownames(QLD_fuel_type_fore_mat_ets_K6) = fuel_type

colnames(QLD_fuel_type_fore_mat_arima) = colnames(QLD_fuel_type_fore_mat_ets) = 
colnames(QLD_fuel_type_fore_mat_arima_K6) = colnames(QLD_fuel_type_fore_mat_ets_K6) = 1:n_test

# holdout data samples

QLD_fuel_amount = QLD_fuel[,2:11]
QLD_fuel_amount_zero_fill = matrix(NA, nrow(QLD_fuel_amount), ncol(QLD_fuel_amount))
for(iw in 1:ncol(QLD_fuel_amount))
{
    QLD_fuel_amount_zero_fill[,iw] = replace(QLD_fuel_amount[,iw], which(is.na(QLD_fuel_amount[,iw])), 0)
    print(iw); rm(iw)
}

fuel_type = c("Battery.Storage", "Black.coal", "Methane", "Diesel.oil", "Hydro", "Kerosene",
              "Natural Gas", "Solar", "Wind")
n_fuel_type = length(fuel_type)

fuel_type_holdout = t(QLD_fuel_amount_zero_fill[(n_train + 1):n_days,1:n_fuel_type])

# MASE calculation

QLD_clr_MASE_arima = QLD_clr_MASE_ets = 
QLD_clr_MASE_arima_K6 = QLD_clr_MASE_ets_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    QLD_clr_MASE_arima[iwk] = ftsa:::mase(forecast = QLD_fuel_type_fore_mat_arima[,iwk], 
                                          outsampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                          insampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    QLD_clr_MASE_ets[iwk] = ftsa:::mase(forecast = QLD_fuel_type_fore_mat_ets[,iwk], 
                                        outsampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                        insampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])

    QLD_clr_MASE_arima_K6[iwk] = ftsa:::mase(forecast = QLD_fuel_type_fore_mat_arima_K6[,iwk], 
                                          outsampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                          insampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    QLD_clr_MASE_ets_K6[iwk] = ftsa:::mase(forecast = QLD_fuel_type_fore_mat_ets_K6[,iwk], 
                                        outsampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                        insampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)  
}

round(mean(QLD_clr_MASE_arima), 4)  # 0.0909
round(mean(QLD_clr_MASE_ets), 4)    # 0.0923

round(mean(QLD_clr_MASE_arima_K6), 4)  # 0.0851
round(mean(QLD_clr_MASE_ets_K6), 4)    # 0.0864

