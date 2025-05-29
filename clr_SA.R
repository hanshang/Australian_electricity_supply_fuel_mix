################
# read data set
################

SA_fuel = read.csv("daily_fuel_mix_SA.csv")

SA_fuel_proportion = SA_fuel[,8:12]
SA_fuel_proportion_zero_fill = matrix(NA, nrow(SA_fuel_proportion), ncol(SA_fuel_proportion))
for(iw in 1:ncol(SA_fuel_proportion))
{
    SA_fuel_proportion_zero_fill[,iw] = replace(SA_fuel_proportion[,iw], which(is.na(SA_fuel_proportion[,iw])), 0)
    print(iw); rm(iw)
}
fuel_type = c("Battery.Storage", "Diesel.oil", "Natural.Gas", "Solar", "Wind")
n_fuel_type = length(fuel_type)

# historical data reconstruction

all(round(rowSums(SA_fuel_proportion_zero_fill),4) == 1) # TRUE

# set up training and testing data

n_days = nrow(SA_fuel)
n_test = ceiling(n_days * 0.25)
n_train = n_days - n_test

###########################
# centered log ratio (clr)
###########################

# EVR

SA_fuel_clr_inverse = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    SA_fuel_clr = clr(SA_fuel_proportion_zero_fill[1:(n_train + iwk - 1),])
    ncomp_EVR = select_K(tau = 0.001, eigenvalue = svd(SA_fuel_clr)$d^2)
    
    ftsm_obj = ftsm(fts(1:n_fuel_type, t(SA_fuel_clr)), order = ncomp_EVR)
    SA_fuel_clr_fore = as.numeric(forecast(ftsm_obj, h = 1)$mean$y)
    
    SA_fuel_clr_inverse[,iwk] = clrInv(SA_fuel_clr_fore)
    print(iwk); rm(iwk)
}
rownames(SA_fuel_clr_inverse) = fuel_type
colnames(SA_fuel_clr_inverse) = 1:n_test

# K = 6

SA_fuel_clr_inverse_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    SA_fuel_clr = clr(SA_fuel_proportion_zero_fill[1:(n_train + iwk - 1),])
    
    ftsm_obj = ftsm(fts(1:n_fuel_type, t(SA_fuel_clr)), order = 6)
    SA_fuel_clr_fore = as.numeric(forecast(ftsm_obj, h = 1)$mean$y)
    
    SA_fuel_clr_inverse_K6[,iwk] = clrInv(SA_fuel_clr_fore)
    print(iwk); rm(iwk)
}
rownames(SA_fuel_clr_inverse_K6) = fuel_type
colnames(SA_fuel_clr_inverse_K6) = 1:n_test


# forecast total supply

SA_fuel_sum_fore_arima = SA_fuel_sum_fore_ets = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    SA_fuel_sum_fore_arima[iwk] = exp(forecast(auto.arima(log(SA_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    SA_fuel_sum_fore_ets[iwk]   = exp(forecast(ets(log(SA_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    print(iwk); rm(iwk)
}

# distribute total to fuel type mix

SA_fuel_type_fore_mat_arima = SA_fuel_type_fore_mat_ets = 
SA_fuel_type_fore_mat_arima_K6 = SA_fuel_type_fore_mat_ets_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    SA_fuel_type_fore_mat_arima[,iwk] = SA_fuel_clr_inverse[,iwk] * SA_fuel_sum_fore_arima[iwk]
    SA_fuel_type_fore_mat_ets[,iwk]   = SA_fuel_clr_inverse[,iwk] * SA_fuel_sum_fore_ets[iwk]

    SA_fuel_type_fore_mat_arima_K6[,iwk] = SA_fuel_clr_inverse_K6[,iwk] * SA_fuel_sum_fore_arima[iwk]
    SA_fuel_type_fore_mat_ets_K6[,iwk]   = SA_fuel_clr_inverse_K6[,iwk] * SA_fuel_sum_fore_ets[iwk]
    print(iwk); rm(iwk)
}
rownames(SA_fuel_type_fore_mat_arima) = rownames(SA_fuel_type_fore_mat_ets) = 
rownames(SA_fuel_type_fore_mat_arima_K6) = rownames(SA_fuel_type_fore_mat_ets_K6) = fuel_type

colnames(SA_fuel_type_fore_mat_arima) = colnames(SA_fuel_type_fore_mat_ets) = 
colnames(SA_fuel_type_fore_mat_arima_K6) = colnames(SA_fuel_type_fore_mat_ets_K6) = 1:n_test

# holdout data samples

SA_fuel_amount = SA_fuel[,2:7]
SA_fuel_amount_zero_fill = matrix(NA, nrow(SA_fuel_amount), ncol(SA_fuel_amount))
for(iw in 1:ncol(SA_fuel_amount))
{
    SA_fuel_amount_zero_fill[,iw] = replace(SA_fuel_amount[,iw], which(is.na(SA_fuel_amount[,iw])), 0)
    print(iw); rm(iw)
}

fuel_type = c("Battery.Storage", "Diesel.oil", "Natural.Gas", "Solar", "Wind")
n_fuel_type = length(fuel_type)

fuel_type_holdout = t(SA_fuel_amount_zero_fill[(n_train + 1):n_days,1:n_fuel_type])

# MASE calculation

SA_clr_MASE_arima = SA_clr_MASE_ets = 
SA_clr_MASE_arima_K6 = SA_clr_MASE_ets_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    SA_clr_MASE_arima[iwk] = ftsa:::mase(forecast = SA_fuel_type_fore_mat_arima[,iwk], 
                                         outsampletrue = SA_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                         insampletrue = SA_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    SA_clr_MASE_ets[iwk] = ftsa:::mase(forecast = SA_fuel_type_fore_mat_ets[,iwk], 
                                       outsampletrue = SA_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                       insampletrue = SA_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    SA_clr_MASE_arima_K6[iwk] = ftsa:::mase(forecast = SA_fuel_type_fore_mat_arima_K6[,iwk], 
                                         outsampletrue = SA_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                         insampletrue = SA_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    SA_clr_MASE_ets_K6[iwk] = ftsa:::mase(forecast = SA_fuel_type_fore_mat_ets_K6[,iwk], 
                                       outsampletrue = SA_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                       insampletrue = SA_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)  
}

round(mean(SA_clr_MASE_arima), 4)  # 0.3333
round(mean(SA_clr_MASE_ets), 4)    # 0.3402

round(mean(SA_clr_MASE_arima_K6), 4)  # 0.3371
round(mean(SA_clr_MASE_ets_K6), 4)    # 0.3439

