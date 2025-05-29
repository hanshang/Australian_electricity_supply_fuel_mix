################
# read data set
################

TAS_fuel = read.csv("daily_fuel_mix_TAS.csv")

TAS_fuel_proportion = TAS_fuel[,6:8]
TAS_fuel_proportion_zero_fill = matrix(NA, nrow(TAS_fuel_proportion), ncol(TAS_fuel_proportion))
for(iw in 1:ncol(TAS_fuel_proportion))
{
    TAS_fuel_proportion_zero_fill[,iw] = replace(TAS_fuel_proportion[,iw], which(is.na(TAS_fuel_proportion[,iw])), 0)
    print(iw); rm(iw)
}

fuel_type = c("Hydro", "Natural Gas", "Wind")
n_fuel_type = length(fuel_type)

# historical data reconstruction

all(round(rowSums(TAS_fuel_proportion_zero_fill),4) == 1) # TRUE

# set up training and testing data

n_days = nrow(TAS_fuel)
n_test = ceiling(n_days * 0.25)
n_train = n_days - n_test

###########################
# centered log ratio (clr)
###########################

# EVR

TAS_fuel_clr_inverse = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    TAS_fuel_clr = clr(TAS_fuel_proportion_zero_fill[1:(n_train + iwk - 1),])
    ncomp_EVR = select_K(tau = 0.001, eigenvalue = svd(TAS_fuel_clr)$d^2)
    
    ftsm_obj = ftsm(fts(1:n_fuel_type, t(TAS_fuel_clr)), order = ncomp_EVR)
    TAS_fuel_clr_fore = as.numeric(forecast(ftsm_obj, h = 1)$mean$y)
    
    TAS_fuel_clr_inverse[,iwk] = clrInv(TAS_fuel_clr_fore)
    print(iwk); rm(iwk)
}
rownames(TAS_fuel_clr_inverse) = fuel_type
colnames(TAS_fuel_clr_inverse) = 1:n_test

# K = 6

TAS_fuel_clr_inverse_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    TAS_fuel_clr = clr(TAS_fuel_proportion_zero_fill[1:(n_train + iwk - 1),])
    
    ftsm_obj = ftsm(fts(1:n_fuel_type, t(TAS_fuel_clr)), order = 6)
    TAS_fuel_clr_fore = as.numeric(forecast(ftsm_obj, h = 1)$mean$y)
    
    TAS_fuel_clr_inverse_K6[,iwk] = clrInv(TAS_fuel_clr_fore)
    print(iwk); rm(iwk)
}
rownames(TAS_fuel_clr_inverse_K6) = fuel_type
colnames(TAS_fuel_clr_inverse_K6) = 1:n_test


# forecast total supply

TAS_fuel_sum_fore_arima = TAS_fuel_sum_fore_ets = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    TAS_fuel_sum_fore_arima[iwk] = exp(forecast(auto.arima(log(TAS_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    TAS_fuel_sum_fore_ets[iwk]   = exp(forecast(ets(log(TAS_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    print(iwk); rm(iwk)
}

# distribute total to fuel type mix

TAS_fuel_type_fore_mat_arima = TAS_fuel_type_fore_mat_ets = 
TAS_fuel_type_fore_mat_arima_K6 = TAS_fuel_type_fore_mat_ets_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    TAS_fuel_type_fore_mat_arima[,iwk] = TAS_fuel_clr_inverse[,iwk] * TAS_fuel_sum_fore_arima[iwk]
    TAS_fuel_type_fore_mat_ets[,iwk]   = TAS_fuel_clr_inverse[,iwk] * TAS_fuel_sum_fore_ets[iwk]

    TAS_fuel_type_fore_mat_arima_K6[,iwk] = TAS_fuel_clr_inverse_K6[,iwk] * TAS_fuel_sum_fore_arima[iwk]
    TAS_fuel_type_fore_mat_ets_K6[,iwk]   = TAS_fuel_clr_inverse_K6[,iwk] * TAS_fuel_sum_fore_ets[iwk]
    print(iwk); rm(iwk)
}
rownames(TAS_fuel_type_fore_mat_arima) = rownames(TAS_fuel_type_fore_mat_ets) = 
rownames(TAS_fuel_type_fore_mat_arima_K6) = rownames(TAS_fuel_type_fore_mat_ets_K6) = fuel_type

colnames(TAS_fuel_type_fore_mat_arima) = colnames(TAS_fuel_type_fore_mat_ets) = 
colnames(TAS_fuel_type_fore_mat_arima_K6) = colnames(TAS_fuel_type_fore_mat_ets_K6) = 1:n_test

# holdout data samples

TAS_fuel_amount = TAS_fuel[,2:5]
TAS_fuel_amount_zero_fill = matrix(NA, nrow(TAS_fuel_amount), ncol(TAS_fuel_amount))
for(iw in 1:ncol(TAS_fuel_amount))
{
    TAS_fuel_amount_zero_fill[,iw] = replace(TAS_fuel_amount[,iw], which(is.na(TAS_fuel_amount[,iw])), 0)
    print(iw); rm(iw)
}

fuel_type = c("Hydro", "Natural Gas", "Wind")
n_fuel_type = length(fuel_type)

fuel_type_holdout = t(TAS_fuel_amount_zero_fill[(n_train + 1):n_days,1:n_fuel_type])

# MASE calculation

TAS_clr_MASE_arima = TAS_clr_MASE_ets = 
TAS_clr_MASE_arima_K6 = TAS_clr_MASE_ets_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    TAS_clr_MASE_arima[iwk] = ftsa:::mase(forecast = TAS_fuel_type_fore_mat_arima[,iwk], 
                                          outsampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                          insampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    TAS_clr_MASE_ets[iwk] = ftsa:::mase(forecast = TAS_fuel_type_fore_mat_ets[,iwk], 
                                        outsampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                        insampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])

    TAS_clr_MASE_arima_K6[iwk] = ftsa:::mase(forecast = TAS_fuel_type_fore_mat_arima_K6[,iwk], 
                                          outsampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                          insampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    TAS_clr_MASE_ets_K6[iwk] = ftsa:::mase(forecast = TAS_fuel_type_fore_mat_ets_K6[,iwk], 
                                        outsampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                        insampletrue = TAS_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)  
}

round(mean(TAS_clr_MASE_arima), 4)  # 0.1849
round(mean(TAS_clr_MASE_ets), 4)    # 0.192

round(mean(TAS_clr_MASE_arima_K6), 4)  # 0.1956
round(mean(TAS_clr_MASE_ets_K6), 4)    # 0.2036

