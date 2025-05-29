################
# read data set
################

VIC_fuel = read.csv("daily_fuel_mix_VIC.csv")

VIC_fuel_proportion = VIC_fuel[,9:14]
VIC_fuel_proportion_zero_fill = matrix(NA, nrow(VIC_fuel_proportion), ncol(VIC_fuel_proportion))
for(iw in 1:ncol(VIC_fuel_proportion))
{
    VIC_fuel_proportion_zero_fill[,iw] = replace(VIC_fuel_proportion[,iw], which(is.na(VIC_fuel_proportion[,iw])), 0)
    print(iw); rm(iw)
}

fuel_type = c("Battery.Storage", "Brown.coal", "Hydro", "Natural.Gas", "Solar", "Wind")
n_fuel_type = length(fuel_type)

# historical data reconstruction

all(round(rowSums(VIC_fuel_proportion_zero_fill),4) == 1) # TRUE

# set up training and testing data

n_days = nrow(VIC_fuel)
n_test = ceiling(n_days * 0.25)
n_train = n_days - n_test

###########################
# centered log ratio (clr)
###########################

# EVR

VIC_fuel_clr_inverse = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    VIC_fuel_clr = clr(VIC_fuel_proportion_zero_fill[1:(n_train + iwk - 1),])
    ncomp_EVR = select_K(tau = 0.001, eigenvalue = svd(VIC_fuel_clr)$d^2)
    
    ftsm_obj = ftsm(fts(1:n_fuel_type, t(VIC_fuel_clr)), order = ncomp_EVR)
    VIC_fuel_clr_fore = as.numeric(forecast(ftsm_obj, h = 1)$mean$y)
    
    VIC_fuel_clr_inverse[,iwk] = clrInv(VIC_fuel_clr_fore)
    print(iwk); rm(iwk)
}
rownames(VIC_fuel_clr_inverse) = fuel_type
colnames(VIC_fuel_clr_inverse) = 1:n_test

# K = 6

VIC_fuel_clr_inverse_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    VIC_fuel_clr = clr(VIC_fuel_proportion_zero_fill[1:(n_train + iwk - 1),])
    
    ftsm_obj = ftsm(fts(1:n_fuel_type, t(VIC_fuel_clr)), order = 6)
    VIC_fuel_clr_fore = as.numeric(forecast(ftsm_obj, h = 1)$mean$y)
    
    VIC_fuel_clr_inverse_K6[,iwk] = clrInv(VIC_fuel_clr_fore)
    print(iwk); rm(iwk)
}
rownames(VIC_fuel_clr_inverse_K6) = fuel_type
colnames(VIC_fuel_clr_inverse_K6) = 1:n_test

# forecast total supply

VIC_fuel_sum_fore_arima = VIC_fuel_sum_fore_ets = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    VIC_fuel_sum_fore_arima[iwk] = exp(forecast(auto.arima(log(VIC_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    VIC_fuel_sum_fore_ets[iwk]   = exp(forecast(ets(log(VIC_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    print(iwk); rm(iwk)
}

# distribute total to fuel type mix

VIC_fuel_type_fore_mat_arima = VIC_fuel_type_fore_mat_ets = 
VIC_fuel_type_fore_mat_arima_K6 = VIC_fuel_type_fore_mat_ets_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    VIC_fuel_type_fore_mat_arima[,iwk] = VIC_fuel_clr_inverse[,iwk] * VIC_fuel_sum_fore_arima[iwk]
    VIC_fuel_type_fore_mat_ets[,iwk]   = VIC_fuel_clr_inverse[,iwk] * VIC_fuel_sum_fore_ets[iwk]
    
    VIC_fuel_type_fore_mat_arima_K6[,iwk] = VIC_fuel_clr_inverse_K6[,iwk] * VIC_fuel_sum_fore_arima[iwk]
    VIC_fuel_type_fore_mat_ets_K6[,iwk]   = VIC_fuel_clr_inverse_K6[,iwk] * VIC_fuel_sum_fore_ets[iwk]
    print(iwk); rm(iwk)
}
rownames(VIC_fuel_type_fore_mat_arima) = rownames(VIC_fuel_type_fore_mat_ets) = 
rownames(VIC_fuel_type_fore_mat_arima_K6) = rownames(VIC_fuel_type_fore_mat_ets_K6) = fuel_type

colnames(VIC_fuel_type_fore_mat_arima) = colnames(VIC_fuel_type_fore_mat_ets) = 
colnames(VIC_fuel_type_fore_mat_arima_K6) = colnames(VIC_fuel_type_fore_mat_ets_K6) = 1:n_test

# holdout data samples

VIC_fuel_amount = VIC_fuel[,2:8]
VIC_fuel_amount_zero_fill = matrix(NA, nrow(VIC_fuel_amount), ncol(VIC_fuel_amount))
for(iw in 1:ncol(VIC_fuel_amount))
{
    VIC_fuel_amount_zero_fill[,iw] = replace(VIC_fuel_amount[,iw], which(is.na(VIC_fuel_amount[,iw])), 0)
    print(iw); rm(iw)
}

fuel_type = c("Battery.Storage", "Brown.coal", "Hydro", "Natural.Gas", "Solar", "Wind")
n_fuel_type = length(fuel_type)

fuel_type_holdout = t(VIC_fuel_amount_zero_fill[(n_train + 1):n_days,1:n_fuel_type])

# MASE calculation

VIC_clr_MASE_arima = VIC_clr_MASE_ets = 
VIC_clr_MASE_arima_K6 = VIC_clr_MASE_ets_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    VIC_clr_MASE_arima[iwk] = ftsa:::mase(forecast = VIC_fuel_type_fore_mat_arima[,iwk], 
                                          outsampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                          insampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    VIC_clr_MASE_ets[iwk] = ftsa:::mase(forecast = VIC_fuel_type_fore_mat_ets[,iwk], 
                                        outsampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                        insampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])

    VIC_clr_MASE_arima_K6[iwk] = ftsa:::mase(forecast = VIC_fuel_type_fore_mat_arima_K6[,iwk], 
                                          outsampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                          insampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    VIC_clr_MASE_ets_K6[iwk] = ftsa:::mase(forecast = VIC_fuel_type_fore_mat_ets_K6[,iwk], 
                                        outsampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                        insampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)  
}

round(mean(VIC_clr_MASE_arima), 4)  # 0.1325
round(mean(VIC_clr_MASE_ets), 4)    # 0.133

round(mean(VIC_clr_MASE_arima_K6), 4)  # 0.1332
round(mean(VIC_clr_MASE_ets_K6), 4)    # 0.1336

