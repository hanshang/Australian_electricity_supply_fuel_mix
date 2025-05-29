#############################
# Forecast proportion by CDF
#############################

# EVR

VIC_fuel_cdf_EVR = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    VIC_fuel_cdf_EVR[,iwk] = cdf_transformation(data = VIC_fuel_proportion_zero_fill[1:(n_train + iwk - 1),], 
                                                fh = 1, fmethod = "ets", ncomp_method = "EVR")
    print(iwk); rm(iwk)
}
rownames(VIC_fuel_cdf_EVR) = fuel_type
colnames(VIC_fuel_cdf_EVR) = 1:n_test

# K = 6

VIC_fuel_cdf_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    VIC_fuel_cdf_K6[,iwk] = cdf_transformation(data = VIC_fuel_proportion_zero_fill[1:(n_train + iwk - 1),], 
                                               fh = 1, fmethod = "ets", ncomp_method = "provide")
    print(iwk); rm(iwk); rm(ftsm_obj_K6); rm(VIC_fuel_clr_fore_K6)
}
rownames(VIC_fuel_cdf_K6) = fuel_type
colnames(VIC_fuel_cdf_K6) = 1:n_test

####################
# MASE (proportion)
####################

# EVR

VIC_fuel_cdf_MASE_prop_EVR = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    VIC_fuel_cdf_MASE_prop_EVR[iwk] = mase(forecast = VIC_fuel_cdf_EVR[,iwk], 
                                           outsampletrue = VIC_fuel_proportion_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                           insampletrue = VIC_fuel_proportion_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)
}

# K = 6

VIC_fuel_cdf_MASE_prop_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    VIC_fuel_cdf_MASE_prop_K6[iwk] = mase(forecast = VIC_fuel_cdf_K6[,iwk], 
                                          outsampletrue = VIC_fuel_proportion_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                          insampletrue = VIC_fuel_proportion_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)
}

round(mean(VIC_fuel_cdf_MASE_prop_EVR), 4) # 0.1249
round(mean(VIC_fuel_cdf_MASE_prop_K6), 4)  # 0.1522

########################
# forecast total supply
########################

VIC_fuel_sum_fore_arima = VIC_fuel_sum_fore_ets = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    VIC_fuel_sum_fore_arima[iwk] = exp(forecast(auto.arima(log(VIC_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    VIC_fuel_sum_fore_ets[iwk]   = exp(forecast(ets(log(VIC_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    print(iwk); rm(iwk)
}

## EVR

VIC_fuel_type_fore_mat_arima_EVR_cdf = VIC_fuel_type_fore_mat_ets_EVR_cdf = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    VIC_fuel_type_fore_mat_arima_EVR_cdf[,iwk] = VIC_fuel_cdf_EVR[,iwk] * VIC_fuel_sum_fore_arima[iwk]
    VIC_fuel_type_fore_mat_ets_EVR_cdf[,iwk]   = VIC_fuel_cdf_EVR[,iwk] * VIC_fuel_sum_fore_ets[iwk]
    print(iwk); rm(iwk)
}
rownames(VIC_fuel_type_fore_mat_arima_EVR_cdf) = rownames(VIC_fuel_type_fore_mat_ets_EVR_cdf) = fuel_type
colnames(VIC_fuel_type_fore_mat_arima_EVR_cdf) = colnames(VIC_fuel_type_fore_mat_ets_EVR_cdf) = 1:n_test

# MASE

VIC_cdf_MASE_arima_EVR = VIC_cdf_MASE_ets_EVR = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    VIC_cdf_MASE_arima_EVR[iwk] = mase(forecast = VIC_fuel_type_fore_mat_arima_EVR_cdf[,iwk], 
                                       outsampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                       insampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    VIC_cdf_MASE_ets_EVR[iwk] = mase(forecast = VIC_fuel_type_fore_mat_ets_EVR_cdf[,iwk], 
                                     outsampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                     insampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)  
}

round(mean(VIC_cdf_MASE_arima_EVR), 4)  # 0.1268
round(mean(VIC_cdf_MASE_ets_EVR), 4)    # 0.1281

## K = 6

VIC_fuel_type_fore_mat_arima_K6_cdf = VIC_fuel_type_fore_mat_ets_K6_cdf = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    VIC_fuel_type_fore_mat_arima_K6_cdf[,iwk] = VIC_fuel_cdf_K6[,iwk] * VIC_fuel_sum_fore_arima[iwk]
    VIC_fuel_type_fore_mat_ets_K6_cdf[,iwk]   = VIC_fuel_cdf_K6[,iwk] * VIC_fuel_sum_fore_ets[iwk]
    print(iwk); rm(iwk)
}
rownames(VIC_fuel_type_fore_mat_arima_K6_cdf) = rownames(VIC_fuel_type_fore_mat_ets_K6_cdf) = fuel_type
colnames(VIC_fuel_type_fore_mat_arima_K6_cdf) = colnames(VIC_fuel_type_fore_mat_ets_K6_cdf) = 1:n_test

# MASE

VIC_cdf_MASE_arima_K6 = VIC_cdf_MASE_ets_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    VIC_cdf_MASE_arima_K6[iwk] = mase(forecast = VIC_fuel_type_fore_mat_arima_K6_cdf[,iwk], 
                                      outsampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                      insampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    VIC_cdf_MASE_ets_K6[iwk] = mase(forecast = VIC_fuel_type_fore_mat_ets_K6_cdf[,iwk], 
                                    outsampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                    insampletrue = VIC_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)  
}

round(mean(VIC_cdf_MASE_arima_K6), 4)  # 0.1522
round(mean(VIC_cdf_MASE_ets_K6), 4)    # 0.1521

