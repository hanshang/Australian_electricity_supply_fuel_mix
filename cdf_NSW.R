#############################
# Forecast proportion by CDF
#############################

# EVR

NSW_fuel_cdf_EVR = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    NSW_fuel_cdf_EVR[,iwk] = cdf_transformation(data = NSW_fuel_proportion_zero_fill[1:(n_train + iwk - 1),], 
                                               fh = 1, fmethod = "ets", ncomp_method = "EVR")
    print(iwk); rm(iwk)
}
rownames(NSW_fuel_cdf_EVR) = fuel_type
colnames(NSW_fuel_cdf_EVR) = 1:n_test

# K = 6

NSW_fuel_cdf_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    NSW_fuel_cdf_K6[,iwk] = cdf_transformation(data = NSW_fuel_proportion_zero_fill[1:(n_train + iwk - 1),], 
                                               fh = 1, fmethod = "ets", ncomp_method = "fixed")
    print(iwk); rm(iwk); rm(ftsm_obj_K6); rm(NSW_fuel_clr_fore_K6)
}
rownames(NSW_fuel_cdf_K6) = fuel_type
colnames(NSW_fuel_cdf_K6) = 1:n_test

####################
# MASE (proportion)
####################

# EVR

NSW_fuel_cdf_MASE_prop_EVR = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    NSW_fuel_cdf_MASE_prop_EVR[iwk] = mase(forecast = NSW_fuel_cdf_EVR[,iwk], 
                                          outsampletrue = NSW_fuel_proportion_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                          insampletrue = NSW_fuel_proportion_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)
}

# K = 6

NSW_fuel_cdf_MASE_prop_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    NSW_fuel_cdf_MASE_prop_K6[iwk] = mase(forecast = NSW_fuel_cdf_K6[,iwk], 
                                          outsampletrue = NSW_fuel_proportion_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                          insampletrue = NSW_fuel_proportion_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)
}

round(mean(NSW_fuel_cdf_MASE_prop_EVR), 4) # 0.0791
round(mean(NSW_fuel_cdf_MASE_prop_K6), 4)  # 0.1134 

########################
# forecast total supply
########################

NSW_fuel_sum_fore_arima = NSW_fuel_sum_fore_ets = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    NSW_fuel_sum_fore_arima[iwk] = exp(forecast(auto.arima(log(NSW_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    NSW_fuel_sum_fore_ets[iwk]   = exp(forecast(ets(log(NSW_fuel[1:(n_train + iwk - 1),"Sum"])), h = 1)$mean)
    print(iwk); rm(iwk)
}

## EVR

NSW_fuel_type_fore_mat_arima_EVR_cdf = NSW_fuel_type_fore_mat_ets_EVR_cdf = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    NSW_fuel_type_fore_mat_arima_EVR_cdf[,iwk] = NSW_fuel_cdf_EVR[,iwk] * NSW_fuel_sum_fore_arima[iwk]
    NSW_fuel_type_fore_mat_ets_EVR_cdf[,iwk]   = NSW_fuel_cdf_EVR[,iwk] * NSW_fuel_sum_fore_ets[iwk]
    print(iwk); rm(iwk)
}
rownames(NSW_fuel_type_fore_mat_arima_EVR_cdf) = rownames(NSW_fuel_type_fore_mat_ets_EVR_cdf) = fuel_type
colnames(NSW_fuel_type_fore_mat_arima_EVR_cdf) = colnames(NSW_fuel_type_fore_mat_ets_EVR_cdf) = 1:n_test

# MASE

NSW_cdf_MASE_arima_EVR = NSW_cdf_MASE_ets_EVR = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    NSW_cdf_MASE_arima_EVR[iwk] = mase(forecast = NSW_fuel_type_fore_mat_arima_EVR_cdf[,iwk], 
                                      outsampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                      insampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    NSW_cdf_MASE_ets_EVR[iwk] = mase(forecast = NSW_fuel_type_fore_mat_ets_EVR_cdf[,iwk], 
                                    outsampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                    insampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)  
}

round(mean(NSW_cdf_MASE_arima_EVR), 4)  # 0.0876
round(mean(NSW_cdf_MASE_ets_EVR), 4)    # 0.0894

## K = 6

NSW_fuel_type_fore_mat_arima_K6_cdf = NSW_fuel_type_fore_mat_ets_K6_cdf = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    NSW_fuel_type_fore_mat_arima_K6_cdf[,iwk] = NSW_fuel_cdf_K6[,iwk] * NSW_fuel_sum_fore_arima[iwk]
    NSW_fuel_type_fore_mat_ets_K6_cdf[,iwk]   = NSW_fuel_cdf_K6[,iwk] * NSW_fuel_sum_fore_ets[iwk]
    print(iwk); rm(iwk)
}
rownames(NSW_fuel_type_fore_mat_arima_K6_cdf) = rownames(NSW_fuel_type_fore_mat_ets_K6_cdf) = fuel_type
colnames(NSW_fuel_type_fore_mat_arima_K6_cdf) = colnames(NSW_fuel_type_fore_mat_ets_K6_cdf) = 1:n_test

# MASE

NSW_cdf_MASE_arima_K6 = NSW_cdf_MASE_ets_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    NSW_cdf_MASE_arima_K6[iwk] = mase(forecast = NSW_fuel_type_fore_mat_arima_K6_cdf[,iwk], 
                                      outsampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                      insampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    NSW_cdf_MASE_ets_K6[iwk] = mase(forecast = NSW_fuel_type_fore_mat_ets_K6_cdf[,iwk], 
                                    outsampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                    insampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)  
}

round(mean(NSW_cdf_MASE_arima_K6), 4)  # 0.1223
round(mean(NSW_cdf_MASE_ets_K6), 4)    # 0.1235

