################
# read data set
################

NSW_fuel = read.csv("daily_fuel_mix_NSW.csv")

#####################
# centered log ratio
#####################

NSW_fuel_proportion = NSW_fuel[,10:16]
NSW_fuel_proportion_zero_fill = matrix(NA, nrow(NSW_fuel_proportion), ncol(NSW_fuel_proportion))
for(iw in 1:ncol(NSW_fuel_proportion))
{
    NSW_fuel_proportion_zero_fill[,iw] = replace(NSW_fuel_proportion[,iw], which(is.na(NSW_fuel_proportion[,iw])), 0)
    print(iw); rm(iw)
}
fuel_type = c("Battery.Storage", "Black.coal", "Diesel.oil", "Hydro", "Natural.Gas", "Solar", "Wind")
colnames(NSW_fuel_proportion_zero_fill) = fuel_type

####################################################
# centered log ratio transformation (EVR and K = 6)
####################################################

# EVR

NSW_fuel_clr_inverse = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    NSW_fuel_clr = clr(NSW_fuel_proportion_zero_fill[1:(n_train + iwk - 1),])
    
    ncomp_EVR = select_K(tau = 0.001, eigenvalue = svd(NSW_fuel_clr)$d^2)
    ftsm_obj = ftsm(fts(1:n_fuel_type, t(NSW_fuel_clr)), order = ncomp_EVR)
    NSW_fuel_clr_fore = as.numeric(forecast(ftsm_obj, h = 1)$mean$y)
    NSW_fuel_clr_inverse[,iwk] = clrInv(NSW_fuel_clr_fore)
    
    print(iwk); rm(iwk); rm(ftsm_obj); rm(NSW_fuel_clr_fore)
}
rownames(NSW_fuel_clr_inverse) = fuel_type
colnames(NSW_fuel_clr_inverse) = 1:n_test

# K = 6

NSW_fuel_clr_inverse_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    NSW_fuel_clr = clr(NSW_fuel_proportion_zero_fill[1:(n_train + iwk - 1),])
    
    ftsm_obj_K6 = ftsm(fts(1:n_fuel_type, t(NSW_fuel_clr)), order = 6)
    NSW_fuel_clr_fore_K6 = as.numeric(forecast(ftsm_obj_K6, h = 1)$mean$y)
    NSW_fuel_clr_inverse_K6[,iwk] = clrInv(NSW_fuel_clr_fore_K6)
    
    print(iwk); rm(iwk); rm(ftsm_obj_K6); rm(NSW_fuel_clr_fore_K6)
}
rownames(NSW_fuel_clr_inverse_K6) = fuel_type
colnames(NSW_fuel_clr_inverse_K6) = 1:n_test
  
#################################################
# Mean Absolute Scaled Error (MASE) (proportion)
##################################################

# EVR

NSW_fuel_clr_MASE_prop = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    NSW_fuel_clr_MASE_prop[iwk] = mase(forecast = NSW_fuel_clr_inverse[,iwk], 
                                       outsampletrue = NSW_fuel_proportion_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                       insampletrue = NSW_fuel_proportion_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)
}

# K = 6

NSW_fuel_clr_MASE_prop_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    NSW_fuel_clr_MASE_prop_K6[iwk] = mase(forecast = NSW_fuel_clr_inverse_K6[,iwk], 
                                       outsampletrue = NSW_fuel_proportion_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                       insampletrue = NSW_fuel_proportion_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)
}

round(mean(NSW_fuel_clr_MASE_prop), 4)    # 0.1256
round(mean(NSW_fuel_clr_MASE_prop_K6), 4) # 0.1262 

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

####################################
# distribute total to fuel type mix
####################################

# EVR

NSW_fuel_type_fore_mat_arima = NSW_fuel_type_fore_mat_ets = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    NSW_fuel_type_fore_mat_arima[,iwk] = NSW_fuel_clr_inverse[,iwk] * NSW_fuel_sum_fore_arima[iwk]
    NSW_fuel_type_fore_mat_ets[,iwk]   = NSW_fuel_clr_inverse[,iwk] * NSW_fuel_sum_fore_ets[iwk]
    print(iwk); rm(iwk)
}
rownames(NSW_fuel_type_fore_mat_arima) = rownames(NSW_fuel_type_fore_mat_ets) = fuel_type
colnames(NSW_fuel_type_fore_mat_arima) = colnames(NSW_fuel_type_fore_mat_ets) = 1:n_test

# K = 6

NSW_fuel_type_fore_mat_arima_K6 = NSW_fuel_type_fore_mat_ets_K6 = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    NSW_fuel_type_fore_mat_arima_K6[,iwk] = NSW_fuel_clr_inverse_K6[,iwk] * NSW_fuel_sum_fore_arima[iwk]
    NSW_fuel_type_fore_mat_ets_K6[,iwk]   = NSW_fuel_clr_inverse_K6[,iwk] * NSW_fuel_sum_fore_ets[iwk]
    print(iwk); rm(iwk)
}
rownames(NSW_fuel_type_fore_mat_arima_K6) = rownames(NSW_fuel_type_fore_mat_ets_K6) = fuel_type
colnames(NSW_fuel_type_fore_mat_arima_K6) = colnames(NSW_fuel_type_fore_mat_ets_K6) = 1:n_test

# holdout data samples

NSW_fuel_amount = NSW_fuel[,2:9]
NSW_fuel_amount_zero_fill = matrix(NA, nrow(NSW_fuel_amount), ncol(NSW_fuel_amount))
for(iw in 1:ncol(NSW_fuel_amount))  
{
    NSW_fuel_amount_zero_fill[,iw] = replace(NSW_fuel_amount[,iw], which(is.na(NSW_fuel_amount[,iw])), 0)
    print(iw); rm(iw)
}
fuel_type_holdout = t(NSW_fuel_amount_zero_fill[(n_train + 1):n_days,1:n_fuel_type])

#################################
# MASE calculation (fuel amount)
#################################

# EVR

NSW_clr_MASE_arima = NSW_clr_MASE_ets = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    NSW_clr_MASE_arima[iwk] = mase(forecast = NSW_fuel_type_fore_mat[,iwk], 
                                   outsampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                   insampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    NSW_clr_MASE_ets[iwk] = mase(forecast = NSW_fuel_type_fore_mat_ets[,iwk], 
                                 outsampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                 insampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)  
}

round(mean(NSW_clr_MASE_arima), 4)  # 0.1317
round(mean(NSW_clr_MASE_ets), 4)    # 0.1327

# K = 6

NSW_clr_MASE_arima_K6 = NSW_clr_MASE_ets_K6 = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    NSW_clr_MASE_arima_K6[iwk] = mase(forecast = NSW_fuel_type_fore_mat_arima_K6[,iwk], 
                                       outsampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                       insampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    NSW_clr_MASE_ets_K6[iwk] = mase(forecast = NSW_fuel_type_fore_mat_ets_K6[,iwk], 
                                     outsampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                     insampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)  
}

round(mean(NSW_clr_MASE_arima_K6), 4)  # 0.1314
round(mean(NSW_clr_MASE_ets_K6), 4)    # 0.1323

