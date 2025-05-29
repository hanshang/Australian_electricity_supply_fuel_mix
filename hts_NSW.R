################
# read data set
################

NSW_fuel = read.csv("daily_fuel_mix_NSW.csv")
NSW_fuel_amount = NSW_fuel[,2:9]
NSW_fuel_amount_zero_fill = matrix(NA, nrow(NSW_fuel_amount), ncol(NSW_fuel_amount))
for(iw in 1:ncol(NSW_fuel_amount))  
{
    NSW_fuel_amount_zero_fill[,iw] = replace(NSW_fuel_amount[,iw], which(is.na(NSW_fuel_amount[,iw])), 0)
    print(iw); rm(iw)
}
colnames(NSW_fuel_amount_zero_fill) = colnames(NSW_fuel)[2:9]

fuel_type = c("Battery.Storage", "Black.coal", "Diesel.oil", "Hydro", "Natural.Gas", "Solar", "Wind")
n_fuel_type = length(fuel_type)

# historical data reconstruction

all(round(apply(NSW_fuel_amount_zero_fill[,1:n_fuel_type], 1, sum), 2) == round(NSW_fuel_amount_zero_fill[,8], 2)) # TRUE

# set up training and testing data

n_days = nrow(NSW_fuel)
n_test = ceiling(n_days * 0.25)
n_train = n_days - n_test

######################################
# Hierarchical or grouped time series
# (top-down or bottom-up)
######################################

NSW_fuel_type_bu_fore = NSW_fuel_type_tdgsa_fore = NSW_fuel_type_tdgsf_fore = NSW_fuel_type_tdfp_fore = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    hts_obj = NSW_fuel_amount_zero_fill[1:(n_train + iwk - 1),1:n_fuel_type]
  
    # BU
  
    dum = forecast(hts(hts_obj), h = 1, method = "bu", fmethod = "ets")
    NSW_fuel_type_bu_fore[,iwk] = dum$bts
    rm(dum)
  
    # TDGSA
    
    dum = forecast(hts(hts_obj), h = 1, method = "tdgsa", fmethod = "ets")
    NSW_fuel_type_tdgsa_fore[,iwk] = dum$bts
    rm(dum)
    
    # TDGSF
    
    dum = forecast(hts(hts_obj), h = 1, method = "tdgsf", fmethod = "ets")
    NSW_fuel_type_tdgsf_fore[,iwk] = dum$bts
    rm(dum)
    
    # TDFP
    
    dum = forecast(hts(hts_obj), h = 1, method = "tdfp", fmethod = "ets")
    NSW_fuel_type_tdfp_fore[,iwk] = dum$bts
    print(iwk); rm(iwk); rm(dum)
}
rownames(NSW_fuel_type_bu_fore) = rownames(NSW_fuel_type_tdgsa_fore) = 
rownames(NSW_fuel_type_tdgsf_fore) = rownames(NSW_fuel_type_tdfp_fore) = fuel_type

# holdout data samples

fuel_type_holdout = t(NSW_fuel_amount_zero_fill[(n_train + 1):n_days,1:7])

# MASE

mase = ftsa:::mase

NSW_hts_bu_MASE = NSW_hts_tdgsa_MASE = NSW_hts_tdgsf_MASE = NSW_hts_tdfp_MASE = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    NSW_hts_bu_MASE[iwk] = mase(forecast = NSW_fuel_type_bu_fore[,iwk], 
                                outsampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                insampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    NSW_hts_tdgsa_MASE[iwk] = mase(forecast = NSW_fuel_type_tdgsa_fore[,iwk], 
                                   outsampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                   insampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    NSW_hts_tdgsf_MASE[iwk] = mase(forecast = NSW_fuel_type_tdgsf_fore[,iwk], 
                                   outsampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                   insampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    NSW_hts_tdfp_MASE[iwk] = mase(forecast = NSW_fuel_type_tdfp_fore[,iwk], 
                                  outsampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                  insampletrue = NSW_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)
}

NSW_hts_err_MASE = cbind(NSW_hts_bu_MASE, NSW_hts_tdgsa_MASE, NSW_hts_tdgsf_MASE, NSW_hts_tdfp_MASE)
colnames(NSW_hts_err_MASE) = c("BU", "TDGSA", "TDGSF", "TDFP")
rownames(NSW_hts_err_MASE) = 1:n_test

table(apply(NSW_hts_err_MASE, 1, which.min)) # 198 39 27 193

round(mean(NSW_hts_bu_MASE), 4)    # 0.0689
round(mean(NSW_hts_tdgsa_MASE), 4) # 0.1181
round(mean(NSW_hts_tdgsf_MASE), 4) # 0.1197
round(mean(NSW_hts_tdfp_MASE), 4)  # 0.0694   

