################
# read data set
################

QLD_fuel = read.csv("daily_fuel_mix_QLD.csv")
QLD_fuel_amount = QLD_fuel[,2:11]
QLD_fuel_amount_zero_fill = matrix(NA, nrow(QLD_fuel_amount), ncol(QLD_fuel_amount))
for(iw in 1:ncol(QLD_fuel_amount))
{
    QLD_fuel_amount_zero_fill[,iw] = replace(QLD_fuel_amount[,iw], which(is.na(QLD_fuel_amount[,iw])), 0)
    print(iw); rm(iw)
}
colnames(QLD_fuel_amount_zero_fill) = c(fuel_type, "Sum")

fuel_type = c("Battery.Storage", "Black.coal", "Methane", "Diesel.oil", "Hydro", "Kerosene",
              "Natural Gas", "Solar", "Wind")
n_fuel_type = length(fuel_type)

# historical data reconstruction

all(round(apply(QLD_fuel_amount_zero_fill[,1:9], 1, sum), 2) == round(QLD_fuel_amount_zero_fill[,10], 2)) # TRUE

# set up training and testing data

n_days = nrow(QLD_fuel)
n_test = ceiling(n_days * 0.25)
n_train = n_days - n_test

# hierarchical time series forecasting (top-down or bottom-up)

QLD_fuel_type_bu_fore = QLD_fuel_type_tdgsa_fore = QLD_fuel_type_tdgsf_fore = QLD_fuel_type_tdfp_fore = matrix(NA, n_fuel_type, n_test)
for(iwk in 1:n_test)
{
    hts_obj = QLD_fuel_amount_zero_fill[1:(n_train + iwk - 1),1:n_fuel_type]
    
    # BU

    dum = forecast(hts(hts_obj), h = 1, method = "bu", fmethod = "ets")
    QLD_fuel_type_bu_fore[,iwk] = dum$bts
    rm(dum)

    # TDGSA

    dum = forecast(hts(hts_obj), h = 1, method = "tdgsa", fmethod = "ets")
    QLD_fuel_type_tdgsa_fore[,iwk] = dum$bts
    rm(dum)

    # TDGSF

    dum = forecast(hts(hts_obj), h = 1, method = "tdgsf", fmethod = "ets")
    QLD_fuel_type_tdgsf_fore[,iwk] = dum$bts
    rm(dum)

    # TDFP

    dum = forecast(hts(hts_obj), h = 1, method = "tdfp", fmethod = "ets")
    QLD_fuel_type_tdfp_fore[,iwk] = dum$bts
    print(iwk); rm(iwk); rm(dum); rm(hts_obj)
}
rownames(QLD_fuel_type_bu_fore) = rownames(QLD_fuel_type_tdgsa_fore) =
rownames(QLD_fuel_type_tdgsf_fore) = rownames(QLD_fuel_type_tdfp_fore) = fuel_type

# holdout data samples

fuel_type_holdout = t(QLD_fuel_amount_zero_fill[(n_train + 1):n_days,1:n_fuel_type])

# MASE

mase = ftsa:::mase

QLD_hts_bu_MASE = QLD_hts_tdgsa_MASE = QLD_hts_tdgsf_MASE = QLD_hts_tdfp_MASE = vector("numeric", n_test)
for(iwk in 1:n_test)
{
    QLD_hts_bu_MASE[iwk] = mase(forecast = QLD_fuel_type_bu_fore[,iwk], 
                                outsampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                insampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    QLD_hts_tdgsa_MASE[iwk] = mase(forecast = QLD_fuel_type_tdgsa_fore[,iwk], 
                                   outsampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                   insampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    QLD_hts_tdgsf_MASE[iwk] = mase(forecast = QLD_fuel_type_tdgsf_fore[,iwk], 
                                   outsampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                   insampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    
    QLD_hts_tdfp_MASE[iwk] = mase(forecast = QLD_fuel_type_tdfp_fore[,iwk], 
                                  outsampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk),1:n_fuel_type], 
                                  insampletrue = QLD_fuel_amount_zero_fill[(n_train + iwk - 1),1:n_fuel_type])
    print(iwk); rm(iwk)
}

QLD_hts_err_MASE = cbind(QLD_hts_bu_MASE, QLD_hts_tdgsa_MASE, QLD_hts_tdgsf_MASE, QLD_hts_tdfp_MASE)
colnames(QLD_hts_err_MASE) = c("BU", "TDGSA", "TDGSF", "TDFP")
rownames(QLD_hts_err_MASE) = 1:n_test

table(apply(QLD_hts_err_MASE, 1, which.min)) # 216 24 9 208

round(mean(QLD_hts_bu_MASE), 4)    # 0.037
round(mean(QLD_hts_tdgsa_MASE), 4) # 0.0797
round(mean(QLD_hts_tdgsf_MASE), 4) # 0.0804
round(mean(QLD_hts_tdfp_MASE), 4)  # 0.0373   

boxplot(QLD_hts_err_MASE, ylab = "MASE")
