
list2tibble <- function(){
  
  rmethod <- names(results)
  fmethod <- names(results$bu)
  dates <- names(results$bu$ets)
  horizons <- names(results$bu$ets$`2000`)
  levels <- colnames(results$bu$ets$`2000`$`1`) 
  levels[grepl("/",levels)] <- sapply(strsplit(levels[grepl("/",levels)],"/"),`[`,2)
  levels <- data.table("ser_long" = colnames(results$bu$ets$`2000`$`1`),
                       "ser_short" = levels,
                       "k" = 1)
  params <- data.table(CJ(rmethod,fmethod,dates,horizons), "k" = 1)
  grid <- merge(grid,levels,by = "k", allow.cartesian = T)[,k:=NULL]
  
  out <- lapply(1:nrow(grid), function(ix) {
    print(ix)
    dat <- results[[grid[[ix,1]]]][[grid[[ix,2]]]][[grid[[ix,3]]]][[grid[[ix,4]]]]["MASE",]
    dat[which(names(dat) == grid[[ix,5]] | names(dat) == grid[[ix,6]])]
    return(unname(dat))
  })
  
  
  if(length(levels) == 1) out <- t(out)
  colnames(out) <- levels
  out <- as_tibble(cbind(grid,out))
  out$Date <- as.character(as.numeric(out$Date)+as.numeric(out$Horizon)-1)
  out %>% 
    add_column(.before = 1,
               "Category" = recode(out$recon_key,
                                   "bu" = "Basic Methods",
                                   "mo_cat" = "Basic Methods",
                                   "mo_reg" = "Basic Methods",
                                   "tdgsa_cat" = "Basic Methods",
                                   "tdgsa_reg" = "Basic Methods",
                                   "tdgsf_reg" = "Basic Methods",
                                   "tdgsf_cat" = "Basic Methods",
                                   "tdfp_reg" = "Basic Methods",
                                   "tdfp_cat" = "Basic Methods",
                                   "ols" = "Optimal Methods",
                                   "wls_cat" = "Optimal Methods",
                                   "wls_reg" = "Optimal Methods",
                                   "nseries" = "Optimal Methods",
                                   "unrecon" = "Basic Methods")) %>% 
    add_column(.before = 1,
               "Reconciliation" = recode(out$recon_key,
                                         "bu" = "Bottom-Up",
                                         "mo_cat" = "Middle-Out (Categories)",
                                         "mo_reg" = "Middle-Out (Regions)",
                                         "tdgsa_cat" = "Top-Down A (Categories)",
                                         "tdgsa_reg" = "Top-Down A (Regions)",
                                         "tdgsf_cat" = "Top-Down F (Categories)",
                                         "tdgsf_reg" = "Top-Down F (Regions)",
                                         "tdfp_reg" = "Top-Down P (Regions)",
                                         "tdfp_cat" = "Top-Down P (Categories)",
                                         "ols" = "OLS",
                                         "wls_cat" = "WLS (Categories)",
                                         "wls_reg" = "WLS (Regions)",
                                         "nseries" = "nSeries",
                                         "unrecon" = "Unreconciled")) %>% 
    add_column(.before = 2,
               "Forecast" = recode(out$fcast_key,
                                   "arima" = "ARIMA",
                                   "ets" = "ETS",
                                   "rw" = "RW")) %>% 
    select(-one_of("recon_key","fcast_key"))
}




get_values <- function(recon,fmethod,fdate,horizon, measure = "MASE"){
  t(results[[recon]][[fmethod]][[fdate]][[horizon]])[,measure]}



get_table <- function(recon,fmethod,fdate,horizon,measure = "MASE",levels = c("Total")){
  grid <- expand.grid(list("recon_key" = recon,
                           "fcast_key" = fmethod,
                           "Date" = as.character(fdate),
                           "Horizon" = as.character(horizon)),
                      stringsAsFactors = F)
  out <- as.matrix(t(sapply(1:nrow(grid), function(ix) t(results[[grid[ix,1]]][[grid[ix,2]]]
                                                         [[grid[ix,3]]][[grid[ix,4]]])[levels,measure])))
  if(length(levels) == 1) out <- t(out)
  colnames(out) <- levels
  out <- as_tibble(cbind(grid,out))
  out$Date <- as.character(as.numeric(out$Date)+as.numeric(out$Horizon)-1)
  out %>% 
    add_column(.before = 1,
               "Category" = recode(out$recon_key,
                                   "bu" = "Basic Methods",
                                   "mo_cat" = "Basic Methods",
                                   "mo_reg" = "Basic Methods",
                                   "tdgsa_cat" = "Basic Methods",
                                   "tdgsa_reg" = "Basic Methods",
                                   "tdgsf_reg" = "Basic Methods",
                                   "tdgsf_cat" = "Basic Methods",
                                   "tdfp_reg" = "Basic Methods",
                                   "tdfp_cat" = "Basic Methods",
                                   "ols" = "Optimal Methods",
                                   "wls_cat" = "Optimal Methods",
                                   "wls_reg" = "Optimal Methods",
                                   "nseries" = "Optimal Methods",
                                   "unrecon" = "Basic Methods")) %>% 
    add_column(.before = 1,
               "Reconciliation" = recode(out$recon_key,
                                         "bu" = "Bottom-Up",
                                         "mo_cat" = "Middle-Out (Categories)",
                                         "mo_reg" = "Middle-Out (Regions)",
                                         "tdgsa_cat" = "Top-Down A (Categories)",
                                         "tdgsa_reg" = "Top-Down A (Regions)",
                                         "tdgsf_cat" = "Top-Down F (Categories)",
                                         "tdgsf_reg" = "Top-Down F (Regions)",
                                         "tdfp_reg" = "Top-Down P (Regions)",
                                         "tdfp_cat" = "Top-Down P (Categories)",
                                         "ols" = "OLS",
                                         "wls_cat" = "WLS (Categories)",
                                         "wls_reg" = "WLS (Regions)",
                                         "nseries" = "nSeries",
                                         "unrecon" = "Unreconciled")) %>% 
    add_column(.before = 2,
               "Forecast" = recode(out$fcast_key,
                                   "arima" = "ARIMA",
                                   "ets" = "ETS",
                                   "rw" = "RW")) %>% 
    select(-one_of("recon_key","fcast_key"))
  
}



regions <- c("Europe",
             "North America",
             "East Asia",
             "Africa and Middle East",
             "Latin America",
             "Central Asia","South Asia",
             "Australia and Oceania")

categories <- c("Chemicals and Pharma",
                "Precision Instruments",
                "Machines and Electronics",
                "Metals",
                "Agricultural Products",
                "Textiles",
                "Vehicles",
                "Leather, Rubber, Plastics",
                "Graphical Products",
                "Energy Source",
                "Various Goods",
                "Stones and Earth")



