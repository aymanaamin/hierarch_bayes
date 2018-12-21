
import_data <- function(){
  
  # countries
  load("dat/countries.rda")
  countries <- countries[validTo == "12.2999", .(numCode, isoCode, regCode)]
  
  # define data files to import
  ls_files <- data.table(
    exports = list.files(file.path("dat/exports"), "*.txt", full.names = TRUE, recursive = TRUE),
    imports = list.files(file.path("dat/imports"), "*.txt", full.names = TRUE, recursive = TRUE)
  )
  
  ls_files <- data.table::melt(ls_files, measure.vars = c("exports", "imports"), variable.name = "dat", value.name = "fil")
  ls_files[, id := seq(.N)]
  
  # import raw data
  data_raw <- ls_files[,
                       .SD[, {
                         message(sprintf("Reading file %s...", fil));
                         laf_open_fwf(fil,
                                      column_types = c("character","character","integer",
                                                       "character","character","numeric",
                                                       "numeric","numeric"),
                                      column_widths = c(1,12,3,7,4,13,13,13),
                                      column_names = c("direction","group",
                                                       "numCode","period","volumecode",
                                                       "volume","weight","value")
                         )[,]
                       }
                       , by = fil]
                       , by = list(id, dat)]
  
  data_raw <- data_raw[!is.na(value)]
  data_raw <- data_raw[!(group %in% c("13.1","13.2","14.1","14.2")),]
  data_raw <- data_raw[, numCode:=as.integer(numCode)]
  
  message("Processing raw data...")
  
  # aggregate value of goods in each entry
  data_agg <- data_raw[, .(value = sum(value)),
                       by = list(dat, direction, numCode, group, period)]
  
  # merge iso country codes and data
  total <- merge(data_agg, countries, by = "numCode")
  
  # generate keys
  total[, group_clean := gsub("[.]","",group)]
  total[, group_nchar := nchar(group_clean)] 
  total[, group_8dig := sapply(1:nrow(total), function(jx) {
    paste0(total$group_clean[jx],
           paste(rep(0, 8 - total$group_nchar[jx]),collapse = ""))})]
  total[, tsKey := paste0(direction, regCode, isoCode, group_8dig)]
  
  return(total[, .(period,value,tsKey)])
  
}


dt2ts <- function(dtx){
  
  # retrieve dates
  toNumericDate <- function(x) {
    num <- as.numeric(x)
    mnth <- floor(num)
    10000*(num - mnth) + (mnth - 1)/12
  }
  
  dtx[, dateNumeric := toNumericDate(period)]
  
  # transform to time series
  value_dt <- dcast(dtx[, .(tsKey, dateNumeric, value)], "dateNumeric ~ tsKey", value.var = "value", fun.aggregate = sum)
  class(value_dt) <- "data.frame"
  tsl <- as.list(as.ts(zoo::zoo(x = value_dt[,-1], order.by = value_dt[, 1])))
  tsl <- lapply(tsl, function(x) {
    x[is.na(x)] <- 0
    x
  })
  
  return(do.call(cbind,tsl))
  
}


# # Some example forecasts to play around with
# sim_hierarch <- function(nodes){
#   
#   # Generate hierarchy
#   q <- sum(tail(nodes,1)[[1]])
#   beta_sim <- runif(q,80,120)
#   dat_hts <- hts(ts(t(beta_sim)), nodes = nodes)
#   
#   # Add recon error
#   S <- smatrix(dat_hts)
#   alpha_sim = runif(nrow(S),-10,10)
#   dat_agg <- aggts(dat_hts)
#   
#   fcasts <- cbind(t(dat_agg + alpha_sim), S %*% runif(q,0,10))
#   colnames(fcasts) <- c("mean","var")
#   
#   return(list("fcasts" = fcasts,
#               "beta" = beta_sim,
#               "alpha" = alpha_sim,
#               "S" = S))
#   
# }
