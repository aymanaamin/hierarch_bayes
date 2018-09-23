
import_data <- function(){
  
  # countries
  load("data/countries.rda")
  countries <- countries[validTo == "12.2999", .(Landcode = numCode, isoCode)]
  
  # define data files to import
  ls_files <- data.table(
    exports = list.files(file.path("data/exports"), "*.txt", full.names = TRUE, recursive = TRUE),
    imports = list.files(file.path("data/imports"), "*.txt", full.names = TRUE, recursive = TRUE)
  )
  
  ls_files <- data.table::melt(ls_files, measure.vars = c("exports", "imports"), variable.name = "dat", value.name = "fil")
  ls_files[, id := seq(.N)]
  
  # import raw data
  data_raw <- ls_files[,
                       .SD[, {
                         message(sprintf("Reading file %s...", fil));
                         laf_open_fwf(fil,
                                      column_types = c("character","character","numeric",
                                                       "character","character","numeric",
                                                       "numeric","numeric"),
                                      column_widths = c(1,12,3,7,4,13,13,13),
                                      column_names = c("direction","group",
                                                       "Landcode","period","volumecode",
                                                       "volume","weight","value")
                         )[,]
                       }
                       , by = fil]
                       , by = list(id, dat)]
  
  data_raw <- data_raw[!is.na(value)]
  
  message("Processing raw data...")
  
  # aggregate value of goods in each entry
  data_agg <- data_raw[, .(value = sum(value)),
                       by = list(dat, direction, Landcode, group, period)]
  
  # merge iso country codes and data
  total <- merge(data_agg, countries)[, Landcode := NULL][, tradingPartner := isoCode][, isoCode := NULL]
  
  # remove entries at the lowest level of aggregation (random shit anyways)
  total = total[-which(nchar(total$group) == 12 | nchar(total$group) == 9), ]
  
  # generate keys
  total[, group_clean := gsub("[.]","",group)]
  total[, group_nchar := nchar(group_clean)] 
  total[, group_8dig := sapply(1:nrow(total), function(jx) {
    paste0(total$group_clean[jx],
           paste(rep(0, 8 - total$group_nchar[jx]),collapse = ""))})]
  total[, tsKey := paste(direction, tradingPartner, group_8dig, sep = "")]
  
  # retrieve dates
  toNumericDate <- function(x) {
    num <- as.numeric(x)
    mnth <- floor(num)
    10000*(num - mnth) + (mnth - 1)/12
  }
  total[, dateNumeric := toNumericDate(period)]
  
  # transform to time series
  value_dt <- dcast(total[, .(tsKey, dateNumeric, value)], "dateNumeric ~ tsKey", value.var = "value")
  class(value_dt) <- "data.frame"
  tsl <- as.list(as.ts(zoo::zoo(x = value_dt[,-1], order.by = value_dt[, 1])))
  tsl <- lapply(tsl, function(x) {
    x[is.na(x)] <- 0
    x
  })
  
  return(tsl)
  
}

create_predictions <- function(x,h){
  
  # Run the bsts model
  ss = AddLocalLinearTrend(list(), x)
  ss = AddSeasonal(ss, x, nseasons = 4)
  bsts.model = bsts(x, state.specification = ss, niter = 500, ping = 0)
  
  # Get a suggested number of burn-ins
  burn = SuggestBurn(0.1, bsts.model)
  
  # Predict
  p = predict.bsts(bsts.model, horizon = h, burn = burn)
  
  out = cbind(p$mean,apply(p$distribution, 2, var))
  colnames(out) = c("mean","var")
  
  return(out)
  
}
