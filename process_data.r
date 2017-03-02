####
#### Process raw data from Excel files.
####

file_names_cost <- list.files(path = "csv/cost")
file_names_price <- list.files(path = "csv/price")
n_files <- length(file_names_cost)
all(file_names_cost == file_names_price)

## Load raw data.
load_raw <- function(file_name_cost, file_name_price) {
  ## Cost
  cost_in <- read.csv(file.path("csv", "cost", file_name_cost), header = FALSE, 
    skip = 3)
  period_cost <- strsplit(as.character(cost_in[, 1]), "[.]")
  na_cost <- (1:nrow(cost_in))[is.na(period_cost)]
  if(length(na_cost) > 0) {
    cost_in <- cost_in[-na_cost, ]  
    period_cost <- unlist(period_cost[-na_cost])
  } else {
    period_cost <- unlist(period_cost)
  }
  n_cost <- nrow(cost_in)
  yr_cost <- as.numeric(period_cost[seq(from = 1, to = 2 * n_cost - 1, by = 2)])
  mo_cost <- as.numeric(period_cost[seq(from = 2, to = 2 * n_cost, by = 2)])
  id_cost <- as.character(cost_in[, 4])
  cls_cost <- apply(cost_in, 1, function(x) { ifelse(!is.na(x[11]), 1, 
    ifelse(!is.na(x[27]), 2, 3)) })
  X_cost <- matrix(NA, nrow = n_cost, ncol = 16)
  colnames(X_cost) <- c("num_vehicles", "payload_capacity", "veh_acq", "tire_cost", 
    "veh_tax", "driver_sal", "oil_cost", "fuel_cost", "maint_cost", "toll_cost", 
    "handling_cost", "park_cost", "other_cost", "mgmt_cost", "veh_deprec", "num_deliv")
  for(i in 1:n_cost) {
    X_cost[i, ] <- as.numeric(cost_in[i, 11:26 + (cls_cost[i] - 1) * 16])
  }
  X_cost <- data.frame(factor(id_cost), factor(cls_cost), cbind(yr_cost, mo_cost, 
    as.numeric(cost_in[, 10]), X_cost))
  colnames(X_cost)[1:5] <- c("id", "class", "year", "month", "line_dist")

  ## Price
  price_in <- read.csv(file.path("csv", "price", file_name_price), header = FALSE, 
    skip = 3)
  period_price <- strsplit(as.character(price_in[, 1]), "[.]")
  na_price <- (1:nrow(price_in))[is.na(period_price)]
  if(length(na_price) > 0) {
    price_in <- price_in[-na_price, ]  
    period_price <- unlist(period_price[-na_price])
  } else {
    period_price <- unlist(period_price)
  }
  n_price <- nrow(price_in)
  yr_price <- as.numeric(period_price[seq(from = 1, to = 2 * n_price - 1, by = 2)])
  mo_price <- as.numeric(period_price[seq(from = 2, to = 2 * n_price, by = 2)])
  n_price <- nrow(price_in)
  yr_price <- as.numeric(period_price[seq(from = 1, to = 2 * n_price - 1, by = 2)])
  mo_price <- as.numeric(period_price[seq(from = 2, to = 2 * n_price, by = 2)])
  id_price <- as.character(price_in[, 4])
  cls_price <- apply(price_in, 1, function(x) { ifelse(!is.na(x[12]), 1, 
    ifelse(!is.na(x[16]), 2, 3)) })
  X_price <- matrix(NA, nrow = n_price, ncol = 4)
  colnames(X_price) <- c("payload", "out_times", "out_price", "return_price")
  for(i in 1:n_price) {
    X_price[i, ] <- as.numeric(price_in[i, 12:15 + (cls_price[i] - 1) * 4])
  }
  X_price <- data.frame(factor(id_price), factor(cls_price), cbind(yr_price, mo_price, 
    as.numeric(price_in[, 11]), X_price))
  colnames(X_price)[1:5] <- c("id", "class", "year", "month", "line_dist")

  out <- list("X_cost" = X_cost, "X_price" = X_price)
  return(out)
}

X_c_all <- X_p_all <- NULL
for(i in 1:n_files) {
  cat(i)
  out <- load_raw(file_names_cost[i], file_names_price[i])
  X_c_all <- rbind(X_c_all, out$X_cost)
  X_p_all <- rbind(X_p_all, out$X_price)
}

