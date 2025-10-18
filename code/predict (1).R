library(xgboost)
library(dplyr)
library(Metrics)
library(readr)
library(tibble)

req_pkgs <- c("xgboost", "dplyr", "Metrics", "readr", "tibble")
installed <- rownames(installed.packages())
missing   <- setdiff(req_pkgs, installed)
if (length(missing) > 0) install.packages(missing, dependencies = TRUE, quiet = TRUE)
invisible(lapply(req_pkgs, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))


# Utility: data.frame -> DMatrix (keep only numeric columns, drop target column)
.make_dmatrix <- function(df, y_col) {
  stopifnot(y_col %in% names(df))
  y <- df[[y_col]]
  X <- df[, setdiff(names(df), y_col), drop = FALSE]
  num_cols <- names(X)[vapply(X, is.numeric, logical(1))]
  if (!length(num_cols)) stop("No numeric predictor columns available.")
  X <- as.matrix(X[, num_cols, drop = FALSE])
  list(dmat = xgb.DMatrix(X, label = y), X_names = num_cols, y = y)
}

# Main function: train only (no train/validation split)
fit_xgb_power <- function(train_csv = "sgv.csv",
                          target_col = "generated_power_kw",
                          day_filter_col = "shortwave_radiation_backwards_sfc",
                          day_min = 50,
                          params = list(
                            objective = "reg:squarederror",
                            eval_metric = "rmse",
                            eta = 0.05,
                            max_depth = 6,
                            subsample = 0.8,
                            colsample_bytree = 0.8
                          ),
                          nrounds = 500,
                          verbose = 0,
                          seed = 123) {
  # 1) Read training set and filter out night-time rows
  df <- readr::read_csv(train_csv, show_col_types = FALSE)
  if (!is.null(day_filter_col) && day_filter_col %in% names(df)) {
    df <- dplyr::filter(df, .data[[day_filter_col]] > day_min)
  }
  if (!(target_col %in% names(df))) stop(sprintf("Target column '%s' not found.", target_col))
  
  # 2) Build DMatrix and train
  dm <- .make_dmatrix(df, y_col = target_col)
  set.seed(seed)
  booster <- xgboost::xgb.train(
    params  = params,
    data    = dm$dmat,
    nrounds = nrounds,
    verbose = verbose
  )
  
  # 3) Training-set metrics (for logging)
  pred_train <- predict(booster, dm$dmat)
  r2  <- 1 - sum((pred_train - dm$y)^2) / sum((mean(dm$y) - dm$y)^2)
  rmse_val <- Metrics::rmse(dm$y, pred_train)
  mae_val  <- Metrics::mae(dm$y, pred_train)
  
  metrics_train <- list(R2 = r2, RMSE = rmse_val, MAE = mae_val)
  
  # 4) Prediction closure (expects predictors only)
  feature_names <- dm$X_names
  
  predict_fn <- function(newX) {
    # newX: data.frame / matrix, predictors only; column names should match training
    if (is.matrix(newX)) newX <- as.data.frame(newX, stringsAsFactors = FALSE)
    if (!is.data.frame(newX)) stop("newX must be a data.frame or matrix.")
    
    miss <- setdiff(feature_names, names(newX))
    if (length(miss)) {
      message("Missing columns in prediction input; filled with NA: ", paste(miss, collapse = ", "))
      for (cc in miss) newX[[cc]] <- NA_real_
    }
    newX <- newX[, feature_names, drop = FALSE]
    newX <- as.matrix(newX)
    tibble(.pred = as.numeric(predict(booster, newX)))
  }
  
  predict_from_csv <- function(feature_csv) {
    new_df <- readr::read_csv(feature_csv, show_col_types = FALSE)
    predict_fn(new_df) |> dplyr::bind_cols(new_df)
  }
  
  list(
    model = booster,
    feature_names = feature_names,
    metrics_train = metrics_train,
    predict = predict_fn,
    predict_from_csv = predict_from_csv
  )
}

# Train on sgv.csv (returns object m)
m <- fit_xgb_power("sgv.csv")
m$metrics_train   # View R2/RMSE/MAE on the training set

# Predict from a CSV of features
pred_df <- m$predict_from_csv("test.csv")

# View predictions
#print(pred_df)
print(pred_df$.pred)
