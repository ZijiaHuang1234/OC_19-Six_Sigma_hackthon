req_pkgs <- c("xgboost", "dplyr", "Metrics", "readr", "tibble")
to_install <- setdiff(req_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(req_pkgs, library, character.only = TRUE))

# 工具：把数据框 -> DMatrix（只保留数值列，剔除目标列）
.make_dmatrix <- function(df, y_col) {
  stopifnot(y_col %in% names(df))
  y <- df[[y_col]]
  X <- df[, setdiff(names(df), y_col), drop = FALSE]
  num_cols <- names(X)[vapply(X, is.numeric, logical(1))]
  if (!length(num_cols)) stop("没有可用的数值型自变量列。")
  X <- as.matrix(X[, num_cols, drop = FALSE])
  list(dmat = xgb.DMatrix(X, label = y), X_names = num_cols, y = y)
}

# 主函数：只训练，不做测试/验证集划分
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
  # 1) 读训练集并过滤夜间
  df <- readr::read_csv(train_csv, show_col_types = FALSE)
  if (!is.null(day_filter_col) && day_filter_col %in% names(df)) {
    df <- dplyr::filter(df, .data[[day_filter_col]] > day_min)
  }
  if (!(target_col %in% names(df))) stop(sprintf("找不到目标列 '%s'", target_col))
  
  # 2) 组装 DMatrix 并训练
  dm <- .make_dmatrix(df, y_col = target_col)
  set.seed(seed)
  booster <- xgboost::xgb.train(
    params = params,
    data   = dm$dmat,
    nrounds = nrounds,
    verbose = verbose
  )
  
  # 3) 训练集上的指标（用于记录）
  pred_train <- predict(booster, dm$dmat)
  r2  <- 1 - sum((pred_train - dm$y)^2) / sum((mean(dm$y) - dm$y)^2)
  rmse_val <- Metrics::rmse(dm$y, pred_train)
  mae_val  <- Metrics::mae(dm$y, pred_train)
  
  metrics_train <- list(R2 = r2, RMSE = rmse_val, MAE = mae_val)
  
  # 4) 构造预测闭包（只接收自变量）
  feature_names <- dm$X_names
  
  predict_fn <- function(newX) {
    # newX: data.frame / matrix，只含自变量；列名尽量与训练时一致
    if (is.matrix(newX)) newX <- as.data.frame(newX, stringsAsFactors = FALSE)
    if (!is.data.frame(newX)) stop("newX 需要是 data.frame 或 matrix")
    
    miss <- setdiff(feature_names, names(newX))
    if (length(miss)) {
      message("预测输入缺少列，已填 NA：", paste(miss, collapse = ", "))
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

# 训练（仅 sgv.csv；返回对象 m）
m <- fit_xgb_power("sgv.csv")
m$metrics_train   # 查看训练集上的 R2/RMSE/MAE

# 从CSV读取测试数据并预测
pred_df <- m$predict_from_csv("test.csv")

# 查看预测结果
#print(pred_df)
print(pred_df$.pred)
