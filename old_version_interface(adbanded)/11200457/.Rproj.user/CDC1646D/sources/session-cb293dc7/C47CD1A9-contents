# ============================================================
# backend.R —— 与 Shiny 前端对接的后端（缓存 + 单一 P_predicted 广播）
# ============================================================

# -------------------- 依赖安装 & 引入 --------------------
.req_pkgs <- c("xgboost", "dplyr", "Metrics", "readr", "tibble", "hms")
.to_install <- setdiff(.req_pkgs, rownames(installed.packages()))
if (length(.to_install)) install.packages(.to_install, quiet = TRUE)
invisible(lapply(.req_pkgs, library, character.only = TRUE))

# -------------------- 工具：数据框 -> DMatrix --------------------
.make_dmatrix <- function(df, y_col) {
  stopifnot(y_col %in% names(df))
  y <- df[[y_col]]
  X <- df[, setdiff(names(df), y_col), drop = FALSE]
  num_cols <- names(X)[vapply(X, is.numeric, logical(1))]
  if (!length(num_cols)) stop("没有可用的数值型自变量列。")
  X <- as.matrix(X[, num_cols, drop = FALSE])
  list(dmat = xgb.DMatrix(X, label = y), X_names = num_cols, y = y)
}

# -------------------- 训练：仅训练（不划分验证集） --------------------
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
                            colsample_bytree = 0.8,
                            nthread = 2
                          ),
                          nrounds = 500,
                          verbose = 0,
                          seed = 123) {
  df <- readr::read_csv(train_csv, show_col_types = FALSE)
  if (!is.null(day_filter_col) && day_filter_col %in% names(df)) {
    df <- dplyr::filter(df, .data[[day_filter_col]] > day_min)
  }
  if (!(target_col %in% names(df))) stop(sprintf("找不到目标列 '%s'", target_col))
  
  dm <- .make_dmatrix(df, y_col = target_col)
  set.seed(seed)
  booster <- xgboost::xgb.train(
    params  = params,
    data    = dm$dmat,
    nrounds = nrounds,
    verbose = verbose
  )
  
  # 训练集指标（可选）
  pred_train <- predict(booster, dm$dmat)
  r2   <- 1 - sum((pred_train - dm$y)^2) / sum((mean(dm$y) - dm$y)^2)
  rmse <- Metrics::rmse(dm$y, pred_train)
  mae  <- Metrics::mae(dm$y, pred_train)
  
  feature_names <- dm$X_names
  
  # 预测闭包
  predict_fn <- function(newX) {
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
    model            = booster,
    feature_names    = feature_names,
    metrics_train    = list(R2 = r2, RMSE = rmse, MAE = mae),
    predict          = predict_fn,
    predict_from_csv = predict_from_csv
  )
}

# -------------------- ✅ 模型缓存：首次训练后复用 --------------------
.xgb_cache <- new.env(parent = emptyenv())

.get_or_fit_model <- function(train_csv = "sgv.csv",
                              target_col = "generated_power_kw",
                              cached_nrounds = 120) {
  key <- paste0(normalizePath(train_csv, winslash = "/"), "|", target_col)
  if (!exists(key, envir = .xgb_cache)) {
    message("[backend] 首次训练模型（仅一次）...")
    mdl <- fit_xgb_power(
      train_csv  = train_csv,
      target_col = target_col,
      params  = list(
        objective = "reg:squarederror", eval_metric = "rmse",
        eta = 0.10, max_depth = 6, subsample = 0.8, colsample_bytree = 0.8,
        nthread = 2
      ),
      nrounds = cached_nrounds,  # 首次训练快一点；确认流程后可上调
      verbose = 0
    )
    assign(key, mdl, envir = .xgb_cache)
  }
  get(key, envir = .xgb_cache)
}

# -------------------- 🧠 与 Shiny 对接的统一入口 --------------------
# 需求：天气 CSV 只有 1 行 → 只算一个 P_predicted → 广播到 actual 全行
run_calculation <- function(weather_csv, actual_csv, out_csv,
                            k = 0.05,
                            train_csv = "sgv.csv",
                            target_col = "generated_power_kw") {
  if (!file.exists(weather_csv)) stop("找不到 weather_csv: ", weather_csv)
  if (!file.exists(actual_csv))  stop("找不到 actual_csv: ", actual_csv)
  out_dir <- dirname(out_csv)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 1) 取模型（缓存）
  m <- .get_or_fit_model(train_csv = train_csv, target_col = target_col)
  
  # 2) 天气 → 预测（通常只有 1 行；若多行则取均值）
  pred_df <- m$predict_from_csv(weather_csv) |>
    dplyr::mutate(P_predicted = .pred * 7 / 30)
  
  if (nrow(pred_df) == 1) {
    P_pred_scalar <- pred_df$P_predicted[1]
    message("[backend] 天气输入仅 1 行，使用单一 P_predicted = ", round(P_pred_scalar, 3))
  } else {
    P_pred_scalar <- suppressWarnings(mean(pred_df$P_predicted, na.rm = TRUE))
    message("[backend] 天气输入多行，使用均值 P_predicted = ", round(P_pred_scalar, 3))
  }
  
  # 3) 读取测试 CSV（必须包含 time, P_test；其他列照常保留，如 packid/packids）
  df_actual <- readr::read_csv(actual_csv, show_col_types = FALSE)
  
  required_cols <- c("time", "P_test")
  miss <- setdiff(required_cols, names(df_actual))
  if (length(miss))
    stop("缺少必要列：", paste(miss, collapse = ", "),
         "（测试使用例.csv 必须含 time 和 P_test）")
  
  # 4) 计算 ratio / severe（不输出 time_0）
  ratio   <- df_actual$P_test / P_pred_scalar
  time_0  <- -log(ratio) / k
  severe  <- time_0 - df_actual$time
  
  # 非法值处理
  bad_idx <- which(!is.finite(time_0))
  if (length(bad_idx) > 0) {
    message("警告：存在 ratio<=0 或非法值，相关 severe 置为 NA。行：",
            paste(bad_idx, collapse = ", "))
    ratio[bad_idx]  <- NA_real_
    severe[bad_idx] <- NA_real_
  }
  
  # 5) 写出结果（保留原有列 + P_predicted/ratio/severe；按 severe 倒序）
  df_out <- df_actual |>
    dplyr::mutate(
      P_predicted = P_pred_scalar,
      ratio = ratio,
      severe = severe
    ) |>
    dplyr::arrange(dplyr::desc(severe))
  
  readr::write_csv(df_out, out_csv)
  
  # 6) 返回前端需要展示的标量 & 输出路径
  list(
    P_predicted = P_pred_scalar,
    out_csv     = out_csv
  )
}


# ------------------------------------------------------------
# 适配新前端：从“单行天气参数 data.frame”直接计算
# 前端会优先调用这个函数；内部仍复用你现有的 run_calculation()
# 要求：actual_csv 必须存在，且含 time 与 P_test
# ------------------------------------------------------------
run_calculation_from_row <- function(weather_row_df,
                                     actual_csv = NULL,
                                     out_csv,
                                     k = 0.05,
                                     train_csv = "sgv.csv",
                                     target_col = "generated_power_kw") {
  # 1) 把单行天气 df 写成临时CSV，让现有流程零改动复用
  if (!is.data.frame(weather_row_df))
    stop("weather_row_df 需要是 data.frame；请检查前端传入。")
  
  tmp_wcsv <- tempfile(fileext = ".csv")
  readr::write_csv(weather_row_df, tmp_wcsv)
  
  # 2) 校验 actual_csv（你的后端逻辑需要它来生成最终表）
  if (is.null(actual_csv) || !nzchar(actual_csv) || !file.exists(actual_csv)) {
    stop("请上传“测试输入例”CSV（必须包含列 time 和 P_test）。")
  }
  
  # 3) 直接复用你已有的 run_calculation（核心逻辑保持不变）
  run_calculation(
    weather_csv = tmp_wcsv,
    actual_csv  = actual_csv,
    out_csv     = out_csv,
    k           = k,
    train_csv   = train_csv,
    target_col  = target_col
  )
}

