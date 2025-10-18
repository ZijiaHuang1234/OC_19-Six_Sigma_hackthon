# ============================================================
# backend.R —— 与 Shiny 前端对接的后端（缓存 + 更稳健的按日对齐）
# ============================================================

# -------------------- 依赖安装 & 引入 --------------------
.req_pkgs <- c("xgboost", "dplyr", "Metrics", "readr", "tibble", "lubridate")
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
  
  pred_train <- predict(booster, dm$dmat)
  r2   <- 1 - sum((pred_train - dm$y)^2) / sum((mean(dm$y) - dm$y)^2)
  rmse <- Metrics::rmse(dm$y, pred_train)
  mae  <- Metrics::mae(dm$y, pred_train)
  
  feature_names <- dm$X_names
  
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
      nrounds = cached_nrounds,
      verbose = 0
    )
    assign(key, mdl, envir = .xgb_cache)
  }
  get(key, envir = .xgb_cache)
}

# -------------------- ⏱️ 时间列自动识别与标准化 --------------------
.candidate_time_cols <- c(
  "date","Date","DATE","ds","day","Day",
  "datetime","Datetime","date_time","timestamp","Timestamp",
  "time","Time"
)

# 返回 list(df=处理后的数据框, key_col="date")；若没找到则 key_col = NULL
.normalize_time_key <- function(df) {
  key <- intersect(.candidate_time_cols, names(df))
  if (length(key) == 0) return(list(df = df, key_col = NULL))
  
  key <- key[[1]]                          # 取第一个匹配
  vals <- df[[key]]
  
  # 尝试解析为 POSIXct 或 Date
  # 1) 先把字符/因子转字符
  if (is.factor(vals)) vals <- as.character(vals)
  
  if (inherits(vals, "POSIXct") || inherits(vals, "POSIXt")) {
    # ok
    dt <- vals
  } else if (inherits(vals, "Date")) {
    dt <- vals
  } else if (is.numeric(vals)) {
    # 可能是 Unix 时间戳（秒）
    dt <- suppressWarnings(as.POSIXct(vals, origin = "1970-01-01", tz = "UTC"))
  } else {
    # 字符串尝试解析
    s <- as.character(vals)
    # 如果带时间（yyyy-mm-dd hh:mm:ss），先转 POSIXct
    dt <- suppressWarnings(lubridate::ymd_hms(s, quiet = TRUE))
    if (all(is.na(dt))) {
      # 再试仅日期（yyyy-mm-dd / yyyy/mm/dd / ymd）
      dt <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
    }
    if (all(is.na(dt))) {
      # 有些数据是 "2024-10-01T12:00:00Z"
      dt <- suppressWarnings(lubridate::ymd_hms(s, tz = "UTC", quiet = TRUE))
    }
  }
  
  # 如果仍解析失败，就保持原列不动，但不给 key
  if (all(is.na(dt))) {
    message("[backend] 时间列解析失败：", key, "（示例：", paste(utils::head(vals,3), collapse=", "), ")")
    return(list(df = df, key_col = NULL))
  }
  
  # 添加规范化列：date（天粒度）
  df$.__dt__ <- dt
  df$date <- as.Date(dt)
  list(df = df, key_col = "date")
}

# 如果天气是小时级，而实际是天级：对天气按天聚合 P_predicted（均值）
# 你也可以改成 sum/median 等口径
.aggregate_weather_daily <- function(pred_df) {
  if (!("date" %in% names(pred_df))) return(pred_df)
  # 如果有多行同一天，说明是小时级或更细；我们聚合
  if (any(duplicated(pred_df$date))) {
    message("[backend] 检测到天气是子日粒度，按天聚合 P_predicted（均值）")
    pred_df <- pred_df |>
      dplyr::group_by(date) |>
      dplyr::summarise(P_predicted = mean(P_predicted, na.rm = TRUE), .groups = "drop")
  } else {
    pred_df <- pred_df |> dplyr::select(date, P_predicted)
  }
  pred_df
}

# -------------------- 🧠 与 Shiny 对接的统一入口 --------------------
run_calculation <- function(weather_csv, actual_csv, out_csv,
                            k = 0.05,
                            train_csv = "sgv.csv",
                            target_col = "generated_power_kw") {
  if (!file.exists(weather_csv)) stop("找不到 weather_csv: ", weather_csv)
  if (!file.exists(actual_csv))  stop("找不到 actual_csv: ", actual_csv)
  out_dir <- dirname(out_csv)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 1) 取（或训练）模型 —— 已缓存
  m <- .get_or_fit_model(train_csv = train_csv, target_col = target_col)
  
  # 2) 天气 → 预测（按你的规则缩放）
  pred_df <- m$predict_from_csv(weather_csv) |>
    dplyr::mutate(P_predicted = .pred * 7 / 30)
  
  # 3) 读“测试输入例”
  df_actual <- readr::read_csv(actual_csv, show_col_types = FALSE)
  
  # 4) 时间键标准化
  norm_pred   <- .normalize_time_key(pred_df);   pred_df   <- norm_pred$df
  norm_actual <- .normalize_time_key(df_actual); df_actual <- norm_actual$df
  
  # 5) 如果实际有日级 date，而天气是小时级/分钟级，先把天气聚合到日级
  if (!is.null(norm_actual$key_col) && norm_actual$key_col == "date") {
    pred_df <- .aggregate_weather_daily(pred_df)
  }
  
  # 6) 合并逻辑（优先按 date；否则行数相等按行对齐）
  if (!is.null(norm_pred$key_col) && !is.null(norm_actual$key_col) &&
      norm_pred$key_col == "date" && norm_actual$key_col == "date") {
    df_out <- df_actual |>
      dplyr::left_join(pred_df |> dplyr::select(date, P_predicted), by = "date")
  } else if (nrow(pred_df) == nrow(df_actual)) {
    message("[backend] 未找到共同的日期键，按行序对齐")
    df_out <- dplyr::bind_cols(df_actual, pred_df["P_predicted"])
  } else {
    # 诊断信息
    message("—— 合并失败诊断 ——")
    message("weather 列名：", paste(names(pred_df), collapse = ", "))
    message("actual  列名：", paste(names(df_actual), collapse = ", "))
    if ("date" %in% names(pred_df))   message("weather 非空日期样例：", paste(utils::head(na.omit(pred_df$date), 3), collapse = ", "))
    if ("date" %in% names(df_actual)) message("actual  非空日期样例：", paste(utils::head(na.omit(df_actual$date), 3), collapse = ", "))
    message("weather 行数：", nrow(pred_df), " | actual 行数：", nrow(df_actual))
    stop(
      "合并失败：未能找到共同的日期键且两表行数不同。\n",
      "请确保：\n",
      "1) 两个CSV都含可解析的日期/时间列（如 date/Date/datetime/timestamp 等），并且能匹配；或\n",
      "2) 两个CSV行数相同以便按行对齐。"
    )
  }
  
  # 7) 计算衰减相关：ratio、severe（不输出 time_0）
  required_cols <- c("time", "P_test", "P_predicted")
  miss <- setdiff(required_cols, names(df_out))
  if (length(miss)) {
    stop("缺少必要列：", paste(miss, collapse = ", "),
         "\n请确认‘测试输入例.csv’包含 time、P_test，且天气预测已生成 P_predicted（若为小时级，已自动按日聚合）。")
  }
  
  ratio  <- df_out$P_test / df_out$P_predicted
  time_0 <- -log(ratio) / k
  severe <- time_0 - df_out$time
  
  bad_idx <- which(!is.finite(time_0))
  if (length(bad_idx) > 0) {
    message("警告：存在 ratio<=0 或非法值，相关 severe 置为 NA。行：",
            paste(bad_idx, collapse = ", "))
    ratio[bad_idx]  <- NA_real_
    severe[bad_idx] <- NA_real_
  }
  
  df_final <- df_out |>
    dplyr::mutate(ratio = ratio, severe = severe) |>
    dplyr::arrange(dplyr::desc(severe))
  
  readr::write_csv(df_final, out_csv)
  
  # 8) 返回给前端展示
  P_pred_scalar <- suppressWarnings(mean(df_final$P_predicted, na.rm = TRUE))
  
  list(P_predicted = P_pred_scalar, out_csv = out_csv)
}
