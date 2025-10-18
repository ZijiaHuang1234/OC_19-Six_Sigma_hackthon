# app.R —— 预设天气（sunny/cloudy/rainy/snowy）+ 温度/海平面气压手输 → 调用后端 → 显示 P_predicted & 输出CSV预览
# 依赖：shiny, bslib, readr, DT
req_pkgs <- c("shiny", "bslib", "readr", "DT", "tibble")
to_install <- setdiff(req_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(req_pkgs, library, character.only = TRUE))

theme_dark <- bs_theme(
  version = 5, bg = "#121212", fg = "#f3f3f3", primary = "#f2a900",
  base_font = font_google("Inter", local = TRUE)
)

kpi_css <- HTML("
.kpi-card { background:#1e1e1e; border-radius:14px; padding:16px; border:1px solid #2a2a2a; }
.kpi-title { font-size:0.9rem; opacity:0.8; margin:0; }
.kpi-value { font-size:2rem; font-weight:800; margin:4px 0 0 0; }
.small-hint { font-size:0.9rem; opacity:0.8; }
")

ui <- page_fluid(
  theme = theme_dark,
  kpi_css,
  title = "功率预测与输出预览（预设天气）",
  layout_columns(
    col_widths = c(4, 8),
    
    # 左侧：天气输入 + 实测CSV + 计算
    card(
      card_header("天气输入（不再上传CSV）"),
      card_body(
        selectInput(
          "weather_preset", "选择天气预设",
          choices = c("sunny", "cloudy", "rainy", "snowy"),
          selected = "sunny"
        ),
        div(class="small-hint",
            "预设会自动填入：relative_humidity_2_m_above_gnd、total_precipitation_sfc、snowfall_amount_sfc、",
            "total_cloud_cover_sfc、wind_speed_10_m_above_gnd、wind_direction_10_m_above_gnd。"
        ),
        hr(),
        numericInput("inp_temp", "temperature_2_m_above_gnd (°C)", value = 25, step = 0.1),
        numericInput("inp_mslp", "mean_sea_level_pressure_MSL (hPa)", value = 1013.25, step = 0.1),
        hr(),
        p("上传用于比对/寿命指标的实际数据 CSV（可选增强，但建议包含 time(年)、P_test(实测功率) 与可选 date）"),
        fileInput("file_actual", "上传：测试输入例 CSV", accept = ".csv", buttonLabel = "选择文件"),
        div(style="display:flex; gap:8px; align-items:center; margin-top:8px;",
            actionButton("do_calc", "计算", class="btn btn-primary"),
            uiOutput("dl_btn_ui")
        ),
        hr(),
        div(class="kpi-card",
            p(class="kpi-title","P_predicted"),
            textOutput("predicted_text", inline = TRUE)
        )
      )
    ),
    
    # 右侧：输出CSV预览
    card(
      full_screen = TRUE,
      card_header("输出 CSV 预览（前 10 行）"),
      card_body(
        div(class="small-hint","计算成功后在此显示输出文件前 10 行内容"),
        DT::dataTableOutput("preview_tbl"),
        uiOutput("out_path_ui")
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(P_pred=NULL, out_csv_path=NULL, preview_df=NULL)
  
  # 预设 → 数值映射（按你给的规格）
  preset_row <- reactive({
    pr <- switch(input$weather_preset,
                 "sunny" = list(rh=30,  tp=0,   snow=0,   tcc=0,  wind=0,  wdir=0),
                 "cloudy"= list(rh=40,  tp=0,   snow=0,   tcc=60, wind=10, wdir=100),
                 "rainy" = list(rh=60,  tp=1,   snow=0,   tcc=70, wind=30, wdir=150),
                 "snowy" = list(rh=35,  tp=0.2, snow=0.1, tcc=20, wind=20, wdir=50)
    )
    tibble::tibble(
      # 你的列名规范（与后端/模型一致）
      date = Sys.Date(),
      relative_humidity_2_m_above_gnd = pr$rh,
      total_precipitation_sfc         = pr$tp,
      snowfall_amount_sfc             = pr$snow,
      total_cloud_cover_sfc           = pr$tcc,
      wind_speed_10_m_above_gnd       = pr$wind,
      wind_direction_10_m_above_gnd   = pr$wdir,
      temperature_2_m_above_gnd       = input$inp_temp,
      mean_sea_level_pressure_MSL     = input$inp_mslp
    )
  })
  
  # 下载按钮
  output$dl_btn_ui <- renderUI({
    req(rv$out_csv_path)
    downloadButton("download_out_csv", "下载输出 CSV")
  })
  output$out_path_ui <- renderUI({
    req(rv$out_csv_path)
    div(class="small-hint", paste("输出文件：", normalizePath(rv$out_csv_path, winslash="/")))
  })
  output$download_out_csv <- downloadHandler(
    filename = function() paste0("output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) file.copy(rv$out_csv_path, file, overwrite = TRUE)
  )
  
  output$predicted_text <- renderText({
    if (is.null(rv$P_pred)) "（尚未计算）" else as.character(rv$P_pred)
  })
  
  output$preview_tbl <- DT::renderDataTable({
    req(rv$preview_df)
    DT::datatable(rv$preview_df, options=list(pageLength=10, dom="tip"), rownames=FALSE)
  })
  
  observeEvent(input$do_calc, {
    withProgress(message="正在计算...", value=0.1, {
      # 1) 实测CSV路径（可为空）
      actual_path <- if (!is.null(input$file_actual)) input$file_actual$datapath else NULL
      
      # 2) 输出路径
      out_dir <- file.path(getwd(), "outputs")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      out_csv_path <- file.path(out_dir, paste0("result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
      
      incProgress(0.2, detail="加载后端脚本...")
      backend_path <- file.path(getwd(), "backend.R")
      validate(need(file.exists(backend_path), "未找到 backend.R。请把后端脚本放至项目根目录并命名为 backend.R。"))
      source(backend_path, local = TRUE)
      
      incProgress(0.5, detail="运行计算...")
      
      # 优先调用新的接口：run_calculation_from_row(weather_row_df, actual_csv, out_csv)
      res <- NULL
      used <- NULL
      row_df <- preset_row()
      
      if (exists("run_calculation_from_row", mode="function")) {
        res <- try(run_calculation_from_row(weather_row_df = row_df, actual_csv = actual_path, out_csv = out_csv_path), silent = TRUE)
        used <- "run_calculation_from_row"
      } else if (exists("run_calculation", mode="function")) {
        # 兼容老接口：把 row_df 暂存成临时CSV再调用
        tmp_wcsv <- tempfile(fileext = ".csv")
        readr::write_csv(row_df, tmp_wcsv)
        res <- try(run_calculation(weather_csv = tmp_wcsv, actual_csv = actual_path, out_csv = out_csv_path), silent = TRUE)
        used <- "run_calculation(tmp)"
      } else if (exists("predict_expected", mode="function")) {
        tmp_wcsv <- tempfile(fileext = ".csv")
        readr::write_csv(row_df, tmp_wcsv)
        res <- try(predict_expected(weather_csv = tmp_wcsv, actual_csv = actual_path), silent = TRUE)
        used <- "predict_expected(tmp)"
      } else {
        stop("后端脚本中未找到 run_calculation_from_row() / run_calculation() / predict_expected()。")
      }
      
      if (inherits(res, "try-error") || is.null(res)) {
        stop(paste0("后端计算失败：", as.character(res)))
      }
      
      incProgress(0.75, detail="整理结果与输出文件...")
      
      if (identical(used, "predict_expected(tmp)")) {
        validate(
          need(!is.null(res$P_predicted), "后端未返回 P_predicted。"),
          need(!is.null(res$output_df), "后端未返回 output_df。")
        )
        rv$P_pred <- res$P_predicted
        readr::write_csv(res$output_df, out_csv_path)
        rv$out_csv_path <- out_csv_path
      } else {
        validate(
          need(!is.null(res$P_predicted), "后端未返回 P_predicted。"),
          need(!is.null(res$out_csv) && file.exists(res$out_csv), "后端未生成有效的输出 CSV。")
        )
        rv$P_pred <- res$P_predicted
        rv$out_csv_path <- res$out_csv
      }
      
      out_all <- suppressMessages(readr::read_csv(rv$out_csv_path, show_col_types = FALSE))
      rv$preview_df <- utils::head(out_all, 10)
      
      incProgress(1, detail="完成 ✓")
    })
  })
}

shinyApp(ui, server)
