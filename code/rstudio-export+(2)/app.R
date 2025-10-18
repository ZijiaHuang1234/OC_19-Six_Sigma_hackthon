# app.R —— 上传“天气使用例/测试输入例” → 调用后端 → 显示 P_predicted & 输出CSV前10行
# 依赖：shiny, bslib, readr, DT（可选）
# 放置位置：项目根目录，与 backend.R 同级（或按需修改 source 路径）

# -------------------- 依赖 --------------------
req_pkgs <- c("shiny", "bslib", "readr", "DT")
to_install <- setdiff(req_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(req_pkgs, library, character.only = TRUE))

# -------------------- 主题样式 --------------------
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

# -------------------- UI --------------------
ui <- page_fluid(
  theme = theme_dark,
  kpi_css,
  title = "功率预测与输出预览",
  layout_columns(
    col_widths = c(4, 8),
    
    # 左侧：上传 & 计算
    card(
      full_screen = FALSE,
      card_header("输入文件（CSV）"),
      card_body(
        p("请分别上传：", strong("天气使用例.csv"), " 与 ", strong("测试输入例.csv")),
        tags$ul(
          class = "small-hint",
          tags$li("文件必须为 CSV 格式（UTF-8 或含表头的常规CSV）。"),
          tags$li("“天气使用例”建议包含列：date、气温/湿度/气压/风速/辐照等与你后端一致的字段名。"),
          tags$li("“测试输入例”建议包含列：用于回归/比对的实际测量数据，与后端期望一致。")
        ),
        fileInput("file_weather", "上传：天气使用例 CSV", accept = c(".csv"), buttonLabel = "选择文件"),
        fileInput("file_actual",  "上传：测试输入例 CSV", accept = c(".csv"), buttonLabel = "选择文件"),
        div(
          style = "display:flex; gap:8px; align-items:center; margin-top:8px;",
          actionButton("do_calc", "计算", class = "btn btn-primary"),
          uiOutput("dl_btn_ui")
        ),
        hr(),
        div(
          class = "kpi-card",
          p(class = "kpi-title", "P_predicted"),
          textOutput("predicted_text", inline = TRUE)
        )
      )
    ),
    
    # 右侧：输出CSV预览（前10行）
    card(
      full_screen = TRUE,
      card_header("输出 CSV 预览（前 10 行）"),
      card_body(
        div(class = "small-hint", "计算成功后在此显示输出文件前 10 行内容"),
        DT::dataTableOutput("preview_tbl"),
        uiOutput("out_path_ui")
      )
    )
  )
)

# -------------------- Server --------------------
server <- function(input, output, session) {
  
  # 结果状态
  rv <- reactiveValues(
    P_pred = NULL,
    out_csv_path = NULL,
    preview_df = NULL
  )
  
  # 下载按钮（仅当 out_csv 存在时显示）
  output$dl_btn_ui <- renderUI({
    req(rv$out_csv_path)
    downloadButton("download_out_csv", "下载输出 CSV")
  })
  output$out_path_ui <- renderUI({
    req(rv$out_csv_path)
    div(class = "small-hint", paste("输出文件：", normalizePath(rv$out_csv_path, winslash = "/")))
  })
  output$download_out_csv <- downloadHandler(
    filename = function() {
      paste0("output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      file.copy(rv$out_csv_path, file, overwrite = TRUE)
    }
  )
  
  # 显示 P_predicted
  output$predicted_text <- renderText({
    if (is.null(rv$P_pred)) "（尚未计算）" else as.character(rv$P_pred)
  })
  
  # 输出表格预览
  output$preview_tbl <- DT::renderDataTable({
    req(rv$preview_df)
    DT::datatable(rv$preview_df, options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
  })
  
  # 点击“计算”
  observeEvent(input$do_calc, {
    validate(
      need(!is.null(input$file_weather), "请先上传“天气使用例” CSV。"),
      need(!is.null(input$file_actual),  "请先上传“测试输入例” CSV。")
    )
    
    withProgress(message = "正在计算...", value = 0.1, {
      
      # 1) 取上传文件临时路径
      weather_path <- input$file_weather$datapath
      actual_path  <- input$file_actual$datapath
      
      # 2) 准备输出目录与文件名
      out_dir <- file.path(getwd(), "outputs")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      out_csv_path <- file.path(out_dir, paste0("result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
      
      incProgress(0.2, detail = "加载后端脚本...")
      # 3) 加载后端脚本（确保 backend.R 在同一 R 项目中）
      backend_path <- file.path(getwd(), "backend.R")
      validate(need(file.exists(backend_path), "未找到 backend.R。请将后端脚本放至项目根目录并命名为 backend.R。"))
      source(backend_path, local = TRUE)
      
      incProgress(0.5, detail = "运行计算...")
      # 4) 调用后端：支持两种接口（任选其一）
      #   A) run_calculation(weather_csv, actual_csv, out_csv) → list(P_predicted=..., out_csv=...)
      #   B) predict_expected(weather_csv, actual_csv) → list(P_predicted=..., output_df=...)
      res <- NULL
      used_interface <- NULL
      
      if (exists("run_calculation", mode = "function")) {
        res <- try(
          run_calculation(weather_csv = weather_path, actual_csv = actual_path, out_csv = out_csv_path),
          silent = TRUE
        )
        used_interface <- "run_calculation"
      } else if (exists("predict_expected", mode = "function")) {
        res <- try(
          predict_expected(weather_csv = weather_path, actual_csv = actual_path),
          silent = TRUE
        )
        used_interface <- "predict_expected"
      } else {
        stop("后端脚本中未找到 run_calculation() 或 predict_expected() 函数。\n",
             "请在 backend.R 中实现其中之一。")
      }
      
      # 错误处理
      if (inherits(res, "try-error") || is.null(res)) {
        stop(paste0("后端计算失败：", as.character(res)))
      }
      
      incProgress(0.75, detail = "整理结果与输出文件...")
      
      # 5) 统一整理结果与输出
      if (identical(used_interface, "run_calculation")) {
        # 期望：res$P_predicted，res$out_csv
        validate(
          need(!is.null(res$P_predicted), "后端未返回 P_predicted。"),
          need(!is.null(res$out_csv) && file.exists(res$out_csv), "后端未生成有效的输出 CSV。")
        )
        rv$P_pred <- res$P_predicted
        rv$out_csv_path <- res$out_csv
      } else {
        # used_interface = "predict_expected"
        # 期望：res$P_predicted，res$output_df（由前端写出 CSV）
        validate(
          need(!is.null(res$P_predicted), "后端未返回 P_predicted。"),
          need(!is.null(res$output_df), "后端未返回 output_df。")
        )
        rv$P_pred <- res$P_predicted
        readr::write_csv(res$output_df, out_csv_path)
        rv$out_csv_path <- out_csv_path
      }
      
      # 6) 读取前10行用于预览（更健壮：先读全部，再 head(10)）
      out_all <- suppressMessages(readr::read_csv(rv$out_csv_path, show_col_types = FALSE))
      rv$preview_df <- utils::head(out_all, 10)
      
      incProgress(1, detail = "完成 ✓")
    })
  })
}

shinyApp(ui, server)
