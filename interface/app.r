library(shiny)
library(bslib)
library(scales)

# ---------- 主题 & 样式 ----------
theme_dark <- bs_theme(
  version   = 5,
  bg        = "#120f0a",
  fg        = "#f1e7d2",
  primary   = "#f2a900",
  base_font = font_google("Inter")
)

css <- HTML("
  .kpi-card { background:#2c2212; border-radius:12px; padding:14px; }
  .kpi-title { font-size:0.9rem; opacity:0.8; }
  .kpi-value { font-size:2rem; font-weight:800; margin-top:2px; }
  .kpi-delta { opacity:0.75; font-size:0.9rem; }
  .section-title { font-weight:800; font-size:1.1rem; margin:18px 0 10px; }
  .pill-ok { display:inline-block; padding:4px 10px; border-radius:999px;
             background:#0f3b2a; color:#c8f5e5; font-weight:700; }
  .pill-alert { display:inline-block; padding:4px 10px; border-radius:999px;
                background:#4e0f0f; color:#ffd1d1; font-weight:700; }
  .cfg-card { background:#1b140c; border-radius:12px; padding:12px; }
  .small-note { opacity:0.7; font-size:0.85rem; }
")

# ---------- 固定天气 → 期望产出 ----------
EXPECTED_BY_WEATHER <- c(
  Sunny  = 40000,
  Cloudy = 3550,
  Rainy  = 32000
)

ui <- fluidPage(
  theme = theme_dark,
  tags$head(tags$style(css)),

  titlePanel(
    div(HTML("☀️ Solar QC — Severity (3σ Control Limit)"), style = "font-weight:700;")
  ),

  fluidRow(
    column(
      4,
      div(
        class = "cfg-card",
        h5("① 选择当天天气"),
        selectInput(
          "weather", "Weather",
          choices = names(EXPECTED_BY_WEATHER),
          selected = "Sunny"
        ),
        helpText(
          class = "small-note",
          sprintf(
            "映射：Sunny=%s，Cloudy=%s，Rainy=%s",
            EXPECTED_BY_WEATHER[["Sunny"]],
            EXPECTED_BY_WEATHER[["Cloudy"]],
            EXPECTED_BY_WEATHER[["Rainy"]]
          )
        )
      ),
      div(
        class = "cfg-card",
        h5("② 输入 Actual Output（总量）"),
        numericInput("actual", "Actual Output", value = NA, min = 0, step = 1),
        actionButton("run", "计算 Severity", class = "btn-warning w-100"),
        br(),
        actionButton("reset", "重置历史", class = "btn-secondary w-100")
      )
    ),

    column(
      8,
      div(class = "section-title", "KPI"),
      fluidRow(
        column(
          2,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "Expected"),
            div(class = "kpi-value", textOutput("kpi_expected", inline = TRUE)),
            div(class = "kpi-delta", textOutput("kpi_weather", inline = TRUE))
          )
        ),
        column(
          2,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "Actual"),
            div(class = "kpi-value", textOutput("kpi_actual", inline = TRUE)),
            div(class = "kpi-delta", "输入的实际产出")
          )
        ),
        column(
          2,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "Severity"),
            div(class = "kpi-value", textOutput("kpi_ratio", inline = TRUE)),
            div(class = "kpi-delta", "Actual / Expected")
          )
        ),
        column(
          2,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "Mean (μ)"),
            div(class = "kpi-value", textOutput("kpi_mu", inline = TRUE)),
            div(class = "kpi-delta", "基于历史(不含当次)")
          )
        ),
        column(
          2,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "Sigma (σ)"),
            div(class = "kpi-value", textOutput("kpi_sigma", inline = TRUE)),
            div(class = "kpi-delta", "基于历史(不含当次)")
          )
        ),
        column(
          2,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "LCL (μ-3σ)"),
            div(class = "kpi-value", textOutput("kpi_lcl", inline = TRUE)),
            div(class = "kpi-delta", uiOutput("kpi_alert", inline = TRUE))
          )
        )
      ),

      # ------ 过滤器 ------
      div(class = "section-title", "过滤器（影响控制图 & 历史表）"),
      fluidRow(
        column(
          4,
          div(
            class = "cfg-card",
            h6("分档筛选"),
            selectInput(
              "sev_band", NULL,
              choices  = c("All", "<0.70", "0.70–0.85", "0.85–1.00", ">1.00"),
              selected = "All"
            )
          )
        ),
        column(
          4,
          div(
            class = "cfg-card",
            h6("连续区间"),
            sliderInput(
              "sev_range", NULL,
              min = 0, max = 1.5,
              value = c(0, 1.5), step = 0.01
            ),
            helpText(class = "small-note", "区间会与分档/仅报警组合生效")
          )
        ),
        column(
          4,
          div(
            class = "cfg-card",
            h6("仅看报警"),
            checkboxInput("only_alerts", "Severity < LCL", value = FALSE)
          )
        )
      ),

      div(class = "section-title", "控制图（Severity - 最近 50 点）"),
      plotOutput("control_chart", height = "260px"),

      div(class = "section-title", "历史（应用过滤后的最近 12 条）"),
      tableOutput("history_preview")
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(history = NULL)

  observeEvent(input$reset, {
    rv$history <- NULL
    showNotification("历史已清空。", type = "message")
  })

  observeEvent(input$run, {
    req(input$weather)
    exp_val <- EXPECTED_BY_WEATHER[[input$weather]]
    act_val <- suppressWarnings(as.numeric(input$actual))
    if (!is.finite(act_val)) {
      showNotification("请填写有效的 Actual Output 数值。", type = "error")
      return()
    }
    severity <- act_val / exp_val
    new_row <- data.frame(
      time     = Sys.time(),
      weather  = input$weather,
      expected = exp_val,
      actual   = act_val,
      severity = severity,
      stringsAsFactors = FALSE
    )
    rv$history <- rbind(rv$history, new_row)
  }, ignoreInit = TRUE)

  # 计算 μ, σ, LCL 基于“历史（不含当次）”
  stats_current <- reactive({
    h <- rv$history
    if (is.null(h) || nrow(h) == 0) return(NULL)

    if (nrow(h) >= 2) {
      hist_for_limits <- head(h$severity, -1)
      hist_for_limits <- hist_for_limits[is.finite(hist_for_limits)]
    } else {
      hist_for_limits <- numeric(0)
    }

    if (length(hist_for_limits) >= 2) {
      mu  <- mean(hist_for_limits, na.rm = TRUE)
      sg  <- sd(hist_for_limits, na.rm = TRUE)
      lcl <- max(0, mu - 3 * sg)
    } else {
      mu <- NA_real_; sg <- NA_real_; lcl <- NA_real_
    }

    list(current = tail(h, 1), mu = mu, sigma = sg, lcl = lcl)
  })

  # ---------- 过滤逻辑（控制图 & 历史表使用） ----------
  filtered_history <- reactive({
    h <- rv$history
    if (is.null(h) || nrow(h) == 0) return(h)

    st      <- stats_current()
    lcl_now <- if (!is.null(st)) st$lcl else NA_real_

    # 1) 分档筛选
    band <- input$sev_band %||% "All"
    if (band != "All") {
      if (band == "<0.70")     h <- subset(h, severity < 0.70)
      if (band == "0.70–0.85") h <- subset(h, severity >= 0.70 & severity < 0.85)
      if (band == "0.85–1.00") h <- subset(h, severity >= 0.85 & severity <= 1.00)
      if (band == ">1.00")     h <- subset(h, severity > 1.00)
    }

    # 2) 连续区间
    rng <- input$sev_range %||% c(0, 1.5)
    h   <- subset(h, is.finite(severity) & severity >= rng[1] & severity <= rng[2])

    # 3) 仅看报警（Severity < LCL）
    if (isTRUE(input$only_alerts) && is.finite(lcl_now)) {
      h <- subset(h, severity < lcl_now)
    }

    h
  })

  # ---------- KPI ----------
  output$kpi_expected <- renderText({
    st <- stats_current(); if (is.null(st)) return("—")
    format(round(st$current$expected, 2), big.mark = ",")
  })

  output$kpi_weather <- renderText({
    st <- stats_current(); if (is.null(st)) return("—")
    paste0("Weather: ", st$current$weather)
  })

  output$kpi_actual <- renderText({
    st <- stats_current(); if (is.null(st)) return("—")
    format(round(st$current$actual, 2), big.mark = ",")
  })

  output$kpi_ratio <- renderText({
    st <- stats_current(); if (is.null(st)) return("—")
    percent(st$current$severity, accuracy = 0.1)
  })

  output$kpi_mu <- renderText({
    st <- stats_current()
    if (is.null(st) || !is.finite(st$mu)) return("—")
    percent(st$mu, accuracy = 0.1)
  })

  output$kpi_sigma <- renderText({
    st <- stats_current()
    if (is.null(st) || !is.finite(st$sigma)) return("—")
    percent(st$sigma, accuracy = 0.1)
  })

  output$kpi_lcl <- renderText({
    st <- stats_current()
    if (is.null(st) || !is.finite(st$lcl)) return("—")
    percent(st$lcl, accuracy = 0.1)
  })

  output$kpi_alert <- renderUI({
    st <- stats_current(); if (is.null(st)) return(span("—"))
    if (!is.finite(st$lcl)) return(span(class = "pill-ok", "样本不足"))
    if (is.finite(st$current$severity) && st$current$severity < st$lcl) {
      span(class = "pill-alert", "ALERT")
    } else {
      span(class = "pill-ok", "OK")
    }
  })

  # ---------- 控制图（点集受过滤器影响；3σ线基于全部数据以保持一致性） ----------
  output$control_chart <- renderPlot({
    h_all <- rv$history
    h     <- filtered_history()
    if (is.null(h_all) || nrow(h_all) == 0) return()

    # 最近 50 点（先按过滤后，再截取）
    if (!is.null(h) && nrow(h) > 0) {
      h   <- tail(h, 50)
      sev <- h$severity
    } else {
      sev <- numeric(0)
    }
    x <- seq_along(sev)

    # 参考线：基于全部数据（含当次）
    r_all   <- h_all$severity
    mu_all  <- if (length(r_all) >= 2) mean(r_all, na.rm = TRUE) else NA_real_
    sg_all  <- if (length(r_all) >= 2) sd(r_all, na.rm = TRUE) else NA_real_
    ucl_all <- if (is.finite(mu_all) && is.finite(sg_all)) pmin(1.5, mu_all + 3 * sg_all) else NA_real_
    lcl_all <- if (is.finite(mu_all) && is.finite(sg_all)) pmax(0,    mu_all - 3 * sg_all) else NA_real_

    par(bg = "#221a0f", col.axis = "#f1e7d2", col.lab = "#f1e7d2", col.main = "#f1e7d2")

    ylim_top <- max(
      1.1,
      ifelse(length(sev) > 0, max(sev, na.rm = TRUE), 1),
      ifelse(is.finite(ucl_all), ucl_all, 1)
    )

    plot(
      x, sev,
      type = "b", pch = 16, lwd = 2,
      xlab = "Observation (filtered)", ylab = "Severity",
      col = "#f2a900", ylim = c(0, ylim_top)
    )
    if (is.finite(mu_all))  abline(h = mu_all,  lwd = 2, col = "#888888")
    if (is.finite(ucl_all)) abline(h = ucl_all, lwd = 2, col = "#2e7d32")
    if (is.finite(lcl_all)) abline(h = lcl_all, lwd = 2, col = "#b81e1e")

    axis(
      2,
      at = pretty(c(0, sev, ucl_all, lcl_all), n = 5),
      labels = percent(pretty(c(0, sev, ucl_all, lcl_all), n = 5))
    )
    title(main = "Severity Control Chart (Filtered Points, Global 3σ Lines)")

    legend(
      "bottomright",
      legend = c(
        paste0("μ=",   ifelse(is.finite(mu_all),  percent(mu_all,  0.1), "—")),
        paste0("UCL=", ifelse(is.finite(ucl_all), percent(ucl_all, 0.1), "—")),
        paste0("LCL=", ifelse(is.finite(lcl_all), percent(lcl_all, 0.1), "—"))
      ),
      bty = "n", text.col = "#f1e7d2"
    )
  })

  # ---------- 历史表（应用过滤） ----------
  output$history_preview <- renderTable({
    h <- filtered_history()
    if (is.null(h) || nrow(h) == 0) return(NULL)
    tail(
      transform(
        h,
        expected = round(expected, 2),
        actual   = round(actual, 2),
        severity = round(severity, 4)
      ),
      12
    )
  })
}

shinyApp(ui, server)
