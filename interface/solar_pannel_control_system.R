# ============================================================
# app.R — Solar Panel Quality Control (with Weibull fault model)
# ============================================================

# ---- deps (base app) ----
library(shiny)
library(bslib)
library(scales)

# ---- deps (fault model) ----
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(stringr)
})

# ---------- Theme & Styles ----------
theme_dark <- bs_theme(
  version = 5,
  bg = "#120f0a", fg = "#f1e7d2", primary = "#f2a900",
  base_font = font_google("Inter")
)

css <- HTML("
  .kpi-card { background:#2c2212; border-radius:12px; padding:14px; }
  .kpi-title { font-size:0.9rem; opacity:0.8; }
  .kpi-value { font-size:2rem; font-weight:800; margin-top:2px; }
  .kpi-delta { opacity:0.75; font-size:0.9rem; }
  .section-title { font-weight:800; font-size:1.1rem; margin:18px 0 10px; }
  .rank-item { border-radius:12px; padding:16px; margin-bottom:10px;
               display:flex; justify-content:space-between; align-items:center; color:#f1e7d2; }
  .rank-high { background:linear-gradient(90deg,#4e0f0f,#351010); }
  .rank-med  { background:linear-gradient(90deg,#3b2a14,#2f220f); }
  .rank-low  { background:linear-gradient(90deg,#0e2c1e,#0b2318); }
  .cfg-card { background:#1b140c; border-radius:12px; padding:12px; }
  .small-note { opacity:0.7; font-size:0.85rem; }
  .subtitle { opacity:0.8; margin-top:-8px; margin-bottom:8px; font-size:0.85rem; }
  .info-icon {
    display:inline-flex; align-items:center; justify-content:center;
    width:18px; height:18px; border-radius:50%;
    background:#3a2b12; color:#f2a900; font-weight:800; font-size:12px; margin-left:6px;
    cursor:pointer; user-select:none;
  }
  .rank-arrow { font-size:22px; line-height:1; padding-left:8px; cursor:pointer; user-select:none; }

  /* ---- New spacing helpers ---- */
  .section-gap { margin-top: 12px; margin-bottom: 12px; }
  .block-gap { margin-bottom: 14px; }

  /* Preview scroll container (fixed height = 300px) */
  #preview_wrap { max-height: 300px; overflow: auto; border: 1px solid #3a2b12; border-radius: 10px; }
")

# ---------- Constants ----------
EXPECTED_BY_WEATHER <- c(Sunny=40000, Cloudy=35000, Rainy=32000)  # kW per pack baseline by weather
FALLBACK_THRESHOLD <- 0.85
`%||%` <- function(a,b) if (is.null(a) || (length(a)==1 && is.na(a))) b else a

# ---------- Helper ----------
info_icon <- function(title, content, placement = "right") {
  tags$span(
    class = "info-icon", "i",
    `data-bs-toggle`="popover", `data-bs-trigger`="hover focus",
    `data-bs-placement`=placement, title=title, `data-bs-content`=content
  )
}
sanitize_id <- function(x) gsub("[^a-zA-Z0-9_]", "_", x)

# ---------- (Fallback) Rule-based fault hypotheses ----------
fault_predict_rules <- function(ratio, weather) {
  if (!is.finite(ratio)) {
    return(list(
      list(name="Missing telemetry / parsing error", conf="High",
           action="Check CSV integrity, sensor connectivity, and data pipeline."),
      list(name="Meter offline", conf="Medium",
           action="Inspect logger status and last heartbeat.")
    ))
  }
  if (ratio < 0.40) {
    return(list(
      list(name="Severe failure — discard pack", conf="Critical",
           action="Immediate removal / RMA; isolate from array."),
      list(name="String/inverter outage", conf="High",
           action="Check inverter alarms, DC string currents, fuses, breakers.")
    ))
  } else if (ratio < 0.50) {
    return(list(
      list(name="String/inverter outage", conf="High",
           action="Check inverter derating and alarms."),
      list(name="DC disconnection", conf="High",
           action="Inspect combiner boxes and wiring continuity.")
    ))
  } else if (ratio < 0.70) {
    return(list(
      list(name="Multiple strings offline or severe derating", conf="High",
           action="Review MPPT imbalance and thermal derate."),
      list(name="Wiring/contact resistance", conf="Medium",
           action="IR imaging; tighten terminals; measure IV.")
    ))
  } else if (ratio < 0.85) {
    return(list(
      list(name="Soiling/partial shading", conf="High",
           action="Schedule cleaning; verify pre/post trend."),
      list(name=sprintf("Weather baseline mismatch (%s)", weather), conf="Low",
           action="Validate expected constants vs recent site conditions.")
    ))
  } else if (ratio > 1.10) {
    return(list(
      list(name="Expected baseline too low", conf="Medium",
           action="Revisit expected-per-weather constants."),
      list(name="Meter calibration / gain error", conf="Medium",
           action="Cross-check SCADA vs on-site meters.")
    ))
  } else {
    return(list(
      list(name="Nominal", conf="—",
           action="No action required. Monitor trend.")
    ))
  }
}

# ---------- Weibull fault model helpers ----------
read_fault_table <- function(path = "pv_fault_reliability_table_days.csv"){
  df <- readr::read_csv(path, show_col_types = FALSE)
  std_names <- names(df) |>
    str_replace_all("%", "pct") |>
    str_replace_all("\\.", "_")
  names(df) <- std_names
  df <- df %>%
    rename(
      fault               = fault,
      MTTF_days           = MTTF_days,
      q2_5_days           = any_of("Q2_5pct_days"),
      q97_5_days          = any_of("Q97_5pct_days"),
      shape_k             = any_of("shape_k"),
      scale_lambda_days   = any_of("scale_lambda_days"),
      AFR_1day_pct        = any_of("AFR_1day_pct"),
      AFR_30days_pct      = any_of("AFR_30days_pct"),
      AFR_365days_pct     = any_of("AFR_365days_pct"),
      note                = any_of("note")
    )
  num_cols <- c("shape_k","scale_lambda_days","MTTF_days","q2_5_days","q97_5_days",
                "AFR_1day_pct","AFR_30days_pct","AFR_365days_pct")
  df %>% mutate(across(any_of(num_cols), as.numeric))
}

read_install_table <- function(path = "solar_panel_installations_700x_seq100.csv"){
  readr::read_csv(path, show_col_types = FALSE) %>%
    mutate(
      installation_end_date = suppressWarnings(ymd(installation_end_date)),
      installation_end_date = if_else(is.na(installation_end_date),
                                      ymd(parse_date_time(installation_end_date, orders = c("Y/m/d","Y/m","Ymd"))),
                                      installation_end_date)
    )
}

rank_faults_by_time <- function(fault_tbl, t_days, use_prior = FALSE, prior_col = "AFR_365days_pct"){
  stopifnot(is.numeric(t_days), length(t_days) == 1, t_days > 0)
  ft <- fault_tbl %>% filter(!is.na(shape_k), !is.na(scale_lambda_days))
  dens <- with(ft, (shape_k/scale_lambda_days) * ((t_days/scale_lambda_days)^(pmax(shape_k-1, 0))) *
                 exp(- (t_days/scale_lambda_days)^shape_k ))
  dens[!is.finite(dens) | dens < .Machine$double.xmin] <- .Machine$double.xmin
  if(use_prior && prior_col %in% names(ft)){
    prior <- ft[[prior_col]]
    if(all(prior <= 100, na.rm = TRUE)) prior <- prior/100
    if(any(is.na(prior)) || sum(prior) <= 0) prior <- rep(1, length(dens))
  } else {
    prior <- rep(1, length(dens))
  }
  score <- dens * prior
  prob <- as.numeric(score / sum(score))
  within95 <- (t_days >= ft$q2_5_days) & (t_days <= ft$q97_5_days)
  tibble::tibble(
    fault = ft$fault,
    t_days = t_days,
    shape_k = ft$shape_k,
    scale_lambda_days = ft$scale_lambda_days,
    MTTF_days = ft$MTTF_days,
    q2_5_days = ft$q2_5_days,
    q97_5_days = ft$q97_5_days,
    prior_used = use_prior,
    prior_weight = prior,
    pdf_score = dens,
    rel_prob = prob,
    in_95pct_window = within95
  ) %>% arrange(desc(rel_prob))
}

# ---------- Diagnose helpers ----------
# (保留按需批量诊断的函数，当前在 modal 内按单 pack 直接 rank 更轻量)
# diagnose_drop_by_pack(...) 可按需调用

# ---------- UI ----------
ui <- fluidPage(
  theme = theme_dark,
  tags$head(
    tags$style(css),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
        const list = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"popover\"]'));
        list.map(function (el) { return new bootstrap.Popover(el); });
      });
      if (typeof Shiny !== 'undefined') {
        Shiny.addCustomMessageHandler('init-popovers', function(_) {
          const list = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"popover\"]'));
          list.map(function (el) { return new bootstrap.Popover(el); });
        });
      }
    "))
  ),
  
  titlePanel(tagList(
    div("Solar Panel Quality Control", style="font-weight:800;"),
    div(class="subtitle", "Team: Liyin Zhang, Zijia Huang, Tianle Pang, Jiahe Wan, Ziyi Chen")
  )),
  
  fluidRow(
    column(
      4,
      div(class="cfg-card",
          h5(tagList("① Weather & Cpk threshold", info_icon(
            "About Cpk threshold", "LCL = max(0, μ − 3σ × Cpk). If <2 samples, fallback to fixed 85%."
          ))),
          selectInput("weather_choice", tagList("Weather", info_icon(
            "Weather → Expected",
            "Weather maps to expected output per pack: Sunny=40000, Cloudy=35000, Rainy=32000 (kW)."
          )), choices=names(EXPECTED_BY_WEATHER), selected="Sunny"),
          dateInput("sys_date", tagList("System date", info_icon(
            "Date usage", "Used for logging and as detection date in fault model."
          )), value=Sys.Date(), format="yyyy-mm-dd"),
          numericInput("cpk_target", tagList("Cpk target", info_icon(
            "Cpk target", "Higher Cpk tightens LCL. Typical targets: 1.0–1.67."
          )), value=1.33, min=0, step=0.01),
          radioButtons("cpk_base", tagList("Threshold baseline", info_icon(
            "Baseline choice",
            "Session history: μ,σ from all previous runs this session; This batch: μ,σ from current CSV."
          )),
          inline=TRUE,
          choices=c("Session history"="hist","This batch"="batch"), selected="hist")
      ),
      div(class="cfg-card",
          h5(tagList("② Upload Actual Output", info_icon(
            "CSV requirements",
            "Required: pack_id, actual_output (or actualoutput). Case and spaces ignored. Legacy accepted: packid/panel_id."
          ))),
          fileInput("act_csv", NULL, accept=".csv")
      ),
      # Optional fault model datasets
      div(class="cfg-card",
          h5(tagList("②.1 (Optional) Fault Model Data", info_icon(
            "Separate datasets",
            "If not provided, the app will look for default files in working dir: pv_fault_reliability_table_days.csv and solar_panel_installations_700x_seq100.csv."
          ))),
          checkboxInput("use_custom_fault_ds", "Use custom datasets for fault prediction", value = FALSE),
          conditionalPanel(
            condition = "input.use_custom_fault_ds == true",
            fileInput("fault_tbl_csv", "Reliability table (Weibull): pv_fault_reliability_table_days.csv", accept = ".csv"),
            fileInput("install_tbl_csv", "Installation table: solar_panel_installations_700x_seq100.csv", accept = ".csv"),
            checkboxInput("use_prior", "Use AFR_365days as prior", value = FALSE),
            helpText(class="small-note","If unchecked, the app tries default files in working directory; if none, it falls back to rule-based predictions.")
          )
      ),
      div(class="cfg-card",
          h5("③ Run"),
          actionButton("run_btn", "Compute ratio (append to history)", class="btn-warning w-100"),
          br(),
          actionButton("reset_hist", "Reset session history", class="btn-secondary w-100"),
          br(), br(),
          h6("Preview (current batch)"),
          div(id = "preview_wrap", tableOutput("preview")),
          div(class="section-gap"),
          downloadButton("dl_csv", "Download current batch CSV", class="btn-secondary w-100")
      )
    ),
    
    # ---------- Right Panel ----------
    column(
      8,
      
      # KPIs (wrapped with bottom gap)
      div(class="block-gap",
          fluidRow(
            column(2, div(class="kpi-card",
                          div(class="kpi-title","Average ratio"),
                          div(class="kpi-value", textOutput("kpi_ratio", inline=TRUE)),
                          div(class="kpi-delta","Current batch mean"))),
            column(2, div(class="kpi-card",
                          div(class="kpi-title","Panels in alert"),
                          div(class="kpi-value", textOutput("kpi_alert", inline=TRUE)),
                          div(class="kpi-delta","Below threshold count"))),
            column(2, div(class="kpi-card",
                          div(class="kpi-title","Expected (total, kW)"),
                          div(class="kpi-value", textOutput("kpi_expected", inline=TRUE)),
                          div(class="kpi-delta","Sum of expected performance"))),
            column(2, div(class="kpi-card",
                          div(class="kpi-title","Actual (total, kW)"),
                          div(class="kpi-value", textOutput("kpi_actual", inline=TRUE)),
                          div(class="kpi-delta","Sum of actual output"))),
            column(4, div(class="kpi-card",
                          div(class="kpi-title", tagList("Current threshold", info_icon(
                            "Threshold source",
                            "Cpk-based LCL from selected baseline; falls back to fixed 85% when insufficient samples."
                          ))),
                          div(class="kpi-value", textOutput("kpi_thresh", inline=TRUE)),
                          div(class="kpi-delta", textOutput("kpi_thresh_src", inline=TRUE))))
          )
      ),
      
      # Severity filters (wrapped with bottom gap)
      div(class="section-title","Severity filters (affect charts & ranking)"),
      div(class="block-gap",
          fluidRow(
            column(4, div(class="cfg-card", h6(tagList("Bucket filter", info_icon(
              "Bucket filter",
              "Predefined ratio ranges for quick filtering, e.g., <0.70 = high severity, 0.70–0.85 = medium, etc."
            ))),
            selectInput("sev_band", NULL,
                        choices=c("All","<0.70","0.70–0.85","0.85–1.00",">1.00"),
                        selected="All"))),
            column(4, div(class="cfg-card", h6(tagList("Continuous range", info_icon(
              "Continuous range",
              "Adjust min–max ratio range interactively. Useful for narrowing focus to a specific ratio interval."
            ))),
            sliderInput("sev_range", NULL, min=0, max=1.5, value=c(0,1.5), step=0.01))),
            column(4, div(class="cfg-card", h6(tagList("Only alerts", info_icon(
              "Only alerts",
              "Check to display only those packs whose ratio is below the current Cpk threshold."
            ))),
            checkboxInput("only_alerts", "Show ratio < threshold", value=FALSE)))
          )
      ),
      
      # Ranking + sort tabs + note
      div(class="section-title",
          fluidRow(
            column(8, div(tagList(
              "Low Ratio Ranking (Pack_ID)",
              info_icon("About Pack_ID","Each Pack_ID corresponds to a group of 100 solar panels.")
            ))),
            column(4, div(style="text-align:right;",
                          radioButtons("rank_order", label=NULL, inline=TRUE,
                                       choices=c("Worst \u2192 Top"="asc","Top \u2192 Worst"="desc"),
                                       selected="asc")))
          )
      ),
      uiOutput("ranking_list"),
      div(class="small-note", style="text-align:right; opacity:0.75;",
          "Each pack represents 100 solar panels."),
      div(class="section-gap"),
      
      # Histogram (with extra spacing and 300px height)
      div(class="section-title block-gap","Ratio distribution (Base R)"),
      div(class="section-gap"),
      plotOutput("ratio_hist", height="300px")
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session){
  observe({ session$sendCustomMessage("init-popovers", list()) })
  rv <- reactiveValues(
    history = NULL,
    bound_ids = character(0)
  )
  
  observeEvent(input$reset_hist, {
    rv$history <- NULL
    showNotification("Session history cleared.", type="message")
  })
  
  # --- helpers ---
  read_csv_norm <- function(datapath){
    df <- tryCatch(read.csv(datapath, header=TRUE, stringsAsFactors=FALSE),
                   error=function(e){ showNotification(paste("Read CSV failed:", e$message), type="error"); return(NULL) })
    if (is.null(df)) return(NULL)
    names(df) <- tolower(gsub("\\s+", "", names(df)))
    df
  }
  
  # Actual CSV (pack_id + actual_output)
  act_df <- reactive({
    req(input$act_csv)
    df <- read_csv_norm(input$act_csv$datapath); if (is.null(df)) return(NULL)
    id_col <- if ("pack_id" %in% names(df)) "pack_id" else if ("packid" %in% names(df)) "packid" else if ("panel_id" %in% names(df)) "panel_id" else if ("panelid" %in% names(df)) "panelid" else NA_character_
    if (is.na(id_col)) { showNotification("CSV is missing 'pack_id' (legacy accepted: packid/panel_id).", type="error"); return(NULL) }
    aout <- if ("actual_output" %in% names(df)) "actual_output" else if ("actualoutput" %in% names(df)) "actualoutput" else NA_character_
    if (is.na(aout)) { showNotification("CSV is missing 'actual_output' (or 'actualoutput').", type="error"); return(NULL) }
    out <- data.frame(
      pack_id = as.character(df[[id_col]]),
      actual_output = suppressWarnings(as.numeric(df[[aout]])),
      stringsAsFactors = FALSE
    )
    out$actual_output[!is.finite(out$actual_output)] <- NA
    out
  })
  
  # Fault datasets (optional uploads or default files)
  fault_tbl <- reactive({
    if (isTRUE(input$use_custom_fault_ds) && !is.null(input$fault_tbl_csv)) {
      return(tryCatch(read_fault_table(input$fault_tbl_csv$datapath),
                      error = function(e){ showNotification(paste("Failed to read reliability table:", e$message), type="error"); NULL }))
    }
    # try default file
    if (file.exists("pv_fault_reliability_table_days.csv")) {
      return(tryCatch(read_fault_table("pv_fault_reliability_table_days.csv"),
                      error = function(e) NULL))
    }
    NULL
  })
  
  install_tbl <- reactive({
    if (isTRUE(input$use_custom_fault_ds) && !is.null(input$install_tbl_csv)) {
      return(tryCatch(read_install_table(input$install_tbl_csv$datapath),
                      error = function(e){ showNotification(paste("Failed to read installation table:", e$message), type="error"); NULL }))
    }
    # try default file
    if (file.exists("solar_panel_installations_700x_seq100.csv")) {
      return(tryCatch(read_install_table("solar_panel_installations_700x_seq100.csv"),
                      error = function(e) NULL))
    }
    NULL
  })
  
  # Cpk-based LCL
  calc_cpk_lcl <- function(ratios, cpk){
    ratios <- ratios[is.finite(ratios)]
    if (length(ratios) < 2) return(list(lcl=NA_real_, mu=NA_real_, sigma=NA_real_))
    mu <- mean(ratios); sg <- sd(ratios)
    list(lcl=max(0, mu - 3 * sg * cpk), mu=mu, sigma=sg)
  }
  
  # Main compute
  results <- eventReactive(input$run_btn, {
    df <- act_df(); req(df)
    weather <- input$weather_choice %||% "Sunny"
    exp_val <- EXPECTED_BY_WEATHER[[weather]]
    ratio <- df$actual_output / exp_val
    ratio[!is.finite(ratio)] <- NA
    
    cpk <- suppressWarnings(as.numeric(input$cpk_target %||% 1.33)); if (!is.finite(cpk) || cpk < 0) cpk <- 1.33
    base <- input$cpk_base %||% "hist"
    cpk_info <- if (base == "hist") {
      hist_ratios <- if (!is.null(rv$history)) rv$history$ratio else numeric(0)
      calc_cpk_lcl(hist_ratios, cpk)
    } else {
      calc_cpk_lcl(ratio, cpk)
    }
    
    use_cpk <- is.list(cpk_info) && is.finite(cpk_info$lcl)
    thr <- if (use_cpk) cpk_info$lcl else FALLBACK_THRESHOLD
    thr_src <- if (use_cpk) sprintf("Cpk LCL (μ=%.3f, σ=%.3f, Cpk=%.2f)", cpk_info$mu, cpk_info$sigma, cpk) else "Fallback: fixed 85%"
    
    alert <- as.integer(ratio < thr); alert[is.na(alert)] <- 0L
    
    out <- data.frame(
      date = as.character(input$sys_date %||% Sys.Date()),
      weather = weather,
      expected_performance = exp_val,
      pack_id = df$pack_id,
      actual_output = round(df$actual_output, 4),
      ratio = round(ratio, 4),
      alert = alert,
      stringsAsFactors = FALSE
    )
    
    rv$history <- rbind(rv$history, transform(out, time = Sys.time())[ , c("time","weather","expected_performance","pack_id","actual_output","ratio")])
    attr(out, "threshold") <- thr
    attr(out, "threshold_src") <- thr_src
    out
  }, ignoreInit = TRUE)
  
  # Filtering (for ranking & histogram)
  filtered_res <- reactive({
    res <- results(); if (is.null(res)) return(NULL)
    thr <- attr(res, "threshold") %||% FALLBACK_THRESHOLD
    band <- input$sev_band %||% "All"
    if (band != "All") {
      if (band == "<0.70")      res <- subset(res, ratio < 0.70)
      if (band == "0.70–0.85")  res <- subset(res, ratio >= 0.70 & ratio < 0.85)
      if (band == "0.85–1.00")  res <- subset(res, ratio >= 0.85 & ratio <= 1.00)
      if (band == ">1.00")      res <- subset(res, ratio > 1.00)
    }
    rng <- input$sev_range %||% c(0, 1.5)
    res <- subset(res, is.finite(ratio) & ratio >= rng[1] & ratio <= rng[2])
    if (isTRUE(input$only_alerts)) res <- subset(res, ratio < thr)
    res
  })
  
  # KPIs
  output$kpi_ratio <- renderText({
    res <- results(); if (is.null(res)) return("—")
    r <- mean(res$ratio, na.rm=TRUE); if (!is.finite(r)) "—" else percent(r, 0.1)
  })
  output$kpi_alert <- renderText({
    res <- results(); if (is.null(res)) return("0")
    thr <- attr(res, "threshold") %||% FALLBACK_THRESHOLD
    sum(is.finite(res$ratio) & res$ratio < thr)
  })
  output$kpi_expected <- renderText({
    res <- results(); if (is.null(res)) return("—")
    s <- sum(res$expected_performance, na.rm=TRUE); if (!is.finite(s)) "—" else format(round(s,1), big.mark=",")
  })
  output$kpi_actual <- renderText({
    res <- results(); if (is.null(res)) return("—")
    s <- sum(res$actual_output, na.rm=TRUE); if (!is.finite(s)) "—" else format(round(s,1), big.mark=",")
  })
  output$kpi_thresh <- renderText({
    res <- results(); if (is.null(res)) return("—")
    thr <- attr(res, "threshold") %||% FALLBACK_THRESHOLD
    percent(thr, 0.1)
  })
  output$kpi_thresh_src <- renderText({
    res <- results(); if (is.null(res)) return("—")
    attr(res, "threshold_src") %||% "—"
  })
  
  # Ranking + clickable arrow
  output$ranking_list <- renderUI({
    res <- filtered_res(); if (is.null(res) || nrow(res) == 0) return(NULL)
    thr <- attr(results(), "threshold") %||% FALLBACK_THRESHOLD
    dec <- identical(input$rank_order, "desc")
    res <- res[order(res$ratio, decreasing=dec, na.last=TRUE), , drop=FALSE]
    topn <- head(res, 20)
    
    lapply(seq_len(nrow(topn)), function(i){
      row <- topn[i, ]
      band <- ifelse(!is.na(row$ratio) && row$ratio < 0.70, "High",
                     ifelse(!is.na(row$ratio) && row$ratio < thr, "Medium", "Low"))
      cls <- if (band == "High") "rank-item rank-high" else if (band == "Medium") "rank-item rank-med" else "rank-item rank-low"
      sid <- sanitize_id(row$pack_id)
      link_id <- paste0("view_", sid)
      
      tags$div(class=cls,
               tags$div(
                 tags$div(style="font-weight:700;", row$pack_id),
                 tags$div(style="opacity:0.85;font-size:0.9rem;",
                          sprintf("Ratio: %s | Actual: %s | Expected: %s | %s",
                                  ifelse(is.na(row$ratio), "NA", percent(row$ratio, 0.1)),
                                  round(row$actual_output, 1),
                                  round(row$expected_performance, 1),
                                  ifelse(row$ratio < thr, "Alert", "OK")))
               ),
               actionLink(link_id, label = HTML('&rsaquo;'), class = "rank-arrow")
      )
    })
  })
  
  # Bind click observers for fault modal
  observe({
    res <- filtered_res(); if (is.null(res) || nrow(res) == 0) return()
    ids <- unique(res$pack_id)
    new_ids <- setdiff(ids, rv$bound_ids)
    if (length(new_ids) == 0) return()
    
    for (pid in new_ids) {
      sid <- sanitize_id(pid)
      link_id <- paste0("view_", sid)
      
      local({
        pack <- pid
        lid  <- link_id
        observeEvent(input[[lid]], ignoreInit = TRUE, {
          cur <- filtered_res()
          row <- cur[cur$pack_id == pack, , drop = FALSE]
          if (nrow(row) == 0) return(NULL)
          row <- row[1, , drop = FALSE]
          
          # Try Weibull model (if both tables available)
          hyp_df <- NULL
          ft <- fault_tbl()
          it <- install_tbl()
          if (!is.null(ft) && !is.null(it)) {
            it_row <- it %>% filter(pack_id == pack)
            det_date <- as.Date(input$sys_date %||% Sys.Date())
            if (nrow(it_row) > 0 && is.finite(it_row$installation_end_date[1])) {
              t_days <- as.numeric(det_date - it_row$installation_end_date[1])
              if (is.finite(t_days) && t_days > 0) {
                ranked <- tryCatch(
                  rank_faults_by_time(ft, t_days = t_days, use_prior = isTRUE(input$use_prior)),
                  error = function(e) NULL
                )
                if (!is.null(ranked)) {
                  hyp_df <- ranked %>%
                    slice_head(n = 3) %>%
                    transmute(
                      fault,
                      confidence = paste0(sprintf("%.1f", 100*rel_prob), "%"),
                      recommendation = ifelse(in_95pct_window,
                                              "Within 95% life window — inspect immediately.",
                                              "Outside 95% life window — verify other causes.")
                    )
                }
              }
            }
          }
          
          # Fallback to rules
          if (is.null(hyp_df) || nrow(hyp_df) == 0) {
            rh <- fault_predict_rules(row$ratio, row$weather)
            hyp_df <- do.call(rbind, lapply(rh, function(h){
              data.frame(fault = h$name, confidence = h$conf, recommendation = h$action, stringsAsFactors = FALSE)
            }))
          }
          
          thr  <- attr(results(), "threshold") %||% FALLBACK_THRESHOLD
          
          showModal(modalDialog(
            title = sprintf("Pack %s — Possible Fault Prediction", pack),
            easyClose = TRUE,
            footer = modalButton("Close"),
            size = "l",
            tagList(
              tags$div(style="margin-bottom:8px; opacity:0.8;",
                       sprintf("Weather: %s | Ratio: %s | Actual: %s kW | Expected: %s kW | Threshold: %s",
                               row$weather,
                               ifelse(is.na(row$ratio),"NA", percent(row$ratio, 0.1)),
                               format(round(row$actual_output,1), big.mark=","),
                               format(round(row$expected_performance,1), big.mark=","),
                               percent(thr, 0.1))),
              if (!is.null(ft) && !is.null(it)) {
                it_row <- it %>% filter(pack_id == pack)
                if (nrow(it_row) > 0) {
                  det_date <- as.Date(input$sys_date %||% Sys.Date())
                  t_days <- as.numeric(det_date - it_row$installation_end_date[1])
                  tags$div(style="margin-bottom:6px; opacity:0.8;",
                           sprintf("Installation: %s | Detection: %s | Elapsed: %s days",
                                   as.character(it_row$installation_end_date[1]),
                                   as.character(det_date),
                                   ifelse(is.finite(t_days), format(round(t_days,0), big.mark=","), "NA")))
                }
              },
              tags$hr(style="border-color:#3a2b12;"),
              tags$h5("Predicted causes"),
              tags$ul(
                lapply(seq_len(nrow(hyp_df)), function(i){
                  h <- hyp_df[i,]
                  tags$li(
                    tags$b(h$fault),
                    sprintf(" — Confidence: %s. ", h$confidence),
                    tags$span(style="opacity:0.85;", h$recommendation)
                  )
                })
              )
            )
          ))
        })
      })
    }
    rv$bound_ids <- union(rv$bound_ids, new_ids)
  })
  
  # Histogram
  output$ratio_hist <- renderPlot({
    res_all <- results(); if (is.null(res_all)) return()
    res <- filtered_res(); if (is.null(res) || nrow(res) == 0) return()
    thr <- attr(res_all, "threshold") %||% FALLBACK_THRESHOLD
    r <- res$ratio; r <- r[is.finite(r)]
    if (length(r) == 0) return()
    par(bg="#221a0f", col.axis="#f1e7d2", col.lab="#f1e7d2", col.main="#f1e7d2")
    hist(r, breaks=20, main="Ratio Distribution (Filtered)", xlab="Actual / Expected",
         col="#f2a900", border="#3a2b12", xaxt="n")
    ax <- axTicks(1); axis(1, at=ax, labels=percent(ax, 1))
    abline(v=thr, lwd=2, col="#b81e1e")
  })
  
  # Preview & Download
  output$preview <- renderTable({
    res <- results(); if (is.null(res)) return(NULL)
    head(res, 12)
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() paste0("solar_ratio_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file){
      res <- results()
      out <- res
      names(out)[names(out) == "pack_id"] <- "Pack_ID"
      write.csv(out, file, row.names=FALSE, fileEncoding="UTF-8")
    }
  )
}

shinyApp(ui, server)
