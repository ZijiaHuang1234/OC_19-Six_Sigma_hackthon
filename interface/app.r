# Solar Quality Control System
# NYSERDA Hackathon 2025
# R Shiny Application

# ---- Packages ----
# install.packages(c("shiny", "bslib", "dplyr", "ggplot2"))
library(shiny)
library(bslib)
library(dplyr)

# ---- Mock Data ----
set.seed(42)

mk_items <- function(n, prefix) {
  tibble::tibble(
    id = sprintf("%s %05d", prefix, sample(10000:99999, n)),
    type = prefix,
    severity = sample(50:180, n, replace = TRUE)
  )
}

panels <- mk_items(40, "Panel")
inverters <- mk_items(20, "Inverter")

assets <- bind_rows(panels, inverters) %>%
  mutate(
    band = case_when(
      severity >= 120 ~ "High",
      severity >= 80  ~ "Medium",
      TRUE            ~ "Low"
    )
  )

# KPI values
kpi_avg_pr <- 92.5
kpi_avail <- 99.8
kpi_alert <- 12
kpi_sev_idx <- 3.7

# ---- Theme & CSS ----
dark <- bs_theme(
  version = 5,
  bg = "#120f0a",
  fg = "#f1e7d2",
  primary = "#f2a900",
  base_font = font_google("Inter")
)

custom_css <- HTML("
  .kpi-card { 
    background: #2c2212; 
    border-radius: 10px; 
    padding: 12px; 
  }
  .kpi-title { 
    font-size: 0.9rem; 
    opacity: 0.8; 
  }
  .kpi-value { 
    font-size: 2rem; 
    font-weight: 800; 
    margin-top: 4px; 
  }
  .section-title { 
    font-weight: 800; 
    font-size: 1.1rem; 
    margin: 20px 0 10px; 
  }
  .rank-item { 
    border-radius: 12px; 
    padding: 16px; 
    margin-bottom: 10px;
    display: flex; 
    justify-content: space-between; 
    align-items: center; 
    color: #f1e7d2; 
  }
  .rank-high { 
    background: linear-gradient(90deg, #4e0f0f, #351010); 
  }
  .rank-med { 
    background: linear-gradient(90deg, #3b2a14, #2f220f); 
  }
  .rank-low { 
    background: linear-gradient(90deg, #0e2c1e, #0b2318); 
  }
  .chip { 
    padding: 6px 10px; 
    border-radius: 16px; 
    margin-right: 8px; 
    cursor: pointer; 
    border: 1px solid #3a2b12; 
    background: #1d170d; 
    display: inline-block;
  }
")

# ---- UI ----
ui <- fluidPage(
  theme = dark,
  tags$head(tags$style(custom_css)),
  
  h3("☀️ Solar Quality Control System"),
  p("NYSERDA Hackathon 2025", style = "opacity: 0.7;"),
  
  fluidRow(
    column(3, 
           div(class = "kpi-card", 
               div(class = "kpi-title", "Average PR"),
               div(class = "kpi-value", paste0(kpi_avg_pr, "%")),
               div("+0.2%", style = "opacity: 0.7;")
           )
    ),
    column(3, 
           div(class = "kpi-card", 
               div(class = "kpi-title", "Availability"),
               div(class = "kpi-value", paste0(kpi_avail, "%")),
               div("-0.1%", style = "opacity: 0.7;")
           )
    ),
    column(3, 
           div(class = "kpi-card", 
               div(class = "kpi-title", "Panels in Alert"),
               div(class = "kpi-value", kpi_alert),
               div("+5%", style = "opacity: 0.7;")
           )
    ),
    column(3, 
           div(class = "kpi-card", 
               div(class = "kpi-title", "Severity Index"),
               div(class = "kpi-value", kpi_sev_idx),
               div("-2%", style = "opacity: 0.7;")
           )
    )
  ),
  
  div(class = "section-title", "Panel/Inverter Severity Ranking"),
  fluidRow(
    column(4,
           actionButton("all_btn", "All", class = "btn-warning"),
           actionButton("panel_btn", "Panels", class = "btn-secondary"),
           actionButton("inv_btn", "Inverters", class = "btn-secondary")
    ),
    column(8,
           div(style = "text-align: right;",
               span(class = "chip", "High"),
               span(class = "chip", "Medium"),
               span(class = "chip", "Low")
           )
    )
  ),
  uiOutput("ranking_list"),
  
  div(class = "section-title", "Reliability Analysis"),
  plotOutput("weibull_plot", height = "280px")
)

# ---- Server ----
server <- function(input, output, session) {
  
  current_type <- reactiveVal("All")
  
  observeEvent(input$all_btn, { current_type("All") })
  observeEvent(input$panel_btn, { current_type("Panel") })
  observeEvent(input$inv_btn, { current_type("Inverter") })
  
  filtered <- reactive({
    dat <- assets
    if (current_type() != "All") {
      dat <- filter(dat, type == current_type())
    }
    dat <- arrange(dat, desc(severity))
    dat
  })
  
  output$ranking_list <- renderUI({
    dat <- filtered()
    lapply(seq_len(nrow(dat)), function(i) {
      row <- dat[i, ]
      cls <- switch(row$band,
                    "High" = "rank-item rank-high",
                    "Medium" = "rank-item rank-med",
                    "Low" = "rank-item rank-low")
      div(class = cls,
          div(
            div(style = "font-weight: 700;", row$id),
            div(style = "opacity: 0.8; font-size: 0.9rem;", 
                paste("Severity:", row$severity))
          ),
          "›"
      )
    })
  })
  
  output$weibull_plot <- renderPlot({
    dat <- filtered()
    shape <- 2.2
    scale <- max(80, mean(dat$severity))
    t <- seq(0, 5, length.out = 300)
    f <- (shape/scale) * (t/scale)^(shape-1) * exp(-(t/scale)^shape)
    
    par(bg = "#221a0f", 
        col.axis = "#f1e7d2", 
        col.lab = "#f1e7d2", 
        col.main = "#f1e7d2")
    plot(t, f, 
         type = "l", 
         lwd = 2, 
         col = "#f2a900",
         xlab = "Normalized Time", 
         ylab = "Failure Density",
         main = "Weibull Reliability Curve")
  })
}

# ---- Run App ----
shinyApp(ui, server)