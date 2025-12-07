# app.R
# Bayesian Interim Monitoring Dashboard for Phase II Clinical Trials
# Author: Jean Mendes de Lucena Vieira
# Designed for GitHub portfolio — reproducible, auditable, scalable

# setup.R — Instala todas as dependências necessárias
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "dplyr", "ggplot2",
  "plotly", "rmarkdown", "officer", "bslib", "extraDistr", "DT"
)

# Instalar pacotes faltantes
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

# Atualizar pacotes críticos
update.packages(oldPkgs = c("xfun", "xml2"), ask = FALSE)

message("✅ All dependencies installed/updated.")

# ── Load libraries ───────────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(officer)
library(bslib)
library(extraDistr)

# 1. Create the 'data' directory if it doesn't exist
dir.create("data", showWarnings = FALSE, recursive = TRUE)

# 2. Simulate trial data
set.seed(2025)
n_patients <- 60
true_response_rate <- 0.45  # true but unknown

sim_data <- tibble::tibble(
  patient_id = 1:n_patients,
  enrollment_date = seq.Date(Sys.Date() - 59, Sys.Date(), by = "day"),
  response = rbinom(n_patients, 1, true_response_rate)  # 1 = success, 0 = failure
)

# 3. Save safely
write.csv(sim_data, "data/simulated_trial_data.csv", row.names = FALSE)

# ── UI ───────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "🔬 Bayesian Trial Monitor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Settings", tabName = "settings", icon = icon("sliders")),
      menuItem("Export Report", tabName = "report", icon = icon("file-pdf"))
    ),
    
    # Prior & decision rule settings
    conditionalPanel(
      condition = "input.tabs == 'settings'",
      h4("📊 Prior Specification (Beta)"),
      numericInput("alpha_prior", "α (prior successes)", value = 2, min = 0.1, step = 0.5),
      numericInput("beta_prior", "β (prior failures)", value = 8, min = 0.1, step = 0.5),
      hr(),
      h4("🎯 Decision Rules (Posterior Probability)"),
      sliderInput("efficacy_thresh", "Efficacy Threshold", min = 0.8, max = 0.99, value = 0.95, step = 0.01),
      sliderInput("futility_thresh", "Futility Threshold", min = 0.01, max = 0.3, value = 0.10, step = 0.01),
      helpText("Stop for efficacy if P(θ > 0.3 | data) > efficacy_thresh"),
      helpText("Stop for futility if P(θ > 0.3 | data) < futility_thresh"),
      hr(),
      h4("👥 Enrollment Progress"),
      sliderInput("n_patients_slider", "Patients Analyzed", 
                  min = 10, max = nrow(sim_data), value = 20, step = 1)
    ),
    
    # Report export
    conditionalPanel(
      condition = "input.tabs == 'report'",
      h4("📥 Generate Interim Report"),
      dateInput("report_date", "Report Date", value = Sys.Date()),
      actionButton("generate_report", "Generate PDF Report", 
                   icon = icon("file-export"), 
                   class = "btn-success"),
      uiOutput("report_link")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        
        # Top row: value boxes + confidence gauge (no controls)
        fluidRow(
          valueBoxOutput("n_enrolled", width = 3),
          valueBoxOutput("n_success", width = 3),
          valueBoxOutput("posterior_mean", width = 3),
          box(
            title = "📊 Decision Confidence",
            status = "success", solidHeader = TRUE,
            plotlyOutput("confidence_gauge", height = 100),
            width = 3
          )
        ),
        
        # Summary panels
        fluidRow(
          box(
            title = "📊 Trial Summary",
            status = "info", solidHeader = TRUE, collapsible = TRUE,
            textOutput("trial_summary"),
            width = 4
          ),
          box(
            title = "🚀 Next Steps",
            status = "warning", solidHeader = TRUE, collapsible = TRUE,
            textOutput("next_steps"),
            width = 4
          ),
          box(
            title = "📜 Regulatory Note",
            status = "primary", solidHeader = TRUE, collapsible = TRUE,
            p("This dashboard follows ICH E9(R1) principles for adaptive designs.",
              br(), "All decisions are based on Bayesian posterior probabilities with predefined thresholds."),
            width = 4
          )
        ),
        
        # Plots
        fluidRow(
          box(
            title = "Posterior & Predictive Distributions", 
            status = "primary", solidHeader = TRUE, collapsible = TRUE,
            plotlyOutput("posterior_plot", height = 350),
            width = 6
          ),
          box(
            title = "Accrual & Responses Over Time", 
            status = "info", solidHeader = TRUE, collapsible = TRUE,
            plotlyOutput("accrual_plot", height = 350),
            width = 6
          )
        ),
        
        # All Patients Table (clean, no DT)
        fluidRow(
          box(
            title = "All Patients Enrolled",
            status = "warning", solidHeader = TRUE,
            uiOutput("recent_patients_table"),
            width = 12
          )
        ),
        
        # Data quality
        fluidRow(
          box(
            title = "🔍 Data Quality Check",
            status = "warning", solidHeader = TRUE,
            textOutput("data_quality"),
            width = 12
          )
        )
      )
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Reactive: current enrollment cutoff
  current_n <- reactive({
    input$n_patients_slider
  })
  
  # Compute posterior & decisions
  analysis_results <- reactive({
    n_obs <- current_n()
    data_obs <- sim_data %>% head(n_obs)
    
    # Sufficient stats
    y <- sum(data_obs$response)
    n <- nrow(data_obs)
    
    # Posterior: Beta(alpha + y, beta + n - y)
    alpha_post <- input$alpha_prior + y
    beta_post  <- input$beta_prior  + n - y
    
    # Posterior mean & 95% CI
    post_mean <- alpha_post / (alpha_post + beta_post)
    post_ci   <- qbeta(c(0.025, 0.975), alpha_post, beta_post)
    
    # P(θ > 0.3 | data)
    prob_above_30 <- 1 - pbeta(0.3, alpha_post, beta_post)
    
    # Decision
    decision <- ifelse(
      prob_above_30 > input$efficacy_thresh, "✅ STOP: Efficacy",
      ifelse(
        prob_above_30 < input$futility_thresh, "🛑 STOP: Futility",
        "▶️ CONTINUE"
      )
    )
    
    list(
      n = n, y = y,
      alpha_post = alpha_post, beta_post = beta_post,
      post_mean = post_mean,
      post_ci = post_ci,
      prob_above_30 = prob_above_30,
      decision = decision,
      data_obs = data_obs
    )
  })
  
  # ── Value Boxes ──
  output$n_enrolled <- renderValueBox({
    valueBox(
      value = analysis_results()$n,
      subtitle = "Patients Enrolled",
      icon = icon("user-md"),
      color = "light-blue"
    )
  })
  
  output$n_success <- renderValueBox({
    valueBox(
      value = analysis_results()$y,
      subtitle = "Responses (Successes)",
      icon = icon("heart"),
      color = "green"
    )
  })
  
  output$posterior_mean <- renderValueBox({
    val <- round(analysis_results()$post_mean, 3)
    valueBox(
      value = val,
      subtitle = "Posterior Mean Response Rate",
      icon = icon("chart-bar"),
      color = "purple"
    )
  })
  
  # ── Interim Decision (clickable button + modal) ──
  output$decision <- renderUI({
    res <- analysis_results()
    color_class <- if (grepl("Efficacy", res$decision)) "btn-success" else 
      if (grepl("Futility", res$decision)) "btn-danger" else "btn-warning"
    
    actionButton("show_decision_modal", 
                 label = res$decision,
                 icon = if (grepl("Efficacy", res$decision)) icon("check-circle") else
                   if (grepl("Futility", res$decision)) icon("times-circle") else icon("play-circle"),
                 class = paste("btn btn-lg", color_class),
                 width = "100%")
  })
  
  # Modal for decision details
  observeEvent(input$show_decision_modal, {
    res <- analysis_results()
    showModal(modalDialog(
      title = div(icon("chart-line"), "📊 Interim Decision Details"),
      HTML(paste0(
        "<strong>Decision:</strong> ", res$decision, "<br><br>",
        "<strong>Patients Analyzed:</strong> ", res$n, "<br>",
        "<strong>Observed Successes:</strong> ", res$y, "<br>",
        "<strong>Posterior Mean Response Rate:</strong> ", round(res$post_mean, 3), "<br>",
        "<strong>P(θ > 0.3 | data):</strong> ", round(res$prob_above_30, 3), "<br>",
        "<strong>Efficacy Threshold:</strong> ", input$efficacy_thresh, "<br>",
        "<strong>Futility Threshold:</strong> ", input$futility_thresh, "<br>"
      )),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # ── Decision Confidence Gauge (NO PLOTLY CONTROLS) ──
  output$confidence_gauge <- renderPlotly({
    res <- analysis_results()
    prob <- res$prob_above_30
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = prob,
      domain = list(x = c(0, 1), y = c(0, 1)),
      gauge = list(
        axis = list(range = c(0, 1), tickwidth = 1, tickcolor = "darkgray"),
        bar = list(color = ifelse(prob > input$efficacy_thresh, "#28a745",
                                  ifelse(prob < input$futility_thresh, "#dc3545", "#ffc107"))),
        bgcolor = "#f8f9fa",
        steps = list(
          list(range = c(0, input$futility_thresh), color = "#f8d7da"),
          list(range = c(input$futility_thresh, input$efficacy_thresh), color = "#fff3cd"),
          list(range = c(input$efficacy_thresh, 1), color = "#d4edda")
        ),
        threshold = list(
          line = list(color = "red", width = 3),
          thickness = 0.8,
          value = 0.3
        )
      ),
      number = list(suffix = "", font = list(size = 20))
    ) %>%
      layout(
        margin = list(l = 10, r = 10, t = 30, b = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        annotations = list(
          text = "P(θ > 0.3 | data)",
          showarrow = FALSE,
          x = 0.5, y = 0.45,
          font = list(size = 12)
        )
      ) %>%
      config(displayModeBar = FALSE)  # ✅ REMOVIDOS os botões!
  })
  
  # ── Trial Summary Panel ──
  output$trial_summary <- renderText({
    res <- analysis_results()
    paste0(
      "• Patients analyzed: ", res$n, "\n",
      "• Observed responses: ", res$y, "\n",
      "• Posterior mean (θ): ", round(res$post_mean, 3), "\n",
      "• 95% CrI: [", round(res$post_ci[1], 3), ", ", round(res$post_ci[2], 3), "]\n",
      "• P(θ > 0.3 | data): ", round(res$prob_above_30, 3)
    )
  })
  
  # ── Next Steps Panel ──
  output$next_steps <- renderText({
    res <- analysis_results()
    if (grepl("Efficacy", res$decision)) {
      "✅ Submit to DSMB for early termination. Prepare briefing package for regulatory submission (e.g., FDA SPA)."
    } else if (grepl("Futility", res$decision)) {
      "🛑 Recommend termination. Notify sponsor and ethics committee. Archive data per GCP."
    } else {
      paste0("▶️ Continue enrollment. Next interim analysis recommended at n = ",
             min(res$n + 10, nrow(sim_data)), " patients.")
    }
  })
  
  # ── Posterior Plot (unchanged) ──
  output$posterior_plot <- renderPlotly({
    res <- analysis_results()
    
    theta_grid <- seq(0, 1, length.out = 500)
    prior_pdf  <- dbeta(theta_grid, input$alpha_prior, input$beta_prior)
    post_pdf   <- dbeta(theta_grid, res$alpha_post, res$beta_post)
    
    p1 <- tibble::tibble(theta = theta_grid, prior = prior_pdf, posterior = post_pdf) %>%
      tidyr::pivot_longer(c(prior, posterior), names_to = "dist", values_to = "density") %>%
      ggplot(aes(x = theta, y = density, color = dist, fill = dist)) +
      geom_line(linewidth = 1.1) +
      geom_area(alpha = 0.2) +
      geom_vline(xintercept = 0.3, linetype = "dashed", color = "gray40") +
      annotate("text", x = 0.32, y = max(prior_pdf)*0.9, label = "Go/No-Go (θ = 0.3)", 
               angle = 90, hjust = 0, size = 3, color = "gray40") +
      labs(title = "Prior vs. Posterior Response Rate (θ)",
           x = expression(theta),
           y = "Density",
           color = "", fill = "") +
      theme_minimal(base_size = 12) +
      scale_color_manual(values = c("prior" = "#6c757d", "posterior" = "#1F77B4")) +
      scale_fill_manual(values = c("prior" = "#6c757d40", "posterior" = "#1F77B440"))
    
    ggplotly(p1, tooltip = c("x", "y")) %>% 
      layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
  })
  
  # ── ✅ Accrual Plot (CORRIGIDO: legenda no canto superior direito) ──
  output$accrual_plot <- renderPlotly({
    res <- analysis_results()
    df_plot <- sim_data %>%
      mutate(
        cumulative_response = cumsum(response),
        cumulative_rate = cumulative_response / row_number()
      ) %>%
      mutate(flag = ifelse(row_number() <= res$n, "Observed", "Pending"))
    
    p2 <- df_plot %>%
      ggplot(aes(x = enrollment_date)) +
      geom_step(aes(y = cumulative_response, color = flag), linewidth = 1.1) +
      geom_hline(yintercept = res$y, linetype = "dotted", color = "red") +
      labs(
        title = "Cumulative Responses Over Time",
        x = "Enrollment Date",
        y = "Cumulative Successes",
        color = ""
      ) +
      theme_minimal() +
      scale_color_manual(values = c("Observed" = "#1F77B4", "Pending" = "#ccc")) +
      scale_x_date(date_labels = "%b %d") +
      theme(
        legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", color = "gray80"),
        legend.box.background = element_rect(fill = "white", color = "gray80"),
        plot.margin = margin(t = 10, r = 10, b = 30, l = 10)
      )
    
    ggplotly(p2, tooltip = c("x", "y")) %>%
      layout(
        legend = list(
          orientation = "v",
          x = 0.98,
          y = 0.98,
          bgcolor = "white",
          bordercolor = "gray80",
          borderwidth = 1
        ),
        margin = list(l = 60, r = 30, t = 30, b = 60)
      )
  })
  
  # ── All Patients Table (clean, all rows) ──
  output$recent_patients_table <- renderUI({
    res <- analysis_results()
    data_recent <- res$data_obs  # ✅ TODOS os pacientes analisados
    
    table_html <- tags$table(
      class = "table table-hover",
      style = "font-size: 15px; margin: 0; width: 100%;",
      tags$thead(
        tags$tr(
          tags$th("Patient", style = "text-align: center; width: 100px; font-weight: 600;"),
          tags$th("Date", style = "text-align: center; width: 120px; font-weight: 600;"),
          tags$th("Outcome", style = "text-align: center; font-weight: 600;")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(data_recent)), function(i) {
          row <- data_recent[i, ]
          outcome_icon <- if (row$response == 1) {
            tags$span(
              icon("check-circle", class = "text-success"),
              tags$span(" Success", style = "margin-left: 6px; font-weight: 500;"),
              style = "font-size: 16px;"
            )
          } else {
            tags$span(
              icon("times-circle", class = "text-danger"),
              tags$span(" Failure", style = "margin-left: 6px; font-weight: 500;"),
              style = "font-size: 16px;"
            )
          }
          
          tags$tr(
            class = if (i %% 2 == 0) "bg-light" else "",
            tags$td(as.character(row$patient_id), style = "text-align: center; vertical-align: middle;"),
            tags$td(as.character(row$enrollment_date), style = "text-align: center; vertical-align: middle;"),
            tags$td(outcome_icon, style = "text-align: center; vertical-align: middle;")
          )
        })
      )
    )
    
    tagList(
      tags$div(
        style = "padding: 15px; background-color: #ffffff; border-radius: 6px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
        table_html
      )
    )
  })
  
  # ── Data Quality Check ──
  output$data_quality <- renderText({
    res <- analysis_results()
    data_obs <- res$data_obs
    
    missing_dates <- sum(is.na(data_obs$enrollment_date))
    missing_responses <- sum(is.na(data_obs$response))
    
    paste0(
      "• Missing enrollment dates: ", missing_dates, "\n",
      "• Missing response outcomes: ", missing_responses, "\n",
      "• All rows complete? ", 
      ifelse(missing_dates == 0 && missing_responses == 0, "✅ Yes", "❌ No")
    )
  })
  
  # ── Report Generation ──
  observeEvent(input$generate_report, {
    res <- analysis_results()
    dir.create("report", showWarnings = FALSE)
    
    # Minimal Rmd
    rmd_content <- '
---
title: "Interim Monitoring Report — Bayesian Phase II Trial"
author: "Jean Mendes de Lucena Vieira, Biostatistician"
date: "`r params$report_date`"
output: pdf_document
params:
  report_date: !r Sys.Date()
  n_enrolled: 20
  n_success: 8
  post_mean: 0.35
  post_ci_low: 0.22
  post_ci_high: 0.51
  prob_above_30: 0.68
  decision: "▶️ CONTINUE"
  alpha_prior: 2
  beta_prior: 8
---

### Trial Summary  
- **Patients enrolled**: `r params$n_enrolled`  
- **Responses observed**: `r params$n_success`  
- **Posterior mean response rate**: `r params$post_mean` (95% CrI: `r params$post_ci_low`–`r params$post_ci_high`)  
- **P(θ > 0.30 | data)**: `r params$prob_above_30`  

### Decision  
**`r params$decision`**  
- Efficacy threshold: `r input$efficacy_thresh`  
- Futility threshold: `r input$futility_thresh`  

### Prior Specification  
Beta(α = `r params$alpha_prior`, β = `r params$beta_prior`) — weakly informative, centered at 20% response.
'
    writeLines(rmd_content, "report/interim_report.Rmd")
    
    rmarkdown::render(
      "report/interim_report.Rmd",
      output_file = "Bayesian_Interim_Report.pdf",
      params = list(
        report_date = input$report_date,
        n_enrolled = res$n,
        n_success = res$y,
        post_mean = round(res$post_mean, 3),
        post_ci_low = round(res$post_ci[1], 3),
        post_ci_high = round(res$post_ci[2], 3),
        prob_above_30 = round(res$prob_above_30, 3),
        decision = res$decision,
        alpha_prior = input$alpha_prior,
        beta_prior = input$beta_prior
      ),
      envir = new.env(parent = globalenv())
    )
    
    output$report_link <- renderUI({
      tags$a(
        href = "Bayesian_Interim_Report.pdf",
        "⬇️ Download PDF Report",
        class = "btn btn-default",
        target = "_blank"
      )
    })
  })
}

# ── Run App ──────────────────────────────────────────────────────
shinyApp(ui, server)