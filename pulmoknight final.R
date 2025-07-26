if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinyWidgets, ggplot2, dplyr, ggrepel, base64enc, scales)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .risk-high { background-color: #ffebee; border-left: 5px solid #e53935; padding: 15px; margin: 15px 0; border-radius: 5px; }
      .risk-moderate { background-color: #fff8e1; border-left: 5px solid #ffb300; padding: 15px; margin: 15px 0; border-radius: 5px; }
      .risk-low { background-color: #e8f5e9; border-left: 5px solid #43a047; padding: 15px; margin: 15px 0; border-radius: 5px; }
      .gauge-container { background: white; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); margin-bottom: 20px; }
      .clinical-recommendation { background: #e3f2fd; padding: 15px; border-radius: 5px; margin-top: 20px; }
      .plot-title { text-align: center; font-weight: bold; margin-bottom: 10px; }
      .disclaimer-box { background: #fff3e0; border-left: 4px solid #ffa000; padding: 10px; margin-top: 20px; font-size: 0.9em; }
      .references-box { background: #f5f5f5; border-left: 4px solid #607d8b; padding: 15px; margin-top: 20px; font-size: 0.85em; }
    "))
  ),

  titlePanel(div("PulmoKnight: Lung Cancer Risk Assessment for Indians",
                 style = "color: #1a237e; font-weight: bold")),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      h4("Demographic Parameters", style = "color: #0d47a1;"),

      pickerInput(
        "state", "State of Residence",
        choices = c("Kerala", "Maharashtra", "Tamil Nadu", "West Bengal",
                    "Delhi", "Gujarat", "Punjab", "Karnataka", "Other"),
        selected = "Maharashtra"
      ),

      numericInput("biomass_years", "Biomass Fuel Exposure (years)",
                   value = 0, min = 0, max = 80),
      helpText("Years of exposure to cooking with wood/dung/agricultural waste"),

      pickerInput(
        "occupation", "Primary Occupation",
        choices = c("Farmer/Agricultural Worker",
                    "Construction Worker",
                    "Textile Worker",
                    "Miner/Quarry Worker",
                    "Industrial Worker",
                    "Office Worker",
                    "Household Work",
                    "Other"),
        selected = "Office Worker"
      ),

      sliderInput("diet", "Daily Fruit/Vegetable Intake",
                  min = 0, max = 5, value = 2, post = " servings/day"),
      helpText("1 serving = 80g (WHO recommendation: ≥5 servings)"),

      h4("Clinical Parameters", style = "color: #0d47a1; margin-top: 20px;"),

      # NEW: Biological sex (gender) parameter
      awesomeRadio("sex", "Biological Sex",
                   choices = c("Male", "Female"),
                   selected = "Male"),

      numericInput("age", "Age (years)", value = 55, min = 30, max = 90),
      sliderInput("smoking", "Tobacco Exposure (pack-years)",
                  min = 0, max = 100, value = 20, post = " packs"),
      helpText("Pack-years = number of packs smoked per day × number of years smoked"),
      numericInput("quit_years", "Years since quitting smoking (0 if current smoker)",
                   value = 0, min = 0, max = 60),

      awesomeRadio("tobacco_type", "Primary Tobacco Use",
                   choices = c("Cigarettes", "Bidis", "Both", "Other"),
                   selected = "Bidis"),

      awesomeRadio("pollution", "Environmental Exposure",
                   choices = c("Rural", "Urban (Non-industrial)", "Urban (Industrial)", "Occupational Hazard"),
                   selected = "Urban (Non-industrial)"),
      helpText("Occupational hazards include prolonged exposure to asbestos, silica dust, diesel exhaust, or other industrial carcinogens"),

      pickerInput("symptoms", "Symptoms Present",
                  choices = c("Persistent cough (>3 weeks)",
                              "Blood in sputum",
                              "Unexplained weight loss (>5kg in 3 months)",
                              "Chest pain",
                              "Recurrent respiratory infections"),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)),

      switchInput("family_history", "Family History of Lung Cancer", value = FALSE),
      switchInput("tuberculosis", "History of Tuberculosis or COPD", value = FALSE),

      actionBttn("predict", "Calculate Risk", style = "gradient",
                 color = "primary", size = "md"),

      downloadButton("download_report", "Download Report",
                     style = "width: 100%; margin-top: 20px; background-color: #4caf50; color: white;")
    ),

    mainPanel(
      div(class = "gauge-container",
          h4(class = "plot-title", "Risk Probability Gauge"),
          plotOutput("risk_gauge", height = "250px"),
          htmlOutput("risk_label")
      ),

      div(class = "gauge-container",
          h4(class = "plot-title", "Risk Factor Breakdown"),
          plotOutput("risk_breakdown", height = "300px")
      ),

      uiOutput("clinical_result"),

      div(class = "clinical-recommendation",
          h4(icon("book-medical"), "NCCN-Aligned Recommendations:"),
          htmlOutput("indian_guidance"),
          div(class = "disclaimer-box",
              h4(icon("exclamation-triangle"), "Clinical Use Guidelines"),
              tags$ul(
                tags$li("Intended for physician-supervised risk assessment, not standalone diagnosis"),
                tags$li("High-risk thresholds calibrated for Indian population (ages 50-80)"),
                tags$li("Symptoms always supersede risk score - clinically evaluate all symptomatic patients")
              )
          )
      ),

      div(class = "references-box",
          h4(icon("book"), "Scientific References:"),
          tags$ul(
            tags$li("Noronha V, et al. (2020). Lung Cancer in India. Journal of Thoracic Oncology."),
            tags$li("Dikshit R, et al. (2012). Cancer mortality in India. The Lancet."),
            tags$li("NCCN Guidelines for Lung Cancer Screening v2.2025"),
            tags$li("India State-Level Disease Burden Initiative Cancer Collaborators (2018). JAMA Oncology."),
            tags$li("Sarkar K, et al. (2021). Biomass fuel and lung cancer risk in Indian women. Environmental Research.")
          )
      ),

      div(class = "clinical-recommendation",
          h4(icon("envelope"), "Contact Us:"),
          tags$ul(
            tags$li("Dishari Ghosh: ",
                    tags$a(href = "mailto:ghoshdisha17@gmail.com", "ghoshdisha17@gmail.com")),
            tags$li("Aditya Banerjee: ",
                    tags$a(href = "mailto:adityapuku@gmail.com", "adityapuku@gmail.com"))
          )
      )
    )
  )
)

server <- function(input, output, session) {

  calculate_risk <- reactive({
    req(input$age, input$smoking, input$quit_years)

    # Enhanced Indian risk model (PLCOm2012-inspired)
    base_risk <- dplyr::case_when(
      input$age >= 50 & input$age <= 80 &
        input$smoking >= 20 &
        input$quit_years <= 15 ~ 0.25,
      TRUE ~ 0.05
    )

    # Tobacco risk (bidis = 1.5x cigarettes)
    tobacco_multiplier <- dplyr::case_when(
      input$tobacco_type == "Bidis" ~ 1.5,
      input$tobacco_type == "Both" ~ 1.8,
      TRUE ~ 1.0
    )
    tobacco_risk <- (input$smoking * 0.008) * tobacco_multiplier

    # Environment
    env_risk <- dplyr::case_when(
      input$pollution == "Rural" ~ 0.015,  # Increased baseline for rural due to biomass
      input$pollution == "Urban (Non-industrial)" ~ 0.015,
      input$pollution == "Urban (Industrial)" ~ 0.03,
      input$pollution == "Occupational Hazard" ~ 0.05,
      TRUE ~ 0
    )

    # New risk factors
    biomass_risk <- min(0.08, input$biomass_years * 0.002)  # ~4% risk for 20 years exposure

    occupation_risk <- dplyr::case_when(
      input$occupation %in% c("Miner/Quarry Worker", "Construction Worker") ~ 0.04,
      input$occupation == "Industrial Worker" ~ 0.03,
      input$occupation == "Textile Worker" ~ 0.02,
      TRUE ~ 0
    )

    # FIXED: low diet intake should INCREASE risk
    diet_penalty <- pmax(0, (5 - input$diet) * 0.01)

    # Symptoms
    symptoms_risk <- dplyr::case_when(
      "Blood in sputum" %in% input$symptoms ~ 0.10,
      length(input$symptoms) >= 3 ~ 0.07,
      length(input$symptoms) >= 1 ~ 0.03,
      TRUE ~ 0
    )

    # Medical history
    medical_risk <- ifelse(input$tuberculosis, 0.04, 0) +
      ifelse(input$family_history, 0.03, 0)

    # Total risk before sex adjustment
    total_risk_raw <- base_risk + tobacco_risk + env_risk + biomass_risk +
      occupation_risk + diet_penalty + symptoms_risk + medical_risk

    # NEW: simple sex multiplier (Female lower baseline)
    sex_multiplier <- ifelse(input$sex == "Female", 0.8, 1.0)

    # Total risk (capped at 95%)
    total_risk <- min(0.95, total_risk_raw * sex_multiplier)

    # Risk thresholds (10%/30%)
    risk_level <- dplyr::case_when(
      total_risk >= 0.3 ~ "High Risk",
      total_risk >= 0.1 ~ "Moderate Risk",
      TRUE ~ "Low Risk"
    )

    list(
      probability = total_risk,
      risk_level = risk_level,
      breakdown = data.frame(
        Factor = c("Base Risk", "Tobacco", "Environment", "Biomass",
                   "Occupation", "Diet", "Symptoms", "Medical History"),
        Contribution = c(base_risk, tobacco_risk, env_risk, biomass_risk,
                         occupation_risk, diet_penalty, symptoms_risk, medical_risk)
      )
    )
  })

  output$risk_gauge <- renderPlot({
    res <- calculate_risk()
    ggplot() +
      geom_col(aes(x = 1, y = 1), fill = "#f5f5f5", width = 0.6) +
      geom_col(aes(x = 1, y = res$probability),
               fill = dplyr::case_when(
                 res$risk_level == "High Risk" ~ "#e53935",
                 res$risk_level == "Moderate Risk" ~ "#ffb300",
                 TRUE ~ "#43a047"
               ),
               width = 0.6) +
      coord_polar(theta = "y", start = -pi/2) +
      ylim(0, 1) +
      theme_void() +
      annotate("text", x = 0, y = 0,
               label = paste0(round(res$probability * 100, 1), "%"),
               size = 10, fontface = "bold")
  })

  output$risk_label <- renderUI({
    res <- calculate_risk()
    div(
      style = "text-align: center;",
      h3(
        style = paste0("color: ",
                       dplyr::case_when(
                         res$risk_level == "High Risk" ~ "#e53935",
                         res$risk_level == "Moderate Risk" ~ "#fb8c00",
                         TRUE ~ "#43a047"
                       ),
                       "; font-weight: bold;"),
        icon(dplyr::case_when(
          res$risk_level == "High Risk" ~ "exclamation-triangle",
          res$risk_level == "Moderate Risk" ~ "exclamation-circle",
          TRUE ~ "check-circle"
        )),
        " ",
        res$risk_level
      )
    )
  })

  output$risk_breakdown <- renderPlot({
    res <- calculate_risk()
    ggplot(res$breakdown, aes(x = reorder(Factor, Contribution), y = Contribution, fill = Factor)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = scales::percent(Contribution, accuracy = 0.1)),
                hjust = -0.1, size = 4.5) +
      coord_flip() +
      scale_fill_manual(values = c("#3949ab", "#7b1fa2", "#00897b", "#d84315",
                                   "#5d4037", "#1976d2", "#e53935", "#43a047")) +
      scale_y_continuous(labels = scales::percent,
                         limits = c(min(res$breakdown$Contribution) * 1.1,
                                    max(res$breakdown$Contribution) * 1.3)) +
      labs(x = NULL, y = "Contribution to Total Risk") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 13))
  })

  output$clinical_result <- renderUI({
    res <- calculate_risk()
    div(
      class = paste0("risk-", tolower(gsub(" ", "-", res$risk_level))),
      h3("Clinical Assessment Summary"),
      p(style = "font-size: 1.2em;",
        "Composite Risk Score: ",
        strong(style = "font-size: 1.3em;", scales::percent(res$probability, accuracy = 0.1))
      ),
      p(style = "font-size: 1.1em;",
        "Risk Category: ",
        strong(res$risk_level)
      )
    )
  })

  output$indian_guidance <- renderUI({
    res <- calculate_risk()
    tagList(
      if (res$risk_level == "High Risk") {
        HTML(paste0(
          "<ul>",
          "<li><strong>Low-dose CT scan recommended</strong></li>",
          "<li>Consult pulmonologist immediately</li>",
          "<li>Consider sputum cytology if CT unavailable</li>",
          "<li>State-specific guidance: ",
          if (input$state %in% c("Kerala", "Maharashtra"))
            "Available at tertiary cancer centers" else
              "Refer to nearest NCRP-affiliated center",
          "</li></ul>"
        ))
      } else if (res$risk_level == "Moderate Risk") {
        HTML(paste0(
          "<ul>",
          "<li>Annual chest X-ray recommended</li>",
          "<li>Clinical evaluation every 6-12 months</li>",
          "<li>Occupational health consult for ", input$occupation, " workers</li>",
          "</ul>"
        ))
      } else {
        HTML(paste0(
          "<ul>",
          "<li>General preventive health check-up</li>",
          "<li>Dietary counseling for cancer prevention (current: ",
          input$diet, "/5 servings)</li>",
          "</ul>"
        ))
      },
      p("Reference: ",
        tags$a(href = "https://www.nccn.org/patients/guidelines/content/PDF/lung_screening-patient.pdf",
               "NCCN Lung Cancer Screening Guidelines", target = "_blank"))
    )
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("PulmoKnight_Clinical_Report_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      res <- calculate_risk()

      # Create temp files for plots (higher DPI)
      temp_gauge <- tempfile(fileext = ".png")
      temp_breakdown <- tempfile(fileext = ".png")

      # Enhanced gauge plot
      gauge_plot <- ggplot() +
        geom_col(aes(x = 1, y = 1), fill = "#f5f5f5", width = 0.6) +
        geom_col(aes(x = 1, y = res$probability),
                 fill = dplyr::case_when(
                   res$risk_level == "High Risk" ~ "#e53935",
                   res$risk_level == "Moderate Risk" ~ "#ffb300",
                   TRUE ~ "#43a047"
                 ),
                 width = 0.6) +
        coord_polar(theta = "y", start = -pi/2) +
        ylim(0, 1) +
        theme_void() +
        annotate("text", x = 0, y = 0,
                 label = paste0(round(res$probability * 100, 1), "%"),
                 size = 12, fontface = "bold", color = "#333333")

      ggsave(temp_gauge, gauge_plot, width = 7, height = 7, dpi = 300, bg = "white")

      # Enhanced breakdown plot
      breakdown_plot <- ggplot(res$breakdown,
                               aes(x = reorder(Factor, Contribution), y = Contribution, fill = Factor)) +
        geom_col(width = 0.8, alpha = 0.9) +
        geom_text(aes(label = scales::percent(Contribution, accuracy = 0.1)),
                  hjust = -0.1, size = 5, fontface = "bold") +
        coord_flip() +
        scale_fill_manual(values = c("#3949ab", "#7b1fa2", "#00897b", "#d84315",
                                     "#5d4037", "#1976d2", "#e53935", "#43a047")) +
        scale_y_continuous(labels = scales::percent,
                           limits = c(min(res$breakdown$Contribution) * 1.2,
                                      max(res$breakdown$Contribution) * 1.4),
                           expand = expansion(mult = c(0, 0.1))) +
        labs(x = NULL, y = "Percentage Contribution to Risk") +
        theme_minimal(base_size = 14) +
        theme(panel.grid.major.y = element_blank(),
              axis.text = element_text(color = "#333333"),
              axis.title = element_text(face = "bold"))

      ggsave(temp_breakdown, breakdown_plot, width = 9, height = 6, dpi = 300, bg = "white")

      # Convert images to base64
      gauge_base64 <- base64enc::base64encode(temp_gauge)
      breakdown_base64 <- base64enc::base64encode(temp_breakdown)

      # Generate comprehensive report HTML
      html <- paste0(
        '<!DOCTYPE html>
        <html lang="en">
        <head>
          <meta charset="UTF-8">
          <meta name="viewport" content="width=device-width, initial-scale=1.0">
          <title>PulmoKnight Clinical Risk Report</title>
          <style>
            body {
              font-family: "Arial", sans-serif;
              line-height: 1.6;
              color: #333;
              max-width: 900px;
              margin: 0 auto;
              padding: 30px;
            }
            .header {
              border-bottom: 2px solid #1a237e;
              padding-bottom: 15px;
              margin-bottom: 25px;
            }
            .header h1 {
              color: #1a237e;
              margin-bottom: 5px;
            }
            .header .subtitle {
              color: #666;
              font-size: 1.1em;
            }
            .risk-level {
              font-size: 1.2em;
              font-weight: bold;
              padding: 12px;
              border-radius: 5px;
              margin: 20px 0;
              text-align: center;
            }
            .high-risk { background-color: #ffebee; color: #c62828; border-left: 5px solid #e53935; }
            .moderate-risk { background-color: #fff8e1; color: #e65100; border-left: 5px solid #ffb300; }
            .low-risk { background-color: #e8f5e9; color: #2e7d32; border-left: 5px solid #43a047; }
            .plot-container {
              text-align: center;
              margin: 25px 0;
              padding: 15px;
              background: white;
              border-radius: 8px;
              box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }
            .plot-container img {
              max-width: 100%;
              height: auto;
            }
            .plot-caption {
              font-style: italic;
              color: #666;
              text-align: center;
              margin-top: 8px;
            }
            .patient-details {
              background: #f5f5f5;
              padding: 15px;
              border-radius: 5px;
              margin: 20px 0;
            }
            .recommendations {
              background: #e3f2fd;
              padding: 20px;
              border-radius: 5px;
              margin: 25px 0;
            }
            .references {
              background: #f5f5f5;
              padding: 15px;
              border-radius: 5px;
              margin: 20px 0;
              font-size: 0.9em;
            }
            .footer {
              margin-top: 40px;
              padding-top: 20px;
              border-top: 1px solid #eee;
              font-size: 0.9em;
              color: #666;
            }
          </style>
        </head>
        <body>
          <div class="header">
            <h1>PulmoKnight Clinical Risk Assessment Report</h1>
            <div class="subtitle">Generated on: ', format(Sys.Date(), "%B %d, %Y"), '</div>
          </div>

          <div class="risk-level ', tolower(gsub(" ", "-", res$risk_level)), '">
            <div style="font-size: 1.5em;">', res$risk_level, ' Risk</div>
            <div style="font-size: 2em; margin: 5px 0;">', round(res$probability * 100, 1), '% Probability</div>
            <div>',
              if (res$risk_level == "High Risk") "Meets NCCN Criteria for LDCT Screening" else
                if (res$risk_level == "Moderate Risk") "Consider Shared Decision-Making for Screening" else
                  "Routine Monitoring Recommended",
            '</div>
          </div>

          <div class="patient-details">
            <h3>Patient Parameters</h3>
            <table style="width: 100%; border-collapse: collapse;">
              <tr>
                <td style="padding: 8px 0; width: 50%;"><strong>Demographics:</strong> ', input$age, 'yo from ', input$state, '</td>
                <td style="padding: 8px 0;"><strong>Occupation:</strong> ', input$occupation, '</td>
              </tr>
              <tr>
                <td style="padding: 8px 0;"><strong>Tobacco:</strong> ', input$smoking, ' pack-years (', input$tobacco_type, ')</td>
                <td style="padding: 8px 0;"><strong>Environment:</strong> ', input$pollution, '</td>
              </tr>
              <tr>
                <td style="padding: 8px 0;"><strong>Biomass Exposure:</strong> ', input$biomass_years, ' years</td>
                <td style="padding: 8px 0;"><strong>Diet:</strong> ', input$diet, '/5 daily servings</td>
              </tr>
              <tr>
                <td style="padding: 8px 0;"><strong>Symptoms:</strong> ',
                  if (length(input$symptoms) > 0) paste(input$symptoms, collapse = ", ") else "None", '</td>
                <td style="padding: 8px 0;"><strong>Medical History:</strong> ',
                  paste(c(ifelse(input$tuberculosis, "TB/COPD", ""),
                          ifelse(input$family_history, "Family History", "")) %>%
                          .[. != ""], collapse = ", "), '</td>
              </tr>
            </table>
          </div>

          <div class="plot-container">
            <h3>Risk Probability Visualization</h3>
            <img src="data:image/png;base64,', gauge_base64, '" alt="Risk Gauge">
            <div class="plot-caption">Figure 1: Visual representation of lung cancer risk probability</div>
          </div>

          <div class="plot-container">
            <h3>Risk Factor Contributions</h3>
            <img src="data:image/png;base64,', breakdown_base64, '" alt="Risk Breakdown">
            <div class="plot-caption">Figure 2: Relative contribution of each risk factor</div>
          </div>

          <div class="recommendations">
            <h3>Clinical Recommendations</h3>
            ', if (res$risk_level == "High Risk") {
              paste0('<ul>
                <li><strong>Immediate Actions:</strong>
                  <ul>
                    <li>Low-dose CT (LDCT) chest scan within 1 month</li>
                    <li>Referral to pulmonologist/thoracic oncology specialist</li>
                    <li>Formal smoking cessation program if applicable</li>
                  </ul>
                </li>
                <li><strong>Indian Context Considerations:</strong>
                  <ul>
                    <li>State-specific: ', input$state, ' cancer registry follow-up</li>
                    <li>Biomass exposure reduction strategies</li>
                  </ul>
                </li>
              </ul>')
            } else if (res$risk_level == "Moderate Risk") {
              paste0('<ul>
                <li><strong>Monitoring Protocol:</strong>
                  <ul>
                    <li>Annual chest X-ray with comparison to prior imaging</li>
                    <li>Clinical reevaluation in 6 months</li>
                    <li>Occupational health assessment for ', input$occupation, '</li>
                  </ul>
                </li>
                <li><strong>Preventive Measures:</strong>
                  <ul>
                    <li>Dietary improvement (current: ', input$diet, '/5 servings)</li>
                    <li>Biomass fuel alternatives if applicable</li>
                  </ul>
                </li>
              </ul>')
            } else {
              paste0('<ul>
                <li><strong>General Health Maintenance:</strong>
                  <ul>
                    <li>Annual health screening</li>
                    <li>Tobacco avoidance/cessation counseling</li>
                  </ul>
                </li>
                <li><strong>Environmental Risk Reduction:</strong>
                  <ul>
                    <li>Indoor air quality improvement (especially for ', input$biomass_years, ' years biomass exposure)</li>
                    <li>PPE use for occupational exposures</li>
                  </ul>
                </li>
              </ul>')
            }, '
            <p><strong>Reference:</strong> NCCN Clinical Practice Guidelines in Oncology: Lung Cancer Screening v2.2025</p>
          </div>

          <div class="references">
            <h4>Evidence Base:</h4>
            <ul>
              <li>Noronha V, et al. (2020). Lung Cancer in India. Journal of Thoracic Oncology</li>
              <li>Dikshit R, et al. (2012). Cancer mortality in India. The Lancet</li>
              <li>Sarkar K, et al. (2021). Biomass fuel and lung cancer risk in Indian women</li>
              <li>India State-Level Disease Burden Initiative Cancer Collaborators (2018). JAMA Oncology</li>
            </ul>
          </div>

          <div class="footer">
            <p>This clinical decision support tool was developed by PulmoKnight using evidence-based risk prediction models adapted for the Indian population.</p>
            <p><strong>Disclaimer:</strong> This report is intended for use by qualified healthcare professionals in conjunction with clinical evaluation. The risk assessment does not constitute a diagnosis.</p>
          </div>
        </body>
        </html>'
      )

      writeLines(html, file)
      unlink(c(temp_gauge, temp_breakdown))
    }
  )
}

shinyApp(ui = ui, server = server)
