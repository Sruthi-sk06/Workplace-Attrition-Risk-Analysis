# ============================================================
# WORKPLACE ATTRITION RISK ANALYSIS
# Final Dynamic R Shiny Application
# ============================================================

library(shiny)
library(readxl)
library(randomForest)
library(plotly)
library(ggplot2)

# ============================================================
# 1. LOAD MODEL
# ============================================================

model_info <- readRDS("../models/attrition_final_model.rds")
final_model <- model_info$model
final_threshold <- model_info$threshold

# ============================================================
# 2. LOAD AND PREPARE DATA
# ============================================================

training_data <- read_excel(
  "../data/WA_Fn-UseC_-HR-Employee-Attrition.csv.xlsx"
)

training_data <- training_data[
  , !names(training_data) %in% c(
    "EmployeeNumber", "EmployeeCount", "Over18", "StandardHours"
  )
]

factor_columns <- c(
  "Attrition", "BusinessTravel", "Department", "EducationField",
  "Gender", "JobRole", "MaritalStatus", "OverTime"
)

for (col in factor_columns) {
  training_data[[col]] <- as.factor(training_data[[col]])
}

# ============================================================
# 3. GLOBAL VALUES
# ============================================================

total_employees <- nrow(training_data)
employees_left <- sum(training_data$Attrition == "Yes")
employees_stayed <- sum(training_data$Attrition == "No")
attrition_rate <- round(employees_left / total_employees * 100, 2)

median_income <- median(training_data$MonthlyIncome, na.rm = TRUE)

# ============================================================
# 4. HELPER FUNCTIONS
# ============================================================

risk_class <- function(probability) {
  if (probability < 0.20) {
    "risk-low"
  } else if (probability < 0.50) {
    "risk-moderate"
  } else {
    "risk-high"
  }
}

risk_label <- function(probability) {
  if (probability < 0.20) {
    "LOW RISK"
  } else if (probability < 0.50) {
    "MODERATE RISK"
  } else {
    "HIGH RISK"
  }
}

# Navigation menu used on EVERY page.
# The visible menu is intentionally separate from the tab bar so the
# user always has the same navigation controls on every screen.
page_menu <- function(active = "home") {
  labels <- c(
    home = "⌂ Home",
    dashboard = "▦ Workforce Dashboard",
    analysis = "◉ Attrition Analysis",
    prediction = "⚠ Employee Risk",
    performance = "▤ Model Performance",
    insights = "💡 HR Insights"
  )
  
  tags$div(
    class = "app-menu",
    lapply(names(labels), function(key) {
      tags$button(
        type = "button",
        class = paste("menu-btn", if (key == active) "active" else ""),
        onclick = sprintf(
          "Shiny.setInputValue('nav_page','%s',{priority:'event'});",
          key
        ),
        labels[[key]]
      )
    })
  )
}

home_menu_cards <- function() {
  cards <- list(
    list("dashboard", "▦", "Workforce Dashboard", "Explore workforce KPIs, departments and employee patterns."),
    list("analysis", "◉", "Attrition Analysis", "Study the main employee factors associated with attrition."),
    list("prediction", "⚠", "Employee Risk Assessment", "Enter an employee profile and estimate attrition probability."),
    list("performance", "▤", "Model Performance", "Review model comparison, evaluation metrics and feature importance."),
    list("insights", "💡", "HR Insights", "Review supporting indicators and HR follow-up suggestions for the latest assessment.")
  )
  
  tags$div(
    class = "home-menu-grid",
    lapply(cards, function(card) {
      tags$button(
        type = "button",
        class = "home-menu-card",
        onclick = sprintf(
          "Shiny.setInputValue('nav_page','%s',{priority:'event'});",
          card[[1]]
        ),
        tags$div(class = "home-menu-icon", card[[2]]),
        tags$div(class = "home-menu-title", card[[3]]),
        tags$div(class = "home-menu-text", card[[4]]),
        tags$span(class = "home-menu-arrow", "→")
      )
    })
  )
}

# ============================================================
# 5. USER INTERFACE
# ============================================================

ui <- navbarPage(
  title = "Workplace Attrition Risk Analysis",
  id = "main_nav",
  collapsible = TRUE,
  
  header = tags$head(
    tags$style(HTML('
      body {
        background: #f4f7fb;
        font-family: Arial, sans-serif;
        color: #1f2937;
      }

      .navbar {
        background: #111827 !important;
        border: none !important;
        border-radius: 0 !important;
        margin-bottom: 0 !important;
        min-height: 58px;
      }

      .navbar-brand {
        color: #ffffff !important;
        font-weight: 700 !important;
        font-size: 19px !important;
      }

      /* The application uses the custom menu below on every page. */
      #main_nav > .navbar-nav {
        display: none !important;
      }

      .page-wrap {
        max-width: 1450px;
        margin: 0 auto;
        padding: 24px 30px 50px 30px;
      }

      .app-menu {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        padding: 10px;
        background: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 14px;
        box-shadow: 0 3px 12px rgba(15,23,42,.06);
        margin-bottom: 25px;
        position: sticky;
        top: 8px;
        z-index: 100;
      }

      .menu-btn {
        border: 0;
        background: #f3f4f6;
        color: #475569;
        padding: 10px 15px;
        border-radius: 9px;
        font-size: 13px;
        font-weight: 600;
        cursor: pointer;
        transition: .15s ease;
      }

      .menu-btn:hover {
        background: #e5e7eb;
        color: #111827;
      }

      .menu-btn.active {
        background: #2563eb;
        color: #ffffff;
      }

      .page-title {
        font-size: 31px;
        font-weight: 750;
        color: #111827;
        margin: 0 0 6px 0;
      }

      .page-subtitle {
        color: #64748b;
        font-size: 15px;
        margin-bottom: 26px;
      }

      .hero {
        background: linear-gradient(135deg, #111827 0%, #1e3a8a 100%);
        color: white;
        border-radius: 18px;
        padding: 34px;
        margin-bottom: 25px;
        box-shadow: 0 8px 25px rgba(15,23,42,.14);
      }

      .hero h1 {
        font-size: 34px;
        margin: 0 0 10px 0;
        font-weight: 750;
      }

      .hero p {
        color: #dbeafe;
        font-size: 16px;
        line-height: 1.6;
        max-width: 850px;
      }

      .hero-badge {
        display: inline-block;
        background: rgba(255,255,255,.12);
        border: 1px solid rgba(255,255,255,.20);
        border-radius: 999px;
        padding: 7px 12px;
        margin-top: 10px;
        font-size: 12px;
        font-weight: 700;
      }

      .stat-card {
        background: white;
        border: 1px solid #e5e7eb;
        border-radius: 14px;
        padding: 21px;
        margin-bottom: 20px;
        box-shadow: 0 3px 12px rgba(15,23,42,.06);
        min-height: 125px;
      }

      .stat-title {
        color: #64748b;
        font-size: 12px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: .5px;
      }

      .stat-value {
        color: #111827;
        font-size: 29px;
        font-weight: 750;
        margin-top: 8px;
      }

      .stat-note {
        color: #94a3b8;
        font-size: 12px;
        margin-top: 6px;
      }

      .input-panel, .result-card, .analysis-card, .profile-card, .info-card {
        background: white;
        border: 1px solid #e5e7eb;
        border-radius: 14px;
        padding: 24px;
        margin-bottom: 22px;
        box-shadow: 0 3px 12px rgba(15,23,42,.06);
      }

      .section-title {
        font-size: 19px;
        font-weight: 750;
        color: #111827;
        margin-bottom: 17px;
      }

      .section-kicker {
        color: #2563eb;
        font-size: 12px;
        font-weight: 750;
        text-transform: uppercase;
        letter-spacing: .6px;
        margin-bottom: 6px;
      }

      .btn-primary {
        background: #2563eb !important;
        border-color: #2563eb !important;
        border-radius: 9px !important;
        font-weight: 700 !important;
        padding: 10px 20px !important;
      }

      .btn-secondary {
        background: #111827 !important;
        color: #ffffff !important;
        border-color: #111827 !important;
        border-radius: 9px !important;
        font-weight: 700 !important;
        padding: 10px 20px !important;
      }

      .home-menu-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
        gap: 15px;
        margin-bottom: 25px;
      }

      .home-menu-card {
        position: relative;
        text-align: left;
        background: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 14px;
        padding: 21px;
        min-height: 165px;
        cursor: pointer;
        box-shadow: 0 3px 12px rgba(15,23,42,.06);
        transition: transform .15s ease, box-shadow .15s ease, border-color .15s ease;
      }

      .home-menu-card:hover {
        transform: translateY(-2px);
        border-color: #93c5fd;
        box-shadow: 0 8px 20px rgba(15,23,42,.10);
      }

      .home-menu-icon {
        font-size: 24px;
        color: #2563eb;
        margin-bottom: 12px;
      }

      .home-menu-title {
        font-size: 16px;
        font-weight: 750;
        color: #111827;
        margin-bottom: 7px;
      }

      .home-menu-text {
        font-size: 13px;
        color: #64748b;
        line-height: 1.45;
        padding-right: 10px;
      }

      .home-menu-arrow {
        position: absolute;
        right: 18px;
        bottom: 16px;
        color: #2563eb;
        font-weight: 800;
      }

      .mini-card {
        background: #f8fafc;
        border: 1px solid #e2e8f0;
        border-radius: 11px;
        padding: 15px;
        min-height: 92px;
        margin-bottom: 12px;
      }

      .mini-label, .profile-label {
        color: #64748b;
        font-size: 11px;
        font-weight: 750;
        text-transform: uppercase;
        letter-spacing: .5px;
      }

      .mini-value, .profile-value {
        color: #111827;
        font-size: 18px;
        font-weight: 750;
        margin-top: 7px;
      }

      .mini-value.small, .profile-value.small {
        font-size: 14px;
        line-height: 1.3;
      }

      .risk-low, .risk-moderate, .risk-high {
        padding: 16px 18px;
        border-radius: 11px;
        font-size: 20px;
        font-weight: 800;
        text-align: center;
        margin: 12px 0;
      }

      .risk-low { background: #dcfce7; color: #166534; }
      .risk-moderate { background: #fef3c7; color: #92400e; }
      .risk-high { background: #fee2e2; color: #991b1b; }

      .assessment-hero {
        background: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 15px;
        padding: 25px;
        margin-bottom: 20px;
        box-shadow: 0 3px 12px rgba(15,23,42,.06);
      }

      .hero-label {
        color: #64748b;
        font-size: 11px;
        font-weight: 750;
        letter-spacing: .6px;
      }

      .hero-title {
        color: #111827;
        font-size: 24px;
        font-weight: 750;
        margin: 6px 0 12px 0;
      }

      .metric-value {
        font-size: 27px;
        font-weight: 800;
        color: #111827;
      }

      .metric-label {
        color: #64748b;
        font-size: 12px;
        margin-top: 5px;
        font-weight: 650;
      }

      .muted { color: #64748b; }
      .empty-state { text-align: center; padding: 35px; }

      .notice {
        background: #eff6ff;
        border-left: 4px solid #2563eb;
        border-radius: 8px;
        padding: 15px 17px;
        color: #334155;
        margin-bottom: 18px;
      }

      .success-note {
        background: #f0fdf4;
        border-left: 4px solid #16a34a;
        border-radius: 8px;
        padding: 15px 17px;
        color: #334155;
        margin-bottom: 18px;
      }

      .warning-note {
        background: #fffbeb;
        border-left: 4px solid #d97706;
        border-radius: 8px;
        padding: 15px 17px;
        color: #334155;
        margin-bottom: 18px;
      }

      .table > thead > tr > th {
        background: #f8fafc;
        color: #334155;
        font-weight: 750;
      }

      .footer-note {
        color: #94a3b8;
        font-size: 12px;
        text-align: center;
        padding: 15px 0 5px 0;
      }

      @media (max-width: 800px) {
        .page-wrap { padding: 18px 14px 40px 14px; }
        .page-title { font-size: 26px; }
        .hero h1 { font-size: 27px; }
        .app-menu { position: relative; top: 0; }
        .menu-btn { flex: 1 1 45%; }
      }
    ')),
    tags$script(HTML('
      // Keeps keyboard focus and gives a small visual cue after navigation.
      $(document).on("shiny:value", function(event) {
        if (event.name === "main_nav") {
          window.scrollTo({top: 0, behavior: "smooth"});
        }
      });
    '))
  ),
  
  # ==========================================================
  # PAGE 1 - HOME
  # ==========================================================
  
  tabPanel(
    "⌂ Home",
    div(
      class = "page-wrap",
      page_menu("home"),
      
      div(
        class = "hero",
        tags$h1("Workplace Attrition Risk Analysis"),
        p("A decision-support dashboard for understanding workforce attrition patterns and estimating individual employee attrition risk using machine learning."),
        span(class = "hero-badge", "R Shiny • Random Forest • HR Analytics")
      ),
      
      div(class = "page-title", "HR Management Overview"),
      div(class = "page-subtitle", "Use the menu below to move between workforce analytics, employee risk assessment, model performance and HR insights."),
      
      home_menu_cards(),
      
      fluidRow(
        column(3, div(class = "stat-card",
                      div(class = "stat-title", "Employees Analysed"),
                      div(class = "stat-value", format(total_employees, big.mark = ",")),
                      div(class = "stat-note", "Project dataset")
        )),
        column(3, div(class = "stat-card",
                      div(class = "stat-title", "Employees Left"),
                      div(class = "stat-value", format(employees_left, big.mark = ",")),
                      div(class = "stat-note", paste0(attrition_rate, "% of employees"))
        )),
        column(3, div(class = "stat-card",
                      div(class = "stat-title", "Employees Stayed"),
                      div(class = "stat-value", format(employees_stayed, big.mark = ",")),
                      div(class = "stat-note", "Observed in dataset")
        )),
        column(3, div(class = "stat-card",
                      div(class = "stat-title", "Prediction Threshold"),
                      div(class = "stat-value", paste0(final_threshold * 100, "%")),
                      div(class = "stat-note", "Used by the application")
        ))
      ),
      
      uiOutput("home_status"),
      
      div(class = "input-panel",
          div(class = "section-kicker", "Project Flow"),
          div(class = "section-title", "How the system is used"),
          fluidRow(
            column(3, div(class = "mini-card", div(class = "mini-label", "01"), div(class = "mini-value", "Enter Profile"), p(class="muted", "Manager/HR enters employee information."))),
            column(3, div(class = "mini-card", div(class = "mini-label", "02"), div(class = "mini-value", "Assess Risk"), p(class="muted", "Random Forest estimates attrition probability."))),
            column(3, div(class = "mini-card", div(class = "mini-label", "03"), div(class = "mini-value", "Review Indicators"), p(class="muted", "Supporting profile indicators are highlighted."))),
            column(3, div(class = "mini-card", div(class = "mini-label", "04"), div(class = "mini-value", "HR Follow-Up"), p(class="muted", "Suggestions support human review and discussion.")))
          )
      ),
      
      div(class = "footer-note", "This application is a decision-support system. Model predictions and profile indicators should be reviewed by HR professionals and should not be treated as automatic employment decisions.")
    )
  ),
  
  # ==========================================================
  # PAGE 2 - WORKFORCE DASHBOARD
  # ==========================================================
  
  tabPanel(
    "▦ Workforce Dashboard",
    div(
      class = "page-wrap",
      page_menu("dashboard"),
      div(class = "page-title", "Workforce Dashboard"),
      div(class = "page-subtitle", "Interactive workforce-level view of the project dataset."),
      
      fluidRow(
        column(3, div(class = "stat-card", div(class="stat-title","Total Employees"), div(class="stat-value", format(total_employees,big.mark=",")), div(class="stat-note","All records"))),
        column(3, div(class = "stat-card", div(class="stat-title","Attrition Rate"), div(class="stat-value", paste0(attrition_rate,"%")), div(class="stat-note","Observed attrition"))),
        column(3, div(class = "stat-card", div(class="stat-title","Employees Left"), div(class="stat-value", format(employees_left,big.mark=",")), div(class="stat-note","Attrition = Yes"))),
        column(3, div(class = "stat-card", div(class="stat-title","Employees Stayed"), div(class="stat-value", format(employees_stayed,big.mark=",")), div(class="stat-note","Attrition = No")))
      ),
      
      div(class="input-panel",
          div(class="section-kicker","Interactive filter"),
          div(class="section-title","Department View"),
          selectInput("dashboard_department", "Select department", choices = c("All Departments", levels(training_data$Department)), selected = "All Departments"),
          uiOutput("dashboard_filter_summary")
      ),
      
      fluidRow(
        column(6, div(class="analysis-card", plotlyOutput("attrition_plot", height="390px"))),
        column(6, div(class="analysis-card", plotlyOutput("department_attrition_plot", height="390px")))
      ),
      
      fluidRow(
        column(6, div(class="analysis-card", plotlyOutput("overtime_plot", height="390px"))),
        column(6, div(class="analysis-card", plotlyOutput("satisfaction_plot", height="390px")))
      ),
      
      div(class="analysis-card", plotlyOutput("income_plot", height="420px"))
    )
  ),
  
  # ==========================================================
  # PAGE 3 - ATTRITION ANALYSIS
  # ==========================================================
  
  tabPanel(
    "◉ Attrition Analysis",
    div(
      class = "page-wrap",
      page_menu("analysis"),
      div(class = "page-title", "Attrition Analysis"),
      div(class = "page-subtitle", "Statistical and exploratory findings from the employee dataset."),
      
      div(class="notice",
          strong("Interpretation note: "),
          "The statistical tests identify associations between employee attributes and attrition. They do not prove that a factor directly causes an employee to leave."
      ),
      
      div(class="input-panel",
          div(class="section-kicker","Interactive analysis"),
          div(class="section-title","Explore a Key Attrition Factor"),
          selectInput(
            "analysis_factor",
            "Choose a factor",
            choices = c(
              "Overtime" = "OverTime",
              "Job Satisfaction" = "JobSatisfaction",
              "Environment Satisfaction" = "EnvironmentSatisfaction",
              "Work-Life Balance" = "WorkLifeBalance",
              "Job Involvement" = "JobInvolvement",
              "Marital Status" = "MaritalStatus",
              "Business Travel" = "BusinessTravel"
            ),
            selected = "OverTime"
          ),
          plotlyOutput("analysis_dynamic_plot", height="430px")
      ),
      
      fluidRow(
        column(6, div(class="analysis-card",
                      div(class="section-title","Key Findings"),
                      tags$ul(
                        tags$li("Overtime shows a clear difference in observed attrition proportions."),
                        tags$li("Job satisfaction is associated with attrition in the project analysis."),
                        tags$li("Job role, marital status, job involvement and business travel showed statistically significant categorical associations."),
                        tags$li("Monthly income, total working years and years at company showed significant distribution differences between attrition groups.")
                      )
        )),
        column(6, div(class="analysis-card",
                      div(class="section-title","Statistical Evidence"),
                      tags$table(class="table table-bordered table-striped",
                                 tags$thead(tags$tr(tags$th("Analysis"),tags$th("Observed result"))),
                                 tags$tbody(
                                   tags$tr(tags$td("OverTime vs Attrition"),tags$td("χ² = 87.564, p < 2.2e-16")),
                                   tags$tr(tags$td("Job Satisfaction"),tags$td("χ² = 17.505, p = 0.0005563")),
                                   tags$tr(tags$td("Total Working Years"),tags$td("Wilcoxon p = 2.398e-14")),
                                   tags$tr(tags$td("Monthly Income"),tags$td("Wilcoxon p = 2.953e-14")),
                                   tags$tr(tags$td("Years at Company"),tags$td("Wilcoxon p = 2.915e-13"))
                                 )
                      )
        ))
      ),
      
      div(class="input-panel",
          div(class="section-title","Important Predictive Variables"),
          p("The Random Forest feature-importance analysis identified variables such as OverTime, MonthlyIncome, TotalWorkingYears, JobRole, Age, JobLevel and YearsAtCompany as important predictive variables."),
          p(class="muted","Feature importance reflects predictive contribution within the model; it should not be interpreted as causal importance.")
      )
    )
  ),
  
  # ==========================================================
  # PAGE 4 - EMPLOYEE RISK ASSESSMENT
  # ==========================================================
  
  tabPanel(
    "⚠ Employee Risk",
    div(
      class = "page-wrap",
      page_menu("prediction"),
      div(class = "page-title", "Employee Risk Assessment"),
      div(class = "page-subtitle", "Enter an employee profile and use the trained Random Forest model to estimate attrition probability."),
      
      div(class="notice",
          strong("How it works: "),
          "The application estimates the probability of Attrition = Yes. A probability of 30% or higher is classified as potentially at risk because the project uses a 0.30 application threshold."
      ),
      
      div(class="input-panel",
          div(class="section-kicker","Employee profile"),
          div(class="section-title","Personal Information"),
          fluidRow(
            column(4, numericInput("Age","Age",value=30,min=18,max=80)),
            column(4, selectInput("BusinessTravel","Business Travel",choices=levels(training_data$BusinessTravel))),
            column(4, selectInput("Department","Department",choices=levels(training_data$Department)))
          ),
          fluidRow(
            column(4, numericInput("DistanceFromHome","Distance From Home (km)",value=5,min=1)),
            column(4, selectInput("EducationField","Education Field",choices=levels(training_data$EducationField))),
            column(4, selectInput("Gender","Gender",choices=levels(training_data$Gender)))
          )
      ),
      
      div(class="input-panel",
          div(class="section-title","Job Information"),
          fluidRow(
            column(4, selectInput("JobRole","Job Role",choices=levels(training_data$JobRole))),
            column(4, numericInput("JobLevel","Job Level",value=1,min=1,max=5)),
            column(4, numericInput("MonthlyIncome","Monthly Income",value=5000,min=1000))
          ),
          fluidRow(
            column(4, selectInput("OverTime","Overtime",choices=levels(training_data$OverTime))),
            column(4, selectInput("MaritalStatus","Marital Status",choices=levels(training_data$MaritalStatus))),
            column(4, numericInput("NumCompaniesWorked","Number of Companies Worked",value=2,min=0))
          )
      ),
      
      div(class="input-panel",
          div(class="section-title","Employee Satisfaction"),
          fluidRow(
            column(4, selectInput("EnvironmentSatisfaction","Environment Satisfaction",choices=c(1,2,3,4),selected=3)),
            column(4, selectInput("JobInvolvement","Job Involvement",choices=c(1,2,3,4),selected=3)),
            column(4, selectInput("JobSatisfaction","Job Satisfaction",choices=c(1,2,3,4),selected=3))
          ),
          fluidRow(
            column(4, selectInput("RelationshipSatisfaction","Relationship Satisfaction",choices=c(1,2,3,4),selected=3)),
            column(4, selectInput("WorkLifeBalance","Work-Life Balance",choices=c(1,2,3,4),selected=3)),
            column(4, numericInput("StockOptionLevel","Stock Option Level",value=0,min=0,max=3))
          )
      ),
      
      div(class="input-panel",
          div(class="section-title","Experience and Career Information"),
          fluidRow(
            column(4, numericInput("TotalWorkingYears","Total Working Years",value=5,min=0)),
            column(4, numericInput("YearsAtCompany","Years at Company",value=3,min=0)),
            column(4, numericInput("YearsInCurrentRole","Years in Current Role",value=2,min=0))
          ),
          fluidRow(
            column(4, numericInput("YearsSinceLastPromotion","Years Since Last Promotion",value=1,min=0)),
            column(4, numericInput("YearsWithCurrManager","Years With Current Manager",value=2,min=0)),
            column(4, numericInput("TrainingTimesLastYear","Training Times Last Year",value=3,min=0))
          )
      ),
      
      div(class="input-panel",
          div(class="section-title","Additional Information"),
          fluidRow(
            column(4, numericInput("DailyRate","Daily Rate",value=800,min=0)),
            column(4, numericInput("HourlyRate","Hourly Rate",value=60,min=0)),
            column(4, numericInput("MonthlyRate","Monthly Rate",value=15000,min=0))
          ),
          fluidRow(
            column(4, numericInput("PercentSalaryHike","Percent Salary Hike",value=15,min=0)),
            column(4, numericInput("PerformanceRating","Performance Rating",value=3,min=1,max=4)),
            column(4, numericInput("Education","Education Level",value=3,min=1,max=5))
          )
      ),
      
      div(class="input-panel",
          actionButton("predict","Assess Employee Risk",class="btn-primary"),
          span(style="margin-left:12px;color:#64748b;font-size:13px;", "The latest assessment is automatically connected to HR Insights.")
      ),
      
      uiOutput("result")
    )
  ),
  
  # ==========================================================
  # PAGE 5 - MODEL PERFORMANCE
  # ==========================================================
  
  tabPanel(
    "▤ Model Performance",
    div(
      class = "page-wrap",
      page_menu("performance"),
      div(class = "page-title", "Model Comparison & Performance"),
      div(class = "page-subtitle", "Evaluation of the three classification models used in the project."),
      
      div(class="input-panel",
          div(class="section-title","Model Comparison"),
          p("The models were evaluated using the same test dataset."),
          tags$table(class="table table-bordered table-striped",
                     tags$thead(tags$tr(
                       tags$th("Model"),tags$th("Accuracy"),tags$th("Precision"),tags$th("Recall"),tags$th("F1 Score"),tags$th("ROC-AUC")
                     )),
                     tags$tbody(
                       tags$tr(tags$td("Logistic Regression"),tags$td("87.12%"),tags$td("64.71%"),tags$td("45.83%"),tags$td("53.66%"),tags$td("88.79%")),
                       tags$tr(tags$td("Decision Tree"),tags$td("83.73%"),tags$td("50.00%"),tags$td("22.92%"),tags$td("31.43%"),tags$td("71.15%")),
                       tags$tr(tags$td("Random Forest"),tags$td("85.42%"),tags$td("54.90%"),tags$td("58.33%"),tags$td("56.57%"),tags$td("85.52%"))
                     )
          )
      ),
      
      fluidRow(
        column(3,div(class="stat-card",div(class="stat-title","Random Forest Accuracy"),div(class="stat-value","85.42%"),div(class="stat-note","Test set"))),
        column(3,div(class="stat-card",div(class="stat-title","Precision"),div(class="stat-value","54.90%"),div(class="stat-note","Test set"))),
        column(3,div(class="stat-card",div(class="stat-title","Recall"),div(class="stat-value","58.33%"),div(class="stat-note","Test set"))),
        column(3,div(class="stat-card",div(class="stat-title","F1 Score"),div(class="stat-value","56.57%"),div(class="stat-note","Test set")))
      ),
      
      fluidRow(
        column(4,div(class="stat-card",div(class="stat-title","ROC-AUC"),div(class="stat-value","85.52%"),div(class="stat-note","Test set"))),
        column(4,div(class="stat-card",div(class="stat-title","Random Forest Trees"),div(class="stat-value","500"),div(class="stat-note","Final model"))),
        column(4,div(class="stat-card",div(class="stat-title","Application Threshold"),div(class="stat-value","0.30"),div(class="stat-note","Probability threshold")))
      ),
      
      div(class="input-panel",
          div(class="section-title","Final Model"),
          p("The final prediction system uses Random Forest classification. Random Forest combines multiple decision trees and can capture complex relationships among employee attributes."),
          p("The model contains 500 trees. The application uses a probability threshold of 0.30 when converting the predicted probability into a Yes/No classification.")
      ),
      
      div(class="input-panel",
          div(class="section-title","Feature Importance"),
          tags$ol(
            tags$li("OverTime"), tags$li("MonthlyIncome"), tags$li("TotalWorkingYears"),
            tags$li("JobRole"), tags$li("Age"), tags$li("JobLevel"),
            tags$li("YearsAtCompany"), tags$li("YearsInCurrentRole"),
            tags$li("YearsWithCurrManager"), tags$li("StockOptionLevel")
          ),
          p(class="muted","Feature importance indicates predictive contribution to the Random Forest model and does not establish causation.")
      )
    )
  ),
  
  # ==========================================================
  # PAGE 6 - HR INSIGHTS
  # ==========================================================
  
  tabPanel(
    "💡 HR Insights",
    div(
      class = "page-wrap",
      page_menu("insights"),
      div(class = "page-title", "HR Insights"),
      div(class = "page-subtitle", "Dynamic decision-support view connected to the latest employee risk assessment."),
      uiOutput("insights_dynamic")
    )
  )
)

# ============================================================
# 6. SERVER
# ============================================================

server <- function(input, output, session) {
  
  prediction_result <- reactiveVal(NULL)
  latest_employee <- reactiveVal(NULL)
  
  # ----------------------------------------------------------
  # NAVIGATION
  # ----------------------------------------------------------
  
  page_map <- c(
    home = "⌂ Home",
    dashboard = "▦ Workforce Dashboard",
    analysis = "◉ Attrition Analysis",
    prediction = "⚠ Employee Risk",
    performance = "▤ Model Performance",
    insights = "💡 HR Insights"
  )
  
  observeEvent(input$nav_page, {
    req(input$nav_page %in% names(page_map))
    updateTabsetPanel(
      session,
      "main_nav",
      selected = page_map[[input$nav_page]]
    )
  }, ignoreInit = TRUE)
  
  # ----------------------------------------------------------
  # HOME - LATEST ASSESSMENT
  # ----------------------------------------------------------
  
  output$home_status <- renderUI({
    result <- prediction_result()
    emp <- latest_employee()
    
    if (is.null(result) || is.null(emp)) {
      return(
        div(class="input-panel empty-state",
            div(class="section-kicker","Latest assessment"),
            div(class="section-title","No employee assessment has been completed yet"),
            p("Use Employee Risk Assessment from the menu to enter an employee profile and generate the first prediction."),
            actionButton("home_go_prediction","Start Employee Risk Assessment",class="btn-primary")
        )
      )
    }
    
    probability <- result$probability
    prediction <- result$prediction
    
    div(class="input-panel",
        div(class="section-kicker","Latest assessment"),
        div(class="section-title","Most Recent Employee Risk Assessment"),
        fluidRow(
          column(3,div(class="mini-card",div(class="mini-label","Prediction"),div(class="mini-value",ifelse(prediction=="Yes","Potentially at risk","Predicted to stay")))),
          column(3,div(class="mini-card",div(class="mini-label","Probability"),div(class="mini-value",paste0(round(probability*100,1),"%")))),
          column(3,div(class="mini-card",div(class="mini-label","Department"),div(class="mini-value small",as.character(emp$Department)))),
          column(3,div(class="mini-card",div(class="mini-label","Job Role"),div(class="mini-value small",as.character(emp$JobRole))))
        ),
        div(class=risk_class(probability),paste0(risk_label(probability)," — ",round(probability*100,1),"%")),
        actionButton("home_view_insights","Open HR Insights for This Assessment",class="btn-primary")
    )
  })
  
  observeEvent(input$home_go_prediction, {
    updateTabsetPanel(session,"main_nav",selected=page_map[["prediction"]])
  })
  
  observeEvent(input$home_view_insights, {
    updateTabsetPanel(session,"main_nav",selected=page_map[["insights"]])
  })
  
  # ----------------------------------------------------------
  # DASHBOARD - FILTERED DATA
  # ----------------------------------------------------------
  
  dashboard_data <- reactive({
    req(input$dashboard_department)
    if (input$dashboard_department == "All Departments") {
      training_data
    } else {
      training_data[training_data$Department == input$dashboard_department, , drop=FALSE]
    }
  })
  
  output$dashboard_filter_summary <- renderUI({
    d <- dashboard_data()
    rate <- if (nrow(d) == 0) 0 else round(mean(d$Attrition == "Yes")*100,2)
    div(class="success-note",
        strong(paste0("Current view: ", nrow(d), " employees. ")),
        paste0("Observed attrition rate in this view: ", rate, "%. Charts below update automatically when the department filter changes.")
    )
  })
  
  output$attrition_plot <- renderPlotly({
    d <- dashboard_data()
    x <- as.data.frame(table(d$Attrition))
    names(x) <- c("Attrition","Employees")
    plot_ly(x,x=~Attrition,y=~Employees,type="bar",text=~paste("Attrition:",Attrition,"<br>Employees:",Employees),hoverinfo="text") %>%
      layout(title="Employee Attrition Distribution",xaxis=list(title="Attrition"),yaxis=list(title="Employees")) %>%
      config(displaylogo=FALSE,modeBarButtonsToRemove=c("lasso2d","select2d"))
  })
  
  output$department_attrition_plot <- renderPlotly({
    d <- dashboard_data()
    x <- as.data.frame(table(d$Department,d$Attrition))
    names(x) <- c("Department","Attrition","Employees")
    plot_ly(x,x=~Department,y=~Employees,color=~Attrition,type="bar",text=~paste("Department:",Department,"<br>Attrition:",Attrition,"<br>Employees:",Employees),hoverinfo="text") %>%
      layout(title="Attrition by Department",barmode="group",xaxis=list(title="Department"),yaxis=list(title="Employees")) %>%
      config(displaylogo=FALSE,modeBarButtonsToRemove=c("lasso2d","select2d"))
  })
  
  output$overtime_plot <- renderPlotly({
    d <- dashboard_data()
    x <- aggregate(list(Count=rep(1,nrow(d))),list(OverTime=d$OverTime,Attrition=d$Attrition),sum)
    x$Percentage <- ave(x$Count,x$OverTime,FUN=function(z) z/sum(z)*100)
    p <- ggplot(x,aes(x=OverTime,y=Percentage,fill=Attrition,text=paste("Overtime:",OverTime,"<br>Attrition:",Attrition,"<br>Employees:",Count,"<br>Proportion:",round(Percentage,1),"%")))+
      geom_col()+labs(title="Overtime and Attrition",x="Overtime",y="Proportion",fill="Attrition")+
      scale_y_continuous(labels=function(z) paste0(z,"%"),limits=c(0,100))+theme_minimal(base_size=13)
    ggplotly(p,tooltip="text") %>% config(displaylogo=FALSE,modeBarButtonsToRemove=c("lasso2d","select2d"))
  })
  
  output$satisfaction_plot <- renderPlotly({
    d <- dashboard_data()
    x <- aggregate(list(Count=rep(1,nrow(d))),list(JobSatisfaction=d$JobSatisfaction,Attrition=d$Attrition),sum)
    x$Percentage <- ave(x$Count,x$JobSatisfaction,FUN=function(z) z/sum(z)*100)
    p <- ggplot(x,aes(x=factor(JobSatisfaction),y=Percentage,fill=Attrition,text=paste("Job Satisfaction:",JobSatisfaction,"<br>Attrition:",Attrition,"<br>Employees:",Count,"<br>Proportion:",round(Percentage,1),"%")))+
      geom_col()+labs(title="Job Satisfaction and Attrition",x="Job Satisfaction",y="Proportion",fill="Attrition")+
      scale_y_continuous(labels=function(z) paste0(z,"%"),limits=c(0,100))+theme_minimal(base_size=13)
    ggplotly(p,tooltip="text") %>% config(displaylogo=FALSE,modeBarButtonsToRemove=c("lasso2d","select2d"))
  })
  
  output$income_plot <- renderPlotly({
    d <- dashboard_data()
    p <- ggplot(d,aes(x=Attrition,y=MonthlyIncome,fill=Attrition,text=paste("Attrition:",Attrition,"<br>Monthly income:",round(MonthlyIncome,0))))+
      geom_boxplot()+labs(title="Monthly Income vs Attrition",x="Attrition",y="Monthly Income")+theme_minimal(base_size=13)+theme(legend.position="none")
    ggplotly(p,tooltip="text") %>% config(displaylogo=FALSE,modeBarButtonsToRemove=c("lasso2d","select2d"))
  })
  
  # ----------------------------------------------------------
  # ATTRITION ANALYSIS - DYNAMIC FACTOR CHART
  # ----------------------------------------------------------
  
  output$analysis_dynamic_plot <- renderPlotly({
    req(input$analysis_factor)
    factor_name <- input$analysis_factor
    x <- aggregate(
      list(Count=rep(1,nrow(training_data))),
      list(Level=training_data[[factor_name]],Attrition=training_data$Attrition),
      sum
    )
    x$Percentage <- ave(x$Count,x$Level,FUN=function(z) z/sum(z)*100)
    p <- ggplot(x,aes(x=factor(Level),y=Percentage,fill=Attrition,text=paste("Level:",Level,"<br>Attrition:",Attrition,"<br>Employees:",Count,"<br>Proportion:",round(Percentage,1),"%")))+
      geom_col()+labs(x="Selected factor",y="Proportion",fill="Attrition")+
      scale_y_continuous(labels=function(z) paste0(z,"%"),limits=c(0,100))+theme_minimal(base_size=13)
    ggplotly(p,tooltip="text") %>% config(displaylogo=FALSE,modeBarButtonsToRemove=c("lasso2d","select2d"))
  })
  
  # ----------------------------------------------------------
  # PREDICTION
  # ----------------------------------------------------------
  
  observeEvent(input$predict, {
    employee <- data.frame(
      Age=input$Age,
      BusinessTravel=input$BusinessTravel,
      DailyRate=input$DailyRate,
      Department=input$Department,
      DistanceFromHome=input$DistanceFromHome,
      Education=input$Education,
      EducationField=input$EducationField,
      EnvironmentSatisfaction=as.numeric(input$EnvironmentSatisfaction),
      Gender=input$Gender,
      HourlyRate=input$HourlyRate,
      JobInvolvement=as.numeric(input$JobInvolvement),
      JobLevel=input$JobLevel,
      JobRole=input$JobRole,
      JobSatisfaction=as.numeric(input$JobSatisfaction),
      MaritalStatus=input$MaritalStatus,
      MonthlyIncome=input$MonthlyIncome,
      MonthlyRate=input$MonthlyRate,
      NumCompaniesWorked=input$NumCompaniesWorked,
      OverTime=input$OverTime,
      PercentSalaryHike=input$PercentSalaryHike,
      PerformanceRating=input$PerformanceRating,
      RelationshipSatisfaction=as.numeric(input$RelationshipSatisfaction),
      StockOptionLevel=input$StockOptionLevel,
      TotalWorkingYears=input$TotalWorkingYears,
      TrainingTimesLastYear=input$TrainingTimesLastYear,
      WorkLifeBalance=as.numeric(input$WorkLifeBalance),
      YearsAtCompany=input$YearsAtCompany,
      YearsInCurrentRole=input$YearsInCurrentRole,
      YearsSinceLastPromotion=input$YearsSinceLastPromotion,
      YearsWithCurrManager=input$YearsWithCurrManager,
      stringsAsFactors=FALSE
    )
    
    # Match every categorical variable to the exact levels used during training.
    for (col in c("BusinessTravel","Department","EducationField","Gender","JobRole","MaritalStatus","OverTime")) {
      employee[[col]] <- factor(employee[[col]],levels=levels(training_data[[col]]))
    }
    
    probability <- as.numeric(predict(final_model,newdata=employee,type="prob")[,"Yes"])
    prediction <- ifelse(probability >= final_threshold,"Yes","No")
    
    prediction_result(list(probability=probability,prediction=prediction))
    latest_employee(employee)
    
    # Move the user directly to the result section on the same page.
    session$sendCustomMessage("scrollToResult", list())
  })
  
  output$result <- renderUI({
    result <- prediction_result()
    emp <- latest_employee()
    if (is.null(result) || is.null(emp)) return(NULL)
    
    probability <- result$probability
    prediction <- result$prediction
    
    div(class="result-card",
        div(class="section-kicker","Model output"),
        div(class="section-title","Employee Risk Assessment Result"),
        div(class=risk_class(probability),paste0(risk_label(probability)," — ",round(probability*100,2),"%")),
        br(),
        fluidRow(
          column(4,div(class="mini-card",div(class="mini-label","Classification"),div(class="mini-value",ifelse(prediction=="Yes","Potentially at risk","Predicted to stay")))),
          column(4,div(class="mini-card",div(class="mini-label","Probability"),div(class="mini-value",paste0(round(probability*100,2),"%")))),
          column(4,div(class="mini-card",div(class="mini-label","Threshold"),div(class="mini-value",paste0(final_threshold*100,"%"))))
        ),
        p(ifelse(prediction=="Yes","The model probability is at or above the application's 0.30 threshold, so the system classifies this profile as potentially at risk.","The model probability is below the application's 0.30 threshold, so the system classifies this profile as predicted to stay.")),
        p(class="muted","This prediction is an estimate from the trained model. It should be interpreted together with the employee profile and human HR review."),
        actionButton("view_insights","View HR Insights for This Assessment",class="btn-primary")
    )
  })
  
  observeEvent(input$view_insights, {
    req(!is.null(prediction_result()))
    updateTabsetPanel(session,"main_nav",selected=page_map[["insights"]])
  })
  
  # ----------------------------------------------------------
  # HR INSIGHTS - CONNECTED TO LATEST PREDICTION
  # ----------------------------------------------------------
  
  output$insights_dynamic <- renderUI({
    result <- prediction_result()
    emp <- latest_employee()
    
    if (is.null(result) || is.null(emp)) {
      return(
        div(class="input-panel empty-state",
            div(class="section-kicker","Waiting for assessment"),
            div(class="section-title","No current employee assessment is available"),
            p("This page is connected to Employee Risk Assessment. Complete an assessment first, then return here to see the indicators for that exact employee profile."),
            actionButton("insights_go_prediction","Go to Employee Risk Assessment",class="btn-primary")
        )
      )
    }
    
    probability <- result$probability
    prediction <- result$prediction
    
    indicators <- character(0)
    actions <- character(0)
    
    if (as.character(emp$OverTime) == "Yes") {
      indicators <- c(indicators,"Overtime is marked as Yes for this employee.")
      actions <- c(actions,"Review workload, overtime frequency and work distribution.")
    }
    if (as.numeric(emp$JobSatisfaction) <= 2) {
      indicators <- c(indicators,"Job satisfaction is at a low level (1–2).")
      actions <- c(actions,"Discuss satisfaction and workplace concerns with the employee.")
    }
    if (as.numeric(emp$EnvironmentSatisfaction) <= 2) {
      indicators <- c(indicators,"Environment satisfaction is at a low level (1–2).")
      actions <- c(actions,"Explore workplace or team-level issues affecting the employee experience.")
    }
    if (as.numeric(emp$WorkLifeBalance) <= 2) {
      indicators <- c(indicators,"Work-life balance is at a low level (1–2).")
      actions <- c(actions,"Review workload, scheduling and work-life balance support.")
    }
    if (as.numeric(emp$YearsAtCompany) <= 2) {
      indicators <- c(indicators,"The employee has relatively short tenure at the company.")
      actions <- c(actions,"Consider onboarding, mentoring and early-career engagement support.")
    }
    if (as.numeric(emp$YearsSinceLastPromotion) >= 3) {
      indicators <- c(indicators,"Several years have passed since the last promotion.")
      actions <- c(actions,"Review career progression and development opportunities.")
    }
    if (as.numeric(emp$DistanceFromHome) >= 15) {
      indicators <- c(indicators,"The entered profile has a relatively long distance from home.")
      actions <- c(actions,"Consider whether commute or work-location flexibility is relevant.")
    }
    if (as.numeric(emp$MonthlyIncome) < median_income) {
      indicators <- c(indicators,"Entered monthly income is below the dataset median.")
      actions <- c(actions,"Review compensation and career-growth context rather than using income alone.")
    }
    if (as.numeric(emp$JobInvolvement) <= 2) {
      indicators <- c(indicators,"Job involvement is at a low level (1–2).")
      actions <- c(actions,"Discuss role engagement, responsibilities and opportunities for meaningful work.")
    }
    
    if (length(indicators) == 0) {
      indicators <- "No predefined profile indicators were triggered by the entered values."
      actions <- "Continue normal employee engagement and use the model probability as a decision-support signal."
    }
    
    indicators <- unique(indicators)
    actions <- unique(actions)
    
    div(
      div(class="assessment-hero",
          div(class="hero-label","CURRENT ASSESSMENT"),
          div(class="hero-title",ifelse(prediction=="Yes","Employee may be at risk of attrition","Employee is predicted to stay")),
          div(class=risk_class(probability),paste0(risk_label(probability)," • ",round(probability*100,1),"% estimated probability"))
      ),
      
      fluidRow(
        column(3,div(class="profile-card",div(class="profile-label","Department"),div(class="profile-value small",as.character(emp$Department)))),
        column(3,div(class="profile-card",div(class="profile-label","Job Role"),div(class="profile-value small",as.character(emp$JobRole)))),
        column(3,div(class="profile-card",div(class="profile-label","Overtime"),div(class="profile-value",as.character(emp$OverTime)))),
        column(3,div(class="profile-card",div(class="profile-label","Job Satisfaction"),div(class="profile-value",as.character(emp$JobSatisfaction))))
      ),
      
      div(class="input-panel",
          div(class="section-kicker","Profile-based explanation"),
          div(class="section-title","Supporting Indicators from This Employee Profile"),
          tags$ul(lapply(indicators,tags$li))
      ),
      
      div(class="input-panel",
          div(class="section-kicker","Decision support"),
          div(class="section-title","Suggested HR Follow-Up"),
          tags$ol(lapply(actions,tags$li)),
          p(class="muted","These suggestions are based on the entered employee profile and project findings. They are not automatic HR decisions and do not establish that any listed factor caused attrition.")
      ),
      
      div(class="input-panel",
          div(class="section-title","Assessment Interpretation"),
          p(ifelse(prediction=="Yes","The estimated probability is at or above the application's 0.30 classification threshold.","The estimated probability is below the application's 0.30 classification threshold.")),
          p(class="muted","Risk labels are application-level interpretations of model probability; they are not separate machine-learning classes."),
          actionButton("insights_reassess","Assess Another Employee",class="btn-primary")
      )
    )
  })
  
  observeEvent(input$insights_go_prediction, {
    updateTabsetPanel(session,"main_nav",selected=page_map[["prediction"]])
  })
  
  observeEvent(input$insights_reassess, {
    updateTabsetPanel(session,"main_nav",selected=page_map[["prediction"]])
  })
}

# ============================================================
# 7. RUN APPLICATION
# ============================================================

shinyApp(ui=ui,server=server)
