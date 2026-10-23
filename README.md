# Workplace Attrition Risk Analysis

An interactive data analytics project built using **R and R Shiny** to explore employee attrition, identify workforce patterns, and understand the factors associated with employees leaving an organization.

## 📌 Project Overview

Employee attrition can affect organizational productivity, recruitment costs, and workforce stability. This project analyzes HR employee data to explore relationships between employee characteristics, job satisfaction, overtime, compensation, and attrition.

The project combines data cleaning, exploratory data analysis, statistical hypothesis testing, and interactive visualization to support data-driven HR insights.

## 🎯 Objectives

- Analyze employee attrition patterns and workforce characteristics.
- Identify factors associated with employee attrition.
- Explore relationships between job satisfaction, overtime, income, and work experience.
- Apply statistical tests to examine differences and associations in employee data.
- Develop an interactive dashboard using R Shiny.
- Explore how analytical findings can support employee retention strategies.

## 🛠️ Technologies Used

- **Programming Language:** R
- **Dashboard Development:** R Shiny
- **Data Analysis:** dplyr, tidyr
- **Data Visualization:** ggplot2
- **Statistical Analysis:** Chi-square tests, Wilcoxon tests, correlation analysis
- **Development Environment:** RStudio

*The package list should be adjusted to match the packages used in the final implementation.*

## 📊 Dataset

The project uses an IBM-style HR employee attrition dataset.

- **Initial dataset:** 1,470 employee records and 35 variables.
- **After data cleaning:** 1,470 observations and 31 variables.
- **Attrition — No:** 1,233 employees (83.88%).
- **Attrition — Yes:** 237 employees (16.12%).
- **Duplicate records:** None identified.

The dataset includes employee demographics, job roles, satisfaction levels, work experience, compensation, and other workplace-related attributes.

## 🔍 Key Analysis

### 1. Data Cleaning and Preparation
- Inspected the dataset structure and variable types.
- Checked for duplicate records and prepared variables for analysis.
- Converted relevant variables into appropriate data types.
- Prepared the cleaned dataset for statistical analysis and visualization.

### 2. Exploratory Data Analysis
- Examined overall employee attrition distribution.
- Analyzed attrition in relation to overtime and job satisfaction.
- Explored employee income, age, work experience, and tenure.
- Investigated relationships among selected workforce variables.

### 3. Statistical Analysis
- Used Chi-square tests to investigate associations between categorical variables and attrition.
- Used Wilcoxon tests to compare selected numerical variables across attrition groups.
- Performed correlation analysis to examine relationships between employee characteristics.

### 4. Interactive Dashboard
The R Shiny application is designed to present workforce analysis through interactive visualizations and accessible summaries of key findings.

*Document only the filters, charts, and dashboard interactions that are implemented in your actual application.*

## 📈 Key Findings

- Employee attrition accounts for approximately **16.12%** of the analyzed dataset.
- Overtime showed a statistically significant association with attrition (Chi-square test, p < 2.2 × 10⁻¹⁶).
- Job satisfaction also showed a statistically significant association with attrition (p = 0.0005563).
- Monthly income and total working years showed a strong positive correlation (r ≈ 0.77).
- Years at the company and years with the current manager showed a strong positive correlation (r ≈ 0.77).

These results describe patterns in the analyzed dataset and do not establish that any factor directly causes attrition.

## 🚀 Getting Started

### Prerequisites

Install [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/).

### Installation

1. Clone or download this repository.
2. Open the project in RStudio.
3. Install the required R packages.

```r
install.packages(c(
  "shiny",
  "dplyr",
  "tidyr",
  "ggplot2"
))
```

4. Place the dataset in the expected project directory.
5. Open the Shiny application entry-point file and run the application.

If the project uses a single `app.R` file, you can launch it with:

```r
shiny::runApp()
```

Make sure all required packages and file paths match the actual project structure before running the application.

## 📁 Project Structure

Example structure (adapt this to your actual files):

```text
Workplace-Attrition-Risk-Analysis/
├── app.R
├── data/
│   └── employee_attrition.csv
├── README.md
└── Workplace-Attrition-Risk-Analysis.Rproj
```

## 💡 Future Enhancements

- Explore and evaluate suitable machine-learning models for attrition prediction.
- Add more interactive dashboard filters and visualizations.
- Develop employee attrition risk indicators, if supported by a validated model.
- Improve dashboard usability and reporting.
- Evaluate model performance and fairness before any real-world HR use.

## 👩‍💻 Author

**Sruthi S K**  
B.Tech Information Technology  
Rajalakshmi Engineering College, Chennai

- GitHub: [Sruthi-sk06](https://github.com/Sruthi-sk06)
- LinkedIn: [Sruthi S K](https://www.linkedin.com/in/sksruthi46/)

## 📄 Disclaimer

This project is intended for educational and analytical purposes. Findings are based on the available dataset and should not be treated as definitive evidence of causation or used alone to make employment decisions.
