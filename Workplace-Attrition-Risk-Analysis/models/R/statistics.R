# ==========================================
# Workplace Attrition Risk Analysis
# Statistical Analysis
# ==========================================

library(dplyr)
# ------------------------------------------
# 1. Chi-Square Tests
# ------------------------------------------

categorical_vars <- c(
  "BusinessTravel",
  "Department",
  "EducationField",
  "Gender",
  "JobRole",
  "MaritalStatus",
  "EnvironmentSatisfaction",
  "JobInvolvement",
  "RelationshipSatisfaction",
  "WorkLifeBalance"
)

chi_results <- data.frame(
  Variable = character(),
  Chi_Square = numeric(),
  DF = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for (var in categorical_vars) {
  
  test_table <- table(data[[var]], data$Attrition)
  
  test <- chisq.test(test_table)
  
  chi_results <- rbind(
    chi_results,
    data.frame(
      Variable = var,
      Chi_Square = as.numeric(test$statistic),
      DF = as.numeric(test$parameter),
      P_Value = test$p.value
    )
  )
}

chi_results$Significant <- ifelse(
  chi_results$P_Value < 0.05,
  "Yes",
  "No"
)

chi_results <- chi_results[
  order(chi_results$P_Value),
]

print(chi_results)
# ------------------------------------------
# EducationField: Simulated Chi-Square Test
# ------------------------------------------

education_test <- chisq.test(
  table(data$EducationField, data$Attrition),
  simulate.p.value = TRUE,
  B = 10000
)

education_test
# ------------------------------------------
# Save Chi-Square Results
# ------------------------------------------

write.csv(
  chi_results,
  "results/chi_square_results.csv",
  row.names = FALSE
)
# ------------------------------------------
# 2. Wilcoxon Rank-Sum Tests
# ------------------------------------------

numeric_vars <- c(
  "Age",
  "DailyRate",
  "DistanceFromHome",
  "HourlyRate",
  "MonthlyIncome",
  "MonthlyRate",
  "NumCompaniesWorked",
  "PercentSalaryHike",
  "PerformanceRating",
  "StockOptionLevel",
  "TotalWorkingYears",
  "TrainingTimesLastYear",
  "YearsAtCompany",
  "YearsInCurrentRole",
  "YearsSinceLastPromotion",
  "YearsWithCurrManager"
)

wilcox_results <- data.frame(
  Variable = character(),
  W = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for (var in numeric_vars) {
  
  test <- wilcox.test(
    data[[var]] ~ data$Attrition,
    exact = FALSE
  )
  
  wilcox_results <- rbind(
    wilcox_results,
    data.frame(
      Variable = var,
      W = as.numeric(test$statistic),
      P_Value = test$p.value
    )
  )
}

wilcox_results$Significant <- ifelse(
  wilcox_results$P_Value < 0.05,
  "Yes",
  "No"
)

wilcox_results <- wilcox_results[
  order(wilcox_results$P_Value),
]

print(wilcox_results)
# ------------------------------------------
# Save Wilcoxon Results
# ------------------------------------------

write.csv(
  wilcox_results,
  "results/wilcoxon_results.csv",
  row.names = FALSE
)
# ------------------------------------------
# 3. Correlation Analysis
# ------------------------------------------

correlation_matrix <- cor(
  data[numeric_vars],
  use = "complete.obs"
)

print(round(correlation_matrix, 2))