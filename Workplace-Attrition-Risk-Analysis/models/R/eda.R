# ==========================================
# Workplace Attrition Risk Analysis
# Exploratory Data Analysis
# ==========================================

# Load required package
library(ggplot2)

# ------------------------------------------
# 1. Employee Attrition Distribution
# ------------------------------------------

ggplot(data, aes(x = Attrition)) +
  geom_bar() +
  labs(
    title = "Employee Attrition Distribution",
    x = "Attrition",
    y = "Number of Employees"
  )

# ------------------------------------------
# 2. Overtime and Employee Attrition
# ------------------------------------------

ggplot(data, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(
    title = "Overtime and Employee Attrition",
    x = "Overtime",
    y = "Proportion"
  )

# ------------------------------------------
# 3. Job Satisfaction and Employee Attrition
# ------------------------------------------

ggplot(data, aes(x = JobSatisfaction, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(
    title = "Job Satisfaction and Employee Attrition",
    x = "Job Satisfaction",
    y = "Proportion"
  )

# ------------------------------------------
# 4. Monthly Income and Employee Attrition
# ------------------------------------------

ggplot(data, aes(x = Attrition, y = MonthlyIncome)) +
  geom_boxplot() +
  labs(
    title = "Monthly Income vs Employee Attrition",
    x = "Attrition",
    y = "Monthly Income"
  )

# ------------------------------------------
# 5. Department and Employee Attrition
# ------------------------------------------

ggplot(data, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Employee Attrition by Department",
    x = "Department",
    y = "Number of Employees"
  )