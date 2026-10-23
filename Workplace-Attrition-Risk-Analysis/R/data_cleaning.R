# ==========================================
# Workplace Attrition Risk Analysis
# Data Cleaning
# ==========================================

# Load required package
library(dplyr)

# Load dataset
data <- read.csv("data/WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Check dataset dimensions
dim(data)

# Remove irrelevant columns
data <- data %>%
  select(
    -EmployeeNumber,
    -EmployeeCount,
    -Over18,
    -StandardHours
  )

# Check missing values
sum(is.na(data))

# Check duplicate rows
sum(duplicated(data))

# Convert categorical variables to factors
data$Attrition <- as.factor(data$Attrition)
data$BusinessTravel <- as.factor(data$BusinessTravel)
data$Department <- as.factor(data$Department)
data$EducationField <- as.factor(data$EducationField)
data$Gender <- as.factor(data$Gender)
data$JobRole <- as.factor(data$JobRole)
data$MaritalStatus <- as.factor(data$MaritalStatus)
data$OverTime <- as.factor(data$OverTime)

# Check structure
str(data)

# Check target distribution
table(data$Attrition)