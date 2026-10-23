# ==========================================
# Workplace Attrition Risk Analysis
# Data Cleaning
# ==========================================

# Load required packages
library(readxl)
library(dplyr)

# ------------------------------------------
# 1. Load dataset
# ------------------------------------------

data <- read_excel(
  "data/WA_Fn-UseC_-HR-Employee-Attrition.csv.xlsx"
)

# Check dimensions
dim(data)

# ------------------------------------------
# 2. Remove irrelevant columns
# ------------------------------------------

data <- data %>%
  select(
    -EmployeeNumber,
    -EmployeeCount,
    -Over18,
    -StandardHours
  )

# Check dimensions after removing columns
dim(data)

# ------------------------------------------
# 3. Check missing values
# ------------------------------------------

sum(is.na(data))

# ------------------------------------------
# 4. Check duplicate rows
# ------------------------------------------

sum(duplicated(data))

# ------------------------------------------
# 5. Convert categorical variables to factors
# ------------------------------------------

data$Attrition <- as.factor(data$Attrition)
data$BusinessTravel <- as.factor(data$BusinessTravel)
data$Department <- as.factor(data$Department)
data$EducationField <- as.factor(data$EducationField)
data$Gender <- as.factor(data$Gender)
data$JobRole <- as.factor(data$JobRole)
data$MaritalStatus <- as.factor(data$MaritalStatus)
data$OverTime <- as.factor(data$OverTime)

# ------------------------------------------
# 6. Check structure
# ------------------------------------------

str(data)

# ------------------------------------------
# 7. Check target variable
# ------------------------------------------

table(data$Attrition)