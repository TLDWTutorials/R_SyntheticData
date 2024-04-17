# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
if (!requireNamespace("charlatan", quietly = TRUE)) {
  install.packages("charlatan")
}

library(dplyr)
library(lubridate)
library(charlatan)

# Set seed for reproducibility
set.seed(123)

# Specify the number of entries (patients)
n <- 1000

# Generate patient data
names <- replicate(n, ch_name())
ages <- sample(18:100, n, replace = TRUE)
race <- sample(c("White", "Black", "Asian", "American Indian or Alaska Native", 
                 "Native Hawaiian or Other Pacific Islander", "Other"), 
               n, replace = TRUE, prob = c(0.7, 0.15, 0.05, 0.04, 0.03, 0.03))
ethnicity <- sample(c("Hispanic", "Non-Hispanic"), n, replace = TRUE, prob = c(0.2, 0.8))
dob <- as.Date("2024-01-01") - years(ages) - days(sample(1:365, n, replace = TRUE))
A1C_levels <- round(runif(n, min = 6.5, max = 10), 1)  # A1C levels

# Initialize the dataframe
synthetic_data <- data.frame(Name = names, Age = ages, Race = race, Ethnicity = ethnicity, 
                             DateOfBirth = dob, A1C = A1C_levels)

# Metformin Dosage Calculation based on A1C levels
calculate_metformin_dosage <- function(a1c) {
  base_dose <- 500  # Starting dose
  if (a1c <= 7) {
    return(base_dose)
  } else {
    additional_dose <- (a1c - 7) * 2 * 250  # Adjust the multiplier for realism
    return(min(base_dose + additional_dose, 2000))  # Max dose of 2000
  }
}

synthetic_data$Metformin_Dosage_MG <- sapply(synthetic_data$A1C, calculate_metformin_dosage)
synthetic_data$Metformin_Type <- ifelse(synthetic_data$A1C <= 8, "Metformin IR", "Metformin XR")

# Blood Pressure
n_high_bp <- round(n * 0.15)
synthetic_data$Systolic_BP <- c(sample(90:140, n - n_high_bp, replace = TRUE), sample(141:180, n_high_bp, replace = TRUE))
synthetic_data$Diastolic_BP <- c(sample(60:90, n - n_high_bp, replace = TRUE), sample(91:120, n_high_bp, replace = TRUE))

# Shuffle the BP readings
synthetic_data <- synthetic_data[sample(nrow(synthetic_data)), ]

# ICD-10 Codes
icd_codes <- read.csv("ICDCodeSet.csv")
e11_codes <- icd_codes %>% filter(grepl("^E11", ICDCode))
specific_codes <- c("E1165", "E119", "E118", "E1169")
specific_codes_df <- e11_codes %>% filter(ICDCode %in% specific_codes)
other_e11_codes_df <- e11_codes %>% filter(!ICDCode %in% specific_codes)
final_sample <- rbind(
  specific_codes_df[sample(nrow(specific_codes_df), round(n * 0.9), replace = TRUE), ],
  other_e11_codes_df[sample(nrow(other_e11_codes_df), n - round(n * 0.9), replace = TRUE), ]
)

# Add ICDCode and Description
synthetic_data$ICDCode <- final_sample$ICDCode[1:n]
synthetic_data$ICDDescription <- final_sample$Description[1:n]

# Export the dataset to a CSV file
write.csv(synthetic_data, "synthetic_patient_data.csv", row.names = FALSE)

# Display the first few rows of the synthetic dataset
head(synthetic_data)
