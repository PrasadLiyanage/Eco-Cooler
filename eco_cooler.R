
#========================================================================================
# Evaluating the Effectiveness of the ‘Eco-Cooler’ for Passive Home Cooling
# 
# Aditi Bunker¹, Karin Lundgren Kownacki², Sudipa Sarker³, Rahmatul Bari⁴,
# Malabika Sarker⁴¹, Jonathan J. Buonocore⁵, Pascal Geldsetzer¹⁶, Johan Revstedt⁷*, Till Bärnighausen¹⁸⁹***
# 
# ¹ Heidelberg Institute of Global Health (HIGH), Heidelberg University, Heidelberg, Germany
# ² Swedish Meteorological and Hydrological Institute, Norrköping, Sweden
# ³ The Business School, Edinburgh Napier University, Edinburgh, UK
# ⁴ James P Grant School of Public Health, BRAC University, Dhaka, Bangladesh
# ⁵ School of Public Health, Boston University, Boston, MA, USA
# ⁶ Division of Primary Care and Population Health, Department of Medicine, Stanford University, Stanford, CA, USA
# ⁷ Department of Energy Sciences, Lund University, Lund, Sweden
# ¹⁸ Africa Health Research Institute (AHRI), KwaZulu-Natal, South Africa
# ⁹ Harvard Center for Population and Development Studies, Harvard T.H. Chan School of Public Health, Boston, USA
# 
# *These authors contributed equally to this work
# 
# Corresponding author:
# Aditi Bunker
# Heidelberg Institute of Global Health (HIGH)
# Heidelberg University
# Im Neuenheimer Feld 130.3,
# 69120 Heidelberg
# Germany
# 
# E-mail: aditi.bunker@uni-heidelberg.de

#==============================================================================
# # Table1
#==============================================================================
# Load  libraries
library(readxl)   # For reading Excel files
library(dplyr)    # For data manipulation
library(broom)    # For tidying up model outputs
library(purrr)    # For functional programming

# Define file path and load the data
file_path <- "~/Downloads/Eco_cooler/full_tables_and_regression_files/1_to_9.xlsx"
data <- read_excel(file_path, sheet = 1)

# Relevel the 'Supp' variable to make "Before" the reference level
data$Supp <- factor(data$Supp, levels = c("Before", "After"))

# Perform linear regression for each combination of 'Run' and 'Intensity'
results <- data %>%
  group_by(Run, Intensity) %>%
  do(
    temperature_lm = lm(Temperature ~ Supp, data = .),
    humidity_lm = lm(Humidity ~ Supp, data = .)
  ) %>%
  summarise(
    Run = first(Run),
    Intensity = first(Intensity),
    temperature_summary = list(tidy(temperature_lm)),
    humidity_summary = list(tidy(humidity_lm)),
    temperature_confint = list(confint(temperature_lm)),  # Extract confidence intervals as matrices
    humidity_confint = list(confint(humidity_lm))         # Extract confidence intervals as matrices
  ) %>%
  mutate(
    # Extract estimates and p-values for 'SuppAfter'
    temperature_estimate = map_dbl(temperature_summary, ~ .x$estimate[.x$term == "SuppAfter"]),
    temperature_pvalue = map_dbl(temperature_summary, ~ .x$p.value[.x$term == "SuppAfter"]),
    humidity_estimate = map_dbl(humidity_summary, ~ .x$estimate[.x$term == "SuppAfter"]),
    humidity_pvalue = map_dbl(humidity_summary, ~ .x$p.value[.x$term == "SuppAfter"]),
    # Extract confidence interval bounds
    conf.low_temp = map_dbl(temperature_confint, ~ .x["SuppAfter", 1]),
    conf.high_temp = map_dbl(temperature_confint, ~ .x["SuppAfter", 2]),
    conf.low_hum = map_dbl(humidity_confint, ~ .x["SuppAfter", 1]),
    conf.high_hum = map_dbl(humidity_confint, ~ .x["SuppAfter", 2])
  )

# Extract and annotate temperature results
temperature_results <- results %>%
  select(
    Run, Intensity,
    estimate_temp = temperature_estimate,
    conf.low_temp,
    conf.high_temp,
    p.value_temp = temperature_pvalue
  ) %>%
  mutate(significance_temp = case_when(
    p.value_temp < 0.001 ~ "***",
    p.value_temp < 0.01  ~ "**",
    p.value_temp < 0.05  ~ "*",
    TRUE ~ ""
  )) %>%
  # Reorder the Intensity levels and arrange by Run and Intensity
  mutate(Intensity = factor(Intensity, levels = c("High", "Medium", "Low"))) %>%
  arrange(Run, Intensity)

# Display temperature results
print("Temperature Results (Reordered):")
print(temperature_results)

# Extract and annotate humidity results
humidity_results <- results %>%
  select(
    Run, Intensity,
    estimate_hum = humidity_estimate,
    conf.low_hum,
    conf.high_hum,
    p.value_hum = humidity_pvalue
  ) %>%
  mutate(significance_hum = case_when(
    p.value_hum < 0.001 ~ "***",
    p.value_hum < 0.01  ~ "**",
    p.value_hum < 0.05  ~ "*",
    TRUE ~ ""
  )) %>%
  # Reorder the Intensity levels and arrange by Run and Intensity
  mutate(Intensity = factor(Intensity, levels = c("High", "Medium", "Low"))) %>%
  arrange(Run, Intensity)

# Display humidity results
print("Humidity Results (Reordered):")
print(humidity_results)

#============================================
# Table 2: Short tests
#=============================================

# Load necessary libraries
library(dplyr)
library(broom)   # For tidy() function
library(purrr)   # For map_dbl() function
library(readr)   # For read_csv() function

# Define the directory containing the CSV files
directory_path <- "~/Downloads/Eco_cooler/full_tables_and_regression_files/individual_run" # Change to the correct path if necessary

# Function to read and label data from CSV files
read_and_label_data <- function(file_path, run_number, intensity_label, design_number) {
  data <- read_csv(file_path)
  data <- data %>%
    mutate(
      Run = run_number,
      Intensity = intensity_label,
      Design = design_number
    )
  return(data)
}

# Define file paths and parameters
file_info <- list(
  # Design 1
  list(files = c("run1_high.csv", "run1_medium.csv", "run1_low.csv",
                 "run5_high.csv", "run5_medium.csv", "run5_low.csv"),
       runs = c(1, 1, 1, 5, 5, 5),
       intensities = c("High", "Medium", "Low", "High", "Medium", "Low"),
       design = 1),
  
  # Design 2
  list(files = c("run10_high.csv", "run10_medium.csv", "run10_low.csv",
                 "run11_high.csv", "run11_medium.csv", "run11_low.csv"),
       runs = c(10, 10, 10, 11, 11, 11),
       intensities = c("High", "Medium", "Low", "High", "Medium", "Low"),
       design = 2),
  
  # Design 3
  list(files = c("run12_high.csv", "run12_medium.csv", "run12_low.csv",
                 "run13_high.csv", "run13_medium.csv", "run13_low.csv"),
       runs = c(12, 12, 12, 13, 13, 13),
       intensities = c("High", "Medium", "Low", "High", "Medium", "Low"),
       design = 3),
  
  # Design 4
  list(files = c("run14_high.csv", "run14_medium.csv", "run14_low.csv",
                 "run15_high.csv", "run15_medium.csv", "run15_low.csv"),
       runs = c(14, 14, 14, 15, 15, 15),
       intensities = c("High", "Medium", "Low", "High", "Medium", "Low"),
       design = 4),
  
  # Design 5
  list(files = c("run16_high.csv", "run16_medium.csv", "run16_low.csv",
                 "run17_high.csv", "run17_medium.csv", "run17_low.csv"),
       runs = c(16, 16, 16, 17, 17, 17),
       intensities = c("High", "Medium", "Low", "High", "Medium", "Low"),
       design = 5),
  
  # Design 6
  list(files = c("run18_high.csv", "run18_medium.csv", "run18_low.csv",
                 "run19_high.csv", "run19_medium.csv", "run19_low.csv"),
       runs = c(18, 18, 18, 19, 19, 19),
       intensities = c("High", "Medium", "Low", "High", "Medium", "Low"),
       design = 6)
)

# Combine data from all specified files
combined_data <- bind_rows(lapply(file_info, function(info) {
  data_list <- mapply(read_and_label_data,
                      file_path = paste0(directory_path, "/", info$files),
                      run_number = info$runs,
                      intensity_label = info$intensities,
                      design_number = info$design,
                      SIMPLIFY = FALSE)
  bind_rows(data_list)
}))

# Display the combined data structure for verification
str(combined_data)

# Relevel the 'Supp' variable to make "Before" the reference level
combined_data$Supp <- factor(combined_data$State, levels = c("Before", "After"))

# Initialize an empty dataframe to store results
table_2_results <- data.frame()

# Function to perform analysis for a given design and store results in a dataframe
perform_analysis_for_design <- function(data, design_number) {
  # Filter data for the specified design
  design_data <- data %>%
    filter(Design == design_number)
  
  # Perform linear regression for each combination of 'Run' and 'Intensity'
  results <- design_data %>%
    group_by(Run, Intensity) %>%
    do(
      temperature_lm = lm(Temperature ~ Supp, data = .),
      humidity_lm = lm(Humidity ~ Supp, data = .)
    ) %>%
    summarise(
      Run = first(Run),
      Intensity = first(Intensity),
      temperature_summary = list(tidy(temperature_lm)),
      humidity_summary = list(tidy(humidity_lm)),
      temperature_confint = list(confint(temperature_lm)),  # Extract confidence intervals
      humidity_confint = list(confint(humidity_lm))         # Extract confidence intervals
    ) %>%
    mutate(
      # Extract estimates and p-values for 'SuppAfter'
      temperature_estimate = map_dbl(temperature_summary, ~ .x$estimate[.x$term == "SuppAfter"]),
      temperature_pvalue = map_dbl(temperature_summary, ~ .x$p.value[.x$term == "SuppAfter"]),
      humidity_estimate = map_dbl(humidity_summary, ~ .x$estimate[.x$term == "SuppAfter"]),
      humidity_pvalue = map_dbl(humidity_summary, ~ .x$p.value[.x$term == "SuppAfter"]),
      # Extract confidence interval bounds
      conf.low_temp = map_dbl(temperature_confint, ~ .x["SuppAfter", 1]),
      conf.high_temp = map_dbl(temperature_confint, ~ .x["SuppAfter", 2]),
      conf.low_hum = map_dbl(humidity_confint, ~ .x["SuppAfter", 1]),
      conf.high_hum = map_dbl(humidity_confint, ~ .x["SuppAfter", 2])
    )
  
  # Extract and annotate temperature results
  temperature_results <- results %>%
    select(
      Run, Intensity,
      estimate_temp = temperature_estimate,
      conf.low_temp,
      conf.high_temp,
      p.value_temp = temperature_pvalue
    ) %>%
    mutate(significance_temp = case_when(
      p.value_temp < 0.001 ~ "***",
      p.value_temp < 0.01  ~ "**",
      p.value_temp < 0.05  ~ "*",
      TRUE ~ ""
    )) %>%
    mutate(Design = design_number) %>%
    # Reorder the Intensity levels and arrange by Run and Intensity
    mutate(Intensity = factor(Intensity, levels = c("High", "Medium", "Low"))) %>%
    arrange(Run, Intensity)
  # Display temperature results
  print(paste("Temperature Results for Design", design_number, "(Short Tests):"))
  print(temperature_results)
  
  # Extract and annotate humidity results
  humidity_results <- results %>%
    select(
      Run, Intensity,
      estimate_hum = humidity_estimate,
      conf.low_hum,
      conf.high_hum,
      p.value_hum = humidity_pvalue
    ) %>%
    mutate(significance_hum = case_when(
      p.value_hum < 0.001 ~ "***",
      p.value_hum < 0.01  ~ "**",
      p.value_hum < 0.05  ~ "*",
      TRUE ~ ""
    )) %>%
    mutate(Design = design_number) %>%
    # Reorder the Intensity levels and arrange by Run and Intensity
    mutate(Intensity = factor(Intensity, levels = c("High", "Medium", "Low"))) %>%
    arrange(Run, Intensity)
  # Display humidity results
  print(paste("Humidity Results for Design", design_number, "(Short Tests):"))
  print(humidity_results)
  # Combine temperature and humidity results into a single dataframe
  combined_results <- left_join(temperature_results, humidity_results, by = c("Run", "Intensity", "Design"))
  
  return(combined_results)
  
}

# Apply the analysis function to each design and store results in table_2_results
for (design in unique(combined_data$Design)) {
  design_results <- perform_analysis_for_design(combined_data, design)
  table_2_results <- bind_rows(table_2_results, design_results)
}

# Display the final results dataframe
print(table_2_results)
str(table_2_results)

#============================================
# Table 2: Long tests
#=============================================

# Load necessary libraries
library(dplyr)
library(broom)   # For tidy() function
library(purrr)   # For map_dbl() function
library(readr)   # For read_csv() function

# Define the directory containing the CSV files
directory_path <- "~/Downloads/Eco_cooler/full_tables_and_regression_files/individual_run" # Change to the correct path if necessary

# Function to read and label data from CSV files
read_and_label_data <- function(file_path, run_number, intensity_label, design_number) {
  data <- read_csv(file_path)
  data <- data %>%
    mutate(
      Run = run_number,
      Intensity = intensity_label,
      Design = design_number
    )
  return(data)
}

# Define file paths and parameters
file_info <- list(
  # Design 1
  list(files = c("run_d1.csv"),
       runs = c(20),
       intensities = c("High"),
       design = 1),
  # Design 2
  list(files = c("run_d2.csv"),
       runs = c(21),
       intensities = c("High"),
       design = 2),
  
  # Design 4
  list(files = c("run_d4.csv"),
       runs = c(22),
       intensities = c("High"),
       design = 4),
  
  # Design 5
  list(files = c("run_d5.csv"),
       runs = c(23),
       intensities = c("High"),
       design = 5),
  
  # Design 6
  list(files = c("run_d6.csv"),
       runs = c(24),
       intensities = c("High"),
       design = 6)
  
)

# Combine data from all specified files
combined_data <- bind_rows(lapply(file_info, function(info) {
  data_list <- mapply(read_and_label_data,
                      file_path = paste0(directory_path, "/", info$files),
                      run_number = info$runs,
                      intensity_label = info$intensities,
                      design_number = info$design,
                      SIMPLIFY = FALSE)
  bind_rows(data_list)
}))

# Display the combined data structure for verification
str(combined_data)

# Relevel the 'Supp' variable to make "Before" the reference level
combined_data$Supp <- factor(combined_data$State, levels = c("Before", "After"))

# Initialize an empty dataframe to store results
table_2_results <- data.frame()

# Function to perform analysis for a given design and store results in a dataframe
perform_analysis_for_design <- function(data, design_number) {
  # Filter data for the specified design
  design_data <- data %>%
    filter(Design == design_number)
  
  # Perform linear regression for each combination of 'Run' and 'Intensity'
  results <- design_data %>%
    group_by(Run, Intensity) %>%
    do(
      temperature_lm = lm(Temperature ~ Supp, data = .),
      humidity_lm = lm(Humidity ~ Supp, data = .)
    ) %>%
    summarise(
      Run = first(Run),
      Intensity = first(Intensity),
      temperature_summary = list(tidy(temperature_lm)),
      humidity_summary = list(tidy(humidity_lm)),
      temperature_confint = list(confint(temperature_lm)),  # Extract confidence intervals
      humidity_confint = list(confint(humidity_lm))         # Extract confidence intervals
    ) %>%
    mutate(
      # Extract estimates and p-values for 'SuppAfter'
      temperature_estimate = map_dbl(temperature_summary, ~ .x$estimate[.x$term == "SuppAfter"]),
      temperature_pvalue = map_dbl(temperature_summary, ~ .x$p.value[.x$term == "SuppAfter"]),
      humidity_estimate = map_dbl(humidity_summary, ~ .x$estimate[.x$term == "SuppAfter"]),
      humidity_pvalue = map_dbl(humidity_summary, ~ .x$p.value[.x$term == "SuppAfter"]),
      # Extract confidence interval bounds
      conf.low_temp = map_dbl(temperature_confint, ~ .x["SuppAfter", 1]),
      conf.high_temp = map_dbl(temperature_confint, ~ .x["SuppAfter", 2]),
      conf.low_hum = map_dbl(humidity_confint, ~ .x["SuppAfter", 1]),
      conf.high_hum = map_dbl(humidity_confint, ~ .x["SuppAfter", 2])
    )
  
  # Extract and annotate temperature results
  temperature_results <- results %>%
    select(
      Run, Intensity,
      estimate_temp = temperature_estimate,
      conf.low_temp,
      conf.high_temp,
      p.value_temp = temperature_pvalue
    ) %>%
    mutate(significance_temp = case_when(
      p.value_temp < 0.001 ~ "***",
      p.value_temp < 0.01  ~ "**",
      p.value_temp < 0.05  ~ "*",
      TRUE ~ ""
    )) %>%
    mutate(Design = design_number) %>%
    # Reorder the Intensity levels and arrange by Run and Intensity
    mutate(Intensity = factor(Intensity, levels = c("High"))) %>%
    arrange(Run, Intensity)
  # Display temperature results
  print(paste("Temperature Results for Design", design_number, "(LongTests):"))
  print(temperature_results)
  
  # Extract and annotate humidity results
  humidity_results <- results %>%
    select(
      Run, Intensity,
      estimate_hum = humidity_estimate,
      conf.low_hum,
      conf.high_hum,
      p.value_hum = humidity_pvalue
    ) %>%
    mutate(significance_hum = case_when(
      p.value_hum < 0.001 ~ "***",
      p.value_hum < 0.01  ~ "**",
      p.value_hum < 0.05  ~ "*",
      TRUE ~ ""
    )) %>%
    mutate(Design = design_number) %>%
    # Reorder the Intensity levels and arrange by Run and Intensity
    mutate(Intensity = factor(Intensity, levels = c("High"))) %>%
    arrange(Run, Intensity)
  # Display humidity results
  print(paste("Humidity Results for Design", design_number, "(Long Tests):"))
  print(humidity_results)
  # Combine temperature and humidity results into a single dataframe
  combined_results <- left_join(temperature_results, humidity_results, by = c("Run", "Intensity", "Design"))
  
  return(combined_results)
  
}

# Apply the analysis function to each design and store results in table_2_results
for (design in unique(combined_data$Design)) {
  design_results <- perform_analysis_for_design(combined_data, design)
  table_2_results <- bind_rows(table_2_results, design_results)
}

# Display the final results dataframe
print(table_2_results)
str(table_2_results)

