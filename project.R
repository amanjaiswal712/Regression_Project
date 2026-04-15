# ---
# NSSO 77th Round - Data Preparation Script for Uttar Pradesh (Schedule 33.1)
#
# This script loads, cleans, aggregates, and merges data from four separate
# NSSO 77th Round files to create a single, household-level dataset for Uttar Pradesh (State Code 9).
#
# ---

# 1. LOAD LIBRARIES
# -------------------------------------------------------------------------
library(dplyr)    # For data manipulation
library(readr)    # For reading text files
library(stringr)  # For string manipulation
library(purrr)    # For 'reduce' to merge multiple files


# 2. DEFINE CLEANING FUNCTION (User's Simplified Version)
# -------------------------------------------------------------------------
fix_and_convert_robust <- function(df) {
  df <- df %>%
    # Remove all spaces from all columns
    mutate(across(everything(), ~ str_remove_all(., " "))) %>%
    # Attempt to convert every column to numeric. Non-numeric values will become NA.
    mutate(across(everything(), as.numeric))
  return(df)
}

# 3. DEFINE COLUMN NAMES (No Change)
# ... (Column definitions remain the same) ...

# Block 3: Demographic and other particulars of household members (Person Level)
my_column_names1 <- c(
  "v1_HHLNo", "v2_Schd", "v3_FSU_SNo", "v4_Round", "v5_Schedule",
  "v6_Sample", "v7_Sector", "v8_NSS_Region", "v9_State",
  "v10_State_District", "v11_District", "v12_Stratum", "v13_SubStrat",
  "v14_SubRound", "v15_FODSubRegion", "v16_SSS", "v17_hh_no",
  "v18_visit_No", "v19_Level", "v20_b3q1",
  "v21_b3q3", "v22_b3q4",
  "v23_b3q5", 
  "v24_b3q6", # 'highest level of education' (Code, higher value = higher education)
  "v25_b3q7", "v26_b3q8",
  "v27_b3q9", "v28_b3q11_Status", "v29_b3q11_NIC",
  "v30_b3q12", 
  "v31_b3q13", 
  "v32_b3q14", 
  "v33_b3q15", "v34_NSC", "v35_MLT"
)

# Block 4: Household characteristics (Household Level)
my_column_names2 <- c(
  "v1_HHLNo", "v2_Schd", "v3_FSU_SNo", "v4_Round", "v5_Schedule",
  "v6_Sample", "v7_Sector", "v8_NSS_Region", "v9_State",
  "v10_State_District", "v11_District", "v12_Stratum", "v13_SubStrat",
  "v14_SubRound", "v15_FODSubRegion", "v16_SSS", "v17_hh_no",
  "v18_visit_No", "v19_Level", "v20_b4q1",  # b4q1 = household size
  "v21_b4q2",  # b4q2 = religion
  "v22_b4q3",  # b4q3 = social group
  "v23_b4q4",  # b4q4 = household classification (Livelihood Type)
  "v24_b4q5",  "v25_b4q6",  "v26_b4q7",  "v27_b4q8",
  "v28_b4q9",  # b4q9 = usual monthly consumer expenditure (Total)
  "v29_b4q10", "v30_b4q11", "v31_b4q12", "v32_b4q13",
  "v33_b4q14", 
  "v34_b4q15", "v35_b4q16",
  "v36_b4q17", 
  "v37_b4q18", "v38_b4q19", "v39_b4q20", "v40_b4q21",
  "v41_NSC", "v42_MLT"
)

# Block 8: Livestock, poultry, duckery, etc. (Item Level)
my_column_names3 <- c(
  "v1_HHLNo", "v2_Schd", "v3_FSU_SNo", "v4_Round", "v5_Schedule",
  "v6_Sample", "v7_Sector", "v8_NSS_Region", "v9_State",
  "v10_State_District", "v11_District", "v12_Stratum", "v13_SubStrat",
  "v14_SubRound", "v15_FODSubRegion", "v16_SSS", "v17_hh_no",
  "v18_visit_No", "v19_Level", "v20_b8q1", # b8q1 = srl. no. of livestock item
  "v21_b8q3",
  "v22_b8q4",
  "v23_b8q5", # b8q5 = total number of livestock for that item (Quantity)
  "v24_NSC", "v25_MLT"
)

# Block 5 (Visit 1): particulars of land of the household and its operation (Item Level)
my_column_names4 <- c(
  "v1_HHLNo", "v2_Schd", "v3_FSU_SNo", "v4_Round", "v5_Schedule",
  "v6_Sample", "v7_Sector", "v8_NSS_Region", "v9_State",
  "v10_State_District", "v11_District", "v12_Stratum", "v13_SubStrat",
  "v14_SubRound", "v15_FODSubRegion", "v16_SSS", "v17_hh_no",
  "v18_visit_No", "v19_Level", "v20_b51q1", # b5q1 = srl. no. of land category (10 is 'Total')
  "v21_b5q3", # b5q3 = area of land (in acres)
  "v22_b5q4", "v23_b5q5", "v24_b5q6", "v25_b5q7",
  "v26_b5q8", "v27_b5q9", "v28_b5q10", "v29_b5q11",
  "v30_b5q12", "v31_b5q13", "v32_b5q14", "v33_b5q15",
  "v34_b5q16","v35_b5q17","v36_b5q18","v37_b5q19",
  "v38_b5q20","v39_b5q21","v40_NSC", "v41_MLT"
)

# Define the common Primary Key for merging the household-level data
hh_key <- c("v3_FSU_SNo", "v16_SSS", "v17_hh_no")
STATE_CODE <- 9 # Uttar Pradesh

# 4. LOAD, CLEAN, AND AGGREGATE DATA (No Change)
# ... (Aggregation logic remains the same) ...
## --- 4.1: Block 4 (Household Details) ---
household_details_up <- read_delim(
  file = "Visit1  Level - 03 (Block 4) - demographic and other particulars of household members.txt",
  delim = "\t",
  col_names = my_column_names2,
  show_col_types = FALSE
) %>%
  fix_and_convert_robust() %>%
  filter(v9_State == STATE_CODE) %>%
  select(
    # Core Household Variables
    all_of(hh_key),
    household_size = v20_b4q1,
    religion = v21_b4q2,
    social_group = v22_b4q3,
    hh_classification = v23_b4q4,
    monthly_expenditure = v28_b4q9,
    # Multipliers (Essential for estimation)
    NSC = v41_NSC,
    MLT = v42_MLT
  )

## --- 4.2: Block 3 (Education & Income) - Person Level Aggregation ---
# Aggregated to household level to get maximum education.
education_income_up <- read_delim(
  file = "Visit1  Level - 02 (Block 3) - demographic and other particulars of household members.txt",
  delim = "\t",
  col_names = my_column_names1,
  show_col_types = FALSE
) %>%
  fix_and_convert_robust() %>%
  filter(v9_State == STATE_CODE) %>%
  group_by(across(all_of(hh_key))) %>%
  summarize(
    # Only keep the highest education variable
    max_edu = max(v24_b3q6, na.rm = TRUE),
    .groups = "drop"
  )


## --- 4.3: Block 5 (Land) ---
# Filter for srl. no. 10 ("total") to get total operational land area.
total_land_up_acres <- read_delim(
  file = "Visit1  Level - 04 (Block 5) - particulars of land of the household and its operation during the period July- December 2018.txt",
  delim = "\t",
  col_names = my_column_names4,
  show_col_types = FALSE
) %>%
  fix_and_convert_robust() %>%
  filter(v9_State == STATE_CODE) %>%
  filter(v20_b51q1 == 10) %>% # Filter for srl. no. 10 ("total")
  # v21_b5q3 is the total area of land in acres.
  mutate(total_land_acres = v21_b5q3) %>%
  select(all_of(hh_key), total_land_acres)


## --- 4.4: Block 8 (Livestock) - Item Level Aggregation ---
# Aggregated to household level to get total livestock counts.
livestock_summary_up <- read_delim(
  file = "Visit 1 Level 9 (Block 8) livestock, poultry, duckery, etc. owned by the household as on the date of survey.txt",
  delim = "\t",
  col_names = my_column_names3,
  show_col_types = FALSE
) %>%
  fix_and_convert_robust() %>%
  filter(v9_State == STATE_CODE) %>%
  group_by(across(all_of(hh_key))) %>%
  summarize(
    # In-milk cattle (srl. no. 1) + in-milk buffalo (srl. no. 4)
    in_milk_cattle_buffalo =
      sum(v23_b8q5[v20_b8q1 == 1], na.rm = TRUE) +
      sum(v23_b8q5[v20_b8q1 == 4], na.rm = TRUE),
    
    # Total cattle: srl. nos. 1, 2, 3
    total_cattle =
      sum(v23_b8q5[v20_b8q1 %in% c(1, 2, 3)], na.rm = TRUE),
    
    # Total buffalo: srl. nos. 4, 5, 6
    total_buffalo =
      sum(v23_b8q5[v20_b8q1 %in% c(4, 5, 6)], na.rm = TRUE),
    
    # Total livestock (srl. no. 11: total (items 1 to 10))
    total_livestock_count =
      sum(v23_b8q5[v20_b8q1 == 11], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(total_cattle_buffalo = total_cattle + total_buffalo) %>%
  select(
    all_of(hh_key),
    in_milk_cattle_buffalo,
    total_cattle,
    total_buffalo,
    total_cattle_buffalo,
    total_livestock_count
  )


# 5. MERGE DATASETS (No Change)
# ... (Merging logic remains the same) ...
# Create a list of all aggregated data frames to merge
data_list <- list(
  household_details_up,
  education_income_up,
  total_land_up_acres,
  livestock_summary_up
)

# Sequentially merge all data frames by the common household key (inner join to keep households present in all blocks)
final_dataset_up <- data_list %>%
  reduce(inner_join, by = hh_key)


# 6. FINAL OUTPUT (No Change)
# ... (Output and saving logic remains the same) ...
print("Part 1 Complete: Data loaded, aggregated, and merged.")

print(paste("Final Merged Dataset (Uttar Pradesh, State Code", STATE_CODE, ")"))
print(dim(final_dataset_up))
print(head(final_dataset_up))

# 7. SAVE FINAL DATA
# The data is saved as both an RDS file (R's native binary format) and a CSV file.
saveRDS(final_dataset_up, file = "final_data_up.rds")
write.csv(final_dataset_up, "final_data_up.csv", row.names = FALSE)








# Load the final dataset
final_dataset_up <- readRDS("final_data_up.rds")

# Cross-Tabulation 1: HH Classification vs. Social Group
# We use table() to get the raw counts for the contingency table.
cross_tab_1 <- table(
  HH_Classification = final_dataset_up$hh_classification,
  Social_Group = final_dataset_up$social_group
)

# Print the table of raw counts
print("Cross-Tabulation 1: Household Livelihood vs. Social Group (Raw Counts)")
print(cross_tab_1)

# Optionally, view as proportions of the total sample:
cross_tab_1_prop <- round(prop.table(cross_tab_1) * 100, 2)
print("Cross-Tabulation 1: Household Livelihood vs. Social Group (Percentage of Total)")
print(cross_tab_1_prop)




# Cross-Tabulation 2: Highest Education vs. HH Classification
cross_tab_2 <- table(
  Max_Education = final_dataset_up$max_edu,
  HH_Classification = final_dataset_up$hh_classification
)

# Print the table of raw counts
print("Cross-Tabulation 2: Highest Education Level vs. Household Livelihood (Raw Counts)")
print(cross_tab_2)

# To interpret education codes easily, you might want to group them (e.g., 'Primary', 'Secondary', 'Graduate').






# Create land holding categories (Landless, Small, Medium, Large)
# Using common cut-off points, e.g., 0.01, 2.5, 5, 10 acres.
# Note: This is an example categorization. The bins may need adjustment based on data distribution.
land_categories <- cut(
  final_dataset_up$total_land_acres,
  breaks = c(-Inf, 0.01, 2.5, 5, 10, Inf),
  labels = c("Landless", "Small (0-2.5)", "Medium (2.5-5)", "Semi-Large (5-10)", "Large (>10)"),
  right = TRUE,
  include.lowest = TRUE
)

# Add the new categorical column to the dataset for aggregation
final_dataset_up_cat <- final_dataset_up %>%
  mutate(land_category = land_categories)

# Cross-Tabulation 3: Mean Monthly Expenditure by Land Category
mean_exp_by_land <- final_dataset_up_cat %>%
  group_by(land_category) %>%
  summarize(
    N_Households = n(),
    Mean_Monthly_Expenditure = round(mean(monthly_expenditure, na.rm = TRUE), 2),
    .groups = "drop"
  )

print("Cross-Tabulation 3: Mean Monthly Expenditure by Land Holding Size (Acres)")
print(mean_exp_by_land)





# Cross-Tabulation 4: Mean Total Livestock Count by Social Group
mean_livestock_by_social <- final_dataset_up %>%
  group_by(social_group) %>%
  summarize(
    N_Households = n(),
    Mean_Total_Livestock = round(mean(total_livestock_count, na.rm = TRUE), 2),
    .groups = "drop"
  )

print("Cross-Tabulation 4: Mean Total Livestock Count by Social Group")
print(mean_livestock_by_social)









# ---
# NSSO 77th Round - Step 3: Create (Reduced) Dummy Variables for Regression
# ---

# 1. LOAD LIBRARIES
# -------------------------------------------------------------------------
library(dplyr)
library(readr)

# 2. LOAD YOUR CLEANED DATA
# -------------------------------------------------------------------------
# Assuming the clean, merged data is saved as 'final_data_up.rds' from the previous step.
data_cleaned <- readRDS("final_data_up.rds")

print("--- Data Loaded. Starting REDUCED dummy variable creation... ---")


# 3. CREATE DUMMY VARIABLES (NEW GROUPINGS) using case_when()
# -------------------------------------------------------------------------
data_for_regression <- data_cleaned %>%
  mutate(
    
    # --- 1. social_group (caste) ---
    # Reference: (9) Others
    # Dummies: (3) OBC, (1, 2) SC/ST
    social_group_OBC    = case_when(social_group == 3 ~ 1, TRUE ~ 0),
    social_group_SC_ST  = case_when(social_group %in% c(1, 2) ~ 1, TRUE ~ 0),
    
    
    # --- 2. religion ---
    # Reference: (1) Hindu
    # Dummies: (2) Muslim, (3,4,5,6,7,9) Other
    religion_Muslim     = case_when(religion == 2 ~ 1, TRUE ~ 0),
    religion_Other      = case_when(religion %in% c(3, 4, 5, 6, 7, 9) ~ 1, TRUE ~ 0),
    
    
    # --- 3. hh_classification (Household Livelihood) ---
    # Reference: (1) Crop Prod
    # Dummies: (2,3,5,7) Other Agri/Labour, (4,6,8,9) Non-Agri
    # Codes: 1(Crop), 2(Animal), 3(OtherAgri), 5(RegAgriW), 7(CasAgriL) -> Agri Related
    # Codes: 4(NonAgriEnt), 6(RegNonAgriW), 8(CasNonAgriL), 9(Others) -> Non-Agri
    hh_class_Agri_Related = case_when(hh_classification %in% c(2, 3, 5, 7) ~ 1, TRUE ~ 0),
    hh_class_NonAgri      = case_when(hh_classification %in% c(4, 6, 8, 9) ~ 1, TRUE ~ 0),
    
    
    # --- 4. max_edu (Max Education in Household) ---
    # Reference: (1,2) Not literate / Below Primary
    # Dummies: (3,4) Primary / Upper Primary, (5,6,7) Secondary / HS / Sec. Diploma, (8,10,11,12) Post-Secondary
    edu_Primary         = case_when(max_edu %in% c(3, 4) ~ 1, TRUE ~ 0),
    edu_Secondary       = case_when(max_edu %in% c(5, 6, 7) ~ 1, TRUE ~ 0),
    edu_PostSecondary   = case_when(max_edu %in% c(8, 10, 11, 12) ~ 1, TRUE ~ 0)
    
  ) %>%
  
  # 4. RETAIN CLUSTER & PRIMARY KEY COLUMNS
  # -------------------------------------------------------------------------
select(
  # *** RETAIN ALL THREE PRIMARY/CLUSTER KEYS ***
  v3_FSU_SNo, # CRITICAL for Clustered Standard Errors
  v16_SSS,    
  v17_hh_no,
  
  # Keep all other variables (quantitative and new dummies)
  everything(), 
  
  # Explicitly remove the original categorical columns that were converted to dummies
  -social_group,
  -religion,
  -hh_classification,
  -max_edu 
)
  
 

print("Step 3 & 4 Complete: NEW reduced dummy variables created.")


# 5. FINAL CHECK & SAVE
# -------------------------------------------------------------------------
print("--- Final Column Names ---")
print(names(data_for_regression))

# Save the final data set for regression analysis
write.csv(data_for_regression, "final_data_for_regression.csv", row.names = FALSE)
saveRDS(data_for_regression, "final_data_for_regression.rds")

print("-----------------------------------------------------------------")
print("SUCCESS: Your data is now 100% ready for regression.")
print("Saved as 'final_data_for_regression.csv'.")
print("-----------------------------------------------------------------")













library(dplyr)
library(readr)

# 1. Define the correct, logical order for the levels
edu_levels <- c(
  "Not Literate/Below Primary (Ref)",
  "Primary/Upper Primary (G2)",
  "Secondary/HS (G3)",
  "Post-Secondary (G4)"
)

social_levels <- c(
  "SC/ST",
  "OBC",
  "Others (Ref)"
)

# Load the data
data_for_regression <- readRDS("final_data_for_regression.rds")


# --- Helper function to create ordered columns ---
create_ordered_groups <- data_for_regression %>%
  mutate(
    # Create and Order EDUCATION GROUP
    Education_Group_Cat = case_when(
      edu_Primary == 1 ~ "Primary/Upper Primary (G2)",
      edu_Secondary == 1 ~ "Secondary/HS (G3)",
      edu_PostSecondary == 1 ~ "Post-Secondary (G4)",
      TRUE ~ "Not Literate/Below Primary (Ref)"
    ),
    Education_Group_Cat = factor(Education_Group_Cat, levels = edu_levels, ordered = TRUE),
    
    # Create and Order SOCIAL GROUP
    Social_Group_Cat = case_when(
      social_group_OBC == 1 ~ "OBC",
      social_group_SC_ST == 1 ~ "SC/ST",
      TRUE ~ "Others (Ref)"
    ),
    Social_Group_Cat = factor(Social_Group_Cat, levels = social_levels, ordered = TRUE)
  )


## 2.1. Cross-Tabulation 1: Livelihood Mix by Max Education Level (Ordered)
print("--- Cross-Tabulation 1: Livelihood Mix by Max Education Level (ORDERED) ---")
cross_tab_1_ordered <- create_ordered_groups %>%
  group_by(Education_Group_Cat) %>%
  summarize(
    `Crop Prod (Ref)` = sum(hh_class_Agri_Related == 0 & hh_class_NonAgri == 0),
    `Other Agri/Labour` = sum(hh_class_Agri_Related == 1),
    `Non-Agri` = sum(hh_class_NonAgri == 1),
    `Total HHs in Edu Group` = n(),
    .groups = 'drop'
  )
print(cross_tab_1_ordered)


## 2.2. Cross-Tabulation 2: Mean Land Area (Acres) by Social Group (Ordered)
print("--- Cross-Tabulation 2: Mean Land Area (Acres) by Social Group (ORDERED) ---")
cross_tab_2_ordered <- create_ordered_groups %>%
  group_by(Social_Group_Cat) %>%
  summarize(
    N_Households = n(),
    Mean_Total_Land_Acres = round(mean(total_land_acres, na.rm = TRUE), 2),
    .groups = 'drop'
  )
print(cross_tab_2_ordered)


## 2.3. Cross-Tabulation 3: Mean Monthly Expenditure (Rs.) by Max Education Group (Ordered)
print("--- Cross-Tabulation 3: Mean Monthly Expenditure (Rs.) by Max Education Group (ORDERED) ---")
cross_tab_3_ordered <- create_ordered_groups %>%
  group_by(Education_Group_Cat) %>%
  summarize(
    N_Households = n(),
    Mean_Monthly_Expenditure_Rs = round(mean(monthly_expenditure, na.rm = TRUE), 2),
    .groups = 'drop'
  )
print(cross_tab_3_ordered)










library(ggplot2)
library(dplyr)
library(readr)

# --- 1. LOAD AND PREPARE DATA ---
# Assuming 'data_for_regression' is the final file from the previous step.
# NOTE: It's important to reload the saved .rds file to ensure the FSU/HH keys are present.
data_analysis_ready <- readRDS("final_data_for_regression.rds") %>%
  # Filter out rows that would cause log(0)
  filter(
    monthly_expenditure > 0, 
    household_size > 0, 
    total_land_acres >= 0, # Land can be zero, but we need to handle log transformation later
    total_livestock_count >= 0,
    in_milk_cattle_buffalo >= 0
  ) %>%
  # Create the NEW Dependent Variable: Log(MPCE)
  mutate(
    mpce = monthly_expenditure / household_size,
    log_mpce = log(mpce)
  )

# --- 2. SAVE ANALYSIS READY FILE ---
# Renaming and saving the final file ready for regression.
saveRDS(data_analysis_ready, file = "data_analysis_ready.rds")
write.csv(data_analysis_ready, "data_analysis_ready.csv", row.names = FALSE)
print("✅ New column log_mpce added. File saved as 'data_analysis_ready.rds'.")


# --- 3. PLOTTING FUNCTION ---
# Function to safely create log-transformed IV for plotting only where X > 0
create_plots <- function(data, x_var_raw, x_var_name, plot_start_index) {
  
  # Filter data to exclude zeros for IV log transformation
  data_filtered <- data %>%
    filter(!!sym(x_var_raw) > 0) %>%
    mutate(log_x = log(!!sym(x_var_raw)))
  
  # Define plot range for better visual assessment
  if (x_var_raw == "total_land_acres") {
    x_limit <- c(0, 20)
  } else if (x_var_raw == "total_livestock_count") {
    x_limit <- c(0, 50)
  } else if (x_var_raw == "in_milk_cattle_buffalo") {
    x_limit <- c(0, 10)
  } else if (x_var_raw == "household_size") {
    x_limit <- c(1, 15)
  } else {
    x_limit <- NULL
  }
  
  plots <- list()
  
  # Plot A: Raw Y vs. Raw X (Level-Level for DV check)
  plots[[1]] <- ggplot(data_filtered, aes(x = !!sym(x_var_raw), y = monthly_expenditure)) +
    geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "red", se = FALSE) +
    labs(title = paste0("Plot ", plot_start_index, ": Raw Expenditure vs. Raw ", x_var_name), x = x_var_name, y = "Monthly Expenditure (Rs.)") +
    theme_minimal() + if (!is.null(x_limit)) xlim(x_limit)
  ggsave(paste0("plot_", plot_start_index, "_raw_exp_vs_", tolower(x_var_raw), ".png"), plots[[1]], width = 7, height = 5)
  
  # Plot B: Log Y vs. Raw X (Log-Level for IV check)
  plots[[2]] <- ggplot(data_filtered, aes(x = !!sym(x_var_raw), y = log_mpce)) +
    geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "red", se = FALSE) +
    labs(title = paste0("Plot ", plot_start_index + 1, ": Log(MPCE) vs. Raw ", x_var_name), x = x_var_name, y = "Log(MPCE)") +
    theme_minimal() + if (!is.null(x_limit)) xlim(x_limit)
  ggsave(paste0("plot_", plot_start_index + 1, "_log_mpce_vs_", tolower(x_var_raw), ".png"), plots[[2]], width = 7, height = 5)
  
  # Plot C: Log Y vs. Log X (Log-Log for IV check)
  plots[[3]] <- ggplot(data_filtered, aes(x = log_x, y = log_mpce)) +
    geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "red", se = FALSE) +
    labs(title = paste0("Plot ", plot_start_index + 2, ": Log(MPCE) vs. Log(", x_var_name, ")"), x = paste0("Log(", x_var_name, ")"), y = "Log(MPCE)") +
    theme_minimal()
  ggsave(paste0("plot_", plot_start_index + 2, "_log_mpce_vs_log_", tolower(x_var_raw), ".png"), plots[[3]], width = 7, height = 5)
  
  # We will skip Plot D (Raw Y vs Log X) since Log(MPCE) is the confirmed DV
  
  return(plots)
}

# --- 4. GENERATE ALL 8 REQUESTED PLOTS ---

# 4.1 Land Acres (Plots 1-3)
create_plots(data_analysis_ready, "total_land_acres", "Land Acres", 1)

# 4.2 Total Livestock (Plots 4-6)
create_plots(data_analysis_ready, "total_livestock_count", "Total Livestock", 4)

# 4.3 In-Milk Cattle/Buffalo (Plots 7-9)
create_plots(data_analysis_ready, "in_milk_cattle_buffalo", "In-Milk Count", 7)

# 4.4 Household Size (Plots 10-12)
# Since household size is already incorporated in MPCE, the plots mostly show the residual effect.
create_plots(data_analysis_ready, "household_size", "Household Size", 10)


print("---")
print("✅ Plots generated successfully (Plots 1 through 12).")
print("Plots 1, 4, 7, 10 show Raw Expenditure vs. Raw X (checking DV transformation).")
print("Plots 2, 5, 8, 11 show Log(MPCE) vs. Raw X (checking Log-Level IV form).")
print("Plots 3, 6, 9, 12 show Log(MPCE) vs. Log(X) (checking Log-Log IV form).")
print("Please upload the images for Plots 2, 3, 5, 6, 8, 9, 11, and 12 for the final functional form assessment.")









library(dplyr)
library(readr)

# Set the threshold for small landholders (2.5 acres ≈ 1 hectare)
LAND_THRESHOLD_ACRES <- 2.5

# --- 1. LOAD THE ANALYSIS-READY FILE ---
# Assuming 'data_analysis_ready.rds' is the file from the last successful step,
# which includes all previous dummies and the log_mpce column.
data_analysis_ready <- readRDS("data_analysis_ready.rds")

print("--- Data Loaded. Adding Small Landholder Dummy Variable. ---")

# --- 2. CREATE THE NEW DUMMY COLUMN ---
data_analysis_ready <- data_analysis_ready %>%
  mutate(
    # Create a dummy variable for Small Landholders
    # 1 = Total Land Acres is less than or equal to 2.5 acres
    # 0 = Total Land Acres is greater than 2.5 acres
    land_smallholder = ifelse(total_land_acres <= LAND_THRESHOLD_ACRES, 1, 0)
  )

# --- 3. CROSS-CHECK THE NEW DUMMY ---
# Verify the count and proportion of smallholders
smallholder_summary <- data_analysis_ready %>%
  summarize(
    N_Total = n(),
    N_Smallholder = sum(land_smallholder),
    Percent_Smallholder = round((N_Smallholder / N_Total) * 100, 2)
  )

print("Smallholder Dummy Summary (<= 2.5 Acres):")
print(smallholder_summary)

# --- 4. SAVE THE FINAL ANALYSIS FILE ---
# Overwrite the previous version with the new dummy added.
saveRDS(data_analysis_ready, file = "data_analysis_ready.rds")
write.csv(data_analysis_ready, "data_analysis_ready.csv", row.names = FALSE)

print("-----------------------------------------------------------------")
print("SUCCESS: New column 'land_smallholder' added (<= 2.5 acres).")
print("File saved as 'data_analysis_ready.rds'.")
print("-----------------------------------------------------------------")









# ---
# NSSO 77th Round - Step 8: Build and Compare Regression Models
#
# This script loads the final analysis-ready data, runs the
# six specified OLS regression models, and prints the
# individual summary for each model.
# ---

# 1. LOAD LIBRARIES
# -------------------------------------------------------------------------
library(dplyr)
library(readr)

# 2. LOAD FINAL DATA
# -------------------------------------------------------------------------
# Load the final dataset which contains all dummies and variables
analysis_data <- readRDS("data_analysis_ready.rds")

print("Analysis-ready data loaded. Building models...")

# 3. DEFINE CONTROL VARIABLES
# -------------------------------------------------------------------------
# Control variables based on the dummy creation script
control_vars <- "household_size + 
                  social_group_OBC + social_group_SC_ST + 
                  religion_Muslim + religion_Other + 
                  hh_class_Agri_Related + hh_class_NonAgri + 
                  edu_Primary + edu_Secondary + edu_PostSecondary"

# 4. ESTIMATE THE SIX MODELS
# -------------------------------------------------------------------------
# NOTE on variable names:
#   Land_i = total_land_acres
#   Livestock_i = total_livestock_count
#   InMilk_i = in_milk_cattle_buffalo
#   SmallLand_i = land_smallholder
#   HHsize_i is included in control_vars for M3-M6

# Model 1 (M1): Simple (Total Livestock)
model_1 <- lm(monthly_expenditure ~ total_land_acres + total_livestock_count, 
              data = analysis_data)

# Model 2 (M2): Simple (In-Milk)
model_2 <- lm(monthly_expenditure ~ total_land_acres + in_milk_cattle_buffalo, 
              data = analysis_data)

# Model 3 (M3): Controls (Total Livestock)
formula_3 <- as.formula(paste("monthly_expenditure ~ total_land_acres + total_livestock_count +", 
                              control_vars))
model_3 <- lm(formula_3, data = analysis_data)

# Model 4 (M4): Controls (In-Milk)
formula_4 <- as.formula(paste("monthly_expenditure ~ total_land_acres + in_milk_cattle_buffalo +", 
                              control_vars))
model_4 <- lm(formula_4, data = analysis_data)

# Model 5 (M5): Controls + SmallLand Dummy (Total Livestock)
formula_5 <- as.formula(paste("monthly_expenditure ~ total_land_acres + total_livestock_count + land_smallholder +", 
                              control_vars))
model_5 <- lm(formula_5, data = analysis_data)

# Model 6 (M6): Controls + SmallLand Dummy (In-Milk)
formula_6 <- as.formula(paste("monthly_expenditure ~ total_land_acres + in_milk_cattle_buffalo + land_smallholder +", 
                              control_vars))
model_6 <- lm(formula_6, data = analysis_data)

print("Step 4 Complete: All 6 regression models have been estimated.")

# 5. PRINT INDIVIDUAL MODEL SUMMARIES
# -------------------------------------------------------------------------
print("-----------------------------------------------------------------")
print("--- MODEL 1: Simple (Total Livestock) ---")
print("-----------------------------------------------------------------")
print(summary(model_1))

print("-----------------------------------------------------------------")
print("--- MODEL 2: Simple (In-Milk) ---")
print("-----------------------------------------------------------------")
print(summary(model_2))

print("-----------------------------------------------------------------")
print("--- MODEL 3: Controls (Total Livestock) ---")
print("-----------------------------------------------------------------")
print(summary(model_3))

print("-----------------------------------------------------------------")
print("--- MODEL 4: Controls (In-Milk) ---")
print("-----------------------------------------------------------------")
print(summary(model_4))

print("-----------------------------------------------------------------")
print("--- MODEL 5: Controls + SmallLand Dummy (Total Livestock) ---")
print("-----------------------------------------------------------------")
print(summary(model_5))

print("-----------------------------------------------------------------")
print("--- MODEL 6: Controls + SmallLand Dummy (In-Milk) ---")
print("-----------------------------------------------------------------")
print(summary(model_6))

print("-----------------------------------------------------------------")
print("SUCCESS: All model summaries have been printed to the console.")
print("Proceeding to Step 9: Diagnostics and Robust Errors.")
print("-----------------------------------------------------------------")












# ---
# NSSO 77th Round - Step 9: OLS Assumption Diagnostics & Robust Errors
#
# This script tests the key OLS assumptions and generates the table
# using Heteroscedasticity-Consistent (Robust) Standard Errors.
# ---

# 1. LOAD LIBRARIES
# -------------------------------------------------------------------------
library(lmtest)     # For Breusch-Pagan (bptest) and Durbin-Watson (dwtest)
library(car)        # For VIF (vif) and visual plots (qqPlot)
library(sandwich)   # For calculating Robust Standard Errors
library(stargazer)  # For the final table
library(graphics)   # For layout

# Models are already estimated from Step 8 (model_1 through model_6)
analysis_data <- readRDS("data_analysis_ready.rds")
print("Analysis-ready data re-loaded for diagnostics.")

# Store models in a list for easier looping
model_list <- list(model_1, model_2, model_3, model_4, model_5, model_6)
model_names <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")

# 4. RUNNING DIAGNOSTIC TESTS (Abridged Output for Clarity)
# -------------------------------------------------------------------------
print("--- STARTING OLS DIAGNOSTIC CHECKS ---")

# --- A: Homoscedasticity (Breusch-Pagan Test) ---
print("Checking Homoscedasticity (Breusch-Pagan)...")
for (i in 1:6) {
  bp_test <- bptest(model_list[[i]])
  print(paste("---", model_names[i], "---"))
  print(bp_test)
}
print("==> INTERPRETATION: p-values are < 0.05, confirming HETEROSCEDASTICITY. Robust SEs are needed.")
cat("\n")


# --- B: Multicollinearity (VIF) ---
print("Checking Multicollinearity (VIF)...")
for (i in 1:6) {
  if (i >= 3) { # VIF is mainly relevant for models with multiple predictors (M3-M6)
    print(paste("--- VIF for", model_names[i], "---"))
    print(vif(model_list[[i]]))
  }
}
print("--- VIF INTERPRETATION ---")
print("Check that VIF values are not excessively high (e.g., < 10).")
cat("\n")


# --- C: Independence of Residuals (Durbin-Watson) ---
print("Checking Independence of Residuals (Durbin-Watson)...")
for (i in 1:6) {
  dw_test <- dwtest(model_list[[i]])
  print(paste("---", model_names[i], "---"))
  print(dw_test)
}
print("==> INTERPRETATION: D-W value close to 1 suggests positive autocorrelation/clustering. Clustered SEs (Step 10) will address this.")
cat("\n")


# 5. THE SOLUTION: RE-COMPUTE WITH ROBUST STANDARD ERRORS
# -------------------------------------------------------------------------
print("--- GENERATING ROBUST STANDARD ERRORS TABLE ---")

# Calculate Heteroscedasticity-Consistent (HC1) standard errors and p-values
se_robust_list <- lapply(model_list, function(m) coeftest(m, vcov = vcovHC(m, type = "HC1"))[, "Std. Error"])
p_robust_list <- lapply(model_list, function(m) coeftest(m, vcov = vcovHC(m, type = "HC1"))[, "Pr(>|t|)"])


# 6. GENERATE FINAL "ROBUST" OUTPUT TABLE (SHOWING ALL VARIABLES)
# -------------------------------------------------------------------------
stargazer(
  model_1, model_2, model_3, model_4, model_5, model_6,
  type = "html",
  title = "Regression Results (ALL VARIABLES) with ROBUST Standard Errors (HC1)",
  dep.var.labels = "Monthly Expenditure (Rs.)",
  
  # Apply the new robust standard errors and p-values
  se = se_robust_list,
  p = p_robust_list,
  
  column.labels = c(
    "M1: Simple (Livestock)",  
    "M2: Simple (In-Milk)",  
    "M3: Controls (Livestock)",  
    "M4: Controls (In-Milk)",  
    "M5: SmallLand (Livestock)",  
    "M6: SmallLand (In-Milk)"
  ),
  
  omit.stat = c("adj.rsq", "f", "ser"),
  align = TRUE,
  no.space = TRUE,
  
  out = "regression_table_ROBUST_ALL_VARS.html"
)

print("-----------------------------------------------------------------")
print("SUCCESS: Robust Table saved! Proceeding to Step 10: Clustered Errors.")
print("-----------------------------------------------------------------")








# ---
# NSSO 77th Round - Step 10: Final Regression with Clustered Standard Errors
#
# This script computes standard errors robust to BOTH heteroscedasticity 
# AND clustering at the village (FSU) level (v3_FSU_SNo).
# ---

# 1. LOAD LIBRARIES
# -------------------------------------------------------------------------
library(lmtest)     # For coeftest
library(sandwich)   # For vcovCL (Clustered)
library(stargazer)  # For the final table

# Models are already estimated from Step 8 (model_1 through model_6)
analysis_data <- readRDS("data_analysis_ready.rds")

# 4. COMPUTE CLUSTERED STANDARD ERRORS
# -------------------------------------------------------------------------
print("Calculating Clustered Standard Errors (at the FSU/Village level: v3_FSU_SNo)...")

# Define the cluster variable
cluster_var <- ~v3_FSU_SNo

# Calculate Clustered SEs and new p-values for each model
se_clustered_list <- lapply(model_list, function(m) coeftest(m, vcov = vcovCL(m, cluster = cluster_var))[, "Std. Error"])
p_clustered_list <- lapply(model_list, function(m) coeftest(m, vcov = vcovCL(m, cluster = cluster_var))[, "Pr(>|t|)"])

print("Clustered errors calculated.")

# 5. GENERATE FINAL "CLUSTERED" OUTPUT TABLE (SHOWING ALL VARIABLES)
# -------------------------------------------------------------------------
stargazer(
  model_1, model_2, model_3, model_4, model_5, model_6,
  type = "html",
  title = "Regression Results (ALL VARIABLES) with CLUSTERED Standard Errors (FSU)",
  dep.var.labels = "Monthly Expenditure (Rs.)",
  
  # Apply the new CLUSTERED standard errors and p-values
  se = se_clustered_list,
  p = p_clustered_list,
  
  column.labels = c(
    "M1: Simple (Livestock)",  
    "M2: Simple (In-Milk)",  
    "M3: Controls (Livestock)",  
    "M4: Controls (In-Milk)",  
    "M5: SmallLand (Livestock)",  
    "M6: SmallLand (In-Milk)"
  ),
  
  omit.stat = c("adj.rsq", "f", "ser"),
  align = TRUE,
  no.space = TRUE,
  
  out = "regression_table_CLUSTERED_ALL_VARS.html"
)

print("-----------------------------------------------------------------")
print("SUCCESS: Your FINAL, FULL table has been saved!")
print("==> Open 'regression_table_CLUSTERED_ALL_VARS.html' to see all variables. <==")
print("-----------------------------------------------------------------")









# ---
# NSSO 77th Round - Step 12: Detailed Descriptive Analysis (Frequencies)
#
# This script calculates the count and percentage for each category
# defined by the dummy variables, including the automatically created
# reference category for each group.
# ---

# 1. LOAD LIBRARIES
# -------------------------------------------------------------------------
library(dplyr)
library(readr)
library(janitor) # For easy frequency counting

# 2. LOAD FINAL DATA
# -------------------------------------------------------------------------
# Load the final dataset which contains all dummies and filtered data
analysis_data <- readRDS("data_analysis_ready.rds")

print("Analysis-ready data loaded. Calculating frequencies...")

# Helper function to compute frequencies and add the reference category
compute_frequencies <- function(data, dummy_cols, reference_name) {
  # Select the dummy columns
  df <- data %>% select(all_of(dummy_cols))
  
  # Calculate the sum of the dummy columns (1 for each group, 0 for ref)
  df <- df %>% mutate(is_dummy = rowSums(.) > 0)
  
  # Identify the reference group: those where all dummies are 0
  n_total <- nrow(data)
  n_ref <- n_total - sum(df$is_dummy)
  
  # Create a data frame for the counts
  counts_df <- data.frame(
    Category = c(reference_name, dummy_cols),
    N = c(n_ref, colSums(df[dummy_cols]))
  ) %>%
    # Calculate percentage
    mutate(Percent = round((N / n_total) * 100, 2)) %>%
    # Rename dummy columns for cleaner output
    mutate(Category = case_when(
      Category == "social_group_OBC" ~ "OBC",
      Category == "social_group_SC_ST" ~ "SC / ST",
      Category == "religion_Muslim" ~ "Muslim",
      Category == "religion_Other" ~ "Other Minority",
      Category == "hh_class_Agri_Related" ~ "Other Agri / Labour",
      Category == "hh_class_NonAgri" ~ "Non-Agricultural",
      Category == "edu_Primary" ~ "Primary / Upper Primary",
      Category == "edu_Secondary" ~ "Secondary / H.S.",
      Category == "edu_PostSecondary" ~ "Post-Secondary & Above",
      TRUE ~ Category
    ))
  
  return(counts_df)
}

# --- 3. Compute and Print Results for each Group ---

# 3.1. Social Group
social_cols <- c("social_group_OBC", "social_group_SC_ST")
social_freq <- compute_frequencies(analysis_data, social_cols, "Others (Reference)")
print("--- Social Group Frequencies ---")
print(social_freq)

# 3.2. Religion
religion_cols <- c("religion_Muslim", "religion_Other")
religion_freq <- compute_frequencies(analysis_data, religion_cols, "Hindu (Reference)")
print("--- Religion Frequencies ---")
print(religion_freq)

# 3.3. Livelihood Classification
livelihood_cols <- c("hh_class_Agri_Related", "hh_class_NonAgri")
livelihood_freq <- compute_frequencies(analysis_data, livelihood_cols, "Crop Production (Reference)")
print("--- Livelihood Frequencies ---")
print(livelihood_freq)

# 3.4. Education (Max HH Education)
education_cols <- c("edu_Primary", "edu_Secondary", "edu_PostSecondary")
education_freq <- compute_frequencies(analysis_data, education_cols, "Illiterate / Below Primary (Reference)")
print("--- Max Household Education Frequencies ---")
print(education_freq)

# ---
## 3.5Corrected Small Landholder Frequencies Calculation
# ---
#install.packages("janitor")
library(dplyr)
library(janitor) # <--- CRITICAL: Must ensure this is loaded!
library(readr)

# 1. Load the data (assuming it's in memory or can be re-loaded)
analysis_data <- readRDS("data_analysis_ready.rds")

# 2. Corrected frequency calculation
land_freq <- analysis_data %>% 
  # tabyl() is from janitor
  tabyl(land_smallholder) %>% 
  # adorn_totals() is from janitor
  adorn_totals("row") %>%
  mutate(land_smallholder = case_when(
    land_smallholder == 1 ~ "Small Landholder (<= 2.5 acres)",
    land_smallholder == 0 ~ "Large Landholder (Reference: > 2.5 acres)",
    TRUE ~ "Total"
  )) %>%
  select(Category = land_smallholder, N = n, Percent = percent) %>%
  mutate(Percent = round(Percent * 100, 2))

print("--- Small Landholder Frequencies (FIXED) ---")
print(land_freq)
print("-----------------------------------------------------------------")
print("Descriptive analysis complete.")










# ---
# NSSO 77th Round - Final Selected Interactions (M6, M7, and M8) - FIXED LABELS & FORMULA
#
# This script re-estimates models M6, M7, and M8, ensuring the covariate
# labels match the estimated coefficients exactly, using Clustered Standard Errors.
#
# UPDATED: Now prints individual model summaries to the console.
# ---

# 1. LOAD LIBRARIES
# -------------------------------------------------------------------------
library(dplyr)
library(readr)
library(lmtest)     
library(sandwich)   
library(stargazer)  

# 2. LOAD FINAL DATA & DEFINE CLUSTER
# -------------------------------------------------------------------------
analysis_data <- readRDS("data_analysis_ready.rds")
cluster_var <- ~v3_FSU_SNo

print("Analysis-ready data loaded. Estimating final interaction models with corrected labels...")

# 3. DEFINE BASE MODEL & CONTROL VARIABLES
# -------------------------------------------------------------------------
# Define all additive terms for the right-hand side of the model
base_predictors_string <- "total_land_acres + in_milk_cattle_buffalo + land_smallholder + household_size + social_group_OBC + social_group_SC_ST + religion_Muslim + religion_Other + hh_class_Agri_Related + hh_class_NonAgri + edu_Primary + edu_Secondary + edu_PostSecondary"

# Full formula string for the base model
base_formula_string <- paste("monthly_expenditure ~", base_predictors_string)
base_formula <- as.formula(base_formula_string)

# M6: Additive Base Model
model_6 <- lm(base_formula, data = analysis_data)
print("-----------------------------------------------------------------")
print("--- MODEL 6 SUMMARY (Additive Base) ---")
print("-----------------------------------------------------------------")
print(summary(model_6))


# 4. ESTIMATE NEW INTERACTION MODELS (M7 & M8)
# -------------------------------------------------------------------------

# M7 (New): Small Landholder x Post-Secondary Education
# FIX: Use paste0 for clean concatenation
formula_M7_string <- paste0(base_formula_string, " + land_smallholder:edu_PostSecondary")
formula_M7 <- as.formula(formula_M7_string)
model_7 <- lm(formula_M7, data = analysis_data)
print("-----------------------------------------------------------------")
print("--- MODEL 7 SUMMARY (Small Land x Post-Secondary) ---")
print("-----------------------------------------------------------------")
print(summary(model_7))


# M8 (New): Land Acres x Social Group
# FIX: Use paste0 for clean concatenation of multiple interaction terms
formula_M8_string <- paste0(base_formula_string, " + total_land_acres:social_group_OBC + total_land_acres:social_group_SC_ST")
formula_M8 <- as.formula(formula_M8_string)
model_8 <- lm(formula_M8, data = analysis_data)
print("-----------------------------------------------------------------")
print("--- MODEL 8 SUMMARY (Land x Social Group) ---")
print("-----------------------------------------------------------------")
print(summary(model_8))


# 5. COMPUTE CLUSTERED STANDARD ERRORS
# -------------------------------------------------------------------------
print("Calculating Clustered Standard Errors (FSU level)...")
model_list <- list(model_6, model_7, model_8)

# Calculate Clustered SEs and new p-values for all three models
se_clustered_list <- lapply(model_list, function(m) coeftest(m, vcov = vcovCL(m, cluster = cluster_var))[, "Std. Error"])
p_clustered_list <- lapply(model_list, function(m) coeftest(m, vcov = vcovCL(m, cluster = cluster_var))[, "Pr(>|t|)"])


# 6. GENERATE FINAL CLUSTERED OUTPUT TABLE (Label Alignment is Correct)
# -------------------------------------------------------------------------
# The order MUST match the variable order in the final models.

covariate_labels_fixed <- c(
  "Total Land Acres",
  "In-Milk Cattle/Buffalo",
  "Small Landholder ($\\le 2.5$ Acres)",
  "Household Size",
  "Social Group: OBC",
  "Social Group: SC/ST",
  "Religion: Muslim",
  "Religion: Other",
  "Livelihood: Other Agri/Labour",
  "Livelihood: Non-Agri",
  "Education: Primary (HH Max)",
  "Education: Secondary (HH Max)",
  "Education: Post-Secondary (HH Max)",
  "Small Landholder $\\times$ Post-Secondary",
  "Land Acres $\\times$ Social Group: OBC",
  "Land Acres $\\times$ Social Group: SC/ST",
  "Constant"
)

stargazer(
  model_6, model_7, model_8,
  type = "html",
  title = "Regression Results: Final Comparison (M6, M7, M8)",
  dep.var.labels = "Monthly Expenditure (Rs.)",
  
  se = se_clustered_list,
  p = p_clustered_list,
  
  column.labels = c(
    "M6: Additive Base",  
    "M7: Small Land $\\times$ Post-Sec",  
    "M8: Land Acres $\\times$ Social Group"
  ),
  
  covariate.labels = covariate_labels_fixed,
  
  omit.stat = c("adj.rsq", "f", "ser"),
  align = TRUE,
  no.space = TRUE,
  
  out = "regression_table_M7_M8_FINAL_COMPARISON_FIXED.html"
)

print("-----------------------------------------------------------------")
print("SUCCESS: Formulas are now correctly structured and the models have been estimated.")
print("The final comparison table is ready for review.")
print("-----------------------------------------------------------------")



