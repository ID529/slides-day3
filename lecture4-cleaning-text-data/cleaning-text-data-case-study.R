

# SET UP ------------------------------------------------------------------
library(tidyverse) # includes stringr and other core tidyverse packages
library(kableExtra) # For formatting table output 

# IMPORT DATA -------------------------------------------------------------

# Read in the data
data <- read_csv("data/messy_data.csv", na = character())

# Take a quick look
glimpse(data)

# DISTINCT VALUES FUNCTION ------------------------------------------------
## Case study: Check distinct values

# This code creates a custom function that prints metadata about the dataset. 
# The result returned from the function should be a data frame that includes 
# the following columns: 
# - col_name: variable names in the dataset
# - type: the data type of that variable
# - distinct_levels: the unique values in the data for a given variable
# - distinct_count: the total number unique values for a given variable

# Function to get a simple look at all the distinct values that appear in the data
check_distinct <- function(data) {
  # Extract existing data types for each column and store as character vector
  col_types <- map_chr(data, class)
  
  # Helper function to replace NA values and sort the distinct values
  str_clean <- function(x) str_sort(str_replace_na(unique(x), replacement = "."))
  
  # Create data frame view to summarize the messy data
  map(data, ~ str_c(str_clean(as.character(.x)), collapse = " | ")) |>
    bind_rows() |> 
    pivot_longer(everything(), names_to = "col_name", values_to = "distinct_levels") |> 
    mutate(distinct_count = str_count(distinct_levels, pattern = "(?<!\\.\\s)\\|") + 1) |>
    add_column(.after = "col_name", type = col_types) |> 
    kbl() |>
    kable_styling(font_size = 13)
}

# Use the function to look at the distinct values for each column/variable
check_distinct(data)



# CASE STUDY: CLEAN-UP COLUMN NAMES (APPROACH 1) --------------------------
# Chain together column name clean-up steps
new_data <- data |> 
  # Remove leading and trailing spaces in the column names
  rename_with(~ str_trim(str_to_lower(.))) |> 
  # Remove spaces (\\s) or periods (\\.) between 'gad' and '7' (i.e., make var prefix gad7)   
  rename_with(~ str_replace_all(., "d\\s7|d\\.7", "d7")) |> 
  # Remove parentheses, and replace spaces, periods, or forward slashes with "_"
  rename_with(~ str_replace_all(., c("\\(|\\)" = "", "\\s|\\.|\\/" = "_"))) |> 
  # Name clean-up for specific columns
  rename_with(~ str_replace_all(., c("question_|q_" = "q", 
                                     "ethn.*" = "ethn", 
                                     "med.*" = "psych_meds", 
                                     "ye.*" = "yrs",
                                     ".*id" = "id"))) 
# Check out your progress
check_distinct(new_data)



# CASE STUDY: CLEAN-UP COLUMN NAMES (APPROACH 2) --------------------------
# Write a function for name column clean-up 
name_cleanup <- function(data, patterns) {
  data <- data |>
    # Convert to lower-case and replace
    rename_with(~ str_trim(str_to_lower(.))) |>
    rename_with(~ str_replace_all(., patterns)) 
  return(data)
}

# Include all clean up patterns in a single "named vector". 
# format: pattern = replacement
patterns <- c("d\\s7|d\\.7" = "d7",   # gad7 prefix
              "\\(|\\)" = "",         # parentheses
              "\\s|\\.|\\/" = "_",    # spaces/periods
              "question_|q_" = "q",   # gad7 suffix
              "ethn.*" = "ethn",      # race/ethnicity
              "med.*" = "psych_meds", # medications
              "ye.*" = "yrs",         # age, years suffix
              ".*id" = "id")          # participant id

# Take our original data
data |> 
  # Then clean-up the variable names
  name_cleanup(patterns) |> 
  # Then check-out the result
  check_distinct()



# CASE STUDY: CLEAN-UP VALUES ---------------------------------------------
# Begin cleaning up values in the data
new_data <- new_data |> 
  mutate(
    # Convert strings in character columns to lower case
    # Remove leading and trailing white space
    across(where(is.character), ~ str_trim(str_to_lower(.))),
    # Correct discrepant values in gad7_q1 and _q2 columns
    across(matches("gad.*[1-2]"), ~ str_replace_all(., "1/2", "half")), 
    # Make specific string replacements across columns
    across(everything(), ~ str_replace_all(., c("n/a" = "na", 
                                                ".*none.*" = "none", 
                                                "\\s-\\s|\\s\\(" = "_", 
                                                "\\)" = "", 
                                                "\\sfor\\s" = "_"))),
    # Replace text string 'na' values with true NA's
    across(everything(), ~ na_if(., "na"))
  ) 

# Check your work so far
check_distinct(new_data)



# CASE STUDY: CLEAN UP MORE VALUES ----------------------------------------
# Correct response values for GAD-7 questions 1 and 2
new_data <- new_data |> 
  mutate(
    across(matches("gad.*[1-2]"), 
           ~ case_when(
             . == "not at all" ~ "0",
             . == "several days" ~ "1",
             . == "more than half the days" ~ "2",
             . == "nearly every day" ~ "3", 
             TRUE ~ .)
    )
  )

# Check yout your progress
check_distinct(new_data)



# CASE STUDY: COERCE DATA TYPES/DERIVE VARIABLES --------------------------
# Coerce data types and generate new variables based on existing columns
new_data <- new_data |>   
  mutate(
    across(matches("id|age|gad"), as.numeric), 
    indication = str_extract(psych_meds, "(?<=_).*"),
    psych_meds = str_replace_all(psych_meds, "_.*", ""),
    depression = if_else(indication == "depression", 1, 0, missing = NA_real_),
    anxiety = if_else(indication == "anxiety", 1, 0, missing = NA_real_),
    gad7_raw = rowSums(across(matches("gad"))), 
    gad7_cat = 
      case_when(
        gad7_raw <= 4 ~ "minimal",
        gad7_raw %in% c(5:9) ~ "mild",
        gad7_raw %in% c(10:14) ~ "moderate",
        gad7_raw >= 15 ~ "severe"
      ),
    across(matches("race|_cat"), as.factor)
  )

# Check your progress
check_distinct(new_data)