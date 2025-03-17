# Title: Predicting Violent Incidents in New Orleans: An Analysis of Calls for Service Data
# Author: c-burruss
# Date: March 14, 2025

################################################################################
# 1. Introduction and Overview
################################################################################
# This analysis explores the relationship between various urban disorder indicators 
# and violent incidents in New Orleans neighborhoods. By examining data from the 
# city's calls for service (CFS) system alongside other civic datasets, we 
# investigate whether factors such as code violations, streetlight outages, 
# abandoned vehicles, and lot abatements can serve as predictors of violent incidents.

################################################################################
# 2. Methods and Analysis
################################################################################

# 2.1 Data Preparation

# 2.1.1 Setting Up the Environment

# First install and load basic packages we need for the installation process
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Define the list of packages
packages <- c(
  # Machine learning packages
  'tidymodels', 'glmnet', 'ranger', 'earth', 'vip',
  # Spatial analysis packages
  'tidygeocoder', 'geojsonio', 'spdep', 'sf',
  # Data wrangling and viz packages
  'tidyverse', 'ggplot2', 'ggthemes', 'ggrepel',
  'janitor', 'knitr', 'readr', 'grid',
  # Other packages
  'RSocrata', 'conflicted', 'purrr'
)

# Install and load all packages
p_load(char = packages)

# After ensuring conflicted is loaded, set preference
if (requireNamespace("conflicted", quietly = TRUE)) {
  conflicted::conflict_prefer('filter', 'dplyr')
} else {
  warning("Package 'conflicted' is not available, filter function conflicts may occur")
}

# Ensure that dplyr's filter() function works
conflict_prefer('filter', 'dplyr')

# 2.1.2 Helper Functions

# Helper function to clean and standardize data
pasteurize <- function(df) {
  # Helper function to clean column names
  clean_column_names <- function(col_name) {
    cleaned <- tolower(col_name)
    cleaned <- gsub("[^a-z0-9_]", "_", cleaned)
    cleaned <- gsub("_+", "_", cleaned)
    cleaned <- gsub("^_|_$", "", cleaned)
    return(cleaned)
  }
  
  # Helper function for title case conversion
  to_title_case <- function(x) {
    # Ensure we're working with a character vector
    if (!is.character(x)) return(x)
    # Apply transformation to each element in the vector
    sapply(x, function(str) {
      # Handle NA values
      if (is.na(str)) return(NA)
      # Split into words and capitalize first letter of each
      words <- strsplit(tolower(str), " ")[[1]]
      words <- paste0(toupper(substr(words, 1, 1)),
                      substr(words, 2, nchar(words)))
      # Rejoin words with spaces
      paste(words, collapse = " ")
    })
  }
  
  # Main data cleaning operations
  df %>%
    filter(!if_all(everything(), is.na)) %>%
    distinct() %>%
    rename_with(clean_column_names) %>%
    mutate(across(where(is.character),
                  ~str_trim(.) %>%
                    to_title_case))
}

# Function for summarizing dataset statistics
stat.summarize <- function(df) {
  numeric_df <- df %>%
    select(where(is.numeric))
  
  # Initialize a list to store results
  summary_list <- list()
  
  # Loop through each numeric column and calculate statistics
  for (col in names(numeric_df)) {
    summary_list[[col]] <- c(
      min = min(numeric_df[[col]], na.rm = TRUE),
      max = max(numeric_df[[col]], na.rm = TRUE),
      median = median(numeric_df[[col]], na.rm = TRUE),
      mean = mean(numeric_df[[col]], na.rm = TRUE),
      sd = round(sd(numeric_df[[col]], na.rm = TRUE), 2),
      n = sum(!is.na(numeric_df[[col]]))
    )
  }
  
  # Combine the results into a data frame and transpose
  result <- as.data.frame(do.call(rbind, summary_list))
  
  # Transpose the result to switch rows and columns
  result <- as.data.frame(t(result))
  
  # Set the column names to the measurements
  colnames(result) <- names(numeric_df)
  
  return(result)
}

# 2.1.3 Data Acquisition

# Download neighborhoods spatial file
path <- './data/neighborhoods.geojson'
github_url <- 'https://raw.githubusercontent.com/CBurruss/HarvardX-CYO/main/data/neighborhoods.geojson'
if (!file.exists(path)) {
  download.file(github_url, path, mode = 'wb') # 'wb' ensures binary mode for non-text files
} else {
  message('The neighborhoods file exists!')
}

# Download 2023 calls for service (cfs)
path <- './data/cfs-2023.csv'
if(!file.exists(path)) {
  # Download data and save it to file
  cfs_file <- read.socrata('https://data.nola.gov/api/odata/v4/pc5d-tvaw')
  write.csv(cfs_file, file = path, row.names = FALSE)
  message('The cfs file has been downloaded and saved to ', path)
} else {
  message('The cfs file already exists at ', path, '!')
}

# Download code violations
path <- './data/violations.csv'
if(!file.exists(path)) {
  # Download data and save it to file
  violations_file <- read.socrata('https://data.nola.gov/api/odata/v4/3ehi-je3s')
  write.csv(violations_file, file = path, row.names = FALSE)
  message('The violations file has been downloaded and saved to ', path)
} else {
  message('The violations file already exists at ', path, '!')
}

# Download 311 calls (for streetlight outages and abandoned vehicles)
path <- './data/311.csv'
if(!file.exists(path)) {
  # Download data and save it to file
  three11_file <- read.socrata('https://data.nola.gov/api/odata/v4/2jgv-pqrq')
  write.csv(three11_file, file = path, row.names = FALSE)
  message('The 311 file has been downloaded and saved to ', path)
} else {
  message('The 311 file already exists at ', path, '!')
}

rm(three11_file)

# Download ch 66 lot abatements
path <- './data/ch66.csv'
if(!file.exists(path)) {
  # Download data and save it to file
  ch66_file <- read.socrata('https://data.nola.gov/api/odata/v4/xhih-vxs6')
  write.csv(ch66_file, file = path, row.names = FALSE)
  message('The ch 66 file has been downloaded and saved to ', path)
} else {
  message('The ch 66 file already exists at ', path, '!')
}

# 2.2 Data Processing and Exploration

# 2.2.1 Processing Calls for Service Data

# Function to load and process CFS data
load_cfs_data <- function() {
  # Read in CFS data
  cfs <- read.csv("./data/cfs-2023.csv") %>%
    # Perform a general cleanse
    pasteurize() %>%
    # Convert date fields
    mutate(date = as.Date(timecreate), .after = timecreate) %>%
    mutate(year = substr(date, 1, 4), .after = date)
  
  # Extract coordinates
  cfs <- cfs %>%
    mutate(
      long_lat = str_extract(location, "\\-?\\d+\\.\\d+ \\-?\\d+\\.\\d+"),
      long_lat = str_replace(long_lat, " ", ",")
    ) %>%
    separate(long_lat, into = c("x", "y"), sep = ",") %>%
    mutate(x = as.numeric(x), y = as.numeric(y)) %>%
    na.omit()
  
  # Convert to spatial object
  cfs_sf <- cfs %>%
    st_as_sf(coords = c("x", "y"), crs = 4326, agr = "constant")
  
  return(cfs_sf)
}

cfs <- load_cfs_data()  # nb: this may take a few minutes

# Display summary of the dataset
glimpse(cfs)

# 2.2.2 Identifying Violent Incidents

identify_violent_calls <- function(cfs_data) {
  # Define keywords associated with violent incidents
  violent_keywords <- c("assault", "shooting", "shot", "homicide", "weapon",
                        "burglary", "robbery", "carjack", "domestic", "murder",
                        "stab", "violence", "violent", "fight", "battery", "rape")
  
  exclusion_keywords <- c("alarm", "not occupied", "brandish", "shots fired")
  
  # Create a new column identifying violent calls
  cfs_data %>%
    mutate(
      typetext_lower = tolower(typetext),
      is_violent = str_detect(typetext_lower, paste(violent_keywords, collapse = "|")) &
        !str_detect(typetext_lower, paste(exclusion_keywords, collapse = "|")),
      predicted_category = if_else(is_violent, "Violent", "Non-violent")
    )
}

cfs <- identify_violent_calls(cfs)
violent_calls <- cfs %>%
  filter(is_violent == TRUE)

# Summarize violent call types
cfs %>%
  filter(is_violent == TRUE) %>%
  group_by(typetext) %>%
  summarize(count = n()) %>%
  mutate(percent = paste0(round(count / sum(count) * 100, 0), '%')) %>%
  arrange(desc(count)) %>%
  as_tibble() %>%
  select(-geometry) %>%
  head(15)

# 2.2.3 Processing Neighborhood Data

load_neighborhoods <- function() {
  neighborhoods <- st_read('./data/neighborhoods.geojson') %>%
    clean_names() %>%
    st_transform(crs = 4326)
  
  return(neighborhoods)
}

neighborhoods <- load_neighborhoods()

# 2.2.4 Visualizing Neighborhoods and Violent Incidents

# Simple map of neighborhoods
neighborhood_map <- ggplot() +
  geom_sf(data = neighborhoods, fill = "lightblue", color = "white") +
  labs(title = "New Orleans neighborhoods") +
  theme_solarized()

print(neighborhood_map)

# Map with violent calls as points
violent_calls_map <- ggplot() +
  geom_sf(data = neighborhoods, fill = "lightblue", alpha = 0.5, color = "white") +
  geom_sf(data = violent_calls, color = "darkgrey", alpha = 0.5, size = 0.5) +
  labs(title = "Violent calls for service in New Orleans (2023)") +
  theme_solarized()

print(violent_calls_map)

# 2.2.5 Processing Risk Factor Data

# Load code violations
load_violations <- function() {
  # Check if the processed file already exists
  if (!file.exists('./data/violations_sf.RDS')) {
    # Initial import and clean
    violations <- read.csv('./data/violations.csv') %>%
      clean_names() %>%
      mutate(violationdate = as.Date(violationdate)) %>%
      mutate(year = (substr(violationdate, 1, 4))) %>%
      filter(year == '2023') %>%
      select(-lastupload)
    
    # Standardize addresses
    violations$location_clean <- violations$location
    violations$location_clean <- gsub(',', ' and', violations$location_clean)
    violations$address <- paste(violations$location_clean, 'New Orleans, LA')
    
    # Geocode if not already
    if("lat" %in% colnames(violations) && "long" %in% colnames(violations)) {
      # Use existing coordinates
      violations_sf <- violations %>%
        st_as_sf(coords = c("long", "lat"), crs = 4326, agr = "constant") %>%
        mutate(Legend = 'Code violations') %>%
        na.omit()
    } else {
      # Geocode addresses
      violations_geo <- violations %>%
        geocode(address = address,
                method = 'arcgis',
                lat = latitude,
                long = longitude)
      
      # Convert to sf object
      violations_sf <- violations_geo %>%
        filter(!is.na(latitude) & !is.na(longitude)) %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
        mutate(Legend = 'Code violations')
    }
    
    # Save the processed data to an RDS file for future use
    saveRDS(violations_sf, './data/violations_sf.RDS')
    message("Geocoding complete!")
  } else {
    # Load the pre-processed data from the saved RDS
    violations_sf <- readRDS('./data/violations_sf.RDS')
    message("Loaded cached file.")
  }
  
  return(violations_sf)
}

# Load streetlight outages from 311 data
load_streetlights <- function() {
  # Check if 311.csv exists
  if(file.exists('./data/311.csv')) {
    lights <- read.csv('./data/311.csv') %>%
      clean_names() %>%
      filter(grepl('Streetlight', request_reason))
    
    # Convert to spatial object
    lights_sf <- lights %>%
      st_as_sf(coords = c('longitude', 'latitude'),
               crs = 4326,
               agr = 'constant',
               remove = FALSE) %>%
      na.omit()
    
    return(lights_sf)
  } else {
    stop("311 file not found")
  }
}

# Load abandoned vehicles from 311 data
load_vehicles <- function() {
  # Check if 311.csv exists
  if(file.exists('./data/311.csv')) {
    vehicles <- read.csv('./data/311.csv') %>%
      clean_names() %>%
      filter(grepl('Junk Vehicles', request_reason))
    
    # Convert to spatial object
    vehicles_sf <- vehicles %>%
      st_as_sf(coords = c('longitude', 'latitude'),
               crs = 4326,
               agr = 'constant',
               remove = FALSE) %>%
      na.omit()
    
    return(vehicles_sf)
  } else {
    stop("311 file not found")
  }
}

# Load chapter 66 abatements
load_abatements <- function() {
  # Check if the processed file already exists
  if (!file.exists('./data/ch66_sf.RDS')) {
    # Initial import and clean
    ch66 <- read.csv('./data/ch66.csv') %>%
      clean_names() %>%
      mutate(casefiled = as.Date(casefiled)) %>%
      mutate(year = substr(casefiled, 1, 4), .after = casefiled) %>%
      filter(year == '2023' & longitude >= -91.18) %>% # exclude cases outside of New Orleans
      filter(!is.na(longitude) & !is.na(latitude)) %>% # ensure both coordinates are non-NA
      distinct()
    
    # Convert to sf object using existing coordinates
    ch66_sf <- ch66 %>%
      st_as_sf(coords = c('longitude', 'latitude'),
               crs = 4326,
               agr = "constant") %>%
      st_make_valid()
    
    # Check if the sf object is valid
    if (any(st_is_valid(ch66_sf) == FALSE)) {
      warning("Some geometries are invalid after applying st_make_valid()")
    }
    
    # Save the processed data to an RDS file for future use
    saveRDS(ch66_sf, './data/ch66_sf.RDS')
    message("Ch 66 abatements data processed and saved.")
  } else {
    # Load the pre-processed data from the saved RDS
    ch66_sf <- readRDS('./data/ch66_sf.RDS')
    message("Ch 66 abatements data loaded from cache.")
  }
  
  return(ch66_sf)
}

# Load all risk factors
violations <- load_violations()
streetlights <- load_streetlights()
vehicles <- load_vehicles()
abatements <- load_abatements()

# 2.3 Data Aggregation by Neighborhood

# Aggregate violent calls by neighborhood
aggregate_violent_calls <- function(violent_calls, neighborhoods) {
  violent_by_neighborhood <- st_join(violent_calls, neighborhoods) %>%
    group_by(gnocdc_lab) %>%
    summarize(violent_count = n()) %>%
    st_drop_geometry()
  
  return(violent_by_neighborhood)
}

# Aggregate code violations by neighborhood
aggregate_violations <- function(violations, neighborhoods) {
  violations_by_neighborhood <- st_join(violations, neighborhoods) %>%
    group_by(gnocdc_lab) %>%
    summarize(code_violations = n()) %>%
    st_drop_geometry()
  
  return(violations_by_neighborhood)
}

# Aggregate streetlight outages by neighborhood
aggregate_streetlights <- function(lights, neighborhoods) {
  lights_by_neighborhood <- st_join(lights, neighborhoods) %>%
    group_by(gnocdc_lab) %>%
    summarize(streetlight_outages = n()) %>%
    st_drop_geometry()
  
  return(lights_by_neighborhood)
}

# Aggregate abandoned vehicles by neighborhood
aggregate_vehicles <- function(vehicles, neighborhoods) {
  vehicles_by_neighborhood <- st_join(vehicles, neighborhoods) %>%
    group_by(gnocdc_lab) %>%
    summarize(abandoned_vehicles = n()) %>%
    st_drop_geometry()
  
  return(vehicles_by_neighborhood)
}

# Aggregate chapter 66 abatements by neighborhood
aggregate_abatements <- function(ch66, neighborhoods) {
  ch66_by_neighborhood <- st_join(ch66, neighborhoods) %>%
    group_by(gnocdc_lab) %>%
    summarize(lot_abatements = n()) %>%
    st_drop_geometry()
  
  return(ch66_by_neighborhood)
}

# Join all neighborhood-level data
create_neighborhood_dataset <- function(neighborhoods, violent_by_neighborhood,
                                        violations_by_neighborhood,
                                        lights_by_neighborhood,
                                        vehicles_by_neighborhood,
                                        ch66_by_neighborhood) {
  neighborhood_data <- neighborhoods %>%
    select(gnocdc_lab, geometry) %>%
    left_join(violent_by_neighborhood, by = "gnocdc_lab") %>%
    left_join(violations_by_neighborhood, by = "gnocdc_lab") %>%
    left_join(lights_by_neighborhood, by = "gnocdc_lab") %>%
    left_join(vehicles_by_neighborhood, by = "gnocdc_lab") %>%
    left_join(ch66_by_neighborhood, by = "gnocdc_lab") %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)))
  
  return(neighborhood_data)
}

# Aggregate data by neighborhood
violent_by_neighborhood <- aggregate_violent_calls(violent_calls, neighborhoods)
violations_by_neighborhood <- aggregate_violations(violations, neighborhoods)
lights_by_neighborhood <- aggregate_streetlights(streetlights, neighborhoods)
vehicles_by_neighborhood <- aggregate_vehicles(vehicles, neighborhoods)
ch66_by_neighborhood <- aggregate_abatements(abatements, neighborhoods)

# Create final neighborhood dataset
neighborhood_data <- create_neighborhood_dataset(
  neighborhoods,
  violent_by_neighborhood,
  violations_by_neighborhood,
  lights_by_neighborhood,
  vehicles_by_neighborhood,
  ch66_by_neighborhood
)

# 2.4 Statistical Analysis

# 2.4.1 Correlation Analysis

# Define a function for running a correlation between risk factors and violent calls
run_correlation_analysis <- function(neighborhood_data) {
  # Prepare data for correlation
  corr_data <- neighborhood_data %>%
    st_drop_geometry() %>%
    select(violent_count, code_violations, streetlight_outages,
           abandoned_vehicles, lot_abatements)
  
  # Calculate correlation matrix
  cor_matrix <- cor(corr_data, use = "pairwise.complete.obs")
  
  # Calculate correlation with p-values
  corr_results <- data.frame()
  predictors <- c("code_violations", "streetlight_outages",
                  "abandoned_vehicles", "lot_abatements")
  
  for (predictor in predictors) {
    test <- cor.test(
      neighborhood_data[[predictor]],
      neighborhood_data[["violent_count"]],
      use = "pairwise.complete.obs"
    )
    
    corr_results <- rbind(corr_results, data.frame(
      factor = predictor,
      correlation = test$estimate,
      p_value = test$p.value,
      significance = case_when(
        test$p.value < 0.001 ~ "***",
        test$p.value < 0.01 ~ "**",
        test$p.value < 0.05 ~ "*",
        TRUE ~ "ns"
      )
    ))
  }
  
  # Return both correlation matrix and detailed test results
  return(list(
    correlation_matrix = cor_matrix,
    correlation_tests = corr_results
  ))
}

# Run the correlation analysis
correlation_results <- run_correlation_analysis(neighborhood_data)
print(correlation_results$correlation_tests)

# 2.4.2 Visualizing Relationships

# Dot plot of code violations vs violent calls with smoothing line
ggplot(neighborhood_data %>% st_drop_geometry(),
       aes(x = code_violations, y = violent_count)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue") +
  theme_solarized() +
  labs(title = "Relationship between code violations and violent calls",
       x = "Number of code violations",
       y = "Number of violent calls") +
  geom_text_repel(aes(label = str_to_title(gnocdc_lab)), size = 3, max.overlaps = 10)

# Dot plot of streetlight outages vs violent calls with smoothing line
ggplot(neighborhood_data %>% st_drop_geometry(),
       aes(x = streetlight_outages, y = violent_count)) +
  geom_point(color = "purple4", alpha = 0.7) +
  geom_smooth(method = "lm", color = "purple4") +
  theme_solarized() +
  labs(title = "Relationship between streetlight outages and violent calls",
       x = "Number of streetlight outages",
       y = "Number of violent calls") +
  geom_text_repel(aes(label = str_to_title(gnocdc_lab)), size = 3, max.overlaps = 10)

# First, prepare the data in long format
correlation_data <- neighborhood_data %>%
  st_drop_geometry() %>%
  select(gnocdc_lab, violent_count, code_violations, streetlight_outages,
         abandoned_vehicles, lot_abatements) %>%
  pivot_longer(cols = c(code_violations, streetlight_outages,
                        abandoned_vehicles, lot_abatements),
               names_to = "risk_factor",
               values_to = "count")

# Create plot with all risk factors colored differently
ggplot(correlation_data, aes(x = count, y = violent_count, color = risk_factor)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = risk_factor), alpha = 0.1) +
  scale_color_brewer(palette = "Set1",
                     labels = c("Abandoned Vehicles", "Code Violations",
                                "Lot Abatements", "Streetlight Outages")) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("Abandoned Vehicles", "Code Violations",
                               "Lot Abatements", "Streetlight Outages")) +
  theme_solarized() +
  labs(title = "Relationship between risk factors and violent calls",
       x = "Count of risk factors",
       y = "Number of violent calls",
       color = "Risk factor",
       fill = "Risk factor") +
  theme(legend.position = "right")

################################################################################
# 3. Predictive Modeling
################################################################################

# 3.1 Data Preparation for Modeling

prepare_ml_data <- function(neighborhood_data) {
  # Prepare data for modeling
  model_data <- neighborhood_data %>%
    st_drop_geometry() %>%
    select(violent_count, code_violations, streetlight_outages,
           abandoned_vehicles, lot_abatements) %>%
    # Remove any rows with NA values
    na.omit()
  
  # Split into training and testing sets (80/20)
  set.seed(90210)
  data_split <- initial_split(model_data, prop = 0.8)
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  # Create validation set for tuning
  set.seed(90210)
  cv_folds <- vfold_cv(train_data, v = 5)
  
  return(list(
    train_data = train_data,
    test_data = test_data,
    cv_folds = cv_folds,
    full_data = model_data
  ))
}

ml_data <- prepare_ml_data(neighborhood_data)

# 3.2 Linear Regression Model

# Re-establish seed
set.seed(90210)

# Define a function for the lm model
build_lm_model <- function(ml_data) {
  # Define the model specification
  lm_spec <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")
  
  # Define the workflow
  lm_workflow <- workflow() %>%
    add_formula(violent_count ~ .) %>%
    add_model(lm_spec)
  
  # Fit the model
  lm_fit <- lm_workflow %>%
    fit(data = ml_data$train_data)
  
  # Predictions on test data
  lm_predictions <- lm_fit %>%
    predict(ml_data$test_data) %>%
    bind_cols(ml_data$test_data)
  
  # Calculate performance metrics
  lm_metrics <- lm_predictions %>%
    metrics(truth = violent_count, estimate = .pred)
  
  # Extract model coefficients
  lm_coefs <- lm_fit %>%
    extract_fit_parsnip() %>%
    tidy() %>%
    mutate(intercept_flag = if_else(term == "(Intercept)", 1, 0)) %>%
    arrange(desc(intercept_flag), desc(estimate)) %>%
    select(-intercept_flag)  # Remove the intercept flag after sorting
  
  return(list(
    workflow = lm_workflow,
    model = lm_fit,
    predictions = lm_predictions,
    metrics = lm_metrics,
    coefficients = lm_coefs
  ))
}

# Print results of the linear model
lm_results <- build_lm_model(ml_data)

# Display linear model performance metrics
cat("Linear Model Performance Metrics:\n")
print(lm_results$metrics)

cat("\nModel Coefficients:\n")
print(lm_results$coefficients)

# 3.3 Random Forest Model

# Re-establish seed
set.seed(90210)

# Build Random Forest model with tidymodels
build_rf_model <- function(ml_data) {
  # Define the model specification
  rf_spec <- rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
  ) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("regression")
  
  # Define the workflow
  rf_workflow <- workflow() %>%
    add_formula(violent_count ~ .) %>%
    add_model(rf_spec)
  
  # Define the grid for tuning
  rf_grid <- grid_regular(
    mtry(range = c(1, 4)),
    min_n(range = c(2, 10)),
    levels = 1
  )
  
  # Tune the model
  rf_tune_results <- rf_workflow %>%
    tune_grid(
      resamples = ml_data$cv_folds,
      grid = rf_grid,
      metrics = metric_set(rmse, rsq)
    )
  
  # Select best hyperparameters
  rf_best_params <- rf_tune_results %>%
    select_best(metric = "rmse")
  
  # Finalize workflow with best parameters
  rf_final_workflow <- rf_workflow %>%
    finalize_workflow(rf_best_params)
  
  # Fit the final model
  rf_final_fit <- rf_final_workflow %>%
    fit(data = ml_data$train_data)
  
  # Predictions on test data
  rf_predictions <- rf_final_fit %>%
    predict(ml_data$test_data) %>%
    bind_cols(ml_data$test_data)
  
  # Calculate performance metrics
  rf_metrics <- rf_predictions %>%
    metrics(truth = violent_count, estimate = .pred)
  
  # Extract variable importance
  rf_imp <- rf_final_fit %>%
    extract_fit_parsnip() %>%
    vip::vi()
  
  return(list(
    workflow = rf_final_workflow,
    model = rf_final_fit,
    predictions = rf_predictions,
    metrics = rf_metrics,
    importance = rf_imp,
    tuning_results = rf_tune_results
  ))
}

# Run random forest model
rf_results <- build_rf_model(ml_data)

# Print results of the random forest model
cat("Random Forest Performance Metrics:\n")
print(rf_results$metrics)

cat("\nVariable Importance:\n")
print(rf_results$importance)

# 3.4 Elastic Net Regression Model

# Re-establish seed
set.seed(90210)

build_elastic_net_model <- function(ml_data) {
  # Define the model specification
  en_spec <- linear_reg(
    penalty = tune(),
    mixture = tune()
  ) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
  
  # Create pre-processing recipe
  en_recipe <- recipe(violent_count ~ ., data = ml_data$train_data) %>%
    step_normalize(all_predictors())
  
  # Define the workflow
  en_workflow <- workflow() %>%
    add_recipe(en_recipe) %>%
    add_model(en_spec)
  
  # Define the grid for tuning
  # Using smaller, more appropriate values for penalty
  en_grid <- grid_regular(
    penalty(range = c(0.001, 1.0)), # Values between 0 and 1
    mixture(range = c(0, 1)), # Values between 0 and 1
    levels = c(10, 5)
  )
  
  # Tune the model
  en_tune_results <- en_workflow %>%
    tune_grid(
      resamples = ml_data$cv_folds,
      grid = en_grid,
      metrics = metric_set(rmse, rsq)
    )
  
  # Select best hyperparameters
  en_best_params <- en_tune_results %>%
    select_best(metric = "rmse")
  
  # Finalize workflow with best parameters
  en_final_workflow <- en_workflow %>%
    finalize_workflow(en_best_params)
  
  # Fit the final model
  en_final_fit <- en_final_workflow %>%
    fit(data = ml_data$train_data)
  
  # Predictions on test data
  en_predictions <- en_final_fit %>%
    predict(ml_data$test_data) %>%
    bind_cols(ml_data$test_data)
  
  # Calculate performance metrics
  en_metrics <- en_predictions %>%
    metrics(truth = violent_count, estimate = .pred)
  
  # Extract model coefficients - handle potential error
  en_coefs <- tryCatch({
    en_final_fit %>%
      extract_fit_parsnip() %>%
      tidy() %>%
      arrange(desc(estimate))
  }, error = function(e) {
    message("Could not extract coefficients: ", e$message)
    return(data.frame(term = character(), estimate = numeric()))
  })
  
  return(list(
    workflow = en_final_workflow,
    model = en_final_fit,
    predictions = en_predictions,
    metrics = en_metrics,
    coefficients = en_coefs,
    tuning_results = en_tune_results
  ))
}

# Run the elastic net model
elastic_net_results <- build_elastic_net_model(ml_data)

# Print results of the elastic net model
cat("Elastic Net Model Performance Metrics:\n")
print(elastic_net_results$metrics)

cat("\nElastic Net Coefficients:\n")
tryCatch({
  print(elastic_net_results$coefficients)
}, error = function(e) {
  cat("Could not display coefficients:", e$message)
})

# 3.5 Multivariate Adaptive Regression Splines (MARS)

# Re-establish seed
set.seed(90210)

build_mars_model <- function(ml_data) {
  # Define the model specification
  mars_spec <- mars(
    num_terms = tune(),
    prod_degree = tune()
  ) %>%
    set_engine("earth") %>%
    set_mode("regression")
  
  # Define the workflow
  mars_workflow <- workflow() %>%
    add_formula(violent_count ~ .) %>%
    add_model(mars_spec)
  
  # Define the grid for tuning
  mars_grid <- grid_regular(
    num_terms(range = c(2, 8)), # Number of terms to retain
    prod_degree(range = c(1, 2)), # Product degree (1 = additive, 2 = two-way interactions)
    levels = c(4, 2)
  )
  
  # Tune the model
  mars_tune_results <- mars_workflow %>%
    tune_grid(
      resamples = ml_data$cv_folds,
      grid = mars_grid,
      metrics = metric_set(rmse, rsq)
    )
  
  # Select best hyperparameters
  mars_best_params <- mars_tune_results %>%
    select_best(metric = "rmse")
  
  # Finalize workflow with best parameters
  mars_final_workflow <- mars_workflow %>%
    finalize_workflow(mars_best_params)
  
  # Fit the final model
  mars_final_fit <- mars_final_workflow %>%
    fit(data = ml_data$train_data)
  
  # Predictions on test data
  mars_predictions <- mars_final_fit %>%
    predict(ml_data$test_data) %>%
    bind_cols(ml_data$test_data)
  
  # Calculate performance metrics
  mars_metrics <- mars_predictions %>%
    metrics(truth = violent_count, estimate = .pred)
  
  return(list(
    workflow = mars_final_workflow,
    model = mars_final_fit,
    predictions = mars_predictions,
    metrics = mars_metrics,
    tuning_results = mars_tune_results
  ))
}

# Run the MARS model
mars_results <- build_mars_model(ml_data)

# Print results of the MARS model
cat("MARS Performance Metrics:\n")
print(mars_results$metrics)

# 3.6 Model Comparison

compare_models <- function(lm_results, rf_results, elastic_net_results, mars_results) {
  # Create comparison dataframe
  model_names <- c("Linear Regression", "Random Forest", "Elastic Net", "MARS")
  
  # Prepare metrics for comparison
  get_rmse <- function(result) {
    result$metrics %>%
      filter(.metric == "rmse") %>%
      pull(.estimate)
  }
  
  get_rsq <- function(result) {
    result$metrics %>%
      filter(.metric == "rsq") %>%
      pull(.estimate)
  }
  
  # Create comparison dataframe
  rmse_values <- c(
    get_rmse(lm_results),
    get_rmse(rf_results),
    get_rmse(elastic_net_results),
    get_rmse(mars_results)
  )
  
  rsq_values <- c(
    get_rsq(lm_results),
    get_rsq(rf_results),
    get_rsq(elastic_net_results),
    get_rsq(mars_results)
  )
  
  # Create and return the comparison dataframe
  comparison_df <- data.frame(
    Model = model_names,
    RMSE = round(rmse_values, 2),
    R_squared = round(rsq_values, 2)
  )
  
  return(comparison_df)
}

# Run function to compare all models
comparison_results <- compare_models(
  lm_results,
  rf_results,
  elastic_net_results,
  mars_results
)

# Sort by RMSE (ascending)
comparison_results %>%
  arrange(RMSE)

################################################################################
# 4. Results and Discussion
################################################################################

# The results interpretation and discussion can be round in the .Rmd file.
# But here's a summary of the key findings:

# 4.1 Correlation Analysis Findings:
# - Code violations showed the strongest correlation with violent incidents (r = 0.74, p < 0.001)
# - Streetlight outages (r = 0.69, p < 0.001) and abandoned vehicles (r = 0.65, p < 0.001) 
#   also strongly correlated with violent incidents
# - Lot abatements showed a weaker correlation (r = 0.16, p = 0.18)

# 4.2 Predictive Model Performance:
# - Random forest model performed best (RMSE = 211.16, R-squared = 0.76)
# - Linear regression (RMSE = 218.75, R-squared = 0.64) and elastic net 
#   (RMSE = 220.75, R-squared = 0.63) performed similarly
# - MARS model under-performed compared to other approaches

# 4.3 Spatial Patterns:
# - Certain neighborhoods showed higher concentrations of both violent incidents
#   and urban disorder indicators
# - This spatial clustering suggests targeted interventions might be effective

# 4.4 Implications:
# - Streetlight outages and code violations are valuable predictors of neighborhood violence
# - Infrastructure maintenance may play a role in crime prevention
# - Resources for environmental remediation and crime prevention might be most
#   efficiently allocated to high-risk neighborhoods

################################################################################
# 5. Conclusion
################################################################################

# This analysis identified significant relationships between urban disorder indicators
# and violent incidents in New Orleans neighborhoods. Predictive models achieved
# reasonable accuracy, explaining up to 76% of the variance in violent incidents
# across neighborhoods.

# Limitations include:
# - Correlation vs. causation considerations
# - Data quality and potential under-reporting
# - Temporal dynamics not captured in this cross-sectional analysis
# - Socioeconomic factors not included in the models

# Future research directions:
# - Longitudinal analysis to examine changes over time
# - Incorporating additional socioeconomic variables
# - More advanced machine learning techniques
# - Analysis at smaller spatial units (e.g., census blocks)

################################################################################
# End of Script
################################################################################