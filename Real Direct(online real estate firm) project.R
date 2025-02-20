# Load necessary libraries
library(readxl) 
library(dplyr) 
library(caret) 
library(randomForest) 
library(ggplot2) 
library(corrplot) 
library(Metrics) 
library(lmtest)

# 1. **Data Collection and Loading** 
file_path <- "C:/Users/91897/OneDrive/Desktop/rollingsales_manhattan.xls" 
real_estate_data <- read_excel(file_path)

# Check column names and structure of the data
print(colnames(real_estate_data))
str(real_estate_data)

# Clean column names (remove extra spaces and assign meaningful names)
colnames(real_estate_data) <- gsub("\\s+", "_", gsub("[^[:alnum:]_]", "", colnames(real_estate_data)))

# Rename columns for better readability
colnames(real_estate_data) <- c("File_Info", "Borough", "Building_Class_Category", 
                                "Tax_Class_Present", "Block", "Lot", "Easement", 
                                "Building_Class_Present", "Address", "Apartment_Number", 
                                "Zip_Code", "Residential_Units", "Commercial_Units", 
                                "Total_Units", "Land_Square_Feet", "Gross_Square_Feet", 
                                "Year_Built", "Tax_Class_Sale", "Building_Class_Sale", 
                                "Sale_Price", "Sale_Date") 

# 2. **Data Preprocessing** 
# Clean and transform variables (convert strings to numbers and handle missing values)
real_estate_data_clean <- real_estate_data %>% 
  select(-Block, -Lot, -Easement, -Address, -Apartment_Number, -Tax_Class_Sale, 
         -Building_Class_Sale) %>% 
  mutate(
    Sale_Price = as.numeric(gsub(",", "", gsub("\\$", "", Sale_Price))),
    Gross_Square_Feet = as.numeric(gsub(",", "", gsub("\\$", "", Gross_Square_Feet))), 
    Residential_Units = as.numeric(Residential_Units), 
    Commercial_Units = as.numeric(Commercial_Units), 
    Land_Square_Feet = as.numeric(Land_Square_Feet), 
    Year_Built = as.numeric(Year_Built)
  ) %>% 
  mutate(
    Sale_Price = ifelse(is.na(Sale_Price), median(Sale_Price, na.rm = TRUE), Sale_Price), 
    Gross_Square_Feet = ifelse(is.na(Gross_Square_Feet), median(Gross_Square_Feet, na.rm = TRUE), Gross_Square_Feet), 
    Residential_Units = ifelse(is.na(Residential_Units), median(Residential_Units, na.rm = TRUE), Residential_Units), 
    Commercial_Units = ifelse(is.na(Commercial_Units), median(Commercial_Units, na.rm = TRUE), Commercial_Units), 
    Land_Square_Feet = ifelse(is.na(Land_Square_Feet), median(Land_Square_Feet, na.rm = TRUE), Land_Square_Feet), 
    Year_Built = ifelse(is.na(Year_Built) | Year_Built < 1900 | Year_Built > 2013, 2013, Year_Built)
  ) %>% 
  filter(!is.na(Gross_Square_Feet))  # Ensure no missing values in critical columns 

# Function to remove outliers using the Interquartile Range (IQR) method
remove_outliers <- function(df, col_name) { 
  Q1 <- quantile(df[[col_name]], 0.25, na.rm = TRUE) 
  Q3 <- quantile(df[[col_name]], 0.75, na.rm = TRUE) 
  IQR <- Q3 - Q1 
  lower_bound <- Q1 - 1.5 * IQR 
  upper_bound <- Q3 + 1.5 * IQR 
  df %>% filter(df[[col_name]] >= lower_bound & df[[col_name]] <= upper_bound) 
}

# Apply outlier removal to columns of interest 
real_estate_data_clean <- real_estate_data_clean %>% 
  remove_outliers("Sale_Price") %>% 
  remove_outliers("Gross_Square_Feet") %>% 
  remove_outliers("Land_Square_Feet") %>% 
  remove_outliers("Residential_Units") %>% 
  remove_outliers("Commercial_Units") 

# **Check for missing values** 
missing_values <- colSums(is.na(real_estate_data_clean)) 
print(missing_values) 

# **Summary of real estate data** 
summary(real_estate_data_clean) 

# 3. **Exploratory Data Analysis (EDA)** 
# Scatter plot for Sale Price vs Gross Square Feet 
ggplot(real_estate_data_clean, aes(x = Gross_Square_Feet, y = Sale_Price)) + 
  geom_point(alpha = 0.5, color = "blue") + 
  labs(title = "Sale Price vs Gross Square Feet", x = "Gross Square Feet", y = "Sale Price") + 
  theme_minimal()

# Correlation plot for numerical features  
corr_data <- real_estate_data_clean %>% select(Sale_Price, Gross_Square_Feet, 
                                               Residential_Units, Commercial_Units, 
                                               Land_Square_Feet, Year_Built)  
cor_matrix <- cor(corr_data, use = "complete.obs")  # Use complete cases to avoid issues with NAs
corrplot(cor_matrix, method = "circle") 

# Boxplot for Sale Price by Borough 
ggplot(real_estate_data_clean, aes(x = Borough, y = Sale_Price, fill = Borough)) + 
  geom_boxplot() + 
  labs(title = "Sale Price Distribution by Borough", x = "Borough", y = "Sale Price") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 4. **Feature Selection and Model Building** 
# Shuffle the data and split into training and testing sets 
set.seed(2000)  # For reproducibility 
if (length(unique(real_estate_data_clean$Gross_Square_Feet)) > 1) {
  train_index <- createDataPartition(real_estate_data_clean$Sale_Price, p = 0.8, list = FALSE) 
  train_data <- real_estate_data_clean[train_index, ] 
  test_data <- real_estate_data_clean[-train_index, ] 
  
  # Remove rows with NA values from training and test sets 
  train_data <- na.omit(train_data) 
  test_data <- na.omit(test_data) 
  
  # Train the Random Forest model 
  rf_model <- randomForest(Sale_Price ~ Borough + Building_Class_Category + 
                             Residential_Units + Commercial_Units + 
                             Land_Square_Feet + Gross_Square_Feet + Year_Built,  
                           data = train_data,  
                           ntree = 100,  
                           mtry = 3,  
                           importance = TRUE) 
  
  # **Model Evaluation and Performance Metrics** 
  # Predictions on test data 
  rf_predictions <- predict(rf_model, newdata = test_data) 
  
  # Calculate R-squared for the predictions
  rf_r_squared <- cor(test_data$Sale_Price, rf_predictions)^2 
  cat("Random Forest R-squared: ", rf_r_squared, "\n") 
  
  # Calculate RMSE (Root Mean Squared Error)
  rf_rmse <- sqrt(mean((rf_predictions - test_data$Sale_Price)^2)) 
  cat("Random Forest RMSE: ", rf_rmse, "\n") 
  
  # Calculate MAE (Mean Absolute Error) 
  rf_mae <- mean(abs(rf_predictions - test_data$Sale_Price)) 
  cat("Random Forest MAE: ", rf_mae, "\n") 
  
  # Plot residuals to check model assumptions 
  residuals <- rf_predictions - test_data$Sale_Price 
  ggplot(data.frame(residuals), aes(x = residuals)) + 
    geom_histogram(binwidth = 5000, fill = "red", color = "black", alpha = 0.7) + 
    labs(title = "Residuals Distribution", x = "Residuals", y = "Frequency") + 
    theme_minimal() 
  
  # **Plot Feature Importance**  
  importance(rf_model) # Print feature importance values
  varImpPlot(rf_model) # Plot importance
  
  # **P-Value Test (ANOVA or T-test) to check feature significance** 
  # Linear model to check p-values for the features 
  lm_model <- lm(Sale_Price ~ Borough + Building_Class_Category + 
                   Residential_Units + Commercial_Units + 
                   Land_Square_Feet + Gross_Square_Feet + Year_Built,  
                 data = train_data) 
  
  # Summary of the linear model to view p-values 
  lm_summary <- summary(lm_model)
  print(lm_summary)
  
  # **Additional Test: Check if categorical features (Borough) significantly affect Sale Price** 
  anova_result <- aov(Sale_Price ~ Borough, data = real_estate_data_clean) 
  print(summary(anova_result))
  
  