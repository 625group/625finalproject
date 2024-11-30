library(dplyr)
library(corrplot)
library(Hmisc)
library(ggplot2)
library(psych)
library(GGally)
library(vioplot)
library(DescTools)
library(leaps)
library(tidyverse)
library(caret)
library(e1071)
library(rattle)
library(dplyr)
library(rpart)
library(stats)
library(factoextra)
library(MASS)
library(car)
library(tidyverse)
library(data.table)
library(skimr)
library(randomForest)
library(ROSE)
library(tuneRanger)
library(VSURF)
library(foreach)
library(doParallel)
library(pdftools)
library(KMEANS.KNN)

raw <- read.csv("/Users/rpravin/Downloads/Crime_Data_from_2010_to_2019_20241122.csv")

summary(raw)

# Getting rid of Records Number, Location(Street Address of Crime), 
# Cross Street(Cross Street of Rounded Address), Latitude, Longitude

raw_subset <- raw[2:24]

raw_subset$AREA <- as.factor(raw_subset$AREA)
raw_subset$AREA.NAME <- as.factor(raw_subset$AREA.NAME)
raw_subset$Part.1.2 <- as.factor(raw_subset$Part.1.2)
# raw_subset$Crm.Cd <- as.factor(raw_subset$Crm.Cd)
# raw_subset$Crm.Cd.2 <- as.factor(raw_subset$Crm.Cd.2)
# raw_subset$Crm.Cd.3 <- as.factor(raw_subset$Crm.Cd.3)
# raw_subset$Crm.Cd.4 <- as.factor(raw_subset$Crm.Cd.4)
# raw_subset$Mocodes <- as.factor(raw_subset$Mocodes)
raw_subset$Vict.Sex <- as.factor(raw_subset$Vict.Sex)
raw_subset$Vict.Descent <- as.factor(raw_subset$Vict.Descent)
# raw_subset$Premis.Cd <- as.factor(raw_subset$Premis.Cd)
# raw_subset$Weapon.Used.Cd <- as.factor(raw_subset$Weapon.Used.Cd)
raw_subset$Status <- as.factor(raw_subset$Status)
raw_subset$Status.Desc <- as.factor(raw_subset$Status.Desc)

table(raw$Status.Desc)

# Re-Coding Response:Status of Case ---------------------------------------


table(raw_subset$Status.Desc)

#Setting rows marked as UNK(unclear) to NA
raw_subset$Status.Desc[raw_subset$Status.Desc == "UNK"] <- NA

#Creating new Variable with our proposed Binary Outcome Legal Actions vs. No Legal Action
raw_subset$Legal_Action <- raw_subset$Status.Desc 
raw_subset$Legal_Action <- as.character(raw_subset$Legal_Action)
raw_subset$Legal_Action <- ifelse(raw_subset$Legal_Action == "Invest Cont", 0, 1)

table(raw_subset$Legal_Action)




# ReCoding Date & Coding Difference in Report Time ----------------------------------------
#Report Date
raw_subset$date_report <- raw_subset$Date.Rptd
raw_subset$date_report <- as.POSIXct(raw_subset$date_report, format = "%m/%d/%Y %I:%M:%S %p")
raw_subset$date_report <- as.Date(raw_subset$date_report)

#Occurence Date
raw_subset$date_occur <- raw_subset$DATE.OCC
raw_subset$date_occur <- as.POSIXct(raw_subset$date_occur, format = "%m/%d/%Y %I:%M:%S %p")
raw_subset$date_occur <- as.Date(raw_subset$date_occur)

#Creating New Column: Difference Between Report vs Occurence
raw_subset$date_occur_report_difference <- as.numeric(difftime(raw_subset$date_report, 
                                                               raw_subset$date_occur, 
                                                               units = "days"))



# Categorizing Time Occurred --------------------------------------------------
# Convert military time to string
raw_subset$military_time <- raw_subset$TIME.OCC 

#Time Ranges:
# Morning 5 am to 12 pm (noon)
# Afternoon 12 pm to 5 pm.
# Evening 5 pm to 9 pm.
# Night 9 pm to 4 am.

military_times_str <- sprintf("%04d", raw_subset$military_time)

hours <- as.integer(substr(military_times_str, 1, 2))

categories <- ifelse(hours >= 5 & hours < 12, "Morning",
                     ifelse(hours >= 12 & hours < 17, "Afternoon",
                            ifelse(hours >= 17 & hours < 21, "Evening", "Night")))

raw_subset$time_occur_cat <- categories




# Creating dictionaries to store descriptions of unique values and frequencies of each category -------------------------------------------------------

summary_tables_top20 <- function(key_input,value_input) {
  dict <- setNames(key_input,value_input)
  df <- data.frame(
    key = names(dict),
    value = unname(dict)
  ) %>%
    dplyr::group_by(key, value) %>%
    dplyr::summarize(frequency = n(), .groups = "drop") %>%
    arrange(desc(frequency))
  df_name <- paste0(gsub(".*\\$", "",deparse(substitute(value_input))))  
  assign(df_name, df, envir = .GlobalEnv)
  
  #Frequency Counts
  row_counts <- c(10,15, 20, 25, 50)
  total_rows <- nrow(df)
  
  cat("Cumulative sums of frequencies for the top categories:\n")
  for (n in row_counts) {
      if (n <= total_rows) {
          cat(paste0("Top ", n, " categories: ", sum(df$frequency[1:n]), "\n"))
      } else {
          cat(paste0("Top ", n, " categories: Not enough categories (only ", total_rows, " categories available).\n"))
      }
  }
  
  return(head(df,20))
}



summary_tables_top20(raw_subset$AREA.NAME, raw_subset$AREA)

summary_tables_top20(raw_subset$Crm.Cd.Desc,raw_subset$Crm.Cd)

summary_tables_top20(raw_subset$Premis.Desc, raw_subset$Premis.Cd)

summary_tables_top20(raw_subset$Weapon.Desc, raw_subset$Weapon.Used.Cd)




# Weapons NA Recode -------------------------------------------------------
#Creating new category None instead of NA for no weapon used
raw_subset$Weapon.Used.Cd <- as.character(raw_subset$Weapon.Used.Cd)
raw_subset$Weapon.Used.Cd[is.na(raw_subset$Weapon.Used.Cd) == T] <- "None"



# # Reading in Mocodes PDF --------------------------------------------------
# 
# # Read text from the PDF
# pdf_text <- pdf_text("/Users/rpravin/Downloads/MO_CODES_Numerical_20191119 (1).pdf")
# 
# # Split the text into lines
# lines <- unlist(strsplit(pdf_text, "\n"))
# head(lines,200)
# # Extract lines matching the pattern of MO codes
# mo_data <- grep("^\\s*\\d{4}\\s+.*", lines, value = TRUE)
# 
# # Split each line into code and description
# mo_split <- strsplit(mo_data, " ", fixed = TRUE)
# 
# # Separate codes and descriptions
# mo_codes <- sapply(mo_split, `[`, 1)
# mo_descriptions <- sapply(mo_split, function(x) paste(x[-1], collapse = " "))
# 
# # Create the dictionary
# mo_dictionary <- setNames(mo_descriptions, mo_codes)
# 
# mo_dict_df <- data.frame(
#     code = substr(mo_dictionary, 1, 6),                    
#     description = trimws(substr(mo_dictionary, 7, nchar(mo_dictionary))),  
#     stringsAsFactors = FALSE
# )
# 
# Mo_top_categories <- head(sort(table(raw_subset$Mocodes), decreasing = TRUE), 50)
# Mo_top_categories[1:10]
# 
# Subsetting Columns needed readying data for cleaning -----------------------------------------------


columns_to_subset <- c("AREA", "Rpt.Dist.No", "Part.1.2", "Crm.Cd", "Mocodes", 
                       "Vict.Age", "Vict.Sex", "Vict.Descent", "Premis.Cd", 
                       "Weapon.Used.Cd", "Status", "Legal_Action", 
                       "date_occur_report_difference", "time_occur_cat")

subset1 <- raw_subset[,columns_to_subset]

#Only including rows whose Crm.Cd is in top 50
crime_top_50_string_vec <- Crm.Cd$key[1:50]
filtered_subset2 <- subset1[subset1$Crm.Cd %in% crime_top_50_string_vec, ]


#Only including rows whose crime took place in Premise in top 50
premise_top_50_string_vec <- Premis.Cd$key[1:50]
filtered_subset3 <- filtered_subset2[filtered_subset2$Premis.Cd %in% premise_top_50_string_vec, ]


#Only including rows if weapon Used in top 10
weapon_top_10_string_vec <- Weapon.Used.Cd$key[1:10]
filtered_subset4 <- filtered_subset3[filtered_subset3$Weapon.Used.Cd %in% weapon_top_10_string_vec, ]

#Dropping Mocodes
filtered_subset5 <- filtered_subset4[, !(colnames(filtered_subset4) %in% "Mocodes")]

#Changing Column types
filtered_subset5$Rpt.Dist.No <- as.factor(filtered_subset5$Rpt.Dist.No)
filtered_subset5$Crm.Cd <- as.factor(filtered_subset5$Crm.Cd)
filtered_subset5$Premis.Cd <- as.factor(filtered_subset5$Premis.Cd)
filtered_subset5$Weapon.Used.Cd <- as.factor(filtered_subset5$Weapon.Used.Cd)
filtered_subset5$Legal_Action <- as.factor(filtered_subset5$Legal_Action)
filtered_subset5$time_occur_cat <- as.factor(filtered_subset5$time_occur_cat)



# Cleaning Data -----------------------------------------------------------
#Identified and cleaning Negative Ages, one age of 118, and sex:X
filtered_subset6 <- filtered_subset5[filtered_subset5$Vict.Age > 0,]
filtered_subset7 <- filtered_subset6[filtered_subset6$Vict.Age <= 100, ]
filtered_subset8 <- filtered_subset7[filtered_subset7$Vict.Sex == "F" | filtered_subset7$Vict.Sex == "M", ]

#Identified and cleaning Null Race and "-" Race entering Race
filtered_subset9 <- filtered_subset8[filtered_subset8$Vict.Descent != "-", ]
filtered_subset9$Vict.Descent.Description <- ifelse(
    filtered_subset9$Vict.Descent == "A",
    "Other Asian",ifelse(filtered_subset9$Vict.Descent == "B",
    "Black", ifelse(filtered_subset9$Vict.Descent == "C",
    "Chinese",ifelse(filtered_subset9$Vict.Descent == "D",
    "Cambodian",ifelse(filtered_subset9$Vict.Descent == "F",
     "Filipino",ifelse(filtered_subset9$Vict.Descent == "G",
      "Guamanian",ifelse(filtered_subset9$Vict.Descent == "H",
       "Hispanic/Latin/Mexican",
       ifelse(filtered_subset9$Vict.Descent == "I",
       "American Indian/Alaskan Native",
       ifelse(filtered_subset9$Vict.Descent == "J","Japanese",
        ifelse(filtered_subset9$Vict.Descent == "K","Korean",
        ifelse(filtered_subset9$Vict.Descent == "L","Laotian",
        ifelse(filtered_subset9$Vict.Descent == "O","Other",
        ifelse(filtered_subset9$Vict.Descent == "P",
        "Pacific Islander",
        ifelse(filtered_subset9$Vict.Descent == "S","Samoan",
        ifelse(filtered_subset9$Vict.Descent == "U",
        "Hawaiian",ifelse(filtered_subset9$Vict.Descent == "V",
        "Vietnamese",ifelse(filtered_subset9$Vict.Descent == "W",
         "White",ifelse(filtered_subset9$Vict.Descent == "X",NA,
         ifelse(filtered_subset9$Vict.Descent == "Z", "Asian Indian", NA)))))))))))))))))))

filtered_subset10 <- filtered_subset9[, !(colnames(filtered_subset9) %in% "Vict.Descent")]
filtered_subset10$Vict.Descent.Description <- as.factor(filtered_subset10$Vict.Descent.Description)

#Only Including those with sex M or F
filtered_subset11 <- filtered_subset10[filtered_subset10$Vict.Sex == "M" | filtered_subset10$Vict.Sex == "F",]

#Removing Sub-Areas as redundant to Geopgraphic Areas
filtered_subset12 <- filtered_subset11[, !(colnames(filtered_subset11) %in% "Rpt.Dist.No")]

#Removing Status as outcome coded into Legal Action
filtered_subset13 <- filtered_subset12[, !(colnames(filtered_subset12) %in% "Status")]


#Omitting Nulls
filtered_subset14 <- na.omit(filtered_subset13)

clean_data <- filtered_subset14


# EDA: Count Plots for Categorical -------------------------------------------------------------

#Number of Crimes Per Geographic Area
ggplot(clean_data, aes(x = AREA)) +
    geom_bar(color = "black", fill = "skyblue") +
    labs(title = "Number of Crimes Per Geographic Area", x = "Geographic Area", y = "Number of Crimes") +
    theme_minimal()


#Number of Crimes Per Part.1.2
ggplot(clean_data, aes(x = Part.1.2)) +
    geom_bar(color = "black", fill = "skyblue") +
    labs(title = "Number of Crimes Per Crime Classification", x = "Crime Classification", y = "Number of Crimes") +
    theme_minimal()

#Number of Crimes Per Crm.Cd
ggplot(clean_data, aes(x = Crm.Cd)) +
    geom_bar(position = position_dodge(width = 0.4),color = "black", fill = "skyblue") +
    theme_minimal() +    
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(title = "Number of Crimes Per Crime Code", x = "Crime Code", y = "Number of Crimes") 

#Number of Crimes Per Vict.Sex
ggplot(clean_data, aes(x = Vict.Sex)) +
    geom_bar(color = "black", fill = "skyblue") +
    labs(title = "Number of Crimes Per Victim Sex", x = "Victim Sex", y = "Number of Crimes") +
    theme_minimal()

#Number of Crimes Per Premis.Cd
ggplot(clean_data, aes(x = Premis.Cd)) +
    geom_bar(position = position_dodge(width = 0.4),color = "black", fill = "skyblue") +
    theme_minimal() +    
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(title = "Number of Crimes Per Premise(Structure) Type", x = "Premise(Structure) Type", y = "Number of Crimes") 

#Number of Crimes Per Weapon.Used.Cd
ggplot(clean_data, aes(x = Weapon.Used.Cd)) +
    geom_bar(color = "black", fill = "skyblue") +
    labs(title = "Number of Crimes Per Weapon Used", x = "Weapon Used", y = "Number of Crimes") +
    theme_minimal()

#Number of Crimes Per Case resolved or not
ggplot(clean_data, aes(x = Legal_Action)) +
    geom_bar(color = "black", fill = "skyblue") +
    labs(title = "Number of Crimes Per Case resolved or not", x = "Case resolved or not", y = "Number of Crimes") +
    theme_minimal()

#Number of Crimes Per time_occur_cat
ggplot(clean_data, aes(x = time_occur_cat)) +
    geom_bar(color = "black", fill = "skyblue") +
    labs(title = "Number of Crimes Per Time Range", x = "Time Range", y = "Number of Crimes") +
    theme_minimal()

#Number of Crimes Per Vict.Descent.Description
ggplot(clean_data, aes(x = Vict.Descent.Description)) +
    geom_bar(color = "black", fill = "skyblue") +  
    theme_minimal() +    
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(title = "Number of Crimes Per Victim Race/Ethnicity", x = "Victim Race/Ethnicity", y = "Number of Crimes") 


# EDA: Box Plots/Histograms for Continuous -------------------------------------------------------------

# Victim Age boxplot
ggplot(clean_data, aes(y = Vict.Age)) +
    geom_boxplot(fill = "steelblue") +
    labs(title = "Boxplot of Victim Ages", y = "Age") +
    theme_minimal()

# Victim Age histogram
ggplot(clean_data, aes(x = Vict.Age)) +
    geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
    labs(title = "Histogram", x = "Victim Age", y = "Count") +
    theme_minimal()


# Days Difference in Crime Occurence and Crime Reported boxplot
ggplot(clean_data, aes(y = date_occur_report_difference)) +
    geom_boxplot(fill = "steelblue") +
    labs(title = "Boxplot of Days Difference in Crime Occurence and Crime Reported", 
         y = "Days Difference in Crime Occurence and Crime Reported") +
    theme_minimal()

# Days Difference in Crime Occurrence and Crime Reported histogram
ggplot(clean_data, aes(x = date_occur_report_difference)) +
    geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
    labs(title = "Histogram", x = "Days Difference in Crime Occurence and Crime Reported", y = "Count") +
    theme_minimal()

#EDA Reveals date_occur_report_difference should be categorized
clean_data$date_occur_report_difference <- cut(clean_data$date_occur_report_difference,
                             breaks = c(0, 30, 60, 90, 180, 365, Inf),
                             labels = c("0-30", "31-60", "61-90", "91-180", "181-365", "365+"),
                             right = FALSE)

#Number of Crimes Per Days Difference in Crime Occurrence and Crime Reported Count Plot
ggplot(clean_data, aes(x = date_occur_report_difference)) +
    geom_bar(position = position_dodge(width = 0.4),color = "black", fill = "skyblue") +
    theme_minimal() +    
    labs(title = "Number of Crimes Per Days Difference in Crime Occurrence and Crime Reported Count Plot", 
         x = "Days Difference in Crime Occurrence and Crime Reported", y = "Number of Crimes") 

# EDA: Summary Stats ------------------------------------------------------
summary(clean_data)






# Subsetting bootstraps defining bootstrap function -------------------------------------------

set.seed(123)

# Randomly shuffling the data and dividing into train/test
clean_data_indexes <- sample(2, nrow(clean_data), 
                             replace = TRUE, prob = c(0.8,0.2))
clean_data_train <- clean_data[clean_data_indexes==1,]
clean_data_test <- clean_data[clean_data_indexes==2,]


#Subsetting Train into 31 datasets
clean_data_train$Group <- sample(1:31, size = nrow(clean_data_train), replace = T)
df_subsets_train <- split(clean_data_train, clean_data_train$Group)


# Define oversampling function
oversample_data <- function(data) {
    return(ovun.sample(Legal_Action ~ ., data = data, p=0.5)$data)
}



# Intializing Parallel and Bootstrapping ------------------------------------------------------------
unregister_dopar <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
}
unregister_dopar()

#initializing parallel processing
num_cores <- detectCores() - 1
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)



oversampled_data_list <- foreach(data = df_subsets_train, .packages = c("ROSE")) %dopar% {
    oversample_data(data)
}



# Fitting Random Forest ---------------------------------------------------
#Calling 31st dataset
extra_clean_train <- oversampled_data_list[[31]]
oversampled_data_list <- oversampled_data_list[-31]


# Define the grid of hyper-parameters to tune
tune_grid <- expand.grid(mtry = c(3,7,11))
tr_control <- trainControl(
    method = "cv",
    number = 5, 
    allowParallel = TRUE)

rf_gridsearch <- caret::train(Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + Vict.Sex + Premis.Cd + Weapon.Used.Cd + date_occur_report_difference + time_occur_cat + Vict.Descent.Description,
                              data = extra_clean_train, 
                              tuneGrid = tune_grid,
                             method = "rf",
                             trControl = tr_control,
                              importance = TRUE,
                             ntree = 100)

rf_elbow <- plot(rf_gridsearch)
rf_elbow



#Creating empty lists
accuracy_vector_rf <- vector("list",length(1:30))
conf_mat_list_rf <- vector("list",length(1:30))
variable_importance_list_rf <- vector("list",length(1:30))


tune_grid2 <- expand.grid(mtry = 11)  

tr_control2 <- trainControl(
    method = "none",
    allowParallel = TRUE)

results_rf <- foreach (i = 1:length(oversampled_data_list), 
                    .packages = c("caret", "dplyr")) %dopar% {
                        # Training the Random Forest model 30 times w/optimal parameters
                        rf_model <- caret::train(
                            Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + Vict.Sex + Premis.Cd + Weapon.Used.Cd + date_occur_report_difference + time_occur_cat + Vict.Descent.Description,
                            data = oversampled_data_list[[i]],
                            method = "rf",
                            tuneGrid = tune_grid2,
                            trControl = tr_control2,
                            importance = TRUE
                        )
                        
                        #Confusion Matrix of final model predicting Resolved Case
                        predictions_rf <- predict(rf_model, newdata = clean_data_testlist[[i]])
                        confusion_mat_rf <- confusionMatrix(predictions_rf, clean_data_testlist[[i]]$Legal_Action)

                        accuracy_vector_rf[i] <- confusion_mat_rf$overall['Accuracy']
                        
                        var_importance_rf <- varImp(rf_model, type = 2)  
                        variable_importance_list_rf[[i]] <- var_importance_rf
                        
                        list(
                            confusion_matrix = confusion_mat_rf,
                            accuracy = confusion_mat_rf$overall['Accuracy'],
                            variable_importance = var_importance_rf
                        )
                    }

foreach (i = 1:30) %dopar% {
    conf_mat_list_rf[[i]] <- results_rf[[i]]$confusion_matrix
    accuracy_vector_rf[i] <- results_rf[[i]]$accuracy
    variable_importance_list_rf[[i]] <- results_rf[[i]]$variable_importance
}


cat("Creating 95% Confidence Interval for Accuracy of Model 
    predicting if case was resolved")

mean_rf_vec  <- mean(accuracy_vector_rf)

#standard error
std_error_rf <- sd(accuracy_vector_rf) / sqrt(30)

#critical t value for 95% CI
critical_value_red <- qt(0.975, df = 30 - 1)

#confidence interval
lower_ci_rf <- mean_rf_vec - (critical_value_rf * std_error_rf)
upper_ci_rf <- mean_rf_vec + (critical_value_rf * std_error_rf)

# 95% CI
cat("95% Confidence Interval Predicting if Case Resolved: [", lower_ci_rf, ", ", upper_ci_rf, "]\n")


#Finding Index of accuracy value closest to mean
closest_index_rf <- which.min(abs(accuracy_vector_rf - mean_rf_vec))


#Confusion Matrix of Model closest to mean accuracy
print(conf_mat_list_rf[closest_index_rf])


#Variable Importance Plot of model 
plot(varImp(rf_gridsearch, type = 2), 
     main = "Variable Importance Ranked by Gini Impurity")


# Fitting Neural Net ---------------------------------------------------------



# Fitting Naive Bayes -----------------------------------------------------
library(ISLR)
library(caret)




# Fitting Logistic Regression ---------------------------------------------

# Fitting KNN -------------------------------------------------------------
tr_control_knn <- trainControl(method = "cv", number = 10)  # 5-fold cross-validation

# Train the kNN model with automatic tuning of k using tuneLength
knn_model <- train(Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + Vict.Sex + 
                       Premis.Cd + Weapon.Used.Cd + date_occur_report_difference + 
                       time_occur_cat + Vict.Descent.Description,
                   data = extra_clean_train,
                   method = "knn",
                   trControl = tr_control_knn,
                   tuneLength = 5)

#Checking for optimal # of neighbors vs accuracy
plot(knn_model, print.thres = 0.5, type="S")




results_knn <- foreach (i = 1:30, 
                       .packages = c("caret", "dplyr")) %dopar% {
                           # Training the knn model 30 times w/optimal parameters
                           
                           #Confusion Matrix of final model predicting Grade A red wine
                           predictions_knn <- predict(knn_model, newdata = clean_data_testlist[[i]])
                           confusion_mat_knn <- confusionMatrix(predictions_knn, clean_data_testlist[[i]]$Legal_Action)

                           accuracy_vector_knn[i] <- confusion_mat_knn$overall['Accuracy']
                           
                           var_importance_knn <- varImp(knn_model, type = 2)  
                           variable_importance_list_knn[[i]] <- var_importance_knn
                           
                           list(
                               confusion_matrix = confusion_mat_knn,
                               accuracy = confusion_mat_knn$overall['Accuracy'],
                               variable_importance = var_importance_knn
                           )
                       }

foreach (i = 1:30) %dopar% {
    confusion_mat_knn[[i]] <- results_knn[[i]]$confusion_matrix
    accuracy_vector_knn[i] <- results_knn[[i]]$accuracy
    variable_importance_list_knn[[i]] <- results_knn[[i]]$variable_importance
}


cat("Creating 95% Confidence Interval for Accuracy of Model 
    predicting if case was resolved")

mean_knn_vec  <- mean(accuracy_vector_knn)

#standard error
std_error_knn <- sd(accuracy_vector_knn) / sqrt(30)

#critical t value for 95% CI
critical_value_knn <- qt(0.975, df = 30 - 1)

#confidence interval
lower_ci_knn <- mean_knn_vec - (critical_value_knn * std_error_knn)
upper_ci_knn <- mean_knn_vec + (critical_value_knn * std_error_knn)

# 95% CI
cat("95% Confidence Interval Predicting if Case Resolved: [", lower_ci_knn, ", ", upper_ci_knn, "]\n")


#Finding Index of accuracy value closest to mean
closest_index_knn <- which.min(abs(accuracy_vector_knn - mean_knn_vec))


#Confusion Matrix of Model closest to mean accuracy
print(conf_mat_list_knn[closest_index_knn])







# Fitting SVM -------------------------------------------------------------
#SVM
svm_grid <- expand.grid(C = 10^seq(-5,2,0.5))

# Fit the model
svm_grid <- caret::train(type ~., data = wine, method = "svmLinear", 
                  trControl = train_control, tuneGrid = grid)
# View grid search result
svm_grid





stopCluster(cl)
unregister_dopar()

