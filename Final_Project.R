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






# Creating index for bootstraps -------------------------------------------

# Randomly shuffling the data and dividing into train/test
clean_data_indexes <- sample(2, nrow(clean_data), 
                             replace = TRUE, prob = c(0.8,0.2))
clean_data_train <- clean_data[clean_data_indexes==1,]
clean_data_test <- clean_data[clean_data_indexes==2,]

set.seed(123)

# Generate indexes for 30 iterations
clean_data_indexes_list <- replicate(31, sample(2, 
                                                nrow(clean_data), 
                                                replace = TRUE, 
                                                prob = c(0.8, 0.2)), 
                                     simplify = FALSE)

clean_data_train_list <- lapply(clean_data_indexes_list, function(index) clean_data_train[index == 1, ])
clean_data_testlist <- lapply(clean_data_indexes_list, function(index) clean_data_test[index == 2, ])


# Parallel ------------------------------------------------------------
unregister_dopar <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
}
unregister_dopar()

#initializing parallel processing
num_cores <- detectCores() - 2
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)

stopCluster(cl)
unregister_dopar()



# Fitting Random Forest ---------------------------------------------------
rf_gridsearch_red <- caret::train(quality ~ .,
                                  red_wine_rf_extra,
                                  method = "rf", 
                                  trControl = train_control, 
                                  tuneGrid = tune_grid,
                                  importance = TRUE)
plot(rf_gridsearch_red)

#Creating empty lists
accuracy_vector_red <- numeric(length(1:30))
conf_mat_list_red <- vector("list",length(1:30))
variable_importance_list_red <- vector("list",length(1:30))

tune_grid2 <- expand.grid(mtry = 2)  

#initializing parallel processing
num_cores <- detectCores() - 2
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)


results <- foreach (i = 1:length(oversampled_red_wine_train_list), 
                    .packages = c("caret", "dplyr")) %dopar% {
                        # Training the Random Forest model with 30 times
                        rf_model_red <- caret::train(
                            quality ~ .,
                            data = oversampled_red_wine_train_list[[i]],
                            method = "rf",
                            tuneGrid = tune_grid2,
                            importance = TRUE
                        )
                        
                        #Confusion Matrix of final model predicting Grade A red wine
                        predictions_red <- predict(rf_model_red, newdata = red_wine_test_list[[i]])
                        confusion_mat <- confusionMatrix(predictions_red, red_wine_test_list[[i]]$quality)
                        #conf_mat_list_red[[i]] <- confusion_mat
                        
                        accuracy_vector_red[i] <- confusion_mat$overall['Accuracy']
                        
                        var_importance <- varImp(rf_model_red, type = 2)  
                        variable_importance_list_red[[i]] <- var_importance
                        
                        list(
                            confusion_matrix = confusion_mat,
                            accuracy = confusion_mat$overall['Accuracy'],
                            variable_importance = var_importance
                        )
                    }
stopCluster(cl)

for (i in 1:length(results)) {
    conf_mat_list_red[[i]] <- results[[i]]$confusion_matrix
    accuracy_vector_red[i] <- results[[i]]$accuracy
    variable_importance_list_red[[i]] <- results[[i]]$variable_importance
}


cat("Creating 95% Confidence Interval for Accuracy of Model 
    predicting Grade A red wine")

mean_red2_vec  <- mean(accuracy_vector_red)

#standard error
std_error_red <- sd(accuracy_vector_red) / sqrt(length(accuracy_vector_red))

#critical t value for 95% CI
critical_value_red <- qt(0.975, df = length(accuracy_vector_red) - 1)

#confidence interval
lower_ci_red <- mean_red2_vec - (critical_value_red * std_error_red)
upper_ci_red <- mean_red2_vec + (critical_value_red * std_error_red)

# 95% CI
cat("95% Confidence Interval Predicting Grade A Red Wine: [", lower_ci_red, ", ", upper_ci_red, "]\n")


#Finding Index of accuracy value closest to mean
closest_index_red <- which.min(abs(accuracy_vector_red - mean_red2_vec))


#Confusion Matrix of Model closest to mean accuracy
print(conf_mat_list_red[closest_index_red])


#Variable Importance Plot of model 
plot(rf_gridsearch_red_importance, 
     main = "Variable Importance Ranked by Gini Impurity")


# Fitting XgBoost ---------------------------------------------------------
library(xgboost)
library(tidyverse)
library(data.table)
xgboost_columns10 <- c("falling21c", "twitches32c", "Age", "BMI", "appetite53c", "soreness15c","nap20c", "SF36_Pain",
                       "fever64c", "night55c", "severe_or_negligable")

Clean_SleepAggregate_xgboost <- Clean_SleepAggregate[, xgboost_columns, drop = FALSE ]


mean.impute = function(x){
    x = as.data.frame(x)
    for (i in 1:ncol(x)){
        x[which(x[,i]==-1),i] = NA
    }
    
    x = x %>% mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>% as.data.table()
    return(x)
}

median.impute = function(x){
    x = as.data.frame(x)
    for (i in 1:ncol(x)){
        x[which(x[,i]==-1),i] = NA
    }
    
    x = x %>% mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>% as.data.table()
    return(x)
}

Clean_SleepAggregate_xgboost_inputed <- Clean_SleepAggregate_xgboost


Clean_SleepAggregate_xgboost_inputed[1:2] = mean.impute(Clean_SleepAggregate_xgboost_inputed[1:2])
Clean_SleepAggregate_xgboost_inputed<- na.omit(Clean_SleepAggregate_xgboost_inputed)


set.seed(643)
parts = createDataPartition(Clean_SleepAggregate_xgboost_inputed$severe_or_negligable, p = .8, list = F)
xgboost_training = Clean_SleepAggregate_xgboost_inputed[parts, ]
xgboost_testing = Clean_SleepAggregate_xgboost_inputed[-parts, ]

#oversampling_xgboost <- ovun.sample(severe_or_negligable~., data = xgboost_training, method = "over", N = 3200)$data
#table(oversampling_xgboost$severe_or_negligable)

training_x = data.matrix(xgboost_training[, -71])
training_y = xgboost_training$severe_or_negligable

testing_x = data.matrix(xgboost_testing[, -71])
testing_y = xgboost_testing$severe_or_negligable

xgboost_train = xgb.DMatrix(data = training_x, label = training_y)
xgboost_test = xgb.DMatrix(data = testing_x, label = testing_y)

xgboost_watchlist = list(train = xgboost_train, test = xgboost_test)

xgboost_model = xgb.train(data = xgboost_train, max.depth= 6, watchlist = xgboost_watchlist, nrounds = 100,alpha=1,eta=0.2, 
                          colsample_bytree=1,subsample=1,min_child_weight=1,lambda=0,gamma=0)

xgboost_modeldf <- as.data.frame(xgboost_model$evaluation_log)
min_row <- xgboost_modeldf[which.min(xgboost_modeldf$test_rmse),]
low_iter <- min_row$iter

xgboost_model_final = xgb.train(data = xgboost_train, max.depth = 6, nrounds = low_iter, 
                                verbose = 0,alpha=1,lambda=0,gamma=0.1,eta=0.2)

xgboost_pred_y <- predict(xgboost_model_final, xgboost_test)

xgboost_threshold <- 0.18
xgboost_pred_y <- ifelse(xgboost_pred_y >= xgboost_threshold, 1, 0)

xgboost_pred_y <- factor(xgboost_pred_y, levels = c(0, 1))
xgboost_testing$severe_or_negligable <- factor(xgboost_testing$severe_or_negligable, levels = c(0, 1))

testing_predictions_xgboost <- confusionMatrix(xgboost_pred_y, xgboost_testing$severe_or_negligable)
testing_predictions_xgboost


evaluation_xgboost_model_actual <- data.frame(xgboost_testing$severe_or_negligable)
evaluation_xgboost_model <- data.frame(actual = evaluation_xgboost_model_actual, predicted = xgboost_pred_y)
evaluation_xgboost_model$xgboost_testing.severe_or_negligable <- as.numeric(evaluation_xgboost_model$xgboost_testing.severe_or_negligable)
evaluation_xgboost_model$predicted <- as.numeric(evaluation_xgboost_model$predicted)
xgboost_roc <- roc(evaluation_xgboost_model$xgboost_testing.severe_or_negligable, evaluation_xgboost_model$predicted)
xgboost_roc_plot <- plot(xgboost_roc ,  main = "53 DSQ Composites + Age ROC Curve", print.auc = TRUE)
xgboost_roc_plot


importance_matrix = xgb.importance(colnames(xgboost_train), model = xgboost_model_final)
importance_matrix
xgb.plot.importance(importance_matrix[1:20,])


# Fitting Naive Bayes -----------------------------------------------------




# Fitting Logistic Regression ---------------------------------------------
combo_five_training_set <- MECFS_and_Controls_Training_Set[, cbind("falling21f", "falling21s","nap20f", "nap20s", "smells66f", "smells66s", "jointpain26f", "jointpain26s", "musclepain25f",  "musclepain25s", "SleepReversalThreshold")]
combo_five_training_set <- data.frame(combo_five_training_set)
combo_five_model <- glm(SleepReversalThreshold ~ falling21f + falling21s + nap20f + nap20s + smells66f + smells66s + jointpain26f + jointpain26s + musclepain25f + musclepain25s, data = combo_five_training_set, family = binomial)
summary(combo_five_model)

combo_five_testing_set <- MECFS_and_Controls_Testing_Set[, cbind("falling21f", "falling21s","nap20f", "nap20s", "smells66f", "smells66s", "jointpain26f", "jointpain26s", "musclepain25f",  "musclepain25s", "SleepReversalThreshold")]
combo_five_testing_set <- data.frame(combo_five_testing_set)
combo_five_testing_predictions <- predict(combo_five_model, newdata = combo_five_testing_set, type = "response")
combo_five_binary_predictions <- ifelse(combo_five_testing_predictions > 0.165, 1, 0)
combo_five_binary_predictions <- factor(combo_five_binary_predictions)
combo_five_testing_set$SleepReversalThreshold <- factor(combo_five_testing_set$SleepReversalThreshold)
combo_five_confusion_matrix <- confusionMatrix(combo_five_binary_predictions, combo_five_testing_set$SleepReversalThreshold)
combo_five_confusion_matrix

evaluation_combo_five_model_actual <- data.frame(MECFS_and_Controls_Testing_Set$SleepReversalThreshold)
evaluation_combo_five_model <- data.frame(actual = evaluation_combo_five_model_actual, predicted = combo_five_testing_predictions)
combo_five_roc <- roc(evaluation_combo_five_model$MECFS_and_Controls_Testing_Set.SleepReversalThreshold, evaluation_combo_five_model$predicted)
combo_five_roc_plot <- plot(combo_five_roc ,  main = "ROC Curve", print.auc = TRUE)
combo_five_roc_plot



# Fitting KNN -------------------------------------------------------------
knn_train <- randomforest_train
knn_test <- randomforest_test

library(caret)

# Run algorithms using 10-fold cross validation
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
fit.knn <- caret::train(severe_or_negligable ~ ., data=knn_train, method="knn", metric=metric , trControl=trainControl)
knn.k1 <- fit.knn$bestTune # keep this Initial k for testing with knn() function in next section
print(fit.knn)
plot(fit.knn)

prediction <- predict(fit.knn, newdata = knn_test)
cf <- confusionMatrix(prediction, knn_test$severe_or_negligable)
print(cf)

fit.knn.k1 <- class::knn(train=knn_train[,-16], test=knn_test[,-16], cl=knn_train$severe_or_negligable, k=knn.k1)

cf <- confusionMatrix(knn_test$severe_or_negligable,fit.knn.k1)
cf

# Fitting SVM -------------------------------------------------------------
#SVM
grid <- expand.grid(C = 10^seq(-5,2,0.5))

# Fit the model
svm_grid <- train(type ~., data = wine, method = "svmLinear", 
                  trControl = train_control, tuneGrid = grid)
# View grid search result
svm_grid






