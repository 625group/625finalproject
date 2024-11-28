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



# Reading in Mocodes PDF --------------------------------------------------

# Read text from the PDF
pdf_text <- pdf_text("/Users/rpravin/Downloads/MO_CODES_Numerical_20191119 (1).pdf")

# Split the text into lines
lines <- unlist(strsplit(pdf_text, "\n"))
head(lines,200)
# Extract lines matching the pattern of MO codes
mo_data <- grep("^\\s*\\d{4}\\s+.*", lines, value = TRUE)

# Split each line into code and description
mo_split <- strsplit(mo_data, " ", fixed = TRUE)

# Separate codes and descriptions
mo_codes <- sapply(mo_split, `[`, 1)
mo_descriptions <- sapply(mo_split, function(x) paste(x[-1], collapse = " "))

# Create the dictionary
mo_dictionary <- setNames(mo_descriptions, mo_codes)

mo_dict_df <- data.frame(
    code = substr(mo_dictionary, 1, 6),                    
    description = trimws(substr(mo_dictionary, 7, nchar(mo_dictionary))),  
    stringsAsFactors = FALSE
)

Mo_top_categories <- head(sort(table(raw_subset$Mocodes), decreasing = TRUE), 50)
Mo_top_categories[1:10]

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

#Identified and cleaning Null Race and "-" Race
filtered_subset9 <- filtered_subset8[filtered_subset8$Vict.Descent != "-",]

#Omitting Nulls
filtered_subset10 <- na.omit(filtered_subset9)

clean_data <- filtered_subset10



# EDA: Count Plots for Categorical -------------------------------------------------------------


ggplot(geo_area_df, aes(x = Geographic_Area, y = frequency)) +
    geom_bar(stat = "identity", position = "dodge", color = "black", fill = "skyblue") +
    labs(title = "Count Plot", x = "Geographic Area", y = "Count") +
    theme_minimal()



# Descent Code: A - Other Asian B - Black C - 
#   Chinese D - Cambodian F - Filipino G - 
#   Guamanian H - Hispanic/Latin/Mexican I - 
#   American Indian/Alaskan Native J - 
#   Japanese K - Korean L - Laotian O - 
#   Other P - Pacific Islander S - Samoan U - 
#   Hawaiian V - Vietnamese W - White X - 
#   Unknown Z - Asian Indian











# Fitting Random Forest ---------------------------------------------------



# Fitting XgBoost ---------------------------------------------------------



# Fitting Naive Bayes -----------------------------------------------------




# Fitting Logistic Regression ---------------------------------------------


# Fitting KNN -------------------------------------------------------------


# Fitting SVM -------------------------------------------------------------







