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
raw_subset$date_occur_report_difference <- as.numeric(difftime(raw_subset$date_report , 
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
  return(head(df,20))
}


summary_tables_top20(raw_subset$Crm.Cd.Desc,raw_subset$Crm.Cd)

summary_tables_top20(raw_subset$Premis.Desc, raw_subset$Premis.Cd)

summary_tables_top20(raw_subset$Weapon.Desc, raw_subset$Weapon.Used.Cd)

summary_tables_top20(raw_subset$Status.Desc,raw_subset$Status)



summary_tables_top20(raw_subset$Status.Desc,raw_subset$Status)
summary_tables_top20(raw_subset$Status.Desc,raw_subset$Status)
summary_tables_top20(raw_subset$Status.Desc,raw_subset$Status)
summary_tables_top20(raw_subset$Status.Desc,raw_subset$Status)





columns_to_subset <- c("Gender", "Poverty", "BMI_WHO", "Diabetes", "DirectChol", 
                       "SleepHrsNight", "PhysActiveDays", "AlcoholDay", "AlcoholYear", "SmokeNow", "Age", "Race1")

str(raw_subset)

# Count Plots -------------------------------------------------------------
#Geo Area Dict + Visualization

geo_area_dict <- setNames(raw_subset$AREA.NAME,raw_subset$AREA)
geo_area_df <- data.frame(
  key = names(geo_area_dict),
  Geographic_Area = unname(geo_area_dict)
) %>%
  group_by(key, Geographic_Area) %>%
  summarize(frequency = n(), .groups = "drop")

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



