library(kknn)
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

#setwd("Downloads")
raw <- read.csv("Crime_Data_from_2010_to_2019_20241122.csv")




summary(raw)
raw_subset <- raw[2:24]
raw_subset <- raw_subset[,-20]

raw_subset$AREA <- as.factor(raw_subset$AREA)
raw_subset$AREA.NAME <- as.factor(raw_subset$AREA.NAME)
raw_subset$Rpt.Dist.No <- as.factor(raw_subset$Rpt.Dist.No)
raw_subset$Part.1.2 <- as.factor(raw_subset$Part.1.2)
raw_subset$Crm.Cd <- as.factor(raw_subset$Crm.Cd)
raw_subset$Crm.Cd.2 <- as.factor(raw_subset$Crm.Cd.2)
raw_subset$Crm.Cd.3 <- as.factor(raw_subset$Crm.Cd.3)
raw_subset$Crm.Cd.4 <- as.factor(raw_subset$Crm.Cd.4)
raw_subset$Mocodes <- as.factor(raw_subset$Mocodes)
raw_subset$Vict.Sex <- as.factor(raw_subset$Vict.Sex)
raw_subset$Vict.Descent <- as.factor(raw_subset$Vict.Descent)
raw_subset$Premis.Cd <- as.factor(raw_subset$Premis.Cd)
raw_subset$Weapon.Used.Cd <- as.factor(raw_subset$Weapon.Used.Cd)
raw_subset$Status <- as.factor(raw_subset$Status)

summary(raw_subset)


summary_tables_top20 <- function(key_input,value_input) {
  dict <- setNames(key_input,value_input)
  df <- data.frame(
    key = names(dict),
    value = unname(dict)
  ) %>%
    group_by(key, value) %>%
    summarize(frequency = n(), .groups = "drop") %>%
    arrange(desc(frequency))
  return(head(df,20))
}



summary_tables_top20(raw_subset$Crm.Cd.Desc,raw_subset$Crm.Cd)

summary_tables_top20(raw_subset$Premis.Desc, raw_subset$Premis.Cd)

summary_tables_top20(raw_subset$Weapon.Desc, raw_subset$Weapon.Used.Cd)

summary_tables_top20(raw_subset$Status.Desc,raw_subset$Status)


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



