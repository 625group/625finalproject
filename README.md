# **Predicting Resolved and Unresolved Criminal Cases Using Logistic Regression**
This project focuses on building a predictive model that can predict whether crimes reported by the Los Angeles Police Department (LAPD) will be resolved or remain unresolved. 

## **Dataset**
This analysis is centered around crime data provided by the LAPD which can be found here: [https://catalog.data.gov/dataset/crime-data-from-2010-to-2019](https://data.lacity.org/Public-Safety/Crime-Data-from-2010-to-2019/63jg-8b9z/about_data)

Data dictionaries can be found here: https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8/about_data

## **Methodology**
The project involves several steps, including data cleaning, exploratory data analysis (EDA), and the implementation of machine learning models to build predictive model for prediction.

## **Data Description**
The raw data consists of 2,171,512 observations and 28 variables. Key predictors include:

- **AREA:** Geographic area in Los Angeles
- **Part 1-2:** Whether LAPD reported the crime to the FBI
- **Crm Cd:** Code representing the crime committed
- **Vict Age:** Age of the victim at the time of the crime
- **Vict Sex:** Sex of the victim
- **Premis Cd:** Type of structure, vehicle, or location where the crime took place
- **Weapon Used Cd:** The type of weapon used in the crime.
- **Victim Descent Description:** Race/Ethnicity of the victim
- **Time Occurred:** Time of occurrence in 24-hour military time
- **Date Occurred:** Date of occurrence (MM/DD/YYYY)
- **Date Reported:** Date of report (MM/DD/YYYY)
- **Status:** Status of the case
