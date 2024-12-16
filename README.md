# **Predicting Resolved and Unresolved Criminal Cases Using Logistic Regression**
This project focuses on building a predictive model that can predict whether crimes reported by the Los Angeles Police Department (LAPD) will be resolved or remain unresolved. 

## **Dataset**
This analysis is centered around crime data provided by the LAPD which can be found here: [https://catalog.data.gov/dataset/crime-data-from-2010-to-2019](https://data.lacity.org/Public-Safety/Crime-Data-from-2010-to-2019/63jg-8b9z/about_data)

Data dictionaries can be found here:
https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8/about_data

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


## **Methodology**
The project involves several steps, including data cleaning, exploratory data analysis (EDA), and the implementation of machine learning models to build predictive model for prediction.

### **Data Cleaning and Re-coding**
- **New Variables:**
  - `date_occur_report_difference`: Number of days between the date occurred and date reported
  - `time_occurr_cat`: Time of occurrence categorized into Morning, Afternoon, Evening, and Night
  - `Legal_Action`: Binary outcome variable representing case resolution (1 for resolved, 0 for unresolved)

- **Subsetting:**
  - Focus on the 50 most common crimes, premises, and 10 most common weapons used
  - Remove rows with victim ages ≤ 0 or > 100 and non-binary victim sex values

### **Machine Learning Models**
Three models were implemented and compared:
1. **Logistic Regression**
2. **Neural Network**
3. **Naive Bayes**

#### **Computation**
- Train and test datasets were partitioned into 31 smaller datasets due to the large size of the data.
- Bootstrapping was used to address class imbalance.
- Parallel processing was employed to speed up computation time.
- Evaluation metrics included accuracy, AUC (Area Under the Curve), and confusion matrices.

## **Results**
- **Neural Network Model:** Achieved a mean accuracy rate of around 75%.
- **Logistic Regression Model:** Achieved a mean accuracy rate of about 76%.
- **Naive Bayes Model:** Achieved an accuracy rate of about 68%.

## **Conclusion**
Both the logistic regression and neural network models were effective in predicting whether a case was resolved at the time of reporting. This project serves as a starting point for further research into patterns of unresolved cases to aid the LAPD in resource allocation and decision-making.
