# IMPORTS

# FIELDS

# This loads the original csv data
individual_final = read.csv("D:\\Virginia Tech\\DSPG\\2021_DSPG_Zimbabwe\\R_Testing_Atticus\\individual_final.csv", header = TRUE)
household_final = read.csv("D:\\Virginia Tech\\DSPG\\2021_DSPG_Zimbabwe\\R_Testing_Atticus\\household_final_1.csv", header = TRUE)

# This creates an empty list of all of the indicators

headings = c(
  "Household",
  "Urban/Rural",
  "District",
  "Primary School",
  "6-12 Not in School",
  "HH Member Chronically Ill",
  "Ill without Healthcare",
  "HH Member Unemployed",
  "No Electricity",
  "No Toilets",
  "Water Source",
  "Piped Water",
  "Energy Source",
  "HH Assets",
  "Land",
  "Animals",
  "Services"
)
indicators <- data.frame(matrix(ncol = length(headings), nrow = nrow(individual_final)))
colnames(indicators) <- headings


# FUNCTIONS

init_individual <- function(ind_data) {
  
  ind_data["edu_020"][is.na(ind_data["edu_020"])] <- 0
  
  return(ind_data)
}

# This function gets the deprivation for the question:
# "Nobody in the household completed primary school"
edu_primary_school <- function(indicators) {
  indicators["Primary School"][is.na(indicators["Primary School"])] <- 0
  indicators["Primary School"][individual_final["edu_020"] == 1] <- 1
  indicators["Primary School"][individual_final["edu_020"] == 3] <- 1
  indicators["Primary School"][individual_final["edu_039"] == 1] <- 1
  return(indicators)
}

# "The household has one child between 6 and 12 not enrolled in school"
edu_unenrolled <- function(indicators) {
  indicators['6-12 Not in School'][is.na(indicators['6-12 Not in School'])] <- 1
  indicators['6-12 Not in School'][individual_final["hhc_004"] < 6] <- 0
  indicators['6-12 Not in School'][individual_final["hhc_004"] > 12] <- 0
  indicators['6-12 Not in School'][individual_final["edu_021"] != 2] <- 0
  
  return(indicators)
}

# This function is responsible for checking to see if household conditions are met.
# For example, many questions deal with "Anyone in the household", "Someone in the
# household" or "No one in the household". This function goes through the individual
# Data and checks these conditions. 
# Categories:
# 'anyone', 'someone', 'no one'
household_filter <- function(indicators, criteria, category='anyone') {
  if (category == 'anyone') {
    household_list = c()
    for (i in nrow(indicators)) {
      
    }
  }
}

# PROCESSING


individual_final = init_individual(individual_final)
indicators = edu_primary_school(indicators)
indicators = edu_unenrolled(indicators)

print(indicators['6-12 Not in School'])

# PLOTS

