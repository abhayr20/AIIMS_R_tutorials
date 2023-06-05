# Making fake clinical data for tutorials

pt_irch = sample(280000:290000, 100, replace = F)
pt_age = sample(30 : 75, 100, replace = T)
pt_gender = sample(c("male", "female"), 100, replace = T, prob = c(0.6, 0.4))
income_status = sample(c("affording", "non_affording"), 100, replace = T, prob = c(0.1,0.9))
fam_hist <- sample(c("positive", "negative"), 100, replace = T, prob = c(0.2, 0.9))
stage <- sample(c("stage_1", "stage_2", "stage_3", "stage_4"),  100, replace = T, prob = c(0.1, 0.2, 0.4, 0.3))
histology <-sample(c("NSCLC", "SCLC"), 100, replace = T, prob = c(0.85,0.15))


# Create the dataframe
pt_data <- data.frame(pt_irch, pt_age, pt_gender, income_status, fam_hist, stage, histology)


#Assign a new column for histology subtypes
pt_data$subtype <- NA

for (i in 1:nrow(pt_data)){
  if (pt_data$histology[i] == "NSCLC"){
    pt_data$subtype[i] = sample(c("LUAD", "LUSC", "LCC"))
  }
  else {
    pt_data$subtype[i] = "SCLC"
  }
}


#Define EGFR status
pt_data$EGFR_status <- NA


#Assign smoking_status "never_smoker", "reformed_smoker", "current_smoker" 
pt_data$smoking_status <- NA

for (i in 1:nrow(pt_data)){
  if (pt_data$pt_gender[i] == "male") {
    pt_data$smoking_status[i] = sample(c("never_smoker", "reformed_smoker", "current_smoker" ), prob = c(0.30, 0.30, 0.40))
  }
  else {
    pt_data$smoking_status[i] = sample(c("never_smoker", "reformed_smoker", "current_smoker" ), prob = c(0.70, 0.10, 0.20))
  }
}


#Assign EGFR_status "positive", "negative" excluding SCLC patients
for (i in 1:nrow(pt_data)){
  if (pt_data$smoking_status[i] == "never_smoker" && pt_data$histology[i] == "NSCLC") {
    pt_data$EGFR_status[i] = sample(c("positive", "negative"), prob = c(0.5,0.5))
  }
  else if (pt_data$histology[i] == "NSCLC") {
    pt_data$EGFR_status[i] = sample(c("positive", "negative"), prob = c(0.1,0.9))
  }
  else {
    pt_data$EGFR_status[i] = "not_tested"
  }
}


# Make smoke years and smoke quantity data
pt_data$smoke_years <- NA
pt_data$smoke_qnt <- NA

for (i in 1:nrow(pt_data)){
  if (pt_data$smoking_status[i] == "current_smoker") {
    pt_data$smoke_years[i] = abs(round(rnorm(100, 35, 10)))
    pt_data$smoke_qnt[i] = sample(c(5,10,15,20,25,30), 100, replace = T, 
                                  prob = c(0.1, 0.15, 0.3, 0.25, 0.1, 0.1))
  }
  else if (pt_data$smoking_status[i] == "reformed_smoker") {
    pt_data$smoke_years[i] = abs(round(rnorm(100, 20, 5)))
    pt_data$smoke_qnt[i] = sample(c(5,10,15,20,25,30), 100, replace = T, 
                                  prob = c(0.3, 0.2, 0.2, 0.1, 0.1, 0.1))
  }
  else {
    pt_data$smoke_years[i] = 0
    pt_data$smoke_qnt[i] = 0
  }
}

for (i in 1:nrow(pt_data)){
  if (pt_data$pt_age[i] <= 50) 
    pt_data$smoke_years[i] = pt_data$smoke_years[i]/2
}

# Save the data
setwd("~/Documents/GitHub/AIIMS_R_tutorials")
write.csv(pt_data, "Session02/session02_data.csv", row.names = F)

