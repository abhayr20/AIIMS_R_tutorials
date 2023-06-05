# Vectors
vtr1 = c(TRUE, FALSE)
vtr2 = c(1,234,664)
class(vtr1)

vtr5 = c(4,TRUE,"Abhay")
vtr6 = c(3.4L, 3, TRUE)


# Matrix
mtr1 = matrix(c(5:29), 5, 5)
class(mtr1)

# Array
arr1 = array(c(0:15), dim = c(4,4,2,2))
arr2 = array(c(1:9), dim = c(3,3,4,2))
class(arr1)

# Lists
lst1 = list("abhay", 25, "MS", TRUE)
lst2 = list(vtr1, vtr2, mtr1)
class(lst1)

#Exercises:
# 1. Create a numeric vector of 100 random IRCH numbers between 280000 and 290000
# 2. Create a numeric vector of 100 random age between 30 and 75
# 3. Create character vectors of 100 random sex, income_status and histology

pt_IRCH = sample(280000:290000, 100, replace = F)
pt_age = sample(30 : 75, 100, replace = T)

pt_gender = sample(c("Male", "Female"), 100, replace = T, prob = c(0.6, 0.4))
income_status = sample(c("Affording", "Non_affording"), 100, replace = T, prob = c(0.1,0.9))
histology <-sample(c("NSCLC", "SCLC"), 100, replace = T, prob = c(0.85,0.15))


# Dataframes
pt_data <- data.frame(pt_IRCH, pt_age, pt_gender, income_status, histology)


#Exercises to manipulate dataframes

# 1. Print all patients below age of 40
# 2. Print all females with SCC
# 3. Print a list of all affording NSCLC patients
# 4. Randomly pick 10 patients using IRCH

young_onset <- pt_data[pt_data$pt_age < 40,]

female_SCC <- pt_data[pt_data$pt_gender == "Female" & pt_data$histology == "SCC",]

affording_NSCLC <- pt_data[pt_data$income_status == "Affording" & pt_data$histology == "NSCLC",]

rand_IRCH = sample(pt_IRCH, 10, replace = F)
pt_random <- pt_data[pt_data$pt_IRCH %in% rand_IRCH,]


# Programming: Teach using for loops and if statements
#Exercise: Print the square of numbers 1 to 10

j = c()
for (i in 1:10){
  j <- c(j, i^2)
}
print(j)

#Exercise: Print the cube of numbers 1 to 4 and square for greater than 5

k = c()
for (i in 1:10){
  if (i < 5){
    k <- c(k, i^3)
  }
  else {
    k <- c(k, i^2)
  }
}

#Exercise: Assign a new column for histology subtypes
pt_data$subtype <- NA

for (i in 1:nrow(pt_data)){
  if (pt_data$histology[i] == "NSCLC"){
    pt_data$subtype[i] = sample(c("LUAD", "LUSC", "LCC"))
  }
  else {
    pt_data$subtype[i] = "SCLC"
  }
}

table(pt_data$pt_gender,  pt_data$subtype, pt_data$income_status)

#Exercise: 
#Assign smoking_statis "Never_smoker", "Reformed_smoker", "Current_smoker" 
#prob_male (0.30, 0.30, 0.40) and prob_female(0.70, 0.10, 0.20)

pt_data$smoking_status <- NA

for (i in 1:nrow(pt_data)){
  if (pt_data$pt_gender[i] == "Male") {
    pt_data$smoking_status[i] = sample(c("Never_smoker", "Reformed_smoker", "Current_smoker" ), prob = c(0.30, 0.30, 0.40))
  }
  else {
    pt_data$smoking_status[i] = sample(c("Never_smoker", "Reformed_smoker", "Current_smoker" ), prob = c(0.70, 0.10, 0.20))
  }
}

#Exercise: 
#Assign EGFR_status "Positive", "Negative"
#prob NSCLC_smoker (0.1,0.9) NSCLC_Never_moker (0.4,0.6)

pt_data$EGFR_status <- NA

for (i in 1:nrow(pt_data)){
  if (pt_data$smoking_status[i] == "Never_smoker" ) {
    pt_data$EGFR_status[i] = sample(c("Positive", "Negative"), prob = c(0.4,0.6))
  }
  else {
    pt_data$EGFR_status[i] = sample(c("Positive", "Negative"), prob = c(0.1,0.9))
  }
}


#Correct to exclude SCLC patients
for (i in 1:nrow(pt_data)){
  if (pt_data$smoking_status[i] == "Never_smoker" && pt_data$histology[i] == "NSCLC") {
    pt_data$EGFR_status[i] = sample(c("Positive", "Negative"), prob = c(0.5,0.5))
  }
  else if (pt_data$histology[i] == "NSCLC") {
    pt_data$EGFR_status[i] = sample(c("Positive", "Negative"), prob = c(0.1,0.9))
  }
  else {
    pt_data$EGFR_status[i] = "NA"
  }
}

table(pt_data$pt_gender,pt_data$EGFR_status, pt_data$smoking_status)

#H.W. Exercise:
#Create a list of all EGFR positive patients
#Create a new dataframe with only EGFR positive patients
#Prescribe gefitinib and osimertinib based on income_status

EGFR_list <- pt_IRCH[pt_data$EGFR_status == "Positive"]

pt_EGFR = pt_data[pt_data$EGFR_status == "Positive",]

pt_EGFR$pt_treatment <- NA

for (i in 1:nrow(pt_EGFR)){
  if (pt_EGFR$income_status[i] == "Affording") {
    pt_EGFR$pt_treatment[i] = "Osimertinib"
  }
  else {
    pt_EGFR$pt_treatment[i] = "Gefitinib"
  }
}
