# Define variables

pt_age <-  sample(40 : 65, size = 100, replace = T)

pt_IRCH <- sample(270000 : 280000, size = 100, replace = F)

pt_sex <-  sample(c("Male", "Female"), 100, replace = T)

smoking_status <- sample(c("Never_smoker", "Reformed_smoker",  "Current_smoker"),
                         100, replace = T)

income_status <- sample(c("affording", "non_affording"), 100, replace = T)

histology <-sample(c("NSCLC", "SCC"), 100, replace = T, prob = c(0.85,0.15))


#Create dataframe
pt_data <- data.frame(pt_IRCH, pt_age, pt_sex, smoking_status, income_status, histology)


# for rows where histology is NSCLC assign subtype, for the rest put NA
pt_data$subtype <- NA 

for (i in 1:nrow(pt_data)) {
  if (pt_data$histology[i] == "NSCLC") {
    pt_data$subtype[i] <- sample(c("LUAD", "LUSC", "LCC"), 1, replace = T)
  }
  else {
    pt_data$subtype[i] <- "SCC"
  }
}

a1 <- table(pt_data$smoking_status, pt_data$pt_sex)


# update smoking status
for (i in 1:nrow(pt_data)) {
  if (pt_data$pt_sex[i] == "Male") {
    pt_data$smoking_status[i] <- sample(c("Never_smoker", "Reformed_smoker", "Current_smoker"),
                                        1,
                                        replace = TRUE,
                                        prob = c(0.30, 0.30, 0.40)
    )
  } else {
    pt_data$smoking_status[i] <- sample(c("Never_smoker", "Reformed_smoker", "Current_smoker"),
                                        1,
                                        replace = TRUE,
                                        prob = c(0.70, 0.10, 0.20)
    )
  }
}

# for NSCLC assign EGFR positivity status
pt_data$EGFR_status <- NA

for (i in 1:nrow(pt_data)) {
  if (pt_data$histology[i] == "NSCLC" && pt_data$pt_sex[i] == "Male") {
    pt_data$EGFR_status[i] <- sample(c("positive", "negative"), 
                                     1,
                                     replace = TRUE,
                                     prob = c(0.3, 0.7))
  } 
  else if (pt_data$histology[i] == "NSCLC" && pt_data$pt_sex[i] == "Female") {
    pt_data$EGFR_status[i] <- sample(c("positive", "negative"), 
                                     1,
                                     replace = TRUE,
                                     prob = c(0.6, 0.4))
  } 
  else {
    pt_data$EGFR_status[i] <- "NA"
  }
}


# create treatment for patients
pt_data$treatment <- NA

for (i in 1:nrow(pt_data)) {
  if (pt_data$EGFR_status[i] == "positive" && pt_data$income_status[i] == "affording") {
    pt_data$treatment[i] <- "osimertinib"
  }
  else if (pt_data$EGFR_status[i] == "positive" && pt_data$income_status[i] == "non_affording") {
    pt_data$treatment[i] <- "gefitinib"
  }
  else {
    pt_data$treatment[i] <- "chemo"
  }
}

#Create a new dataframe 
pt_EGFR <- pt_data[pt_data$EGFR_status == "positive",]

EGFR_list <- as.array(pt_EGFR$pt_IRCH)

EGFR_list2 <- as.array(pt_data$pt_IRCH[pt_data$EGFR_status == "positive",])
  


