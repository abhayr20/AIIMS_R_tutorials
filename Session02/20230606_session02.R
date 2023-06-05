#READ excel data
#######################################################################
setwd("~/Documents/GitHub/AIIMS_R_tutorials/Session02")
pt_data <- read.csv("~/Documents/GitHub/AIIMS_R_tutorials/Session02/session02_data.csv")
#######################################################################


#INSTALLING AWND LOADING PACKAGES
#######################################################################
#install.packages('tidyverse')
library(tidyverse)
library(ggrepel)
#######################################################################


#DATA EXPLORATION
#######################################################################
#Recommendation: Always keep variable names in snake_case !!
glimpse(pt_data)
dim(pt_data)
names(pt_data)
class(pt_data$income_status)

#useful for numeric variables
summary(pt_data$pt_age) 

#useful for categorical vairable
table(pt_data$pt_gender) 
table(pt_data$pt_gender, pt_data$histology)

# Dealing with categorical variables using factors
pt_data$pt_gender <- as.factor(pt_data$pt_gender)
class(pt_data$pt_gender)
levels(pt_data$pt_gender)

pt_data$stage <- factor(pt_data$stage, ordered = T, 
                     levels = c("stage_1", "stage_2", "stage_3", "stage_4"),
                    # labels = c('I','II','III','IV'))
                    labels = c(1,2,3,4))
levels(pt_data$stage)
class(pt_data$stage)
#######################################################################


#DYPLR package: Manipulating dataframes
#######################################################################
# 1. Rename : Change the column names
#Format for assignment
pt_data <- pt_data %>%
  rename(egfr_status = EGFR_status)

# 2. Mutate : add more columns to the pt_dataframe
pt_data <- pt_data %>%
  mutate(smoking_index = smoke_years * smoke_qnt) %>%
  mutate(pack_years = smoke_years * smoke_qnt/20)

# 3. Select : Choose certain columns of the pt_dataframe
pt_data %>%
  select(pt_irch, smoking_status)

# 4. Filter : choose rows according to a logical conditon
pt_data %>%
  filter(smoking_status == 'current_smoker')

pt_data %>% 
  filter(egfr_status == 'positive' & pt_gender == 'female')

pt_data %>% 
  filter(smoking_status %in% c("reformed_smoker", "current_smoker"))
#######################################################################


#SAVE changes into a new excel
#######################################################################
getwd()
write.csv(pt_data, "session02_pt_data_new.csv", row.names = F)
#######################################################################


#GGPLOT package: Grammar of Graphics
#######################################################################
# Example 1: Visualize patients smoking habits...
smokers <- pt_data %>%
  filter(smoking_status != 'never_smoker')

smoker_habit <- ggplot(data = smokers,
       mapping = aes(x = pt_age,
                     y = smoking_index,
                     color = pt_gender,
                     shape = smoking_status,
                     size = smoke_qnt)) +
  geom_point() +
  geom_text_repel(aes(label = pt_irch), color = "black", size = 4, box.padding = 0.5)

# Saving the plot 
getwd()
ggsave(smoker_habit, filename = "smoker_habit.png")


# Example 2: Visualize smoking habits with relation to income status...
smoker_income <- ggplot(data = smokers,
       mapping = aes (x = income_status,
                      y = smoking_index)) + 
  geom_boxplot() 

ggsave(smoker_income, filename = "smoker_income.png")


# Example 3: Visualize patient gender and disease stage distribution...
ggplot(data = pt_data, mapping = aes(x = pt_gender, fill = stage)) +
  geom_histogram(stat = "count", color = "white", width = 0.5) +
  labs(title = "Frequency of Patient's Gender by Stage", x = "Gender", y = "Frequency") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

#######################################################################