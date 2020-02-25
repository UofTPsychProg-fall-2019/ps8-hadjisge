### In this problem set, you will tidy up an IAT dataset 
### The original data is available at https://osf.io/szwuf/, but it comes as an SPSS .sav file
### I've included trimmed down .csv version of 2019's data in this repository for you to work with

# loading libraries  ---------------------------------------------
library(tidyverse)

# reading in IAT data  ---------------------------------------------

# use a tidyverse function to read in the included IAT_2019.csv file 
tbl <- read_csv("IAT.csv")

# Removing unnecessary rows and columns  ---------------------------------------------
# This data frame only contains 21 of the 454 available variables, but it's still too much

# use tidyverse functions so that only the following variables are included: 'session_id',"genderidentity","raceomb_002","D_biep.White_Good_all","Mn_RT_all_3467",
#       "edu_14","politicalid_7","STATE","att_7","tblacks_0to10","twhites_0to10","labels"

tbl_clean <- select(tbl, 1:12) #columns to select happen to be the first 12 columns

# next, clean up the rows 
# our primary dependent variable is D_biep.White_Good_all, but some subjects
# don't have any data. Remove the rows with missing D_biep.White_Good_all entries 
tbl_clean <- drop_na(tbl_clean, D_biep.White_Good_all)


# Renaming varialbles  ---------------------------------------------

# next rename variables with more intuitive, short labels 
# here are some suggestions (along with variable info)
# id : session_id (subject number)
# gender : genderidentity (gender 1 "Male" 2 "Female" 3 "Trans male/Trans man" 4 "Trans female/Trans woman" 5 "Genderqueer/Gender nonconforming" 6 "A different identity") 
# race : raceomb_002 (race: 1 "American Indian" 2 "East Asian" 3 "South Asian" 4 "Hawaiian Pacifica Islander" 5 "black Africian American" 6 "white" 7 "other" 8 "multiracial")
# bias :D_biep.White_Good_all (overall IAT score)
# rt : Mn_RT_all_3467 (overall reaction time)
# edu : edu_14 (education: 1 "elementary" 2 "junior high" 3 "some high school" 4 "HS grad" 5 "some college" 6 "associate's" 7 "bachelor's" 8 "some grad" 9 "MA" 10 "JD" 11 "MD" 12 "PHD" 13 "other advanced" 14 "MBA")
# pol : politicalid_7 (political identification: 1 "strongly conservative 7 "strongly liberal)
# state : STATE
# att : att_7 (race attitude 1 "strongly prefer AA" 7 "strongly prefer white")
# temp_b : tblacks_0to10 (temperature feelings black 1 "extremely cold" 10 "extremly warm")
# temp_w : twhites_0to10 (temperature feelings black 1 "extremely cold" 10 "extremly warm")

tbl_clean <- rename(tbl_clean,
                    id = session_id, 
                    race = raceomb_002,
                    bias = D_biep.White_Good_all,
                    rt = Mn_RT_all_3467,
                    edu = edu_14,
                    pol = politicalid_7,
                    state = STATE,
                    att = att_7,
                    temp_b = tblacks_0to10,
                    temp_w = twhites_0to10)

#  missing values  ---------------------------------------------  

summary(tbl_clean)
# some of our variables have missing values that aren't properly coded as missing  
# recode missing values in gender and state

tbl_clean$gender <- replace_na(tbl_clean$gender, "missing") #changing NA values to 'missing'

tbl_clean$state <- replace_na(tbl_clean$state, "missing") #changing NA values to 'missing'

# changing variable types  ---------------------------------------------  
# next, convert id and all variables that are character types to factors
# try to convert all variables at once using tidyverse functions
str(tbl_clean) #check the type of each variable
factorVar = c('id', 'gender', 'state')
tbl_clean <- mutate_at(tbl_clean, factorVar, ~factor(.)) #selecting only the variables in factorVar to convert
tbl_clean[sapply(tbl_clean, class) == 'factor'] #checking whether variables were converted properly

# recoding variables  ---------------------------------------------  
# participants were instructed to select all the gender idenities that apply to them
# this results in a lot of combinations!
# this pipeline tabulates the number of participants who endorse different gender identities. 
gender_count <- tbl_clean %>% group_by(gender) %>% tally()  

# sort the output and then use indexing to print the 3 most common response (not inlcuding missing values)
gender_count <- arrange(gender_count, desc(n)) #sort output based on descending n values
gender_count <- filter(gender_count, gender != "missing") #remove the missing value
print(gender_count[(1:3),1]) #printing the most common responses

# create a new variable that recodes gender to have 4 levels: the 3 most common responses and the others collapsed together
# you can use the key provided on line 31 to understand the levels
# check out recode documentation to see if there's a trick for setting defaults values for unspecified rows
# *note that this excercise in data recoding doesn't reflect the instructors' views on gender identities...
tbl_clean$gender4 <- recode(tbl_clean$gender, "[1]" = "Male", "[2]" = "Female", "[5]" = "Genderqueer/Gender nonconforming", .default = "Other")

# Now take a look at how highest obtained education is coded (key on line 35)
edu_count <- tbl_clean %>% group_by(edu) %>% tally()  

#create a new variable that recodes education into: no highscool, some highschool, highschool graduate, some college, postsecondary degree, masters (MA & MBA), advanced degree
#remember that the recode function isn't always the best solution for numeric variables
tbl_clean$edu7 <- recode(tbl_clean$edu, '1' = 'No HS', '2' = 'No HS', '3' = 'Some HS', 
                         '4' = 'HS Grad', '5' = 'Some college', '6' = 'PS degree', '7' = 'PS degree', '8' = 'PS degree',
                         '9' = 'Masters', '10' = 'Advanced Degree', '11' = 'Advanced Degree', '12' = 'Advanced Degree', '13' = 'Advanced Degree', '14' = 'Masters')

# mutating variables ---------------------------------------------  
# rewrite the above recoding steps so that they both occur within a single call of the mutate function
tbl_clean <- mutate(tbl_clean, gender4 = recode(tbl_clean$gender, "[1]" = "Male", "[2]" = "Female", "[5]" = "Genderqueer/Gender nonconforming", .default = "Other"),
                    edu7 = recode(tbl_clean$edu, '1' = 'No HS', '2' = 'No HS', '3' = 'Some HS', '4' = 'HS Grad', '5' = 'Some college', 
                    '6' = 'PS degree', '7' = 'PS degree', '8' = 'PS degree', '9' = 'Masters', '10' = 'Advanced Degree', '11' = 'Advanced Degree',
                    '12' = 'Advanced Degree', '13' = 'Advanced Degree', '14' = 'Masters'))

  
# filtering and math ---------------------------------------------  

# using filtering, calculate and print the mean bias score for:

# white men
white_men_bias <- filter(tbl_clean, gender4 == 'Male' & race == '6')
print(mean(white_men_bias$bias))

# white women
white_women_bias <- filter(tbl_clean, gender4 == 'Female' & race == '6')
print(mean(white_women_bias$bias))

# advanced degree holders who are men
advanced_men <- filter(tbl_clean, gender4 == 'Male' & edu7 == 'Advanced Degree')
print(mean(advanced_men$bias))

# high school graduates who are men
highschool_men <- filter(tbl_clean, gender4 == 'Male' & edu7 == 'HS Grad')
print(mean(highschool_men$bias))




