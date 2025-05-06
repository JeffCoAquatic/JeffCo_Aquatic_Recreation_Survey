# load libraries
library(dplyr)
library(gt)
library(ggplot2)
library(here)
library(viridis)
library(webshot)
library(stringr)

############################################################################################################
## Load and Clean Data for Analysis
############################################################################################################

 survey.dat <- read.csv(here("data",'JAC_Survey_3.31.25.csv'))

############################################################################################################
# replace longer column names with more easily referenced names

colnames.og <- colnames(survey.dat) 

# create character vector of clearer column names for the resident survey questions (cols 1:93 (data) and 124(comments))
colnames.new <- c("Response.ID","IP.address","User.agent","Response.status","Survey.URL","Start.time","Completion.time","Time.taken",
                  "Collector","JeffCo.resident","City.area","Age","Under7","Age7_12","Age13_18","Age19_34","Age35_54","Age55-64","Age65_74","Age75_older","RateNewPoolImportance",
                  "Under7_Non.swimmer","Under7_Beginner","Under7_Intermediate","Under7_Advanced",
                  "Age7_12_Non.swimmer","Age7_12_Beginner","Age7_12_Intermediate","Age7_12_Advanced",
                  "Age13_18_Non.swimmer","Age13_18_Beginner","Age13_18_Intermediate","Age13_18_Advanced",
                  "Age19_34_Non.swimmer","Age19_34_Beginner","Age19_34_Intermediate","Age19_34_Advanced",
                  "Age35_54_Non.swimmer","Age35_54_Beginner","Age35_54_Intermediate","Age35_54_Advanced",
                  "Age55-64_Non.swimmer","Age55-64_Beginner","Age55-64_Intermediate","Age55-64_Advanced",
                  "Age65_74_Non.swimmer","Age65_74_Beginner","Age65_74_Intermediate","Age65_74_Advanced",
                  "Age75_older_Non.swimmer","Age75_older_Beginner","Age75_older_Intermediate","Age75_older_Advanced","UsedAquaticsPast2Years",
                  "WhichAquaticFacilities","FacilityUsageRate","AquaAmenities_LapSwimming","AquaAmenities_OpenSwim","AquaAmenities_YouthLessons",
                  "AquaAmenities_AdultLessons","AquaAmenities_CompetitiveSwim","AquaAmenities_LifeguardTrain","AquaAmenities_Aerobics",
                  "AquaAmenities_SyncroSwim","AquaAmenities_SportsLeagues","AquaAmenities_SafetyTrain","AquaAmenities_Therapeutic",
                  "AquaAmenities_PartyRental","AquaAmenities_ParentChild","AquaAmenities_Sauna","AquaAmenities_HotTub","AquaAmenities_PublicShower",
                  "AquaAmenities_OutdoorPool","AquaAmenities_SplashPark","AquaAmenities_UsageRate","RecAmenities_AfterschoolCamps",
                  "RecAmenities_Preschool","RecAmenities_Childcare","RecAmenities_Playgroups","RecAmenities_FitnessClass","RecAmenities_PhysTherapy",
                  "RecAmenities_EventSpace","RecAmenities_WellnessClass","RecAmenities_Kitchen","RecAmenities_Cafe","RecAmenities_Gym",
                  "RecAmenities_OutPickleball","RecAmenities_InPickleball","RecAmenities_RockClimb","RecAmenities_TeenFit","RecAmenities_TeenCenter",
                  "RecAmenities_UsageRate","TaxSupport","Comments")

# create character vector of clearer column names for the resident survey questions (cols 94:122 (data) and 123(comments))
colnames.nonres <- c("JeffCo_Visit_Freq","Visit_Season","Visit_Length","Visit_Reason","AquaAmenities_LapSwimming",
                     "AquaAmenities_SwimLessons","AquaAmenities_OpenSwim","AquaAmenities_Aerobics","AquaAmenities_SwimMeets",
                     "AquaAmenities_SafetyTrain","AquaAmenities_Therapeutic","AquaAmenities_ParentChild","AquaAmenities_Sauna",
                     "AquaAmenities_HotTub","AquaAmenities_PublicShower","AquaAmenities_OutdoorPool","AquaAmenities_SplashPark",
                     "RecAmenities_FitnessClass","RecAmenities_Gym","RecAmenities_OutPickleball","RecAmenities_InPickleball",
                     "RecAmenities_RockClimb","RecAmenities_Childcare","RecAmenities_DaycareSummerCamp","RecAmenities_YouthActivities",
                     "RecAmenities_TeenFit","RecAmenities_TeenCenter","RecAmenities_Cafe","AquaRecAmenities_UsageRate","Comments")

#Validate column name changes for resident survey (step completed and file saved to project)
#resident_names_old <- c(colnames.og[1:93],colnames.og[123])
#manually inspect each row to verify the renaming assignment
#resident_cols_check <- data.frame(resident_names_old,colnames.new)
#write.csv(resident_cols_check,file = "resident_cols_check.csv")

#Validate column name changes for non-resident survey (step completed and file saved to project)
#nonresident_names_old <- colnames.og[94:123]
#manually inspect each row to verify the renaming assignment
#nonresident_cols_check <- data.frame(nonresident_names_old,colnames.nonres)
#write.csv(nonresident_cols_check,file = "nonresident_cols_check.csv")

### Set Column Names and Remove Non-resident/tourist surveys
ResidentSurvey.dat <- survey.dat %>%
  {cbind(.[, 1:93], .[, 123])} %>%
  filter(Do.you.live.in.Jefferson.County.!="No, I am visiting")%>%
  setNames(colnames.new)
write.csv(ResidentSurvey.dat,file = "Resident_Survey_Responses_3.31.2025.csv")

# create nonresident survey data frame for future analysis
NonResidentSurvey.dat <- survey.dat %>%
  filter(Do.you.live.in.Jefferson.County.=="No, I am visiting")%>%
  select(94:123) %>%
  setNames(colnames.nonres)
write.csv(NonResidentSurvey.dat,file = "Nonresident_Survey_Responses_3.31.2025.csv")

# save comments and survey ID as separate data frame for later
Comment.dat <- ResidentSurvey.dat %>%
  select(Response.ID,Comments)

# Comments
write.csv(Comment.dat,"Comments_Final_3.31.25.csv")

# drop the comment column in working data frame due to length; can link data to Comment.dat using Response.ID
ResidentSurvey.dat <- ResidentSurvey.dat[, -c(3,5:9,94)] 

##########################################################################################################
## Review each age category for entry errors and non-numeric entries (this question was an open-entry format by necessity so entry errors are present)

unique(ResidentSurvey.dat$Under7)

ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(Under7_clean = case_when(
    str_to_upper(Under7) %in% c("", "0", "00", "O", "NONE") ~ NA_character_,
    str_detect(tolower(Under7), "due|baby") ~ NA_character_,
    str_to_upper(Under7) == "I" ~ "1",
    Under7 == "46" ~ "0",
    str_to_upper(Under7) == "2P" ~ "2",
    TRUE ~ Under7
  ),
  Under7_clean = as.numeric(Under7_clean))
unique(ResidentSurvey.dat$Under7_clean)

unique(ResidentSurvey.dat$Age7_12)

ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(Age7_12_clean = case_when(
    str_to_upper('7_12') %in% c("", "0", "00", "O", "None") ~ NA_character_,
    str_detect(tolower('7_12'), "due|baby") ~ NA_character_,
    TRUE ~ Age7_12
  ),
  Age7_12_clean = as.numeric(Age7_12_clean))


unique(ResidentSurvey.dat$`13_18`)
#[1] "1"    ""     "0"    "2"    "3"    "None" "O"    "00"   "5"    "1’"   "4" 
ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(Age13_18_clean = case_when(
    str_to_upper(Age13_18) %in% c("", "0", "00", "O", "None") ~ NA_character_,
    TRUE ~ Age13_18
  ),
Age13_18_clean = as.numeric(Age13_18_clean))
unique(ResidentSurvey.dat$Age13_18_clean)

unique(ResidentSurvey.dat$`19_34`)
#[1] ""     "0"    "1"    "2"    "3"    "One"  "None" "O"    "4" 
ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(Age19_34_clean = case_when(
    str_to_upper(Age19_34) %in% c("", "0", "00", "O", "One","None") ~ NA_character_,
    TRUE ~ Age19_34
  ),
  Age19_34_clean = as.numeric(Age19_34_clean))

unique(ResidentSurvey.dat$`35_54`)
[1] "2"    ""     "0"    "1"    "3"    "None" "O"    "I"    "5"    "4"    "00"   "A"    ",0"   "6"   

ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(Age35_54_clean = case_when(
    str_to_upper(Age35_54) %in% c("", "0", "00", "O", "A","None") ~ NA_character_,
    TRUE ~ Age35_54
  ),
  Age35_54_clean = as.numeric(Age35_54_clean))

unique(ResidentSurvey.dat$`55-64`)
[1] ""     "0"    "1"    "2"    "Two"  "None" "3"    "O"    "-"    "1`"  

unique(ResidentSurvey.dat$`65_74`)
[1] ""     "0"    "1"    "2"    "Two"  "one"  "3"    "O"    "None" "One"  "two"  "Q"

unique(ResidentSurvey.dat$`75_older`)
[1] ""    "2"   "0"   "1"   "3"   "One" "O" 


#### removing partial responses that didn't answer questions beyond mandatory demographics
partial <- filter(ResidentSurvey.dat,Response.status=="PARTIAL")
write.csv(partial,file = "Partial.Responses.csv")

# write a script that removes partial responses without info beyond required demographic information
# Find the column index of "Age"
age_col_index <- which(names(partial) == "Age")

# Get all columns after Age
after_age_cols <- names(partial)[(age_col_index + 1):ncol(partial)]

# Remove rows where all columns after Age are NA or blank ("")
partial_cleaned <- partial %>%
  filter(if_any(all_of(after_age_cols), ~ !is.na(.) & . != ""))

##### ultimately removed 44 partial responses which did not contain any information beyond basic demographic information (and many lacked that as well)

## rejoin with completed resident suervey data

completed <- filter(ResidentSurvey.dat,Response.status=="COMPLETED")
Resident.clean <- rbind(completed,partial_cleaned)

### load the spreadsheet from Katelyn and replace changes to locations, remove flagged locations, and keep certain ones blank
### make a list of steps and flowchart
### add QAQC of IP address process here

