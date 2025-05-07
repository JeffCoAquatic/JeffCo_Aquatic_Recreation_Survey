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
                  "Collector","JeffCo.resident","City.area","Age","Under7","Age7_12","Age13_18","Age19_34","Age35_54","Age55_64","Age65_74","Age75_older","RateNewPoolImportance",
                  "Under7_Non.swimmer","Under7_Beginner","Under7_Intermediate","Under7_Advanced",
                  "Age7_12_Non.swimmer","Age7_12_Beginner","Age7_12_Intermediate","Age7_12_Advanced",
                  "Age13_18_Non.swimmer","Age13_18_Beginner","Age13_18_Intermediate","Age13_18_Advanced",
                  "Age19_34_Non.swimmer","Age19_34_Beginner","Age19_34_Intermediate","Age19_34_Advanced",
                  "Age35_54_Non.swimmer","Age35_54_Beginner","Age35_54_Intermediate","Age35_54_Advanced",
                  "Age55_64_Non.swimmer","Age55_64_Beginner","Age55_64_Intermediate","Age55_64_Advanced",
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

#####################################################################################################
## manual validation step - export data frame of old and new column names to verify correct assignment
# Resident survey
#
#resident_names_old <- c(colnames.og[1:93],colnames.og[123])
#manually inspect each row to verify the renaming assignment
#resident_cols_check <- data.frame(resident_names_old,colnames.new)
#write.csv(resident_cols_check,file = "resident_cols_check.csv")
#
# column name reassignment verified and correct; step complete

#####################################################################################################
## manual validation step - export data frame of old and new column names to verify correct assignment
# Non-resident survey
#
#Validate column name changes for non-resident survey (step completed and file saved to project)
#nonresident_names_old <- colnames.og[94:123]
#manually inspect each row to verify the renaming assignment
#nonresident_cols_check <- data.frame(nonresident_names_old,colnames.nonres)
#write.csv(nonresident_cols_check,file = "nonresident_cols_check.csv")
#
# column name reassignment verified and correct; step complete

#####################################################################################################
## Set Column Names and Remove Non-resident/tourist surveys
# columns 1-93 = resident survey questions
# column 123 = comments
# 
ResidentSurvey.dat <- survey.dat %>%
  {cbind(.[, 1:93], .[, 123])} %>%
  filter(Do.you.live.in.Jefferson.County.!="No, I am visiting")%>%
  setNames(colnames.new)

##########################################################################################
#### manually inspecting partial responses
#partial <- filter(ResidentSurvey.dat,Response.status=="PARTIAL")
#write.csv(partial,file = "Partial.Responses.csv")

# Remove partial survey entries where there was NO information beyond demographic categories
remove_ids <- c("wrB669Dd", "LjCYPvXx", "W3CKRi57", "M0CYtV3I", "lkfOSlMH",
                "VCCK50rS", "elfRxUYm", "nEk07L6j", "bKC4wpxG", "bdDTfvrM",
                "TgB6u4vs", "u0kqJRIf", "LDCYkW96", "u6fE6EZh", "hyCYb8Ul",
                "EWkaRMGf", "19B6uWA5", "T2CYwrxn", "4MC4cft9", "0CCYA8lN",
                "nDCKFOw9", "AJC4gO1m", "BQfsUQwv", "kMC4MSVb", "StfFfwil",
                "XNf2isK9", "PbCYqJVi", "6ek1abrz", "mIkh6czp", "G2CiTog4",
                "YDCKapcH", "nlCKM67V", "AoCiyqa4", "83kTxut7", "ZzCiOsIX",
                "aXCiKFc4", "1MDTyVyF", "iEDToYxY", "dEEBXT0w", "nDEBDe9W",
                "63fPOlnK", "Sifbmlr7", "77fpV8Oj", "qQfqZlTi", "Faf5Sia9",
                "GKff6LRu", "plCYtQ0h", "oHB6SXjl", "vLC4IKkz", "1mCYFICp",
                "aXfqDG3V", "WufIa78z", "nwC4FrV4", "MaC4YPlM", "sYklCl67")

ResidentSurvey.dat <- ResidentSurvey.dat %>%
  filter(!Response.ID %in% remove_ids)


######################################################################################################
## create nonresident survey data frame for future analysis 
# columns 94-123 = nonresident survey questions
# column 123 = comments
#
NonResidentSurvey.dat <- survey.dat %>%
  filter(Do.you.live.in.Jefferson.County.=="No, I am visiting")%>%
  select(94:123) %>%
  setNames(colnames.nonres)

# write csv file that represents the FULL, unaltered dataset (aside from column renaming)
# write.csv(NonResidentSurvey.dat,file = "Nonresident_Survey_Responses_3.31.2025.csv")

######################################################################################################
# save comments and survey ID as separate data frame - can link back to data with survey ID
# 
ResidentComment.dat <- ResidentSurvey.dat %>%
  select(Response.ID,Comments)
# save csv file that represents the FULL, unaltered comment data for future analysis/linking to response data
# write.csv(ResidentComment.dat,"Comments_Final_3.31.25.csv")

# drop the comment column in working data frame due to length; can link data to ResidentComment.dat using Response.ID
ResidentSurvey.dat <- ResidentSurvey.dat %>%
  select(!Comments)

##########################################################################################################
## Review each age category for entry errors and non-numeric entries 
# note: this question was an open-entry format by necessity (due to question format) and therefore allowed non-numeric data

# Under 7 age cat - inspect unique entries for formatting errors (i.e. non-numeric; illogical)
unique(ResidentSurvey.dat$Under7)
table(ResidentSurvey.dat$Under7)

ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(Under7 = case_when(
    str_to_upper(Under7) %in% c("", "0", "00", "O", "NONE") ~ NA_character_,
    str_detect(tolower(Under7), "due|baby") ~ NA_character_,
    str_to_upper(Under7) == "I" ~ "1",
    Under7 == "46" ~ NA_character_, 
    str_to_upper(Under7) == "2P" ~ "2",
    TRUE ~ Under7
  ),
  Under7 = as.numeric(Under7))

# re-inspect unique entries; NA's ok and indicate not answered
unique(ResidentSurvey.dat$Under7)
table(ResidentSurvey.dat$Under7)

# Age 7-12 age cat - inspect unique entries for formatting errors (i.e. non-numeric; illogical)
unique(ResidentSurvey.dat$Age7_12)
table(ResidentSurvey.dat$Age7_12)

ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(
    Age7_12 = str_trim(Age7_12),  # Remove leading/trailing whitespace
    Age7_12 = case_when(
      str_to_upper(Age7_12) %in% c("", "0", "00", "O", "NONE") ~ NA_character_,
      str_detect(tolower(Age7_12), "baby") ~ NA_character_,
      !str_detect(Age7_12, "^\\d+$") ~ NA_character_,  # remove anything non-numeric
      TRUE ~ Age7_12
    ),
    Age7_12 = as.numeric(Age7_12)
  )

# re-inspect 
unique(ResidentSurvey.dat$Age7_12)
table(ResidentSurvey.dat$Age7_12)

# Age 13-18 age cat - inspect unique entries for formatting errors (i.e. non-numeric; illogical)
unique(ResidentSurvey.dat$Age13_18)
table(ResidentSurvey.dat$Age13_18)

ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(
    Age13_18 = str_trim(Age13_18),
    Age13_18 = case_when(
    str_to_upper(Age13_18) %in% c("","0", "00", "O", "None") ~ NA_character_,
    !str_detect(Age13_18, "^\\d+$") ~ NA_character_,  # remove anything non-numeric
    TRUE ~ Age13_18
  ),
Age13_18 = as.numeric(Age13_18))

# re-inspect
unique(ResidentSurvey.dat$Age13_18)
table(ResidentSurvey.dat$Age13_18)


# Age 19-34 age cat - inspect unique entries for formatting errors (i.e. non-numeric; illogical)
unique(ResidentSurvey.dat$Age19_34)
table(ResidentSurvey.dat$Age19_34)

ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(
    Age19_34 = str_trim(Age19_34),
    Age19_34 = str_replace_all(Age19_34, regex("^One$", ignore_case = TRUE), "1"),  # convert "One" to "1"
    Age19_34 = case_when(
    str_to_upper(Age19_34) %in% c("", "0", "00", "O","None") ~ NA_character_,
    !str_detect(Age19_34, "^\\d+$") ~ NA_character_,  # remove anything non-numeric
    TRUE ~ Age19_34
  ),
  Age19_34 = as.numeric(Age19_34))

#re-inspect
unique(ResidentSurvey.dat$Age19_34)
table(ResidentSurvey.dat$Age19_34)

# Age 35-54 age cat - inspect unique entries for formatting errors (i.e. non-numeric; illogical)
unique(ResidentSurvey.dat$Age35_54)
table(ResidentSurvey.dat$Age35_54)

ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(
    Age35_54 = str_trim(Age35_54),
    Age35_54 = str_replace_all(Age35_54, regex("^I$", ignore_case = TRUE), "1"),  # convert "I" to "1"
    Age35_54 = case_when(
    str_to_upper(Age35_54) %in% c("", "0", "00", "O", "A","None") ~ NA_character_,
    !str_detect(Age35_54, "^\\d+$") ~ NA_character_,  # remove anything non-numeric
    TRUE ~ Age35_54
  ),
  Age35_54 = as.numeric(Age35_54))

# re-inspect
unique(ResidentSurvey.dat$Age35_54)
table(ResidentSurvey.dat$Age35_54)

# Age 55-64 age cat - inspect unique entries for formatting errors (i.e. non-numeric; illogical)
unique(ResidentSurvey.dat$Age55_64)
table(ResidentSurvey.dat$Age55_64)

ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(
    Age55_64 = str_trim(Age55_64),
    Age55_64 = str_replace_all(Age55_64, regex("^Two$", ignore_case = TRUE), "2"),  # convert "Two" to "2"
    Age55_64 = str_replace_all(Age55_64, "[^0-9]", ""),  # strip non-numeric characters like apostrophes
    Age55_64 = case_when(
      str_to_upper(Age55_64) %in% c("", "0", "00", "O", "A","None") ~ NA_character_,
      !str_detect(Age55_64, "^\\d+$") ~ NA_character_,  # remove anything non-numeric
      TRUE ~ Age55_64
    ),
    Age55_64 = as.numeric(Age55_64))

# re-inspect
unique(ResidentSurvey.dat$Age55_64)
table(ResidentSurvey.dat$Age55_64)

# Age 65-74 age cat - inspect unique entries for formatting errors (i.e. non-numeric; illogical)
unique(ResidentSurvey.dat$Age65_74)
table(ResidentSurvey.dat$Age65_74)

ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(
    Age65_74 = str_trim(Age65_74),
    Age65_74 = str_replace_all(Age65_74, regex("^Two$", ignore_case = TRUE), "2"),
    Age65_74 = str_replace_all(Age65_74, regex("^One$", ignore_case = TRUE), "1"),
    Age65_74 = str_replace_all(Age65_74, "[^0-9]", ""),  # remove non-numeric characters
    Age65_74 = case_when(
      str_to_upper(Age65_74) %in% c("", "0", "00", "O", "Q", "NONE") ~ NA_character_,
      !str_detect(Age65_74, "^\\d+$") ~ NA_character_,
      TRUE ~ Age65_74
    ),
    Age65_74 = as.numeric(Age65_74)
  )

# re-inspect
unique(ResidentSurvey.dat$Age65_74)
table(ResidentSurvey.dat$Age65_74)

# Age75_older age cat - inspect unique entries for formatting errors (i.e. non-numeric; illogical)

unique(ResidentSurvey.dat$Age75_older)
table(ResidentSurvey.dat$Age75_older)

ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(
    Age75_older = str_trim(Age75_older),
    Age75_older = str_replace_all(Age75_older, regex("^Two$", ignore_case = TRUE), "2"),
    Age75_older = str_replace_all(Age75_older, regex("^One$", ignore_case = TRUE), "1"),
    Age75_older = str_replace_all(Age75_older, "[^0-9]", ""),  # remove non-numeric characters
    Age75_older = case_when(
      str_to_upper(Age75_older) %in% c("", "0", "00", "O", "NONE") ~ NA_character_,
      !str_detect(Age75_older, "^\\d+$") ~ NA_character_,
      TRUE ~ Age75_older
    ),
    Age75_older = as.numeric(Age75_older)
  )

#re-inspect
unique(ResidentSurvey.dat$Age75_older)
table(ResidentSurvey.dat$Age75_older)



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

# Read in your CSV with the mapping
recode_area <- read.csv("OtherArea_reponses.csv")

# Left join to add the revised city name to your main dataset
ResidentSurvey.dat <- ResidentSurvey.dat %>%
  left_join(recode_area %>% select(Response.ID, city.area.revised), by = "Response.ID")

# Recode City.area using city.area.revised where available
ResidentSurvey.dat <- ResidentSurvey.dat %>%
  mutate(City.area = ifelse(!is.na(city.area.revised), city.area.revised, City.area)) %>%
  select(-city.area.revised)  # Remove the helper column if no longer needed

write.csv(ResidentSurvey.dat,file="JAC_Survey_3.31.25_CLEAN.csv")


