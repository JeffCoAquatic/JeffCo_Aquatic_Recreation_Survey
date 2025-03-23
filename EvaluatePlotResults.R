library(dplyr)
library(ggplot2)
library(RColorBrewer)

# load most recent version of survey data
# downloaded as individual responses, .csv format
# set working directory to source file (convert to Rproject at some point to avoid this)
#####################################################
# load data and format as needed
#####################################################

survey.dat <- read.csv('JAC_Survey_3.2.25.csv')

colnames.og <- colnames(survey.dat) 
colnames.new <- c("Response.ID","IP.address","Response.status","Survey.URL","Start.time","Completion.time","Time.taken",
                          "Collector","JeffCo.resident","City.area","Age","Under7","7_12","13_18","19_34","35_54","55-64","65_74","75_older","RateNewPoolImportance",
                          "Under7_Non.swimmer","Under7_Beginner","Under7_Intermediate","Under7_Advanced",
                          "7_12_Non.swimmer","7_12_Beginner","7_12_Intermediate","7_12_Advanced",
                          "13_18_Non.swimmer","13_18_Beginner","13_18_Intermediate","13_18_Advanced",
                          "19_34_Non.swimmer","19_34_Beginner","19_34_Intermediate","19_34_Advanced",
                          "35_54_Non.swimmer","35_54_Beginner","35_54_Intermediate","35_54_Advanced",
                          "55-64_Non.swimmer","55-64_Beginner","55-64_Intermediate","55-64_Advanced",
                          "65_74_Non.swimmer","65_74_Beginner","65_74_Intermediate","65_74_Advanced",
                          "75_older_Non.swimmer","75_older_Beginner","75_older_Intermediate","75_older_Advanced","UsedAquaticsPast2Years",
                          "WhichAquaticFacilities","FacilityUsageRate","AquaAmenities_LapSwimming","AquaAmenities_OpenSwim","AquaAmenities_YouthLessons",
                          "AquaAmenities_AdultLessons","AquaAmenities_CompetitiveSwim","AquaAmenities_LifeguardTrain","AquaAmenities_Aerobics",
                          "AquaAmenities_SyncroSwim","AquaAmenities_SportsLeagues","AquaAmenities_SafetyTrain","AquaAmenities_Therapeutic",
                          "AquaAmenities_PartyRental","AquaAmenities_ParentChild","AquaAmenities_Sauna","AquaAmenities_HotTub","AquaAmenities_PublicShower",
                          "AquaAmenities_OutdoorPool","AquaAmenities_SplashPark","AquaAmenities_UsageRate","RecAmenities_AfterschoolCamps",
                          "RecAmenities_Preschool","RecAmenities_Childcare","RecAmenities_Playgroups","RecAmenities_FitnessClass","RecAmenities_PhysTherapy",
                          "RecAmenities_EventSpace","RecAmenities_WellnessClass","RecAmenities_Kitchen","RecAmenities_Cafe","RecAmenities_Gym",
                          "RecAmenities_OutPickleball","RecAmenities_InPickleball","RecAmenities_RockClimb","RecAmenities_TeenFit","RecAmenities_TeenCenter",
                          "RecAmenities_UsageRate","TaxSupport","Comments")

ResidentSurvey.dat <- cbind(survey.dat[,1:92],survey.dat[,122])
colnames(ResidentSurvey.dat) <- colnames.new

####################################################################################################################
### Set Column Names and Remove Non-resident/tourist surveys
####################################################################################################################

ResidentSurvey.dat <- survey.dat %>%
                        {cbind(.[, 1:92], .[, 122])} %>%
                        filter(Do.you.live.in.Jefferson.County.!="No, I am visiting")%>%
                        setNames(colnames.new)

# drop columns we aren't using
ResidentSurvey.dat <- ResidentSurvey.dat[,10:92]                        
####################################################################################################################
### Evaluate Duplicate IP addresses
### covert unique IP addresses to UUID's

duplicate_ips <- ResidentSurvey.dat %>%
  group_by(IP.address) %>%
  filter(n() > 1) %>%
  ungroup()

# Create a table of unique duplicate IP addresses and their frequency
duplicate_ip_table <- ResidentSurvey.dat %>%
  group_by(IP.address) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  arrange(desc(count))

# View the duplicate IP addresses with their count
print(duplicate_ip_table,n=200)

#Investigate individual duplicates
HighestUsageIP <- filter(ResidentSurvey.dat,IP.address=="64.184.145.20") 
  # this one was used 83 times
  # diversity of responses (TaxQ = 4 IDK, 8 NMI, 21 No, and 50 Yes)
  # likely a public wifi location
  # IP lookup suggests public institution in Port Townsend (NOANET)
  # PT library maybe?
HighUsageIP <- filter(ResidentSurvey.dat,IP.address=="24.113.125.226") 
  # 14 times
  # diversity of responses (TaxQ = 1 NV, 2 IDK, 2 No, 9 Yes)
  # likely a public wifi location in PT
  # IP also suggests it is a public org/institution/nonprofit (NOANET)
UsageIP_3rd <- filter(ResidentSurvey.dat,IP.address=="209.166.87.82")
  # 11 times
  # this one indicates whidbey island telephone co./Langley?
  # less diversity in responses (1 IDK, 10 Yes)
  # this one probably needs more investigation
UsageIP_4th <- filter(ResidentSurvey.dat,IP.address=="104.28.116.136")
  # 10 times
  # from icloud private relay, maybe Seattle
  # some diversity of opinion (2 IDK, 1 No, 7 Yes)
  # comments very specific and diverse
UsageIP_5th <- filter(ResidentSurvey.dat,IP.address=="209.166.69.49")
  # 10 times
  # whidbey telephone co. again
  # diversity of responses (Tax Support = 1 IDK, 3 No, 5 Yes, and 1 blank)
####################################################################################################################


###############################################################################################################
## 3.) create figure and table illustrating geo distribution of responses

# filter out 'other' category temporarily; need to go reclassify as possible
# Remove City.area names that start with "Other (Please specify)"
ResidentSurvey.dat <- ResidentSurvey.dat[!grepl("^Other \\(Please specify\\)", ResidentSurvey.dat$City.area), ]

# drop the blank entries
ResidentSurvey.dat <- filter(ResidentSurvey.dat, City.area != "")

# Count the occurrences of each unique 'City.area'
city_area_counts <- table(ResidentSurvey.dat$City.area)

# Convert the table to a data frame for ggplot
city_area_df <- as.data.frame(city_area_counts)
colnames(city_area_df) <- c("City.area", "Count")

# Create a bar plot with a colorblind-friendly palette
plot <- ggplot(city_area_df, aes(x = City.area, y = Count, fill = City.area)) +
  geom_bar(stat = "identity") +
  scale_color_viridis(discrete = TRUE) +  # Use a colorblind-friendly palette
  labs(title = "Number of Records for Each City Area",
       x = "City/Area",
       y = "Number of Responses") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

print(plot)

# save figure with fixed dimensions to standardize w/others
ggsave("city_area_plot.png", plot = plot, width = 8, height = 6, units = "in", dpi = 300)

###
# create table of the same information
###

# Convert the table to a data frame for better handling
data <- as.data.frame(city_area_counts)
colnames(data) <- c("City.area", "Num.responses")

# create a table using gt package
# Create a styled table with the required bold and grey lines
table_gt <- data %>%
  gt() %>%
  cols_label(
    City.area = "City/Area",
    Num.responses = "Number of Responses"
  ) %>%
  # Left-justify the City/Area column data
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_body(columns = "City.area")
  ) %>%
  # Left-justify the "City/Area" column name
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_column_labels(columns = "City.area")
  ) %>%
  # Make column headers bold
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c("City.area", "Num.responses"))
  ) %>%
  # Bold the line above the column headers (top of the table)
  tab_style(
    style = cell_borders(sides = "top", weight = 2),
    locations = cells_column_labels(columns = c("City.area", "Num.responses"))
  ) %>%
  # Bold the line below the column headers (under the header row)
  tab_style(
    style = cell_borders(sides = "bottom", weight = 2),
    locations = cells_column_labels(columns = c("City.area", "Num.responses"))
  ) %>%
  # Bold the bottom line of the table (under the last row of data)
  tab_style(
    style = cell_borders(sides = "bottom", weight = 2),
    locations = cells_body(rows = nrow(city_area_df), columns = c("City.area", "Num.responses"))
  ) 

# Save the flextable as a PNG image
png("City_Area_Table.png", width = 8, height = 4, units = "in", res = 300)
# Render the table using gridExtra's grid.table
grid.table(ft)
dev.off()


                         