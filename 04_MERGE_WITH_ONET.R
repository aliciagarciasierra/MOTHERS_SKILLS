
####################################################################
#############  MERGE WITH O*NET #####################################
####################################################################

#############################################################################
#----- Note that two crosswalks are constructed based on the taxonomies #####
#----- and crosswalks specified at https://www.onetcenter.org/taxonomy.html #
#############################################################################

#------ FIRST WE NEED TO USE TWO CROSS-WALKS TO NAVIGATE THROUGH THE ONET SOC CODES

# Reshape data to long format
long <- reshape(finalocc, idvar = "ID", varying = 5:32, timevar = "year", sep = "", direction = "long")

# Rename year variables
year_map <- c(79 = 1979, 80 = 1980, 81 = 1981, 82 = 1982, 83 = 1983, 84 = 1984, 85 = 1985, 86 = 1986, 87 = 1987, 88 = 1988, 89 = 1989, 90 = 1990, 91 = 1991, 92 = 1992, 93 = 1993, 94 = 1994, 96 = 1996, 98 = 1998, 0 = 2000, 2 = 2002, 4 = 2004, 6 = 2006, 8 = 2008, 10 = 2010, 12 = 2012, 14 = 2014, 16 = 2016, 18 = 2018)
long$year <- year_map[as.character(long$year)]

# Create id and year indicator
long$idyear <- as.numeric(paste0(long$ID, long$year))

# Save the longitudinal version
save(long, file = "long.RData")

# Open long data
rm(list = ls(all = TRUE))
data <- load("long.RData")
data <- long

# Merge with first O*NET crosswalk
onet <- read_excel("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/THIRD PAPER/Data/data/first.xls")
onet <- transform(onet, occ = occ1990dd, onet2010 = onetsoc2010code)
onet <- onet[c("occ", "onet2010")]

# Merge data with O*NET crosswalk
merge <- merge(long, onet, by = "occ", all.x = TRUE)

# Solve duplication
dup <- which(duplicated(merge$idyear))[1:abs(nrow(long) - nrow(merge))]
data <- merge[-dup,]
ordered <- data[order(data$ID, data$year),]

# Extract those of interest
col_order <- c("ID", "year", "idyear", "race", "gender", "subsample", "occ", "onet2010")
onet10 <- ordered[, col_order]
save(onet10, file = "onet10.RData")

# Open data for the second crosswalk
rm(list = ls(all = TRUE))
data <- load("onet10.RData")
data <- onet10
onet <- read_excel("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/THIRD PAPER/Data/data/second.xlsx")

# Create variable for merging
onet <- transform(onet, onet2010 = `O*NET-SOC 2010 Code`, onet2019 = `O*NET-SOC 2019 Code`)
onet <- onet[c("onet2010", "onet2019")]

# Merge data with second O*NET crosswalk
merge <- merge(data, onet, by = "onet2010", all.x = TRUE)

# Solve duplication
dup <- which(duplicated(merge$idyear))[1:abs(nrow(data) - nrow(merge))]
ordered <- merge[-dup,]
ordered <- ordered[order(ordered$ID, ordered$year),]

# Order the columns
col_order <- c("ID", "year", "idyear", "race", "gender", "subsample", "occ", "onet2010", "onet2019")
onet19 <- ordered[, col_order]

# Save the final data
save(onet19, file = "onet19.RData")

#------ SECOND, WE MERGE THIS WITH THE SKILLS DATA DOWNLOADED FROM O*NET

# Crosswalk to skills
rm(list=ls(all=TRUE))

# Load the data
load("onet19.RData")
data <- onet19

# Open O*NET skills file
library(readxl)
skills <- read_excel("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/THIRD PAPER/Data/data/Skills.xlsx")

# Rename variables (needed to reshape so that there's all the information about each occupation in a row)
skills$occ <- skills$`O*NET-SOC Code`  # making the id variable simpler
skills$skill <- skills$`Element Name`
skills$scale <- skills$`Scale Name`
skills$value <- skills$`Data Value`

# Create skills+scale variable (to have both options of scales until deciding which one makes more sense substantively)
skills$skillid <- paste(skills$skill, skills$`Scale ID`, sep="_")

# Selecting only relevant variables
myvars <- c("occ", "skillid", "value")
skills <- skills[myvars]

# Reshaping
skillswide <- reshape(as.data.frame(skills), idvar="occ", timevar="skillid", direction="wide")

# Now it's ready to merge
skillswide$onet2019 <- skillswide$occ
merged_data <- merge(onet19, skillswide, by="onet2019", all.x=TRUE)

# Rearrange order
merged_data <- merged_data[order(merged_data$ID, merged_data$year),]

# Relabeling and rearranging
relabel_vars <- function(df, old_suffix, new_suffix) {
  for (var in names(df)) {
    if (grepl(old_suffix, var)) {
      new_var <- sub(old_suffix, new_suffix, var)
      df[[new_var]] <- df[[var]]
    }
  }
}

relabel_vars(merged_data, "value.Active Learning_", "Active_Learning_")
relabel_vars(merged_data, "value.Active Listening_", "Active_Listening_")
relabel_vars(merged_data, "value.Complex Problem Solving_", "Problem_Solving_")
relabel_vars(merged_data, "value.Coordination_", "Coordination_")
relabel_vars(merged_data, "value.Critical Thinking_", "Critical_Thinking_")
relabel_vars(merged_data, "value.Equipment Maintenance_", "Equipment_Maintenance_")
relabel_vars(merged_data, "value.Equipment Selection_", "Equipment_Selection_")
relabel_vars(merged_data, "value.Installation_", "Installation_")
relabel_vars(merged_data, "value.Instructing_", "Instructing_")
relabel_vars(merged_data, "value.Judgment and Decision Making_", "Decision_Making_")
relabel_vars(merged_data, "value.Management of Financial Resources_", "Management_Financial_")
relabel_vars(merged_data, "value.Management of Material Resources_", "Management_Material_")
relabel_vars(merged_data, "value.Management of Personnel Resources_", "Management_Personnel_")
relabel_vars(merged_data, "value.Mathematics_", "Mathematics_")
relabel_vars(merged_data, "value.Monitoring_", "Monitoring_")
relabel_vars(merged_data, "value.Negotiation_", "Negotiation_")
relabel_vars(merged_data, "value.Operation and Control_", "Control_")
relabel_vars(merged_data, "value.Operations Analysis_", "Analysis_")
relabel_vars(merged_data, "value.Persuasion_", "Persuasion_")
relabel_vars(merged_data, "value.Programming_", "Programming_")
relabel_vars(merged_data, "value.Quality Control Analysis_", "Quality_Control_")
relabel_vars(merged_data, "value.Reading Comprehension_", "Reading_Comprehension_")
relabel_vars(merged_data, "value.Repairing_", "Repairing_")
relabel_vars(merged_data, "value.Science_", "Science_")
relabel_vars(merged_data, "value.Service Orientation_", "Service_Orientation_")
relabel_vars(merged_data, "value.Social Perceptiveness_", "Social_Perceptiveness_")
relabel_vars(merged_data, "value.Speaking_", "Speaking_")
relabel_vars(merged_data, "value.Systems Analysis_", "Systems_Analysis_")
relabel_vars(merged_data, "value.Systems Evaluation_", "Systems_Evaluation_")
relabel_vars(merged_data, "value.Technology Design_", "Technology_Design_")
relabel_vars(merged_data, "value.Time Management_", "Time_Management_")
relabel_vars(merged_data, "value.Troubleshooting_", "Troubleshooting_")
relabel_vars(merged_data, "value.Writing_", "Writing_")

merged_data$occ <- merged_data$occ.x

# Select the variables
myvars <- c("ID", "year", "idyear", 
            "race", "gender", "subsample",
            "occ", "onet2010", "onet2019",
            "Active_Learning_IM", "Active_Learning_LV",
            "Active_Listening_IM", "Active_Listening_LV",
            "Analysis_IM", "Analysis_LV",
            "Control_IM", "Control_LV",
            "Coordination_IM", "Coordination_LV",
            "Critical_Thinking_IM", "Critical_Thinking_LV",
            "Decision_Making_IM", "Decision_Making_LV",
            "Equipment_Maintenance_IM", "Equipment_Maintenance_LV",
            "Equipment_Selection_IM", "Equipment_Selection_LV",
            "Installation_IM", "Installation_LV",
            "Instructing_IM", "Instructing_LV",
            "Management_Financial_IM", "Management_Financial_LV",
            "Management_Material_IM", "Management_Material_LV",
            "Management_Personnel_IM", "Management_Personnel_LV",
            "Mathematics_IM", "Mathematics_LV",
            "Monitoring_IM", "Monitoring_LV",
            "Negotiation_IM", "Negotiation_LV",
            "Persuasion_IM", "Persuasion_LV",
            "Problem_Solving_IM", "Problem_Solving_LV",
            "Programming_IM", "Programming_LV",
            "Quality_Control_IM", "Quality_Control_LV",
            "Reading_Comprehension_IM", "Reading_Comprehension_LV",
            "Repairing_IM", "Repairing_LV",
            "Science_IM", "Science_LV",
            "Service_Orientation_IM", "Service_Orientation_LV",
            "Social_Perceptiveness_IM", "Social_Perceptiveness_LV",
            "Speaking_IM", "Speaking_LV",
            "Systems_Analysis_IM", "Systems_Analysis_LV",
            "Systems_Evaluation_IM", "Systems_Evaluation_LV",
            "Technology_Design_IM", "Technology_Design_LV",
            "Time_Management_IM", "Time_Management_LV",
            "Troubleshooting_IM", "Troubleshooting_LV",
            "Writing_IM", "Writing_LV")

data <- merged_data[myvars]

# Arrange columns
col_order <- c("ID", "year", "idyear", 
               "race", "gender", "subsample",
               "occ", "onet2010", "onet2019",
               "Active_Learning_IM", "Active_Learning_LV",
               "Active_Listening_IM", "Active_Listening_LV",
               "Analysis_IM", "Analysis_LV",
               "Control_IM", "Control_LV",
               "Coordination_IM", "Coordination_LV",
               "Critical_Thinking_IM", "Critical_Thinking_LV",
               "Decision_Making_IM", "Decision_Making_LV",
               "Equipment_Maintenance_IM", "Equipment_Maintenance_LV",
               "Equipment_Selection_IM", "Equipment_Selection_LV",
               "Installation_IM", "Installation_LV",
               "Instructing_IM", "Instructing_LV",
               "Management_Financial_IM", "Management_Financial_LV",
               "Management_Material_IM", "Management_Material_LV",
               "Management_Personnel_IM", "Management_Personnel_LV",
               "Mathematics_IM", "Mathematics_LV",
               "Monitoring_IM", "Monitoring_LV",
               "Negotiation_IM", "Negotiation_LV",
               "Persuasion_IM", "Persuasion_LV",
               "Problem_Solving_IM", "Problem_Solving_LV",
               "Programming_IM", "Programming_LV",
               "Quality_Control_IM", "Quality_Control_LV",
               "Reading_Comprehension_IM", "Reading_Comprehension_LV",
               "Repairing_IM", "Repairing_LV",
               "Science_IM", "Science_LV",
               "Service_Orientation_IM", "Service_Orientation_LV",
               "Social_Perceptiveness_IM", "Social_Perceptiveness_LV",
               "Speaking_IM", "Speaking_LV",
               "Systems_Analysis_IM", "Systems_Analysis_LV",
               "Systems_Evaluation_IM", "Systems_Evaluation_LV",
               "Technology_Design_IM", "Technology_Design_LV",
               "Time_Management_IM", "Time_Management_LV",
               "Troubleshooting_IM", "Troubleshooting_LV",
               "Writing_IM", "Writing_LV")

data <- data[, col_order]

# Rename complete date
mothers <-data
  
