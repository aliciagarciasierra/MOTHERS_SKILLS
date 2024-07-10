
######################################################
# CROSS-WALK THE OCCUPATIONS WITH THE 70S CENSUS DATA #
######################################################

###########################################################################
#----- Note that to make the occupation variables from different -------- #
# ----- decades equivalent I use the crosswalks ------------------------- #
# ------ 1970 Census occ to occ1990dd and 2000 Census occ to occ1990dd -- #
#------ constructed by David Dorn and available here:-------------------- #
#------  https://www.ddorn.net/data.htm#Occupation%20Codes -------------- #

# ----- Full reference:
# ----- David Autor and David Dorn. 
# ----- "The Growth of Low Skill Service Jobs and the Polarization of the U.S. 
# ------ Labor Market." American Economic Review, 103(5), 1553-1597, 2013.


# Rename dataset
data <- occupations

# Rename basic demographic variable names
data <- transform(data,
                  ID = CASEID_1979,
                  subsample = SAMPLE_ID_1979,
                  race = SAMPLE_RACE_78SCRN,
                  gender = SAMPLE_SEX_1979
)

# Rename occupation variables
for (year in 1979:1993) {
  if (year %in% c(1995, 1997, 1999)) next
  var_name <- paste0("occ70_", year)
  data[[var_name]] <- data[[paste0("CPSOCC70_", year)]]
}
for (year in c(1994, 1996, 1998, 2000)) {
  var_name <- paste0("occ70_", year)
  data[[var_name]] <- data[[paste0("OCCALL-EMP.01_", year)]]
}

# Function to process yearly data
process_yearly_data <- function(year) {
  myvars <- c("ID", paste0("occ70_", year))
  base <- data[myvars]
  census90 <- read_dta("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/THIRD PAPER/Crosswalk Occupations/occ1970_occ1990dd.dta")
  merged <- merge(base, census90, by.x = paste0("occ70_", year), by.y = "occ", all.x = TRUE)
  merged[[paste0("occ", substr(year, 3, 4))]] <- merged$occ1990dd
  myvars <- c("ID", paste0("occ", substr(year, 3, 4)))
  merged[myvars]
}

# Process and save data for each year from 1979 to 2000
years <- c(1979:1993, 1994, 1996, 1998, 2000)
data_list <- lapply(years, process_yearly_data)
names(data_list) <- years
for (year in years) {
  save(data_list[[as.character(year)]], file = paste0("data", substr(year, 3, 4), ".RData"))
}

# Load and merge data up to 2000
upto2000 <- join_all(data_list, by = 'ID', type = 'full')
save(upto2000, file = "upto2000.RData")

######################################################### 
#  CROSS-WALK THE OCCUPATIONS WITH THE 00S CENSUS DATA ##
########################################################

# Rename occupation variables for the 00s census data
for (year in seq(2002, 2018, by = 2)) {
  var_name <- paste0("occ00_", year)
  data[[var_name]] <- data[[paste0("OCCALL-EMP.01_", year)]]
}

# Function to process data for 2002 and onwards
process_yearly_data_00s <- function(year) {
  myvars <- c("ID", paste0("occ00_", year))
  base <- data[myvars]
  census90 <- read_dta("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/THIRD PAPER/Crosswalk Occupations/occ2000_occ1990dd.dta")
  merged <- merge(base, census90, by.x = paste0("occ00_", year), by.y = "occ", all.x = TRUE)
  merged[[paste0("occ", substr(year, 3, 4))]] <- merged$occ1990dd
  myvars <- c("ID", paste0("occ", substr(year, 3, 4)))
  merged[myvars]
}

# Process and save data for each year from 2002 to 2018
years_00s <- seq(2002, 2018, by = 2)
data_list_00s <- lapply(years_00s, process_yearly_data_00s)
names(data_list_00s) <- years_00s
for (year in years_00s) {
  save(data_list_00s[[as.character(year)]], file = paste0("data", substr(year, 3, 4), ".RData"))
}

# Load and merge data from 2002 onwards
after2000 <- join_all(data_list_00s, by = 'ID', type = 'full')
save(after2000, file = "after2000.RData")

###################################################################
# COMBINING BOTH PARTS AND NAMING THE DATA WITH OCCUPATIONS READY #
###################################################################

finalocc <- merge(upto2000, after2000, by = "ID")

