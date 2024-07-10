
####################################################################
###################  ADD CONTROL VARIABLES  ####################
####################################################################

# In this script I add three group of control variables that 
# are used in the robustness checks and extensions of the paper.



#---------- The first set of control variables can be acessed by
#--------- uploading the tagset controls.NLSY79 to the NLS investigator
#--------- and running the generated code. Once this has been done,
# -------- the code below cleans this dataset and merges it with the main data.


# GENERAL DEMOGRAPHICS

new_data$ID <- new_data$CASEID_1979
new_data$subsample <- new_data$SAMPLE_ID_1979
new_data$race <- new_data$SAMPLE_RACE_78SCRN
new_data$gender <- new_data$SAMPLE_SEX_1979

# FAMILY SIZE

famsize_vars <- c("ID", "FAMSIZE_1979", "FAMSIZE_1980", "FAMSIZE_1981", "FAMSIZE_1982", 
                  "FAMSIZE_1983", "FAMSIZE_1984", "FAMSIZE_1985", "FAMSIZE_1986", 
                  "FAMSIZE_1987", "FAMSIZE_1988", "FAMSIZE_1989", "FAMSIZE_1990", 
                  "FAMSIZE_1991", "FAMSIZE_1992", "FAMSIZE_1993", "FAMSIZE_1994", 
                  "FAMSIZE_1996", "FAMSIZE_1998", "FAMSIZE_2000", "FAMSIZE_2002", 
                  "FAMSIZE_2004", "FAMSIZE_2006", "FAMSIZE_2008", "FAMSIZE_2010", 
                  "FAMSIZE_2012", "FAMSIZE_2014", "FAMSIZE_2016", "FAMSIZE_2018")

famsize <- new_data[famsize_vars]
famsize_long <- reshape(famsize, idvar="ID", varying=2:29, timevar="year", sep="_", direction="long")

# TOTAL AMOUNT RECEIVED UNEMPLOYED

unemp_vars <- c("ID", "UNEMPR-TOTAL-1986_REVISED_XRND", "UNEMPR-TOTAL-1988_REVISED_XRND", 
                "UNEMPR-TOTAL-1990_REVISED_XRND", "UNEMPR-TOTAL-1992_REVISED_XRND", 
                "UNEMPR-TOTAL-1994_REVISED_XRND", "UNEMPR-TOTAL-1996_REVISED_XRND", 
                "UNEMPR-TOTAL-1998_REVISED_XRND", "UNEMPR-TOTAL-2000_REVISED_XRND", 
                "UNEMPR-TOTAL-2002_REVISED_XRND", "UNEMPR-TOTAL-2004_XRND", 
                "UNEMPR-TOTAL-2006_XRND", "UNEMPR-TOTAL-2008_XRND", 
                "UNEMPR-TOTAL-2010_XRND", "UNEMPR-TOTAL-2012_XRND", 
                "UNEMPR-TOTAL-2014_XRND")

unemp <- new_data[unemp_vars]
unemp <- setNames(unemp, c("ID", paste0("unemp_", seq(1986, 2014, by=2))))
unemp_long <- reshape(unemp, idvar="ID", varying=2:16, timevar="year", sep="_", direction="long")

# HOURS PER WEEK WORKED

hourswork_vars <- c("CASEID_1979", "QES-52A.01_1986", "QES-52A.01_1988", "QES-52A.01_1990", 
                    "QES-52A.01_1992", "QES-52A.01_1994", "QES-52A.01_1996", "QES-52A.01_1998", 
                    "QES-52A.01_2000", "QES-52A.01_2002", "QES-52A.01_2004", "QES-52A.01_2006", 
                    "QES-52A.01_2008", "QES-52C.01_2010", "QES-52C.01_2012", "QES-52D.01_2014")

hourswork <- new_data[hourswork_vars]
hourswork$ID <- hourswork$CASEID_1979
hourswork <- setNames(hourswork, c("ID", paste0("hourswork_", seq(1986, 2014, by=2))))
hourswork_long <- reshape(hourswork, idvar="ID", varying=2:16, timevar="year", sep="_", direction="long")

# AGE OF YOUNGEST MEMBER IN HOUSEHOLD

ageyoungest_vars <- c("ID", "AGEYCH86_1986", "AGEYCH88_1988", "AGEYCH90_1990", 
                      "AGEYCH92_1992", "AGEYCH94_1994", "AGEYCH96_1996", "AGEYCH98_1998", 
                      "AGEYCH00_2000", "AGEYCH02_2002", "AGEYCH04_2004", "AGEYCH06_2006", 
                      "AGEYCH08_2008", "AGEYCH10_2010", "AGEYCH12_2012", "AGEYCH14_2014")

ageyoungest <- new_data[ageyoungest_vars]
ageyoungest <- setNames(ageyoungest, c("ID", paste0("ageyoungest_", seq(1986, 2014, by=2))))
ageyoungest_long <- reshape(ageyoungest, idvar="ID", varying=2:16, timevar="year", sep="_", direction="long")

# HOURS WORKED BY PARTNER

hoursworkpartner_vars <- c("ID", "Q2-15B_1986", "Q2-15B_1988", "Q2-15B_1990", 
                           "Q2-15B_1992", "Q2-15B_1994", "Q2-15B_1996", "Q2-15B_1998", 
                           "Q2-15B_2000", "Q2-15B_2002", "Q2-15B_2004", "Q2-15B_2006", 
                           "Q2-15B_2008", "Q2-15B_2010", "Q2-15B_2012", "Q2-15B_2014")

hoursworkpartner <- new_data[hoursworkpartner_vars]
hoursworkpartner <- setNames(hoursworkpartner, c("ID", paste0("hoursworkpartner_", seq(1986, 2014, by=2))))
hoursworkpartner_long <- reshape(hoursworkpartner, idvar="ID", varying=2:16, timevar="year", sep="_", direction="long")

# MARITAL STATUS

marstatus_vars <- c("ID", "MARSTAT-KEY_1986", "MARSTAT-KEY_1988", "MARSTAT-KEY_1990", 
                    "MARSTAT-KEY_1992", "MARSTAT-KEY_1994", "MARSTAT-KEY_1996", 
                    "MARSTAT-KEY_1998", "MARSTAT-KEY_2000", "MARSTAT-KEY_2002", 
                    "MARSTAT-KEY_2004", "MARSTAT-KEY_2006", "MARSTAT-KEY_2008", 
                    "MARSTAT-KEY_2010", "MARSTAT-KEY_2012", "MARSTAT-KEY_2014")

marstatus <- new_data[marstatus_vars]
marstatus <- setNames(marstatus, c("ID", paste0("marstatus_", seq(1986, 2014, by=2))))
marstatus_long <- reshape(marstatus, idvar="ID", varying=2:16, timevar="year", sep="_", direction="long")

# HEALTH LIMITATIONS

healthlimit_vars <- c("ID", "Q11-4_1986", "Q11-4_1988", "Q11-4_1990", "Q11-4_1992", 
                      "Q11-4_1994", "Q11-4_1996", "Q11-4_1998", "Q11-4_2000", 
                      "Q11-4_2002", "Q11-4_2004", "Q11-4_2006", "Q11-4_2008", 
                      "Q11-4_2010", "Q11-4_2012", "Q11-4_2014")

healthlimit <- new_data[healthlimit_vars]
healthlimit <- setNames(healthlimit, c("ID", paste0("healthlimit_", seq(1986, 2014, by=2))))
healthlimit_long <- reshape(healthlimit, idvar="ID", varying=2:16, timevar="year", sep="_", direction="long")

# MERGE ALL THESE DATASETS WITH THE MAIN DATA 
data <- datawithcontrolsB
data <- merge(data, famsize_long, by=c("ID", "year"), all.x=TRUE)
data <- merge(data, unemp_long, by=c("ID", "year"), all.x=TRUE)
data <- merge(data, hourswork_long, by=c("ID", "year"), all.x=TRUE)
data <- merge(data, ageyoungest_long, by=c("ID", "year"), all.x=TRUE)
data <- merge(data, hoursworkpartner_long, by=c("ID", "year"), all.x=TRUE)
data <- merge(data, marstatus_long, by=c("ID", "year"), all.x=TRUE)
data <- merge(data, healthlimit_long, by=c("ID", "year"), all.x=TRUE)

#---------- The second set of control variables can be acsessed by
#--------- uploading the tagset additional_mothers.NLSY79 to the NLS investigator
#--------- and running the generated code. Once this has been done,
# -------- the code below cleans this dataset and merges it with the main data.

# Arrange the new_data general demographics
new_data <- new_data %>%
  select(ID = CASEID_1979, subsample = SAMPLE_ID_1979, race = SAMPLE_RACE_78SCRN, gender = SAMPLE_SEX_1979)

# Function to reshape and merge data
reshape_and_merge <- function(data, new_data, vars, id_var="ID", time_var="year", value_name) {
  long <- reshape(new_data[vars], idvar=id_var, varying=2:length(vars), timevar=time_var, sep="_", direction="long")
  names(long)[names(long) == vars[2]] <- value_name
  merge(data, long, by.x=c("ID", "year"), by.y=c("ID", "year"), all.x=TRUE)
}

# Working hours
working_hours_vars <- c("ID", paste0("QES-52A.01_", 1982:2012))
data <- reshape_and_merge(data, new_data, working_hours_vars, value_name="workinghours")

# Income
income_vars <- c("ID", paste0("TNFI_TRUNC_", c(1979, 1983:1996, 1998:2016)[-c(5,7,9,11,13)]))
data <- reshape_and_merge(data, new_data, income_vars, value_name="income")

# Residence changes
migration_vars <- c("ID", paste0("MIGR-3_", 2000:2012))
data <- reshape_and_merge(data, new_data, migration_vars, value_name="residencechange")

# Anxiety
anxiety_vars <- c("ID", "H60-GAD-7_SCORE_XRND")
anxiety <- new_data[anxiety_vars]
names(anxiety)[2] <- "stress"
data <- merge(data, anxiety, by="ID", all.x=TRUE)

# Reason to leave the job
reasontoleave_vars <- c("ID", paste0("Q11-5_", 1982:2010))
data <- reshape_and_merge(data, new_data, reasontoleave_vars, value_name="reasontoleave")

# Sector industry
industry_vars <- c("ID", paste0("CPSIND80_", c(1982:1998, 2000)))
data <- reshape_and_merge(data, new_data, industry_vars, value_name="industry")

# Job quality
jobquality_vars <- c("ID", paste0("QES-23A.01_", 1982:2010))
data <- reshape_and_merge(data, new_data, jobquality_vars, value_name="jobquality")

# Supervisory responsibilities
supervisory_vars <- c("ID", paste0("QES-RESPONS37.01_", c(1996, 1998, 2000)))
data <- reshape_and_merge(data, new_data, supervisory_vars, value_name="supervisory")

# Firm size
size_vars <- c("ID", paste0("QES-FIRMSZ2.01_", c(1994, 1996, 1998, 2000, 2002, 2004, 2006, 2008, 2010)))
data <- reshape_and_merge(data, new_data, size_vars, value_name="firmsize")

# Flexible work
flexible_vars <- c("ID", paste0("QES-84J.01_", c(1994, 1996, 1998, 2000, 2002)))
data <- reshape_and_merge(data, new_data, flexible_vars, value_name="flexiblework")

# Region
region_vars <- c("ID", paste0("REGION_", c(1979:1996, 1998:2018)))
data <- reshape_and_merge(data, new_data, region_vars, value_name="region")


#---------- The thirdset of control variables can be asscessed by
#--------- uploading the tagset additional_children.CHILDYA to the NLS investigator
#--------- and running the generated code. Once this has been done,
# -------- the code below cleans this dataset and merges it with the main data.

# School Changes
new_data<- new_data %>%
  mutate(school_2000=`Q4-25A_2000`,
         school_2002=`Q4-25A_2002`,
         school_2004=`Q4-25A_2004`,
         school_2006=`Q4-25A_2006`,
         school_2008=`Q4-25A_2008`,
         school_2010=`Q4-25A_2010`,
         school_2012=`Q4-25A_2012`,
         school_2014=`Q4-25A_2014`
  )

myvars <- c("ID", 
            "school_2000", "school_2002","school_2004",
            "school_2006","school_2008","school_2010",
            "school_2012","school_2014")

school<-new_data[myvars]

school_long <- reshape(school, idvar = "ID", varying = 2:9, timevar = "year", sep = "_", direction = "long")

# Mothers' Extracurricular Training
new_data<- new_data %>%
  mutate(training_1986=ATTSCH1986_1986,
         training_1988=ATTSCH1988_1988,
         training_1990=ATTSCH1990_1990,
         training_1992=ATTSCH1992_1992,
         training_1994=ATTSCH1994_1994,
         training_1996=ATTSCH1996_1996,
         training_1998=ATTSCH1998_1998,
         training_2000=ATTSCH2000_2000,
         training_2002=ATTSCH2002_2002,
         training_2004=ATTSCH2004_2004,
         training_2006=ATTSCH2006_2006,
         training_2008=ATTSCH2008_2008,
         training_2010=ATTSCH2010_2010,
         training_2012=ATTSCH2012_2012,
         training_2014=ATTSCH2014_2014)

myvars <- c("ID",  "training_1986", 
            "training_1988", "training_1990",
            "training_1992", 
            "training_1994","training_1996",
            "training_1998",
            "training_2000", "training_2002","training_2004",
            "training_2006","training_2008","training_2010",
            "training_2012","training_2014")
training<-new_data[myvars]

training_long <- reshape(training, idvar = "ID", varying = 2:16, timevar = "year", sep = "_", direction = "long")

# Learning Disabilities

new_data<- new_data %>%
  mutate(disab_1986=`CS860660_1986`,
         disab_1988=`CS881149_1988`,
         disab_1990=`CS901645_1990`,
         disab_1992=`CS921772_1992`,
         disab_1994=`CS94H-5_A_1994`,
         disab_2000=`Q14-8A~000022_2000`,
         disab_2002=`Q14-8A~000022_2002`,
         disab_2004=`Q14-8A~000022_2004`,
         disab_2006=`Q14-8A~000022_2006`,
         disab_2008=`Q14-8A~000022_2008`,
         disab_2010=`Q14-8A~000022_2010`,
         disab_2012=`Q14-8A~000022_2012`,
         disab_2014=`Q14-8A~000022_2014`)

myvars <- c("ID",  "disab_1986", 
            "disab_1988", "disab_1990",
            "disab_1992", 
            "disab_1994",
            "disab_2000", "disab_2002","disab_2004",
            "disab_2006","disab_2008","disab_2010",
            "disab_2012","disab_2014")
disab<-new_data[myvars]

disab_long <- reshape(disab, idvar = "ID", varying = 2:14, timevar = "year", sep = "_", direction = "long")

# Behavioral Problems

new_data$behav_1986<-new_data$BPIP1986_1986
new_data$behav_1988<-new_data$BPIP1988_1988
new_data$behav_1990<-new_data$BPIP1990_1990
new_data$behav_1992<-new_data$BPIP1992_1992
new_data$behav_1994<-new_data$BPIP1994_1994
new_data$behav_1996<-new_data$BPIP1996_1996
new_data$behav_1998<-new_data$BPIP1998_1998
new_data$behav_2000<-new_data$BPIP2000_2000
new_data$behav_2002<-new_data$BPIP2002_2002
new_data$behav_2004<-new_data$BPIP2004_2004
new_data$behav_2006<-new_data$BPIP2006_2006
new_data$behav_2008<-new_data$BPIP2008_2008
new_data$behav_2010<-new_data$BPIP2010_2010
new_data$behav_2012<-new_data$BPIP2012_2012
new_data$behav_2014<-new_data$BPIP2014_2014
new_data$behav_2016<-new_data$BPIP2016_2016
new_data$behav_2018<-new_data$BPIP2018_2018

myvars <- c("ID", 
            "behav_1986", "behav_1988", "behav_1990",
            "behav_1992", "behav_1994", "behav_1996",
            "behav_1998", "behav_2000", "behav_2002",
            "behav_2004", "behav_2006", "behav_2008",
            "behav_2010", "behav_2012", "behav_2014")
behav <- new_data[myvars]

behav_long <- reshape(behav, idvar = "ID", varying = 2:16, timevar = "year", sep = "_", direction = "long")

# Mothers' Education (last recorded observation)
new_data<- new_data %>%
  mutate(edulevels=`MOMHGC_EVER_XRND`)

myvars <- c("ID",  "edulevels")
edu<-new_data[myvars]

# Childcare

childcare<-new_data
# To avoid the double _ that confuses reshape
childcare <- childcare %>% 
  mutate(ID=CPUBID_XRND,
         childcare_1988 = `CS880971_1988`,
         childcare_1990 = `CS901613_1990`,
         childcare_1992 = `CS921740_1992`,
         childcare_1994 = `CS94-11A_1994`,
         childcare_1996 = `CS96-11A_1996`,
         childcare_1998 = `CS98-11A_1998`,
         childcare_2000 = `BKGN-6_2000`,
         childcare_2002 = `BKGN-GRDCHK-PRESCHL_2002`,
         childcare_2004 = `BKGN-GRDCHK-PRESCHL_2004`,
         childcare_2006 = `MS-BKGN-GRDCHK-PRESCHL_2006`,
         childcare_2008 = `MS-BKGN-GRDCHK-PRESCHL_2008`,
         childcare_2010 = `MS-BKGN-GRDCHK-PRESCHL_2010`,
         childcare_2012 = `MS-BKGN-GRDCHK-PRESCHL_2012`,
         childcare_2014= `MS-BKGN-GRDCHK-PRESCHL_2014`,
         childcare_2016= `MS-BKGN-GRDCHK-PRESCHL_2016`,
  ) 


# Clean
cols <- c("childcare_1998", "childcare_2000", "childcare_2002", 
          "childcare_2004", "childcare_2006", "childcare_2008", 
          "childcare_2010", "childcare_2012", "childcare_2014", 
          "childcare_2016")

for (col in cols) {
  print(table(childcare[[col]]))
  childcare[[col]] <- ifelse(childcare[[col]] == 90, 1, 0)
  print(table(childcare[[col]]))
}

myvars <- c("ID",  
            "childcare_1988", "childcare_1990",
            "childcare_1992", 
            "childcare_1994","childcare_1996", "childcare_1998", 
            "childcare_2000", "childcare_2002","childcare_2004",
            "childcare_2006","childcare_2008","childcare_2010",
            "childcare_2012","childcare_2014", "childcare_2016")
childcare<-childcare[myvars]

childcare_long <- reshape(childcare, idvar = "ID", varying = 2:16, timevar = "year", sep = "_", direction = "long")

# Age at interview

new_data<- new_data %>%
  mutate(ageatin_1986=`CSAGE1986_1986`,
         ageatin_1988=`CSAGE1988_1988`,
         ageatin_1990=`CSAGE1990_1990`,
         ageatin_1992=`CSAGE1992_1992`,
         ageatin_1994=`CSAGE1994_1994`,
         ageatin_1996=`CSAGE1996_1996`,
         ageatin_1998=`CSAGE1998_1998`,
         ageatin_2000=`CSAGE2000_2000`,
         ageatin_2002=`CSAGE2002_2002`,
         ageatin_2004=`CSAGE2004_2004`,
         ageatin_2006=`CSAGE2006_2006`,
         ageatin_2008=`CSAGE2008_2008`,
         ageatin_2010=`CSAGE2010_2010`,
         ageatin_2012=`CSAGE2012_2012`,
         ageatin_2014=`CSAGE2014_2014`)

myvars <- c("ID",  "ageatin_1986", 
            "ageatin_1988", "ageatin_1990",
            "ageatin_1992", 
            "ageatin_1994","ageatin_1996","ageatin_1998",
            "ageatin_2000", "ageatin_2002","ageatin_2004",
            "ageatin_2006","ageatin_2008","ageatin_2010",
            "ageatin_2012","ageatin_2014")
ageatint<-new_data[myvars]

#Reshape#
age_long <- reshape(age, idvar = "ID", varying = 2:16, timevar = "year", sep = "_", direction = "long")


#------ Merge all subsets with data

data <- merge(data, school_long, by.x = c("childID", "year"), by.y = c("ID", "year"), all.x = TRUE)
data <- merge(data, training_long, by.x = c("childID", "year"), by.y = c("ID", "year"), all.x = TRUE)
data <- merge(data, disab_long, by.x = c("childID", "year"), by.y = c("ID", "year"), all.x = TRUE)
data <- merge(data, behav_long, by.x = c("childID", "year"), by.y = c("ID", "year"), all.x = TRUE)
data <- merge(data, edu, by.x = "childID", by.y = "ID", all.x = TRUE)
data <- merge(data, childcare_long, by.x = c("childID", "year"), by.y = c("ID", "year"), all.x = TRUE)
data <- merge(data, age_long, by.x = c("childID", "year"), by.y = c("ID", "year"), all.x = TRUE)

#------ View the final merged data: ready to clean!
head(data)

