
####################################################################
#######  MERGE MOTHERS AND CHILDREN INFORMATION ####################
####################################################################

# Rename main ID
children$childID<-children$ID

# Select only variables of interest
myvars <- c("childID", "YEAR", "motherID", "sex",
            "motheredu","residence", "datebirth", "birthorder", "birthage",
            "homez", "cognz", "emotz",
            "mathz", "recoz", "vocaz", "motoz", "digitz")

children<-children[myvars]

# Merge
merge<-merge(mothers, children, by.x=c("ID", "year"), by.y=c("motherID", "YEAR"),all.x=TRUE)


#Select only women with children
data<- merge[ which(merge$childID!="NA"), ]