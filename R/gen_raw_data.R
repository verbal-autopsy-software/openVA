library(EAVA)
data(data_public)
names(data_public)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# NEONATE
odk_neonate <- data_public[, 13:ncol(data_public)]
dim(odk_neonate)

for (i in 1:10) {
  odk_neonate <- rbind(odk_neonate, odk_neonate)
}
odk_neonate <- as.data.frame(odk_neonate)
dim(odk_neonate)

odk_neonate$ageInDaysNew <- sample(0:27, nrow(odk_neonate), replace = TRUE)
odk_neonate$ageInDays <- odk_neonate$ageInDaysNew
odk_neonate$ageInYears <- odk_neonate$ageInDaysNew / 365

odk_neonate$Id10104 <- "yes"
odk_neonate$Id10109 <- "yes"
odk_neonate$Id10110 <- "yes"

eava_cod_neonate <- c("NNT", "Malformation", "Intrapartum", "Preterm",
                      "Meningitis", "Diarrhea", "Pneumonia", "Sepsis",
                      "Other", "Unspecified")

#------------------------------------------------------------------------------#
# NNT
#------------------------------------------------------------------------------#
idx <- 1:500
odk_neonate[idx, "Id10219"] <- "yes"
odk_neonate[idx, "Id10271"] <- "yes"
odk_neonate[idx, "Id10273"] <- "yes"
id <- 501:1234
odk_neonate[idx, "Id10106"] <- "yes"
odk_neonate[idx, "Id10107"] <- "yes"

#------------------------------------------------------------------------------#
# Malformation
#------------------------------------------------------------------------------#
idx <- 1235:1500
odk_neonate[idx, "Id10373"] <- "yes"
odk_neonate[idx, "Id10371"] <- "yes"

#------------------------------------------------------------------------------#
# Intrapartum
#------------------------------------------------------------------------------#
idx <- 2001:2500
odk_neonate[idx, "Id10115"] <- "yes"
idx <- 2501:3456
odk_neonate[idx, "Id10106"] <- "yes"
odk_neonate[idx, "Id10286"] <- "yes"

#------------------------------------------------------------------------------#
# Preterm
#------------------------------------------------------------------------------#
idx <- 3457:5000
odk_neonate[idx, "Id10367"] <- 7 # this is a problem for odk2EAVA()

#------------------------------------------------------------------------------#
# Meningitis
#------------------------------------------------------------------------------#
idx <- 5001:5500
odk_neonate[idx, "Id10147"] <- "yes"
odk_neonate[idx, "Id10278"] <- "yes"
odk_neonate[idx, "Id10286"] <- "yes"
odk_neonate[idx, "Id10219"] <- "yes"
## idx <- 5501:6039  ==> using this for Pneumonia
## odk_neonate[idx, "Id10147"] <- "yes"
## odk_neonate[idx, "Id10275"] <- "yes"
## odk_neonate[idx, "Id10281"] <- "yes"
## odk_neonate[idx, "Id10219"] <- "yes"

#------------------------------------------------------------------------------#
# Diarrhea
#------------------------------------------------------------------------------#
idx <- 6049:7632
odk_neonate[idx, "Id10181"] <- "yes"
odk_neonate[idx, "Id10183"] <- 5

#------------------------------------------------------------------------------#
# Pneumonia
#------------------------------------------------------------------------------#
idx <- 5501:6039
odk_neonate[idx, "Id10166"] <- "yes"
odk_neonate[idx, "Id10167"] <- 1

#------------------------------------------------------------------------------#
# Sepsis
#------------------------------------------------------------------------------#
idx <- 7633:8000
odk_neonate[idx, "Id10147"] <- "yes"
odk_neonate[idx, "Id10183"] <- 6
odk_neonate[idx, "Id10271"] <- "yes"
odk_neonate[idx, "Id10172"] <- "yes"
idx <- 8001:8259
odk_neonate[idx, "Id10147"] <- "yes"
odk_neonate[idx, "Id10183"] <- 6
odk_neonate[idx, "Id10219"] <- "yes"
odk_neonate[idx, "Id10265"] <- "yes"

#------------------------------------------------------------------------------#
# Other
#------------------------------------------------------------------------------#
idx <- 8260:8500
odk_neonate[idx, "Id10289"] <- "yes"
odk_neonate[idx, "Id10273"] <- "yes"
odk_neonate[idx, "Id10147"] <- "no"
odk_neonate[idx, "Id10284"] <- "no"
idx <- 8501:9712
odk_neonate[idx, "Id10241"] <- "yes"
odk_neonate[idx, "Id10147"] <- "no"
odk_neonate[idx, "Id10284"] <- "no"

#------------------------------------------------------------------------------#
# Unspecified  -- seems like there are already a good number
#------------------------------------------------------------------------------#
## idx <- 
## odk_neonate[idx, "Id10"] <- "yes"
## odk_neonate[idx, "Id10"] <- "yes"
## odk_neonate[idx, "Id10"] <- "yes"
## odk_neonate[idx, "Id10"] <- "yes"
## odk_neonate[idx, "Id10"] <- "yes"


#------------------------------------------------------------------------------#
# Output results
#------------------------------------------------------------------------------#
## new_order <- sample(1:nrow(odk_neonate), nrow(odk_neonate))
## odk_neonate_neonate <- odk_neonate[new_order,]
## odk_neonate_neonate$id <- paste0("ID_", 1:nrow(odk_neonate_neonate))
## write.csv(odk_neonate_neonate, file = "eava_n.csv", row.names = FALSE)

## RandomEAVA_N <- odk2EAVA(odk_neonate_neonate, "id")
## save(RandomEAVA_N, file = "RandomEAVA_N.rda")
## results <- codEAVA(RandomEAVA_N, "neonate")
## table(results$cause)
## table(results$cause)/sum(table(results$cause))

## library(InSilicoVA)
## r6 <- read.csv("random6.csv")
## dim(r6)
## ins <- insilico(r6)
## load('results_insilico.RData')
## summary(ins, top = 10)
# now check this with pyCrossVA, InSilico, InterAV


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# CHILD
eava_cod_child <- c("Malformation", "Intrapartum", "Preterm",
                    "Injury", "AIDS", "Measles", "Malnutrition", "Other infections",
                    "Meningitis/Encephalitis", "Diarrhea/Dysentery", "Pneumonia", 
                    "Unspecified")

odk_child <- data_public[, 13:ncol(data_public)]
dim(odk_child)

for (i in 1:12) {
  odk_child <- rbind(odk_child, odk_child)
}
odk_child <- as.data.frame(odk_child)
odk_child[, "Id10250"] <- 0
dim(odk_child)

odk_child$ageInDaysNew <- sample(28:1800, nrow(odk_child), replace = TRUE)
odk_child$ageInDays <- odk_child$ageInDaysNew
odk_child$ageInYears <- odk_child$ageInDaysNew / 365

#------------------------------------------------------------------------------#
# Malformation
#------------------------------------------------------------------------------#
idx <- 1:1588
odk_child[idx, "Id10373"] <- "y"
idx <- 1589:3214
odk_child[idx, "Id10370"] <- "y"

#------------------------------------------------------------------------------#
# Intrapartum
#------------------------------------------------------------------------------#
idx <- 3215:5578
odk_child[idx, "Id10106"] <- sample(5:9, length(idx), replace = TRUE)
odk_child[idx, "Id10271"] <- "no"
idx <- 5579:6217
odk_child[idx, "Id10111"] <- "no"
odk_child[idx, "Id10219"] <- "yes"

#------------------------------------------------------------------------------#
# Preterm
#------------------------------------------------------------------------------#
idx <- 6218:8744
odk_child[idx, "Id10367"] <- 8
odk_child[idx, "Id10166"] <- "yes"
odk_child[idx, "Id10167"] <- odk_child[idx, "ageInDaysNew"]
odk_child[idx, "Id10147"] <- "no"
idx <- 8744:12066
odk_child[idx, "Id10367"] <- 8

#------------------------------------------------------------------------------#
# Injury
#------------------------------------------------------------------------------#
idx <- 12067:14471
for (i in 22:ncol(odk_child)) {
    if (!is.numeric(odk_child[,i])) {
        odk_child[idx, i] <- "."
    }
}
odk_child[, "Id10358"] <- NA
odk_child[, "Id10359"] <- NA
odk_child[idx, "Id10077"] <- "yes"
odk_child[idx, "Id10120"] <- 1


#------------------------------------------------------------------------------#
# AIDS
#------------------------------------------------------------------------------#
idx <- 14472:16821

#------------------------------------------------------------------------------#
# Measles
#------------------------------------------------------------------------------#
idx <- 16822:21107
odk_child[idx, "Id10147"] <- "yes"
odk_child[idx, "Id10148"] <- 3
odk_child[idx, "Id10233"] <- "yes"
odk_child[idx, "Id10234"] <- 4
odk_child[idx, "Id10235"] <- "everywhere"

#------------------------------------------------------------------------------#
# Malnutrition
#------------------------------------------------------------------------------#
idx <- 21108:24084
odk_child[idx, "Id10249"] <- "yes"
odk_child[idx, "Id10250"] <- 8  # I think this is a bug in odk2EAVA
odk_child[idx, "Id10120"] <- 3  # I think this is a bug in odk2EAVA
odk_child[idx, "Id10181"] <- "yes"
odk_child[idx, "Id10183"] <- 7
odk_child[idx, "Id10186"] <- "no"

#------------------------------------------------------------------------------#
# Other infections
#------------------------------------------------------------------------------#
idx <- 24085:26261
odk_child[idx, "Id10153"] <- "yes"
odk_child[idx, "Id10156"] <- "yes"

#------------------------------------------------------------------------------#
# Meningitis/Encephalitis
#------------------------------------------------------------------------------#
idx <- 26262:29112
odk_child[idx, "Id10147"] <- "yes"
odk_child[idx, "Id10208"] <- "yes"

#------------------------------------------------------------------------------#
# Diarrhea/Dysentery
#------------------------------------------------------------------------------#
idx <- 29113:36816
odk_child[idx, "Id10181"] <- "yes"
odk_child[idx, "Id10183"] <- 7
odk_child[idx, "Id10186"] <- "no"

#------------------------------------------------------------------------------#
# Pneumonia
#------------------------------------------------------------------------------#
idx <- 36817: 38703
odk_child[idx, "Id10153"] <- "yes"
odk_child[idx, "Id10159"] <- "yes"
odk_child[idx, "Id10161"] <- 3
odk_child[idx, "Id10166"] <- "yes"
odk_child[idx, "Id10167"] <- 3

## odk_child$id <- 1:nrow(odk_child)
## out <- odk2EAVA(odk_child, "id")
## results <- codEAVA(out, "child")
## table(results$cause)/sum(table(results$cause))
## write.csv(odk_child, file = "eava_c.csv", row.names = FALSE)
## library(InterVA5)
## library(InSilicoVA)
## odk_c <- read.csv("random6_c.csv")

## input <- odk_c[sample(1:nrow(odk_c), 300),]
## int_c <- InterVA5(input, HIV='l', Malaria='l', directory=getwd())
## ins_c <- insilico(input)

## summary(int_c, top = 10)
## summary(ins_c, top = 10)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# MERGE AND SAVE RESULTS FOR OPENVA PACKAGE
idx_neonate <- sample(1:nrow(odk_neonate), 792)
idx_child <- sample(1:nrow(odk_child), 944)

odk <- rbind(odk_neonate[idx_neonate,],
             odk_child[idx_child,])
RandomVA6 <- odk[sample(1:nrow(odk), nrow(odk)), ]
RandomVA6$id <- paste0("id_", 1:nrow(RandomVA6))

DataEAVA <- odk2EAVA(RandomVA6, "id")
save(DataEAVA, file = "DataEAVA.rda")
write.csv(RandomVA6, file = "RandomVA6.csv", row.names = FALSE)
