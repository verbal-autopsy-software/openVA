# Produce larger EAVA data set for neonates and children
library(EAVA)
data(data_public)

#------------------------------------------------------------------------------#
# Neonate
#------------------------------------------------------------------------------#
out <- odk2EAVA(data_public, "comsa_id")

dim(out)
for (i in 1:10) {
  out <- rbind(out, out)
}
dim(out)
out$ID <- paste0("ID", 1:nrow(out))
names(out)[6:17]
out[,6:17] <- "n"
names(out)[12]
out[,12] <- "y"
out[, "age"] <- sample(1:27, nrow(out), replace = TRUE)
out[, "i104o"] <- "y"
out[, "i109o"] <- "y"
out[, "i110o"] <- "y"

eava_cod_neonate <- c("NNT", "Malformation", "Intrapartum", "Preterm",
                      "Meningitis", "Diarrhea", "Pneumonia", "Sepsis",
                      "Other", "Unspecified")

## data$nnt1 <- ifelse((data$age > 2 & data$age < 27) & 
##                     data$i219o == "y" & ((data$i271o == "y" & data$i273o == 
##                                           "y") | (data$i106a == "y" & data$i107o == "y")), 
##                     1, 2)
index_ntt <- 1:500
out[index_ntt, "i219o"] <- "y"
out[index_ntt, "i271o"] <- "y"
out[index_ntt, "i273o"] <- "y"
index_ntt <- 501:1234
out[index_ntt, "i106a"] <- "y"
out[index_ntt, "i107o"] <- "y"

## data$congmalf2 <- ifelse(data$i373o == "y" | data$i372o == 
##                          "y" | data$i371o == "y" | data$i370o == "y", 1, 2)
index_malf <- 1235:1500
out[index_malf, "i373o"] <- "y"
index_malf <- 1501:2000
out[index_malf, "i371o"] <- "y"

## data$bi5 <- ifelse(data$i115o %in% "y", 1, 2)
## data$ba5 <- ifelse((data$i106a == "y" | data$i111o == 
##                     "n") & ((data$i271o == "n" | data$i219o == "y" | 
##                              data$i275o == "y" | data$i276o == "y" | data$i286o == 
##                              "y") | data$age == 0), 1, 2)
index_intra <- 2001:2500
out[index_intra, "i115o"] <- "y"
index_intra <- 2501:3456
out[index_intra, "i106a"] <- "y"
out[index_intra, "i286o"] <- "y"

## data$preterm_all_mo <- ifelse((data$i367b %in% "y" & 
##                                data$i166o %in% "y" & data$fb_day0 == "y" & data$i147o %in% 
##                                "n" & data$i284o %in% "n") | data$i367c %in% "y", 
##                               1, 2)
index_pret <- 3457:3500
out[index_pret, "i367c"] <- "y"
index_pret <- 3501:5000
out[index_pret, "i367c"] <- "y"


## data$meningitis451 <- ifelse(data$i147o %in% "y" & (data$i278o %in% 
##                                                     "y" | data$i219o %in% "y" | data$i275o %in% "y" | 
##                                                     data$i276o %in% "y") & (data$i286o %in% "y" | data$i281o %in% 
##                                                                             "y"), 1, 2)
## data$meningitis451_nonnt1 <- ifelse(data$meningitis451 %in% 
##                                     1 & data$nnt1 %in% 2, 1, 2)
index_men <- 5001:5500
out[index_men, "i147o"] <- "y"
out[index_men, "i278o"] <- "y"
out[index_men, "i286o"] <- "y"
out[index_men, "i219o"] <- "n"
index_men <- 5501:6039
out[index_men, "i147o"] <- "y"
out[index_men, "i275o"] <- "y"
out[index_men, "i281o"] <- "y"
out[index_men, "i219o"] <- "n"

## data$diarrhea8 <- ifelse(data$i181o == "y" & data$i183b == 
##                          "y", 1, 2)
index_diar <- 6049:6500
out[index_diar, "i181o"] <- "y"
out[index_diar, "i183b"] <- "y"
index_diar <- 6501:7632
out[index_diar, "i181o"] <- "y"
out[index_diar, "i183b"] <- "y"

## data$sepsisfvr <- ifelse(data$i147o %in% "y" | data$i284o %in% 
##                          "y", 1, 2)
## data$sepsisfvr2sign1 <- ifelse(data$i147o == "y" | data$i284o == 
##                                "y", 1, 0)
## data$sepsisfvr2sign3 <- ifelse(data$i271o == "n" | data$i273o == 
##                                "y", 1, 0)
## data$sepsisfvr2sign5 <- ifelse(data$i219o == "y", 1, 
##                                0)
## data$sepsisfvr2sign7 <- ifelse(data$i289o == "y" | data$i265o == 
##                                "y", 1, 0)
## data$sepsisfvr2sign9 <- ifelse(data$i107o == "y", 1, 
##                                0)
## data$sepsisfvr2sign10 <- ifelse(data$i286o == "y" | data$i281o == 
##                                 "y", 1, 0)
## data$sepsisfvr2sign12 <- ifelse(data$i172o == "y" | data$i173b == 
##                                 "y", 1, 0)
## data$sepsisfvr2signs = data$sepsisfvr2sign1 + data$sepsisfvr2sign3 + 
##     data$sepsisfvr2sign5 + data$sepsisfvr2sign9 + data$sepsisfvr2sign10 + 
##     data$sepsisfvr2sign12
## data$sepsisfvr2_2 <- ifelse(data$sepsisfvr == 1 | data$sepsisfvr2signs > 
##                             1, 1, 2)
## data$sepsisfvr2_2_nonnt1 <- ifelse(data$sepsisfvr2_2 == 
##                                    1 & data$nnt1 == 2, 1, 2)

index_seps <- 7633:8000
out[index_seps, "i147o"] <- "y"
## out[index_seps, "i284o"] <- "y"
out[index_seps, "i183b"] <- "y"
out[index_seps, "i271o"] <- "y"
## out[index_seps, "i273o"] <- "y"
out[index_seps, "i172o"] <- "y"
index_seps <- 8001:8259
out[index_seps, "i147o"] <- "y"
## out[index_seps, "i284o"] <- "y"
out[index_seps, "i183b"] <- "y"
out[index_seps, "i219o"] <- "y"
out[index_seps, "i265o"] <- "y"

## data$jaundice2 <- ifelse((data$i289o == "y" | data$i265o == 
##                           "y") & (data$i273o == "y" | data$i286o == "y" | data$i215o == 
##                                   "y" | data$i282o == "y" | data$i283o == "y") & (data$i147o == 
##                                                                                   "n" & data$i284o == "n"), 1, 2)
## data$hemorrhageNN <- ifelse(data$i241o == "y" & data$i147o == 
##                             "n" & data$i284o == "n", 1, 2)

index_jau <- 8260:8500
out[index_jau, "i289o"] <- "y"
out[index_jau, "i273o"] <- "y"
out[index_jau, "i147o"] <- "n"
out[index_jau, "i284o"] <- "n"
index_hem <- 8501:9712
out[index_hem, "i241o"] <- "y"
out[index_hem, "i147o"] <- "n"
out[index_hem, "i284o"] <- "n"


results <- codEAVA(out, "neonate")
table(results$cause)
new_order <- sample(1:nrow(out), nrow(out))
RandomEAVA_N <- out[new_order,]
RandomEAVA_N$ID <- paste0("ID_", 1:nrow(out))
save(RandomEAVA_N, file = "/Users/thomas.3912/GitHub/openVA/data/RandomEAVA_N.rda")

#------------------------------------------------------------------------------#
# codEAVA
#------------------------------------------------------------------------------#
##     if (age_group == "neonate") {
##         data <- df
##         data$Stillbirth <- ifelse(data$i104o %in% c("n", ".") & 
##             data$i109o %in% c("n", ".") & data$i110o %in% c("n", 
##             "."), 1, 0)
##         data <- data[data$Stillbirth != 1, ]
##         data <- subset(data, age < 28)
##         dim(data)
##         data$nnt1 <- ifelse((data$age > 2 & data$age < 27) & 
##             data$i219o == "y" & ((data$i271o == "y" & data$i273o == 
##             "y") | (data$i106a == "y" & data$i107o == "y")), 
##             1, 2)
##         table(data$nnt1, exclude = NULL)

##         table(data$congmalf2, exclude = NULL)
##         data$bi5 <- ifelse(data$i115o %in% "y", 1, 2)
##         table(data$bi5, exclude = NULL)
##         data$ba5 <- ifelse((data$i106a == "y" | data$i111o == 
##             "n") & ((data$i271o == "n" | data$i219o == "y" | 
##             data$i275o == "y" | data$i276o == "y" | data$i286o == 
##             "y") | data$age == 0), 1, 2)
##         table(data$ba5, exclude = NULL)
##         data$preterm_rds_mo <- ifelse((data$i367b == "y" | data$i367c == 
##             "y") & (data$i166o == "y" & data$fb_day0 == "y" & 
##             data$i147o == "n" & data$i284o == "n"), 1, 2)
##         table(data$preterm_rds_mo, exclude = NULL)
##         data$preterm_all_mo <- ifelse((data$i367b %in% "y" & 
##             data$i166o %in% "y" & data$fb_day0 == "y" & data$i147o %in% 
##             "n" & data$i284o %in% "n") | data$i367c %in% "y", 
##             1, 2)
##         table(data$preterm_all_mo, exclude = NULL)
##         data$meningitis451 <- ifelse(data$i147o %in% "y" & (data$i278o %in% 
##             "y" | data$i219o %in% "y" | data$i275o %in% "y" | 
##             data$i276o %in% "y") & (data$i286o %in% "y" | data$i281o %in% 
##             "y"), 1, 2)
##         table(data$meningitis451, exclude = NULL)
##         data$meningitis451_nonnt1 <- ifelse(data$meningitis451 %in% 
##             1 & data$nnt1 %in% 2, 1, 2)
##         table(data$meningitis451_nonnt1, exclude = NULL)
##         data$diarrhea8 <- ifelse(data$i181o == "y" & data$i183b == 
##             "y", 1, 2)
##         table(data$diarrhea8, exclude = NULL)
##         data$pneumo157sign1 <- ifelse(data$i166o == "y", 1, 0)
##         data$pneumo157sign2 <- ifelse(data$i172o == "y", 1, 0)
##         data$pneumo157sign3 <- ifelse(data$i173b == "y", 1, 0)
##         data$pneumo157sign4 <- ifelse(data$i271o == "n" | data$i273o == 
##             "y", 1, 0)
##         data$pneumo157sign5 <- ifelse(data$i104o == "n" | data$i107o == 
##             "y", 1, 0)
##         data$pneumo157signs <- data$pneumo157sign2 + data$pneumo157sign3 + 
##             data$pneumo157sign5
##         data$pneumonia157 <- ifelse((data$i166o == "y" & data$i167c == 
##             "y") | data$i159o %in% "y" & data$i161b == "y" & 
##             data$pneumo157signs > 1, 1, 2)
##         table(data$pneumonia157, exclude = NULL)
##         data$sepsisfvr <- ifelse(data$i147o %in% "y" | data$i284o %in% 
##             "y", 1, 2)
##         table(data$sepsisfvr, exclude = NULL)
##         data$sepsisfvr2sign1 <- ifelse(data$i147o == "y" | data$i284o == 
##             "y", 1, 0)
##         data$sepsisfvr2sign3 <- ifelse(data$i271o == "n" | data$i273o == 
##             "y", 1, 0)
##         data$sepsisfvr2sign5 <- ifelse(data$i219o == "y", 1, 
##             0)
##         data$sepsisfvr2sign7 <- ifelse(data$i289o == "y" | data$i265o == 
##             "y", 1, 0)
##         data$sepsisfvr2sign9 <- ifelse(data$i107o == "y", 1, 
##             0)
##         data$sepsisfvr2sign10 <- ifelse(data$i286o == "y" | data$i281o == 
##             "y", 1, 0)
##         data$sepsisfvr2sign12 <- ifelse(data$i172o == "y" | data$i173b == 
##             "y", 1, 0)
##         data$sepsisfvr2signs = data$sepsisfvr2sign1 + data$sepsisfvr2sign3 + 
##             data$sepsisfvr2sign5 + data$sepsisfvr2sign9 + data$sepsisfvr2sign10 + 
##             data$sepsisfvr2sign12
##         data$sepsisfvr2_2 <- ifelse(data$sepsisfvr == 1 | data$sepsisfvr2signs > 
##             1, 1, 2)
##         table(data$sepsisfvr2_2, exclude = NULL)
##         data$sepsisfvr2_2_nonnt1 <- ifelse(data$sepsisfvr2_2 == 
##             1 & data$nnt1 == 2, 1, 2)
##         table(data$sepsisfvr2_2_nonnt1, exclude = NULL)
##         data$possiblepneumonia9 <- ifelse(data$i159o == "y" & 
##             data$sepsisfvr2_2 == 1 & data$pneumonia157 == 2, 
##             1, 2)
##         table(data$possiblepneumonia9, exclude = NULL)
##         data$jaundice2 <- ifelse((data$i289o == "y" | data$i265o == 
##             "y") & (data$i273o == "y" | data$i286o == "y" | data$i215o == 
##             "y" | data$i282o == "y" | data$i283o == "y") & (data$i147o == 
##             "n" & data$i284o == "n"), 1, 2)
##         table(data$jaundice2, exclude = NULL)
##         data$hemorrhageNN <- ifelse(data$i241o == "y" & data$i147o == 
##             "n" & data$i284o == "n", 1, 2)
##         table(data$hemorrhageNN, exclude = NULL)
##         data$suid <- ifelse(data$i290o == "y" & data$i115o != 
##             "y" & data$i370o != "y" & data$i111o != "n" & data$i112o != 
##             "y" & data$i113o != "y" & (data$i105o != "n" | data$i106a == 
##             "n") & data$i107o != "y" & data$i271o != "n" & data$i273o != 
##             "y" & data$i159o != "y" & data$i166o != "y" & data$i172o != 
##             "y" & data$i173b != "y" & data$i219o != "y" & data$i147o != 
##             "y" & data$i284o != "y" & data$i286o != "y" & data$i281o != 
##             "y" & data$i278o != "y" & data$i287o != "y" & data$i288o != 
##             "y" & data$i240o != "y" & data$i239o != "y" & data$i241o != 
##             "y" & data$i181o != "y" & data$i188o != "y" & data$i289o != 
##             "y" & data$i265o != "y", 1, 2)
##         table(data$suid, exclude = NULL)
##         data$allexpertdxs <- NA
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$nnt1 == 
##             1] <- "NNT"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$congmalf2 == 
##             1] <- "Malformation"
##         data$allexpertdxs[is.na(data$allexpertdxs) & (data$ba5 == 
##             1 | data$bi5 == 1)] <- "Intrapartum"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$preterm_all_mo == 
##             1] <- "Preterm"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$meningitis451_nonnt1 == 
##             1] <- "Meningitis"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$diarrhea8 == 
##             1] <- "Diarrhea"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$pneumonia157 == 
##             1] <- "Pneumonia"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$possiblediar8_8 == 
##             1] <- "Diarrhea"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$possiblepneumonia9 == 
##             1] <- "Pneumonia"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$sepsisfvr2_2_nonnt1 == 
##             1] <- "Sepsis"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$jaundice2 == 
##             1] <- "Other"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$hemorrhageNN == 
##             1] <- "Other"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$suid == 
##             1] <- "Other"
##         data$allexpertdxs[is.na(data$allexpertdxs)] <- "Unspecified"
##     }


#------------------------------------------------------------------------------#
# Child
#------------------------------------------------------------------------------#

eava_cod_child <- c("Malformation", "Intrapartum", "Preterm",
                    "Injury", "AIDS", "Measles", "Malnutrition", "Other infections",
                    "Meningitis/Encephalitis", "Diarrhea/Dysentery", "Pneumonia", 
                    "Unspecified")

out <- odk2EAVA(data_public, "comsa_id")

dim(out)
for (i in 1:12) {
  out <- rbind(out, out)
}
dim(out)
names(out)[6:17]
out[,6:17] <- "n"
names(out)[11]
out[,11] <- "y"
out[, "age"] <- sample(28:1800, nrow(out), replace = TRUE)

# Malformation
## data$congmalf2 <- ifelse(data$i373o == "y" | data$i372o == 
##             "y" | data$i371o == "y" | data$i370o == "y", 1, 2)
idx <- 1:1588
out[idx, "i373o"] <- "y"
idx <- 1589:3214
out[idx, "i370o"] <- "y"

# Intrapartum
##         data$ba5 <- ifelse(data$age < 4 * 30.4 & ((data$i106a == 
##             "y" | data$i111o == "n") & ((data$i271o == "n" | 
##             data$i219o == "y" | data$i275o == "y" | data$i276o == 
##             "y" | data$i286o == "y") | data$age == 0)), 1, 2)
idx <- 3215:5578
out[idx, "i106a"] <- "y"
out[idx, "i271o"] <- "n"
idx <- 5579:6217
out[idx, "i111o"] <- "n"
out[idx, "i219o"] <- "y"

# Preterm
##         data$preterm_all_mo <- ifelse((data$i367b %in% "y" & 
##             data$i166o %in% "y" & data$fb_day0 == "y" & data$i147o %in% 
##             "n" & data$i284o %in% "n") | data$i367c %in% "y", 
##             1, 2)
idx <- 6218:8744
out[idx, "i367b"] <- "y"
out[idx, "i166o"] <- "y"
out[idx, "fb_day0"] <- "y"
out[idx, "147o"] <- "n"
idx <- 8744:12066
out[idx, "i367c"] <- "y"

# Injury
##         data$injury <- ifelse(data$i077o %in% "y", 1, 2)
##         table(data$injury, exclude = NULL)
##         "%!in%" <- function(x, y) !(x %in% y)
##         data$injury3_slide15_4 <- ifelse((data$injury == 1 & 
##             data$i120d == "y") | (data$injury == 1 & data$AIDS5 != 
##             1 & data$measles4 != 1 & data$meningitis != 1 & data$dysentery8 != 
##             1 & data$diarrhea8 != 1 & data$pneumoniafb2daysgr != 
##             1 & data$malaria251 != 1 & data$possibledysn8_4 != 
##             1 & data$possiblediar8_4 != 1 & data$possibleari3 != 
##             1 & data$hemfever != 1 & data$sepsis_nomal251 != 
##             1 & data$residual_infect_slide15_4 != 1) | (data$injury == 
##             1 & data$i147o == "y"), 1, 2)
idx <- 12067:14471
names(out)
out[idx, 22:ncol(out)] <- "."
out[idx, "i077o"] <- "y"
out[idx, "i120d"] <- "y"
out[idx, "age"] <- sample(28:1800, length(idx), replace = TRUE)
out[, "age"] <- as.numeric(out[, "age"])

# AIDS
##         data$AIDS5 <- ifelse(data$i127o == "y", 1, ifelse((data$i126o == 
##             "yes" | data$i446o == "yes") & data$K > 2, 1, ifelse(((data$i126 %in% 
##             c("dk", "ref") & data$i446o %in% c("dk", "ref")) | 
##             (data$i126o %in% c("dk", "ref") & data$i446o == c("no")) | 
##             (data$i126o == c("no") & data$i446o %in% c("dk", 
##                 "ref"))) & (data$i256o == c("y") | data$i245o == 
##             c("y")) & data$K > 2, 1, 2)))

##         data$allexpertdxs[is.na(data$allexpertdxs) & data$AIDS5 == 
##             1 & data$diardysn8 == 1] <- "AIDS"
idx <- 14472:16821
## names(out)
## out[idx, 22:ncol(out)] <- "."
## out[idx, "i077o"] <- "y"
## out[idx, "i120d"] <- "y"


# Measles
##         data$measles4 <- ifelse(data$age >= 120 & (data$i147o == 
##             "y" & data$i148e == "y") & (data$i233o == "y" & data$i234d > 
##             2) & (data$i235a == "y" | data$i235d == "y"), 1, 
##             2)
idx <- 16822:21107
out[idx, "i147o"] <- "y"
out[idx, "i148e"] <- "y"
out[idx, "i233o"] <- "y"
out[idx, "i234d"] <- 4
out[, "i234d"] <- as.numeric(out[, "i234d"])  ## I think this is a bug!
out[idx, "i235d"] <- "y"

# Malnutrition
##         data$malnutrition2 <- ifelse(data$i249o == "y" & data$i250b >= 
##             data$i120c, 1, 2)
## ...
##         data$diarrhea8 <- ifelse((data$i181o == "y" & data$i183c == 
##             "y" & data$i186o == "n") | (data$i181o == "y" & data$i182e == 
##             "y" & data$i186o == "n"), 1, 2)
## ...
##         data$diardysn8 <- ifelse(data$diarrhea8 == 1 | data$dysentery8 == 
##             1, 1, 2)
## ...
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$malnutrition2 == 
##             1 & data$diardysn8 == 1] <- "Malnutrition"
idx <- 21108:24084
out[idx, "i249o"] <- "y"
out[, "i250b"] <- as.numeric(out[, "i250b"])  ## I think another bug (i250b and i120c are strings but treated as numeric)
out[idx, "i250b"] <- 8
out[, "i120c"] <- as.numeric(out[, "i120c"])  ## I think another bug (i250b and i120c are strings but treated as numeric)
out[idx, "i120c"] <- 3
out[idx, "i181o"] <- "y"
out[idx, "i183c"] <- "y"
out[idx, "i186o"] <- "n"

# Other infections
##         data$pertussis <- ifelse((data$i153o == "y" & data$i154c == 
##             "y") & (data$i156o == "y" | data$i158o == "y" | data$i173b == 
##             "y"), 1, 2)
## ...
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$pertussis == 
##             1] <- "Other infections"
idx <- 24085:26261
out[idx, "i153o"] <- "y"
out[idx, "i156o"] <- "y"

# Meningitis/Encephalitis
##         data$meningitis <- ifelse(data$i147o == "y" & (data$i208o == 
##             "y" | data$i278o == "y"), 1, 2)
idx <- 26262:29112
out[idx, "i147o"] <- "y"
out[idx, "i208o"] <- "y"

# Diarrhea/Dysentery
##         data$diarrhea8 <- ifelse((data$i181o == "y" & data$i183c == 
##             "y" & data$i186o == "n") | (data$i181o == "y" & data$i182e == 
##             "y" & data$i186o == "n"), 1, 2)
##
## ..
##         data$dysentery8 <- ifelse((data$i181o == "y" & data$i183c == 
##             "y" & data$i186o == "y") | (data$i181o == "y" & data$i182e == 
##             "y" & data$i186o == "y"), 1, 2)
## ..
##         data$diardysn8 <- ifelse(data$diarrhea8 == 1 | data$dysentery8 == 
##             1, 1, 2)
idx <- 29113:36816
out[idx, "i181o"] <- "y"
out[idx, "i183c"] <- "y"
out[idx, "i186o"] <- "n"

# Pneumonia
##         data$pneumoniafb2daysgr <- ifelse(((data$i153o %in% "y" & 
##             data$i154d == "y") | (data$i159o %in% "y" & data$i161a == 
##             "y")) & ((data$i166o %in% "y" & data$i167d == "y") | 
##             data$i172o == "y" | data$i173c == "y"), 1, 2)
idx <- 36817: 38703
out[idx, "i153o"] <- "y"
out[idx, "i159o"] <- "y"
out[idx, "i161a"] <- "y"
out[idx, "i166o"] <- "y"
out[idx, "i167d"] <- "y"

# Unspecified

results <- codEAVA(out, "child")
table(results$cause)

new_order <- sample(1:nrow(out), nrow(out))
RandomEAVA_C <- out[new_order,]
RandomEAVA_C$ID <- paste0("ID_", 1:nrow(out))
RandomEAVA_C <- RandomEAVA_C[1:8362,]
save(RandomEAVA_C, file = "/Users/thomas.3912/GitHub/openVA/data/RandomEAVA_C.rda")


#------------------------------------------------------------------------------#
# codEAVA
#------------------------------------------------------------------------------#
##     if (age_group == "child") {
##         data <- df
##         data <- subset(data, age >= 28 & age < 60 * 30.4)
##         data$malnutrition1 <- ifelse(data$i244o %in% "y" | data$i249o %in% 
##             "y", 1, 2)
##         table(data$malnutrition1, exclude = NULL)
##         data$malnutrition2 <- ifelse(data$i249o == "y" & data$i250b >= 
##             data$i120c, 1, 2)
##         table(data$malnutrition2, exclude = NULL)
##         data$a <- ifelse(data$i244o %in% "y", 1, 0)
##         data$b <- ifelse(data$i200o %in% "y", 1, 0)
##         data$c <- ifelse(data$i256o %in% "y", 1, 0)
##         data$d <- ifelse(data$i245o %in% "y", 1, 0)
##         data$e1 <- ifelse(data$i181o %in% "y" & data$i182d == 
##             "y", 1, 0)
##         data$f1 <- ifelse(data$i147o %in% "y" & data$i148d == 
##             "y", 1, 0)
##         data$g1 <- ifelse(data$i233o %in% "y" & data$i234c == 
##             "y", 1, 0)
##         data$h <- ifelse(data$i166o %in% "y", 1, 0)
##         data$i <- ifelse(data$i172o %in% "y", 1, 0)
##         data$K = data$a + data$b + data$e1 + data$f1 + data$g1 + 
##             data$h + data$i
##         data$AIDS5 <- ifelse(data$i127o == "y", 1, ifelse((data$i126o == 
##             "yes" | data$i446o == "yes") & data$K > 2, 1, ifelse(((data$i126 %in% 
##             c("dk", "ref") & data$i446o %in% c("dk", "ref")) | 
##             (data$i126o %in% c("dk", "ref") & data$i446o == c("no")) | 
##             (data$i126o == c("no") & data$i446o %in% c("dk", 
##                 "ref"))) & (data$i256o == c("y") | data$i245o == 
##             c("y")) & data$K > 2, 1, 2)))
##         table(data$AIDS5, exclude = NULL)
##         data$diarrhea8 <- ifelse((data$i181o == "y" & data$i183c == 
##             "y" & data$i186o == "n") | (data$i181o == "y" & data$i182e == 
##             "y" & data$i186o == "n"), 1, 2)
##         table(data$diarrhea8, exclude = NULL)
##         data$dysentery8 <- ifelse((data$i181o == "y" & data$i183c == 
##             "y" & data$i186o == "y") | (data$i181o == "y" & data$i182e == 
##             "y" & data$i186o == "y"), 1, 2)
##         table(data$dysentery8, exclude = NULL)
##         data$diardysn8 <- ifelse(data$diarrhea8 == 1 | data$dysentery8 == 
##             1, 1, 2)
##         table(data$diardysn8, exclude = NULL)
##         data$possiblediar8_4 <- ifelse(data$i181o %in% "y" & 
##             (data$i147o %in% "y" | data$i219o %in% "y" | data$i218 %in% 
##                 "y") & data$i186o %in% "n" & data$diarrhea8 %in% 
##             2, 1, 2)
##         table(data$possiblediar8_4, exclude = NULL)
##         data$possibledysn8_4 <- ifelse(data$i181o %in% "y" & 
##             (data$i147o %in% "y" | data$i219o %in% "y" | data$i218o %in% 
##                 "y") & data$i186o %in% "y" & data$dysentery8 %in% 
##             2, 1, 2)
##         table(data$possibledysn8_4, exclude = NULL)
##         data$possdiardysn8_4 <- ifelse(data$possiblediar8_4 == 
##             1 | data$possibledysn8_4 == 1, 1, 2)
##         table(data$possdiardysn8_4, exclude = NULL)
##         data$hemfever <- ifelse(data$i147o %in% "y" & (data$i241o %in% 
##             "y" | data$i239o %in% "y"), 1, 2)
##         table(data$hemfever, exclude = NULL)
##         data$malaria251 <- ifelse((data$i147o == "y" & data$i149o %in% 
##             "yes" & data$i151b == "y" & data$i208o == "n" & ((data$age < 
##             365.25 & data$i278o %in% "n") | data$age >= 365.25) & 
##             (data$i268o %in% "y" | data$i159o %in% "y" | data$i219o %in% 
##                 "y" | data$i218o %in% "y")) | (data$i147o %in% 
##             c("y") & data$i149o %in% c("y") & data$i150a %in% 
##             "y" & data$i208o %in% "n" & ((data$age < 365.25 & 
##             data$i278o %in% "n") | data$age >= 365.25) & (data$i268o %in% 
##             "y" | data$i219o %in% "y" | data$i218o %in% "y")), 
##             1, 2)
##         table(data$malaria251, exclude = NULL)
##         data$measles4 <- ifelse(data$age >= 120 & (data$i147o == 
##             "y" & data$i148e == "y") & (data$i233o == "y" & data$i234d > 
##             2) & (data$i235a == "y" | data$i235d == "y"), 1, 
##             2)
##         table(data$measles4, exclude = NULL)
##         data$meningitis <- ifelse(data$i147o == "y" & (data$i208o == 
##             "y" | data$i278o == "y"), 1, 2)
##         table(data$meningitis, exclude = NULL)
##         data$pertussis <- ifelse((data$i153o == "y" & data$i154c == 
##             "y") & (data$i156o == "y" | data$i158o == "y" | data$i173b == 
##             "y"), 1, 2)
##         table(data$pertussis, exclude = NULL)
##         data$pneumoniafb2daysgr <- ifelse(((data$i153o %in% "y" & 
##             data$i154d == "y") | (data$i159o %in% "y" & data$i161a == 
##             "y")) & ((data$i166o %in% "y" & data$i167d == "y") | 
##             data$i172o == "y" | data$i173c == "y"), 1, 2)
##         table(data$pneumoniafb2daysgr, exclude = NULL)
##         data$possibleari3 <- ifelse((data$i153o %in% "y" | data$i159o %in% 
##             "y" | (data$i166o %in% "y" & (data$i172o %in% "y" | 
##             data$i173c %in% "y"))) & (data$i156o %in% "y" | data$i166o %in% 
##             "y" | data$i172o %in% "y" | data$i173c %in% "y" | 
##             data$i147o %in% "y" | data$i219o %in% "y" | data$i218o %in% 
##             "yes") & data$pertussis %in% 2 & data$pneumoniafb2daysgr %in% 
##             2, 1, 2)
##         table(data$possibleari3, exclude = NULL)
##         data$sepsis_nomal251 <- ifelse(data$i147o %in% "y" & 
##             (data$i235b == "y" | data$i235d == "y" | data$i219o %in% 
##                 "y" | data$i218o %in% "y") & data$malaria251 %in% 
##             2, 1, 2)
##         table(data$sepsis_nomal251, exclude = NULL)
##         data$residual_infect_slide15_4 <- ifelse(data$i147o %in% 
##             "y" & (data$AIDS5 %in% 2 & data$measles4 %in% 2 & 
##             data$meningitis %in% 2 & data$malaria251 %in% 2 & 
##             data$dysentery8 %in% 2 & data$diarrhea8 %in% 2 & 
##             data$pertussis %in% 2 & data$pneumoniafb2daysgr %in% 
##             2 & data$sepsis_nomal251 %in% 2 & data$possibledysn8_4 %in% 
##             2 & data$possiblediar8_4 %in% 2 & data$possibleari3 %in% 
##             2 & data$hemfever %in% 2), 1, 2)
##         table(data$residual_infect_slide15_4, exclude = NULL)
##         data$injury <- ifelse(data$i077o %in% "y", 1, 2)
##         table(data$injury, exclude = NULL)
##         "%!in%" <- function(x, y) !(x %in% y)
##         data$injury3_slide15_4 <- ifelse((data$injury == 1 & 
##             data$i120d == "y") | (data$injury == 1 & data$AIDS5 != 
##             1 & data$measles4 != 1 & data$meningitis != 1 & data$dysentery8 != 
##             1 & data$diarrhea8 != 1 & data$pneumoniafb2daysgr != 
##             1 & data$malaria251 != 1 & data$possibledysn8_4 != 
##             1 & data$possiblediar8_4 != 1 & data$possibleari3 != 
##             1 & data$hemfever != 1 & data$sepsis_nomal251 != 
##             1 & data$residual_infect_slide15_4 != 1) | (data$injury == 
##             1 & data$i147o == "y"), 1, 2)
##         table(data$injury3_slide15_4, exclude = NULL)
##         data$congmalf2 <- ifelse(data$i373o == "y" | data$i372o == 
##             "y" | data$i371o == "y" | data$i370o == "y", 1, 2)
##         table(data$congmalf2, exclude = NULL)
##         data$bi5 <- ifelse(data$age < 4 * 30.4 & data$i115o %in% 
##             "y", 1, 2)
##         table(data$bi5, exclude = NULL)
##         data$ba5 <- ifelse(data$age < 4 * 30.4 & ((data$i106a == 
##             "y" | data$i111o == "n") & ((data$i271o == "n" | 
##             data$i219o == "y" | data$i275o == "y" | data$i276o == 
##             "y" | data$i286o == "y") | data$age == 0)), 1, 2)
##         table(data$ba5, exclude = NULL)
##         data$preterm_all_mo <- ifelse((data$i367b %in% "y" & 
##             data$i166o %in% "y" & data$fb_day0 == "y" & data$i147o %in% 
##             "n" & data$i284o %in% "n") | data$i367c %in% "y", 
##             1, 2)
##         table(data$preterm_all_mo, exclude = NULL)
##         data$allexpertdxs <- NA
##         data$allexpertdxs[is.na(data$allexpertdxs) & (data$ba5 == 
##             1 | data$bi5 == 1)] <- "Intrapartum"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$congmalf2 == 
##             1] <- "Malformation"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$preterm_all_mo == 
##             1] <- "Preterm"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$injury3_slide15_4 == 
##             1] <- "Injury"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$AIDS5 == 
##             1 & data$measles4 == 2 & data$meningitis == 2 & data$pertussis == 
##             2 & data$malaria251 == 2 & data$hemfever == 2] <- "AIDS"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$measles4 == 
##             1] <- "Measles"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$meningitis == 
##             1] <- "Meningitis/Encephalitis"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$AIDS5 == 
##             1 & data$diardysn8 == 1] <- "AIDS"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$malnutrition2 == 
##             1 & data$diardysn8 == 1] <- "Malnutrition"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$diardysn8 == 
##             1] <- "Diarrhea/Dysentery"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$pertussis == 
##             1] <- "Other infections"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$AIDS5 == 
##             1 & data$pneumoniafb2daysgr == 1] <- "AIDS"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$malnutrition2 == 
##             1 & data$pneumoniafb2daysgr == 1] <- "Malnutrition"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$pneumoniafb2daysgr == 
##             1] <- "Pneumonia"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$malaria251 == 
##             1] <- "Malaria"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$AIDS5 == 
##             1 & (data$possdiardysn8_4 == 1 | data$possibleari3 == 
##             1)] <- "AIDS"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$malnutrition2 == 
##             1 & (data$possdiardysn8_4 == 1 | data$possibleari3 == 
##             1)] <- "Malnutrition"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$possdiardysn8_4 == 
##             1] <- "Diarrhea/Dysentery"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$possibleari3 == 
##             1] <- "Pneumonia"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$hemfever == 
##             1] <- "Other infections"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$sepsis_nomal251 == 
##             1] <- "Other infections"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$residual_infect_slide15_4 == 
##             1] <- "Other infections"
##         data$allexpertdxs[is.na(data$allexpertdxs) & data$malnutrition1 == 
##             1] <- "Malnutrition"
##         data$allexpertdxs[is.na(data$allexpertdxs)] <- "Unspecified"
##     }
##     eava_cod <- data[, c("ID", "allexpertdxs")]
##     names(eava_cod)[names(eava_cod) == "allexpertdxs"] <- "cause"
##     return(as.data.frame(eava_cod, stringsAsFactors = FALSE))
## }
