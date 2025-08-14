library(openVA)
data(probbaseV5)
data(RandomVA5)

pb <- probbaseV5[-1,]

NeonatesVA5 <- RandomVA5
NeonatesVA5[] <- lapply(NeonatesVA5, as.character)
NeonatesVA5[, "i022g"] <- "y"
index_neo <- c("i022h", "i022i", "i022j", "i022k")
for (i in 1:nrow(NeonatesVA5)) {
  NeonatesVA5[i, index_neo] <- sample(c("y", "n", "n", "n"), size = 4)
}
index_dontask <- paste0("dontask", 1:8)
index_noneo <- apply(pb[, index_dontask], 1, function(x) any(x == "i022gY"))
ind_noneo <- pb[index_noneo, "indic"]
NeonatesVA5[, index_noneo] <- "n"
ind_neo <- pb[!index_noneo, "indic"]
# missed "i022l", "i022m", "i022n"
NeonatesVA5[, c("i022l", "i022m", "i022n")] <- "n"
NeonatesVA5[, "i077o"] <- "n"
external <- c("i079o", "i082o", "i083o", "i084o", "i085o",
              "i086o", "i087o", "i089o", "i090o", "i091o",
              "i092o", "i093o", "i094o", "i095o", "i096o",
              "i098o", "i099o", "i100o")
NeonatesVA5[, external] <- "n"

index_neo_only <- which(pb[, "nnonly"] == "i022gY")
nnonly_ind <- pb[index_neo_only, "indic"]
nnonly_ind
nnonly_ind <- nnonly_ind[5:length(nnonly_ind)]
nnonly_ind
for (ind in nnonly_ind) { # ind <- nnonly_ind[1]
  NeonatesVA5[, ind] <- sample(c("n", "y", "."),
                              size = nrow(NeonatesVA5),
                              replace = TRUE)
}

# add congenital malformation
# data("causetextV5")
# causetextV5
#
#[6,] "i370o"
#[7,] "i371o"
#[8,] "i372o"
#[10,] "i408o"
#[6,] "Was any part of the baby physically abnormal at time of delivery? (for example: body part too large or too small, additional growth on body)?"
#[7,] "Did the baby/child have a swelling or defect on the back?"                                                                                    
#[8,] "Did the baby/child have a very large head?"                                                                                                   
#[10,] "Before the illness that led to death, was the baby/child growing normally?" 
NeonatesVA5[seq(20, 200, 17), 20:ncol(NeonatesVA5)] <- "."
NeonatesVA5[seq(20, 200, 17), c("i370o", "i371o", "i372o")] <- "y"

# add birth asphyxia
high_grades <- c("I", "A+", "A", "A-", "B+", "B")
index_basph <- pb[,"b_1002"] %in% high_grades
table(index_basph)
indic_basph <- pb[index_basph, "indic"]
indic_basph
NeonatesVA5[seq(10, 200, 20), indic_basph[8:length(indic_basph)]] <- "y"

# add pneumonia
index_pneu <- pb[,"b_1003"] %in% high_grades
table(index_pneu)
indic_pneu <- pb[index_pneu, "indic"]
indic_pneu
indic_pneu <- indic_pneu[grep("o", indic_pneu)]
NeonatesVA5[seq(7, 200, 20), indic_pneu] <- "y"

out <- codeVA(NeonatesVA5)
table(getTopCOD(out)$cause1)
out <- codeVA(NeonatesVA5, model = "InterVA", version = "5", write = FALSE)
summary(out, top = 8)

save(NeonatesVA5, file = "data/NeonatesVA5.rda")
