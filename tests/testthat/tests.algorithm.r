context("Algorithm")

data("RandomVA3", envir = environment())
RandomVA3 <- get("RandomVA3", envir  = environment())
test <- RandomVA3[1:200, ]
train <- RandomVA3[201:400, ]

data("RandomVA5", envir = environment())
RandomVA5 <- get("RandomVA5", envir  = environment())

# generate a grouping dataset that can encompass every possible COD
data("SampleCategory3", envir = environment())
SampleCategory3 <- get("SampleCategory3", envir  = environment())
data("SampleCategory", envir = environment())
SampleCategory <- get("SampleCategory", envir  = environment())
names(SampleCategory) <- names(SampleCategory3)
grouping <- rbind(SampleCategory3, SampleCategory, data.frame(cause="Undetermined", broad_cause="Undetermined"))
grouping <- grouping[!duplicated(grouping$cause), ]

test_that("Insilico - ID and output correct", {

  # insilico
  set.seed(13)
  fit1 <- codeVA(data = test, data.type = "customize", model = "InSilicoVA",
               data.train = train, causes.train = "cause",
               Nsim=200, auto.length = FALSE)

  top <- getTopCOD(fit1)
  top5 <- getTopCOD(fit1, n = 5)
  s <- sum(!top$ID %in% test[, 1])
  csmf <- getCSMF(fit1)
  csmf0 <- table(train$cause) / dim(train)[1]
  acc <- getCSMF_accuracy(csmf = csmf[, 1], truth = csmf0)
  acc_from_fit <- getCSMF_accuracy(csmf = fit1, truth = csmf0)
  ss <- as.numeric((acc > 0.5) && (acc <= 1))
  ss_from_fit <- as.numeric(all(acc_from_fit > rep(0.5, 10)) && 
                              all(acc_from_fit <= rep(1, 10)))
  indiv_prob <- getIndivProb(fit1)
  indiv_prob_95ci <- getIndivProb(fit1, CI = 0.95)
  ccc <- getCCC(top, test[, 1:2])

  expect_equal(s, 0)
  expect_equal(dim(top)[1], dim(test)[1])
  expect_equal(dim(csmf)[1], length(unique(train$cause)))
  expect_equal(dim(csmf)[2], 5)
  expect_equal(ss, 1)
  expect_equal(ss_from_fit, 1)
  expect_equal(nrow(indiv_prob), nrow(test))
  expect_equal(ncol(indiv_prob), length(unique(train$cause)))
  for (i in 1:nrow(indiv_prob)) {
    expect_equal(sum(indiv_prob[i, ]), 1)
  }
  expect_equal(nrow(indiv_prob_95ci$indiv.prob.lower), nrow(test))
  expect_equal(ncol(indiv_prob_95ci$indiv.prob.lower), length(unique(train$cause)))
  for (i in 1:nrow(indiv_prob_95ci$indiv.prob.lower)) {
    expect_true(all(indiv_prob_95ci$indiv.prob.lower[i, ] < 
                      indiv_prob_95ci$median[i, ]))
  }
  expect_equal(nrow(indiv_prob_95ci$indiv.prob.upper), nrow(test))
  expect_equal(ncol(indiv_prob_95ci$indiv.prob.upper), length(unique(train$cause)))
  for (i in 1:nrow(indiv_prob_95ci$indiv.prob.upper)) {
    expect_true(all(indiv_prob_95ci$indiv.prob.upper[i, ] > 
                      indiv_prob_95ci$median[i, ]))
  }
  expect_true(ccc >= 0 & ccc <= 1)
  expect_equal(names(top5), 
               c("ID", "cause1", "cause2", "cause3", "cause4", "cause5"))
  expect_equal(nrow(top5), nrow(test))
})

test_that("InterVA4 - ID and output correct", {

  # interva
  set.seed(13)
  fit2 <- codeVA(data = test, data.type = "customize", model = "InterVA",
               data.train = train, causes.train = "cause",
               version = "4.02", HIV = "h", Malaria = "l")
  top <- getTopCOD(fit2)
  top5 <- getTopCOD(fit2, n = 5)
  s <- sum(!top$ID %in% test[, 1])
  csmf <- getCSMF(fit2)
  csmf0 <- table(train$cause) / dim(train)[1]
  acc <- getCSMF_accuracy(csmf = csmf, truth = csmf0, undet = "Undetermined")
  ss <- as.numeric((acc > 0.5) && (acc <= 1))
  csmf2 <- getCSMF(fit2, interVA.rule = F)
  acc <- getCSMF_accuracy(csmf = csmf2, truth = csmf0)
  sss <- as.numeric((acc > 0.5) && (acc <= 1))
  indiv_prob <- getIndivProb(fit2)

  expect_equal(s, 0)
  expect_equal(dim(top)[1], dim(test)[1])
  expect_equal(length(csmf), length(unique(train$cause)) + 1)
  expect_equal(length(csmf2), length(unique(train$cause)))
  expect_equal(ss, 1)
  expect_equal(sss, 1)
  expect_equal(names(top5), 
               c("ID", "cause1", "cause2", "cause3", "cause4", "cause5"))
  expect_equal(nrow(top5), nrow(test))
  expect_equal(nrow(indiv_prob), nrow(test))
  expect_equal(ncol(indiv_prob), length(unique(train$cause)))
  for (i in 1:nrow(indiv_prob)) {
    expect_equal(sum(indiv_prob[i, ]), 1)
  }


})

test_that("InterVA5 - ID and output correct", {
  
  # interva
  set.seed(13)
  fit2b <- codeVA(data = RandomVA5[1:10,], data.type = "WHO2016", model = "InterVA",
                  version = "5", HIV = "h", Malaria = "l", write=FALSE)
  top <- getTopCOD(fit2b)
  top5 <- getTopCOD(fit2b, n = 5)
  csmf <- getCSMF(fit2b)
  indiv_prob <- getIndivProb(fit2b)
  
  expect_equal(sum(csmf), 1)
  expect_equal(names(top5), 
               c("ID", "cause1", "cause2", "cause3", "cause4", "cause5"))
  expect_equal(nrow(top5), nrow(RandomVA5[1:10,]))
  expect_equal(nrow(indiv_prob), nrow(RandomVA5[1:10,]))
  expect_true(all(colnames(indiv_prob) %in% names(csmf)))
  for (i in 1:nrow(indiv_prob)) {
    expect_equal(sum(indiv_prob[i, ]), 1)
  }
})

test_that("Tariff - ID and output correct", {


  # tariff
  set.seed(13)
  fit3 <- codeVA(data = test, data.type = "customize", model = "Tariff",
                 data.train = train, causes.train = "cause",
                 nboot.sig = 100)
  top <- getTopCOD(fit3)
  s <- sum(!top$ID %in% test[, 1])
  csmf <- getCSMF(fit3)
  csmf0 <- table(train$cause) / dim(train)[1]
  acc <- getCSMF_accuracy(csmf = csmf, truth = csmf0)
  ss <- as.numeric((acc > 0.5) && (acc <= 1))
 
  expect_equal(s, 0)
  expect_equal(dim(top)[1], dim(test)[1])
  expect_equal(length(csmf), length(unique(train$cause)))
  expect_equal(ss, 1)

})

test_that("Convert Data", {
 id <- c("d1", "d2")
 x <- matrix(c("Yes", "No", "Don't know", 
        "Yes", "Refused to answer", "No"), 
      byrow = TRUE, nrow = 2, ncol = 3)
 x <- cbind(id, x)

 colnames(x) <- c("ID", "S1", "S2", "S3")
 new <- ConvertData(x, yesLabel = "Yes", noLabel = "No", 
      missLabel = c("Don't know", "Refused to answer"))
 new2 <- data.frame(ID = c("d1", "d2"), S1 = c("Y", "Y"), S2 = c("", "."), S3 = c(".", ""))
 expect_equal(new, new2)


})
