context("Algorithm")

data("RandomVA3", envir = environment())
RandomVA3 <- get("RandomVA3", envir  = environment())
test <- RandomVA3[1:200, ]
train <- RandomVA3[201:400, ]

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
  s <- sum(!top$ID %in% test[, 1])
  csmf <- getCSMF(fit1)
  csmf0 <- table(train$cause) / dim(train)[1]
  acc <- getCSMF_accuracy(csmf = csmf[, 1], truth = csmf0)
  ss <- as.numeric((acc > 0.5) && (acc <= 1))

  expect_equal(s, 0)
  expect_equal(dim(top)[1], dim(test)[1])
  expect_equal(dim(csmf)[1], length(unique(train$cause)))
  expect_equal(dim(csmf)[2], 5)
  expect_equal(ss, 1)

})

test_that("InterVA4 - ID and output correct", {

  # interva
  set.seed(13)
  fit2 <- codeVA(data = test, data.type = "customize", model = "InterVA",
               data.train = train, causes.train = "cause",
               version = "4.02", HIV = "h", Malaria = "l")
  top <- getTopCOD(fit2)
  s <- sum(!top$ID %in% test[, 1])
  csmf <- getCSMF(fit2)
  csmf0 <- table(train$cause) / dim(train)[1]
  acc <- getCSMF_accuracy(csmf = csmf, truth = csmf0, undet = "Undetermined")
  ss <- as.numeric((acc > 0.5) && (acc <= 1))
  csmf2 <- getCSMF(fit2, interVA.rule = F)
  acc <- getCSMF_accuracy(csmf = csmf2, truth = csmf0)
  sss <- as.numeric((acc > 0.5) && (acc <= 1))

  expect_equal(s, 0)
  expect_equal(dim(top)[1], dim(test)[1])
  expect_equal(length(csmf), length(unique(train$cause)) + 1)
  expect_equal(length(csmf2), length(unique(train$cause)))
  expect_equal(ss, 1)
  expect_equal(sss, 1)


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
