context("Plotting")

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


grouping$broad_cause <- rep("everyone is the same", nrow(grouping))

# insilico
set.seed(13)
fit1 <- codeVA(data = test, data.type = "customize", model = "InSilicoVA",
         data.train = train, causes.train = "cause",
         Nsim=200, auto.length = FALSE)

# interva
set.seed(13)
fit2 <- codeVA(data = test, data.type = "customize", model = "InterVA",
           data.train = train, causes.train = "cause",
           version = "4.02", HIV = "h", Malaria = "l")

# tariff
set.seed(13)
fit3 <- codeVA(data = test, data.type = "customize", model = "Tariff",
             data.train = train, causes.train = "cause",
             nboot.sig = 100)

test_that("Insilico - plotVA", {
  s <- plotVA(fit1)
  s_data <- layer_data(s)
  expect_equal(sum(diff(s_data$y) > 0), 0)
})

test_that("InterVA - plotVA", {
  s <- plotVA(fit2)
  expect_equal(length(s), 13)
})


test_that("Tariff - plotVA", {
  s <- plotVA(fit3)
  ss <- plotVA(fit3, 5)
  expect_equal(length(s), 10)
  expect_equal(length(ss), 5)
})

test_that("Insilico - every symptom is the same broad cause", {

  s <- stackplotVA(fit1, grouping = grouping, type ="stack",
    ylim = c(0, 1), title = "InSilicoVA")
  s_data <- layer_data(s)
  ss <- plotVA(fit1)
  ss_data <- layer_data(ss)

  expect_equal(nrow(s_data), 1)
  expect_equal(max(s_data$ymax), 1)
  expect_equal(min(s_data$ymin), 0)
  expect_equal(sum(diff(ss_data$y) > 0), 0)

})

test_that("InterVA4 - every symptom is the same broad cause", {


  s <- stackplotVA(fit2, grouping = grouping, type = "stack",
    ylim = c(0, 1), title = "InterVA4.02")
  s_data <- layer_data(s)

  expect_equal(nrow(s_data), 1)
  expect_equal(max(s_data$ymax), 1)
  expect_equal(min(s_data$ymin), 0)

})


test_that("Tariff - every symptom is the same broad cause", {


  s <- stackplotVA(fit3, grouping = grouping, type = "stack",
  ylim = c(0, 1), title = "Tariff")
  s_data <- layer_data(s)

  expect_equal(nrow(s_data), 1)
  expect_equal(max(s_data$ymax), 1)
  expect_equal(min(s_data$ymin), 0)


})

test_that("InSilico - list of three identical runs", {

  l <- list(fit1, fit1, fit1)
  s <- stackplotVA(l, grouping = grouping, type ="stack", ylim = c(0, 1), title = "InSilicoVA")

  s_data <- layer_data(s)
  subset <- s_data[1:length(unique(grouping$broad_cause)),
                            !names(s_data) %in% c("x", "group", "xmax", "xmin")]
  generated <- rbind(subset, subset, subset)
  expect_equal(generated, s_data[, !names(s_data) %in% c("x", "group", "xmax", "xmin")])

})

test_that("InterVA4 - list of three identical runs", {

  l <- list(fit2, fit2, fit2)
  s <- stackplotVA(l, grouping = grouping, type = "stack",
    ylim = c(0, 1), title = "InterVA4.02")
  s_data <- layer_data(s)

  subset <- s_data[1:length(unique(grouping$broad_cause)),
                            !names(s_data) %in% c("x", "group", "xmax", "xmin")]
  generated <- rbind(subset, subset, subset)
  expect_equal(generated, s_data[, !names(s_data) %in% c("x", "group", "xmax", "xmin")])

})



test_that("Tariff - list of three identical runs", {

  l <- list(fit3, fit3, fit3)
  s <- stackplotVA(l, grouping = grouping, type = "stack",
  ylim = c(0, 1), title = "Tariff")
  s_data <- layer_data(s)

  subset <- s_data[1:length(unique(grouping$broad_cause)),
                            !names(s_data) %in% c("x", "group", "xmax", "xmin")]
  generated <- rbind(subset, subset, subset)
  expect_equal(generated, s_data[, !names(s_data) %in% c("x", "group", "xmax", "xmin")])

})


test_that("InterVA4 - category missing from grouping is handled", {

  set.seed(13)

  grouping <- grouping[1:(nrow(grouping)-1), ]

  fit2 <- codeVA(data = test, data.type = "customize", model = "InterVA",
                 data.train = train, causes.train = "cause",
                 version = "4.02", HIV = "h", Malaria = "l")

  expect_warning(stackplotVA(fit2, grouping = grouping, type = "stack",
                                  ylim = c(0, 1), title = "InterVA4.02"),
                                  "Causes exist in the CSMF that are not specified in the grouping. Automatically carrying through missed CODs.")

  s <- suppressWarnings(stackplotVA(fit2, grouping = grouping, type = "stack",
                                   ylim = c(0, 1), title = "InterVA4.02") )
  s_data <- layer_data(s)

  expect_equal(max(s_data$ymax), 1)
  expect_equal(min(s_data$ymin), 0)

})
