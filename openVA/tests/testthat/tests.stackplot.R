context("Plotting")
library(openVA)

data("RandomVA3", envir = environment())
RandomVA3 <- get("RandomVA3", envir  = environment())
test <- RandomVA3[1:200, ]
train <- RandomVA3[201:400, ]

data("SampleCategory3", envir = environment())
SampleCategory3 <- get("SampleCategory3", envir  = environment())
grouping <- SampleCategory3

test_that("Insilico - every symptom is the same broad cause", {

  grouping$broad_cause <- rep("everyone is the same", nrow(grouping))

  # insilico
  set.seed(13)
  fit1 <- codeVA(data = test, data.type = "customize", model = "InSilicoVA",
               data.train = train, causes.train = "cause",
               Nsim=1000, auto.length = FALSE)
  s <- stackplotVA(fit1, grouping = grouping, type ="stack",
    ylim = c(0, 1), title = "InSilicoVA")
  s_data <- layer_data(s)

  expect_equal(nrow(s_data), 1)
  expect_equal(max(s_data$ymax), 1)
  expect_equal(min(s_data$ymin), 0)

})

test_that("InterVA4 - every symptom is the same broad cause", {

  grouping$broad_cause <- rep("everyone is the same", nrow(grouping))

  # interva
  set.seed(13)
  fit2 <- codeVA(data = test, data.type = "customize", model = "InterVA",
               data.train = train, causes.train = "cause",
               version = "4.02", HIV = "h", Malaria = "l")
  s <- stackplotVA(fit2, grouping = grouping, type = "stack",
    ylim = c(0, 1), title = "InterVA4.02")
  s_data <- layer_data(s)

  expect_equal(nrow(s_data), 1)
  expect_equal(max(s_data$ymax), 1)
  expect_equal(min(s_data$ymin), 0)

})

test_that("InterVA5 - every symptom is the same broad cause", {

  grouping$broad_cause <- rep("everyone is the same", nrow(grouping))

  # interva
  set.seed(13)
  fit2 <- codeVA(data = test, data.type = "customize", model = "InterVA",
               data.train = train, causes.train = "cause",
               version = "5.0", HIV = "h", Malaria = "l")
  s <- stackplotVA(fit2, grouping = grouping, type = "stack",
    ylim = c(0, 1), title = "InterVA5.0")
  s_data <- layer_data(s)

  expect_equal(nrow(s_data), 1)
  expect_equal(max(s_data$ymax), 1)
  expect_equal(min(s_data$ymin), 0)

})

test_that("Tariff - every symptom is the same broad cause", {

  grouping$broad_cause <- rep("everyone is the same", nrow(grouping))

  # tariff
  set.seed(13)
  fit3 <- codeVA(data = test, data.type = "customize", model = "Tariff",
                 data.train = train, causes.train = "cause",
                 nboot.sig = 100)
  s <- stackplotVA(fit3, grouping = grouping, type = "stack",
  ylim = c(0, 1), title = "Tariff")
  s_data <- layer_data(s)

  expect_equal(nrow(s_data), 1)
  expect_equal(max(s_data$ymax), 1)
  expect_equal(min(s_data$ymin), 0)


})

test_that("NBC - every symptom is the same broad cause", {

  grouping$broad_cause <- rep("everyone is the same", nrow(grouping))

  # nbc
  set.seed(13)
  fit4 <- codeVA(data = test, data.type = "customize", model = "NBC",
                 data.train = train, causes.train = "cause", known.nbc = TRUE)
  s <- stackplotVA(fit4, grouping = grouping, type = "stack",
  ylim = c(0, 1), title = "NBC")
  s_data <- layer_data(s)

  expect_equal(nrow(s_data), 1)
  expect_equal(max(s_data$ymax), 1)
  expect_equal(min(s_data$ymin), 0)

})

test_that("InSilico - list of three identical runs", {

  # insilico
  set.seed(13)
  fit1 <- codeVA(data = test, data.type = "customize", model = "InSilicoVA",
                 data.train = train, causes.train = "cause",
                 Nsim=1000, auto.length = FALSE)
  l <- list(fit1, fit1, fit1)
  s <- stackplotVA(l, grouping = grouping, type ="stack", ylim = c(0, 1), title = "InSilicoVA")

  s_data <- layer_data(s)
  subset <- s_data[1:length(unique(grouping$broad_cause)),
                            !names(s_data) %in% c("x", "group", "xmax", "xmin")]
  generated <- rbind(subset, subset, subset)
  expect_equal(generated, s_data[, !names(s_data) %in% c("x", "group", "xmax", "xmin")])

})

test_that("InterVA4 - list of three identical runs", {

  set.seed(13)
  fit2 <- codeVA(data = test, data.type = "customize", model = "InterVA",
               data.train = train, causes.train = "cause",
               version = "4.02", HIV = "h", Malaria = "l")
  l <- list(fit2, fit2, fit2)
  s <- stackplotVA(l, grouping = grouping, type = "stack",
    ylim = c(0, 1), title = "InterVA4.02")
  s_data <- layer_data(s)

  subset <- s_data[1:length(unique(grouping$broad_cause)),
                            !names(s_data) %in% c("x", "group", "xmax", "xmin")]
  generated <- rbind(subset, subset, subset)
  expect_equal(generated, s_data[, !names(s_data) %in% c("x", "group", "xmax", "xmin")])

})

test_that("InterVA5 - list of three identical runs", {

  set.seed(13)
  fit2 <- codeVA(data = test, data.type = "customize", model = "InterVA",
               data.train = train, causes.train = "cause",
               version = "5.0", HIV = "h", Malaria = "l")
  l <- list(fit2, fit2, fit2)
  s <- stackplotVA(l, grouping = grouping, type = "stack",
    ylim = c(0, 1), title = "InterVA5.0")
  s_data <- layer_data(s)

  subset <- s_data[1:length(unique(grouping$broad_cause)),
                            !names(s_data) %in% c("x", "group", "xmax", "xmin")]
  generated <- rbind(subset, subset, subset)
  expect_equal(generated, s_data[, !names(s_data) %in% c("x", "group", "xmax", "xmin")])

})

test_that("Tariff - list of three identical runs", {

  set.seed(13)
  fit3 <- codeVA(data = test, data.type = "customize", model = "Tariff",
                 data.train = train, causes.train = "cause",
                 nboot.sig = 100)
  l <- list(fit3, fit3, fit3)
  s <- stackplotVA(l, grouping = grouping, type = "stack",
  ylim = c(0, 1), title = "Tariff")
  s_data <- layer_data(s)

  subset <- s_data[1:length(unique(grouping$broad_cause)),
                            !names(s_data) %in% c("x", "group", "xmax", "xmin")]
  generated <- rbind(subset, subset, subset)
  expect_equal(generated, s_data[, !names(s_data) %in% c("x", "group", "xmax", "xmin")])

})

test_that("NBC - list of three identical runs", {

  set.seed(13)
  fit4 <- codeVA(data = test, data.type = "customize", model = "NBC",
                 data.train = train, causes.train = "cause", known.nbc = TRUE)
  l <- list(fit4, fit4, fit4)
  s <- stackplotVA(l, grouping = grouping, type = "stack",
  ylim = c(0, 1), title = "NBC")
  s_data <- layer_data(s)

  subset <- s_data[1:length(unique(grouping$broad_cause)),
                            !names(s_data) %in% c("x", "group", "xmax", "xmin")]
  generated <- rbind(subset, subset, subset)
  expect_equal(generated, s_data[, !names(s_data) %in% c("x", "group", "xmax", "xmin")])

})
