context("ConvertData")

# ConvertData()
id <- c("d1", "d2", "d3")
x <- matrix(c("Yes", "No", "Don't know",
              "No", "Refused to answer", "Yes",
              "Don't know", "Yes", "No"),
            byrow = TRUE, nrow = 3, ncol = 3)
x <- cbind(id, x)
colnames(x) <- c("ID", "S1", "S2", "S3")
new_data_2012 <- ConvertData(x, yesLabel = "Yes", noLabel = "No",
                             missLabel = c("Don't know", "Refused to answer"))
new_data_2016 <- ConvertData(x, yesLabel = "Yes", noLabel = "No",
                             missLabel = c("Don't know", "Refused to answer"),
                             data.type="WHO2016")

test_that("ConvertData - ConvertData 2012", {
  expect_s3_class(new_data_2012, "data.frame")
  expect_equal(colnames(x), colnames(new_data_2012))
  for (i in 2:ncol(new_data_2012)) {
    expect_setequal(new_data_2012[, i], c("Y", "", "."))
  }
  expect_equal(new_data_2012[1, 2], "Y")
  expect_equal(new_data_2012[2, 2], "")
  expect_equal(new_data_2012[3, 2], ".")
})

test_that("ConvertData - ConvertData 2016", {
  expect_s3_class(new_data_2016, "data.frame")
  expect_equal(colnames(x), colnames(new_data_2016))
  for (i in 2:ncol(new_data_2016)) {
    expect_setequal(new_data_2016[, i], c("y", "n", "-"))
  }
  expect_equal(new_data_2016[1, 2], "y")
  expect_equal(new_data_2016[2, 2], "n")
  expect_equal(new_data_2016[3, 2], "-")
})
