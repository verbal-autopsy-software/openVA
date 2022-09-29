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

# getPHMRC_url()

phmrc_url_adult = getPHMRC_url("adult")
phmrc_url_child = getPHMRC_url("child")
phmrc_url_neonate = getPHMRC_url("neonate")
phmrc_adult <- read.csv(phmrc_url_adult)
phmrc_child <- read.csv(phmrc_url_child)
phmrc_neonate <- read.csv(phmrc_url_neonate)

test_that("ConvertData - getPHMRC_url", {
  expect_s3_class(phmrc_url_adult, "url")
  expect_s3_class(phmrc_url_child, "url")
  expect_s3_class(phmrc_url_neonate, "url")
  expect_error(getPHMRC_url("males"))
  expect_equal("adult", tolower(phmrc_adult$module[1]))
  expect_equal("child", tolower(phmrc_child$module[1]))
  expect_equal("neonate", tolower(phmrc_neonate$module[1]))
})

# ConvertData.phmrc()

converted_adult <- ConvertData.phmrc(phmrc_adult, phmrc.type = "adult")
#converted_child <- ConvertData.phmrc(phmrc_child, phmrc.type = "child")
#converted_neonate <- ConvertData.phmrc(phmrc_neonate, phmrc.type = "neonate")

test_that("ConvertData - ConvertData.phmrc", {
  expect_s3_class(converted_adult$output, "data.frame")
  #expect_s3_class(converted_child$output, "data.frame")
  #expect_s3_class(converted_neonate$output, "data.frame")
  
  expect_equal(converted_adult$output$a2_02[phmrc_adult$a2_02 == "Yes"],
               rep("Y", sum(phmrc_adult$a2_02 == "Yes")))
  expect_equal(converted_adult$output$a2_02[phmrc_adult$a2_02 == "No"],
               rep("", sum(phmrc_adult$a2_02 == "No")))
  expect_equal(
    converted_adult$output$a2_02[phmrc_adult$a2_02 == "Don't Know" |
                                   phmrc_adult$a2_02 == "Refused to Answer"],
               rep(".", sum(phmrc_adult$a2_02 == "Don't Know" |
                             phmrc_adult$a2_02 == "Refused to Answer")))
})
