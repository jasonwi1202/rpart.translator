
context("rpart.translator input/output")

test_that("rpart.translator runs against binary classification", {

  library(rpart)

  fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)

  expect_type(rpart.translator(fit, kyphosis), "list")

})

test_that("rpart.translator runs against multiclass classification", {

  library(rpart)

  fit <- rpart(Species ~ ., data = iris, method = "class")

  expect_type(rpart.translator(fit, iris), "list")

})

test_that("rpart.translator has correct columns", {

  library(rpart)

  fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)

  rules <- rpart.translator(fit, kyphosis)

  expect_equal(colnames(rules)[1], "Rule")
  expect_equal(colnames(rules)[2], "Subrule")
  expect_equal(colnames(rules)[3], "Variable")
  expect_equal(colnames(rules)[4], "Operator")
  expect_equal(colnames(rules)[5], "Value")
  expect_equal(colnames(rules)[6], "TotalVolume")
  expect_equal(colnames(rules)[7], "TargetPredicted")
  expect_equal(colnames(rules)[8], "TargetPredictedVolume")

})

test_that("rpart.translator has correct example rule from kyphosis data set", {

  library(rpart)

  fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)

  rules <- rpart.translator(fit, kyphosis)

  expect_equal(as.character(lapply(rules[1,1], as.character)), "4")
  expect_equal(as.character(lapply(rules[1,2], as.character)), "L1")
  expect_equal(as.character(lapply(rules[1,3], as.character)), "Start")
  expect_equal(as.character(lapply(rules[1,4], as.character)), ">=")
  expect_equal(as.character(lapply(rules[1,5], as.character)), "8.5")
  expect_equal(as.character(lapply(rules[1,6], as.character)), "29")
  expect_equal(as.character(lapply(rules[1,7], as.character)), "absent")
  expect_equal(as.character(lapply(rules[1,8], as.character)), "29")

  expect_equal(as.character(lapply(rules[4,1], as.character)), "22")
  expect_equal(as.character(lapply(rules[4,2], as.character)), "L1")
  expect_equal(as.character(lapply(rules[4,3], as.character)), "Start")
  expect_equal(as.character(lapply(rules[4,4], as.character)), ">=")
  expect_equal(as.character(lapply(rules[4,5], as.character)), "8.5")
  expect_equal(as.character(lapply(rules[4,6], as.character)), "14")
  expect_equal(as.character(lapply(rules[4,7], as.character)), "absent")
  expect_equal(as.character(lapply(rules[4,8], as.character)), "12")

  expect_equal(as.character(lapply(rules[13,1], as.character)), "23")
  expect_equal(as.character(lapply(rules[13,2], as.character)), "R4")
  expect_equal(as.character(lapply(rules[13,3], as.character)), "Age")
  expect_equal(as.character(lapply(rules[13,4], as.character)), "<")
  expect_equal(as.character(lapply(rules[13,5], as.character)), "111")
  expect_equal(as.character(lapply(rules[13,6], as.character)), "7")
  expect_equal(as.character(lapply(rules[13,7], as.character)), "present")
  expect_equal(as.character(lapply(rules[13,8], as.character)), "4")

})

test_that("rpart.translator has correct rule volume from kyphosis data set", {

  library(rpart)

  fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)

  rules <- rpart.translator(fit, kyphosis)

  expect_equal(nrow(rules), 14)

})

test_that("rpart.translator has correct example rule from iris data set", {

  library(rpart)

  fit <- rpart(Species ~ ., data = iris, method = "class")

  rules <- rpart.translator(fit, iris)

  expect_equal(as.character(lapply(rules[1,1], as.character)), "6")
  expect_equal(as.character(lapply(rules[1,2], as.character)), "R1")
  expect_equal(as.character(lapply(rules[1,3], as.character)), "Petal.Length")
  expect_equal(as.character(lapply(rules[1,4], as.character)), ">=")
  expect_equal(as.character(lapply(rules[1,5], as.character)), "2.45")
  expect_equal(as.character(lapply(rules[1,6], as.character)), "54")
  expect_equal(as.character(lapply(rules[1,7], as.character)), "versicolor")
  expect_equal(as.character(lapply(rules[1,8], as.character)), "49")

  expect_equal(as.character(lapply(rules[4,1], as.character)), "2")
  expect_equal(as.character(lapply(rules[4,2], as.character)), "L1")
  expect_equal(as.character(lapply(rules[4,3], as.character)), "Petal.Length")
  expect_equal(as.character(lapply(rules[4,4], as.character)), "<")
  expect_equal(as.character(lapply(rules[4,5], as.character)), "2.45")
  expect_equal(as.character(lapply(rules[4,6], as.character)), "50")
  expect_equal(as.character(lapply(rules[4,7], as.character)), "setosa")
  expect_equal(as.character(lapply(rules[4,8], as.character)), "50")

  expect_equal(as.character(lapply(rules[5,1], as.character)), "6")
  expect_equal(as.character(lapply(rules[5,2], as.character)), "L2")
  expect_equal(as.character(lapply(rules[5,3], as.character)), "Petal.Width")
  expect_equal(as.character(lapply(rules[5,4], as.character)), "<")
  expect_equal(as.character(lapply(rules[5,5], as.character)), "1.75")
  expect_equal(as.character(lapply(rules[5,6], as.character)), "54")
  expect_equal(as.character(lapply(rules[5,7], as.character)), "versicolor")
  expect_equal(as.character(lapply(rules[5,8], as.character)), "49")

})

test_that("rpart.translator has correct rule volume from iris data set", {

  library(rpart)

  fit <- rpart(Species ~ ., data = iris, method = "class")

  rules <- rpart.translator(fit, iris)

  expect_equal(nrow(rules), 5)

})

test_that("rpart.translator incorrect/missing parameters return errors", {

  library(rpart)

  fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)

  expect_error(rpart.translator(NULL, kyphosis))
  expect_error(rpart.translator(fit, NULL))
  expect_error(rpart.translator("test", kyphosis))
  expect_error(rpart.translator(fit, "test"))

})

