context("Lorenz Int Function")
library(lorenz)

freqs_lst <- c(45, 31, 33, 27, 43, 40, 51, 50, 63, 97, 121, 132, 64, 54, 32, 12)
bounds <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 60000, 75000,
            100000, 125000, 150000, 200000)
mean_lst <- 66500


# Gini Check
test_that("lorenz_interp Gini check", {
  expect_equal(round(lorenz::lorenz_interp(freqs_lst, bounds = bounds, mean_lst, stat = 'gini', eta = .5), 5), 0.37079)
})


# Theil Check
test_that("lorenz_interp Theil check", {
  expect_equal(round(lorenz_interp(freqs_lst, bounds, mean_lst, stat = 'theil'), 5), 0.23841)
})


# Atkinson Check
test_that("lorenz_interp Atkinson check", {
  expect_equal(round(lorenz_interp(freqs_lst, bounds, mean_lst, stat = 'atkinson', eta = .5), 5), 0.12049)
})


