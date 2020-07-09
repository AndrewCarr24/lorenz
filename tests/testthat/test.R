context("Lorenz Int Function")
library(lorenz)

freqs_lst <- c(45, 31, 33, 27, 43, 40, 51, 50, 63, 97, 121, 132, 64, 54, 32, 12)
bounds <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 60000, 75000,
            100000, 125000, 150000, 200000)
mean_lst <- 66500

load(system.file("inst/test_vec.rda",package="lorenz"))

test_that("lorenz_interp produces expected result", {
  expect_equal(lorenz_interp(freqs_lst, bounds, mean_lst), test_vec)
})


