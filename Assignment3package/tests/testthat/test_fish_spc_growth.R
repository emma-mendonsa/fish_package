test_that("fish_spc_growth works", {
            expect_that(length(fish_spc_growth(a=1, b=1, c=1, d=1, temp_range = 1)), equals(2))
          })
