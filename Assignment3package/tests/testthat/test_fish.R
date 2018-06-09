test_that(
  "testing fish_calcs function",
  {
    test_table = cbind.data.frame(rep(0, times = 5), rep(0, times = 5));

    Price <- rep(0, times = 5)
    fish_name <- rownames(test_table)

    test_price = cbind.data.frame(Price, fish_name)

    expect_that(length(fish_calcs(catch_table = test_table, test_price)),
      equals(5)
    )
  }
)


