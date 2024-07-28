test_that(
  "Pythagoras is correct", {
    calc <- pythagoras(1:10, 10:1)
    expect_equal(
      sum(calc),
      87.37186249587302)
  })