test_that("bearing angle works", {
  expect_equal(nnba(x = rep(1,4),
                    y = c(1, 2.1, 3, 4.1), 
                    hs = rep(pi/2, 4)
                    ), 
               c(0, 0, pi, pi))
  
  expect_equal(nnba(y = rep(1,4),
                    x = c(1, 3, 4.1, 5), 
                    hs = rep(pi/4, 4)
                    ), 
               c(pi/4, pi/4, pi/4, -3*pi/4))
})
