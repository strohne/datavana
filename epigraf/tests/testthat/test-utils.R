
test_that("Dataframes with different column types are row binded", {

  df1 <- tibble::tribble(~x, ~y, ~z, ~a,
                 1L, "A", 1, "a",
                 2L, "B", 2, "b"
         )

  df2 <- tibble::tribble(~x, ~z, ~y, ~b,
                 "3", 3, "C", 11,
                 "X", 4, "D", 22
         )

  df <- bind_rows_char(list(df1, df2))

  expect_equal(sapply(df, class),c("x"="character","y"="character","z" = "numeric","a" = "character","b" = "numeric"))
})



test_that("Empty dataframes are row binded", {

  df1 <- tibble::tribble(~x, ~y, ~z, ~a,
                         1L, "A", 1, "a",
                         2L, "B", 2, "b"
  )

  df2 <- tibble::tibble()

  df <- bind_rows_char(list(df1, df2))

  expect_equal(sapply(df, class),c("x"="integer","y"="character","z" = "numeric","a" = "character"))
})
