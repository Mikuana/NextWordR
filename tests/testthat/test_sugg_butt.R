context("Suggestion Buttons")

in_vec = c('test', 'again')

test_that("str_length returns character vectors as regex defined words", {
  expect_type(sugg_butt_name(in_vec[1]), "character")
  expect_equal(length(sugg_butt_name(in_vec[1])), 1)
  expect_equal(length(sugg_butt_name(in_vec)), 2)
  expect_true(grepl('\\w+', sugg_butt_name(in_vec[1])))
})
