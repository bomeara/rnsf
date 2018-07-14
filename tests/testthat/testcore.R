testthat("Downloading data works", {
  phylotastic <- nsf_return(keyword="phylotastic")
  expect_gte(nrow(phylotastic),1)
})
