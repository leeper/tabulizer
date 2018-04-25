context("Non-latin characters tests")

sf <- system.file("examples", "text_non_latin.pdf", package = "tabulizer")

test_that("Extract text in Spanish", {
  txtes <- extract_text(sf, area = list(c(195, 125, 218, 401)))
  txtes <- gsub("[^[:print:]]", "", txtes)
  # Force NFD form
  sentes <- "Cada vez que me trabo, F\u0065\u0301lix paga un whisky a\u006e\u0303ejo."
  expect_identical(txtes, sentes)
})

test_that("Extract text as table in Spanish", {
  tablees <- extract_tables(sf, area = list(c(195, 125, 218, 401)), guess = FALSE)
  tablees <- unlist(gsub("[^[:print:]]", "", tablees))
  sentes <- "Cada vez que me trabo, Félix paga un whisky añejo."
  expect_identical(tablees, sentes)
})

test_that("Extract text in Russian", {
  txtru <- extract_text(sf, area = list(c(268, 123, 288, 470)))
  txtru <- gsub("[^[:print:]]", "", txtru)
  sentru <- "Съешь же ещё этих мягких французских булок да выпей чаю."
  expect_identical(txtru, sentru)
})

test_that("Extract text as table in Russian", {
  tableru <- extract_tables(sf, area = list(c(268, 123, 288, 470)), guess = FALSE)
  tableru <- unlist(gsub("[^[:print:]]", "", tableru))
  sentru <- "Съешь же ещё этих мягких французских булок да выпей чаю."
  expect_identical(tableru, sentru)
})

test_that("Extract text in Greek", {
  txtel <- extract_text(sf, area = list(c(340, 127, 359, 349)))
  txtel <- gsub("[^[:print:]]", "", txtel)
  sentel <- "Ξεσκεπάζω την ψυχοφθόρα σας βδελυγ\u00b5ία."
  expect_identical(txtel, sentel)
})

test_that("Extract text as table in Greek", {
  tableel <- extract_tables(sf, area = list(c(340, 127, 359, 349)), guess = FALSE)
  tableel <- unlist(gsub("[^[:print:]]", "", tableel))
  sentel <- "Ξεσκεπάζω την ψυχοφθόρα σας βδελυγμία."
  expect_identical(tableel, sentel)
})

test_that("Extract text in Japanese", {
  txtjp <- extract_text(sf, area = list(c(410, 107, 447, 505)))
  txtjp <- gsub("[^[:print:]]", "", txtjp)
  sentjp <- "いろはにほへと ちりぬるを わかよたれそ つねならむ うゐのおくやま けふこえて あさきゆめみし ゑひもせす."
  expect_identical(txtjp, sentjp)
})

test_that("Extract text as table in Japanese", {
  tablejp <- extract_tables(sf, area = list(c(410, 107, 447, 505)), guess = FALSE)
  tablejp <- paste(unlist(tablejp), collapse = "")
  sentjp <- "いろはにほへと ちりぬるを わかよたれそ つねならむ うゐのおくやま けふこえて あさきゆめみし ゑひもせす."
  expect_identical(tablejp, sentjp)
})
