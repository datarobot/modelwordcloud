test_that("it plots to the screen", {
  tmp <- file.path(tempdir(), "test.png")
  png(tmp)
  wordcloud(words = "word", freq = 2)
  dev.off()
  expect_true(file.exists(tmp))
  tmp_handle <- file(tmp, 'r')
  expect_true(length(suppressWarnings(readLines(tmp_handle))) > 0)
  close(tmp_handle)
  unlink(tmp)
})

local({
  tempfile <- tempfile()
  jpeg(tempfile)

  test_that("it plots one word in black", {
    wc <- wordcloud(words = "word", freq = 2, color = "black")
    expect_true(wc$status)
    expect_equal(length(wc$record), 1)
    record <- wc$record[[1]]
    expect_equal(record$word, "word")
    expect_equal(record$coefficient, NULL)
    expect_equal(record$color, "black")
  })

  test_that("it plots two words in black", {
    wc <- wordcloud(words = c("wise", "otherwise"),
                    freq = c(2, 2),
                    color = "black")
    expect_true(wc$status)
    expect_equal(length(wc$record), 2)
    expect_equal(wc$record[[1]]$word, "wise")
    expect_equal(wc$record[[2]]$word, "otherwise")
    for (record in wc$record) {
      expect_equal(record$coefficient, NULL)
      expect_equal(record$color, "black")
    }
  })

  test_that("it plots 20 words in black", {
    wc <- wordcloud(words = rep("word", 20),
                    freq = rep(2, 20),
                    color = "black")
    expect_true(wc$status)
    expect_equal(length(wc$record), 20)
    for (record in wc$record) {
      expect_equal(record$word, "word")
      expect_equal(record$coefficient, NULL)
      expect_equal(record$color, "black")
    }
  })

  describe("it plots five words in different sizes according to frequency", {
    wc <- wordcloud(words = rep("word", 5), freq = seq(10, 14))
    expect_true(wc$status)
    expect_equal(length(wc$record), 5)
    test_that("frequencies are plotted biggest to smallest", {
        expect_equal(vapply(wc$record, `[[`, numeric(1), "frequency"), rev(seq(10, 14)))
    })
    test_that("sizes are plotted biggest to smallest", {
        expect_true(all(diff(vapply(wc$record, `[[`, numeric(1), "size")) < 0))
    })
  })

  test_that("words get ordered by frequency", {
    wc <- wordcloud(words = c("smallest", "small", "biggest", "big"), 
                    freq = c(4, 5, 10, 8))
    expect_true(wc$status)
    expect_equal(vapply(wc$record, `[[`, character(1), "word"),
                 c("biggest", "big", "small", "smallest"))
  })

  test_that("it plots a rainbow", {
    rainbow <- c("red", "orange", "yellow", "green", "blue", "violet")
    wc <- wordcloud(words = rep("rainbow", length(rainbow)),
                    freq = rep(2, length(rainbow)),
                    color = rainbow)
    expect_true(wc$status)
    expect_equal(length(wc$record), length(rainbow))
    for (i in seq_along(wc$record)) {
      expect_equal(wc$record[[i]]$word, "rainbow")
      expect_equal(wc$record[[i]]$coefficient, NULL)
      expect_equal(wc$record[[i]]$color, rainbow[[i]])
    }
  })

  describe("it can color by coefficients", {
    data(iris)
    iris <- iris[sample(seq(nrow(iris))),]
    model <- lm(Petal.Width ~ Species, iris)
    words_and_freqs <- rle(sort(as.character(iris$Species)))
    freqs <- words_and_freqs$lengths
    words <- words_and_freqs$values
    coefficients <- model$coefficients
    colors <- c("red", "orange", "blue")
    wc <- wordcloud(words = words,
                    freq = freqs,
                    coefficients = coefficients,
                    colors = colors)
    test_that("the words are in the order of the coefficients", {
      expect_equal(vapply(wc$record, `[[`, character(1), "word"),
                   words[order(coefficients)])
    })
    test_that("the colors range across the coefficients", {
      expect_equal(vapply(wc$record, `[[`, character(1), "color"),
                   colors[order(coefficients)])
    })
    test_that("frequencies match the source material", {
      expect_equal(vapply(wc$record, `[[`, numeric(1), "frequency"),
                   freqs[order(coefficients)])
    })
  })

  test_that("it plots a rainbow in a random order when random_color is TRUE", {
    rainbow <- c("red", "orange", "yellow", "green", "blue", "violet")
    wc <- wordcloud(words = rep("rainbow", length(rainbow)),
                    freq = rep(2, length(rainbow)),
                    color = rainbow,
                    random_color = TRUE)
    expect_true(wc$status)
    colors <- vapply(wc$record, `[[`, character(1), "color")
    expect_true(any(colors != rainbow))
    expect_true(all(colors %in% rainbow))
  })

  test_that("it ignores words with frequency under min_freq", {
    wc <- wordcloud(words = c("no1", "no2", "yes1", "yes2", "yes3"),
                    freq = c(1, 2, 5, 6, 7),
                    min_freq = 4)
    expect_true(wc$status)
    expect_equal(length(wc$record), 3)
    expect_equal(vapply(wc$record, `[[`, character(1), "word"), c("yes3", "yes2", "yes1"))
  })

  test_that("it ignores words after max words", {
    wc <- wordcloud(words = c("no1", "no2", "no3", "yes1", "yes2"),
                    freq = c(5, 6, 7, 8, 9),
                    max_words = 2)
    expect_true(wc$status)
    expect_equal(length(wc$record), 2)
    expect_equal(vapply(wc$record, `[[`, character(1), "word"), c("yes2", "yes1"))
  })

  test_that("words are plotted in a random order when random_order is TRUE", {
    words_in <- c("a", "b", "c", "d", "e", "f", "g", "h")
    wc <- wordcloud(words = words_in,
                    freq = rep(5, length(words_in)),
                    random_order = TRUE)
    expect_true(wc$status)
    expect_equal(length(wc$record), length(words_in))
    words_out <- vapply(wc$record, `[[`, character(1), "word")
    expect_true(any(words_out != words_in))
    expect_true(all(sort(words_out) == sort(words_in)))
  })

  test_that("scale parameter", {
    wc1 <- wordcloud(words = c("one", "two", "three"), freq = c(11, 12, 13), scale = c(2, 0.2))
    wc2 <- wordcloud(words = c("one", "two", "three"), freq = c(11, 12, 13), scale = c(4, 0.4))
    expect_true(wc1$status)
    expect_true(wc2$status)
    expect_equal(length(wc1$record), 3)
    expect_equal(length(wc2$record), 3)
    scale_1 <- vapply(wc1$record, `[[`, numeric(1), "size")
    scale_2 <- vapply(wc2$record, `[[`, numeric(1), "size")
    expect_equal(max(scale_1), 2)
    expect_equal(max(scale_2), 4)
    expect_equal(min(scale_1), (2 - 0.2) * (11 / 13) + 0.2)
    expect_equal(min(scale_2), (4 - 0.4) * (11 / 13) + 0.4)
    expect_equal(scale_2, scale_1 * 2)
  })

  describe("when random_color is TRUE and coefficients is NULL", {
    test_that("if more than one color, length of colors must match the length of words", {
      expect_error(wordcloud(words = c("word", "otherword", "thirdword"),
                             freq = c(2, 3),
                             colors = c("black", "blue"),
                             random_color = FALSE,
                             coefficients = NULL),
                   "does not match length")
    })

    test_that("if only one color, length of colors does not need to match the length of words", {
      expect_true(wordcloud(words = c("word", "otherword", "thirdword"),
                            freq = c(12, 13, 14),
                            colors = "black",
                            random_color = FALSE,
                            coefficients = NULL)$status)
    })
  })

  test_that("it errors if nothing is passed", {
    expect_error(wordcloud(), "either pass a model_object or words")
  })

  test_that("it errors if a model_object and words are both passed", {
    data(iris)
    model <- lm(Petal.Width ~ Species, iris)
    expect_error(wordcloud(model_object = model, words = c("word", "otherword")),
                 "should not be specified if a model_object is also passed")
  })

  test_that("it errors if length(freq) != length(words)", {
    expect_error(wordcloud(words = c("word", "otherword", "thirdword"), freq = c(12, 13)),
                 "does not match length")
  })

  test_that("it errors if length(coefficients) != length(words)", {
    expect_error(wordcloud(words = c("word", "otherword", "thirdword"),
                           coefficients = c(0.1, 0.3),
                           freq = c(12, 13, 14)),
                 "does not match length")
  })

  dev.off()
  unlink(tempfile)
})
