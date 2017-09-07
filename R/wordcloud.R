#' Make a word cloud.
#'
#' @param model_object lm. A linear model object. If this is passed, words, freq, and
#'   coefficients can be derived and do not need to be passed.
#' @param words character. A vector of words to plot.
#' @param freq numeric. The frequency of those words.
#' @param coefficients numeric. If provided, colors will be assigned according to coefficients.
#' @param colors character. The colors to use for plotting.
#' @param scale numeric. The range of sizes.
#' @param min_freq numeric. Words with less frequency than this will not be plotted.
#' @param max_words numeric. Don't plot more words than this amount.
#' @param random_order logical. Should words be plotted in a random_order or by frequency (default FALSE)?
#' @param random_color logical. Allocate words a color by random? (default FALSE).
#' @param rot_per numeric. Amount of rotation to apply to each word, between 0 and 1. Defaults to 0 (no rotation).
#' @param bg_color character. The color of the background.
#' @examples
#'   data(iris)
#'   model <- lm(Petal.Width ~ Species, iris)
#'   library(modelwordcloud)
#'   colors <- c("red", "orange", "blue")
#'   wordcloud(model, colors = colors)
#'   words_and_freqs <- rle(as.character(iris$Species))
#'   freqs <- words_and_freqs$lengths
#'   words <- words_and_freqs$values
#'   coefficients <- model$coefficients
#'   wordcloud(words = words, freq = freqs, coefficients = coefficients, colors = colors)
#' @export
wordcloud <- function(model_object = NULL, words = NULL, freq = NULL, coefficients = NULL,
                      colors = "black", scale = c(4, 0.5), min_freq = 3, max_words = Inf,
                      random_order = FALSE, random_color = FALSE, rot_per = 0,
                      bg_color = "#FFFFFF") { 

  if (is.null(model_object) && (is.null(freq) || is.null(words))) {
    stop("You must either pass a model_object or words and frequencies.")
  }
  if (!is.null(model_object) && (!is.null(words) || !is.null(freq) || !is.null(coefficients))) {
    stop("Words, frequencies, and coefficients will be derived from passed model object. ",
         "They should not be specified if a model_object is also passed.")
  }

  if (!is.null(model_object)) {
    features <- model_object$call$formula[[3]]
    if (length(features) == 1) {
      text_feature <- as.character(features)
      coefficients <- as.numeric(model_object$coefficients)
    } else {
      text_feature <- features[[2]]
      while (length(text_feature) > 1) {
        text_feature <- text_feature[[2]]
      }
      text_feature <- as.character(text_feature)
      warning("There is more than one parameter in your model. The first parameter, ",
              sQuote(text_feature), " is being used as your text feature.")
      coefficients <- as.numeric(model_object$coefficients)
      num_terms <- length(all.vars(model_object$call$formula))
      coefficients <- coefficients[seq(length(coefficients) - num_terms + 2)]
    }
    words_and_freqs <- rle(as.character(model_object$model[[text_feature]]))
    freq <- words_and_freqs$lengths
    words <- words_and_freqs$values
  }

  if (length(freq) != length(words)) {
    stop("Length of words does not match length of frequencies.")
  }
  if (!is.null(coefficients) && length(coefficients) != length(words)) {
    stop("Length of coefficients does not match length of words.")
  }

  ordered_colors <- (!isTRUE(random_color)) && is.null(coefficients)
  if (ordered_colors && (length(colors) != 1 && length(colors) != length(words))) {
     stop("Length of colors does not match length of word.")
  }

  tails <- "g|j|p|q|y"
  last <- 1

  if (length(colors) == 1) { colors <- rep(colors, length(words)) }

  if(min_freq > max(freq)) { min_freq <- 0 }

  overlap <- function(x1, y1, sw1, sh1) {
    s <- 0
    if (length(boxes) == 0) { return(FALSE) }
    for (i in c(last, seq_along(boxes))) {
      bnds <- boxes[[i]]
      x2 <- bnds[1]
      y2 <- bnds[2]
      sw2 <- bnds[3]
      sh2 <- bnds[4]
      if (x1 < x2) {
        overlap <- x1 + sw1 > x2 - s
      } else {
        overlap <- x2 + sw2 > x1 - s
      }
      if (y1 < y2) {
        overlap <- overlap && (y1 + sh1 > y2 - s)
      } else {
        overlap <- overlap && (y2 + sh2 > y1 - s)
      }
      if(overlap) {
        last <<- i
        return(TRUE)
      }
    }
    FALSE
  }
  
  rank <- rank(-freq, ties.method = "random")
  words <- words[rank <= max_words]
  freq <- freq[rank <= max_words]
  if (ordered_colors) {
    colors <- colors[rank <= max_words]
  }
  
  if (isTRUE(random_order)) {
    ord <- sample.int(length(words))
  } else {
    ord <- order(freq, decreasing = TRUE)
  }

  words <- words[ord]
  freq <- freq[ord]
  words <- words[freq >= min_freq]

  if (!is.null(coefficients)) {
    coefficients <- coefficients[ord][freq >= min_freq]
    # Scale coefficients between 0 and 1
    coefficients <- (coefficients - min(coefficients)) / diff(range(coefficients))
  }

  if (!ordered_colors) {
    colors <- colors[ord][freq >= min_freq]
  }

  freq <- freq[freq >= min_freq]
  
  thetaStep <- 0.1
  rStep <- 0.05
  graphics::plot.new()
  op <- graphics::par("mar")
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.window(c(0, 1), c(0, 1), asp = 1)
  normedFreq <- freq / max(freq)
  size <- (scale[1] - scale[2]) * normedFreq + scale[2]
  boxes <- list()
  plot_record <- list()
  
  for (i in seq_along(words)) {
    word <- words[[i]]
    rotWord <- stats::runif(1) < rot_per
    r <- 0
    theta <- stats::runif(1, 0, 2 * pi)
    x1 <- 0.5
    y1 <- 0.5
    wid <- graphics::strwidth(words[i], cex = size[i])
    ht <- graphics::strheight(words[i], cex = size[i])

    #mind your ps and qs
    if (grepl(tails, word)) { ht <- ht + ht * 0.2 }

    if (rotWord) {
      tmp <- ht
      ht <- wid
      wid <- tmp  
    }

    isOverlaped <- TRUE
    while (isOverlaped) {
      if (!overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht) &&
          x1 - 0.5 * wid > 0 && y1 - 0.5 * ht > 0 &&
          x1 + 0.5 * wid < 1 && y1 + 0.5 * ht < 1) {
        coefficient <- NULL
        if (!isTRUE(random_color)) {
          if (!is.null(coefficients)) {
            coefficient <- coefficients[[i]]
            color <- colors[[round(coefficient * (length(colors) - 1)) + 1]]
          } else {
            color <- colors[[i]]
          }
        } else {
         color <- colors[sample(seq_along(colors), 1)]
        }
        graphics::text(x1, y1, word, cex = size[[i]],
                       offset = 0, srt = rotWord * 90, col = color)
        boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, ht)
        plot_record[[length(plot_record) + 1]] <- list(word = word,
                                                       coefficient = coefficient,
                                                       color = color,
                                                       frequency = freq[[i]],
                                                       size = size[[i]])
        isOverlaped <- FALSE
      } else {
        if(r > sqrt(0.5)) {
          warning(word, "could not be fit on page. It will not be plotted.")
          isOverlaped <- FALSE
        }
        theta <- theta + thetaStep
        r <- r + rStep * thetaStep / (2 * pi)
        x1 <- 0.5 + r * cos(theta)
        y1 <- 0.5 + r * sin(theta)
      }
    }
  }
  graphics::par(mar = op, bg = bg_color)
  invisible(list(status = TRUE,
                 record = plot_record))
}
