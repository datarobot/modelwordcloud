#' Make a word cloud.
#'
#' @param words character. A vector of words to plot.
#' @param freq numeric. The frequency of those words.
#' @param coefficients numeric. If provided, colors will be assigned according to coefficients.
#' @param colors character. The colors to use for plotting.
#' @param scale numeric. The range of sizes.
#' @param min.freq numeric. Words with less frequency than this will not be plotted.
#' @param max.words numeric. Don't plot more words than this amount.
#' @param random.order logical. Should words be plotted in a random order or by frequency (default FALSE)?
#' @param random.color logical. Allocate words a color by random? (default FALSE).
#' @param rot.per numeric. Amount of rotation to apply to each word, between 0 and 1. Defaults to 0 (no rotation).
#' @param bg_color character. The color of the background.
#' @export
wordcloud <- function(words, freq, coefficients = NULL, colors = "black",
                      scale = c(4, .5), min.freq = 3, max.words = Inf,
                      random.order = FALSE, random.color = FALSE, rot.per = 0,
                      ordered.colors = FALSE, bg_color = "#FFFFFF") { 
  tails <- "g|j|p|q|y"
  last <- 1
  nc <- length(colors)
  if (ordered.colors && (length(colors) != 1 && length(colors) != length(words))) {
    stop("Length of colors does not match length of words vector.")
  }
  if(min.freq > max(freq)) { min.freq <- 0 }

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
        overlap <- x1 + sw1 > x2-s
      } else {
        overlap <- x2 + sw2 > x1-s
      }
      if (y1 < y2) {
        overlap <- overlap && (y1 + sh1 > y2-s)
      } else {
        overlap <- overlap && (y2 + sh2 > y1-s)
      }
      if(overlap) {
        last <<- i
        return(TRUE)
      }
    }
    FALSE
  }
  
  ord <- rank(-freq, ties.method = "random")
  words <- words[ord <= max.words]
  freq <- freq[ord <= max.words]
  if (ordered.colors) {
      colors <- colors[ord <= max.words]
  }
  
  if(random.order) {
    ord <- sample.int(length(words))
  } else {
    ord <- order(freq,decreasing = TRUE)
  }

  words <- words[ord]
  words <- words[freq >= min.freq]
  freq <- freq[ord]

  if (!is.null(coefficients)) {
    coefficients <- coefficients[ord]
    coefficients <- coefficients[freq >= min.freq]
    # Scale coefficients between 0 and 1
    coefficients <- (coefficients - min(coefficients)) / (max(coefficients) - min(coefficients))
  }

  freq <- freq[freq >= min.freq]
  if (ordered.colors) {
      colors <- colors[ord][freq >= min.freq]
  }
  
  thetaStep <- .1
  rStep <- .05
  plot.new()
  op <- par("mar")
  par(mar = c(0, 0, 0, 0))
  plot.window(c(0,1), c(0,1), asp = 1)
  normedFreq <- freq / max(freq)
  size <- (scale[1] - scale[2]) * normedFreq + scale[2]
  boxes <- list()
  
  for(i in seq_along(words)) {
    rotWord <- runif(1) < rot.per
    r <-0
    theta <- runif(1, 0, 2 * pi)
    x1<-.5
    y1<-.5
    wid <- strwidth(words[i], cex = size[i])
    ht <- strheight(words[i], cex = size[i])
    #mind your ps and qs
    if (grepl(tails, words[i])) { ht <- ht + ht*.2 }
    if (rotWord) {
      tmp <- ht
      ht <- wid
      wid <- tmp  
    }
    isOverlaped <- TRUE
    while(isOverlaped) {
      if (!overlap(x1 - .5 * wid, y1 - .5 * ht, wid, ht) &&
          x1 - .5 * wid > 0 && y1 - .5 * ht > 0 &&
          x1 + .5 * wid < 1 && y1 + .5 * ht < 1) {
        if (!random.color) {
          if (!is.null(coefficients)) {
            cc <- colors[[round(coefficients[[i]] * (length(colors) - 1)) + 1]]
          } else if (ordered.colors) {
            cc <- colors[i]
          } else {
            cc <- ceiling(nc*normedFreq[i])
            cc <- colors[cc]
          }
        } else {
         cc <- colors[sample(1:nc,1)]
        }
        text(x1, y1, words[i], cex = size[i], offset = 0, srt = rotWord * 90, col = cc)
        boxes[[length(boxes) + 1]] <- c(x1 - .5 * wid, y1 - .5 * ht, wid, ht)
        isOverlaped <- FALSE
      } else {
        if(r > sqrt(.5)) {
          warning(words[i], "could not be fit on page. It will not be plotted.")
          isOverlaped <- FALSE
        }
        theta <- theta + thetaStep
        r <- r + rStep * thetaStep / (2*pi)
        x1 <- .5 + r * cos(theta)
        y1 <- .5 + r * sin(theta)
      }
    }
  }
  par(mar = op, bg = bg_color)
  invisible()
}
