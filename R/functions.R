#' @export
annual_return <- function(x, scale = 250) {
  prod(1 + x) ^ (scale / length(x)) - 1
}

#' @export
max_drawdown <- function(x, na.rm = FALSE) {
  dx <- cumprod(1 + x)
  dd <- dx / cummax(dx)
  list(depth = 1 - min(dd, na.rm = na.rm),
    trough = trough <- which.min(dd),
    from = from <- which.max(cumsum(dd[1:trough] == 1)) + 1L,
    to = to <- trough + which(dd[trough:length(dd)] == 1)[1] - 1L,
    length = to - from + 1L,
    peaktotrough = trough - from + 1L,
    recovery = to - trough)
}

#' @export
max_drawdown_depth <- function(x, na.rm = FALSE) {
  dx <- cumprod(1 + x)
  dd <- dx / cummax(dx)
  1 - min(dd, na.rm = na.rm)
}

#' @export
longest_drawdown <- function(x, na.rm = FALSE) {
  dx <- cumprod(1 + x)
  dd <- dx / cummax(dx)
  di <- c(1L, which(dd == 1), length(dd))
  did <- diff(di)
  dim <- max(did, na.rm = na.rm)
  # if multiple entries in dim is maximal, take the first
  # alternative is to take the deepest
  dii <- which.max(did)
  dbegin <- di[dii]
  dend <- di[dii + 1L]
  ddx <- dd[dbegin:dend]
  list(depth = 1 - min(ddx, na.rm = na.rm),
    trough = trough <- dbegin + which.min(ddx),
    from = dbegin, to = dend,
    length = dim,
    peaktotrough = trough - dbegin + 1L,
    recovery = if (dd[dend] == 1) dend - trough else NA_integer_)
}

#' @export
average_drawdown <- function(x, p = 2, ...) {
  dx <- cumprod(1 + x)
  dd <- dx / cummax(dx)
  mean((1 - dd) ^ p, ...) ^ (1 / p)
}

#' @export
quantile_drawdown <- function(x, ..., names = FALSE) {
  dx <- cumprod(1 + x)
  dd <- dx / cummax(dx)
  quantile(1 - dd, ..., names = names)
}

#' @export
median_drawdown <- function(x, ...) {
  dx <- cumprod(1 + x)
  dd <- dx / cummax(dx)
  median(1 - dd, ...)
}

#' @export
sharpe_ratio <- function(x, scale = 250, na.rm = TRUE) {
  annual_return(x, scale = scale) / (sqrt(scale) * sd(x, na.rm = na.rm))
}
