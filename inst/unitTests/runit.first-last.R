# need tests for: vector, matrix, list, data.frame

# basic functionality on data.frame
test.first_data.frame_xtsible <- function() {
  x1 <- data.frame(x = 1:2, row.names = c("2017-01-01", "2017-01-02"))
  checkIdentical(first(x1), head(x1, 1))
  x2 <- data.frame(x1, y = 2:1)
  checkIdentical(first(x2), head(x2, 1))
}
test.last_data.frame_xtsible <- function() {
  x1 <- data.frame(x = 1:2, row.names = c("2017-01-01", "2017-01-02"))
  checkIdentical(last(x1), head(x1, 1))
  x2 <- data.frame(x1, y = 2:1)
  checkIdentical(last(x2), head(x2, 1))
}
