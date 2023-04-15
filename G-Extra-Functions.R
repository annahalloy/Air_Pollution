
Percentile <- function(perc) function(x) 
  ## `perc` is the percentile which should be computed for the numeric vector `x`
  quantile(x, perc*1e-2, na.rm=TRUE)

ReadMet <- function(filename) {
  data <- read.table(filename, skip=15, col.names=c("year", "month", "day", "hour", "minute", "WIRI", "WIGE"))
  data %>%
    mutate(datetime = as.chron(paste(year, month, day, hour, minute), "%Y %m %d %H %M"),
           year = years(datetime),
           month = months(datetime),
           day = days(datetime),
           hour = hours(datetime),
           minute = minutes(datetime),
           WIRI = ifelse(WIRI <= -9999, NA, WIRI),
           WIGE = ifelse(WIGE <= -9999, NA, WIGE))
}

mean.angle <- function(theta, r=1, ...) {
  ## Function for averaging angles
  ## Polar coordinates -> Cartesian coordinates -> polar coordinates
  ## 'theta' is in degrees
  ## 'r=1' for unit circle
  ## returns value is mean theta in degrees
  theta.rad <- theta * pi/180
  x <- mean(r * cos(theta.rad), ...)
  y <- mean(r * sin(theta.rad), ...)
  theta.deg <- atan2(y, x) * 180/pi
  ifelse(sign(theta.deg) < 0, (theta.deg + 360) %% 360, theta.deg) # -179--180 to 0--359
}