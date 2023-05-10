ReadTSeries <- function(filename, timecolumn="datetime", timeformat="%d.%m.%Y %H:%M") {
  ## read the table, strip units in column names, rename time column
  ## and change data type of time column from a string of characters to
  ## a numeric type so that we can perform operations on it
  data <- read.table(filename, skip=5, header=TRUE, sep=";", check.names=FALSE)
  names(data) <- sub("[ ].*$","",names(data)) # strip units for simplification
  names(data) <- sub("Date/time", timecolumn, names(data), fixed=TRUE)
  data[,timecolumn] <- as.chron(data[,timecolumn], timeformat) - 1/24 # end time -> start tim
  e
  ## extract additional variables from the time column
  data[,"year"] <- years(data[,timecolumn])
  data[,"month"] <- months(data[,timecolumn])
  data[,"day"] <- days(data[,timecolumn])
  data[,"hour"] <- hours(data[,timecolumn])
  data[,"dayofwk"] <- weekdays(data[,timecolumn])
  data[,"daytype"] <- ifelse(data[,"dayofwk"] %in% c("Sat","Sun"), "Weekend", "Weekday")
  data[,"season"] <- Month2Season(unclass(data[,"month"]))
  ## return value
  data
}

Month2Season <- function(month) {
  ## month is an integer (1-12) or a string ("Jan", "Feb", ...)
  ## a factor with levels {"DJF", "MAM", "JJA", "SON"} is returned
  seasons <- c("DJF", "MAM", "JJA", "SON")
  if (is.character(month)) {
    month <- match(month, month.abb)
  }
  month <- as.numeric(month)
  index <- findInterval(month %% 12, seq(0, 12, 3))
  factor(seasons[index], seasons)
}




Percentile <- function(perc) function(x) 
  ## `perc` is the percentile which should be computed for the numeric vector `x`
  quantile(x, perc*1e-2, na.rm=TRUE)

ComputeStats <- function(x) {
  x <- na.omit(x)
  m <- mean(x)
  s <- sd(x)
  n <- length(x)
  t <- qt(.975, n - 1)
  data.frame(mean=m,
             conf.lower=m-t*s/sqrt(n),
             conf.upper=m+t*s/sqrt(n),
             sd.lower=m-s,
             sd.upper=m+s)
}

TTest <- function(value, daytype) {
  ## if all values are missing,
  ## the t-test will raise an error
  wkend <- value[daytype=="Weekend"]
  wkday <- value[daytype=="Weekday"]
  if(length(na.omit(wkend)) > 0 & length(na.omit(wkday)) > 0) {
    out <- t.test(wkend, wkday, alternative="two.sided")
    pval <- out[["p.value"]]
  } else {
    pval <- NA
  }
  pval
}


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