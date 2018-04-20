setClass("Timespan")

setClass("Duration", contains = c("Timespan", "numeric"), validity = NULL) # check_duration)


setClass("Period", contains = c("Timespan", "numeric"),
  slots = c(year = "numeric", month = "numeric", day = "numeric",
    hour = "numeric", minute = "numeric"),
  prototype = prototype(year = 0, month = 0, day = 0, hour = 0, minute = 0),
  validity = NULL) #  check_period


setClass("Interval", contains = c("Timespan", "numeric"),
  slots = c(start = "POSIXct",   tzone = "character"), validity = NULL) # check_interval)


setClass("Interval", contains = c("Timespan", "numeric"),
  slots = c(start = "POSIXct",   tzone = "character"), validity = NULL) # check_interval)
