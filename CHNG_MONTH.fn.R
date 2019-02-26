# Functions that change the month of the date
# Name   : CHNG_MONTH.fn
# Date   : 2019-02-26
# Author : Jeenee, Lee (@AgileSoDA)
# ver    : v1.0


# 1. With basic packages

CHNG_MONTH.fn <- function(date, format = '%Y-%m-%d', mm = 0) {
  # date : the date of the change
  # format : date`s format
  # mm : the month of movement
  
  basis_date <- as.POSIXlt(date, format = format)
  
  date <- basis_date
  date$mday <- 1
  date$mon  <- date$mon + mm
  
  date$mday <- basis_date$mday
  
  return(date)
  
}

# Test
CHNG_MONTH.fn(date = '20190226', format = '%Y%m%d', mm = 3)

# Think Out it : expect 2019-02-29 but...? Why?
CHNG_MONTH.fn(date = '20181129', format = '%Y%m%d', mm = 3)


# 2. With lubridate package

CHNG_MONTH.fn <- function(date, format = '%Y-%m-%d', mm = 0) {
  
  require(lubridate)
  date <- as.Date(date, format = format)
  
  date_day <- day(date)
  day(date) <- 1
  month(date) <- month(date) + mm
  day(date) <- date_day
  
  return(format(date, format = format))
  
}


# Test
CHNG_MONTH.fn(date = '20190226', format = '%Y%m%d', mm = 3)


# We want change months and find SPECIFIC date
# 3-1. Find specific date with basis packages
CHNG_MONTH.fn <- function(date, format = '%Y-%m-%d', mm = 0, type = 'same') {
  # type : 'first'('f') or 'same'('s') or 'last'('l') or date with character (1-31)
  
  if (!type %in% c('first', 'same', 'last', 'f', 's', 'l', as.character(1:31))) {
    cat('Check Type!')
    stop()
  }
  
  basis_date <- as.POSIXlt(date, format = format)
  
  date <- basis_date
  date$mday <- 1
  date$mon  <- date$mon + mm
  
  if (type %in% c('same', 's')) date$mday <- basis_date$mday
  if (type %in% c('last', 'l')) {
    date$mon  <- date$mon + 1
    date$mday <- date$mday - 1
  }
  if (type %in% as.character(1:31)) date$mday <- type
  
  return(format(date, format = format))
}

# Same result with #1
CHNG_MONTH.fn(date = '20190226', format = '%Y%m%d', mm = 3, type = 'same')
CHNG_MONTH.fn(date = '20190226', format = '%Y%m%d', mm = 3, type = 'first')
CHNG_MONTH.fn(date = '20190226', format = '%Y%m%d', mm = 3, type = 'last')
CHNG_MONTH.fn(date = '20190226', format = '%Y%m%d', mm = 3, type = '5')


# 3-2. Find specific date with lubridate packages
CHNG_MONTH.fn <- function(date, format = '%Y-%m-%d', mm = 0, type = 'same') {
  # type : 'first'('f') or 'same'('s') or 'last'('l') or date with character (1-31)
  
  if (!type %in% c('first', 'same', 'last', 'f', 's', 'l')) {
    cat('Check Type!')
    stop()
  }
  
  basis_date <- as.Date(date, format = format)
  
  date_day <- day(date)
  day(date) <- 1
  month(date) <- month(date) + mm
  
  if (type %in% c('same', 's')) day(date) <- date_day
  if (type %in% c('last', 'l')) {
    month(date)  <- month(date) + 1
    day(date) <- day(date) - 1
  }
  if (type %in% as.character(1:31)) day(date) <- type
  
  return(format(date, format = format))
}

# Same result with #2
CHNG_MONTH.fn(date = '20190226', format = '%Y%m%d', mm = 3, type = 'same')
CHNG_MONTH.fn(date = '20190226', format = '%Y%m%d', mm = 3, type = 'first')
CHNG_MONTH.fn(date = '20190226', format = '%Y%m%d', mm = 3, type = 'last')
CHNG_MONTH.fn(date = '20190226', format = '%Y%m%d', mm = 3, type = '5')
