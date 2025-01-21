## ----date2week----------------------------------------------------------------
library("aweek")

set_week_start("Sunday") # setting the default week_start to Sunday

set.seed(2019-03-03)
dat <- as.Date("2019-03-03") + sample(-6:7, 10, replace = TRUE)
dat
print(w <- date2week(dat))

## ----date2week_week_start-----------------------------------------------------
# Use character days
date2week(dat, week_start = "Monday")
# Use ISO 8601 days
date2week(dat, week_start = 1)

## ----as.aweek.Date------------------------------------------------------------
as.aweek(dat, week_start = 1)

## ----date2week2date-----------------------------------------------------------
week2date(w)
as.Date(w)

## ----example_day--------------------------------------------------------------
the_date <- as.Date("2016-12-06")
jan_1    <- as.Date("2016-01-01")

i <- as.POSIXlt(the_date)$wday # 2, the ISO date for Tuesday 
s <- 7L                        # week_start for sunday

# 1. Find the day of the week
print(d <- 1L + ((i + (7L - s)) %% 7L))

# 2. Find the date that represents midweek
print(m <- the_date + (4L - d))

# 3. Find the week
print(w <- 1L + as.integer(m - jan_1) %/% 7L)

# Format the week
sprintf("2016-W%02d-%d", w, d)

## ----as.aweek.character-------------------------------------------------------
x <- as.aweek("2019-W10-1")
x

## ----ascharacter--------------------------------------------------------------
as.character(x)

## ----date2week_floor----------------------------------------------------------
print(wf <- date2week(dat, week_start = "Saturday", floor_day = TRUE))
table(wf)

## ----date2week_floor2date-----------------------------------------------------
print(dwf <- week2date(wf))
weekdays(dwf)

## ----trunc--------------------------------------------------------------------
w <- date2week(dat)
w
trunc(w)

## ----factors------------------------------------------------------------------
dat[1] + c(0, 15)
date2week(dat[1] + c(0, 15), week_start = 1, factor = TRUE)

## ----factor_aweek-------------------------------------------------------------
factor_aweek(w)

## ----week2week_wednesday------------------------------------------------------
w # week starting on Sunday
ww <- change_week_start(w, week_start = "wednesday") # same dates, starting on Wednesday
ww
identical(as.Date(w), as.Date(ww))

## ----week2week, R.options=list(width = 100)-------------------------------------------------------
# create a table with all days in the week
d   <- as.Date("2019-03-03") + 0:6
res <- lapply(weekdays(d), function(i) date2week(d, week_start = i))
names(res) <- weekdays(d)
data.frame(res)

## ----week2week2date, R.options=list(width = 100)--------------------------------------------------
data.frame(lapply(res, as.Date))

## ----caweekdate---------------------------------------------------------------
c(as.aweek("2010-W10-1"), 
  res$Sunday, 
  "2010-W12-1", 
  as.Date(res$Monday[1]) + 14)

## ----cweek2week_err, error = TRUE---------------------------------------------
c(res$Sunday[1], res$Wednesday[2], res$Friday[3])

## ----cweekweek----------------------------------------------------------------
wed <- change_week_start(res$Wednesday, get_week_start())
fri <- change_week_start(res$Friday, get_week_start())
c(res$Sunday[1], wed[2], fri[3])

## ----add_dates----------------------------------------------------------------
c(res$Monday, as.Date("2019-04-03"))

## ----add_chars----------------------------------------------------------------
s <- c(res$Saturday, "2019-W14-3")
s
m <- c(res$Monday, "2019-W14-3")
m

## ----char2date----------------------------------------------------------------
as.Date(s[7:8])
as.Date(m[7:8])

## ----date_a_frame-------------------------------------------------------------
# create a table with all days in the week
d    <- as.Date("2019-03-03") + 0:6
res  <- lapply(weekdays(d), function(i) date2week(d, week_start = i))
resn <- lapply(weekdays(d), function(i) date2week(d, week_start = i, numeric = TRUE))
datf <- data.frame(wday = rep(weekdays(d), each = 7), 
                   week = unlist(res), # note: unlist converts to character
                   week_number = unlist(resn),
                   year  = 2019,
                   stringsAsFactors = FALSE)
datf$day <- substring(datf$week, 10, 11)
head(datf, 10)

## ----get_aweek----------------------------------------------------------------
datf$aweek <- with(datf, get_aweek(week = week_number, year = year, day = day, start = wday))
datf$date  <- with(datf, get_date(week = week_number, year = year, day = day, start = wday))
head(datf, 10)

## -----------------------------------------------------------------------------
get_aweek(11, 2019)
get_date(11, 2019)

## ----remove_things, include = FALSE-------------------------------------------
datf$aweek <- NULL
datf$date  <- NULL

## ----date_a_frame_2-----------------------------------------------------------
datf$aweek <- with(datf, as.aweek(week, start = wday))
head(datf, 10)
str(datf)

## ----tabluate_data_frame, R.options = list(width = 100)-------------------------------------------
print(with(datf, table(before = week, after = aweek)), zero.print = ".")

## ----week2date----------------------------------------------------------------
week2date("2019-W10-1", week_start = "Sunday") # 2019-03-03
week2date("2019-W10-1", week_start = "Monday") # 2019-03-04

## ----week2date_aweek----------------------------------------------------------
set_week_start("Monday") # Set the default week_start to ISO week
get_week_start(w)        # show the default week_start for w
week2date(w)
identical(week2date(w), dat)               # TRUE
identical(week2date(as.character(w)), dat) # FALSE

## ----asdate-------------------------------------------------------------------
as.Date(w)
as.POSIXlt(w)

