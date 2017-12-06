#' Error bar calculations
#'
#' @param mydata the data frame we will calculate error bars for
#' @param measurevar the dependent variable we are calculating error bars for
#' @param groupvars grouping variables for error bars
#' @param time time variable (ie. isi, interval)
#' @param type type variable (ie. stimulus category)
#' @param group group variable (ie. condition)
#' @return N, mean, standard deviation, standard error, 95% confidence interval
#' @import
#'    plyr
#' @examples
#'    errorBars(mydata="mydata", measurevar="measurevar", groupvars=c(time="time",type="type",group="group"))
#' @export

errorBars <- function(mydata="mydata", measurevar="measurevar", groupvars=c(time="time",type="type",group="group"), na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (yy, na.rm=FALSE) {
    if (na.rm) sum(!is.na(yy))
    else       length(yy)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(mydata, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar)

  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)}

#plotdata=errorBars(mydata)
