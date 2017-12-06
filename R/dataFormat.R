#' Format data for plots
#'
#' @param x data frame to format
#' @param categories list of grouping variables
#' @param measurevar dependent variable we are calculating values for
#' @param subject name of subject id
#' @param time grouping variable for time (ie. isi)
#' @param type grouping variable for stimulus type
#' @param group grouping variable for experimental group
#' @return correctly formatted data frame with means for each subject in each category
#' @examples
#'    dataFormat(x,measurevar="residuals",categories=c(subject="sub",time="isi",type="type",group="music"))
#' @export

dataFormat <- function(x,
                       measurevar="residuals",
                       categories=c(subject="sub",
                       time="isi",
                       type="type",
                       group="music")){

  mydata<- aggregate(as.formula(paste(measurevar, paste(categories, collapse="+"), sep="~")), data=x, FUN= "mean")
  names(mydata)=c("subject","time","type","group","measurevar")
  return(mydata)
}

#mydata=dataFormat(x)

