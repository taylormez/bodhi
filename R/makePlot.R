#' Make super cool plots
#'
#' @param plotData formatted data frame to use for plot
#' @param time time variable for x-axis
#' @param measurevar dependent variable for y-axis
#' @param type grouping variable for stimulus type (what lines are split by)
#' @param group grouping variable for experimental group (each on separate plot)
#' @param xaxis label for x-axis
#' @param yaxis label for y-axis
#' @param se standard error value for error bars
#' @param barWidth width of error bars (default .025)
#' @import
#'     ggplot2
#' @return awesome plot of our formatted data; includes separate lines for stimulus type and separate plots for experimental group
#' @examples
#'    makePlot(mydata="mydata",time="time",measurevar="measurevar",type="type",group="group",xaxis="interstimulus interval (sec)",yaxis="residual reaction time (ms)",se="sem")
#' @export




makePlot<-function(plotData="plotData",time="time",measurevar="measurevar",type="type",
                   group="group",xaxis="interstimulus interval (sec)",
                   yaxis="residual reaction time (ms)",se="se",barWidth=.03,
                   legtitle="Word Type",
                   legcolors = c("firebrick1","limegreen","royalblue1"),
                   leglevels=c("n","w","a"),
                   leglabels=c("non-word","non-associate","associate"),
                   plotorder=c("nomusic","slowmusic","fastmusic"),
                   plottitles=c("No Music","Slow Music","Fast Music")){
  plotData$group=factor(plotData$group,levels=c(plotorder[1],plotorder[2],plotorder[3]),labels=c(plottitles[1],plottitles[2],plottitles[3]))
  library(ggplot2)
  ggplot(plotData, aes(x=time, y=measurevar, colour=type)) +
  facet_wrap(~group)+
  geom_errorbar(aes(ymin=measurevar-se, ymax=measurevar+se),width=barWidth,size=.7) +
  geom_line(size=.7) +
  geom_point(size=1.5) +
  xlab(xaxis) +
  ylab(yaxis) +
  expand_limits() +                        # Expand y range
  scale_y_continuous() +
  scale_x_continuous()+# Set tick every 4
  theme_minimal()+
  scale_color_manual(name=legtitle,
                     values=c(legcolors[1],legcolors[2],legcolors[3]),
                     breaks=c(leglevels[1],leglevels[2],leglevels[3]),
                     labels=c(leglabels[1],leglabels[2],leglabels[3])) +
  theme(text = element_text(size=15),legend.background = element_rect(fill="white",size=0.3, linetype="solid", colour ="black"),
          panel.border = element_rect(colour = "black", fill=NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_line(size = .5),
          axis.ticks.y = element_line(size = .5))}

#myplot=makePlot(plotData)

