#' @title Generates lift and cumulative gain performance table and plot
#' @description It retrieves the cumulative positive rate -gain curve- and the lift chart & plot when score is divided in 5, 10 or 20 segments. Both metrics give a quality measure about how well the model predicts. Higher values at the beginning of the population implies a better model.
#' @param data input data source
#' @param str_score the variable which contains the score number, or likelihood of being positive class
#' @param str_target target binary variable indicating class label
#' @param q_segments quantity of segments to split str_score, valid values: 5, 10 or 20
#' @examples
#' \dontrun{fit_glm=glm(has_heart_disease ~ age + oldpeak, data=heart_disease, family = binomial)
#' heart_disease$score=predict(fit_glm, newdata=heart_disease, type='response')
#' gain_lift(data=heart_disease,str_score='score',str_target='has_heart_disease')}
#'
#' @return lift/gain table, column: gain implies how much positive cases are catched if the cut point to define the positive class is set to the column "Score Point"
#' @export
gain_lift <- function(data, str_score, str_target, q_segments)
{
  options(scipen = 999)
  # The negative score produces that the highest score are at the top
  # data=heart_disease; str_score='score'; str_target='has_heart_disease'; q_segments='5'
  data$neg_score=-data[, str_score]

  # Valid values for q_segments
  if(missing(q_segments))
    q_segments=10

  if(q_segments==20)
    seq_v=seq(from=0.05, to=0.95, by=0.05)

  if(q_segments==10 | !(q_segments %in% c(5,10,20)))
    seq_v=seq(from=0.1, to=0.9, by=0.1)

  if(q_segments==5)
    seq_v=seq(from=0.2, to=0.8, by=0.2)

  seq_v=c(seq_v, 1)

  quantile_cuts=quantile(data$neg_score, probs=seq_v)

  data[,str_target]=as.character(data[,str_target])

  grp=group_by(data, data[,str_target]) %>% summarise(q=n()) %>% arrange(q)

  less_representative_class=as.character(grp[1,1])

  lift_table=round(100*sapply(quantile_cuts, function(x) sum(data[data$neg_score<=x, str_target]==less_representative_class))/sum(data[, str_target]==less_representative_class),2)

  lift_res=rbind(lift_table,-quantile_cuts)
  rownames(lift_res)=c("Gain", "Score.Point")

  # likelihood of being less representative class (lrc)
  likelihood_lrc=grp[1,2]/(grp[2,2]+grp[1,2])

  ## Create table
  lift_res_t=data.frame(t(lift_res))
  lift_res_t$Population=as.numeric(100*seq_v)
  row.names(lift_res_t)=NULL
  lift_res_t=select(lift_res_t, Population, Gain, Score.Point)

  ## Generate lift variable
  lift_res_t$Lift=round(lift_res_t$Gain/100/seq_v,2)
  lift_res_t_gain=rbind(c(0, 0, NA, NA), lift_res_t)

  p_gain=
    ggplot(lift_res_t_gain, aes(Population, Gain, label=round(Gain,1), group=1)) + geom_line(stat="identity") +   geom_point(aes(colour=Gain) ) +
    theme_bw()  + ylab("Cumulative Gain (%)") + xlab("Population (%)")+
    theme(
      panel.grid.minor=element_blank(),
      legend.title=element_blank(),
      plot.title = element_text(vjust=2),
      axis.ticks.y=element_blank(),
      axis.text.y=element_blank(),
      panel.background = element_blank(),
      axis.title.x=element_text(margin=margin(15,0,0,0)),
      axis.title.y=element_text(margin=margin(0,15,0,0))
    )+geom_label(aes(fill = factor(Gain)), colour = "white", fontface = "bold",vjust = -.5, label.padding = unit(.2, "lines")) + ylim(0, 110)  +
    guides(fill=F) +  scale_colour_continuous(guide = FALSE)  +
    geom_segment(x = 0, y = 0, xend = 100, yend = 100,linetype="dotted") + scale_x_continuous(breaks = c(0, seq(10, 100, by=10)))


  p_lift=
    ggplot(lift_res_t, aes(Population, Lift, label=Lift, group=1)) + geom_line(stat="identity") + geom_point(aes(colour=Lift)) +
    theme_bw()  + ylab("Lift") + xlab("Population (%)")+
    theme(
      panel.grid.minor=element_blank(),
      legend.title=element_blank(),
      plot.title = element_text(vjust=2),
      axis.ticks.y=element_blank(),
      axis.text.y=element_blank(),
      axis.title.x=element_text(margin=margin(15,0,0,0)),
      axis.title.y=element_text(margin=margin(0,15,0,0))
    )+geom_label(aes(fill=-Lift), size=3.5, colour="white", vjust = -.5, label.padding = unit(.2, "lines")) + ylim(min(lift_res_t$Lift), max(lift_res_t$Lift*1.1)) +
    guides(fill=F) + scale_colour_continuous(guide = FALSE) + scale_x_continuous(breaks = c(0, seq(10, 100, by=10)))



  grid.arrange(p_gain, p_lift, ncol=2)

  lift_res_t=select(lift_res_t, Population, Gain, Lift, Score.Point)

  print(lift_res_t)
}
