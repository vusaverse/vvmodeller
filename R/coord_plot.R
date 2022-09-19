#' @title Coordinate plot
#' @description Calculate the means (or other function defined in 'group_func' parameter) per group to analyze how each segment behave. It scales each variable mean inti the 0 to 1 range to easily profile the groups according to its mean. It also calculate the mean regardless the grouping. This function is also useful when you want to profile cluster results in terms of its means.
#' @param data input data source
#' @param group_var variable to make the group by
#' @param group_func the data type of this parameter is a function, not an string, this is the function to be used in the group by, the default value is: mean
#' @param print_table False by default, if true it retrieves the mean table used to generate the plot.
#' @examples
#' \dontrun{
#' # calculating the differences based on function 'mean'
#' coord_plot(data=mtcars, group_var="cyl")
#' # printing the table used to generate the coord_plot
#' coord_plot(data=mtcars, group_var="cyl", print_table=TRUE)
#' # printing the table used to generate the coord_plot
#' coord_plot(data=mtcars, group_var="cyl", group_func=median, print_table=TRUE)
#' }
#' @return coordinate plot, if print_table=T it also prints a table with the average per column plus the average of the whole column
#' @export
coord_plot <- function(data, group_var, group_func=mean, print_table=FALSE)
{
  all_results_report=desc_groups(data = data, group_var = group_var, group_func = group_func)

  # excluding group_var column
  all_results=all_results_report[,2:ncol(all_results_report)]
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##  Group profiling. Extracting main characteristics from each one.
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ## Scale data to plot all in only one graph
  maxs=apply(all_results, 2, max)
  mins=apply(all_results, 2, min)
  cl_scaled=as.data.frame(scale(all_results, center = mins, scale = maxs - mins))

  ## Assign group number (label)
  cl_scaled[,group_var]=all_results_report[, group_var]

  ## This transform the data according to needed input of ggplot. The best way to understand this is to take a look at the data.
  melted_data=melt(cl_scaled, id.vars = group_var)

  colourCount = length(unique(cl_scaled[,group_var]))
  getPalette = suppressWarnings(colorRampPalette(brewer.pal(9, "Set2")))

  ## Coordinate plot
  co_plot=ggplot(melted_data, aes_string(x="variable", y="value",  group=group_var, color=group_var),  environment = environment()) +
    geom_path(alpha = 0.9) +
    geom_point() +
    xlab("Variables") +
    ylab("Scaled value") +
    ggtitle("Coordinate Plot") +
    theme_bw() +
    theme(axis.text.x=element_text(angle = 90, vjust = 0.5), plot.title=element_text(size=14,face="bold")) +
    scale_fill_manual(values = getPalette(colourCount))

  plot(co_plot)

  if(print_table)
  {
    return(all_results_report)
  }
}
