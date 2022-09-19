############################################### #############################
### SA_Exploration functions.R
############################################### #############################
### R code for Student Analytics VU University Amsterdam
### Copyright 2018 VU
### Web Page: http://www.vu.nl
### Contact: Theo Bakker (t.c.bakker@vu.nl)
###
### Filename: SA_Exploration functions.R
### Purpose: A series of (wrapper) functions for Exploring data
###
### Dependencies: None
###
### Datasets used: Datasets
###
### Comments:
### 1) Comments.
### 2) ___
###
############################################### #############################
### History:
### 07/13/2017: TB: File Creation
### 2017-07-14: TB: Line2user added and sa_drop_vif
############################################### #############################


#' SA drop vif
#'
#' Function to drop variable based on their VIF value.
#'
#' @param df The data frame
#' @param threshold The threshold, all variables with a VIF value above
#' this limit will be dropped.
#'
#' @return Data frame without correlated variables.
#' @export
sa_drop_vif <- function(df, threshold = 2.5){
  vif <- NULL
  drop <- TRUE
  aftervif <- data.frame()
  while(drop == TRUE) {
    vfit <- vif(df)
    aftervif <- dplyr::bind_rows(aftervif,
                                 as.data.frame(t(vfit))
    )
    if(max(vfit) > threshold) {
      df <- stats::update(df,
                          stats::as.formula(paste(".","~",".","-",names(which.max(vfit)))))
    }
    else {
      drop = FALSE
    }
  }
  return(df)
}

############################################### #############################
## sa_line2user
############################################### #############################
## Function to create a title
## See https://stackoverflow.com/questions/29125019/get-margin-line-locations-mgp-in-user-coordinates/29893376#29893376
## and https://stackoverflow.com/questions/14660372/common-main-title-of-a-figure-panel-compiled-with-parmfrow

## IN: line, side
## OUT: title
#' SA line2user
#'
#' Function to create a title.
#'
#' @param line Line
#' @param side A value of 1, 2, 3, or 4.
#'
#'@export
sa_line2user <- function(line, side) {
  ## Check if graphics is installed
  vvmover::check_installed_package("graphics")
  lh <- graphics::par('cin')[2] * graphics::par('cex') * graphics::par('lheight')
  x_off <- diff(graphics::grconvertX(0:1, 'inches', 'user'))
  y_off <- diff(graphics::grconvertY(0:1, 'inches', 'user'))
  switch(side,
         `1` = graphics::par('usr')[3] - line * y_off * lh,
         `2` = graphics::par('usr')[1] - line * x_off * lh,
         `3` = graphics::par('usr')[4] + line * y_off * lh,
         `4` = graphics::par('usr')[2] + line * x_off * lh,
         stop("side must be 1, 2, 3, or 4", call.=FALSE))
}