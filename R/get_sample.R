#' @title Sampling training and test data
#' @description Split input data into training and test set, retrieving always same sample by setting the seed.
#' @param data input data source
#' @param percentage_tr_rows percentage of training rows, range value from 0.1 to 0.99, default value=0.8 (80 percent of training data)
#' @param seed to generate the sample randomly, default value=987
#' @examples
#' \dontrun{## Training and test data. Percentage of training cases default value=80%.
#' index_sample=get_sample(data=heart_disease, percentage_tr_rows=0.8)
#' ## Generating the samples
#' data_tr=heart_disease[index_sample,]
#' data_ts=heart_disease[-index_sample,]}
#' @return TRUE/FALSE vector same length as 'data' param. TRUE represents that row position is for training data
#' @export
get_sample <- function(data, percentage_tr_rows=0.8, seed=987)
{
  set.seed(seed)
  tr=sample(nrow(data), round(percentage_tr_rows*nrow(data)))
  return(tr)
}
