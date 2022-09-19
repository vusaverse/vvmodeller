#' Adjust_Tableau_size
#'
#' Adjust the size according to the 'stijlgids'in the selected tableau file/files.
#'
#' @param import_files selected tableau file or folder, to change its size. If
#'  it is only one file, make sure it is a twb file.
#'
#' @return tableau file with the correct size.
#' @export
#'
Adjust_Tableau_size <- function(import_files) {
  if (tools::file_ext(import_files) != "twb") {
    import_files = list.files(import_files, full.names = TRUE)
  }
  for (file in import_files) {
    data <- XML::xmlParse(file = file)

    ## Extract de root node.
    rootnode <- XML::xmlRoot(data)

    ## find the size part
    size_part <- rootnode[["dashboards"]][[1]][["size"]]

    ## adjust the size part
    XML::xmlAttrs(size_part) <- c(maxheight = 800, maxwidth = 1300, minheight = 800, minwidth = 1300)

    ## save adjusments
    XML::saveXML(data, file=file)
  }
}
