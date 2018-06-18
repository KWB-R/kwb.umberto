#' Plot results
#'
#' @param grouped_data  data.frame with grouped rawdata as retrieved by 
#' function group_data()  
#' @param x_col column name to be used for plotting on x-axis (default: "model")
#' @param y_col column name to be used for plotting on y-axis (default: 
#' "quantity_sum") 
#' @param fill_col column name to be used for filling (default: "process") 
#' @import ggplot2 
#' @importFrom ggforce facet_wrap_paginate
#' @return simple plots for all differn
#' @export
#' @examples
#' zipfile <- system.file("extdata/Beispiel_Auswertung.zip", package = "kwb.lca")
#' temp <- file.path(tempdir(), "Beispiel_Auswertung")
#' unzip(zipfile, exdir = temp)
#' rawdata <- kwb.lca::import_rawdata(csv_dir = temp)
#' data_grouped <- kwb.lca::group_data(rawdata)
#' kwb.lca::plot_results(grouped_data = data_grouped)
  
plot_results <- function(grouped_data, 
                         x_col = "model",
                         y_col = "quantity_sum",
                         fill_col = "process") {
  
  
  grouped_data$label <-  sprintf("%s (%s)", 
                                 grouped_data$lci_method, 
                                 grouped_data$unit)
  
  
  for (i in seq_along(unique(grouped_data$lci_method))) {
    
    
    g1 <- ggplot2::ggplot(grouped_data, ggplot2::aes_string(x = x_col,
                                                            y = y_col,
                                                            fill = fill_col)) +
      ggforce::facet_wrap_paginate(~label,
                                   nrow = 1,
                                   ncol = 1,
                                   scales = "free_y",
                                   page = i) +
      ggplot2::geom_bar(position="stack", stat="identity") +  
      ggplot2::theme_bw(base_size = 10) +
      ggplot2::theme(legend.position = "top"
                     , strip.text.x = element_text(face = "bold")
                     , legend.title = element_blank()) +
      ggplot2::labs(x = "", y = "")
    
    print(g1) }
}
