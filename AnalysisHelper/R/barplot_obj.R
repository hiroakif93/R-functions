#' @useDynLib AnalysisHelper, .registration=TRUE
#' @importFrom dplyr arrange bind_rows
#' @importFrom dplyr bind_rows
#' @param mat x is a data.frame of community matrix.
#' @export
#'
barplot_obj = function(x, color = palettes(35, "rainbow"), othersCol = "grey30", specificName = NULL,
                       specificColor = "grey90", sortFun = sum, na.rm = TRUE){


  ## Ranking by your definition (default sum)
  total.abundance <- apply(x, 2, sortFun, na.rm = na.rm)
  ranking_table <-
    data.frame(
      taxa = colnames(x),
      total.abundance = total.abundance,
      stringsAsFactors = FALSE
    )|>
    dplyr::arrange(-total.abundance)
  ranking_table$color = NA
  ## Assigned Specific taxa

  if(!is.null(specificName)){

    add_taxa = data.frame(taxa=specificName, color=specificColor)
    ranking_table =
      ranking_table |>
      bind_rows(add_taxa)

  }

  ## Assinged Others
  if(nrow(ranking_table) > length(color)) {
    add_taxa = data.frame(taxa="Others", color=othersCol)

    rows=c(1:length(color), which(ranking_table$taxa==specificName))
    ranking_table = ranking_table[rows, ] |>
      bind_rows(add_taxa)
    ranking_table$color[1:length(color)] <- color

  }else{
    non_assigned_col_row = which(is.na(ranking_table$color))
    ranking_table$color[non_assigned_col_row] <- color[1:length(non_assigned_col_row)]
  }

  ## Definition color of each taxa

  non_dominant = which(!colnames(x) %in% ranking_table$taxa)

  res = x
  if(length(non_dominant) == 1) res = cbind(x[,-non_dominant], Others=x[,non_dominant])
  if( length(non_dominant) > 1 ){
    res = cbind(x[,-non_dominant], Others=rowSums(x[,non_dominant]))
  }

  col =ranking_table$color
  names(col) = ranking_table$taxa

  return(list(as.data.frame(res), col))
}
