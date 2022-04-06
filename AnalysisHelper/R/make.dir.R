#' Make directory to save
#' 
#' @description To tidy up your R directory.
#' 
#' @param file.name file.name is directory name.
#' 
#' @examples
#' make.dir('output)
#'
#' @export

make.dir <- function (file.name) 
{
    save.dir <- file.name
    save.rdata <- sprintf("%s/RData", save.dir)
    save.fig <- sprintf("%s/Figure", save.dir)
    save.rds <- sprintf("%s/RDS", save.dir)
    save.table <- sprintf("%s/Table", save.dir)
    dir.create(save.dir, showWarnings = FALSE)
    dir.create(save.rdata, showWarnings = FALSE)
    dir.create(save.fig, showWarnings = FALSE)
    dir.create(save.rds, showWarnings = FALSE)
    dir.create(save.table, showWarnings = FALSE)
    list(dir = save.dir, rdatadir = save.rdata, rdsdir = save.rds, 
         figdir = save.fig, tabledir = save.table)
}
