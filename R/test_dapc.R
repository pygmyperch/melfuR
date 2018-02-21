#' Run test DAPC on rainbow.genind
#'
#' This function tests the package by running a simple DAPC on the example data
#' @param x A genind object
#' @keywords dapc
#' @export
#' @examples
#' data(rainbow.genind)
#' test_dapc(rainbow.genind)


test_dapc <- function(x){
  a <- dapc(x, pop=x$pop, n.pca = 30, n.da = 2)
  scatter(a)
}
