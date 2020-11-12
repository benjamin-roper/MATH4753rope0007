#' myci
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
myci<-function(x){
  mp <-c(-1,1)
  t<-qt(1-0.05/2, 25-1)
  mean(x)+mp*t*sd(x)/sqrt(25)
}
