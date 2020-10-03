#' @Title normal curve for lab 7
#'
#' @param mu
#' @param sigma
#' @param a
#'
#' @return
#' @export
#'
#' @examples
myncurve=function(mu, sigma, a)
{
  curve(dnorm(x,mu,sigma),xlim=c(mu-3*sigma,mu+3*sigma))
  x=seq(mu-3*sigma,a,length=1000)
  y=dnorm(x,mu,sigma)
  polygon(c(mu-3*sigma,x,a),c(0,y,0),col="red")
  t=pnorm(a,mu,sigma)
  t
}
