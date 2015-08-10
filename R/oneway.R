# This is an example function that conducts the Oneway-ANOVA test.
#' Oneway ANOVA table
#'
#' Takes a list as an input and return the Some of Squares Within and Between , Degrees of Freedom
#' Between and Within, Group names and thier factors, Means values of each group.
#' @param z A list that contains groups.
#' @return The list contains that ssb, ssw, dfw, dfb ...
#' @name oneway.default
#' @docType package
#' \item{coefficients}{ a named vector of coefficients }
#'
#' \item{vcov}{ covariance matrix of coefficients }
#'
#' \item{fitted.values}{ fitted values }
#'
#' \item{residuals}{ residuals }
#' mod1 <- linmod(Hwt~Bwt*Sex, cats)
#' mod1
#' summary(mod1)
#' @export
#' @describeIn linmod linmod.default(x, y, ...)
NULL


oneway.default <- function(z, ...) {
  g <- length(z)
  n <- length(unlist(z))
  namesGroups <- unique(stack(z)$ind)

  grand.mean <- mean(unlist(z))
  GroupMeanVec <- as.vector(sapply(z, mean))
  grp.var <- as.vector(sapply(z, var))
  grp.n <-as.vector(sapply(z, length))


  ssb <-sum(grp.n * (GroupMeanVec - grand.mean)^2)
  ssw <-sum((grp.n - 1) * grp.var)
  list(ssb=ssb, ssw=ssw, dfb=(g-1), dfw=(n-g),GroupMeanVec = GroupMeanVec,namesGroups=namesGroups)

}

