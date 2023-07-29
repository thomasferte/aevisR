#' RDfunct
#'
#' @description calcul du RD (pris de fmsb::riskdifference)
#'
#' @param a The number of disease occurence among exposed cohort.
#' @param b The number of disease occurence among non-exposed cohort.
#' @param N1 The population at risk of the exposed cohort.
#' @param N0 The population at risk of the unexposed cohort.
#' @param CRC Logical. If TRUE, calculate confidence intervals for each risk. Default is FALSE.
#' @param conf.level Probability for confidence intervals. Default is 0.95.
#'
#' @return A list with estimate, conf.int and p.value
#' @export
#'
RDfunct <- function (a, b, N1, N0, CRC = FALSE, conf.level = 0.95) {
  .M <- a + b
  .T <- N1 + N0
  .R1 <- a/N1
  .R0 <- b/N0
  .RT <- .M/.T
  norm.pp <- qnorm(1 - (1 - conf.level)/2)
  if (CRC) {
    .RC1 <- norm.pp * sqrt(a * (N1 - a)/(N1^3))
    .RC0 <- norm.pp * sqrt(b * (N0 - b)/(N0^3))
    .RCT <- norm.pp * sqrt(.M * (.T - .M)/(.T^3))
    .MAT <- matrix(c(a, b, .M, N1, N0, .T, .R1, .R0, .RT,
                     .R1 - .RC1, .R0 - .RC0, .RT - .RCT, .R1 + .RC1,
                     .R0 + .RC0, .RT + .RCT), 3, 5)
    colnames(.MAT) <- c("Cases", "People at Risk", "Risk",
                        "Lower CL", "Upper CL")
    rownames(.MAT) <- c("Exposed", "Unexposed", "Total")
  } else {
    .MAT <- matrix(c(a, b, .M, N1, N0, .T, .R1, .R0, .RT),
                   3, 3)
    colnames(.MAT) <- c("Cases", "People at risk", "Risk")
    rownames(.MAT) <- c("Exposed", "Unexposed", "Total")
  }
  class(.MAT) <- "table"
  ################ print(.MAT) ####################
  ESTIMATE <- .R1 - .R0
  .CHI <- ESTIMATE/sqrt(a * (N1 - a)/(N1^3) + b * (N0 - b)/(N0^3))
  p.v <- 2 * (1 - pnorm(abs(.CHI)))
  RDL <- ESTIMATE - norm.pp * sqrt(a * (N1 - a)/(N1^3) + b *
                                     (N0 - b)/(N0^3))
  RDU <- ESTIMATE + norm.pp * sqrt(a * (N1 - a)/(N1^3) + b *
                                     (N0 - b)/(N0^3))
  CINT <- c(RDL, RDU)
  attr(CINT, "conf.level") <- conf.level
  RVAL <- list(p.value = p.v, conf.int = CINT, estimate = ESTIMATE,
               method = "Risk difference and its significance probability (H0: The difference equals to zero)",
               data.name = paste(deparse(substitute(a)), deparse(substitute(b)),
                                 deparse(substitute(N1)), deparse(substitute(N0))))
  class(RVAL) <- "htest"
  return(RVAL)
}
