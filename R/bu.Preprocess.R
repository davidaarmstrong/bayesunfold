bu.Preprocess <-
function(input, ...){
T <- input
T <- as.matrix(T)
T[T < 0 | T > 100] <- NA
#
nstimuli <- ncol(T)
#
#vote.turnout <- ANES1968[,13]
#presidential.vote <- ANES1968[,14]
#
#  Delete rows (respondents) with less than (nstimuli - 7) thermometer ratings
#
cutoff <- nstimuli - 7
keep <- which(rowSums(!is.na(T))>=cutoff)
#presidential.vote <- presidential.vote[keep]
T <- T[keep,]
T <- (100-T)/50
#T <- (100-T)/50 + 0.02
#
nrowX <- nrow(T)
ncolX <- ncol(T)
nburn <- 2000
nslice <- 1000
NS <- 2
N <- NS*(nrowX+ncolX) - ((NS*(NS+1))/2)
NDIM <- NS*(nrowX+ncolX) - (NS-1)
UNFOLD <- 1
#
#  ROWS MUST HAVE LESS THAN 7 MISSING ENTRIES
#  COLUMNS MUST HAVE AT LEAST 1/4 NON-MISSING ENTRIES (THIS IS HARD WIRED IN THE C CODE)
#
NMISSING <- 7
#
TT <- T
TT[is.na(TT)] <- -999.0
X <- as.vector(t(TT))
#
#  CONSTRAINTS
#
CONSTRAINTS <- rep(1,NS*(nrowX+ncolX))
#
if (NS==1){
  CONSTRAINTS[NS*(nrowX+ncolX)] <- 0
}
if (NS==2){
  CONSTRAINTS[(NS*(nrowX+ncolX)-NS):(NS*(nrowX+ncolX))] <- 0
}
if (NS==3){
  CONSTRAINTS[(NS*(nrowX+ncolX)-4):(NS*(nrowX+ncolX))] <- 0
  CONSTRAINTS[(NS*(nrowX+ncolX)-6)] <- 0
}
#
#  SMACOF (METRIC UNFOLDING) FOR COMPARISON PURPOSES
#
weights <- matrix(1, nrow=nrowX, ncol=ncolX)
weights[is.na(T)] <- 0
#
SMACOF.result <- smacofRect(T, ndim=NS, circle = "none", weightmat=weights, itmax=10000)

zmetric <- as.numeric(t(SMACOF.result$conf.col))
xmetric <- as.numeric(t(SMACOF.result$conf.row))
rmatrix <- c(zmetric,xmetric)
rmatrix[(NS*(nrowX+ncolX)-NS):(NS*(nrowX+ncolX))] <- 0
yrotate <- rep(0,(NS*(nrowX+ncolX)))
#
#  SMACOF PLOT
#
zz <- SMACOF.result$conf.col
xx <- SMACOF.result$conf.row
invisible(list(zmetric=zmetric, xmetric=xmetric, rmatrix=rmatrix, yrotate=yrotate, X=X, CONSTRAINTS=CONSTRAINTS, data=T, smacof.result=SMACOF.result))
}
