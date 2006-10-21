require(lhs)

graph2DaugmentLHS <- function(sims, extras, plotHist, plotDriver, filePath)
{
  A <- randomLHS(sims, 2)
  B <- augmentLHS(A, extras)

  if(plotDriver=="pdf"){
    pdf(file=paste(filePath, "//original", sims, ".pdf", sep=""),
               width=4, height=4)
  } else windows()
  plot.default(A[,1], A[,2], type="n", ylim=c(0,1),
    xlim=c(0,1), xlab="x1", ylab="x2", xaxs="i", yaxs="i",
    #main=paste("Original Design with", sims, "simulations and 2 parameters")
    )
  for(i in 1:length(A[,1]))
  {
    rect(floor(A[i,1]*sims)/sims, floor(A[i,2]*sims)/sims,
      ceiling(A[i,1]*sims)/sims, ceiling(A[i,2]*sims)/sims, col="grey")
  }
  points(A[,1], A[,2], pch=19, col="red")
  abline(v=(0:sims)/sims, h=(0:sims)/sims)
  if(plotDriver!="win") dev.off()

  if(plotDriver=="pdf"){
    pdf(file=paste(filePath, "//augmented", extras, ".pdf", sep=""),
        width=4, height=4)
  } else windows()
  plot.default(A[,1], A[,2], type="n", ylim=c(0,1),
    xlim=c(0,1), xlab="x1", ylab="x2", xaxs="i", yaxs="i",
    #main=paste("Augmented Design with", sims, "+", extras, "simulations and 2 parameters")
    )
  N <- sims + extras
  for(i in 1:length(B[,1]))
  {
    rect(floor(B[i,1]*N)/N, floor(B[i,2]*N)/N,
      ceiling(B[i,1]*N)/N, ceiling(B[i,2]*N)/N, col="grey")
  }
  points(A[,1], A[,2], pch=19, col="red")
  points(B[((sims+1):(sims+extras)),1], B[((sims+1):(sims+extras)),2],
    pch=19, col="blue")
  abline(v=(0:N)/N, h=(0:N)/N)
  if(plotDriver!="win") dev.off()

  if(plotHist)
  {
    windows()
      par(mfrow=c(2,2))
      hist(A[,1], breaks=sims, xlab="x1",
        main=paste("Original design in", sims, "bins"))
      hist(A[,1], nclass=floor(sims/3), xlab="x1",
        main=paste("Original design in", sims, "/ 3 bins"))
      hist(B[,1], breaks=N, xlab="x1",
        main=paste("Augmented design in", N, "bins"))
      hist(B[,1], breaks=floor(N/3), xlab="x1",
        main=paste("Augmented design in", N, "/ 3 bins"))
  }
}

set.seed(10)
graph2DaugmentLHS(5, 5, FALSE, "pdf", "C:\\\\program files\\R\\lhs\\vignettes")
graph2DaugmentLHS(7,1,FALSE)
graph2DaugmentLHS(50,50,TRUE)

set.seed(12)
graph2DaugmentLHS(7, 3, FALSE, "pdf", "C:\\\\program files\\R\\lhs\\vignettes")

set.seed(12)
A <- randomLHS(7,2)
B <- augmentLHS(A,3)
sims <- 7
N <- 10
    windows()
      par(mfrow=c(2,2))
      hist(A[,1], breaks=seq(0,1,by=1/sims), xlab="x1",
        main=paste("Original design in", sims, "bins"), col="blue")
      hist(A[,2], breaks=seq(0,1,by=1/sims), xlab="x2",
        main=paste("Original design in", sims, "bins"), col="blue")
      hist(B[,1], breaks=seq(0,1,by=1/N), xlab="x1",
        main=paste("Augmented design in", N, "bins"), col="blue")
      hist(B[,2], breaks=seq(0,1,by=1/N), xlab="x2",
        main=paste("Augmented design in", N, "bins"), col="blue")


