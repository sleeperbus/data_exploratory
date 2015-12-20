votes.repub <- cluster::votes.repub
str(votes.repub)

years <- as.numeric(gsub("X", "", colnames(votes.repub)))
par(las=2, mar=c(4.5,3,3,2)+0.1, cex=.8)
#MASS::parcoord(votes.repub, var.label=FALSE, lwd=1)
matplot(1L:ncol(votes.repub), t(votes.repub), type="l", col=1, lty=1,
        axes=F, xlab="", ylab="")
axis(1, at=seq_along(years), labels = years)
axis(2)
title("Votes for Republican Candidate\n in Presidential Elections\n 
      (each line is a country - over the years")