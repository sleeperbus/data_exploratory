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
# 이런 방식으로는 패턴을 찾기 힘드라. 
# heatmap 을 그려보자.
library(dendextend)
arcsin_transformation <- function(x) asin(x/100)

dend_NA <- votes.repub %>% is.na %>% 
  dist %>% hclust %>% as.dendrogram %>% ladderize

dend <- votes.repub %>% arcsin_transformation %>% 
  dist %>% hclust(method="com") %>% as.dendrogram %>%
  rotate(labels(dend_NA)) %>%
  color_branches(k=3)

some_col_func <- colorspace::diverge_hcl

# par(mar=c(3,3,3,3))
# library(gplots)
gplots::heatmap.2(as.matrix(votes.repub),
                  main="Votes for \n Republican Presidential Candidate\n (clustered using complete)",
                  srtCol = 60,
                  dendrogram = "row",
                  Rowv = dend, 
                  Colv = "NA",
                  trace = "none",
                  margins = c(3, 6),
                  key.xlab = "% Votes for Republican\n Presidential Candidate",
                  labCol = years,
                  denscol = "grey",
                  density.info = "density",
                  col = some_col_func
                  )


