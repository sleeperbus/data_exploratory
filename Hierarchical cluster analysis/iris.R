# https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html
# iris data 
iris <- datasets::iris
str(iris)

# 결과값을 분리시킨다. 
iris2 <- iris[, -5]
species_labels <- iris[, 5]

library(colorspace)
species_col <- rev(rainbow_hcl(3))[as.numeric(species_labels)]
pairs(iris2, col=species_col, lower.panel = NULL, cex.labels = 2, pch=19, cex=1)
par(xpd=TRUE)
legend(x=0.05, y=0.4, cex=2, legend=as.character(levels(species_labels)), 
       fill=unique(species_col))
par(xpd=NA)
# setosa 가 versicolor 와 virginica 와 명확히 분리된다. petal width/length 가 다른 것들보다 많이 짧다.
# 다른 그래프로 알아보자. 
par(las=1, mar=c(4.5, 3, 3, 2) + 0.1, cex = 0.8)
MASS::parcoord(iris2, col=species_col, var.label=TRUE, lwd=2)
title("Parallel coordinates plot of the Iris data")
par(xpd=TRUE)
legend(x=1.75, y=-0.25, cex=1, 
       legend=as.character(levels(species_labels)),
       fill=unique(species_col), horiz=TRUE)
par(xpd=NA)

# clustering 
d_iris <- dist(iris2)
hc_iris <- hclust(d_iris, method="complete")
iris_species <- rev(levels(iris[, 5]))

library(dendextend)
dend <- as.dendrogram(hc_iris)
dend <- rotate(dend, 1:150)
dend <- color_branches(dend, k=3)
labels_colors(dend) <- 
  rainbow_hcl(3)[sort_levels_values(as.numeric(iris[,5])[order.dendrogram(dend)])]
labels(dend) <- paste(as.character(iris[,5])[order.dendrogram(dend)], "(", labels(dend), ")", sep="")
dend <- hang.dendrogram(dend, hang_height = 0.1)
dend <- set(dend, "labels_cex", 0.5)
par(mar=c(3,3,3,7))
plot(dend, 
     main="Clustered Iris data set 
     (the labels give the true flower species)",
     horiz=TRUE, nodePar=list(cex=0.007))
legend("topleft", legend=iris_species, fill=rainbow_hcl(3))

# 원형으로 표시
par(mar=rep(0,4))
circlize_dendrogram(dend)

# heatmap 
some_col_func <- function(n) {
  rev(colorspace::heat_hcl(n, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5)))
}
gplots::heatmap.2(as.matrix(iris2), 
                  main="Heatmap for the Iris data set",
                  srtCol=20,
                  dendrogram="row",
                  Rowv=dend,
                  Colv="NA",
                  trace="none",
                  margins=c(5,0.1),
                  key.xlab="Cm",
                  denscol="grey",
                  density.info="density",
                  RowSideColors=rev(labels_colors(dend)),
                  col=some_col_func)

# interactive heatmap
d3heatmap::d3heatmap(as.matrix(iris2),
                     dendrogram="row",
                     Rowv=dend,
                     colors="Greens",
                     width=600,
                     show_grid=FALSE)

### 클러스터링 알고리즘에 따른 유사성과 차이점 
hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty", "median",
                    "centroid", "ward.D2")
iris_dendlist <- dendlist()
for (i in seq_along(hclust_methods)) {
  hc_iris <- hclust(d_iris, method=hclust_methods[i])
  iris_dendlist <- dendlist(iris_dendlist, as.dendrogram(hc_iris))
}
names(iris_dendlist) <- hclust_methods
iris_dendlist
iris_dendlist_cor <- cor.dendlist(iris_dendlist)
iris_dendlist_cor
corrplot::corrplot(iris_dendlist_cor, "pie", "lower")
# complete 알고리즘이 다른 것들과 차이가 난다. cor.denlist 는 default 로 
# pearson's measure 를 쓴다. 다른 방법을 써보자. 
iris_dendlist_cor_spearman <- cor.dendlist(iris_dendlist, method_coef="spearman")
corrplot::corrplot(iris_dendlist_cor_spearman, "pie", "lower")

# ward.D 와 ward.D2 분석
iris_dendlist %>% dendlist(which=c(1,8)) %>% 
  ladderize %>% 
  set("branches_k_color", k=3) %>%
  tanglegram(faster=TRUE)

# ward.D 와 average 분석
iris_dendlist %>% dendlist(which=c(1,4)) %>% 
  ladderize %>% 
  set("branches_k_color", k=2) %>%
  tanglegram(faster=TRUE)

iris_dendlist %>% dendlist(which=c(1,4)) %>% 
  ladderize %>% 
  set("rank_branches") %>%
  tanglegram(common_subtrees_color_branches=TRUE)
# 두 개의 dendrogram 에서 39개의 동일한 서브트리가 있다.
length(unique(common_subtrees_clusters(iris_dendlist[[1]],
                                       iris_dendlist[[4]]))[-1])

# 다른 알고리즘과 유사성이 떨어지는 complete 과의 비교
# complete 과 average
iris_dendlist %>% dendlist(which=c(3,4)) %>% ladderize %>%
  untangle(method="step1side", k_seq=2:6) %>%
  set("branches_k_color", k=2) %>%
  tanglegram(faster=TRUE)
par(mfrow=c(4,2))
for (i in 1:8) {
  iris_dendlist[[i]] %>% set("branches_k_color", k=2) %>%
    plot(axes=FALSE, horiz=TRUE)
  title(names(iris_dendlist)[i])
}

# Clustering prediction 
get_ordered_3_clusters <- function(dend) {
  cutree(dend, k=3)[order.dendrogram(dend)]
}

dend_3_clusters <- lapply(iris_dendlist, get_ordered_3_clusters)

compare_clusters_to_iris <- function(clus) {
  FM_index(clus, rep(1:3, each=50), assume_sorted_vectors = TRUE)
}

clusters_perfomance <- sapply(dend_3_clusters, compare_clusters_to_iris)
dotchart(sort(clusters_perfomance), xlim=c(0.7, 1), 
         xlab="Fowlkes-Mallow Index (from 0 to 1)",
         main="Performance of clustering algorithms \n in detecting the 3 species",
         pch = 19)
