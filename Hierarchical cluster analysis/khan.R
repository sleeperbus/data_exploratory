# https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html 
library(dendextend)
train <- dendextend::khan$train
test <- dendextend::khan$test

d_train <- train %>% dist %>% hclust %>% as.dendrogram()
d_test <- test %>% dist %>% hclust %>% as.dendrogram()
d_train_test <- dendlist(train=d_train, test=d_test)

d_train_test %>% cor.dendlist()
d_train_test %>% cor.dendlist(method_coef="spearman")

# 어떤 레벨에서 cut 하는 것이 최적인지 볼 것이다. 
Bk_plot(d_train, d_test, k=2:30, xlim=c(2,30))
# 7 clusters 에서 얼추 비슷해짐이라고 함... 

pred_tang_d_train_test <- d_train_test %>% ladderize %>% set("branches_k_color", k=7)
train_branches_colors <- get_leaves_branches_col(pred_tang_d_train_test$train)
pred_tang_d_train_test %>% tanglegram(fast=TRUE, color_lines=train_branches_colors)
