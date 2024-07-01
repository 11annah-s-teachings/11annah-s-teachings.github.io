library(dplyr)
library(ggplot2)
library(factoextra)
library(ggfortify)
library(cowplot)

#a)
data(iris)
iris.pca<-prcomp(iris[,-5],scale=TRUE)
iris.pca

#b)
plot_grid(fviz_eig(iris.pca,addlabels = TRUE)+ggtitle(""),
          fviz_eig(iris.pca, choice = "eigenvalue",addlabels = TRUE)+ggtitle(""))
ggsave("ScreePlot.png",width=8,height=6)

#c)
autoplot(iris.pca, loadings=TRUE, 
         loadings.colour='darkorchid4', loadings.label=TRUE, loadings.label.size=3,repel = TRUE)+
          theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank() 
  )
ggsave("BiPlot.png",width=6,height=5)



