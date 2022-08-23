
library(igraph)
library(ggplot2)
library(ggsci)


adj1 <- read.csv('FE_adjacency_matrix.csv', row.names = 1, check.names = FALSE)
adj2 <- read.csv('FP_adjacency_matrix.csv', row.names = 1, check.names = FALSE)
adj3 <- read.csv('SE_adjacency_matrix.csv', row.names = 1, check.names = FALSE)
adj4 <- read.csv('SP_adjacency_matrix.csv', row.names = 1, check.names = FALSE)
adj5 <- read.csv('TE_adjacency_matrix.csv', row.names = 1, check.names = FALSE)
adj6 <- read.csv('TP_adjacency_matrix.csv', row.names = 1, check.names = FALSE)


nc <- function(adj_matrix) {
  
  adj_matrix <- as.matrix(adj_matrix)
  adj_matrix[abs(adj_matrix) != 0] <- 1
  
  
  lambda <- eigen(adj_matrix, only.values = TRUE)$values
  lambda <- sort(lambda, decreasing = TRUE)
  
  lambda_sum <- 0
  N = length(lambda)
  for (i in 1:N) lambda_sum = lambda_sum + exp(lambda[i])
  lambda_average <- log(lambda_sum/N, base = exp(1))
  lambda_average
}



natural_connectivity1 <- nc(adj1)
natural_connectivity2 <- nc(adj2)
natural_connectivity3 <- nc(adj3)
natural_connectivity4 <- nc(adj4)
natural_connectivity5 <- nc(adj5)
natural_connectivity6 <- nc(adj6)



g1 <- graph_from_adjacency_matrix(as.matrix(adj1), mode = 'undirected', diag = FALSE)
g2 <- graph_from_adjacency_matrix(as.matrix(adj2), mode = 'undirected', diag = FALSE)
g3 <- graph_from_adjacency_matrix(as.matrix(adj3), mode = 'undirected', diag = FALSE)
g4 <- graph_from_adjacency_matrix(as.matrix(adj4), mode = 'undirected', diag = FALSE)
g5 <- graph_from_adjacency_matrix(as.matrix(adj5), mode = 'undirected', diag = FALSE)
g6 <- graph_from_adjacency_matrix(as.matrix(adj6), mode = 'undirected', diag = FALSE)



average_degree1 <- mean(degree(g1))
average_degree2 <- mean(degree(g2))
average_degree3 <- mean(degree(g3))
average_degree4 <- mean(degree(g4))
average_degree5 <- mean(degree(g5))
average_degree6 <- mean(degree(g6))



for (i in 1:99) {

  remove_node1 <- sample(1:nrow(adj1), 0.01*i*nrow(adj1))
  adj1_remove <- adj1[-remove_node1,-remove_node1]

  natural_connectivity1 <- c(natural_connectivity1, nc(adj1_remove))

  g1 <- graph_from_adjacency_matrix(as.matrix(adj1_remove), mode = 'undirected', diag = FALSE)
  
  average_degree1 <- c(average_degree1, mean(degree(g1)))
  
  }




 for (i in 1:99) {

  remove_node2 <- sample(1:nrow(adj2), 0.01*i*nrow(adj2))
  adj2_remove <- adj2[-remove_node2,-remove_node2]

  natural_connectivity2 <- c(natural_connectivity2, nc(adj2_remove))

  g2 <- graph_from_adjacency_matrix(as.matrix(adj2_remove), mode = 'undirected', diag = FALSE)

  average_degree2 <- c(average_degree2, mean(degree(g2)))

}





for (i in 1:99) {
  
  remove_node3 <- sample(1:nrow(adj3), 0.01*i*nrow(adj3))
  adj3_remove <- adj3[-remove_node3,-remove_node3]

  natural_connectivity3 <- c(natural_connectivity3, nc(adj3_remove))

  g3 <- graph_from_adjacency_matrix(as.matrix(adj3_remove), mode = 'undirected', diag = FALSE)
  
  average_degree3 <- c(average_degree3, mean(degree(g3)))
  
}





for (i in 1:99) {

  remove_node4 <- sample(1:nrow(adj4), 0.01*i*nrow(adj4))
  adj4_remove <- adj4[-remove_node4,-remove_node4]

  natural_connectivity4 <- c(natural_connectivity4, nc(adj4_remove))

  g4 <- graph_from_adjacency_matrix(as.matrix(adj4_remove), mode = 'undirected', diag = FALSE)
  
  average_degree4 <- c(average_degree4, mean(degree(g4)))
  
}





for (i in 1:99) {

  remove_node5 <- sample(1:nrow(adj5), 0.01*i*nrow(adj5))
  adj5_remove <- adj5[-remove_node5,-remove_node5]

  natural_connectivity5 <- c(natural_connectivity5, nc(adj5_remove))

  g5 <- graph_from_adjacency_matrix(as.matrix(adj5_remove), mode = 'undirected', diag = FALSE)
  
  average_degree5 <- c(average_degree5, mean(degree(g5)))
  
}





for (i in 1:99) {

  remove_node6 <- sample(1:nrow(adj6), 0.01*i*nrow(adj6))
  adj6_remove <- adj6[-remove_node6,-remove_node6]

  natural_connectivity6 <- c(natural_connectivity6, nc(adj6_remove))

  g6 <- graph_from_adjacency_matrix(as.matrix(adj6_remove), mode = 'undirected', diag = FALSE)
  
  average_degree6 <- c(average_degree6, mean(degree(g6)))
  
}



dat1 <- data.frame(remove_node = rep((0:99), 2),
                   variable = c(rep('natural_connectivity', 100), 
                                rep('average_degree', 100)),
                   values = c(natural_connectivity1, average_degree1),
                   group = rep("freshwater_environment", 100))


dat2 <- data.frame(remove_node = rep((0:99), 2),
                   variable = c(rep('natural_connectivity', 100), 
                                rep('average_degree', 100)),
                   values = c(natural_connectivity2, average_degree2),
                   group = rep("freshwater_plastisphere", 100))


dat3 <- data.frame(remove_node = rep((0:99), 2),
                   variable = c(rep('natural_connectivity', 100), 
                                rep('average_degree', 100)),
                   values = c(natural_connectivity3, average_degree3),
                   group = rep("seawater_environment", 100))

dat4 <- data.frame(remove_node = rep((0:99), 2),
                   variable = c(rep('natural_connectivity', 100), 
                                rep('average_degree', 100)),
                   values = c(natural_connectivity4, average_degree4),
                   group = rep("seawater_plastisphere", 100))


dat5 <- data.frame(remove_node = rep((0:99), 2),
                   variable = c(rep('natural_connectivity', 100), 
                                rep('average_degree', 100)),
                   values = c(natural_connectivity5, average_degree5),
                   group = rep("terrestrial_environment", 100))


dat6 <- data.frame(remove_node = rep((0:99), 2),
                   variable = c(rep('natural_connectivity', 100), 
                                rep('average_degree', 100)),
                   values = c(natural_connectivity6, average_degree6),
                   group = rep("terrestrial_plastisphere", 100))


data <- rbind(dat1, dat2, dat3, dat4, dat5, dat6)


write.csv(data, 'robust_propotional_remove_nodes.csv', row.names = FALSE, quote = FALSE)



#########   plot ###

library(ggpmisc)
library(ggsci)

lccol2 =c("#377EB8", "#E41A1C")

dat <- read.csv('robust_propotional_remove_nodes_data.csv', header = T)

p1 = ggplot(dat, aes(x= Node_removal_percentage, y=values, color = carrier)) +
  
  geom_point(shape = 19,  size = 1, alpha = 0.8) +
  
  geom_line(size = 0.6) +
  
  scale_color_manual(values = lccol2) +
  
  theme_minimal()+
  
  theme(legend.position = c(0.8, 0.9), panel.border = element_blank()) +
  
  # geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE) +
  # 
  # stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
  #              formula = y~x, parse = TRUE, 
  #              label.x.npc = 'right', label.y.npc = 'top', size = 2.7) + 
  
  facet_wrap(ecosystem~variable, nrow = 3, scales = "free")

p1



ggsave(plot = p1, "network_robustness.pdf", width = 110, height = 160, units = c("mm"), dpi = 300)



