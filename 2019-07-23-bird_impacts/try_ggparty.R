library(ggparty)
library(dplyr)

set.seed(1)
iris2 = iris %>% 
  dplyr::select(Sepal.Width, Sepal.Length) %>% 
  dplyr::mutate(random = rnorm(150))
  
  
tree <- lmtree(random ~ ., data = iris2)

ggparty(tree,
        terminal_space = 0.5,
        add_vars = list(p.value = "$node$info$p.value")) +
  geom_edge(size = 1.5) +
  geom_edge_label(colour = "grey", size = 6) +
  geom_node_splitvar()
#############################

library(ggparty)

airq <- subset(airquality, !is.na(Ozone))
airct <- ctree(Ozone ~ ., data = airq)

ggparty(airct, horizontal = TRUE, terminal_space = 0.6) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  geom_node_plot(gglist = list(
    geom_density(aes(x = Ozone))),
    shared_axis_labels = TRUE)