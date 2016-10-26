library("igraph")

# IMPORT DATA
mat = read.csv('full_weighted_net.csv', header=T)
att = read.csv('default_attributes.csv', header=T)

# CREATE NETWORK OBJECT
mat = as.matrix(mat)
net = graph_from_adjacency_matrix(mat, weighted=T, directed=F)
net <- simplify(net, remove.loops = T)

# CHECK EDGE WEIGHTS ATTACHED
E(net)$weight
E(net)$arrow.size = 0

# ADD NODE ATTRIBUTES
V(net)$subscribers = att$subscribers
V(net)$subreddit = unlist(att$subreddit) # returns actually alphabetically ordered id numbers - curious!
V(net)$color = 'lightblue'
V(net)$size = 5
V(net)$label.cex = 1
V(net)$label.font= 1
V(net)$label.color = 'black'
V(net)$degree <- degree(net, mode="all")
E(net)$width = E(net)$weight

plot(net, layout=layout_with_fr, vertex.size = V(net)$subscribers / 4000000,
     vertex.frame.color=NA)
