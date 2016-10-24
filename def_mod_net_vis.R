# setwd("~/Programmming/R/phd-sem-1")
### LOAD PACKAGES
library("igraph")

### IMPORT DATA
# partial_edges = read.csv('edgelist.csv', header=T)
# partial_nodes = read.csv('nodelist.csv', header=T)
edges = read.csv('edgelist_full.csv', header=T)
nodes = read.csv('nodelist_full.csv', header=T)


# creating network objects
net <- graph_from_data_frame(d=edges, vertices=nodes)


### ADDING NODE ATTIBUTES
# 1) network measure attributes
# V(net)$degree <- degree(net, mode="all")
# V(net)$between <- betweenness(net) # 0 for all
# V(net)$closeness<- closeness(net)

# 2) network visualisation attributesattributes
colrs <- c("red", "blue")
V(net)$color <- colrs[V(net)$type+1] # red = redditors (mod), blue = subs
V(net)$shape <- c("square", "circle")[V(net)$type+1]  # square = redditors (mod), circle = subs
V(net)$label.font = V(net)$type+1 # redditors = plain, subs = bold
V(net)$label.cex = (V(net)$type+1)*0.5

### SELECTING FOR LARGE COMPONETS
clu = components(net, mode = c("weak", "strong"))
groups(clu) #### MUST CUSTOMISE HERE FOR DIFFERENT NETWORKS

# for orgs partial network:
# c1 = mapply(c, groups(clu)[1],SIMPLIFY=FALSE)
# x = unlist(conn)
# sub = induced_subgraph(net,x)

# for full network:
sub = induced_subgraph(net,unlist(groups(clu)[1]))
sub2 = induced_subgraph(net,unlist(groups(clu)[2]))
sub3 = induced_subgraph(net,unlist(groups(clu)[3]))

### PLOT NETWORKS
# node shape plot
pdf('?.pdf')
plot(sub2, vertex.label = V(sub)$label, vertex.size=1, edge.arrow.size=0)
dev.off()

# plot giant component
# setting labels for subreddits only
V(sub)$label[V(sub)$type==0] <- ""
V(sub)$label[V(sub)$type==1] = V(sub)$name[V(sub)$type==1]
V(sub)$label.cex = 1
V(sub)$label.font= 1
V(sub)$label.color = 'black'

l <- layout_with_fr(sub) # best layout for orgs net

# pdf('giantcomponent.pdf')
# plot(sub, vertex.shape = 'none', edge.arrow.size=0, layout=l,
#      vertex.label.cex=log(V(sub)$degree))
# dev.off()

# plot small components
plot(sub2, vertex.shape = 'none', edge.arrow.size=0, layout=l,
     vertex.label.cex=1, vertex.label.color=V(sub2)$color)

plot(sub3, vertex.shape = 'none', edge.arrow.size=0, layout=layout_with_fr,
     vertex.label.cex=1, vertex.label.color=V(sub3)$color)