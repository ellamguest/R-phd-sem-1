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
# net <- graph_from_data_frame(d=partial_edges, vertices=partial_nodes)


### ADDING NODE ATTIBUTES
# network visualisation attributesattributes
colrs <- c("red", "blue")
# V(net)$color <- colrs[V(net)$type+1] # red = redditors (mod), blue = subs
V(net)$color = adjustcolor(colrs[V(net)$type+1], 0.5)
V(net)$shape <- c("square", "circle")[V(net)$type+1]  # square = redditors (mod), circle = subs
V(net)$label.font = V(net)$type+1 # redditors = plain, subs = bold
V(net)$label.cex = (V(net)$type+1)*0.5
V(net)$label.color = colrs[V(net)$type+1]

### SELECTING FOR LARGE COMPONETS
clu = components(net, mode = c("weak", "strong"))

#### MUST CUSTOMISE HERE FOR DIFFERENT NETWORKS
# for orgs partial network:


### PLOT ORIGINAL MODERATORS NETWORK
# # orginial moderators network components:
giant = mapply(c, groups(clu)[1], groups(clu)[7], SIMPLIFY=FALSE)
sub = induced_subgraph(net, unlist(giant))

loose = mapply(c, groups(clu)[2:6], groups(clu)[8:23], SIMPLIFY=FALSE)
sub2 = induced_subgraph(net, unlist(loose))


# node shape plot
pdf('orgs_giant_label.pdf')
plot(sub, vertex.frame.color=NA, vertex.shape='none',
     vertex.size=5, edge.arrow.size=0)
dev.off()

## PLOTTING FULL NETWORK
# # full network components:
sub = induced_subgraph(net,unlist(groups(clu)[1]))
sub2 = induced_subgraph(net,unlist(groups(clu)[2]))
sub3 = induced_subgraph(net,unlist(groups(clu)[3]))

# # plot full giant component
# # setting labels for subreddits only
V(sub)$label[V(sub)$type==0] <- ""
V(sub)$label[V(sub)$type==1] = V(sub)$name[V(sub)$type==1]
V(sub)$label.cex = 1
V(sub)$label.font= 1
V(sub)$label.color = 'black'
V(sub)$degree <- degree(sub, mode="all")
# 
# pdf('giantcomponent.pdf')
# plot(sub, vertex.shape = 'none', edge.arrow.size=0, layout=l,
#      vertex.label.cex=1)
# dev.off()
# 
# # plot full net small components
# plot(sub2, vertex.shape = 'none', edge.arrow.size=0, layout=layout_with_fr,
#      vertex.label.cex=1, vertex.label.color=V(sub2)$color)
# 
# plot(sub3, vertex.shape = 'none', edge.arrow.size=0, layout=layout_with_fr,
#      vertex.label.cex=1, vertex.label.color=V(sub3)$color)