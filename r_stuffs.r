
# NETWORK ANALYSES SECTION.....

# NETWORKS FROM DATA...
setwd("...")

library("igraph")
G <- read.graph("data2.csv", format="edgelist", directed = FALSE)


delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0)
  delete.vertices(graph, isolates)
}

G2<-delete.isolates(G)
# plot(G2)

G3<-simplify(G2, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = igraph_opt("edge.attr.comb"))
plot(G3, layout=layout.fruchterman.reingold, vertex.size=8, edge.width=2.5)
title(main="the network")
is_simple(G3)
list<-get.edgelist(G2)
edges<-vcount(G2)

degree(G3)
centralization.degree(G3)



# CREATING NETWORKS

n<- 20
G <- make_ring(n, directed = FALSE, mutual = FALSE, circular = TRUE)
plot(G)

G <- make_star(n, mode = c("in", "out", "mutual", "undirected"), center = 1)
plot(G)

G <- make_tree(n, children = 2, mode = c("out", "in", "undirected"))
plot(G)

n <- 5
dimensions <- 3
G <- make_lattice(dimvector = NULL, length = n, dim = dimensions, nei = 1,
             directed = FALSE, mutual = FALSE, circular = FALSE)
layout_on_grid(G, dim = dimensions)
plot(G)

G <- make_full_graph(n, directed = FALSE, loops = FALSE)
plot(G)


# TIDYGRAPH

library("tidygraph")
Gtidy <- as_tibble(G)



# ------------------------
# ------------------------
# ------------------------
#  THEORETICAL FORAGING 
# ------------------------
# ------------------------
# ------------------------


library(raster)

# NORMAL DISTRIBUTION
food<-rnorm(1000, mean=0.5, sd=0.1)
hist(food, breaks = 20)

# POISSON DISTRIBUTION
food<-rpois(1000, 15)
# to normalize between 0 and 1
food<-food/max(food)
hist(food, breaks = 20)

# UNIFORM DISTRIBUTION
food<-runif(1000, 0, 1)
hist(food, breaks = 20)

# --------------

library(spatstat)

C<-rpoispp(0.01, win=as.owin(c(0,100,0,100)))
plot(C)
D<-runifpoint(100, win=as.owin(c(0,100,0,100)))
plot(D)
E<-rpoint(100, win=as.owin(c(0,100,0,100)))
plot(E)
G<-rCauchy(0.002, omega=0.02, mu=5, win=as.owin(c(0,100,0,100)))
plot(G)

plot(Kest(G))

plot(envelope(G, Kest))



# EXPLORATORY ANALYSIS
miplot(G)
fryplot(G)
clarkevans(G)
summary(G)
density.ppp(G)
clusterset(G)


# -------- CREATE RASTER FILES OF FOOD PATCHES ------

marks(G)[1:G$n]<-food[1:G$n]
# Gsmooth<-smooth.ppp(G,.07, axes=F)
Gsmooth<-idw(G, power=10)
plot(Gsmooth)
plot(raster(Gsmooth))
GRaster<-raster(Gsmooth)
writeRaster(GRaster, "provaG.asc")


# ------------------------
# ------------------------
# ------------------------
#  ABM using R
# ------------------------
# ------------------------
# ------------------------


library("simecol")

# Lotka-Volterra

library("simecol")
data(lv, package="simecol")
plot(sim(lv))


# Conway???s Game of Life

library("simecol")
data(conway, package="simecol")
plot(sim(conway))
m <- matrix(0, 40, 40)
m[5:35,19:21] <- 1
init(conway) <- m
sim(conway, animate=TRUE, delay=100, col=c("white", "green"), axes=FALSE)






