rm(list = ls())

library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(ggplot2)

mov <- read.csv("imdb_movie_dataset.csv")

movies <- mov %>%
  select(Title, Actors, Year, Genre) %>%
  filter(Year >= 2000) %>%
  filter(!is.na(Title), !is.na(Actors))   

# Build bipartite edge list: Movie -> Actor

movieactoredges <- movies %>%
  separate_rows(Actors, sep = ",") %>%
  mutate(actor = str_trim(Actors)) %>%
  transmute(
    movie = Title,
    actor = actor,
    year  = Year,
    genre = Genre
  ) %>%
  filter(actor != "") %>%
  distinct(movie, actor, year, genre)

# Bipartite graph + projection to Actor–Actor network

g_bip <- graph_from_data_frame(movieactoredges, directed = FALSE)

# Mark node types: TRUE = movies, FALSE = actors
V(g_bip)$type <- V(g_bip)$name %in% movieactoredges$movie
table(V(g_bip)$type)
# Project onto actors (type == FALSE -> proj1)
g_actor <- bipartite_projection(g_bip)$proj1

# Keep giant component (main connected collaboration space)
comp <- components(g_actor)
giant_id <- which.max(comp$csize)
g_actor <- induced_subgraph(g_actor, which(comp$membership == giant_id))

# Basic structure metrics

net_statistics <- list(
  nodes = vcount(g_actor),
  edges = ecount(g_actor),
  density = edge_density(g_actor),
  avg_path = mean_distance(g_actor),
  diameter = diameter(g_actor),
  connected = is.connected(g_actor)
)

print(net_statistics)

# Centrality metrics

deg <- degree(g_actor)
strg <- strength(g_actor, weights = E(g_actor)$weight)
btw <- betweenness(g_actor, normalized = TRUE)
eig <- eigen_centrality(g_actor, weights = E(g_actor)$weight)$vector
clo <- closeness(g_actor, normalized = TRUE)

centrality <- tibble(
  actor = names(deg),
  degree = as.numeric(deg),
  strength = as.numeric(strg),
  betweenness = as.numeric(btw),
  eigenvector = as.numeric(eig),
  closeness = as.numeric(clo)
) %>%
  arrange(desc(degree))

print(head(centrality, 10))

 # Visualization

set.seed(123)

plot(
  g_actor,
  layout = layout_with_fr(g_actor),
  vertex.size = 3,
  vertex.label = NA,
  edge.width = log(E(g_actor)$weight + 1),
  main = "Actor Collaboration Network (IMDb)"
)
# Community detection (Louvain) on actor network

comm <- cluster_louvain(g_actor, weights = E(g_actor)$weight)
V(g_actor)$community <- membership(comm)

comm_summary <- list(
  n_communities = length(comm),
  modularity = modularity(comm),
  sizes = sort(sizes(comm), decreasing = TRUE)
)

print(comm_summary$n_communities)
print(comm_summary$modularity)
print(head(comm_summary$sizes, 10))

# 7) Genre interpretation by community 

actor_comm <- tibble(
  actor = V(g_actor)$name,
  community = V(g_actor)$community
)

genre_comm <- movieactoredges %>%
  inner_join(actor_comm, by = "actor") %>%
  separate_rows(genre, sep = ",") %>%
  mutate(genre = str_trim(genre)) %>%
  count(community, genre, name = "n") %>%
  group_by(community) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  arrange(community, desc(prop))

# top 8 genres in community 9 - biggest community
genre_comm %>%
  filter(community == 9) %>%
  slice_max(prop, n = 8) %>%
  print()

# Visualizations


# Plot top-100 actors only (cleaner community visualization)
topN <- 100
top_actors <- centrality$actor[1:topN]
g_top <- induced_subgraph(g_actor, vids = top_actors)

comm_top <- cluster_louvain(g_top, weights = E(g_top)$weight)
V(g_top)$community <- membership(comm_top)

set.seed(123)
plot(g_top,
     layout = layout_with_fr(g_top),
     vertex.size = 5,
     vertex.label = NA,
     vertex.color = V(g_top)$community,
     edge.width = log(E(g_top)$weight + 1),
     main = paste0("Top-", topN, " actors — Communities (Louvain)"))

# Bar chart of genre composition for one community
genre_comm %>%
  filter(community %in% c(1, 2, 3)) %>%
  slice_max(prop, n = 8, by = community) %>%
  ggplot(aes(x = reorder(genre, prop), y = prop, fill = factor(community))) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Genre composition across top communities",
    x = "Genre",
    y = "Proportion of actor–movie appearances"
  ) +
  theme_minimal()
