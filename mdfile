---
title: "Seat Choice Analysis"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

In this analysis, we will create a dataset based on the seating adjacency described in the problem, calculate centrality measures for each seat choice (A-D), discuss the consequences of the seat choice, and provide the code for plotting the network graph with labels and centrality values.

## Load Required Packages

```{r}
library(igraph)
```

## Create Dataset and Calculate Centrality Measures

```{r}
# Create a graph based on seat adjacency
edges <- data.frame(
  from = c('1', '1', '2', '2', '3', '3', '3', '4', '4', '4', '5', '5', '6', '6', 'A', 'A', 'B', 'C'),
  to = c('2', 'C', 'B', 'C', '4', 'C', 'D', '5', 'C', 'D', '6', 'D', 'B', 'D', 'B', 'C', 'D', 'D')
)
G <- graph_from_data_frame(edges, directed = FALSE)

# Calculate centrality measures for each seat choice
seat_choices <- c('A', 'B', 'C', 'D')
centrality_measures <- list()

for (seat in seat_choices) {
  # Degree centrality
  degree_centrality <- degree(G, v = seat) / (vcount(G) - 1)
  
  # Closeness centrality
  closeness_centrality <- closeness(G, vids = seat)
  
  # Betweenness centrality
  betweenness_centrality <- betweenness(G)[seat] / ((vcount(G) - 1) * (vcount(G) - 2) / 2)
  
  centrality_measures[[seat]] <- list(
    Degree_Centrality = degree_centrality,
    Closeness_Centrality = closeness_centrality,
    Betweenness_Centrality = betweenness_centrality
  )
}

# Print centrality measures for each seat choice
for (seat in seat_choices) {
  cat(paste0("Seat ", seat, ":\n"))
  for (measure in names(centrality_measures[[seat]])) {
    cat(paste0("  ", measure, ": ", round(centrality_measures[[seat]][[measure]], 3), "\n"))
  }
  cat("\n")
}
```
## Discussion

- Seat A: This seat has the lowest degree centrality (0.222) and betweenness centrality (0.009) among all the seat choices, indicating limited direct connections and minimal influence over information flow. It also has the lowest closeness centrality (0.059), suggesting that it may not be as efficient in reaching other seats compared to the other options. This seat may be beneficial if you want to maintain a very low profile and are comfortable with potentially having less access to information from other seats.

- Seat B: This seat has a moderate degree centrality (0.444) and the second-highest betweenness centrality (0.139), indicating a fair number of direct connections and some influence over information flow. It has a closeness centrality (0.071) that is higher than Seat A but lower than Seats C and D. This seat may be a good balance between being connected and not being too central, allowing for some level of access to information and influence without being in the most prominent position.

- Seat C: This seat has the highest degree centrality (0.667) and closeness centrality (0.083), tied with Seat D. This indicates that Seat C has the most direct connections and is most efficient in reaching other seats. It also has the highest betweenness centrality (0.333), suggesting a significant role in information flow and potential influence within the network. This seat may be beneficial if you want to be well-connected and have a central role in the network. However, it may also attract the most attention and come with higher expectations.

- Seat D: This seat has the same high degree centrality (0.667) and closeness centrality (0.083) as Seat C, indicating many direct connections and efficient access to other seats. It has the second-highest betweenness centrality (0.231), suggesting a significant role in information flow, although slightly lower than Seat C. This seat may be beneficial if you want to be influential and have access to diverse information, similar to Seat C. However, it may also come with increased responsibility and pressure, albeit to a slightly lesser extent compared to Seat C.

In summary, Seats C and D emerge as the most central and influential positions, with Seat C having a slight edge in terms of betweenness centrality. Seat B offers a moderate level of connectivity and influence, while Seat A is the least central and may be suitable for those seeking a low profile.

## Network Graph

```{r}
# Create a layout for the graph
layout <- matrix(c(0, 0, 1, 0, 2, 1, 1, 1, 0, 1, 0, 2, 1, 2, 2, 3, 1, 4, 2, 4), ncol = 2, byrow = TRUE)
layout <- layout[c(1, 2, 6, 5, 9, 3, 4, 8, 7, 10), ]
rownames(layout) <- V(G)$name

# Plot the graph
plot(G, layout = layout, vertex.size = 30, vertex.label.cex = 1.2, vertex.label.color = "black", 
     vertex.label.dist = 2, edge.arrow.size = 0.5, main = "Seat Choice Network")

# Add centrality values as labels
for (seat in seat_choices) {
  text(layout[seat, 1], layout[seat, 2] + 0.05, 
       labels = paste0("DC: ", round(centrality_measures[[seat]]$Degree_Centrality, 2), "\nCC: ", round(centrality_measures[[seat]]$Closeness_Centrality, 2), "\nBC: ", round(centrality_measures[[seat]]$Betweenness_Centrality, 2)),
       cex = 0.8, pos = 3)
}
```

## Conclusion

In conclusion, the choice of seat on the company bus can have implications for informal connections and communication with coworkers. Each seat has its own advantages and disadvantages in terms of centrality measures. The decision should be based on personal preferences and goals, whether it is to maintain a low profile, balance connections, be well-connected, or have influence over information flow. The network graph provides a visual representation of the seating arrangement and the centrality values for each seat choice.
