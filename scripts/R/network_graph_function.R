# -----------------------------
# GLOBAL SCALES (shared across all traits)
# -----------------------------
GLOBAL_SIZE_BREAKS <- c(1, 10, 20,30, 70)
GLOBAL_SIZE_RANGE  <- c(4, 12)

GLOBAL_BF_RANGE <- c(0.7, 2.5)

format_bf_labels <- function(x) scales::comma(round(x))

## Generic network function

prepare_network_data <- function(merged_summary, rewards) {
  
  edges <- merged_summary %>%
    dplyr::filter(supported) %>%
    dplyr::mutate(
      from = as.character(from),
      to = as.character(to),
      origin = from
    )
  
  # Build clean node list
  nodes <- tibble::tibble(
    name = unique(c(edges$from, edges$to))
  ) %>%
    dplyr::left_join(
      rewards %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(
          reward = mean(reward, na.rm = TRUE),
          percent_time = mean(percent_time, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "name"
    ) %>%
    dplyr::distinct(name, .keep_all = TRUE)
  
  list(edges = edges, nodes = nodes)
}


plot_trait_network <- function(edges, nodes, state_cols,
                               panel_label = "B",
                               layout_type = "manual") {
  
  library(ggraph)
  library(igraph)
  
  # Build graph
  g <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)
  
  # ---- Layout ----
  if (layout_type == "manual") {
    
    n <- length(unique(nodes$name))
    
    # Arrange in vertical + side layout if 3 states, otherwise circle fallback
    if (n == 3) {
      lay <- create_layout(
        g,
        layout = "manual",
        x = c(-1, 0, 1),
        y = c(-1, 1, 0)
      )
    } else {
      lay <- create_layout(g, layout = "circle")
    }
    
  } else {
    lay <- create_layout(g, layout = layout_type)
  }
  
  # ---- Extract BF values ----
  bf_vals <- sort(unique(edges$bayes_factor))
  
  # ---- Plot ----
  p <- ggraph(lay) +
    # Nodes first 
    geom_node_point(aes(size = percent_time, color = name)) +
    
    geom_node_text(
      aes(label = name),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    # Edges second 
    geom_edge_arc(
      aes(
        edge_width = log10(bayes_factor),
        edge_color = origin
      ),
      arrow = arrow(length = unit(3, "mm")),
      end_cap = circle(2.5, "mm"),
      alpha = 0.9
    ) +
    
    # Scales
    scale_edge_color_manual(values = state_cols, guide = "none") +
    
    scale_color_manual(
      values = state_cols,
      name = "State/Origin"
    ) +
    
    scale_edge_width(
      range = GLOBAL_BF_RANGE,
      breaks = log10(bf_vals),
      labels = format_bf_labels(bf_vals),
      name = "Bayes factor"
    ) +
    
    scale_size_continuous(
      range = GLOBAL_SIZE_RANGE,
      breaks = GLOBAL_SIZE_BREAKS,
      labels = paste0(GLOBAL_SIZE_BREAKS, "%"),
      name = "Persistence\n(% time)"
    ) +
    
    theme_void(base_size = 10) +
    theme(
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width = unit(0.6, "cm"),
      plot.title = element_text(face = "bold", hjust = 0)
    ) +
    
    ggtitle(panel_label)
  
  return(p)
}


net_data <- prepare_network_data(merged_summary, area_rewards)

network_plot <- plot_trait_network(
  edges = net_data$edges,
  nodes = net_data$nodes,
  state_cols = state_cols,
  panel_label = "B"
)
