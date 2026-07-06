library(ape)

# Load tree
tree <- read.tree("analysis/contextual_analysis/lac-all/la_tree_apr82026/redcap_plus_la_plus_outgroup_stripped_aln.fa.contree",comment.char = "")

# --- Terminal branch cleaning ---

# Compute terminal branch lengths
term_branches <- tree$edge.length[tree$edge[,2] <= length(tree$tip.label)]
names(term_branches) <- tree$tip.label

# Identify terminal outliers (e.g., top 1%)
term_threshold <- quantile(term_branches, 0.99)
terminal_outliers <- names(term_branches[term_branches > term_threshold])

# --- Internal branch cleaning ---

# Helper: get all descendant tips of a node
get_tip_descendants <- function(tree, node) {
  descendants <- c()
  children <- tree$edge[tree$edge[,1] == node, 2]
  for (child in children) {
    if (child <= length(tree$tip.label)) {
      descendants <- c(descendants, child)
    } else {
      descendants <- c(descendants, get_tip_descendants(tree, child))
    }
  }
  return(descendants)
}

# Compute internal branch lengths
internal_nodes <- unique(tree$edge[,1])
internal_nodes <- internal_nodes[internal_nodes > length(tree$tip.label)]  # only internal

internal_branch_lengths <- sapply(internal_nodes, function(node) {
  # Take average of edges descending from this node
  child_edges <- tree$edge.length[tree$edge[,1] == node]
  mean(child_edges)
})

# Identify internal outliers (e.g., top 1%)
internal_threshold <- quantile(internal_branch_lengths, 0.99)
internal_outlier_nodes <- internal_nodes[internal_branch_lengths > internal_threshold]

# Get all descendant tips of these internal outliers
internal_tips_to_drop <- unlist(lapply(internal_outlier_nodes, function(node) {
  get_tip_descendants(tree, node)
}))
internal_tips_to_drop_labels <- tree$tip.label[internal_tips_to_drop]

# --- Drop all tips ---

#all_tips_to_drop <- unique(c(terminal_outliers, internal_tips_to_drop_labels))
#without internal outliers:
all_tips_to_drop <- terminal_outliers
all_tips_to_drop <- setdiff(all_tips_to_drop, "KF154998")

# Check the updated list
all_tips_to_drop
tree_clean <- drop.tip(tree, all_tips_to_drop)
#tree_clean <- drop.tip(tree, terminal_outliers)
plot(tree_clean)
root_tree=root(tree_clean, "KF154998")
plot(root_tree)
# Save cleaned tree
write.tree(tree_clean, file = "analysis/contextual_analysis/lac-all/la_tree_apr82026/redcap_plus_la_plus_outgroup_stripped_outlierBranchesRemoved.treefile")

#cat("Removed", length(all_tips_to_drop), "tips (terminal + internal outliers).\n")
