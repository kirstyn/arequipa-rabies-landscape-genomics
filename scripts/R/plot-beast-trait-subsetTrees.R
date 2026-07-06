library(treeio)
library(ggtree)


# Read only the first 10 trees
subset_trees <- read.beast("test-tree.nexus")
# The district info is stored in node annotations
district <- subset_trees@data$district  # this should return "hunter", "ccolorado", etc.

# Now you can colour tips by district
ggtree(subset_trees, aes(color=district)) + geom_tippoint()
