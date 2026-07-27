# ==============================================================================
# Title: plot_contextualTree.R
# Description: Plot phylogeny for all LAC RABV sequences and focused subtree of AM5- alongside plot_contextual_map script
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))

# source contextual map
source("scripts/R/plot_contextual_map.R")

# Import tree
tree=read.newick(here("analysis","contextual_analysis","la_tree_apr82026","redcap_plus_la_plus_outgroup_stripped_outlierBranchesRemoved.treefile"))

# tree plots:  aesthetics, ladderize
root_tree=root(tree, "KF154998")
root_tree=drop.tip(root_tree, "KF154998")

## edit display names
annot$data_source <- gsub("Our Study", "Our_study", annot$data_source)
annot$EPA_minor_clade <- annot$EPA_minor_clade %>%
  as.character() %>%
  na_if("") %>%
  na_if(" - ") %>%
  tidyr::replace_na("Unassigned")
gplot <- ggtree(root_tree,ladderize = TRUE, size=0.2, col="darkgrey") %<+% annot

## tree with country coloured
country_plot <-   gplot+ 
  ## added bootstraps >0.8 on internal nodes
  geom_nodepoint( fill = "lightgrey",
                  colour = "black",
                  shape = 23,
                  alpha = 0.7,
                  size = 1.2, aes(subset= !is.na(as.numeric(label)) & as.numeric(label) > 0.8))+
    new_scale_fill() +
    geom_point2(aes(subset=(data_source %in% "ncbi"), fill=country),shape = 21, size = 2, color = "grey30", alpha = 0.8) +
   colScale3+ # Highlight clades/tips from our study
   # geom_point2(aes(subset=(data_source %in% "Our_study"), fill=country), shape=23,color = "black",size=3)+ 
  guides(fill = guide_legend(ncol = 2))+ 
  coord_cartesian(clip = "off")+
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) ; country_plot

# ggsave(
#   filename = "figures/LAC_tree_country.tiff",
#   plot = country_plot,
#   device = "tiff",
#   dpi = 600,
#   width = 180,
#   height = 220,
#   units = "mm",
#   compression = "lzw"
# )
# ggsave(
#   filename = "figures/LAC_tree_country.pdf",
#   plot = country_plot,
#   device = cairo_pdf,
#   width = 180,
#   height = 220,
#   units = "mm"
# )


  #geom_tippoint(aes(subset=(Study %in% "This study")),col="grey", pch=21, fill="black")
  
# tree with clade coloured 
 clade_plot <-  gplot+
    ## added bootstraps >0.8 on internal nodes
    geom_nodepoint(   fill = "lightgrey",
                      colour = "black",
                      shape = 23,
                      alpha = 0.7,
                      size = 1.2, aes(subset= !is.na(as.numeric(label)) & as.numeric(label) > 0.8))+
    new_scale_fill() +
    geom_tippoint(
      aes(fill = EPA_minor_clade),
      ,shape = 21, size = 2, color = "grey30", alpha = 0.8
    ) +
    colScale2+
    guides(fill = guide_legend(ncol = 2))+ 
   
   coord_cartesian(clip = "off") +
   
   scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) ; clade_plot
  # ggsave(
  #   filename = "figures/LAC_tree_minorClade.tiff",
  #   plot = clade_plot,
  #   device = "tiff",
  #   dpi = 600,
  #   width = 180,
  #   height = 220,
  #   units = "mm",
  #   compression = "lzw"
  # )
  # ggsave(
  #   filename = "figures/LAC_tree_minorClade.pdf",
  #   plot = clade_plot,
  #   device = cairo_pdf,
  #   width = 180,
  #   height = 220,
  #   units = "mm"
  # )

######################

##expand the AM5 clade 
MRCA(root_tree, annot$taxon[gplot +
  geom_nodepoint(
    fill = "lightgrey",
    colour = "black",
    shape = 23,
    alpha = 0.7,
    size = 1.2,
    aes(subset = !is.na(as.numeric(label)) & as.numeric(label) > 0.8)
  ) +
  new_scale_fill() +
  geom_tippoint(
    aes(
      fill = EPA_minor_clade,
      shape = data_source == "Our_study"
    ),
    size = 2,
    color = "grey30",
    alpha = 0.8
  ) +
  scale_shape_manual(values = c(`FALSE` = 21, `TRUE` = 24)) +
  colScale2 +
  guides(fill = guide_legend(ncol = 2)) & annot$taxon != "219_2024" & annot$taxon != "52_2021"])
 
  MRCA(root_tree, annot$taxon[annot$minor_clade =="AM5"])
  mrca=1734
  mrca2=1419
  mrca_am5=2421
  mrca_aqp=2455
subset_tree <- tidytree::tree_subset(root_tree, mrca_am5, levels_back = 0, root_edge = T)
subset_tree2 <- tidytree::tree_subset(root_tree, mrca_aqp, levels_back = 0)

zoom_peru <- ggtree(subset_tree,ladderize = TRUE, size=0.5, col="black") %<+% annot
zoom_peru+ geom_text2(
  aes(label = node,subset = isTip == FALSE),
  hjust =1.3, vjust = 0, size = 2.5, color = "darkred"
)+geom_tiplab(size=0.4)
mrca_aqp_cluster=MRCA(zoom_peru, annot$taxon[annot$data_source == "Our_study" & annot$taxon != "219_2024" & annot$taxon != "PP965355"])
mrca_main_aqp_cluster=mrca_aqp_cluster+1
mrca_ped_cluster=562
zoom_peru2 <- ggtree(subset_tree2,ladderize = TRUE, size=0.5, col="darkgrey") %<+% annot

# 1. First scale clade
zoom_peru_scaled1 <- scaleClade(zoom_peru, node = 647, scale = 1.5, vertical = TRUE)

# 3. Collapse that clade
zoom_peru_collapsed <- scaleClade(zoom_peru_scaled1 , mrca_main_aqp_cluster, .1, vertical = TRUE) %>% scaleClade(mrca_ped_cluster, .1, vertical = TRUE) %>%ggtree::collapse(mrca_main_aqp_cluster, 'max', fill="#1B511B")  %>%
  ggtree::collapse(mrca_ped_cluster, 'max', fill = "#1B511B")


#collapse at node 349 to simplify showing arequipa in context here
# 349 is the main arequipa cluster or 320 for all study seq
# expand clade 535 to show intro
context <- zoom_peru_collapsed +
  layout_rectangular()+
  geom_treescale(
    width = 0.008,
    x=0,
    y=45,
    fontsize = 0   # hides default label
  ) +
  annotate(
    "text",
    x = 0.005,
    y = 38,
    label = "0.008 subs/site\n(~95 SNPs)",
    size = 3
  )+
  # geom_treescale(
  #   x = 0,      # horizontal position along x-axis
  #   y = 30, 
  #   offset.label = 0.3, 
  #   fontsize = 5
  # )+
  # geom_tiplab(aes(label = ifelse(country == "Peru", geo_loc, "")),
  #             size = 4, hjust = -0.2)+
  # geom_tiplab(aes(label = ifelse(country == "Peru" & data_source == "ncbi",
  # paste(geo_loc, collection_year,sep=":"), "")), size = 1.2, hjust = -0.05)+
  geom_nodepoint(
    aes(subset = !is.na(as.numeric(label)) & as.numeric(label) > 0.9),
    fill = "lightgrey",
    colour = "black",
    shape = 23,
    alpha = 0.7,
    size = 1.2
  ) +
  # geom_nodepoint(color="grey", shape=18, alpha=1, size=3 , aes(subset= !is.na(as.numeric(label)) & as.numeric(label) > 0.4))+
  geom_point2(aes(subset=(data_source %in% "ncbi"), fill=country),shape = 21, size = 2, color = "grey30", alpha = 0.8 )+
colScale3+
  geom_star(
    aes(subset=(data_source %in% "Our_study"), fill=country), starshape=1, size=4)+
  #geom_point2(aes(subset=(data_source %in% "Our_study"), color=country), shape=11,size=2)+ coord_cartesian(clip = "off")+
  geom_fruit(
    geom=geom_col,
    mapping=aes(x=real_length/1000, fill=country),
    pwidth=0.4,
    offset = 0.1,
    axis.params=list(
      axis="x", # add axis text of the layer.
      text.size=5,
      vjust=0, hjust=0 ,nbreak=6,
      text.angle=-45,
      line.size=0.3, line.color="grey"# adjust the horizontal position of text of axis.
    ))+
  theme(legend.position = "none")+
  coord_cartesian(clip = "off")+
  theme(plot.margin = unit(c(1,1,2,1), "cm"))  # top, right, bottom, left  

plot_context <- context +
  theme(
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA)
  )
plot_context
ggsave(
  "analysis/contextual_analysis/figures/am5-contextTree-nolabels.png",
  plot = plot_context,
  width = 210,
  height = 200,
  units = "mm",
  dpi = 600,
  bg = "transparent"
)
ggsave(
  "analysis/contextual_analysis/figures/am5-contextTree-nolabels.pdf",
  plot = plot_context,
  width = 210,
  height = 200,
  units = "mm",
  dpi = 600,
  bg = "transparent"
)

   scale_fill_manual(name = "",
  labels =  c('Other', 'This study'),
  values=country_cols, na.translate=FALSE) +
  scale_starshape_manual(values=c(13,1), na.translate=FALSE)+
  guides(starshape = F,fill=FALSE)+
  guides(fill=FALSE)+
  new_scale_fill()+
  geom_fruit(geom=geom_tile, mapping=aes(fill=alignment.displayName), width=0.005)+theme(legend.text=element_text(size=10))+
  colScale2+ theme(legend.position='bottom') + guides(fill = guide_legend(ncol = 1, order=2,title.position = "top"))+
  guides(fill =F)+
  new_scale_fill()+
  geom_fruit(
    geom=geom_col,
    mapping=aes(x=sequence.gb_length, fill=sequence.m49_country.display_name),
    pwidth=0.6,
    offset = 0.1,
    axis.params=list(
      axis="x", # add axis text of the layer.
      text.angle=-45,
      text.size=3,# the text angle of x-axis.
      vjust=1, hjust=0 ,nbreak=5 # adjust the horizontal position of text of axis.
    )
  )+  
  colScale1+
  theme(plot.margin = unit(c(1, 1, 1.5, 1), "lines"))+ # top, right, bottom, left)
  guides(fill =F);zoom1

#73_2021

# zoom2=zoom_peru2+
#   layout_rectangular()+
#   geom_treescale(col="grey")+
#   geom_nodepoint(color="grey21", shape=18, alpha=1, size=3 , aes(subset= !is.na(as.numeric(label)) & as.numeric(label) > 0.9))+
#   geom_fruit(
#     geom=geom_star,
#     mapping=aes(fill=sequence.m49_country.display_name,  starshape=Study),
#     position="identity",colour="black",
#     starstroke=0.2, size=2.5
#   )+
#   colScale1+
#   #  scale_fill_manual(name = "",
#   # labels =  c('Other', 'This study'),
#   # values=country_cols, na.translate=FALSE) +
#   scale_starshape_manual(values=c(13,1), na.translate=FALSE)+
#   guides(starshape = FALSE,fill=FALSE)+
#   guides(fill=FALSE)+
#   new_scale_fill()+
#   geom_fruit(geom=geom_tile, mapping=aes(fill=alignment.displayName), width=0.008)+theme(legend.text=element_text(size=10))+
#   colScale2+ theme(legend.position='bottom') +guides(fill=FALSE)+
#   new_scale_fill()+
#   geom_fruit(
#     geom=geom_col,
#     mapping=aes(x=sequence.gb_length, fill=sequence.m49_country.display_name),
#     pwidth=0.6,
#     offset = 0.1,
#     axis.params=list(
#       axis="x", # add axis text of the layer.
#       text.angle=-45,
#       text.size=2,# the text angle of x-axis.
#       vjust=1, hjust=0 ,nbreak=10 # adjust the horizontal position of text of axis.
#     )
#   )+
#   colScale1+
#   guides(fill =FALSE);zoom2

