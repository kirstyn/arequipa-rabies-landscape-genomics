# comparing metadata versions
# code will compare 2 dfs and identify discrepancies (additions/deletions/changes)

library(compareDF)
#https://github.com/alexsanjoseph/compareDF
#devtools::install_github('alexsanjoseph/compareDF')

#df1
#old=read.csv("datasets/rabiescases_genomics_11feb22.csv")
old=epi_subset


#df2
new=read.table("/Users/kirstyn.brunker/GitHub/rage-redcap-developer/peru/epi_subset99.txt", sep="\t", header=T)

#make comparable
#either limit to columns that are the same (as I have here) or use exclude parameter in compare_df to ignore cols in one df but not the other
compare_old=old[,which(names(old)  %in% names(new))]
compare_new=new[,which(names(new)  %in% names(old))]
compare_old<-compare_old[names(compare_new)]

# do the comparison
c1=compare_df(compare_new,compare_old,c("Taxon"))

# summarise
# for html (in viewer):
#create_output_table(c1)
# for excel output
#create_output_table(c1, output_type = 'xlsx', file_name = "outputs/discrepancies_15feb.xlsx")
create_output_table(c1, output_type = 'xlsx', file_name = "/Users/kirstyn.brunker/GitHub/rage-redcap-developer/peru/epi_subset99_changes.xls")
