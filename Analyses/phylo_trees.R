library(ape)
maxage=max(diag(vcv(tree)))
# br.patterns <- function(tree,maxage){
#   require(ape) # This library needs to be installed if not already 
#   # if (class(tree) != 'phylo'){
#   # return("data were not tree in phylo format") 
#   #   } else {
    br <- sort(branching.times(tree), decreasing = T)
    br <- maxage - c(br,min(br)) # Reverses axes and adds last segment, where the graph reaches its end
    br <- c(0,br) # Add the stem branch
    n <- seq(br) # 'n' is the number of branches at each time point 
    n[length(n)] <- n[length(n) - 1] #changes last value to reflect the fact that
    # the number of lineages at the last two time points are the same (and equal to the tree size) 
    out <- data.frame(br,log(n))
    colnames(out) <- c("br.times","log(n)")
    # return(out) }
# }
# to run the function you have to have the phylogenies studied in a list, here called “Phylotrees”.
# branching.list <- lapply(tree,br.patterns,maxage=maxage)

plot(out)
tree <- ape::read.tree("../../../../Downloads/tykrHOmYb6Nie3Y1Xc886A_newick.txt")
tree <- ape::read.tree("../../../../Downloads/phyloT_generated_GTD_tree_1676632220_newick.txt")
tree <- ape::read.tree("../../../../Downloads/monegros_complete.txt")

maxage=max(diag(vcv(tree)))
branching.times(tree)
