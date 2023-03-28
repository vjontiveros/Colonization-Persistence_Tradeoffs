library(ape)
library(tidyverse)

tree <- ape::read.tree("../../../../Downloads/pyrenees.txt")

# We modified the script from Yguel et al. 2016, as it produced negative branching times.

maxage=max(diag(vcv(tree)))

    br <- sort(branching.times(tree), decreasing = T)
    br <- maxage - c(br,min(br)) # Reverses axes and adds last segment, where the graph reaches its end
    br <- c(0,br) # Add the stem branch
    n <- seq(br) # 'n' is the number of branches at each time point 
    n[length(n)] <- n[length(n) - 1] #changes last value to reflect the fact that

    out <- data.frame(br,log(n))
    colnames(out) <- c("br.times","log(n)")

out1 <- out

tree <- ape::read.tree("../../../../Downloads/soils.txt")

maxage=max(diag(vcv(tree)))

br <- sort(branching.times(tree), decreasing = T)
br <- maxage - c(br,min(br)) # Reverses axes and adds last segment, where the graph reaches its end
br <- c(0,br) # Add the stem branch
n <- seq(br) # 'n' is the number of branches at each time point 
n[length(n)] <- n[length(n) - 1] #changes last value to reflect the fact that

out <- data.frame(br,log(n))
colnames(out) <- c("br.times","log(n)")

out2 <- out

tree <- ape::read.tree("../../../../Downloads/monegros_complete.txt")

maxage=max(diag(vcv(tree)))

br <- sort(branching.times(tree), decreasing = T)
br <- maxage - c(br,min(br)) # Reverses axes and adds last segment, where the graph reaches its end
br <- c(0,br) # Add the stem branch
n <- seq(br) # 'n' is the number of branches at each time point 
n[length(n)] <- n[length(n) - 1] #changes last value to reflect the fact that

out <- data.frame(br,log(n))
colnames(out) <- c("br.times","log(n)")

out3 <- out


ltts <- rbind(out1 %>% add_column(Type = "A"), out2 %>% add_column(Type = "B"),
      out3 %>% add_column(Type = "C"))

ltts_plot <- ggplot(ltts, aes(x = br.times, y = `log(n)`, color = Type)) + geom_point() +
    facet_wrap(~ Type, ncol = 1, scales = 'free') + 
    geom_abline(data = data.frame(slope = c(5.598422/2.01345, 6.329721/2.25700, 6.781058/2.81841), Type = c("A", "B", "C")),
                aes(slope = slope, intercept = 0)) +
    scale_color_manual(values = c("#14878c", "#81392a", "#e4b925")) +
    theme_bw() +
    theme(aspect.ratio = .618, panel.grid.minor = element_blank(), legend.position = 'none', 
          strip.text = element_text(hjust = 0, size = 14), strip.background = element_blank()) +
    xlab("Branching times") + ylab("Log Number of Lineages")

ggsave(ltts_plot, filename = "Figures/ltts.eps", dpi = 300, width = 9, units = "cm")   
