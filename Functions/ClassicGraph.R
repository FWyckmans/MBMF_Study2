ClassicGraph <- function(d, X, Y, ymin, ymax, Title = "Title"){
  g <- ggplot(d, aes(X, Y, fill = X)) +
    geom_bar(stat = "identity", color = "black") +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
    
    # Title
    ggtitle(Title) +
    
    # Axis
    ylab("P(Stay)") +
    scale_y_continuous(limits=c(0.4, 1), oob = rescale_none) +
    
    # Theme
    theme_classic() +
    theme(legend.title = element_blank(), legend.position = "none") +
    scale_fill_manual(values=c("blue3", "blue3", "gray80", "gray80")) +
    theme(plot.title = element_text(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12))
  print(g)
}
