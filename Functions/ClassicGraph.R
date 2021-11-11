ClassicGraph <- function(d, X, Y, ymin, ymax, Title = "Title"){
  g <- ggplot(d, aes(X, Y)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
    
    # Title
    ggtitle(Title) +
    # Axis
    ylab("P(Stay)") +
    ylim(0, 1) +
    theme_classic() +
    theme(plot.title = element_text(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12))
  print(g)
}