

plot_fig1 <- function(fig1a, fig1b1, fig1b2, outfile) {
  

  pp <- fig1a + fig1b1 / fig1b2 + plot_layout(widths = c(30, 12)) +
    plot_annotation(tag_levels = list(c('a)', 'b)', 'c)')))
  
  
  ggsave(outfile, plot = pp, width = 10, height = 7)
  
}