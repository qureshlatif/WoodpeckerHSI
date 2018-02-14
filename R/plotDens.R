plotDens <- function(dat.plot, dat.class, sampleHSIs, thresholds, binPntSize = 2, classPntSize = 5,
                     axisLabSize = 20, tickLabSize = 15,
                     BS = F, ylabel = "Observed density (nests per 100 ha)",
                     xlabel = "Habitat Suitability Index (HSI)") {
  theme_set(theme_bw())
  if(nrow(dat.class) != (length(thresholds) + 1)) stop("Incorrect number of thresholds provided for the number of suitability classes.")
  plt <- ggplot(dat.plot, aes(HSI.md, Density)) +
    geom_point(size = binPntSize, shape = 16)
  if(BS == T) plt <- plt +
    geom_errorbar(data = dat.class, aes(x = HSI.md, ymin = Dens95lo, ymax = Dens95hi), size = 1, width = 0.05)
  plt <- plt + geom_point(data = dat.class, aes(x = HSI.md, y = Density),
                          fill = "white", size = classPntSize, shape = 1, stroke = 1.5) +
    geom_rug(data = data.frame(HSI = sampleHSIs), aes(x = HSI, y = NULL), colour = "black",
             alpha = 0.3, size = 1) +
    ylab(ylabel) + xlab(xlabel) +
    theme(axis.text.x=element_text(size=tickLabSize)) +
    theme(axis.text.y=element_text(size=tickLabSize)) +
    theme(axis.title.x=element_text(size=axisLabSize)) +
    theme(axis.title.y=element_text(size=axisLabSize))
  for(t in 1:length(thresholds))
    plt <- plt + geom_vline(xintercept = thresholds[t], linetype = "dashed")
  
  return(plt)
}