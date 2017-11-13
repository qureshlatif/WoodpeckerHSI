plotDens <- function(dat.plot, dat.class, sampleHSIs, thresholds, binPntSize = 2, classPntSize = 5,
                         axisLabSize = 20, tickLabSize = 15, classLabSize = 5, lab.params,
                         BS = F, ylabel = "Observed density (nests per 100 ha)",
                     xlabel = "Habitat Suitability Index (HSI)") {
  theme_set(theme_bw())
  plt <- ggplot(dat.plot, aes(HSI.md, Density)) +
    geom_point(size = binPntSize, shape = 16) +
    geom_point(data = dat.class, aes(x = HSI.md, y = Density), size = classPntSize, shape = 1, stroke = 1.5) +
    geom_rug(data = data.frame(HSI = sampleHSIs), aes(x = HSI, y = NULL), colour = "black",
             alpha = 0.3, size = 1) +
    geom_vline(xintercept = thresholds[1], linetype = "dashed") +
    geom_vline(xintercept = thresholds[2], linetype = "dashed") +
    ylab(ylabel) + xlab(xlabel) +
    theme(axis.text.x=element_text(size=tickLabSize)) +
    theme(axis.text.y=element_text(size=tickLabSize)) +
    theme(axis.title.x=element_text(size=axisLabSize)) +
    theme(axis.title.y=element_text(size=axisLabSize)) +
    annotate("text", x = lab.params[["maxSSS"]][1], y = lab.params[["maxSSS"]][2], label = "maxSSS", angle = 90) +
    annotate("text", x = lab.params[["Low"]][1], y = lab.params[["Low"]][2], label = "Low", size = classLabSize) +
    annotate("text", x = lab.params[["Moderate"]][1], y = lab.params[["Moderate"]][2], label = "Moderate",
             size = classLabSize, angle = lab.params[["Moderate"]][3]) +
    annotate("text", x = lab.params[["High"]][1], y = lab.params[["High"]][2], label = "High", size = classLabSize)
  
  if(BS == T) plt <- plt +
    geom_errorbar(data = dat.class, aes(x = HSI.md, ymin = Dens95lo, ymax = Dens95hi), size = 1, width = 0.05)
  
  return(plt)
}