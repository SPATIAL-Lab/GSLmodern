library(colorblindcheck)
library(gplots)

plot(0, 0, xlim = c(-1,1), ylim = c(-1, 1), type = "n", axes = FALSE,
     xlab = "", ylab = "")
rect(-1, -1, 0, 0, col = "cadetblue")
rect(-1, 0, 0, 1, col = "coral2")

rect(0, 1/3, 1, 1, col = "red3")
rect(0, -1/3, 1, 1/3, col = "darkgoldenrod2")
rect(0, -1, 1, -1/3, col = "palegreen3")

rect(-1, -1, 1, 1, border = "gray50", lwd = 10)

pal = col2hex(c("cadetblue", "coral2", "red3", "darkgoldenrod2", "palegreen3"))
palette_plot(pal)
