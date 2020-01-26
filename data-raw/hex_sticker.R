library(ggplot2)
library(hexSticker)

p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p <- p + theme_void() #+ theme_transparent()


sticker(ggplot() + theme_void(), package = "footballR",
        p_size = 8, p_x = 1, p_y = 1,
        # s_x=1, s_y=.75, s_width=1.3, s_height=1,
        h_color = "deepskyblue4", h_fill = "chocolate1",
        p_color = "deepskyblue4",
        filename = "inst/figures/footballR.png")

