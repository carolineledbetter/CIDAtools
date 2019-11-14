library(hexSticker)
imgurl <- system.file("~/Downloads/Tools-spanner-hammer.svg", package="hexSticker")
sticker(imgurl, package="CIDAtools", p_size=20, s_x=1, s_y=.75, s_width=.6,
        h_fill = '#CFB87C',
        h_color = "#000000",
        p_color = "#000000",
        filename="inst/figures/CIDAtoolshex.png")
