
library("quarto")
quarto_render("fisheries_documentation.qmd")

# library("servr")
# servr::httd(".", host = "0.0.0.0", port = 4001)

quarto::quarto_preview("fisheries_documentation.qmd", host = "0.0.0.0", port = 4002)

quarto::quarto_preview("fisheries_documentation.qmd")
