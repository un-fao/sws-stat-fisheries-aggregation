library(shiny)
library(bslib)
library(SwsApiClient)
# Required for direct DB access; this is only a Suggests dependency of the client.
library(RPostgres)
library(faoswsFlag)
library(data.table)
library(DT)
library(ggplot2)
library(treemapify)
library(shinyTree)
message("[Fisheries runtime] ", paste(vapply(
  c("SwsApiClient", "DBI", "RPostgres", "DT"),
  function(package) paste0(package, "=", utils::packageVersion(package)),
  character(1)
), collapse = " "))

source("R/core.R", local = TRUE)
source("ui/ui.R", local = TRUE)
source("R/codelists.R", local = TRUE)
source("R/cache.R", local = TRUE)
source("server/server.R", local = TRUE)

shinyApp(ui, server)
