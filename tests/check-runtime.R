# Offline regression checks: Rscript --vanilla tests/check-runtime.R
library(data.table)

# Load helpers without starting Shiny or authenticating to SWS.
app <- parse("shiny_aggregation.R")
helpers <- c(
  "get_daily_cache_context", "get_daily_cache_path", "get_daily_shared_cache",
  "set_daily_shared_cache", "is_valid_codelist_table", "fetch_codelist_table"
)
env <- new.env(parent = globalenv())
for (expression in app) {
  if (is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
      as.character(expression[[2L]]) %in% helpers) eval(expression, env)
}
env$CODELIST_CACHE_DIR <- tempfile("fisheries-cache-test-")
dir.create(env$CODELIST_CACHE_DIR)
env$KEEP_EXPIRED_CODELISTS <- "fisheriesAsfis"
endpoint <- "https://sws.qa.fao.org"
env$getClient <- function() list(sws_endpoint = endpoint)

run_checks <- function() {
  on.exit(unlink(env$CODELIST_CACHE_DIR, recursive = TRUE), add = TRUE)
  codes <- data.table(id = c("001", "002"), children = list("002", character()))
  tree <- data.table(level_1_id = "001", level_2_id = "002")
  stopifnot(env$is_valid_codelist_table(codes), env$is_valid_codelist_table(tree, TRUE))
  stopifnot(!env$is_valid_codelist_table(codes[0]),
            !env$is_valid_codelist_table(data.table(id = "001")),
            !env$is_valid_codelist_table(rbind(codes, codes)),
            !env$is_valid_codelist_table(data.table(level_1_id = NA_character_), TRUE))

  env$set_daily_shared_cache("codes", codes)
  stopifnot(isTRUE(all.equal(env$get_daily_shared_cache("codes"), codes)))
  qa_path <- env$get_daily_cache_path("codes")
  endpoint <<- "https://sws.fao.org"
  stopifnot(env$get_daily_cache_path("codes") != qa_path,
            is.null(env$get_daily_shared_cache("codes")))
  endpoint <<- "https://sws.qa.fao.org"

  # Legacy, wrong-day and truncated files must all be treated as cache misses.
  saveRDS(codes, qa_path)
  stopifnot(is.null(env$get_daily_shared_cache("codes")))
  saveRDS(list(context = env$get_daily_cache_context(), date = Sys.Date() - 1L,
               cache_id = "codes", value = codes), qa_path)
  stopifnot(is.null(env$get_daily_shared_cache("codes")))
  writeBin(charToRaw("incomplete RDS"), qa_path)
  stopifnot(is.null(env$get_daily_shared_cache("codes")))

  # Failed publication leaves the previous complete object available.
  env$set_daily_shared_cache("codes", codes)
  env$file.rename <- function(...) FALSE
  suppressWarnings(env$set_daily_shared_cache("codes", tree))
  stopifnot(isTRUE(all.equal(env$get_daily_shared_cache("codes"), codes)),
            length(list.files(env$CODELIST_CACHE_DIR, pattern = "^\\.codelist_", all.files = TRUE)) == 0L)
  rm("file.rename", envir = env)

  # Readers must see a complete old/new value during concurrent publication.
  if (.Platform$OS.type == "unix") {
    writer <- parallel::mcparallel({
      for (i in seq_len(40L)) env$set_daily_shared_cache("codes", if (i %% 2L) codes else tree)
      TRUE
    })
    for (i in seq_len(200L)) {
      value <- env$get_daily_shared_cache("codes")
      stopifnot(isTRUE(all.equal(value, codes)) || isTRUE(all.equal(value, tree)))
    }
    stopifnot(isTRUE(parallel::mccollect(writer)[[1L]]))
  }

  # A poisoned response is retried once, always bypassing both cache layers.
  calls <- 0L
  env$getCodelistInfo <- function(codelist_id, use_cache) {
    stopifnot(identical(use_cache, FALSE))
    calls <<- calls + 1L
    list(codes = if (calls == 1L) codes[0] else codes)
  }
  stopifnot(isTRUE(all.equal(env$fetch_codelist_table("example"), codes)), calls == 2L)
  env$getCodelistTree <- function(codelist_id, use_cache) {
    stopifnot(identical(use_cache, FALSE))
    tree
  }
  stopifnot(isTRUE(all.equal(env$fetch_codelist_table("example", TRUE), tree)))
  calls <- 0L
  env$getCodelistInfo <- function(...) {
    calls <<- calls + 1L
    stop("upstream failure")
  }
  result <- tryCatch(env$fetch_codelist_table("example"), error = identity)
  stopifnot(inherits(result, "error"), calls == 2L)

  # Validate the complete lockfile dependency graph, including the optional driver.
  lock <- jsonlite::read_json("renv.lock")
  requirements <- unique(unlist(lapply(lock$Packages, `[[`, "Requirements")))
  base <- rownames(installed.packages(priority = c("base", "recommended")))
  stopifnot("RPostgres" %in% names(lock$Packages),
            length(setdiff(requirements, c(names(lock$Packages), base, "R"))) == 0L)
  cat("Runtime/cache regression checks passed\n")
}
run_checks()
