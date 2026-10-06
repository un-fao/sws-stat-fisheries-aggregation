# -------------------------------------------------------------------------
# Shared daily codelist cache
# -------------------------------------------------------------------------

R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH", unset = "")
if (!nzchar(R_SWS_SHARE_PATH)) {
  R_SWS_SHARE_PATH <- Sys.getenv("SWS_SHARED_DRIVE", unset = "")
}
if (!nzchar(R_SWS_SHARE_PATH)) {
  R_SWS_SHARE_PATH <- tempdir()
}

CODELIST_CACHE_DIR <- file.path(R_SWS_SHARE_PATH,"Fisheries_aggregation_shiny_app")

dir.create(
  CODELIST_CACHE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)



if(!dir.exists(CODELIST_CACHE_DIR)){
  warning("Shared codelist cache unavailable; using a temporary cache: ", CODELIST_CACHE_DIR)
  CODELIST_CACHE_DIR <- file.path(tempdir(), "Fisheries_aggregation_shiny_app")
  dir.create(CODELIST_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
}

# Invalidate old layouts and separate endpoints/client versions on the shared drive.
get_daily_cache_context <- function() {
  list(
    schema = 2L,
    endpoint = sub("/+$", "", getClient()$sws_endpoint),
    client_version = as.character(utils::packageVersion("SwsApiClient")),
    keep_expired = KEEP_EXPIRED_CODELISTS
  )
}

# Build the file path used for one cached object.
get_daily_cache_path <- function(
    cache_id,
    cache_date = Sys.Date()
) {
  
  safe_id <- gsub(
    "[^A-Za-z0-9_-]",
    "_",
    cache_id
  )
  
  file.path(
    CODELIST_CACHE_DIR,
    paste0(
      digest::digest(get_daily_cache_context(), algo = "sha256"),
      "__",
      safe_id,
      "__",
      as.character(cache_date),
      ".rds"
    )
  )
}


# Retrieve today's cached object.
get_daily_shared_cache <- function(
    cache_id
) {
  
  cache_file <- get_daily_cache_path(
    cache_id = cache_id
  )
  
  if (!file.exists(cache_file)) {
    return(NULL)
  }
  
  value <- tryCatch(
    readRDS(
      cache_file
    ),
    error = function(e) {
      NULL
    }
  )
  
  if (!is.list(value) ||
      !identical(value$context, get_daily_cache_context()) ||
      !identical(value$date, Sys.Date()) ||
      !identical(value$cache_id, cache_id)) {
    return(NULL)
  }
  
  value$value
}


# Save today's cached object.
set_daily_shared_cache <- function(
    cache_id,
    value
) {
  
  cache_file <- get_daily_cache_path(
    cache_id = cache_id
  )
  
  # Write alongside the destination so publication is an atomic rename.
  # A failed cache write must not prevent use of freshly retrieved data.
  temp_file <- tempfile(pattern = ".codelist_", tmpdir = CODELIST_CACHE_DIR)
  on.exit(unlink(temp_file), add = TRUE)
  tryCatch({
    saveRDS(list(
      context = get_daily_cache_context(), date = Sys.Date(),
      cache_id = cache_id, value = value
    ), temp_file)
    if (!file.rename(temp_file, cache_file)) {
      stop("Atomic rename failed")
    }
  }, error = function(e) {
    warning("Could not save codelist cache '", cache_id, "': ", conditionMessage(e))
  })
  
  invisible(
    value
  )
}

# Clean the old cached data every two days 
clean_old_codelist_cache <- function(
    keep_days = 2L
) {
  
  cache_files <- list.files(
    CODELIST_CACHE_DIR,
    pattern = "\\.rds$",
    full.names = TRUE
  )
  
  if (length(cache_files) == 0L) {
    return(
      invisible(NULL)
    )
  }
  
  file_age_days <- as.numeric(
    difftime(
      Sys.time(),
      file.info(cache_files)$mtime,
      units = "days"
    )
  )
  
  old_files <- cache_files[
    !is.na(file_age_days) &
      file_age_days > keep_days
  ]
  
  if (length(old_files) > 0L) {
    unlink(
      old_files
    )
  }
  
  invisible(NULL)
}

clean_old_codelist_cache()

is_valid_codelist_table <- function(value, tree = FALSE) {
  if (!is.data.frame(value) || nrow(value) == 0L) return(FALSE)
  if (tree) {
    id_columns <- grep("^level_[0-9]+_id$", names(value), value = TRUE)
    return(length(id_columns) > 0L && any(vapply(id_columns, function(column) {
      ids <- value[[column]]
      any(!is.na(ids) & nzchar(trimws(as.character(ids))))
    }, logical(1))))
  }
  if (!all(c("id", "children") %in% names(value))) return(FALSE)
  ids <- as.character(value$id)
  all(!is.na(ids) & nzchar(trimws(ids))) && !anyDuplicated(ids)
}

fetch_codelist_table <- function(codelist_id, tree = FALSE) {
  # A daily disk-cache miss must refresh both the wrapper and the API cache.
  # Otherwise a long-lived R process can write yesterday's wrapper data today.
  fetch <- function() {
    value <- if (tree) {
      getCodelistTree(codelist_id, use_cache = FALSE)
    } else {
      getCodelistInfo(codelist_id, use_cache = FALSE)$codes
    }
    if (!is_valid_codelist_table(value, tree)) {
      stop("Empty or invalid codelist response for '", codelist_id, "'", call. = FALSE)
    }
    copy(as.data.table(value))
  }
  tryCatch(fetch(), error = function(e) {
    message("Retrying codelist '", codelist_id, "' after: ", conditionMessage(e))
    fetch()
  })
}


