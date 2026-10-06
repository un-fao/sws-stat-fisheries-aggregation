# -------------------------------------------------------------------------
# Fisheries aggregation Shiny app
#
# Purpose:
# - Initialise the SWS API client.
# - Load Fisheries or disseminated Fisheries-related datasets.
# - Allow users to filter and/or aggregate across several dimensions:
#     1. Country / geographical area
#     2. Species / ASFIS
#     3. Fishing area
#     4. Environment type
#     5. Flag / quality-status values
# - Allow several dimensions to be used at the same time.
# - Produce result tables, yearly summaries, visual checks, and CSV downloads.
# - Process quantity, price, and value datasets through the same workflow
#   for the moment, as requested.
#
# Current aggregation assumption:
# - The selected value column is aggregated using the same aggregation logic
#   across datasets.
# - If flagObservationStatus and flagMethod are available, flags are aggregated
#   with faoswsFlag::aggregateFlagData().
# - If those flag columns are not available, values are summed without flag
#   aggregation.
# -------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Dataset registry
#
# This registry defines exactly which datasets the app supports.
# Each configured dataset has:
# - a dataset id
# - a user-facing label
# - a group: current or disseminated
# - a type: quantity, price, or value
# - the exact column names to use
# -------------------------------------------------------------------------

SWS_ENDPOINT = Sys.getenv("SWS_ENDPOINT", unset = "https://sws.fao.org")

# Common column names used across all supported Fisheries datasets.
COMMON_FISHERIES_COLUMNS <- list(
  year_col = "timePointYears",
  value_col = "Value",
  geographical_area_col = "geographicAreaM49_fi",
  species_col = "fisheriesAsfis",
  fishing_area_col = "fisheriesCatchArea",
  measured_element_col = "measuredElement",
  observation_flag_col = "flagObservationStatus",
  method_flag_col = "flagMethod"
)


# Build the configuration for one dataset, combining dataset metadata,
# common Fisheries columns, and dataset-specific optional columns.
make_dataset_config <- function(dataset_id,
                                label,
                                dataset_group,
                                dataset_type,
                                has_production_source = FALSE,
                                has_currency_flag = FALSE) {
  c(
    list(
      dataset_id = dataset_id,
      label = label,
      dataset_group = dataset_group,
      dataset_type = dataset_type
    ),
    COMMON_FISHERIES_COLUMNS,
    list(
      production_source_col = if (has_production_source) {
        "fisheriesProductionSource"
      } else {
        NULL
      },
      currency_flag_col = if (has_currency_flag) {
        "flagCurrency"
      } else {
        NULL
      }
    )
  )
}


# Populated automatically from SWS after client initialisation.
DATASET_CONFIG <- list()


build_dataset_config_from_sws <- function() {
  
  datasets <- as.data.table(
    getAllDatasets()
  )
  
  # Keep only datasets belonging to the two domains used by the app.
  datasets[
    ,
    domain_id_clean := tolower(
      trimws(
        as.character(domain_id)
      )
    )
  ]
  
  # Standardised dataset IDs for matching.
  datasets[
    ,
    dataset_id_clean := tolower(
      trimws(
        as.character(id)
      )
    )
  ]
  
  # IDs of all datasets belonging to the Fisheries domain.
  fisheries_ids <- datasets[
    domain_id_clean == "fisheries",
    dataset_id_clean
  ]
  
  # Possible disseminated counterpart of each Fisheries dataset.
  fisheries_disseminated_ids <- paste0(
    fisheries_ids,
    "_disseminated"
  )
  
  # Keep:
  # 1. every dataset from the Fisheries domain;
  # 2. only Fisheries-related datasets from the Disseminated domain.
  datasets <- datasets[
    domain_id_clean == "fisheries" |
      (
        domain_id_clean == "disseminated" &
          (
            grepl(
              "fi",
              dataset_id_clean,
              fixed = TRUE
            ) |
              grepl(
                "fisheries",
                dataset_id_clean,
                fixed = TRUE
              ) |
              dataset_id_clean %in%
              fisheries_disseminated_ids
          )
      )
  ]
  
  # Keep published datasets only.
  datasets <- datasets[
    state == "published"
  ]
  
  if (nrow(datasets) == 0L) {
    return(list())
  }
  
  datasets[
    ,
    dimensions_clean := lapply(
      dimensions,
      function(x) {
        trimws(
          unlist(
            strsplit(
              as.character(x),
              ",",
              fixed = TRUE
            )
          )
        )
      }
    )
  ]
  
  configs <- lapply(
    seq_len(nrow(datasets)),
    function(i) {
      
      row_i <- datasets[i]
      
      dims_i <- row_i$dimensions_clean[[1]]
      
      make_dataset_config(
        dataset_id = as.character(row_i$id),
        label = as.character(row_i$label),
        
        dataset_group = if (
          identical(
            as.character(row_i$domain_id_clean),
            "fisheries"
          )
        ) {
          "current"
        } else {
          "disseminated"
        },
        
        # Quantity / value / price all currently use the same
        # aggregation workflow in the app.
        dataset_type = "fisheries",
        
        has_production_source =
          "fisheriesProductionSource" %in% dims_i,
        
        has_currency_flag =
          "flagCurrency" %in% dims_i
      )
    }
  )
  
  names(configs) <- as.character(
    datasets$id
  )
  
  configs
}

AGGREGATION_DIMENSIONS <- list(
  geographical_area = list(
    label = "Country / geographical area",
    dataset_column = "geographicAreaM49_fi",
    codelist = "geographicAreaM49_fi",
    remainder_label = "Other filtered geographical areas"
  ),
  
  species = list(
    label = "Species / ASFIS",
    dataset_column = "fisheriesAsfis",
    codelist = "fisheriesAsfis",
    remainder_label = "Other filtered species"
  ),
  
  fishing_area = list(
    label = "Fishing area",
    dataset_column = "fisheriesCatchArea",
    codelist = "fisheriesCatchArea",
    remainder_label = "Other filtered fishing areas"
  ),
  
  production_source = list(
    label = "Production source / environment type",
    dataset_column = "fisheriesProductionSource",
    codelist = "fisheriesProductionSource",
    remainder_label = "Other filtered production sources"
  )
)

# Define the dataset dimensions available for filtering in the application,
# including the corresponding dataset column and codelist, where applicable.
FILTER_DIMENSIONS <- list(
  
  measured_element = list(
    label = "Measured element",
    dataset_column = "measuredElement",
    codelist = NULL
  ),
  
  geographical_area = list(
    label = "Geographical area",
    dataset_column = "geographicAreaM49_fi",
    codelist = "geographicAreaM49_fi"
  ),
  
  species = list(
    label = "Species / ASFIS",
    dataset_column = "fisheriesAsfis",
    codelist = "fisheriesAsfis"
  ),
  
  fishing_area = list(
    label = "Fishing area",
    dataset_column = "fisheriesCatchArea",
    codelist = "fisheriesCatchArea"
  ),
  
  production_source = list(
    label = "Production source / environment type",
    dataset_column = "fisheriesProductionSource",
    codelist = "fisheriesProductionSource"
  ),
  
  observation_flag = list(
    label = "Observation flag",
    dataset_column = "flagObservationStatus",
    codelist = NULL
  ),
  
  # method_flag = list(
  #   label = "Method flag",
  #   dataset_column = "flagMethod",
  #   codelist = NULL
  # ),
  
  currency_flag = list(
    label = "Currency flag",
    dataset_column = "flagCurrency",
    codelist = NULL
  )
)



# Retrieve the configuration associated with the selected dataset.
get_dataset_config <- function(dataset_id) {
  if (is.null(dataset_id) || !nzchar(dataset_id)) {
    stop("No dataset has been selected.")
  }
  
  if (!dataset_id %in% names(DATASET_CONFIG)) {
    stop(
      "Dataset '", dataset_id, "' is not configured in DATASET_CONFIG."
    )
  }
  
  DATASET_CONFIG[[dataset_id]]
}


# Return the datasets available for selection, optionally restricted to a dataset group.
get_dataset_choices <- function(dataset_group = NULL) {
  configs <- DATASET_CONFIG
  
  if (
    is.null(configs) ||
    length(configs) == 0L
  ) {
    return(character(0))
  }
  
  if (!is.null(dataset_group)) {
    configs <- configs[
      vapply(
        configs,
        function(x) identical(x$dataset_group, dataset_group),
        logical(1)
      )
    ]
  }
  
  if (length(configs) == 0L) {
    return(character(0))
  }
  
  ids <- names(configs)
  
  labels <- vapply(
    configs,
    function(x) paste0(
      x$dataset_id,
      " - ",
      x$label
    ),
    character(1)
  )
  
  stats::setNames(
    ids,
    labels
  )
}


get_primary_dataset_choices <- function(dataset_group = NULL) {
  
  # Tags belong to a base Fisheries dataset.
  # Therefore the first selector must show the current Fisheries
  # datasets whose tags can then be retrieved.
  if (identical(dataset_group, "tagged")) {
    return(
      get_dataset_choices("current")
    )
  }
  
  get_dataset_choices(dataset_group)
}

# Check that the selected dataset contains all required columns
# and report any missing optional columns.
validate_dataset_columns <- function(data, dataset_id) {
  cfg <- get_dataset_config(dataset_id)
  
  required_cols <- c(
    cfg$year_col,
    cfg$value_col,
    cfg$measured_element_col
  )
  
  optional_cols <- unique(
    na.omit(
      c(
        cfg$geographical_area_col,
        cfg$species_col,
        cfg$fishing_area_col,
        cfg$observation_flag_col,
        cfg$method_flag_col,
        cfg$production_source_col,
        cfg$currency_flag_col
      )
    )
  )
  missing_required <- setdiff(required_cols, names(data))
  missing_optional <- setdiff(optional_cols, names(data))
  
  if (length(missing_required) > 0) {
    stop(
      "The selected dataset is missing required columns: ",
      paste(missing_required, collapse = ", ")
    )
  }
  
  list(
    config = cfg,
    required_cols = required_cols,
    optional_cols = optional_cols,
    missing_required = missing_required,
    missing_optional = missing_optional
  )
}




# Return y when x is NULL or empty; otherwise return x.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# Fisheries source values contain at most four decimal places.
VALUE_DECIMAL_DIGITS <- 4L
# Standardize a vector of codes by converting to character,
# trimming whitespace, removing missing/empty values, and keeping unique codes.
clean_code_vector <- function(x) {
  
  if (is.null(x)) {
    return(character(0))
  }
  
  x <- unique(
    trimws(
      as.character(x)
    )
  )
  
  x[
    !is.na(x) &
      nzchar(x)
  ]
}


# Build a descriptive context string for aggregation operations,
# including the dimension, dataset column, codelist, selections, and processing stage.
format_aggregation_context <- function(
    dimension_id,
    label,
    dataset_column,
    codelist = NULL,
    selected_codes = NULL,
    stage = NULL
) {
  
  # Clean and standardize all context inputs.
  label <- clean_code_vector(label)
  dimension_id <- clean_code_vector(dimension_id)
  dataset_column <- clean_code_vector(dataset_column)
  codelist <- clean_code_vector(codelist)
  selected_codes <- clean_code_vector(selected_codes)
  stage <- clean_code_vector(stage)
  
  # Use the provided label when available; otherwise use a fallback label.
  label_text <- if (length(label) > 0L) {
    label[1L]
  } else {
    "Unknown aggregation dimension"
  }
  # Initialize the vector that will contain the context details.
  details <- character(0)
  # Add the aggregation dimension ID when available.
  if (length(dimension_id) > 0L) {
    details <- c(
      details,
      paste0(
        "dimension ID: ",
        dimension_id[1L]
      )
    )
  }
  # Add the corresponding dataset column when available.
  if (length(dataset_column) > 0L) {
    details <- c(
      details,
      paste0(
        "dataset column: ",
        dataset_column[1L]
      )
    )
  }
  # Add the codelist used for the dimension when available.
  if (length(codelist) > 0L) {
    details <- c(
      details,
      paste0(
        "codelist: ",
        codelist[1L]
      )
    )
  }
  # Add the selected aggregation codes when available.
  if (length(selected_codes) > 0L) {
    details <- c(
      details,
      paste0(
        "selected code(s): ",
        paste(
          selected_codes,
          collapse = ", "
        )
      )
    )
  }
  # Add the processing stage when available.
  if (length(stage) > 0L) {
    details <- c(
      details,
      paste0(
        "stage: ",
        stage[1L]
      )
    )
  }
  # Combine the label and all available details into one context string.
  paste0(
    label_text,
    " [",
    paste(
      details,
      collapse = "; "
    ),
    "]"
  )
}

# Evaluate an aggregation expression while attaching detailed aggregation
# context to any error message that occurs.
with_aggregation_context <- function(
    expression,
    dimension_id,
    label,
    dataset_column,
    codelist = NULL,
    selected_codes = NULL,
    stage = NULL
) {
  # Build a descriptive context string for the current aggregation operation.
  context <- format_aggregation_context(
    dimension_id = dimension_id,
    label = label,
    dataset_column = dataset_column,
    codelist = codelist,
    selected_codes = selected_codes,
    stage = stage
  )
  # Run the expression and intercept any error raised during execution.
  tryCatch(
    force(expression),
    # If an error occurs, re-throw it together with the aggregation context.
    error = function(e) {
      stop(
        paste0(
          context,
          ": ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}


normalise_and_drop_empty_values <- function(
    data,
    value_col = "Value"
) {
  
  dt <- copy(
    as.data.table(data)
  )
  
  # Tagged datasets may return lowercase "value".
  if (
    "value" %in% names(dt) &&
    !value_col %in% names(dt)
  ) {
    setnames(
      dt,
      "value",
      value_col
    )
  }
  
  if (!value_col %in% names(dt)) {
    stop(
      paste0(
        "Value column '",
        value_col,
        "' was not found."
      )
    )
  }
  
  value_character <- trimws(
    as.character(
      dt[[value_col]]
    )
  )
  
  # Empty and whitespace-only values become NA.
  value_character[
    is.na(value_character) |
      !nzchar(value_character)
  ] <- NA_character_
  
  # Nonnumeric values also become NA.
  value_numeric <- suppressWarnings(
    as.numeric(value_character)
  )
  
  dt[
    ,
    (value_col) := value_numeric
  ]
  
  # Zero remains valid. Only NA rows are discarded.
  dt <- dt[
    !is.na(get(value_col))
  ]
  
  dt[]
}

uses_filter <- function(action) {
  action %in% c("filter", "filter_aggregate")
}

uses_aggregate <- function(action) {
  action %in% c("aggregate", "filter_aggregate")
}


sum_or_na <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  sum(x, na.rm = TRUE)
}


infer_value_decimal_places <- function(
    x,
    max_digits = 10L,
    tolerance = 1e-9
) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  
  x <- x[
    !is.na(x) &
      is.finite(x)
  ]
  
  if (length(x) == 0) {
    return(0L)
  }
  
  for (digits_i in 0:max_digits) {
    
    rounded_x <- round(
      x,
      digits = digits_i
    )
    
    if (
      all(
        abs(x - rounded_x) <= tolerance
      )
    ) {
      return(
        as.integer(digits_i)
      )
    }
  }
  
  as.integer(max_digits)
}


sum_or_na_rounded <- function(
    x,
    digits
) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  round(
    sum(
      x,
      na.rm = TRUE
    ),
    digits = digits
  )
}

has_children <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(FALSE)
  }
  y <- unlist(x, recursive = TRUE, use.names = FALSE)
  y <- y[!is.na(y) & nzchar(as.character(y))]
  length(y) > 0
}

extract_child_ids <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  
  # If SWS returns children as a data.frame/data.table with an id column.
  if (is.data.frame(x)) {
    if ("id" %in% names(x)) {
      ids <- as.character(x$id)
      ids <- ids[!is.na(ids) & nzchar(ids)]
      return(unique(ids))
    }
    
    ids <- unlist(x, recursive = TRUE, use.names = FALSE)
    ids <- as.character(ids)
    ids <- ids[!is.na(ids) & nzchar(ids)]
    return(unique(ids))
  }
  
  # If SWS returns children as a simple vector.
  if (is.atomic(x)) {
    ids <- as.character(x)
    ids <- ids[!is.na(ids) & nzchar(ids)]
    return(unique(ids))
  }
  
  # If SWS returns children as a list.
  if (is.list(x)) {
    if (!is.null(x$id)) {
      ids <- as.character(x$id)
      ids <- ids[!is.na(ids) & nzchar(ids)]
      return(unique(ids))
    }
    
    ids <- unlist(
      lapply(x, extract_child_ids),
      recursive = TRUE,
      use.names = FALSE
    )
    
    ids <- as.character(ids)
    ids <- ids[!is.na(ids) & nzchar(ids)]
    return(unique(ids))
  }
  
  character(0)
}

get_direct_children <- function(codes, parent_code) {
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  
  parent_code <- as.character(parent_code)
  
  if (!"id" %in% names(codes) || !"children" %in% names(codes)) {
    stop("The codelist must contain columns 'id' and 'children'.")
  }
  
  row <- codes[id == parent_code]
  
  if (nrow(row) == 0) {
    return(character(0))
  }
  
  extract_child_ids(row$children[[1]])
}

get_descendants <- function(codes, parent_code) {
  
  # Make sure the codelist is a data.table.
  # This allows us to use data.table syntax inside get_direct_children().
  codes <- as.data.table(codes)
  
  # Make sure the parent code is treated as character,
  # because codelist ids are usually character codes.
  parent_code <- as.character(parent_code)
  
  # This will store all descendants found under the parent code.
  # It starts empty.
  out <- character(0)
  
  # This keeps track of codes that have already been processed.
  # It avoids processing the same code more than once.
  visited <- character(0)
  
  # Start with the direct children of the selected parent code.
  # For example, if parent_code = "ISSCAAP",
  # this may return c("1501", "1502", ..., "1509").
  queue <- get_direct_children(codes, parent_code)
  
  # Continue as long as there are still codes to process.
  while (length(queue) > 0) {
    
    # Take the first code from the queue.
    current <- queue[1]
    
    # Remove that code from the queue,
    # because we are processing it now.
    queue <- queue[-1]
    
    # If this code has already been processed before,
    # skip it and move to the next one.
    if (current %in% visited) {
      next
    }
    
    # Mark the current code as processed.
    visited <- c(visited, current)
    
    # Add the current code to the list of descendants.
    out <- c(out, current)
    
    # Look for children of the current code.
    # If the current code has children, they are added to the queue.
    # If it has no children, get_direct_children() returns character(0),
    # so nothing is added.
    queue <- c(queue, get_direct_children(codes, current))
  }
  
  # Return all descendants without duplicates.
  unique(out)
}

# Create named code choices for display, using the best available label column.
make_code_choices <- function(codes) {
  codes <- as.data.table(codes)
  # Return an empty vector if the codelist does not contain an ID column.
  if (!"id" %in% names(codes)) {
    return(character(0))
  }
  # Use the first available descriptive label column.
  label_col <- intersect(c("label_en", "label", "description"), names(codes))[1]
  # Combine each code with its label when available; otherwise use the code alone.
  if (!is.na(label_col)) {
    labels <- paste0(codes$id, " - ", codes[[label_col]])
  } else {
    labels <- codes$id
  }
  # Return the codes as values, with the display labels as names.
  stats::setNames(as.character(codes$id), labels)
}



# Create filter choices using only values that are present in the loaded dataset.
# When codelist information is available, use descriptive labels and units for display.
make_filter_choices_from_data <- function(data,
                                          column_name,
                                          codes = NULL) {
  dt <- data
  # Return no choices if the requested column is not available.
  if (is.null(column_name) || !column_name %in% names(dt)) {
    return(character(0))
  }
  
  # Keep only distinct, non-missing values that actually occur in the dataset.
  values <- sort(unique(as.character(dt[[column_name]])))
  values <- values[!is.na(values) & nzchar(values)]
  # Return no choices if the column contains no usable values.
  if (length(values) == 0) {
    return(character(0))
  }
  # By default, use the values themselves as both stored values and display labels.
  out <- values
  names(out) <- values
  # If a codelist is available, enrich the display labels.
  if (!is.null(codes)) {
    codes <- as.data.table(codes)
    codes[, id := as.character(id)]
    # Use the first available descriptive label column.
    label_col <- intersect(
      c("label_en", "label", "description"),
      names(codes)
    )[1]
    # Build user-facing labels when a descriptive column is available.
    if (!is.na(label_col)) {
      label_map <- codes[
        id %in% values,
        .(
          id,
          label_text = as.character(get(label_col)),
          unit_text = if ("unit" %in% names(codes)) {
            as.character(unit)
          } else {
            ""
          }
        )
      ]
      # Fall back to the code itself when the descriptive label is missing.
      label_map[
        is.na(label_text) | !nzchar(label_text),
        label_text := id
      ]
      # Replace missing units with an empty string.
      label_map[
        is.na(unit_text),
        unit_text := ""
      ]
      
      label_map[, unit_text := trimws(unit_text)]
      # Display code, label and unit when a unit is available.
      label_map[
        nzchar(unit_text),
        label := paste0(id, " - ", label_text, " [", unit_text, "]")
      ]
      # Display only code and label when no unit is available.
      label_map[
        !nzchar(unit_text),
        label := paste0(id, " - ", label_text)
      ]
      # Match the dataset values to their corresponding codelist labels.
      matched <- match(values, label_map$id)
      has_match <- !is.na(matched)
      # Replace the default display names only for values found in the codelist.
      names(out)[has_match] <- label_map$label[matched[has_match]]
    }
  }
  # Return named choices: displayed labels as names and codes as values.
  out
}

# Create display choices from the full codelist.
# Optionally restrict the choices to aggregate codes that have children.
make_full_codelist_choices <- function(codes,
                                       aggregate_only = FALSE) {
  codes <- as.data.table(codes)
  # Return no choices if the codelist does not contain an ID column.
  if (!"id" %in% names(codes)) {
    return(character(0))
  }
  # Standardize codelist IDs as character values.
  codes[, id := as.character(id)]
  # If requested, keep only aggregate codes that have child categories.
  if (isTRUE(aggregate_only)) {
    # Apply the restriction only when hierarchy information is available.
    if ("children" %in% names(codes)) {
      codes <- codes[
        vapply(children, has_children, logical(1))
      ]
    }
  }
  # Use the first available descriptive label column.
  label_col <- intersect(
    c("label_en", "label", "description"),
    names(codes)
  )[1]
  # Combine each code with its label when available;
  # otherwise use the code itself as the display label.
  if (!is.na(label_col)) {
    labels <- paste0(codes$id, " - ", codes[[label_col]])
  } else {
    labels <- codes$id
  }
  # Return named choices: display labels as names and codes as values.
  stats::setNames(as.character(codes$id), labels)
}


# Expand selected hierarchy codes to include each selected code
# together with all of its descendants.
expand_selected_codes_with_descendants <- function(selected_codes,
                                                   codes) {
  # Return an empty vector when no codes have been selected.
  if (is.null(selected_codes) || length(selected_codes) == 0) {
    return(character(0))
  }
  # Standardize the codelist and code IDs.
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  # Convert selected codes to unique, non-missing character values.
  selected_codes <- unique(as.character(selected_codes))
  selected_codes <- selected_codes[!is.na(selected_codes) & nzchar(selected_codes)]
  # Return an empty vector if no valid selected codes remain.
  if (length(selected_codes) == 0) {
    return(character(0))
  }
  # For each selected code, include the code itself
  # and all descendants below it in the hierarchy.
  expanded <- unique(unlist(
    lapply(
      selected_codes,
      function(code_i) {
        c(
          code_i,
          get_descendants(
            codes = codes,
            parent_code = code_i
          )
        )
      }
    ),
    use.names = FALSE
  ))
  # Remove any missing or empty codes from the expanded selection.
  expanded <- expanded[!is.na(expanded) & nzchar(expanded)]
  # Return the final set of unique selected and descendant codes.
  unique(expanded)
}

# Check whether values represent a logical TRUE condition.
is_true_value <- function(x) {
  # Convert input values to character for consistent comparison.
  x <- as.character(x)
  # Treat "true", "t", "1", and "yes" as TRUE, ignoring capitalization.
  # Missing values are always treated as FALSE.
  !is.na(x) & tolower(x) %in% c("true", "t", "1", "yes")
}

# Create a display label for a hierarchy tree node using its code
# and, when available, its descriptive label.
make_tree_label <- function(code_id, label_text = NULL) {
  # Standardize the code as character.
  code_id <- as.character(code_id)
  # If no descriptive label is available, display only the code in brackets.
  if (is.null(label_text) || is.na(label_text) || !nzchar(label_text)) {
    return(paste0("[", code_id, "]"))
  }
  # Otherwise display both the code and the descriptive label.
  paste0("[", code_id, "] ", label_text)
}


extract_code_from_tree_label <- function(x) {
  # shinyTree selections can arrive as a character vector, a named vector,
  # or a nested list. We collect both values and names because sometimes
  # the useful label is stored as the name, not as the value.
  values <- unlist(
    x,
    recursive = TRUE,
    use.names = FALSE
  )
  
  names_with_path <- names(
    unlist(
      x,
      recursive = TRUE,
      use.names = TRUE
    )
  )
  
  x <- unique(c(values, names_with_path))
  x <- as.character(x)
  x <- trimws(x)
  x <- x[!is.na(x) & nzchar(x)]
  
  # Keep only the actual node label if shinyTree gives a path-like name.
  # Example: "[ISSCAAP] ISSCAAP.[1501] Freshwater fishes"
  # We want the last bracketed code if there are multiple.
  out <- vapply(
    x,
    function(label_i) {
      matches <- regmatches(
        label_i,
        gregexpr("\\[[^\\]]+\\]", label_i)
      )[[1]]
      
      if (length(matches) == 0) {
        return(label_i)
      }
      
      last_match <- matches[length(matches)]
      sub("^\\[([^\\]]+)\\]$", "\\1", last_match)
    },
    character(1)
  )
  
  out <- trimws(out)
  out <- out[!is.na(out) & nzchar(out)]
  
  unique(out)
}

# Return the first available descriptive label column from the codelist.
get_codelist_label_column <- function(codes) {
  intersect(
    c("label_en", "label", "description"),
    names(codes)
  )[1]
}

# Create display labels for hierarchy-tree nodes using code IDs
# and the best available descriptive labels.
make_tree_display_labels <- function(codes) {
  # Standardize the codelist and code IDs.
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  # Identify the first available descriptive label column.
  label_col <- get_codelist_label_column(codes)
  # If no descriptive label column exists, use the code itself.
  if (is.na(label_col)) {
    label_values <- codes$id
  } else {
    
    # Otherwise use the descriptive labels from the codelist.
    label_values <- as.character(codes[[label_col]])
    # Replace missing or empty labels with the corresponding code ID.
    label_values[
      is.na(label_values) | !nzchar(label_values)
    ] <- codes$id[
      is.na(label_values) | !nzchar(label_values)
    ]
  }
  # Use the regular code ID as the default displayed code.
  display_code <- codes$id
  # If an alternative display ID is available, use it where provided.
  if ("display_id" %in% names(codes)) {
    display_code <- ifelse(
      !is.na(codes$display_id) & nzchar(as.character(codes$display_id)),
      as.character(codes$display_id),
      display_code
    )
  }
  # Combine the displayed code and descriptive label.
  labels <- paste0(
    display_code,
    " - ",
    label_values
  )
  # Some synthetic hierarchy nodes should display only their descriptive label.
  if (
    "synthetic_label_only" %in%
    names(codes)
  ) {
    # Identify the rows marked as label-only synthetic nodes.
    synthetic_rows <- is_true_value(
      codes$synthetic_label_only
    )
    # Remove the code prefix from those synthetic-node labels.
    labels[synthetic_rows] <-
      label_values[synthetic_rows]
  }
  # Return display labels named by the underlying code IDs.
  stats::setNames(
    labels,
    codes$id
  )
}

# Convert selected hierarchy-tree display labels back to their underlying code IDs.
# For flat dimensions without a codelist, return the selected values directly.
tree_display_labels_to_codes <- function(selected_labels,
                                         codes = NULL) {
  if (is.null(selected_labels) || length(selected_labels) == 0) {
    return(character(0))
  }
  
  selected_labels <- unlist(
    selected_labels,
    recursive = TRUE,
    use.names = FALSE
  )
  
  selected_labels <- as.character(selected_labels)
  selected_labels <- trimws(selected_labels)
  selected_labels <- selected_labels[!is.na(selected_labels) & nzchar(selected_labels)]
  
  if (length(selected_labels) == 0) {
    return(character(0))
  }
  
  # For flat dimensions, such as flags, there is no codelist.
  # In that case the selected labels are already the values to filter by.
  if (is.null(codes)) {
    return(unique(selected_labels))
  }
  
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  
  display_labels <- make_tree_display_labels(codes)
  
  # lookup: "1501 - Freshwater fishes" -> "1501"
  label_to_id <- stats::setNames(
    names(display_labels),
    unname(display_labels)
  )
  
  selected_codes <- unname(label_to_id[selected_labels])
  
  # Safety fallback:
  # if a selected value is already just "1501", keep "1501".
  missing <- is.na(selected_codes)
  
  if (any(missing)) {
    fallback <- selected_labels[missing]
    
    fallback <- sub(
      "^([^ ]+)\\s+-\\s+.*$",
      "\\1",
      fallback
    )
    
    fallback <- sub(
      "^\\[([^\\]]+)\\].*$",
      "\\1",
      fallback
    )
    
    selected_codes[missing] <- fallback
  }
  
  selected_codes <- as.character(selected_codes)
  selected_codes <- trimws(selected_codes)
  selected_codes <- selected_codes[!is.na(selected_codes) & nzchar(selected_codes)]
  
  unique(selected_codes)
}

# Identify the root nodes of an SWS codelist hierarchy.
# Virtual roots are preferred when available; otherwise infer roots from parent-child relationships.
get_sws_tree_root_codes <- function(codes) {
  # Standardize the codelist and code IDs.
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  
  # If no hierarchy information is available, treat all codes as possible roots.
  if (!"children" %in% names(codes)) {
    return(codes$id)
  }
  
  # Keep only codes that have at least one child.
  parent_codes <- codes[
    vapply(children, has_children, logical(1)),
    id
  ]
  
  # If no parent codes exist, return all codes.
  if (length(parent_codes) == 0) {
    return(codes$id)
  }
  
  # First preference: use SWS virtual root nodes when available.
  # Example in fisheriesAsfis:
  # ISSCAAP and TAXONOMIC have virtual == TRUE.
  if ("virtual" %in% names(codes)) {
    virtual_roots <- codes[
      id %in% parent_codes &
        is_true_value(virtual),
      id
    ]
    
    # Return virtual roots when at least one is available.
    if (length(virtual_roots) > 0) {
      return(virtual_roots)
    }
  }
  
  # Fallback: roots are parent codes that are not children of any other code.
  all_children <- unique(unlist(
    lapply(codes$children, extract_child_ids),
    recursive = TRUE,
    use.names = FALSE
  ))
  
  all_children <- as.character(all_children)
  all_children <- all_children[!is.na(all_children) & nzchar(all_children)]
  
  root_codes <- setdiff(parent_codes, all_children)
  
  if (length(root_codes) == 0) {
    root_codes <- parent_codes
  }
  
  root_codes
}

# Build a nested hierarchy tree from an SWS codelist using its parent–child relationships.
build_sws_codelist_tree <- function(codes,
                                    max_depth = 50) {
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  
  label_col <- get_codelist_label_column(codes)
  
  display_labels <- make_tree_display_labels(codes)
  
  label_for_code <- function(code_id) {
    code_id <- as.character(code_id)
    
    out <- unname(display_labels[code_id])
    
    if (length(out) == 0 || is.na(out) || !nzchar(out)) {
      return(code_id)
    }
    
    out
  }
  
  
  order_codes <- function(code_vector) {
    code_vector <- as.character(code_vector)
    code_vector <- code_vector[!is.na(code_vector) & nzchar(code_vector)]
    code_vector <- code_vector[code_vector %in% codes$id]
    
    if (length(code_vector) == 0) {
      return(character(0))
    }
    
    if (!"order" %in% names(codes)) {
      return(code_vector)
    }
    
    tmp <- codes[
      id %in% code_vector,
      .(
        id,
        order_tmp = suppressWarnings(as.numeric(order))
      )
    ]
    
    tmp <- tmp[order(order_tmp, id)]
    
    tmp$id
  }
  
  make_node <- function(code_id,
                        depth = 1,
                        visited = character(0)) {
    code_id <- as.character(code_id)
    
    if (code_id %in% visited) {
      return("")
    }
    
    if (depth > max_depth) {
      return("")
    }
    
    children <- get_direct_children(
      codes = codes,
      parent_code = code_id
    )
    
    children <- order_codes(children)
    
    if (length(children) == 0) {
      return("")
    }
    
    child_nodes <- lapply(
      children,
      function(child_id) {
        make_node(
          code_id = child_id,
          depth = depth + 1,
          visited = c(visited, code_id)
        )
      }
    )
    
    child_labels <- vapply(
      children,
      label_for_code,
      character(1),
      USE.NAMES = FALSE
    )
    
    child_labels[is.na(child_labels) | !nzchar(child_labels)] <- children[
      is.na(child_labels) | !nzchar(child_labels)
    ]
    
    child_nodes <- stats::setNames(
      child_nodes,
      child_labels
    )
    
    child_nodes
  }
  
  root_codes <- get_sws_tree_root_codes(codes)
  root_codes <- order_codes(root_codes)
  
  if (length(root_codes) == 0) {
    return(list())
  }
  
  root_nodes <- lapply(
    root_codes,
    function(root_code) {
      make_node(root_code)
    }
  )
  
  root_labels <- vapply(
    root_codes,
    label_for_code,
    character(1),
    USE.NAMES = FALSE
  )
  
  root_labels[is.na(root_labels) | !nzchar(root_labels)] <- root_codes[
    is.na(root_labels) | !nzchar(root_labels)
  ]
  
  root_nodes <- stats::setNames(
    root_nodes,
    root_labels
  )
  
  root_nodes
}

#Return the number of hierarchy levels available in a codelist tree.
get_codelist_tree_display_depth <- function(tree_dt) {
  id_cols <- get_tree_id_cols(tree_dt)
  
  if (length(id_cols) == 0L) {
    return(1L)
  }
  
  # Display every hierarchy level available in getCodelistTree().
  as.integer(length(id_cols))
}


#Identifies and orders the columns representing hierarchy levels in a codelist tree.
get_tree_id_cols <- function(tree_dt) {
  id_cols <- grep("^level_[0-9]+_id$", names(tree_dt), value = TRUE)
  id_cols[order(as.integer(sub("^level_([0-9]+)_id$", "\\1", id_cols)))]
}

#Returns the immediate children of a selected parent code from the flattened codelist tree.
get_direct_children_from_codelist_tree <- function(tree_dt, parent_code) {
  tree_dt <- as.data.table(tree_dt)
  id_cols <- get_tree_id_cols(tree_dt)
  
  parent_code <- as.character(parent_code)
  out <- character(0)
  
  for (i in seq_along(id_cols)) {
    if (i == length(id_cols)) {
      next
    }
    
    rows_i <- tree_dt[as.character(get(id_cols[i])) == parent_code]
    
    if (nrow(rows_i) == 0) {
      next
    }
    
    next_col <- id_cols[i + 1]
    vals <- as.character(rows_i[[next_col]])
    vals <- vals[!is.na(vals) & nzchar(vals)]
    
    out <- c(out, vals)
  }
  
  unique(out)
}


# Fishing-area paths can contain the same code at several hierarchy levels.
# For a selected node, use only its shallowest occurrence in the flattened
# tree so that descendants are not mistaken for direct children.
get_direct_children_from_shallowest_tree_level <- function(
    tree_dt,
    parent_code
) {
  tree_dt <- as.data.table(tree_dt)
  id_cols <- get_tree_id_cols(tree_dt)
  
  parent_code <- trimws(as.character(parent_code))[1L]
  
  if (
    length(id_cols) < 2L ||
    is.na(parent_code) ||
    !nzchar(parent_code)
  ) {
    return(character(0))
  }
  
  parent_levels <- which(
    vapply(
      id_cols,
      function(column_i) {
        values_i <- trimws(as.character(tree_dt[[column_i]]))
        any(
          !is.na(values_i) &
            nzchar(values_i) &
            values_i == parent_code
        )
      },
      logical(1)
    )
  )
  
  parent_levels <- parent_levels[
    parent_levels < length(id_cols)
  ]
  
  if (length(parent_levels) == 0L) {
    return(character(0))
  }
  
  parent_level <- min(parent_levels)
  parent_col <- id_cols[parent_level]
  child_col <- id_cols[parent_level + 1L]
  
  rows_i <- tree_dt[
    trimws(as.character(get(parent_col))) == parent_code
  ]
  
  children <- trimws(
    as.character(
      rows_i[[child_col]]
    )
  )
  
  children <- children[
    !is.na(children) &
      nzchar(children) &
      children != parent_code
  ]
  
  unique(children)
}

#Returns all descendants below a selected code in the hierarchy.
get_descendants_from_codelist_tree <- function(tree_dt, parent_code) {
  tree_dt <- as.data.table(tree_dt)
  id_cols <- get_tree_id_cols(tree_dt)
  
  parent_code <- as.character(parent_code)
  out <- character(0)
  
  for (i in seq_along(id_cols)) {
    rows_i <- tree_dt[as.character(get(id_cols[i])) == parent_code]
    
    if (nrow(rows_i) == 0) {
      next
    }
    
    if (i < length(id_cols)) {
      later_cols <- id_cols[(i + 1):length(id_cols)]
      vals <- unlist(rows_i[, ..later_cols], use.names = FALSE)
      vals <- as.character(vals)
      vals <- vals[!is.na(vals) & nzchar(vals)]
      out <- c(out, vals)
    }
  }
  
  out <- unique(out)
  setdiff(out, parent_code)
}

#Returns all ancestor codes above a selected code in the hierarchy.
get_ancestors_from_codelist_tree <- function(tree_dt, code) {
  tree_dt <- as.data.table(tree_dt)
  id_cols <- get_tree_id_cols(tree_dt)
  
  code <- as.character(code)
  out <- character(0)
  
  for (i in seq_along(id_cols)) {
    rows_i <- tree_dt[as.character(get(id_cols[i])) == code]
    
    if (nrow(rows_i) == 0) {
      next
    }
    
    if (i > 1) {
      earlier_cols <- id_cols[1:(i - 1)]
      vals <- unlist(rows_i[, ..earlier_cols], use.names = FALSE)
      vals <- as.character(vals)
      vals <- vals[!is.na(vals) & nzchar(vals)]
      out <- c(out, vals)
    }
  }
  
  unique(out)
}

#Keeps only the highest selected nodes when both a parent and one of its descendants are selected.
keep_top_selected_codes_from_codelist_tree <- function(selected_codes, tree_dt) {
  selected_codes <- unique(as.character(selected_codes))
  selected_codes <- selected_codes[!is.na(selected_codes) & nzchar(selected_codes)]
  
  if (length(selected_codes) <= 1) {
    return(selected_codes)
  }
  
  selected_set <- selected_codes
  
  keep <- vapply(
    selected_codes,
    function(code_i) {
      ancestors_i <- get_ancestors_from_codelist_tree(
        tree_dt = tree_dt,
        code = code_i
      )
      
      !any(ancestors_i %in% setdiff(selected_set, code_i))
    },
    logical(1)
  )
  
  selected_codes[keep]
}

#Returns a selected hierarchy node together with all codes belonging to its branch, 
#with special handling for fishing-area hierarchies.
get_hierarchy_branch_codes <- function(
    tree_dt,
    root_code,
    codelist_id = NULL
) {
  
  tree_dt <- as.data.table(
    tree_dt
  )
  
  root_code <- clean_code_vector(
    root_code
  )
  
  if (length(root_code) == 0L) {
    return(character(0))
  }
  
  root_code <- root_code[1L]
  
  # ------------------------------------------------------------
  # Fishing areas can contain the same code at multiple levels.
  # Use the shallowest occurrence, consistently with the existing
  # direct-child fishing-area logic.
  # ------------------------------------------------------------
  if (identical(
    codelist_id,
    "fisheriesCatchArea"
  )) {
    
    id_cols <- get_tree_id_cols(
      tree_dt
    )
    
    if (length(id_cols) == 0L) {
      return(root_code)
    }
    
    root_levels <- which(
      vapply(
        id_cols,
        function(column_i) {
          
          values_i <- trimws(
            as.character(
              tree_dt[[column_i]]
            )
          )
          
          any(
            !is.na(values_i) &
              nzchar(values_i) &
              values_i == root_code
          )
        },
        logical(1)
      )
    )
    
    if (length(root_levels) == 0L) {
      return(root_code)
    }
    
    root_level <- min(
      root_levels
    )
    
    root_column <- id_cols[
      root_level
    ]
    
    branch_rows <- tree_dt[
      trimws(
        as.character(
          get(root_column)
        )
      ) == root_code
    ]
    
    branch_columns <- id_cols[
      root_level:length(id_cols)
    ]
    
    branch_codes <- unlist(
      branch_rows[
        ,
        ..branch_columns
      ],
      recursive = TRUE,
      use.names = FALSE
    )
    
  } else {
    
    # ASFIS, geographical area and production source.
    branch_codes <- c(
      root_code,
      
      get_descendants_from_codelist_tree(
        tree_dt = tree_dt,
        parent_code = root_code
      )
    )
  }
  
  clean_code_vector(
    branch_codes
  )
}

#Keeps only the most specific selected nodes when both a parent and one or more descendants are selected.
keep_most_specific_selected_codes_from_codelist_tree <-
  function(
    selected_codes,
    tree_dt,
    codelist_id = NULL
  ) {
    
    selected_codes <- clean_code_vector(
      selected_codes
    )
    
    if (length(selected_codes) <= 1L) {
      return(selected_codes)
    }
    
    selected_set <- selected_codes
    
    keep <- vapply(
      selected_codes,
      function(code_i) {
        
        descendants_i <- setdiff(
          get_hierarchy_branch_codes(
            tree_dt = tree_dt,
            root_code = code_i,
            codelist_id = codelist_id
          ),
          code_i
        )
        
        # Remove code_i when another selected code is
        # more specific and belongs to its branch.
        !any(
          descendants_i %in%
            setdiff(
              selected_set,
              code_i
            )
        )
      },
      logical(1)
    )
    
    selected_codes[keep]
  }

#Identifies the root nodes of a flattened SWS codelist tree, preferring virtual roots when available.
get_sws_tree_root_codes_from_codelist_tree <- function(tree_dt, codes = NULL) {
  tree_dt <- as.data.table(tree_dt)
  id_cols <- get_tree_id_cols(tree_dt)
  
  parent_codes <- character(0)
  
  for (i in seq_along(id_cols)) {
    if (i == length(id_cols)) {
      next
    }
    
    rows_i <- tree_dt[
      !is.na(get(id_cols[i])) &
        nzchar(as.character(get(id_cols[i]))) &
        !is.na(get(id_cols[i + 1])) &
        nzchar(as.character(get(id_cols[i + 1])))
    ]
    
    parent_codes <- c(parent_codes, as.character(rows_i[[id_cols[i]]]))
  }
  
  parent_codes <- unique(parent_codes)
  
  if (length(parent_codes) == 0) {
    return(unique(as.character(tree_dt[[id_cols[1]]])))
  }
  
  all_children <- character(0)
  
  if (length(id_cols) >= 2) {
    for (j in 2:length(id_cols)) {
      vals <- as.character(tree_dt[[id_cols[j]]])
      vals <- vals[!is.na(vals) & nzchar(vals)]
      all_children <- c(all_children, vals)
    }
  }
  
  all_children <- unique(all_children)
  
  root_codes <- setdiff(parent_codes, all_children)
  
  if (!is.null(codes)) {
    codes <- as.data.table(codes)
    codes[, id := as.character(id)]
    
    if ("virtual" %in% names(codes)) {
      virtual_roots <- codes[
        id %in% parent_codes &
          is_true_value(virtual),
        id
      ]
      
      if (length(virtual_roots) > 0) {
        root_codes <- virtual_roots
      }
    }
    
    if ("order" %in% names(codes)) {
      tmp <- codes[
        id %in% root_codes,
        .(
          id,
          order_tmp = suppressWarnings(as.numeric(order))
        )
      ]
      
      tmp <- tmp[order(order_tmp, id)]
      root_codes <- tmp$id
    }
  }
  
  root_codes
}

# Build the nested hierarchy displayed in the app from the flattened
# SWS codelist tree, using a precomputed parent-child lookup.
build_sws_codelist_tree_from_codelist_tree <- function(
    tree_dt,
    codes,
    max_depth = 50,
    root_codes = NULL
) {
  
  tree_dt <- as.data.table(tree_dt)
  codes <- as.data.table(codes)
  
  codes[, id := as.character(id)]
  
  id_cols <- get_tree_id_cols(tree_dt)
  
  display_labels <- make_tree_display_labels(codes)
  
  # Prepare code IDs and ordering once for repeated tree-node ordering.
  code_ids <- as.character(codes$id)
  
  code_order_values <- NULL
  
  if ("order" %in% names(codes)) {
    code_order_values <- suppressWarnings(
      as.numeric(codes$order)
    )
  }
  
  
  label_for_code <- function(code_id) {
    
    code_id <- as.character(code_id)
    
    out <- unname(
      display_labels[code_id]
    )
    
    if (
      length(out) == 0 ||
      is.na(out) ||
      !nzchar(out)
    ) {
      return(code_id)
    }
    
    out
  }
  
  
  order_codes <- function(code_vector) {
    
    code_vector <- as.character(code_vector)
    
    code_vector <- code_vector[
      !is.na(code_vector) &
        nzchar(code_vector)
    ]
    
    if (length(code_vector) == 0L) {
      return(character(0))
    }
    
    if (is.null(code_order_values)) {
      return(code_vector)
    }
    
    matched <- match(
      code_vector,
      code_ids
    )
    
    found <- !is.na(matched)
    
    found_codes <- code_vector[
      found
    ]
    
    found_order <- code_order_values[
      matched[found]
    ]
    
    if (length(found_codes) > 0L) {
      
      found_codes <- found_codes[
        base::order(
          found_order,
          found_codes
        )
      ]
    }
    
    c(
      found_codes,
      setdiff(
        code_vector,
        found_codes
      )
    )
  }
  
  
  # Build all direct parent-child relationships once.
  parent_child_pairs <- data.table(
    parent = character(0),
    child = character(0)
  )
  
  if (length(id_cols) >= 2L) {
    
    parent_child_pairs <- rbindlist(
      lapply(
        seq_len(length(id_cols) - 1L),
        function(i) {
          
          parent_values <- as.character(
            tree_dt[[id_cols[i]]]
          )
          
          child_values <- as.character(
            tree_dt[[id_cols[i + 1L]]]
          )
          
          keep <- (
            !is.na(parent_values) &
              nzchar(parent_values) &
              !is.na(child_values) &
              nzchar(child_values)
          )
          
          data.table(
            parent = parent_values[keep],
            child = child_values[keep]
          )
        }
      ),
      use.names = TRUE,
      fill = TRUE
    )
    
    parent_child_pairs <- unique(
      parent_child_pairs
    )
  }
  
  
  # Store the children associated with each parent.
  child_lookup <- split(
    parent_child_pairs$child,
    parent_child_pairs$parent
  )
  
  
  get_children <- function(code_id) {
    
    code_id <- as.character(code_id)
    
    children <- child_lookup[[code_id]]
    
    if (is.null(children)) {
      return(character(0))
    }
    
    unique(
      as.character(children)
    )
  }
  
  
  make_node <- function(
    code_id,
    depth = 1L,
    visited = character(0)
  ) {
    
    code_id <- as.character(code_id)
    
    if (code_id %in% visited) {
      return("")
    }
    
    if (depth > max_depth) {
      return("")
    }
    
    children <- get_children(
      code_id
    )
    
    children <- order_codes(
      children
    )
    
    if (length(children) == 0L) {
      return("")
    }
    
    child_nodes <- lapply(
      children,
      function(child_id) {
        
        make_node(
          code_id = child_id,
          depth = depth + 1L,
          visited = c(
            visited,
            code_id
          )
        )
      }
    )
    
    child_labels <- vapply(
      children,
      label_for_code,
      character(1),
      USE.NAMES = FALSE
    )
    
    stats::setNames(
      child_nodes,
      child_labels
    )
  }
  
  
  if (
    is.null(root_codes) ||
    length(root_codes) == 0L
  ) {
    
    root_codes <-
      get_sws_tree_root_codes_from_codelist_tree(
        tree_dt = tree_dt,
        codes = codes
      )
    
  } else {
    
    root_codes <- unique(
      trimws(
        as.character(root_codes)
      )
    )
    
    root_codes <- root_codes[
      !is.na(root_codes) &
        nzchar(root_codes) &
        root_codes %in% codes$id
    ]
  }
  
  
  root_codes <- order_codes(
    root_codes
  )
  
  if (length(root_codes) == 0L) {
    return(list())
  }
  
  
  root_nodes <- lapply(
    root_codes,
    function(root_code) {
      make_node(root_code)
    }
  )
  
  
  root_labels <- vapply(
    root_codes,
    label_for_code,
    character(1),
    USE.NAMES = FALSE
  )
  
  
  stats::setNames(
    root_nodes,
    root_labels
  )
}




#Creates a child-to-parent lookup table from the codelist hierarchy for efficient ancestor searches.
make_parent_lookup <- function(codes) {
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  
  out <- rbindlist(
    lapply(seq_len(nrow(codes)), function(i) {
      children_i <- extract_child_ids(codes$children[[i]])
      
      if (length(children_i) == 0) {
        return(NULL)
      }
      
      data.table(
        parent = codes$id[i],
        child = as.character(children_i)
      )
    }),
    fill = TRUE
  )
  
  if (is.null(out) || nrow(out) == 0) {
    return(data.table(parent = character(0), child = character(0)))
  }
  
  out <- unique(out[!is.na(child) & nzchar(child)])
  setkey(out, child)
  
  out
}

#Retrieve all ancestor codes of a selected code using the parent lookup table.
get_ancestors_fast <- function(code, parent_lookup) {
  code <- as.character(code)
  
  out <- character(0)
  queue <- code
  visited <- character(0)
  
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    
    if (current %in% visited) {
      next
    }
    
    visited <- c(visited, current)
    
    parents <- parent_lookup[.(current), parent]
    parents <- parents[!is.na(parents) & nzchar(parents)]
    
    if (length(parents) > 0) {
      out <- c(out, parents)
      queue <- c(queue, parents)
    }
  }
  
  unique(out)
}

#Keep only the highest selected hierarchy nodes when both a parent and one of its descendants are selected.
keep_top_selected_codes <- function(selected_codes, codes) {
  selected_codes <- unique(as.character(selected_codes))
  selected_codes <- selected_codes[!is.na(selected_codes) & nzchar(selected_codes)]
  
  if (length(selected_codes) <= 1) {
    return(selected_codes)
  }
  
  parent_lookup <- make_parent_lookup(codes)
  
  if (nrow(parent_lookup) == 0) {
    return(selected_codes)
  }
  
  selected_set <- selected_codes
  
  keep <- vapply(
    selected_codes,
    function(code_i) {
      ancestors_i <- get_ancestors_fast(
        code = code_i,
        parent_lookup = parent_lookup
      )
      
      !any(ancestors_i %in% setdiff(selected_set, code_i))
    },
    logical(1)
  )
  
  selected_codes[keep]
}

#Read the selected tree nodes, convert them to codes, 
#apply the requested selection rule, and optionally include descendants.
get_selected_codes_from_tree <- function(
    tree_input,
    codes = NULL,
    tree_dt = NULL,
    expand_descendants = TRUE,
    selection_rule = c(
      "top",
      "most_specific",
      "none"
    ),
    codelist_id = NULL
) {
  
  selection_rule <- match.arg(
    selection_rule
  )
  
  selected_labels <- shinyTree::get_selected(
    tree_input,
    format = "names"
  )
  
  if (length(selected_labels) == 0L) {
    return(character(0))
  }
  
  selected_codes <- tree_display_labels_to_codes(
    selected_labels = selected_labels,
    codes = codes
  )
  
  selected_codes <- clean_code_vector(
    selected_codes
  )
  
  if (length(selected_codes) == 0L) {
    return(character(0))
  }
  
  if (!is.null(tree_dt)) {
    
    if (identical(
      selection_rule,
      "top"
    )) {
      
      selected_codes <-
        keep_top_selected_codes_from_codelist_tree(
          selected_codes = selected_codes,
          tree_dt = tree_dt
        )
    }
    
    if (identical(
      selection_rule,
      "most_specific"
    )) {
      
      selected_codes <-
        keep_most_specific_selected_codes_from_codelist_tree(
          selected_codes = selected_codes,
          tree_dt = tree_dt,
          codelist_id = codelist_id
        )
    }
  }
  
  if (
    !is.null(tree_dt) &&
    isTRUE(expand_descendants)
  ) {
    
    expanded_codes <- unique(
      unlist(
        lapply(
          selected_codes,
          function(code_i) {
            
            get_hierarchy_branch_codes(
              tree_dt = tree_dt,
              root_code = code_i,
              codelist_id = codelist_id
            )
          }
        ),
        use.names = FALSE
      )
    )
    
    return(
      clean_code_vector(
        expanded_codes
      )
    )
  }
  
  selected_codes
}

#Retrieve the selected root and its descendants, keeping only codes that represent aggregate groups with children.
get_aggregate_codes_under_root <- function(codes, root_code) {
  codes <- as.data.table(codes)
  root_code <- as.character(root_code)
  
  # Get the root itself plus everything below it
  codes_under_root <- c(root_code, get_descendants(codes, root_code))
  
  # Keep only codes that are aggregate/group codes,
  # meaning codes that have children.
  aggregate_codes <- codes[
    id %in% codes_under_root &
      vapply(children, has_children, logical(1))
  ]
  
  aggregate_codes[]
}

#Create direct-child aggregation groups for the selected hierarchy parents 
#and assign categories outside those groups to the corresponding Other output.
build_classification_map_with_remainder <- function(
    tree_dt,
    selected_parents,
    filtered_raw_codes,
    codes = NULL,
    remainder_label = "Other filtered records",
    allow_remainder_only = FALSE
) {
  
  tree_dt <- as.data.table(
    tree_dt
  )
  
  selected_parents <- clean_code_vector(
    selected_parents
  )
  
  filtered_raw_codes <- clean_code_vector(
    filtered_raw_codes
  )
  
  if (length(selected_parents) == 0L) {
    stop(
      "No hierarchy parents were selected for direct-child aggregation."
    )
  }
  
  if (length(filtered_raw_codes) == 0L) {
    stop(
      "No filtered codes are available for classification aggregation."
    )
  }
  
  # Build one direct-child map for every selected parent.
  selected_parent_map <- rbindlist(
    lapply(
      selected_parents,
      function(parent_code_i) {
        
        map_i <- tryCatch(
          build_direct_child_aggregation_map(
            tree_dt = tree_dt,
            root_code = parent_code_i,
            codes = codes,
            filtered_raw_codes = filtered_raw_codes
          ),
          error = function(e) {
            stop(
              paste0(
                "Direct-child aggregation failed for hierarchy parent '",
                parent_code_i,
                "': ",
                e$message
              ),
              call. = FALSE
            )
          }
        )
        
        map_i[
          ,
          selected_parent_code := as.character(
            parent_code_i
          )
        ]
        
        map_i
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  # Keep only codes that are present after filtering.
  selected_parent_map <- selected_parent_map[
    raw_code %in% filtered_raw_codes
  ]
  
  if (
    nrow(selected_parent_map) == 0L &&
    !isTRUE(allow_remainder_only)
  ) {
    stop(
      paste0(
        "The selected hierarchy parents contain no records ",
        "in the filtered data."
      )
    )
  }
  
  # The same direct-child output must not belong to two
  # different selected parents.
  duplicated_output_groups <- unique(
    selected_parent_map[
      ,
      .(
        selected_parent_code,
        group_code
      )
    ]
  )[
    ,
    .(
      number_of_parents = uniqueN(
        selected_parent_code
      ),
      
      parent_codes = paste(
        sort(
          unique(
            selected_parent_code
          )
        ),
        collapse = ", "
      )
    ),
    by = group_code
  ][number_of_parents > 1L]
  
  if (nrow(duplicated_output_groups) > 0L) {
    stop(
      paste0(
        "At least one direct-child output belongs to more than one ",
        "selected hierarchy parent. Select non-overlapping parents."
      )
    )
  }
  
  # Every raw filtered code must belong to only one output.
  ambiguous_codes <- selected_parent_map[
    ,
    .(
      number_of_groups = uniqueN(
        group_code
      )
    ),
    by = raw_code
  ][number_of_groups > 1L]
  
  if (nrow(ambiguous_codes) > 0L) {
    stop(
      paste0(
        "Some filtered codes belong to direct-child outputs under more ",
        "than one selected hierarchy parent. Select non-overlapping parents."
      )
    )
  }
  
  classified_codes <- unique(
    as.character(
      selected_parent_map$raw_code
    )
  )
  
  selected_parent_map[
    ,
    selected_parent_code := NULL
  ]
  
  # Everything outside all selected parent classifications
  # is assigned to one dimension-specific Other output.
  remainder_codes <- setdiff(
    filtered_raw_codes,
    classified_codes
  )
  
  remainder_map <- data.table(
    raw_code = character(0),
    group_code = character(0)
  )
  
  if (length(remainder_codes) > 0L) {
    remainder_map <- data.table(
      raw_code = remainder_codes,
      group_code = as.character(
        remainder_label
      )
    )
  }
  
  aggregation_map <- rbindlist(
    list(
      selected_parent_map,
      remainder_map
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  unique(
    aggregation_map[
      !is.na(raw_code) &
        nzchar(raw_code) &
        !is.na(group_code) &
        nzchar(group_code)
    ],
    by = "raw_code"
  )
}




#Create the mapping that assigns each code under 
#a selected parent to one of its direct-child groups and check for overlaps.
build_direct_child_aggregation_map <- function(
    tree_dt,
    root_code,
    codes = NULL,
    filtered_raw_codes = NULL
) {
  
  tree_dt <- as.data.table(tree_dt)
  
  root_code <- trimws(
    as.character(root_code)
  )[1L]
  
  if (
    is.na(root_code) ||
    !nzchar(root_code)
  ) {
    stop("The aggregation root code is missing.")
  }
  
  if (!is.null(codes)) {
    
    # Fishing area: use only the shallowest occurrence of the
    # selected parent, then keep every child tied to that exact path.
    id_cols <- get_tree_id_cols(tree_dt)
    
    parent_levels <- which(
      vapply(
        id_cols,
        function(column_i) {
          values_i <- trimws(
            as.character(
              tree_dt[[column_i]]
            )
          )
          
          any(
            !is.na(values_i) &
              nzchar(values_i) &
              values_i == root_code
          )
        },
        logical(1)
      )
    )
    
    parent_levels <- parent_levels[
      parent_levels < length(id_cols)
    ]
    
    if (length(parent_levels) == 0L) {
      stop(
        paste0(
          "The selected aggregation classification '",
          root_code,
          "' has no direct child groups."
        )
      )
    }
    
    parent_level <- min(parent_levels)
    parent_col <- id_cols[parent_level]
    child_col <- id_cols[parent_level + 1L]
    
    parent_rows <- tree_dt[
      trimws(
        as.character(
          get(parent_col)
        )
      ) == root_code
    ]
    
    direct_groups <- trimws(
      as.character(
        parent_rows[[child_col]]
      )
    )
    
    direct_groups <- unique(
      direct_groups[
        !is.na(direct_groups) &
          nzchar(direct_groups) &
          direct_groups != root_code
      ]
    )
    
    get_group_members <- function(group_code_i) {
      
      group_rows <- parent_rows[
        trimws(
          as.character(
            get(child_col)
          )
        ) == group_code_i
      ]
      
      member_cols <- id_cols[
        (parent_level + 1L):length(id_cols)
      ]
      
      member_codes <- unlist(
        group_rows[, ..member_cols],
        recursive = TRUE,
        use.names = FALSE
      )
      
      member_codes <- trimws(
        as.character(member_codes)
      )
      
      unique(
        member_codes[
          !is.na(member_codes) &
            nzchar(member_codes)
        ]
      )
    }
    
  } else {
    
    # Existing behaviour for ASFIS, geographical area
    # and production source.
    direct_groups <-
      get_direct_children_from_codelist_tree(
        tree_dt = tree_dt,
        parent_code = root_code
      )
    
    get_group_members <- function(group_code_i) {
      
      unique(
        c(
          group_code_i,
          get_descendants_from_codelist_tree(
            tree_dt = tree_dt,
            parent_code = group_code_i
          )
        )
      )
    }
  }
  
  direct_groups <- unique(
    trimws(
      as.character(direct_groups)
    )
  )
  
  direct_groups <- direct_groups[
    !is.na(direct_groups) &
      nzchar(direct_groups)
  ]
  
  if (length(direct_groups) == 0L) {
    stop(
      paste0(
        "The selected aggregation classification '",
        root_code,
        "' has no direct child groups."
      )
    )
  }
  
  aggregation_map <- rbindlist(
    lapply(
      direct_groups,
      function(group_code_i) {
        
        member_codes <- get_group_members(
          group_code_i
        )
        
        data.table(
          raw_code = as.character(member_codes),
          group_code = as.character(group_code_i)
        )
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  aggregation_map <- unique(
    aggregation_map[
      !is.na(raw_code) &
        nzchar(raw_code) &
        !is.na(group_code) &
        nzchar(group_code)
    ]
  )
  
  # Keep only codes that are actually present in the filtered dataset
  # before checking whether a code belongs to multiple output groups.
  if (!is.null(filtered_raw_codes)) {
    
    filtered_raw_codes <- clean_code_vector(
      filtered_raw_codes
    )
    
    aggregation_map <- aggregation_map[
      raw_code %in% filtered_raw_codes
    ]
  }
  
  ambiguous_codes <- aggregation_map[
    ,
    .(
      number_of_groups = uniqueN(
        group_code
      )
    ),
    by = raw_code
  ][
    number_of_groups > 1L
  ]
  
  if (nrow(ambiguous_codes) > 0L) {
    
    ambiguous_details <- aggregation_map[
      raw_code %in% ambiguous_codes$raw_code,
      .(
        direct_groups = paste(
          sort(unique(group_code)),
          collapse = " | "
        )
      ),
      by = raw_code
    ][order(raw_code)]
    
    print(
      list(
        aggregation_root = root_code,
        ambiguous_details = ambiguous_details
      )
    )
    
    stop(
      paste0(
        "Some codes belong to more than one direct child ",
        "of aggregation root '",
        root_code,
        "'. See the R console for the exact codes."
      )
    )
  }
  
  aggregation_map[]
}

#Create separate aggregation groups from the selected hierarchy groups, 
#including the codes contained in each selected branch.
build_selected_groups_aggregation_map <- function(
    tree_dt,
    selected_codes,
    codelist_id = NULL
) {
  
  tree_dt <- as.data.table(
    tree_dt
  )
  
  selected_codes <- clean_code_vector(
    selected_codes
  )
  
  selected_codes <-
    keep_top_selected_codes_from_codelist_tree(
      selected_codes = selected_codes,
      tree_dt = tree_dt
    )
  
  if (length(selected_codes) == 0L) {
    stop(
      "No filter groups were selected."
    )
  }
  
  aggregation_map <- rbindlist(
    lapply(
      selected_codes,
      function(group_code_i) {
        
        member_codes <- get_hierarchy_branch_codes(
          tree_dt = tree_dt,
          root_code = group_code_i,
          codelist_id = codelist_id
        )
        
        data.table(
          raw_code = as.character(
            member_codes
          ),
          group_code = as.character(
            group_code_i
          )
        )
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  aggregation_map <- unique(
    aggregation_map[
      !is.na(raw_code) &
        nzchar(raw_code) &
        !is.na(group_code) &
        nzchar(group_code)
    ]
  )
  
  ambiguous_codes <- aggregation_map[
    ,
    .(
      number_of_groups = uniqueN(
        group_code
      )
    ),
    by = raw_code
  ][number_of_groups > 1L]
  
  if (nrow(ambiguous_codes) > 0L) {
    stop(
      paste0(
        "Some selected groups overlap. ",
        "Do not select both a parent and one of its descendants."
      )
    )
  }
  
  aggregation_map[]
}

#Combine the selected hierarchy nodes and their branches into one custom aggregation group.
build_custom_aggregation_map <- function(
    tree_dt,
    selected_codes,
    aggregate_code,
    codelist_id = NULL
) {
  
  tree_dt <- as.data.table(
    tree_dt
  )
  
  selected_codes <- clean_code_vector(
    selected_codes
  )
  
  selected_codes <-
    keep_top_selected_codes_from_codelist_tree(
      selected_codes = selected_codes,
      tree_dt = tree_dt
    )
  
  if (length(selected_codes) == 0L) {
    stop(
      "No nodes were selected for the custom aggregation."
    )
  }
  
  member_codes <- unique(
    unlist(
      lapply(
        selected_codes,
        function(code_i) {
          
          get_hierarchy_branch_codes(
            tree_dt = tree_dt,
            root_code = code_i,
            codelist_id = codelist_id
          )
        }
      ),
      use.names = FALSE
    )
  )
  
  member_codes <- clean_code_vector(
    member_codes
  )
  
  data.table(
    raw_code = member_codes,
    group_code = as.character(
      aggregate_code
    )
  )
}

#Create outputs for the selected filter groups and assign all remaining filtered categories to the Other group.
build_selected_groups_map_with_remainder <- function(
    tree_dt,
    selected_codes,
    filtered_raw_codes,
    codelist_id = NULL,
    remainder_label = "Other filtered records"
) {
  
  filtered_raw_codes <- clean_code_vector(
    filtered_raw_codes
  )
  
  aggregation_map <-
    build_selected_groups_aggregation_map(
      tree_dt = tree_dt,
      selected_codes = selected_codes,
      codelist_id = codelist_id
    )
  
  aggregation_map <- aggregation_map[
    raw_code %in% filtered_raw_codes
  ]
  
  mapped_codes <- unique(
    as.character(
      aggregation_map$raw_code
    )
  )
  
  remainder_codes <- setdiff(
    filtered_raw_codes,
    mapped_codes
  )
  
  if (length(remainder_codes) > 0L) {
    
    aggregation_map <- rbindlist(
      list(
        aggregation_map,
        
        data.table(
          raw_code = remainder_codes,
          group_code = as.character(
            remainder_label
          )
        )
      ),
      use.names = TRUE,
      fill = TRUE
    )
  }
  
  unique(
    aggregation_map[
      !is.na(raw_code) &
        nzchar(raw_code) &
        !is.na(group_code) &
        nzchar(group_code)
    ],
    by = "raw_code"
  )
}

#Create the custom aggregation group and assign all remaining filtered categories to the Other group.
build_custom_map_with_remainder <- function(
    tree_dt,
    selected_codes,
    aggregate_code,
    filtered_raw_codes,
    codelist_id = NULL,
    remainder_label = "Other filtered records",
    allow_remainder_only = FALSE
) {
  
  filtered_raw_codes <- clean_code_vector(
    filtered_raw_codes
  )
  
  custom_map <- build_custom_aggregation_map(
    tree_dt = tree_dt,
    selected_codes = selected_codes,
    aggregate_code = aggregate_code,
    codelist_id = codelist_id
  )
  
  custom_map <- custom_map[
    raw_code %in% filtered_raw_codes
  ]
  
  if (
    nrow(custom_map) == 0L &&
    !isTRUE(allow_remainder_only)
  ) {
    stop(
      "None of the selected custom-aggregation nodes is present in the filtered data."
    )
  }
  
  custom_codes <- unique(
    as.character(
      custom_map$raw_code
    )
  )
  
  remainder_codes <- setdiff(
    filtered_raw_codes,
    custom_codes
  )
  
  remainder_map <- data.table(
    raw_code = character(0),
    group_code = character(0)
  )
  
  if (length(remainder_codes) > 0L) {
    
    remainder_map <- data.table(
      raw_code = remainder_codes,
      group_code = as.character(
        remainder_label
      )
    )
  }
  
  unique(
    rbindlist(
      list(
        custom_map,
        remainder_map
      ),
      use.names = TRUE,
      fill = TRUE
    ),
    by = "raw_code"
  )
}

#Aggregate one dimension according to the aggregation map, 
#sum values, and calculate the resulting observation and method flags.
aggregate_by_codelist <- function(
    data,
    key_dim_name,
    aggregation_map,
    value_col = "Value",
    observation_flag = "flagObservationStatus",
    method_flag = "flagMethod",
    group_by_observation_flag = FALSE,
    value_digits = VALUE_DECIMAL_DIGITS
) {
  
  dt <- copy(data)
  
  technical_cols <- intersect(
    c("raw_code", "group_code"),
    names(dt)
  )
  
  if (length(technical_cols) > 0) {
    dt[, (technical_cols) := NULL]
  }
  
  original_cols <- names(dt)
  
  if (!value_col %in% names(dt)) {
    stop(
      sprintf(
        "Column '%s' was not found in the selected dataset.",
        value_col
      )
    )
  }
  
  if (!key_dim_name %in% names(dt)) {
    stop(
      sprintf(
        "Column '%s' was not found in the selected dataset.",
        key_dim_name
      )
    )
  }
  
  if (!observation_flag %in% names(dt)) {
    stop(
      sprintf(
        "Column '%s' was not found in the selected dataset.",
        observation_flag
      )
    )
  }
  
  if (!method_flag %in% names(dt)) {
    stop(
      sprintf(
        "Column '%s' was not found in the selected dataset.",
        method_flag
      )
    )
  }
  
  aggregation_map <- as.data.table(
    aggregation_map
  )
  
  if (
    !all(
      c("raw_code", "group_code") %in%
      names(aggregation_map)
    )
  ) {
    stop(
      "The aggregation map must contain raw_code and group_code."
    )
  }
  
  aggregation_map <- unique(
    aggregation_map[
      ,
      .(
        raw_code = as.character(raw_code),
        group_code = as.character(group_code)
      )
    ]
  )
  
  dt[
    ,
    raw_code := as.character(
      get(key_dim_name)
    )
  ]
  
  
  mapped_codes <- unique(
    as.character(
      aggregation_map$raw_code
    )
  )
  
  data_codes <- unique(
    as.character(
      dt$raw_code
    )
  )
  
  unmapped_codes <- setdiff(
    data_codes,
    mapped_codes
  )
  
  if (length(unmapped_codes) > 0) {
    stop(
      paste0(
        "The selected aggregation hierarchy does not contain ",
        length(unmapped_codes),
        " filtered code(s) for dimension '",
        key_dim_name,
        "'. No records were aggregated because otherwise those codes ",
        "would be silently discarded. Unmapped codes: ",
        paste(
          head(unmapped_codes, 20),
          collapse = ", "
        ),
        if (length(unmapped_codes) > 20) {
          paste0(
            " ... and ",
            length(unmapped_codes) - 20,
            " more."
          )
        } else {
          ""
        }
      )
    )
  }
  
  dt_child <- merge(
    dt,
    aggregation_map,
    by = "raw_code",
    all = FALSE,
    sort = FALSE
  )
  
  if (nrow(dt_child) == 0) {
    stop(
      paste0(
        "No rows could be assigned to the selected ",
        "aggregation classification for dimension '",
        key_dim_name,
        "'."
      )
    )
  }
  
  dt_child[
    ,
    (key_dim_name) := NULL
  ]
  
  observation_group_col <- "__observation_flag_group__"
  
  if (isTRUE(group_by_observation_flag)) {
    dt_child[
      ,
      (observation_group_col) := as.character(
        get(observation_flag)
      )
    ]
  }
  
  keys_by <- c(
    "group_code",
    
    if (isTRUE(group_by_observation_flag)) {
      observation_group_col
    } else {
      character(0)
    },
    
    setdiff(
      names(dt_child),
      c(
        value_col,
        observation_flag,
        method_flag,
        "raw_code",
        "group_code",
        observation_group_col
      )
    )
  )
  
  keys_by <- unique(keys_by)
  
  missing_key_value <- "__MISSING_KEY__"
  
  key_cols <- intersect(
    keys_by,
    names(dt_child)
  )
  
  for (col in key_cols) {
    dt_child[
      ,
      (col) := as.character(
        get(col)
      )
    ]
    
    dt_child[
      is.na(get(col)) |
        !nzchar(get(col)),
      (col) := missing_key_value
    ]
  }
  
  
  deterministic_order_cols <- intersect(
    c(
      keys_by,
      observation_flag,
      method_flag,
      value_col
    ),
    names(dt_child)
  )
  
  if (
    length(deterministic_order_cols) > 0 &&
    nrow(dt_child) > 0
  ) {
    setorderv(
      dt_child,
      cols = deterministic_order_cols
    )
  }
  
  # Calculate the numerical total directly from the input records.
  # faoswsFlag is still used to calculate the resulting flags.
  direct_value_totals <- dt_child[
    ,
    .(
      direct_value_total = sum_or_na_rounded(
        get(value_col),
        digits = value_digits
      )
    ),
    by = keys_by
  ]
  
  out <- faoswsFlag::aggregateFlagData(
    data = dt_child,
    keys = keys_by,
    observationFlag = observation_flag,
    methodFlag = method_flag,
    forceMissingAggregateValues = FALSE,
    includeMissingFlags = TRUE
  )
  
  out <- unique(
    as.data.table(out)
  )
  
  if (!value_col %in% names(out)) {
    stop(
      paste0(
        "faoswsFlag did not return the expected value column '",
        value_col,
        "'."
      )
    )
  }
  
  # Retain the flags produced by faoswsFlag, but use the
  # direct sum calculated from the actual input records.
  setnames(
    out,
    value_col,
    "faosws_value"
  )
  
  out <- merge(
    out,
    direct_value_totals,
    by = keys_by,
    all.x = TRUE,
    sort = FALSE
  )
  
  out[
    ,
    (value_col) := direct_value_total
  ]
  
  out[
    ,
    c(
      "faosws_value",
      "direct_value_total"
    ) := NULL
  ]
  
  out <- out[
    !is.na(get(value_col))
  ]
  
  
  for (
    col in intersect(
      key_cols,
      names(out)
    )
  ) {
    out[
      get(col) == missing_key_value,
      (col) := NA_character_
    ]
  }
  
  if (
    isTRUE(group_by_observation_flag) &&
    observation_group_col %in% names(out)
  ) {
    out[
      ,
      (observation_flag) := as.character(
        get(observation_group_col)
      )
    ]
    
    out[, (observation_group_col) := NULL]
  }
  
  setnames(
    out,
    "group_code",
    key_dim_name,
    skip_absent = TRUE
  )
  
  if ("raw_code" %in% names(out)) {
    out[, raw_code := NULL]
  }
  
  missing_cols <- setdiff(
    original_cols,
    names(out)
  )
  
  for (col in missing_cols) {
    out[, (col) := NA]
  }
  
  setcolorder(
    out,
    original_cols
  )
  
  out[]
}




#Restrict the dataset to the selected inclusive year range.
filter_data_by_year <- function(data,
                                year_range = NULL,
                                year_col = "timePointYears") {
  dt <- data
  
  if (!year_col %in% names(dt)) {
    return(dt)
  }
  
  if (is.null(year_range) || length(year_range) != 2) {
    return(dt)
  }
  
  year_values <- suppressWarnings(
    as.integer(dt[[year_col]])
  )
  
  dt <- dt[
    year_values >= as.integer(year_range[1]) &
      year_values <= as.integer(year_range[2])
  ]
  
  dt[]
}

#Restrict the dataset to the selected measured elements.
filter_data_by_measured_element <- function(data,
                                            measured_elements = NULL,
                                            measured_element_col = "measuredElement") {
  dt <- data
  
  if (is.null(measured_elements) || length(measured_elements) == 0) {
    return(dt)
  }
  
  if (!measured_element_col %in% names(dt)) {
    return(dt)
  }
  
  measured_elements <- as.character(measured_elements)
  
  dt[
    as.character(get(measured_element_col)) %in% measured_elements
  ]
}

#Restrict a dataset column to the selected values.
filter_data_by_selected_values <- function(data,
                                           selected_values = NULL,
                                           column_name = NULL) {
  dt <- data
  
  if (is.null(column_name) || !column_name %in% names(dt)) {
    return(dt)
  }
  
  if (is.null(selected_values) || length(selected_values) == 0) {
    return(dt)
  }
  
  selected_values <- as.character(selected_values)
  
  dt[
    as.character(get(column_name)) %in% selected_values
  ]
}


# Aggregate the filtered dataset separately by observation flag.
# Years remain separate unless "Total all selected years into one period"
# is also selected.
aggregate_filtered_data_by_observation_flag <- function(
    data,
    value_col = "Value",
    observation_flag = "flagObservationStatus",
    method_flag = "flagMethod",
    year_col = "timePointYears"
) {
  
  dt <- copy(data)
  original_cols <- names(dt)
  
  if (!value_col %in% names(dt)) {
    stop(
      sprintf(
        "Column '%s' was not found in the selected dataset.",
        value_col
      )
    )
  }
  
  if (!observation_flag %in% names(dt)) {
    stop(
      sprintf(
        "Column '%s' was not found in the selected dataset.",
        observation_flag
      )
    )
  }
  
  if (!method_flag %in% names(dt)) {
    stop(
      sprintf(
        "Column '%s' was not found in the selected dataset.",
        method_flag
      )
    )
  }
  
  observation_group_col <- "__observation_flag_group__"
  
  dt[
    ,
    (observation_group_col) :=
      as.character(get(observation_flag))
  ]
  
  keys_by <- c(
    observation_group_col,
    if (year_col %in% names(dt)) {
      year_col
    } else {
      character(0)
    }
  )
  
  direct_totals <- dt[
    ,
    .(
      direct_value_total = sum_or_na_rounded(
        get(value_col),
        digits = VALUE_DECIMAL_DIGITS
      )
    ),
    by = keys_by
  ]
  
  out <- faoswsFlag::aggregateFlagData(
    data = dt,
    keys = keys_by,
    observationFlag = observation_flag,
    methodFlag = method_flag,
    forceMissingAggregateValues = FALSE,
    includeMissingFlags = TRUE
  )
  
  out <- unique(
    as.data.table(out)
  )
  
  setnames(
    out,
    value_col,
    "faosws_value"
  )
  
  out <- merge(
    out,
    direct_totals,
    by = keys_by,
    all.x = TRUE,
    sort = FALSE
  )
  
  out[
    ,
    (value_col) := direct_value_total
  ]
  
  out[
    ,
    c(
      "faosws_value",
      "direct_value_total"
    ) := NULL
  ]
  
  # Preserve the original flag category.
  out[
    ,
    (observation_flag) :=
      as.character(get(observation_group_col))
  ]
  
  out[
    ,
    (observation_group_col) := NULL
  ]
  
  # Restore the original output structure.
  # A column is preserved when the filtered data contain only one value;
  # otherwise it becomes NA because that dimension has been aggregated over.
  missing_cols <- setdiff(
    original_cols,
    names(out)
  )
  
  for (col in missing_cols) {
    
    values <- unique(dt[[col]])
    values <- values[!is.na(values)]
    
    if (length(values) == 1) {
      out[, (col) := values[1]]
    } else {
      out[, (col) := NA]
    }
  }
  
  setcolorder(
    out,
    original_cols
  )
  
  out[]
}


#Apply the selected aggregation rules across multiple dimensions and 
#optionally aggregate years or group separately by observation flag.
aggregate_by_multiple_dimensions <- function(
    data,
    aggregation_specs,
    value_col = "Value",
    observation_flag = "flagObservationStatus",
    method_flag = "flagMethod",
    group_by_observation_flag = FALSE,
    aggregate_selected_years = FALSE,
    year_col = "timePointYears",
    year_total_label = "Selected period"
) {
  
  dt <- data
  
  # No aggregation requested:
  # simply return the filtered dataset.
  if (
    length(aggregation_specs) == 0 &&
    !isTRUE(group_by_observation_flag) &&
    !isTRUE(aggregate_selected_years)
  ) {
    return(dt[])
  }
  
  # Observation-flag aggregation requested without any other
  # aggregation dimension.
  if (
    length(aggregation_specs) == 0 &&
    isTRUE(group_by_observation_flag)
  ) {
    
    dt <- aggregate_filtered_data_by_observation_flag(
      data = dt,
      value_col = value_col,
      observation_flag = observation_flag,
      method_flag = method_flag,
      year_col = year_col
    )
  }
  
  aggregation_order <- c(
    "geographical_area",
    "production_source",
    "species",
    "fishing_area"
  )
  
  aggregation_order <- aggregation_order[
    aggregation_order %in%
      names(aggregation_specs)
  ]
  
  for (dim_id in aggregation_order) {
    
    spec <- aggregation_specs[[dim_id]]
    aggregation_map_i <- spec$aggregation_map
    
    if (identical(
      spec$aggregation_mode,
      "total"
    )) {
      
      raw_values <- unique(
        as.character(
          dt[[spec$key_dim_name]]
        )
      )
      
      raw_values <- raw_values[
        !is.na(raw_values) &
          nzchar(raw_values)
      ]
      
      aggregation_map_i <- data.table(
        raw_code = raw_values,
        group_code = as.character(
          spec$aggregate_code %||% "TOTAL"
        )
      )
    }
    
    dt <- with_aggregation_context(
      
      aggregate_by_codelist(
        data = dt,
        key_dim_name = spec$key_dim_name,
        aggregation_map = aggregation_map_i,
        value_col = value_col,
        observation_flag = observation_flag,
        method_flag = method_flag,
        group_by_observation_flag =
          group_by_observation_flag
      ),
      
      dimension_id =
        spec$dimension %||%
        dim_id,
      
      label =
        spec$label %||%
        dim_id,
      
      dataset_column =
        spec$key_dim_name,
      
      codelist =
        spec$codelist,
      
      selected_codes =
        spec$selected_codes %||%
        spec$aggregate_code %||%
        spec$root_code,
      
      stage = paste0(
        spec$aggregation_mode,
        " aggregation execution"
      )
    )
  }
  
  if (
    isTRUE(aggregate_selected_years) &&
    year_col %in% names(dt)
  ) {
    
    years_present <- unique(
      as.character(
        dt[[year_col]]
      )
    )
    
    years_present <- years_present[
      !is.na(years_present) &
        nzchar(years_present)
    ]
    
    dt <- aggregate_by_codelist(
      data = dt,
      key_dim_name = year_col,
      
      aggregation_map = data.table(
        raw_code = years_present,
        group_code = as.character(
          year_total_label
        )
      ),
      
      value_col = value_col,
      observation_flag = observation_flag,
      method_flag = method_flag,
      group_by_observation_flag =
        group_by_observation_flag
    )
  }
  
  technical_cols <- intersect(
    c("raw_code", "group_code"),
    names(dt)
  )
  
  if (length(technical_cols) > 0) {
    dt[, (technical_cols) := NULL]
  }
  
  dt[]
}


#Extract the distinct available years from the dataset and return them in sorted order.
get_year_values <- function(data, year_col = "timePointYears") {
  dt <- copy(data)
  
  if (!year_col %in% names(dt)) {
    return(integer(0))
  }
  
  years <- suppressWarnings(as.integer(dt[[year_col]]))
  years <- sort(unique(years))
  years <- years[!is.na(years)]
  
  years
}

#Calculate the total value and number of records for each year or aggregated period.
summarise_total_by_year <- function(
    data,
    year_col = "timePointYears",
    value_col = "Value"
) {
  dt <- copy(data)
  
  if (!year_col %in% names(dt)) {
    return(data.table())
  }
  
  if (!value_col %in% names(dt)) {
    return(data.table())
  }
  
  dt[
    ,
    period_tmp :=
      normalise_period_label(
        get(year_col)
      )
  ]
  
  dt <- dt[!is.na(period_tmp)]
  
  if (nrow(dt) == 0) {
    return(data.table())
  }
  
  out <- dt[
    ,
    .(
      total_value = sum_or_na(
        get(value_col)
      ),
      n_rows = .N
    ),
    by = .(
      year = period_tmp
    )
  ]
  
  out[
    ,
    year_order :=
      period_start_value(year)
  ]
  
  setorder(
    out,
    year_order,
    year
  )
  
  out[, year_order := NULL]
  
  out[]
}

#Standardize year or period labels and convert empty values to missing values.
normalise_period_label <- function(x) {
  out <- trimws(as.character(x))
  out[is.na(out) | !nzchar(out)] <- NA_character_
  out
}

#Extract the starting numeric year from a year or period label for ordering purposes.
period_start_value <- function(x) {
  suppressWarnings(
    as.numeric(
      sub(
        "^\\s*([0-9]+).*$",
        "\\1",
        as.character(x)
      )
    )
  )
}


#Calculate total values and record counts by year and measured element.
summarise_total_by_year_and_element <- function(data,
                                                year_col = "timePointYears",
                                                value_col = "Value",
                                                measured_element_col = "measuredElement") {
  dt <- copy(data)
  
  if (!year_col %in% names(dt)) {
    return(data.table())
  }
  
  if (!value_col %in% names(dt)) {
    return(data.table())
  }
  
  if (!measured_element_col %in% names(dt)) {
    return(data.table())
  }
  
  dt[, year_tmp := suppressWarnings(as.integer(get(year_col)))]
  dt <- dt[!is.na(year_tmp)]
  
  dt[, measured_element_tmp := as.character(get(measured_element_col))]
  dt <- dt[!is.na(measured_element_tmp) & nzchar(measured_element_tmp)]
  
  if (nrow(dt) == 0) {
    return(data.table())
  }
  
  dt[
    ,
    .(
      total_value = sum_or_na(get(value_col)),
      n_rows = .N
    ),
    by = .(
      year = year_tmp,
      measured_element = measured_element_tmp
    )
  ][order(measured_element, year)]
}

#Calculate yearly totals for each component defined by the aggregation map.
summarise_composition_by_year <- function(
    data,
    aggregation_spec,
    codes,
    tree_dt,
    year_col = "timePointYears",
    value_col = "Value"
) {
  dt <- copy(data)
  
  if (is.null(aggregation_spec)) {
    return(data.table())
  }
  
  key_col <- aggregation_spec$key_dim_name
  
  if (!year_col %in% names(dt)) {
    return(data.table())
  }
  
  if (!value_col %in% names(dt)) {
    return(data.table())
  }
  
  if (!key_col %in% names(dt)) {
    return(data.table())
  }
  
  # Use the actual aggregation map that produced the result.
  component_map <- as.data.table(
    copy(
      aggregation_spec$aggregation_map
    )
  )
  
  if (
    !all(
      c("raw_code", "group_code") %in%
      names(component_map)
    )
  ) {
    return(data.table())
  }
  
  component_map <- unique(
    component_map[
      ,
      .(
        raw_code = as.character(raw_code),
        component_code = as.character(group_code)
      )
    ]
  )
  
  dt[
    ,
    period_tmp :=
      normalise_period_label(
        get(year_col)
      )
  ]
  
  dt[
    ,
    raw_code := as.character(
      get(key_col)
    )
  ]
  
  dt <- dt[
    !is.na(period_tmp) &
      raw_code %in%
      component_map$raw_code
  ]
  
  if (nrow(dt) == 0) {
    return(data.table())
  }
  
  dt <- merge(
    dt,
    component_map,
    by = "raw_code",
    all.x = FALSE,
    all.y = FALSE,
    sort = FALSE
  )
  
  composition <- dt[
    ,
    .(
      total_value = sum_or_na(
        get(value_col)
      )
    ),
    by = .(
      year = period_tmp,
      component_code
    )
  ]
  
  composition[
    ,
    year_order :=
      period_start_value(year)
  ]
  
  setorder(
    composition,
    year_order,
    year,
    component_code
  )
  
  composition[, year_order := NULL]
  
  composition[]
}

#Keep the largest categories and combine all remaining categories into Other.
collapse_top_categories <- function(plot_data,
                                    top_n = 10,
                                    category_col = "category",
                                    value_col = "total_value") {
  dt <- copy(plot_data)
  
  if (nrow(dt) == 0) {
    return(dt)
  }
  
  if (!all(c("year", category_col, value_col) %in% names(dt))) {
    return(data.table())
  }
  
  dt[, (category_col) := as.character(get(category_col))]
  dt[is.na(get(category_col)) | !nzchar(get(category_col)), (category_col) := "<missing>"]
  
  category_totals <- dt[
    ,
    .(
      period_total = sum(get(value_col), na.rm = TRUE)
    ),
    by = category_col
  ][order(-period_total)]
  
  keep_categories <- head(category_totals[[category_col]], top_n)
  
  dt[
    !get(category_col) %in% keep_categories,
    (category_col) := "Other"
  ]
  
  dt[
    ,
    .(
      total_value = sum(get(value_col), na.rm = TRUE)
    ),
    by = .(
      year,
      category = get(category_col)
    )
  ][order(year, category)]
}


#Restrict the data according to the aggregation settings of all dimensions except the selected one.
filter_data_to_other_aggregation_context <- function(data,
                                                     aggregation_specs,
                                                     selected_dim_id) {
  dt <- copy(data)
  
  if (is.null(aggregation_specs) || length(aggregation_specs) == 0) {
    return(dt)
  }
  
  for (dim_id in names(aggregation_specs)) {
    if (identical(dim_id, selected_dim_id)) {
      next
    }
    
    spec <- aggregation_specs[[dim_id]]
    
    if (!spec$key_dim_name %in% names(dt)) {
      next
    }
    
    dt[, (spec$key_dim_name) := as.character(get(spec$key_dim_name))]
    
    dt <- dt[
      get(spec$key_dim_name) %in% as.character(spec$child_codes)
    ]
  }
  
  dt[]
}

#Prepare treemap values for a selected year, keep the largest components, 
#group the rest as Other, and calculate their shares.
prepare_treemap_data <- function(composition,
                                 selected_year,
                                 top_n = 15) {
  dt <- copy(composition)
  
  if (nrow(dt) == 0) {
    return(data.table())
  }
  
  if (!all(c("year", "component_code", "total_value") %in% names(dt))) {
    return(data.table())
  }
  
  dt <- dt[year == selected_year]
  
  if (nrow(dt) == 0) {
    return(data.table())
  }
  
  dt <- dt[
    ,
    .(
      total_value = sum(total_value, na.rm = TRUE)
    ),
    by = component_code
  ]
  
  dt <- dt[!is.na(total_value) & total_value > 0]
  
  if (nrow(dt) == 0) {
    return(data.table())
  }
  
  component_totals <- dt[order(-total_value)]
  
  keep_components <- head(component_totals$component_code, top_n)
  
  dt[
    !component_code %in% keep_components,
    component_code := "Other"
  ]
  
  dt <- dt[
    ,
    .(
      total_value = sum(total_value, na.rm = TRUE)
    ),
    by = component_code
  ][order(-total_value)]
  
  dt[
    ,
    share := total_value / sum(total_value, na.rm = TRUE)
  ]
  
  dt[
    ,
    label := paste0(
      component_code,
      "\n",
      round(100 * share, 1),
      "%"
    )
  ]
  
  dt[]
}


