# Is the codelist code active or it has an end date?
# Active codes usually have end_date = NA or an empty value
is_active_codelist_code <- function(codes) {
  codes <- as.data.table(codes)
  
  # If the codelist has no end_date column, I can not identify expired codes.
  # In that case, keep all codes.
  if (!"end_date" %in% names(codes)) {
    return(rep(TRUE, nrow(codes)))
  }
  
  # Convert end_date to character to handle both numeric timestamps and strings
  end_date_chr <- trimws(as.character(codes$end_date))
  # A code is considered active only when end_date is missing or empty.
  return(is.na(end_date_chr) | !nzchar(end_date_chr))
}


drop_expired_codelist_codes <- function(codes) {
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  
  active <- is_active_codelist_code(codes)
  active_ids <- codes[active, id]
  
  codes <- codes[active]
  
  # Also clean the children lists, otherwise an active parent could still point
  # to expired children.
  if ("children" %in% names(codes)) {
    codes[, children := lapply(children, function(x) {
      child_ids <- extract_child_ids(x)
      child_ids <- child_ids[child_ids %in% active_ids]
      
      if (length(child_ids) == 0) {
        return(NULL)
      }
      
      child_ids
    })]
  }
  
  codes[]
}

#Remove hierarchy paths containing expired codelist codes.
drop_expired_codes_from_codelist_tree <- function(tree_dt, active_codes) {
  tree_dt <- as.data.table(tree_dt)
  active_codes <- as.data.table(active_codes)
  
  active_ids <- unique(as.character(active_codes$id))
  id_cols <- get_tree_id_cols(tree_dt)
  
  if (length(id_cols) == 0) {
    return(tree_dt)
  }
  
  keep <- rep(TRUE, nrow(tree_dt))
  
  for (col in id_cols) {
    vals <- as.character(tree_dt[[col]])
    
    keep <- keep & (
      is.na(vals) |
        !nzchar(vals) |
        vals %in% active_ids
    )
  }
  
  tree_dt[keep]
}



# -------------------------------------------------------------------------
# Expired codelists and artificial hierarchy roots
# -------------------------------------------------------------------------

KEEP_EXPIRED_CODELISTS <- c(
  "geographicAreaM49_fi",
  "fisheriesAsfis",
  "fisheriesCatchArea"
)

CODELISTS_WITH_EXPIRED_ROOTS <- c(
  "fisheriesAsfis",
  "fisheriesCatchArea"
)

SYNTHETIC_EXPIRED_ROOTS_ID <- "__EXPIRED_ROOTS__"

#Check whether a codelist uses the synthetic Expired roots branch.
uses_expired_roots_branch <- function(codelist_id) {
  as.character(codelist_id) %in%
    CODELISTS_WITH_EXPIRED_ROOTS
}

#Retrieve the original top-level codes from a codelist tree.
get_original_tree_roots <- function(tree_dt) {
  
  tree_dt <- as.data.table(tree_dt)
  
  id_cols <- get_tree_id_cols(tree_dt)
  
  if (
    length(id_cols) == 0L ||
    nrow(tree_dt) == 0L
  ) {
    return(character(0))
  }
  
  roots <- trimws(
    as.character(
      tree_dt[[id_cols[1L]]]
    )
  )
  
  unique(
    roots[
      !is.na(roots) &
        nzchar(roots)
    ]
  )
}

#Separate the original hierarchy roots into current and expired roots.
get_current_and_expired_tree_roots <- function(
    tree_dt,
    codes
) {
  
  codes <- copy(as.data.table(codes))
  codes[, id := trimws(as.character(id))]
  
  original_roots <- get_original_tree_roots(
    tree_dt
  )
  
  active_status <- is_active_codelist_code(
    codes
  )
  
  expired_ids <- codes[
    !active_status &
      !is.na(id) &
      nzchar(id),
    id
  ]
  
  expired_roots <- intersect(
    original_roots,
    expired_ids
  )
  
  current_roots <- setdiff(
    original_roots,
    expired_roots
  )
  
  list(
    current_roots = unique(current_roots),
    expired_roots = unique(expired_roots)
  )
}

#Add one or more hierarchy levels before the existing levels of a codelist tree.
prepend_tree_levels <- function(
    tree_dt,
    prefix_codes
) {
  
  tree_dt <- copy(as.data.table(tree_dt))
  
  old_id_cols <- get_tree_id_cols(tree_dt)
  
  if (
    length(old_id_cols) == 0L ||
    nrow(tree_dt) == 0L
  ) {
    return(data.table())
  }
  
  prefix_codes <- trimws(
    as.character(prefix_codes)
  )
  
  prefix_codes <- prefix_codes[
    !is.na(prefix_codes) &
      nzchar(prefix_codes)
  ]
  
  out <- copy(
    tree_dt[, ..old_id_cols]
  )
  
  shifted_names <- paste0(
    "level_",
    seq_along(old_id_cols) + length(prefix_codes),
    "_id"
  )
  
  setnames(
    out,
    old_id_cols,
    shifted_names
  )
  
  if (length(prefix_codes) > 0L) {
    
    for (i in seq_along(prefix_codes)) {
      out[
        ,
        (paste0("level_", i, "_id")) :=
          prefix_codes[i]
      ]
    }
  }
  
  final_names <- paste0(
    "level_",
    seq_len(
      length(prefix_codes) +
        length(old_id_cols)
    ),
    "_id"
  )
  
  setcolorder(
    out,
    final_names
  )
  
  out[]
}


#Add the synthetic Expired roots entry to a codelist.
add_synthetic_expired_root_code <- function(codes) {
  
  codes <- copy(
    as.data.table(codes)
  )
  
  codes[
    ,
    id := as.character(id)
  ]
  
  label_col <- get_codelist_label_column(
    codes
  )
  
  if (is.na(label_col)) {
    codes[, label_en := id]
    label_col <- "label_en"
  }
  
  codes[
    ,
    synthetic_label_only := FALSE
  ]
  
  synthetic_code <- data.table(
    id = SYNTHETIC_EXPIRED_ROOTS_ID,
    synthetic_label_only = TRUE
  )
  
  synthetic_code[
    ,
    (label_col) := "Expired roots"
  ]
  
  out <- rbindlist(
    list(
      codes,
      synthetic_code
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  unique(
    out,
    by = "id"
  )
}

#Keep current hierarchy roots at the top level and group expired roots
#under the synthetic Expired roots branch.
#Remove active terminal codes from expired paths when the same code
#also belongs to a current hierarchy.
add_expired_roots_branch_to_tree <- function(
    tree_dt,
    codes
) {
  
  tree_dt <- copy(
    as.data.table(tree_dt)
  )
  
  codes <- copy(
    as.data.table(codes)
  )
  
  codes[
    ,
    id := trimws(
      as.character(id)
    )
  ]
  
  id_cols <- get_tree_id_cols(
    tree_dt
  )
  
  if (length(id_cols) == 0L) {
    return(tree_dt)
  }
  
  
  root_info <- get_current_and_expired_tree_roots(
    tree_dt = tree_dt,
    codes = codes
  )
  
  original_root_column <- id_cols[1L]
  
  
  # Keep current hierarchy roots at the original top level.
  current_top_level_paths <- tree_dt[
    as.character(
      get(original_root_column)
    ) %in% root_info$current_roots,
    ..id_cols
  ]
  
  
  # Retrieve hierarchy paths belonging to expired roots.
  expired_original_paths <- tree_dt[
    as.character(
      get(original_root_column)
    ) %in% root_info$expired_roots,
    ..id_cols
  ]
  
  
  # Identify every code that belongs to a current hierarchy.
  current_hierarchy_codes <- clean_code_vector(
    unlist(
      current_top_level_paths[
        ,
        ..id_cols
      ],
      recursive = TRUE,
      use.names = FALSE
    )
  )
  
  
  # Identify codes that are themselves still active.
  active_ids <- codes[
    is_active_codelist_code(codes) &
      !is.na(id) &
      nzchar(id),
    id
  ]
  
  
  # Identify the terminal code of every path under an expired root.
  expired_terminal_codes <- rep(
    NA_character_,
    nrow(expired_original_paths)
  )
  
  for (column_i in rev(id_cols)) {
    
    values_i <- trimws(
      as.character(
        expired_original_paths[[column_i]]
      )
    )
    
    fill_i <- (
      is.na(expired_terminal_codes) &
        !is.na(values_i) &
        nzchar(values_i)
    )
    
    expired_terminal_codes[fill_i] <-
      values_i[fill_i]
  }
  
  
  # Remove active terminal codes from Expired roots when the same
  # code already belongs to a current hierarchy.
  remove_from_expired <- (
    expired_terminal_codes %in%
      current_hierarchy_codes &
      expired_terminal_codes %in%
      active_ids
  )
  
  expired_original_paths <-
    expired_original_paths[
      !remove_from_expired
    ]
  
  
  # Place the remaining expired paths under one synthetic
  # Expired roots branch.
  expired_paths <- prepend_tree_levels(
    tree_dt = expired_original_paths,
    prefix_codes = SYNTHETIC_EXPIRED_ROOTS_ID
  )
  
  
  expired_branch_header <- data.table(
    level_1_id = SYNTHETIC_EXPIRED_ROOTS_ID
  )
  
  
  out <- rbindlist(
    list(
      current_top_level_paths,
      expired_paths,
      expired_branch_header
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  
  unique(out)
}


#Select the hierarchy roots to display according to the codelist, tree purpose, configured roots, and relevant codes.
get_display_roots_for_tree <- function(
    codelist_id,
    configured_roots,
    relevant_codes,
    purpose = c(
      "filter",
      "classification",
      "custom"
    )
) {
  
  purpose <- match.arg(purpose)
  
  configured_roots <- unique(
    trimws(
      as.character(configured_roots)
    )
  )
  
  configured_roots <- configured_roots[
    !is.na(configured_roots) &
      nzchar(configured_roots)
  ]
  
  relevant_codes <- unique(
    trimws(
      as.character(relevant_codes)
    )
  )
  
  relevant_codes <- relevant_codes[
    !is.na(relevant_codes) &
      nzchar(relevant_codes)
  ]
  
  if (uses_expired_roots_branch(codelist_id)) {
    
    # Keep configured current roots and add Expired roots
    # at the same top level.
    roots <- unique(
      c(
        configured_roots,
        SYNTHETIC_EXPIRED_ROOTS_ID
      )
    )
    
  } else {
    
    roots <- configured_roots
  }
  
  if (length(relevant_codes) > 0L) {
    roots <- intersect(
      roots,
      relevant_codes
    )
  }
  
  unique(roots)
}


##########################################
# Helper: dimensions that should be shown as a flat checkbox tree

# Most codelist dimensions, such as geographic areas, ASFIS and fishing area, are hierarchical and should be displayed as trees.

# Some dimensions are not used as hierarchies. for some dimensons, the app should display only the values actually present
# in the selected dataset, as a flat list of tickable values
##########################################
is_flat_filter_dimension <- function(dim_id){
  dim_id %in% c(
    "measured_element",
    "observation_flag",
    "currency_flag"
  )
}


# Splitting the aggregated outputs if two measured elements are chosen
split_aggregated_outputs_by_measured_element <- function(aggr, cfg) {
  dt <- copy(aggr)
  
  measured_col <- cfg$measured_element_col
  
  if (is.null(measured_col) || !measured_col %in% names(dt)) {
    return(list("Aggregated output" = dt))
  }
  
  measured_elements <- sort(unique(as.character(dt[[measured_col]])))
  measured_elements <- measured_elements[
    !is.na(measured_elements) &
      nzchar(measured_elements)
  ]
  
  if (length(measured_elements) == 0) {
    return(list("Aggregated output" = dt))
  }
  
  out <- lapply(
    measured_elements,
    function(me_i) {
      dt[as.character(get(measured_col)) == me_i]
    }
  )
  
  names(out) <- measured_elements
  
  out
}



