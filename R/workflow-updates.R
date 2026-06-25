# Scan subdirectories for GitHub Actions workflows and report outdated action versions.
# Respects a '.workflow-update-excludes' file in the directory in which this is run.

# --- 1. Find candidate directories --------------------

find_candidate_dirs <- function (root = ".", max_depth = 2L) {
    dirs <- system2 (
        "find",
        c (
            normalizePath (root, mustWork = TRUE),
            "-mindepth", 1L, "-maxdepth", max_depth,
            "-type", "d"
        ),
        stdout = TRUE, stderr = FALSE
    )
    if (length (dirs) == 0L) {
        return (character (0L))
    }

    dirs [
        fs::dir_exists (file.path (dirs, ".github", "workflows")) &
            fs::file_exists (file.path (dirs, "README.md"))
    ]
}

# --- 2. Extract uses: keys from workflow YAML files --------------------

extract_uses_from_file <- function (yaml_file) {
    # rg exits with status 1 on no matches; system2 warns on any non-zero exit
    matches <- suppressWarnings (system2 (
        "rg",
        c (
            "--no-filename",
            "-o",
            shQuote ("uses:\\s+\\S+@v[\\d.]+"),
            shQuote (yaml_file)
        ),
        stdout = TRUE, stderr = FALSE
    ))
    if (length (matches) == 0L) {
        return (NULL)
    }

    tibble::tibble (
        location = yaml_file,
        key = stringr::str_trim (
            stringr::str_remove (matches, "^\\s*uses:\\s*")
        )
    )
}

extract_uses_keys <- function (candidate_dirs) {
    dplyr::bind_rows (lapply (candidate_dirs, function (d) {
        yaml_files <- fs::dir_ls (
            file.path (d, ".github", "workflows"),
            regexp = "\\.ya?ml$"
        )
        dplyr::bind_rows (lapply (yaml_files, extract_uses_from_file))
    }))
}

# ----- 3. Compute max version per action --------------------

parse_version <- function (key) {
    numeric_version (
        stringr::str_remove (stringr::str_extract (key, "@v[\\d.]+$"), "@v")
    )
}

find_max_versions <- function (uses_table) {

    # suppress no visible binding notes:
    key <- action <- NULL

    uses_table |>
        dplyr::mutate (
            action  = stringr::str_extract (key, "^[^@]+"),
            version = parse_version (key)
        ) |>
        dplyr::group_by (action) |>
        dplyr::summarise (max_version = max (version), .groups = "drop")
}

# --- 4. Return outdated references grouped by action key --------------------

find_outdated <- function (uses_table, max_versions) {

    # suppress no visible binding notes:
    key <- action <- max_version <- location <- current <- NULL

    uses_table |>
        dplyr::mutate (
            action  = stringr::str_extract (key, "^[^@]+"),
            version = parse_version (key)
        ) |>
        dplyr::left_join (max_versions, by = "action") |>
        dplyr::filter (version < max_version) |>
        dplyr::transmute (
            action,
            latest  = paste0 (action, "@v", max_version),
            current = key,
            location
        ) |>
        dplyr::arrange (action, current, location)
}

# --- 5. Exclude paths listed in .workflow-update-excludes --------------------

filter_excludes <- function (dirs) {
    excl_file <- ".workflow-update-excludes"
    if (!fs::file_exists (excl_file)) {
        return (dirs)
    }

    lines <- readLines (excl_file, warn = FALSE)
    lines <- trimws (lines [nzchar (trimws (lines))])
    if (length (lines) == 0L) {
        return (dirs)
    }

    exclude_abs <- normalizePath (lines, mustWork = FALSE)
    keep <- !vapply (dirs, function (d) {
        any (d == exclude_abs | startsWith (d, paste0 (exclude_abs, "/")))
    }, logical (1L))
    dirs [keep]
}

find_and_filter_candidates <- function (root, max_depth) {

    cli::cli_alert_info (
        "Scanning {.path {normalizePath(root)}} (max depth {max_depth})"
    )

    candidates <- find_candidate_dirs (root, max_depth)

    n_before <- length (candidates)
    candidates <- filter_excludes (candidates)
    n_excluded <- n_before - length (candidates)
    if (n_excluded > 0L) {
        cli::cli_alert_info (
            "Excluded {n_excluded} director{?y/ies} via \\
            {.file .workflow-update-excludes}"
        )
    }
    cli::cli_alert_info (
        "{length(candidates)} candidate director{?y/ies} found"
    )
    return (candidates)
}

# --- Main --------------------

#' A local-only version of dependabot.
#'
#' Scans all directories above where function is called, extracts maximum
#' version number of all unique GitHub actions used in every workflow, and
#' reports paths to any repositories which use workflow versions less than
#' those.
#'
#' @param root Directory in which function is to be run.
#' @param max_depth Maximal recursive depth for trawling sub-directories.
#' @return (Invisibly) A \pkg{tibble} `data.frame` of all obsolete workflow
#' versions. Function is primarily intended to be called for the side-effect of
#' printing all results to screen.
#' @export

workflow_updates <- function (root = ".", max_depth = 2L) {

    # suppress no visible binding notes:
    action <- location <- current <- NULL


    candidates <- find_and_filter_candidates (root, max_depth)
    if (length (candidates) == 0L) {
        return (invisible (tibble::tibble ()))
    }

    cli::cli_alert_info ("Extracting {.code uses:} keys from workflow files")
    uses_table <- extract_uses_keys (candidates)
    if (nrow (uses_table) == 0L) {
        cli::cli_alert_warning ("No versioned {.code uses:} entries found.")
        return (invisible (tibble::tibble ()))
    }
    cli::cli_alert_info (
        "{nrow(uses_table)} {.code uses:} entr{?y/ies} across \\
        {dplyr::n_distinct(uses_table$location)} file{?s}"
    )

    max_versions <- find_max_versions (uses_table)
    outdated <- find_outdated (uses_table, max_versions)

    if (nrow (outdated) == 0L) {
        cli::cli_alert_success (
            "All workflow actions are at their latest observed version."
        )
        return (invisible (tibble::tibble ()))
    }

    cli::cli_alert_warning (
        "{nrow(outdated)} outdated reference{?s} - \\
        {dplyr::n_distinct(outdated$action)} action{?s}, \\
        {dplyr::n_distinct(outdated$location)} location{?s}"
    )

    for (act in unique (outdated$action)) {
        sub <- dplyr::filter (outdated, action == act)
        latest_ver <- sub$latest [[1L]]
        cli::cli_h3 ("{act}")
        cli::cli_text ("Latest: {cli::col_green(latest_ver)}")

        for (ver in unique (sub$current)) {
            locs <- dplyr::filter (sub, current == ver) |>
                dplyr::pull (location)
            rel_locs <- as.character (fs::path_rel (locs))
            cli::cli_bullets (c (
                "x" = cli::col_red (ver),
                stats::setNames (
                    paste0 ("{.path ", rel_locs, "}"),
                    rep (" ", length (rel_locs))
                )
            ))
        }
        cat ("\n")
    }

    invisible (outdated)
}
