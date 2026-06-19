#' Scan subdirectories for outdated GitHub Actions workflow versions
#'
#' Walks subdirectories of `root` up to `max_depth` levels deep, finds any that
#' contain both a `README.md` and a `.github/workflows/` directory, and reports
#' every `uses:` reference whose version tag is older than the highest version
#' of the same action observed anywhere in the scan.
#'
#' Directories (or whole subtrees) can be silently skipped by listing their
#' paths - one per line - in a `.workflow-update-excludes` file located in
#' `root`.
#'
#' @param root Root directory to search. Defaults to the current working
#'   directory (`"."`).
#' @param max_depth Maximum directory depth to descend below `root`. Passed
#'   directly to `find -maxdepth`. Defaults to `2L`.
#'
#' @return A [tibble][tibble::tibble] with columns `action`, `latest`,
#'   `current`, and `location` describing every outdated `uses:` reference
#'   found, invisibly. Returns an empty tibble (also invisibly) when all
#'   references are up to date or no workflow files are found.
#'
#' @examples
#' \dontrun{
#' # Scan two levels deep from the current directory
#' workflow_updates ()
#'
#' # Scan a specific path, one level deep
#' workflow_updates ("~/projects", max_depth = 1L)
#' }
#'
#' @export

workflow_updates <- function (root = ".", max_depth = 2L) {

    requireNamespace ("stringr", quietly = TRUE)

    # Suppress no visible binding noteS:
    action <- location <- current <- NULL

    cli::cli_alert_info (
        "Scanning {.path {normalizePath(root)}} (max depth {max_depth})"
    )

    candidates <- find_candidate_dirs (root, max_depth)

    n_before <- length (candidates)
    candidates <- filter_excludes (candidates, root)
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
        "{nrow(uses_table)} {.code uses:} entr{?y/ies} \\
        across {dplyr::n_distinct(uses_table$location)} file{?s}"
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
        "{nrow(outdated)} outdated reference{?s} \\
        - {dplyr::n_distinct(outdated$action)} action{?s}, \\
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
            rel_locs <- as.character (
                fs::path_rel (locs, start = normalizePath (root))
            )
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

# -- 1. Find candidate directories ----

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

# -- 2. Extract uses: keys from workflow YAML files ----

extract_uses_from_file <- function (yaml_file) {
    # rg exits with status 1 on no matches; system2 warns on any non-zero exit
    matches <- suppressWarnings (system2 (
        "rg",
        c (
            "--no-filename", "-o",
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

# -- 3. Compute max version per action ----

parse_version <- function (key) {
    numeric_version (
        stringr::str_remove (
            stringr::str_extract (key, "@v[\\d.]+$"),
            "@v"
        )
    )
}

find_max_versions <- function (uses_table) {

    # Suppress no visible binding noteS:
    key <- action <- NULL

    uses_table |>
        dplyr::mutate (
            action  = stringr::str_extract (key, "^[^@]+"),
            version = parse_version (key)
        ) |>
        dplyr::group_by (action) |>
        dplyr::summarise (max_version = max (version), .groups = "drop")
}

# -- 4. Return outdated references grouped by action key ----

find_outdated <- function (uses_table, max_versions) {

    # Suppress no visible binding noteS:
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

# -- 5. Exclude paths listed in .workflow-update-excludes ----

filter_excludes <- function (dirs, root = ".") {
    excl_file <- file.path (normalizePath (root), ".workflow-update-excludes")
    if (!fs::file_exists (excl_file)) {
        return (dirs)
    }

    lines <- readLines (excl_file, warn = FALSE)
    lines <- trimws (lines [nzchar (trimws (lines))])
    if (length (lines) == 0L) {
        return (dirs)
    }

    exclude_abs <- normalizePath (
        file.path (normalizePath (root), lines),
        mustWork = FALSE
    )
    keep <- !vapply (dirs, function (d) {
        any (d == exclude_abs | startsWith (d, paste0 (exclude_abs, "/")))
    }, logical (1L))
    dirs [keep]
}
