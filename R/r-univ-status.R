#' Function to check status on R-Universe and issue any warnings.
#'
#' @param univ Name of universe, as sub-domain prefix of main R-universe URL.
#' @param maintainer Optional name of maintainer. If given, results with be
#' filtered to only those packages within specified univsere maintained by that
#' person.
#'
#' @return Nothing. Function is called only for its side-effect of generating
#' messages in an active terminal session.
#'
#' @export
r_univ_build_status <- function (univ = "ropensci-review-tools", maintainer = NULL) {
    u <- paste0 ("https://", univ, ".r-universe.dev/api/packages")
    packages <- httr2::request(u) |>
        httr2::req_user_agent("R-universe docs") |>
        httr2::req_perform() |>
        httr2::resp_body_json()
    if (!is.null (maintainer)) {
        index <- which (vapply (
            packages,
            function (p) grepl (maintainer, p$Maintainer, ignore.case = TRUE),
            logical (1L)
        ))
        packages <- packages [index]
    }
    pkg_names <- vapply (packages, function (p) p$Package, character (1L))
    pkg_jobs <- lapply (packages, function (p) p$`_jobs`)
    pkg_checks <- lapply (pkg_jobs, function (i) {
        vapply (i, function (j) j$check, character (1L))
    })
    has_this <- function (pkg_checks, what = "warning") {
        which (vapply (pkg_checks, function (p) {
            any (grepl (what, p, ignore.case = TRUE))
        }, logical (1L)))
    }
    warnings <- has_this (pkg_checks, "warning")
    errors <- has_this (pkg_checks, "errors")
    if (length (warnings) == 0L && length (errors) == 0L) {
        cli::cli_alert_success (
            "All r-universe builds okay for {univ}"
        )
    } else {
        if (length (warnings) > 0L) {
            cli::cli_alert_warning (
                "Warnings for {univ}: {pkg_names[warnings]}"
            )
        }
        if (length (errors) > 0L) {
            cli::cli_alert_danger (
                "Errors for {univ}: {pkg_names[warnings]}"
            )
        }
    }
}
