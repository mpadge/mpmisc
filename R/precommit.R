#' add_precommit_hooks
#'
#' Add precommit hooks from the \pkg{precommit} package, and modify to include
#' an additional hook to ensure package version is incremented on every commit
#' (via the \link{increment_dev_version} function). This function can also be
#' used to update a local '.pre-commit-config.yaml' file to reflect the latest
#' version at the specified 'url'.
#'
#' @note This function must be run within a package directory, and requires the
#' \pkg{precommit} package to be installed.
#'
#' @param url The location of a repository containing the desired
#' '.pre-commit-hooks.yaml' file to be installed in the local repository.
#' @param The branch of the repository from which the `.pre-commit-hooks.yaml`
#' file should be copied.
#'
#' @export
add_precommit_hooks <- function (url = "https://github.com/mpadge/mpmisc",
                                 branch = "master") {
    requireNamespace ("precommit")
    here <- here::here ()

    if (!hooks_exist (here)) {
        precommit::use_precommit ()
    }

    grab_local_hooks (url, branch)
}

hooks_exist <- function (here) {
    lf <- list.files (file.path (here, ".git", "hooks"))
    any (grepl ("^pre-commit$", lf))
}

grab_local_hooks <- function (url, branch) {
    url <- paste0 (gsub ("/$", "", url),
                   "/raw/",
                   branch,
                   "/.pre-commit-config.yaml")
    x <- httr::GET (url)
    if (x$status_code != 200)
        stop ("http status [", x$status_code, "]")

    xt <- httr::content (x, as = "text", encoding = "UTF-8")
    xt <- strsplit (xt, "\n") [[1]]
    writeLines (xt, file.path (here, ".pre-commit-config.yaml"))

    add_to_rbuildignore (here)

    check_hook_location (here)
}

add_to_rbuildignore <- function (here)
{
    f <- file.path (here, ".Rbuildignore")
    rb <- NULL
    if (file.exists (f))
        rb <- readLines (f)

    chk <- grepl ("\\^\\\\\\.pre-commit-config.yaml\\$", rb)
    if (!any (chk))
        rb <- c (rb, "^\\.pre-commit-config\\.yaml$")
    writeLines (rb, con = f)
}

check_hook_location <- function (here) {
    f <- file.path (here, ".pre-commit-config.yaml")
    if (!file.exists (f))
        stop (".pre-commit-config.yaml does not exist here")

    x <- readLines (f)
    i <- grep ("id: description version", x)
    if (length (i) == 1) {
        i <- i + grep ("entry:", x [i:length (x)]) - 1
        xi <- gsub ("\\s+entry:\\s", "", x [i])
        message ("Please modify line#", i, " of 'pre-commit-config.yaml' to\n",
                 "specify location of 'description' precommit hook")
    }
}
