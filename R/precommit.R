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
#' @note The function downloads a pre-commit hook file in
#' 'inst/precommit/description', and notes that this file should be moved to a
#' more general location, and the '.pre-commit-hooks.yaml' file updated
#' accordingly. It may also be necessary to change the permissions of the
#' 'description' pre-commit hook, for example (on *nix systems) via 'sudo chmod
#' u_x description'.
#'
#' @param url The location of a repository containing the desired
#' '.pre-commit-hooks.yaml' file to be installed in the local repository.
#' @param branch The branch of the remote repository from which the
#' `.pre-commit-hooks.yaml` file should be copied.
#' @param location Local location to store precommit hooks. Default is relative
#' path within current directory, but it is advised to set an absolute path to a
#' more general location (such as '~/bin/precommit').
#'
#' @export
add_precommit_hooks <- function (url = "https://github.com/mpadge/mpmisc",
                                 branch = "main",
                                 location = "inst/precommit") {
    requireNamespace ("precommit")
    here <- here::here ()

    if (!hooks_exist (here)) {
        precommit::use_precommit ()
    }

    grab_local_hooks (url, branch, location, here)
}

hooks_exist <- function (here) {
    lf <- list.files (here, all.files = TRUE)
    any (grepl ("^\\.pre-commit-config.yaml", lf))
}

grab_local_hooks <- function (url, branch, location, here) {
    u <- paste0 (gsub ("/$", "", url),
                 "/raw/",
                 branch,
                 "/.pre-commit-config.yaml")
    x <- httr::GET (u)
    if (x$status_code != 200)
        stop ("http status [", x$status_code, "]")

    xt <- httr::content (x, as = "text", encoding = "UTF-8")
    xt <- strsplit (xt, "\n") [[1]]

    message ("here = [", here, "]")
    xt <- sub_location (xt, location)

    writeLines (xt, file.path (here, ".pre-commit-config.yaml"))

    add_to_rbuildignore (here)

    download_hook (url, branch, here)

    check_hook_location (here)
}

sub_location <- function (x, location) {
    loc_default <- "inst/precommit"
    if (location != loc_default) {
        i <- grep (loc_default, x)
        x_start <- paste0 (strsplit (x [i], ":") [[1]] [1], ": ")
        x [i] <- paste0 (x_start, location, "/description")
    }
    return (x)
}

add_to_rbuildignore <- function (here) {
    f <- file.path (here, ".Rbuildignore")
    rb <- NULL
    if (file.exists (f))
        rb <- readLines (f)

    chk <- grepl ("\\^\\\\\\.pre-commit-config\\\\.yaml\\$", rb)
    if (!any (chk))
        rb <- c (rb, "^\\.pre-commit-config\\.yaml$")
    writeLines (rb, con = f)
}

download_hook <- function (url, branch, here) {
    fp <- file.path (here, "inst", "precommit")
    if (!file.exists (fp))
        dir.create (fp, recursive = TRUE)

    f <- file.path (fp, "description")
    if (!file.exists (f)) {
        u <- paste0 (gsub ("/$", "", url),
                     "/raw/",
                     branch,
                     "/inst/precommit/description")
        x <- httr::GET (u)
        if (x$status_code != 200)
            stop ("http status [", x$status_code, "]")

        xt <- httr::content (x, as = "text", encoding = "UTF-8")
        xt <- strsplit (xt, "\n") [[1]]

        writeLines (xt, f)
    }
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
                 "specify location of 'description' precommit hook\n",
                 "Current location: ", xi)
    }
}
