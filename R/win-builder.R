#' Upload tarball to win-builder checks
#'
#' Code adapted from devtools/R/check-win.R
#' @export
win_builder_checks <- function () {

    requireNamespace ("curl")
    requireNamespace ("pkgbuild")

    here <- here::here ()
    setwd (here)

    build_dir <- normalizePath (file.path (here, ".."))
    path_tar_file <- file.path (build_dir, tar_from_desc (here))
    if (!file.exists (path_tar_file)) {
        path_tar <- pkgbuild::build (here, build_dir)
        path_tar_file <- basename (path_tar)
    }

    url_base <- "ftp://win-builder.r-project.org/"
    versions <- c ("R-oldrelease", "R-release", "R-devel")

    for (v in versions) {

        url <- paste0 (url_base, v, "/", path_tar_file)
        con <- file (path_tar, open = "rb")
        on.exit (close (con), add = TRUE)
        h <- curl::new_handle (upload = TRUE, filetime = FALSE)
        curl::handle_setopt (h, readfunction = function (n) {
            readBin (con, raw (), n = n)
        }, verbose = FALSE)
        out <- curl::curl_fetch_memory (url, handle = h)

        message ("Uploaded ", v)
    }
}

tar_from_desc <- function (path) {

    desc <- normalizePath (file.path (here, "DESCRIPTION"))
    if (!file.exists (desc)) {
        stop ("Not an R package", call. = FALSE)
    }

    desc <- data.frame (read.dcf (desc))
    pkg <- desc$Package
    v <- desc$Version

    paste0 (pkg, "_", v, ".tar.gz")
}
