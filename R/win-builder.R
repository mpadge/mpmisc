#' Upload tarball to win-builder checks
#'
#' Code adapted from devtools/R/check-win.R
#' @export
win_builder_checks <- function () {

    requireNamespace ("curl")
    requireNamespace ("pkgbuild")

    here <- here::here ()
    wd <- setwd (here)

    build_dir <- normalizePath (file.path (here, ".."))
    path_tar <- file.path (build_dir, tar_from_desc (here))
    if (!file.exists (path_tar)) {
        path_tar <- pkgbuild::build (here, build_dir)
    }
    file_tar <- basename (path_tar)
    setwd (build_dir)

    url_base <- "ftp://win-builder.r-project.org/"
    versions <- c ("R-oldrelease", "R-release", "R-devel")

    for (v in versions) {

        url <- paste0 (url_base, v, "/", file_tar)
        con <- file (basename (file_tar), open = "rb")
        h <- curl::new_handle (upload = TRUE, filetime = FALSE)
        curl::handle_setopt (h, readfunction = function (n) {
            readBin (con, raw (), n = n)
        }, verbose = FALSE)
        out <- curl::curl_fetch_memory (url, handle = h)
        close (con)

        message ("Uploaded ", v)
    }
    setwd (wd)
}

tar_from_desc <- function (here) {

    desc <- normalizePath (file.path (here, "DESCRIPTION"))
    if (!file.exists (desc)) {
        stop ("Not an R package", call. = FALSE)
    }

    desc <- data.frame (read.dcf (desc))
    pkg <- desc$Package
    v <- desc$Version

    paste0 (pkg, "_", v, ".tar.gz")
}
