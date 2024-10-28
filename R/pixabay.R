#' Grab random pixabay image for leftwm background
#'
#' @param category Pixabay category from list at
#' \url{https://pixabay.com/api/docs/#api_search_images}.
#' @export
pixabay <- function (category = "buildings") {

    key <- Sys.getenv ("PIXABAY")
    u <- paste0 (
        "https://pixabay.com/api/?key=",
        key, "&image_type=photo&order=latest&per_page=100",
        "&orientation=horizontal&category=",
        category,
        "&editors_choice=true"
    )
    x <- jsonlite::fromJSON (u)
    imgs <- x$hits$largeImageURL

    f <- normalizePath ("~/.config/leftwm/themes/Garden/background.jpg")
    utils::download.file (sample (imgs, 1L), f, quiet = TRUE)

    system2 ("leftwm", args = list ("command", "SoftReload"))
}

#' Grab random local image for leftwm background
#'
#' Local directory is specified in envvar 'PIX_LOCAL_DIR'
#' @export
pix_local <- function () {

    key <- Sys.getenv ("PIX_LOCAL_DIR")
    if (!nzchar (key)) {
        stop ("Enivronment variable 'PIX_LOCAL_DIR' must be specified.",
            call. = FALSE
        )
    }
    if (!dir.exists (key)) {
        stop ("Directory [", key, "] does not exist.",
            call. = FALSE
        )
    }
    f_new <- sample (list.files (key, full.names = TRUE), 1L)

    f <- normalizePath ("~/.config/leftwm/themes/Garden/background.jpg")
    file.copy (f_new, f, overwrite = TRUE)

    system2 ("leftwm", args = list ("command", "SoftReload"))
}
