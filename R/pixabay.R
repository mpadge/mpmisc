
#' Grab random pixabay image for leftwm background
#'
#' @export
pixabay <- function () {

    key <- Sys.getenv ("PIXABAY")
    u <- paste0 ("https://pixabay.com/api/?key=",
                 key, "&image_type=photo&order=latest&per_page=100",
                 #"&orientation=horizontal&category=buildings|places",
                 "&orientation=horizontal&category=buildings",
                 "&editors_choice=true")
    x <- jsonlite::fromJSON (u)
    imgs <- x$hits$largeImageURL

    f <- normalizePath ("~/.config/leftwm/themes/Garden/background.jpg")
    download.file (sample (imgs, 1L), f, quiet = TRUE)

    system2 ("leftwm", args = list ("command", "SoftReload"))
}
