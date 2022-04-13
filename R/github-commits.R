
#' Count GitHub commits for current day
#'
#' @param quiet If `FALSE`, print notifications to screen
#' @return Single integer count of daily commits
#' @export
gh_daily_commits <- function (quiet = FALSE) {

    gh_tok <- Sys.getenv ("GITHUB_TOKEN")
    auth <- paste ("Bearer", gh_tok, sep = " ")

    u <- paste0 ("https://api.github.com/search/",
                 "commits?q=author:mpadge&sort=author-date&order=desc&page=1")

    h <- httr::add_headers (Authorization = auth)

    x <- httr::GET (u, h) |>
        httr::content ("text") |>
        jsonlite::fromJSON ()

    commits <- x$items$commit
    dates <- as.Date (strftime (commits$committer$date, "%Y-%m-%d"))
    num_commits = length (which (dates == Sys.Date ()))

    # screen output stuff:
    NC <- "\033[0m"                                            # nolint
    ARG <- "\033[0;31m" # red                                  # nolint
    TXT <- "\033[0;32m" # green, or 1;32m for light green      # nolint
    SYM <- "\u2192" # right arrow                              # nolint

    msg <- paste0 (SYM, " ", ARG, num_commits,
                   TXT, " daily commits (",
                   Sys.Date (), ")", NC)

    message (msg)
}
