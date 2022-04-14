
#' Count GitHub commits for current day
#'
#' @param quiet If `FALSE`, print notifications to screen
#' @param ndays Number of preceding days for which commits should be extracted.
#' @return Single integer count of daily commits
#' @export
gh_daily_commits <- function (quiet = FALSE, ndays = 3L) {

    page <- 1
    dates <- get_one_page_of_commits (page = page)
    while (length (unique (dates)) < (ndays + 1L)) {
        page <- page + 1
        dates <- c (dates, get_one_page_of_commits (page = page))
    }

    dates <- as.Date (dates)
    num_commits = table (dates)
    num_commits <- data.frame (date = names (num_commits),
                               commits = as.integer (num_commits))
    num_commits <- num_commits [order (num_commits$date, decreasing = TRUE), ]
    rownames (num_commits) <- NULL

    # screen output stuff:
    NC <- "\033[0m"                                            # nolint
    ARG <- "\033[0;31m" # red                                  # nolint
    TXT <- "\033[0;32m" # green, or 1;32m for light green      # nolint
    SYM <- "\u2192" # right arrow                              # nolint

    if (ndays == 1L) {
        msg <- paste0 (SYM, " ", ARG, num_commits,
                       TXT, " daily commits (",
                       Sys.Date (), ")", NC)

        message (msg)
    } else {
        message (TXT, "----", SYM, " Daily numbers of commits:", NC)
        for (i in seq (ndays)) {
            message ("   ", ARG, num_commits$date [i],
                     "  ", TXT, num_commits$commits [i], NC)
        }
    }
}

# Returns just the dates of commits
get_one_page_of_commits <- function (page = 1) {

    gh_tok <- Sys.getenv ("GITHUB_TOKEN")
    auth <- paste ("Bearer", gh_tok, sep = " ")

    u <- paste0 ("https://api.github.com/search/",
                 "commits?q=committer:mpadge&sort=committer-date&order=desc&page=",
                 page)

    h <- httr::add_headers (Authorization = auth)

    x <- httr::GET (u, h) |>
        httr::content ("text") |>
        jsonlite::fromJSON ()

    dates <- x$items$commit$committer$date
    as.Date (strftime (dates, "%Y-%m-%d"))
}
