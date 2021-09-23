
#' Get latest unread github notifications
#'
#' Not yet on graphql API:
#' https://github.community/t/get-notification-list-via-graphql-api/13836
#' but available on REST API:
#' https://docs.github.com/en/rest/reference/activity#notifications
#' @param quiet If `FALSE`, print notifications to screen
#' @return `data.frame` of notifications (invisibly)
#' @export
gh_notifications <- function (quiet = FALSE) {

    gh_tok <- Sys.getenv ("GITHUB_TOKEN")
    auth <- paste ("Bearer", gh_tok, sep = " ")

    u <- "https://api.github.com/notifications"

    h <- httr::add_headers (Authorization = auth)

    x <- httr::GET (u, h) |>
        httr::content ("text") |>
        jsonlite::fromJSON ()

    if (length (x) > 0L) {

        x$title <- x$subject$title
        x$repository <- x$repository$full_name
        s <- do.call (rbind, strsplit (x$subject$url, "/"))
        x$issue_num <- suppressWarnings (
                    as.integer (s [, ncol (s)])
                    )
        x$issue_num [is.na (x$issue_num)] <- "commit"
        x$type <- x$subject$subject.type

        x$subscription_url <- x$subject <- NULL
        x$updated_at <- strptime (x$updated_at, "%Y-%m-%dT%H:%M:%SZ")
        x$last_read_at <- strptime (x$last_read_at, "%Y-%m-%dT%H:%M:%SZ")
    }

    if (!quiet)
        notifications_to_screen (x)

    cache_notifications (x)

    invisible (x)
}

notifications_to_screen <- function (x) {

    NC <- "\033[0m"                                            # nolint
    ARG <- "\033[0;31m" # red                                  # nolint
    TXT <- "\033[0;32m" # green, or 1;32m for light green      # nolint
    SYM <- "\u2192" # right arrow                              # nolint

    if (length (x) > 0) {

        n <- max (nchar (x$repository))
        repo <- vapply (x$repository, function (i)
                        paste0 (i, paste0 (rep (" ", n - nchar (i)),
                                           collapse = "")),
                        character (1))

        for (i in seq (nrow (x))) {

            msg <- paste0 (SYM, " ", ARG, repo[i], " #",
                           x$issue_num [i], NC, ": ",
                           TXT, x$title [i], NC)

            message (msg)
        }
    } else {

        msg <- paste0 (SYM, " ", TXT, "No new notifications.", NC)
        message (msg)
    }

}

cache_notifications_file <- function () {

    cache_dir <- rappdirs::user_cache_dir ("mpmisc")
    if (!dir.exists (cache_dir))
        dir.create (cache_dir, recursive = TRUE)

    file.path (cache_dir, "latest_gh_notifications.Rds")
}

cache_notifications <- function (x) {

    saveRDS (x, cache_notifications_file ())
}

#' Open web browser to specified GitHub notification
#'
#' This accepts only the single parameter which corresponds to a number from the
#' most recently extracted and cached notification list.
#'
#' @param number Number of notification as generated from
#' \link{gh_notifications}
#' @return Nothing
#' @export
open_gh_notification <- function (n) {

    NC <- "\033[0m"                                            # nolint
    ARG <- "\033[0;31m" # red                                  # nolint
    TXT <- "\033[0;32m" # green, or 1;32m for light green      # nolint
    SYM <- "\u2192" # right arrow                              # nolint

    x <- readRDS (cache_notifications_file ())

    if (length (x) == 0) {

        msg <- paste0 (SYM, " ", TXT, "There are no cached notifications.", NC)
        message (msg)

    } else if (n > nrow (x)) {

        msg <- paste0 (SYM, " ", TXT,
                       "There are not that many cached notifications.", NC)
        message (msg)

    } else {

        x <- x [n, ]

        url <- paste0 ("https://github.com/",
                       x$repository,
                       "/issues/",
                       x$issue_num)

        browseURL (url = url)
    }
}

#' Mark all new GitHub notifications as read and remove from list
#'
#' @export
mark_gh_notifications_as_read <- function () {

    gh_tok <- Sys.getenv ("GITHUB_TOKEN")
    auth <- paste ("Bearer", gh_tok, sep = " ")

    u <- "https://api.github.com/notifications"

    h <- httr::add_headers (Authorization = auth)

    x <- httr::PUT (u, h)
}
