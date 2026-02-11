#' Get latest unread notifications from GitHub or Codeberg
#'
#' Not yet on graphql API:
#' https://github.community/t/get-notification-list-via-graphql-api/13836
#' but available on REST API:
#' https://docs.github.com/en/rest/reference/activity#notifications
#' And Codeberg API:
#' https://codeberg.org/api/swagger#/notification
#'
#' @param where Service to get notifications from; one of "github" or
#' "codeberg" (case-insensitive).
#' @param quiet If `FALSE`, print notifications to screen
#' @return `data.frame` of notifications (invisibly)
#'
#' @export
gh_notifications <- function (where = "github", quiet = FALSE) {

    where <- match.arg (tolower (where), c ("github", "codeberg"))
    tok <- get_token (where)

    u <- switch (where,
        "github" = "https://api.github.com/notifications",
        "codeberg" = "https://codeberg.org/api/v1/notifications"
    )

    req <- httr2::request (u) |>
        httr2::req_headers ("Authorization" = paste0 ("Bearer ", tok))

    resp <- httr2::req_perform (req)
    body <- httr2::resp_body_json (resp)

    if (length (body) > 0L) {

        getone <- function (body, what = "title", sub = NULL) {
            vapply (body, function (b) {
                if (is.null (sub)) {
                    this <- b [[what]]
                } else {
                    this <- b [[sub]] [[what]]
                }
                ifelse (is.null (this), NA_character_, this)
            }, character (1L))
        }

        urls <- getone (body, "url", "subject")
        issue_nums <- as.integer (gsub ("^.*\\/", "", urls))
        issue_nums [is.na (issue_nums)] <- "commit"

        x <- list (
            title = getone (body, "title", "subject"),
            repository = getone (body, "full_name", "repository"),
            issue_num = issue_nums,
            type = getone (body, "type", "subject"),
            subscription_url = getone (body, "subscription_url"),
            updated_at = getone (body, "updated_at"),
            last_read_at = getone (body, "last_read_at")
        )
        check_suite <- which (x$type == "CheckSuite")
        x$issue_num [check_suite] <- "CheckSuite"

    }

    if (!quiet) {
        notifications_to_screen (x)
    }

    cache_notifications (x)

    invisible (x)
}

get_token <- function (where = "github") {

    where <- match.arg (where, c ("github", "codeberg"))

    if (where == "github") {
        tok <- Sys.getenv ("GITHUB_TOKEN")
    } else if (where == "codeberg") {
        e <- Sys.getenv ()
        tok <- e [[grep ("codeberg", names (e), ignore.case = TRUE)]]
    }
    return (tok)
}


notifications_to_screen <- function (x) {

    NC <- "\033[0m" # nolint
    ARG <- "\033[0;31m" # red                                  # nolint
    TXT <- "\033[0;32m" # green, or 1;32m for light green      # nolint
    SYM <- "\u2192" # right arrow                              # nolint

    if (length (x) > 0) {

        n <- max (nchar (x$repository))
        repo <- vapply (
            x$repository, function (i) {
                paste0 (i, paste0 (rep (" ", n - nchar (i)),
                    collapse = ""
                ))
            },
            character (1)
        )

        for (i in seq_along (x$title)) {

            msg <- paste0 (
                SYM, " ", ARG, repo [i], " #",
                x$issue_num [i], NC, ": ",
                TXT, x$title [i], NC
            )

            message (msg)
        }
    } else {

        msg <- paste0 (SYM, " ", TXT, "No new notifications.", NC)
        message (msg)
    }

}

cache_notifications_file <- function () {

    cache_dir <- rappdirs::user_cache_dir ("mpmisc")
    if (!dir.exists (cache_dir)) {
        dir.create (cache_dir, recursive = TRUE)
    }

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
#' @param n Number of notification as generated from
#' \link{gh_notifications}
#' @return Nothing
#' @export
open_gh_notification <- function (n) {

    NC <- "\033[0m" # nolint
    ARG <- "\033[0;31m" # red                                  # nolint
    TXT <- "\033[0;32m" # green, or 1;32m for light green      # nolint
    SYM <- "\u2192" # right arrow                              # nolint

    x <- readRDS (cache_notifications_file ())

    if (length (x) == 0) {

        msg <- paste0 (SYM, " ", TXT, "There are no cached notifications.", NC)
        message (msg)

    } else if (n > nrow (x)) {

        msg <- paste0 (
            SYM, " ", TXT,
            "There are not that many cached notifications.", NC
        )
        message (msg)

    } else {

        x <- x [n, ]

        url <- paste0 (
            "https://github.com/",
            x$repository,
            "/issues/",
            x$issue_num
        )

        utils::browseURL (url = url)
    }
}

#' Mark all new GitHub notifications as read and remove from list
#'
#' This does not remove them from the top of the most recent notifications list,
#' but just marks them as read. There is currently no way to remove them from
#' the list.
#' @export
mark_gh_notifications_as_read <- function () {

    gh_tok <- Sys.getenv ("GITHUB_TOKEN")
    auth <- paste ("Bearer", gh_tok, sep = " ")

    u <- "https://api.github.com/notifications"

    h <- httr::add_headers (Authorization = auth)

    httr::PUT (u, h)
}
