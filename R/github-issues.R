
# https://docs.github.com/en/graphql/reference/objects#user
# https://docs.github.com/en/graphql/reference/objects#contributionscollection

get_qry <- function (gh_cli, user = "mpadge", n = 20)
{
    q <- paste0 ("{
        user(login:\"", user, "\") {
             issueComments (last:", n, ") {
                 edges {
                     node {
                         createdAt
                         issue {
                             number
                             closed
                             repository {
                                 name
                                 nameWithOwner
                             }
                         }
                     }
                 }
             }
            }
        }")

    qry <- ghql::Query$new()
    qry$query('user', q)

    return (qry)
}

#' Get latest comments from GitHub issues
#'
#' @param user GitHub user name
#' @param n Number of latest comments to fetch
#' @param open If `TRUE`, only return comments from open issues, otherwise
#' include closed issues too.
#' @return `data.frame` of commit data
#' @export
latest_comments <- function (user = "mpadge", n = 20, open = TRUE) {

    token <- Sys.getenv("GITHUB_TOKEN")
    gh_cli <- ghql::GraphqlClient$new (
                       url = "https://api.github.com/graphql",
                       headers = list (Authorization = paste0 ("Bearer ", token))
                        )

    qry <- get_qry (user = user, n = n * 5)

    dat <- gh_cli$exec(qry$queries$user) |>
        jsonlite::fromJSON ()

    out <- dat$data$user$issueComments$edges$node
    out <- data.frame (createdAt = out$createdAt,
                       name = out$issue$repository$name,
                       nameWithOwner = out$issue$repository$nameWithOwner,
                       number = out$issue$number,
                       closed = out$issue$closed)

    out$repo_issue <- paste0 (out$name, "#", out$number)
    out$org_repo_issue <- paste0 (out$nameWithOwner, "#", out$number)

    if (open)
        out <- out [which (!out$closed), ]
    
    out$createdAt <- strptime (out$createdAt, "%Y-%m-%dT%H:%M:%SZ")

    out <- out [order (out$createdAt, decreasing = TRUE), ]

    out <- out [which (!duplicated (out$org_repo_issue)), ]

    out <- out [seq (n), ]

    rownames (out) <- NULL

    cache_comments (out)

    return (out)
}

cache_comments_file <- function () {

    cache_dir <- rappdirs::user_cache_dir ("mpmisc")
    if (!dir.exists (cache_dir))
        dir.create (cache_dir, recursive = TRUE)

    file.path (cache_dir, "latest_gh_comments.Rds")
}


cache_comments <- function (x) {

    saveRDS (x, cache_comments_file ())
}

retrieve_cached_comments <- function () {

    readRDS (cache_comments_file ())
}

#' Open web browser to specified GitHub issue
#'
#' Open issue only with name of repo, or if no issue specified open main issues
#' page of repo. This uses \link{latest_comments} to extract issues for
#' specified user in order to associate repo name with the correct GitHub
#' organization.
#'
#' @param repo Name of repo
#' @param issue Optional number of issue
#' @return Nothing
#' @export
open_gh_issue <- function (repo = NULL, issue = NULL, user = "mpadge") {

    if (is.null (repo))
        stop ("repo must be specified")
    if (length (repo) > 1L)
        stop ("only one repo may be specified")

    cmts <- retrieve_cached_comments ()

    if (!repo %in% cmts$name)
        cmts <- latest_comments (user = user, open = FALSE)

    cmts <- cmts [which (cmts$name == repo), ]

    url <- paste0 ("https://github.com/",
                   cmts$nameWithOwner [1],
                   "/issues")

    if (!is.null (issue)) {

        if (!is.numeric (issue))
            stop ("issue must be a number")
        if (length (issue) > 1L)
            stop ("issue must be a single number")

        url <- paste0 (url, "/", as.integer (issue))
    }

    browseURL (url = url)
}

#' Get latest unread github notifications
#'
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

    x$title <- x$subject$title
    x$repository <- x$repository$full_name
    s <- do.call (rbind, strsplit (x$subject$url, "/"))
    x$issue_num <- as.integer (s [, ncol (s)])
    x$type <- x$subject$subject.type

    x$subscription_url <- x$subject <- NULL
    x$updated_at <- strptime (x$updated_at, "%Y-%m-%dT%H:%M:%SZ")
    x$last_read_at <- strptime (x$last_read_at, "%Y-%m-%dT%H:%M:%SZ")

    if (!quiet) {

        NC="\033[0m"
        ARG="\033[0;31m" # red
        TXT="\033[0;32m" # green, or 1;32m for light green
        SYM="\u2192" # right arrow

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

    }

    cache_notifications (x)

    invisible (x)
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

    x <- readRDS (cache_notifications_file ())
    if (n > nrow (x))
        stop ("There are not that many notifications")

    x <- x [n, ]

    url <- paste0 ("https://github.com/",
                   x$repository,
                   "/issues/",
                   x$issue_num)

    browseURL (url = url)
}
