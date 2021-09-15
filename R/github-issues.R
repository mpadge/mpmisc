
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
