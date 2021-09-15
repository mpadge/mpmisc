
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
#' @return `data.frame` of commit data
#' @export
latest_comments <- function (user = "mpadge", n = 20) {

    token <- Sys.getenv("GITHUB_TOKEN")
    gh_cli <- ghql::GraphqlClient$new (
                       url = "https://api.github.com/graphql",
                       headers = list (Authorization = paste0 ("Bearer ", token))
                        )

    qry <- get_qry (user = user, n = n)

    dat <- gh_cli$exec(qry$queries$user) |>
        jsonlite::fromJSON ()

    out <- dat$data$user$issueComments$edges$node
    out <- data.frame (createdAt = out$createdAt,
                       name = out$issue$repository$name,
                       nameWithOwner = out$issue$repository$nameWithOwner,
                       number = out$issue$number)

    out$repo_issue <- paste0 (out$name, "#", out$number)
    out$org_repo_issue <- paste0 (out$nameWithOwner, "#", out$number)
    
    out$createdAt <- strptime (out$createdAt, "%Y-%m-%dT%H:%M:%SZ")

    out <- out [order (out$createdAt, decreasing = TRUE), ]

    out <- out [which (!duplicated (out$org_repo_issue)), ]
    rownames (out) <- NULL

    return (out)
}
