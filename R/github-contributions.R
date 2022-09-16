gh_contrib_query <- function () {

    q <- paste0 ("{
        user(login:\"mpadge\") {
             login
             name
             contributionsCollection {
                 firstIssueContribution
                 totalRepositoriesWithContributedCommits
                 contributionCalendar {
                     totalContributions
                     weeks {
                         contributionDays {
                             contributionCount
                             date
                             weekday
                         }
                     }
                 }
                 commitContributionsByRepository {
                     contributions (last: 100) {
                         nodes {
                             commitCount
                             occurredAt
                             repository {
                                 name
                             }
                         } 
                     }
                 }
             }
        }
    }")
}

#' Count GitHub contributions for previous range of days.
#'
#' @param quiet If `FALSE`, print notifications to screen
#' @param ndays Number of preceding days for which contributions should be
#' extracted.
#' @param annual If `TRUE`, also show total annual commits.
#' @return `data.frame` of calendar dates and contribution counts.
#' @export
gh_contributions <- function (quiet = FALSE, ndays = 7L, annual = TRUE) {

    q <- gh_contrib_query ()

    qry <- ghql::Query$new()
    qry$query("user", q)

    token <- Sys.getenv("GITHUB_TOKEN") # or whatever
    gh_cli <- ghql::GraphqlClient$new (
        url = "https://api.github.com/graphql",
        headers = list (Authorization = paste0 ("Bearer ", token))
    )
    dat <- gh_cli$exec(qry$queries$user) |>
        jsonlite::fromJSON ()

    cc <- dat$data$user$contributionsCollection

    annual_total <- cc$contributionCalendar$totalContributions

    calendar <- do.call (rbind, cc$contributionCalendar$weeks$contributionDays)
    calendar$date <- as.Date (calendar$date)
    calendar <- calendar [order (calendar$date, decreasing = TRUE), 1:2]
    calendar$weekday <- as.character (lubridate::wday (calendar$date,
                                                       label = TRUE,
                                                       abbr = TRUE))

    # screen output stuff:
    NC <- "\033[0m"                                            # nolint
    ARG <- "\033[0;31m" # red                                  # nolint
    TXT <- "\033[0;32m" # green, or 1;32m for light green      # nolint
    SYM <- "\u2192" # right arrow                              # nolint

    if (ndays == 1L) {
        msg <- paste0 (SYM, " ", ARG, calendar$contributionCount [1],
                       TXT, " daily contributions (",
                       Sys.Date (), ")", NC)

        message (msg)
    } else {
        message (TXT, "----", SYM, " Daily numbers of contributions:", NC)
        for (i in seq (ndays)) {
            message ("   ", ARG, calendar$weekday [i],
                     "  ", calendar$date [i], " ",
                     TXT, calendar$contributionCount [i])
        }
    }

    if (annual) {
        message (TXT, "----", SYM, " ", TXT, " Total annual contributions: ",
                 annual_total, NC)
    }

    gh_daily_contributions ()

    invisible (calendar)
}

#' Details of GitHub contributions for particular day
#'
#' @param day 0 for today; 1 for yesterday, and so on.
#' @param quiet If `FALSE`, print notifications to screen
#' @return `data.frame` of repositories and contribution counts.
#' @export
gh_daily_contributions <- function (day = 0L, quiet = FALSE) {

    q <- gh_contrib_query ()

    qry <- ghql::Query$new()
    qry$query('user', q)

    token <- Sys.getenv("GITHUB_TOKEN") # or whatever
    gh_cli <- ghql::GraphqlClient$new (
                       url = "https://api.github.com/graphql",
                       headers = list (Authorization = paste0 ("Bearer ", token))
                        )
    dat <- gh_cli$exec(qry$queries$user) |>
        jsonlite::fromJSON (flatten = TRUE)
    dat <- dat$data$user$contributionsCollection
    dat <- dat$commitContributionsByRepository$contributions.nodes

    dat <- do.call (rbind, dat)

    dates <- lubridate::ymd (as.Date (dat$occurredAt))
    today <- lubridate::ymd (as.Date (Sys.time ()))
    target_date <- today - day
    
    dat <- dat [which (dates == target_date), ]
    if (nrow (dat) == 0L) {
        return (NULL)
    }

    weekday <- as.character (lubridate::wday (
        target_date, label = TRUE, abbr = TRUE))

    # screen output stuff:
    NC <- "\033[0m"                                            # nolint
    ARG <- "\033[0;31m" # red                                  # nolint
    TXT <- "\033[0;32m" # green, or 1;32m for light green      # nolint
    SYM <- "\u2192" # right arrow                              # nolint

    msg <- paste0 (TXT, "----", SYM,
        " Commit contributions for ", weekday, " ", target_date, ":", NC)
    message (msg)

    for (i in seq (nrow (dat ))) {
        message ("   ", ARG, dat$repository.name [i],
                 "  ", TXT, dat$commitCount [i])
    }

    invisible (dat)
}
