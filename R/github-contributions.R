
#' Count GitHub contributions for previous range of days.
#'
#' @param quiet If `FALSE`, print notifications to screen
#' @param ndays Number of preceding days for which contributions should be
#' extracted.
#' @return `data.frame` of calendar dates and contribution counts.
#' @export
gh_contributions <- function (quiet = FALSE, ndays = 7L) {

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
             }
        }
    }")

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

    #annual_total <- cc$contributionCalendar$totalContributions
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

    invisible (calendar)
}
