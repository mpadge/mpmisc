#' Add text of daily activity to 'daily.md' file
#'
#' @param txt A string summarising daily activity.
#' @export
mpmisc_daily <- function (txt = "") {

    hdir <- Sys.getenv ("HACKMD_DIR")
    if (!nzchar (hdir) || !nzchar (txt)) {
        return (NULL)
    }

    daily_file <- file.path (hdir, "daily.md")
    if (!file.exists (daily_file)) {
        stop ("[", daily_file, "] not found")
    }
    daily <- readLines (daily_file)

    today <- as.Date (strftime (Sys.time (), "%Y-%m-%d"))
    daily_dates <- gsub ("\\*\\*", "", grep ("^\\*\\*", daily, value = TRUE))
    daily_dates <- as.Date (strptime (gsub ("^\\w+\\s", "", daily_dates), "%d/%m/%Y"))
    daily_dates <- daily_dates [which (!is.na (daily_dates))]
    if (max (daily_dates) < today) {
        daily <- add_new_dates (today, daily)
    }

    today <- strftime (today, "%d/%m/%Y")
    today_i <- grep (today, daily, fixed = TRUE)
    next_i <- grep ("^(\\*\\*[A-Z]|<details>)", daily)
    next_i <- next_i [which (next_i > today_i) [1]]
    today_lines <- daily [seq (today_i + 1, next_i - 1)]
    pts <- grep ("^\\-", today_lines)
    if (length (pts) == 0) {
        pts <- 1L
    }
    pts <- max (pts)
    today_lines <- c (
        today_lines [seq_len (pts)],
        paste0 ("- ", txt),
        today_lines [seq_along (today_lines) [-seq_len (pts)]]
    )

    daily <- c (
        daily [seq_len (today_i)],
        today_lines,
        daily [seq (next_i, length (daily))]
    )

    writeLines (daily, daily_file)
}

add_new_dates <- function (d, daily) {

    if (length (d) == 0L) {
        return (daily)
    }

    # add any new dates from calendar:
    daily_dates <- gsub ("\\*\\*", "", grep ("^\\*\\*", daily, value = TRUE))
    daily_dates <- as.Date (strptime (gsub ("^\\w+\\s", "", daily_dates), "%d/%m/%Y"))
    daily_dates <- daily_dates [which (!is.na (daily_dates))]

    dnew <- unique (d [which (d > max (daily_dates))])
    if (length (dnew) == 0L) {
        return (daily)
    }

    # today <- as.Date (strftime (Sys.time (), "%Y-%m-%d"))
    # dnew <- rev (seq (today, max (dnew), by = "days") [-1])
    dnew <- rev (seq (max (daily_dates) + 1, max (dnew), by = "day"))
    dnew_abbr <- as.character (lubridate::wday (dnew, label = TRUE, abbr = TRUE))
    dnew <- strftime (dnew, "%d/%m/%Y")
    index <- which (!dnew_abbr %in% c ("Sat", "Sun"))
    dnew <- dnew [index]
    dnew_abbr <- dnew_abbr [index]
    dnew <- paste0 ("**", dnew_abbr, " ", dnew, "**")
    n <- length (dnew)
    dnew <- rep (dnew, each = 2)
    dnew [seq (n) * 2] <- ""

    index <- 1:grep ("^\\#\\#\\#", daily) [1] - 1
    daily <- c (
        daily [index],
        dnew,
        daily [-index]
    )

    return (daily)
}
