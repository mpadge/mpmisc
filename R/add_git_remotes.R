#' add_git_remotes
#'
#' Automatically add git remotes for Gitlab, Bitbucket, Sourcehut, and
#' Codeberg, if the remote repositories exist.
#'
#' @param force_bb Bitbucket repos can not be directly queried; this option
#' simply forces a Bitbucket remote to be added regardless of whether or not
#' that remote repository actually exists or not.
#' @export
add_git_remotes <- function (force_bb = FALSE) {
    x <- gert::git_remote_list ()
    origin <- x$url

    repo <- utils::tail (strsplit (origin, "/") [[1]], 1)
    org <- orggl <- utils::tail (strsplit (origin, "/") [[1]], 2) [1]
    # I've got "atfutures" orgs everywhere except for gitlab, where it's called
    # "atfutures1"
    orggl <- ifelse (orggl == "atfutures", "atfutures1", orggl)

    originbb <- paste0 ("git@bitbucket.org:", org, "/", repo)
    origingl <- paste0 ("git@gitlab.com:", orggl, "/", repo)
    originsh <- paste0 ("git@git.sr.ht:~mpadge/", repo)
    origincb <- paste0 ("git@codeberg.org:", org, "/", repo)

    origin_to_url <- function (o) {
        gsub ("git@", "https://", gsub (":", "/", o))
    }

    origin_exists <- function (o) {
        chk <- httr::GET (origin_to_url (o))
        chk$status_code == 200
    }

    origins <- list (
        originbb = originbb,
        origingl = origingl,
        originsh = originsh,
        origincb = origincb
    )
    for (i in seq (origins)) {
        (origin_exists (origins [[i]]) & !names (origins) [i] %in% x$name)
        if (origin_exists (origins [[i]]) && !names (origins) [i] %in% x$name) {
            gert::git_remote_add (names (origins) [i], origins [[i]])
            message ("adding remote: ", names (origins) [i])
        }
    }

    if (force_bb) {
        o <- origins [[grep ("bitbucket", origins)]]
        gert::git_remote_add ("originbb", o)
        message ("adding remote: originbb")
    }
}
