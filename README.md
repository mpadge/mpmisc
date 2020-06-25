# mpmisc

<!-- badges: start -->

[![R build
status](https://github.com/mpadge/mpmisc/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/mpmisc/actions)
<!-- badges: end -->

Miscellaneous R functions which I load in my `.Rprofile`.

### Increment development version

One function `increment_dev_version()` is an alternative version of
[`usethis::use_dev_version()`](https://usethis.r-lib.org/reference/use_version.html),
but adapted to my workflow of incrementing the fourth component of
version numbers for every single commit. I have this function aliased to
`incr="Rscript -e 'mpmisc::increment_dev_version()'"`, so simply have to
type `incr` to automatically increment my version and update
`codemeta.json` (if that file exists).

### Set up git remotes

I mirror many repositories on various git remote services, including
[Sourcehut](https://sourcehut.org), [GitLab](https://gitlab.com),
[BitBucket](https://bitbucket.org), and [GitHub](https://github.com).
The `add_git_remotes()` function automatically adds the appropriate
`remote` entries for any locations in which a repository is mirrored. I
use a single `git push` alias to mirror that command to all listed
remotes. This `add_git_remotes()` function saves me the trouble of
having to manually specify these for each new repository.
