# mpmisc

<!-- badges: start -->

[![R build
status](https://github.com/mpadge/mpmisc/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/mpmisc/actions)
<!-- badges: end -->

Miscellaneous R functions which I load in my `.Rprofile`, including
functions to

  - Increment development version
  - Set up multiple git remotes
  - Add pre-commit hooks to ensure package version is incremented on
    each commit

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

### Pre-Commit Hooks

I use the [`precommit`
package](https://github.com/lorenzwalthert/precommit/) to implement
pre-commit hooks, including a personalised hook to ensure that the
development version number has been incremented using the above
`increment_dev_version()` function on each commit. This requires
installing the [`pre-commit` library](https://pre-commit.com), and the
[`precommit` R package](https://github.com/lorenzwalthert/precommit/).
This repository then has both:

1.  A version of `.pre-commit-config.yaml` modified from the original
    version in the `precommit` R package, primarily through the addition
    of:

<!-- end list -->

    -   id: description version
        name: Version has been incremeneted in DESCRIPTION
        entry: inst/precommit/description
        language: script

2.  The corresponding `inst/precommit/description` file which checks
    first that `DESCRIPTION` has been modified, and secondly that the
    modification includes a change to `"Version"`.

In practice, my `description` hook lives in a general location at
`~/bin/precommit/description` rather than within any particular repo,
with the `entry` value reflecting this location.
