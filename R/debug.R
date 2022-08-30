#' Configure an R package path for debugging with 'gdb' or 'lldb'
#'
#' Note that no path arguments are used or recognised, and this function can
#' only be called and used in the current working directory.
#'
#' @return Nothing; this just modifies or writes a 'Makevars' script. The
#' debugger can then be started with a single extra command.
#' @export
debug <- function () {

    write_makevars ()

    #message ("Starting gdb ...")
    #cmd <- paste0 ("R -d gdb -e 'source(\"", script, "\")'")
    #system (cmd)
    # -> gdb must be called directly from terminal
}

write_makevars <- function () {

    path <- normalizePath (here::here ())
    flist <- list.files (path, full.names = TRUE, recursive = TRUE)
    flist <- normalizePath (flist)

    src_ptn <- paste0 (.Platform$file.sep, "src", .Platform$file.sep)
    if (!any (grepl (src_ptn, flist, fixed = TRUE))) {
        stop ("Directory has no 'src' sub-directory")
    }

    makevars <- grep (paste0 ("src\\", .Platform$file.sep, "[Mm]akevars$"),
                      flist,
                      fixed = FALSE,
                      value = TRUE)

    if (length (makevars) > 0L) {

        if (length (makevars) > 1L) {
            makevars <- makevars [which (!grepl ("\\.win$", makevars))]
        }

        # Insert gdb line into Makevars:
        x <- readLines (makevars)
        i <- grep ("^PKG\\_CPPFLAGS", x) [1]

        if (!grepl ("-UDEBUG -g", x [i], fixed = TRUE)) {

            x [i] <- paste0 (x [i], " -UDEBUG -g")
            writeLines (x, makevars)
            message (
                "'Makevars' has been modified; revert changes after debugging"
            )
        }

    } else {

        makevars <- normalizePath (
            file.path (here::here (), "src", "Makevars"),
            mustWork = FALSE
        )

        writeLines ("PKG_CPPFLAGS=-UDEBUG -g", makevars)
        message ("A 'Makevars' file has been created; rm after debugging")
    }
}
