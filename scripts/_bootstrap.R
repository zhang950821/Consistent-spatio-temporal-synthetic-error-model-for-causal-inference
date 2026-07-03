pstsem_current_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[length(file_arg)]]),
                         winslash = "/", mustWork = TRUE))
  }

  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    ofile <- frames[[i]]$ofile
    if (!is.null(ofile)) {
      return(normalizePath(ofile, winslash = "/", mustWork = TRUE))
    }
  }

  NA_character_
}

pstsem_script_root <- function(default = getwd()) {
  script <- pstsem_current_script()
  if (!is.na(script)) {
    return(normalizePath(file.path(dirname(script), ".."),
                         winslash = "/", mustWork = TRUE))
  }
  normalizePath(default, winslash = "/", mustWork = TRUE)
}
