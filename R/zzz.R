.onAttach <- function (libname, pkgname) {
  corenlp_dir <- if (nchar(Sys.getenv("CORENLP_DIR")) > 0L){
    Sys.getenv("CORENLP_DIR")
  } else {
    system.file(package = "cleanNLP", "extdata", "stanford-corenlp-full-2016-10-31")
  }
  options(
    "bignlp.corenlp_dir" = corenlp_dir,
    "bignlp.properties_file" = ""
  )
  
  packageStartupMessage(
    sprintf(
      "CoreNLP Code Directory: %s",
      if (length(getOption("bignlp.corenlp_dir")) == 0)
        "NOT SET"
      else
        getOption("bignlp.corenlp_dir")
      )
  )
  
}

