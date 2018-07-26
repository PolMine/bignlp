.onAttach <- function (libname, pkgname) {
  options(
    "bignlp.corenlp_dir" = corenlp_get_jar_dir(),
    "bignlp.properties_file" = ""
  )
  
  packageStartupMessage(
    sprintf(
      "CoreNLP jar directory: %s",
      if (length(getOption("bignlp.corenlp_dir")) == 0)
        "NOT SET"
      else
        getOption("bignlp.corenlp_dir")
      )
  )
  
}

