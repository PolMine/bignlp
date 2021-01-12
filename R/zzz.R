.onLoad <- function(libname, pkgname) {
  options(java.parameters = "-Xmx4g")
  rJava::.jpackage(name = pkgname, lib.loc = libname)
}


.onAttach <- function (libname, pkgname) {
  options("bignlp.corenlp_dir" = corenlp_get_jar_dir())
  
  packageStartupMessage(
    sprintf(
      "CoreNLP jar directory: %s",
      if (length(getOption("bignlp.corenlp_dir")) == 0)
        "NOT SET"
      else
        getOption("bignlp.corenlp_dir")
      )
  )
  
  packageStartupMessage(sprintf("JVM memory allocated: %s", jvm_heap_space()))
}

