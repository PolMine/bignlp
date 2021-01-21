.onLoad <- function(libname, pkgname) {
  rJava::.jpackage(name = pkgname, lib.loc = libname)
  if (nchar(corenlp_get_jar_dir()) > 0L){
    stanford_path <- Sys.glob(paste0(corenlp_get_jar_dir(),"/*.jar"))
    .jaddClassPath(stanford_path)
  }
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

