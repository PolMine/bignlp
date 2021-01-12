#' R-Package 'bignlp'.
#' 
#' R corpus annotation pipeline for large corpora.
#' 
#' The package is an interface to the Stanford CoreNLP Natural Language
#' Processing Toolkit. CoreNLP is implemented in Java and its "jars" needs to be
#' available. When the package is attached, the availability of Stanford CoreNLP
#' is checked. The first take is to use the environment variable CORENLP_DIR. If
#' it is set, the option bignlp.corenlp_dir is set accordingly. Then we move to
#' searching for CoreNLP in packages. If it has not been
#' found, the option bignlp.corenlp_dir is empty ("").
#' 
#' @name bignlp
#' @docType package
#' @rdname bignlp
#' @import methods
#' @author Andreas Blaette
#' @examples
#' Sys.setenv("_JAVA_OPTIONS" = "")
#' if (getOption("bignlp.corenlp_dir") == "") corenlp_install(lang = "de")
NULL


#' @name corenlp_install
#' @title Install Stanford CoreNLP.
#' @description The function provides an installation mechanism to download and
#'   install Stanford CoreNLP within the bignlp package or externally.
#' @param lang Languages to install.
#' @param loc Directory where to put jar files. If missing, the files will be
#'   placed in the bignlp package.
#' @export corenlp_install
#' @rdname corenlp_install
#' @importFrom utils download.file unzip zip
corenlp_install <- function(lang = "de", loc){
  # create necessary directories
  if (missing(loc)) loc <- system.file(package = "bignlp", "extdata")
  exttools_dir <- loc
  if (!file.exists(exttools_dir)) dir.create(exttools_dir)
  corenlp_dir <- file.path(exttools_dir, "corenlp")
  if (!file.exists(corenlp_dir)) dir.create(corenlp_dir)
  
  corenlp_url <- "http://nlp.stanford.edu/software/stanford-corenlp-4.2.0.zip"
  zipfile <- file.path(corenlp_dir, basename(corenlp_url))
  download.file(url = corenlp_url, destfile = zipfile)
  unzip(zipfile = zipfile, exdir = corenlp_dir)
  file.remove(zipfile)
  
  options(
    bignlp.corenlp_dir = system.file(
      package = "bignlp", "extdata", "corenlp", "stanford-corenlp-4.2.0"
    )
  )
  
  languages <- list(
    de = function(){
      message("... installing model files for: German")
      german_jar_url <- "http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models-german.jar"
      german_jar <- file.path(corenlp_dir, "stanford-corenlp-4.2.0", basename(german_jar_url))
      download.file(url = german_jar_url, destfile = german_jar)
      unzip(german_jar, files = "StanfordCoreNLP-german.properties")
      zip(zipfile = german_jar, files = "StanfordCoreNLP-german.properties", flags = "-d")
    },
    en = function(){
      message("... installing model files for: English")
      english_jar_url <- "http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models-english.jar"
      english_jar <- file.path(corenlp_dir, "stanford-corenlp-4.2.0", basename(english_jar_url))
      download.file(url = english_jar_url, destfile = english_jar)
      unzip(english_jar, files = "StanfordCoreNLP.properties")
      zip(zipfile = english_jar, files = "StanfordCoreNLP.properties", flags = "-d")
    }
  )
  for (language in lang) languages[[lang]]()
}


`:=` <- function(...) NULL
.GRP <- .SD <- NULL

