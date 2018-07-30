#' R-Package 'bignlp'.
#' 
#' R corpus annotation pipeline for large corpora.
#' 
#' The package is an interface to Stanford CoreNLP, so the CoreNLP jars needs to
#' be available. When the package is attached, the availability of Stanford
#' CoreNLP is checked. The first take is to use the environment variable
#' CORENLP_DIR. If it is set, the option bignlp.corenlp_dir is set accordingly.
#' Then we move to searching for CoreNLP in packages. If it is not present in
#' the bignlp package itself, its presence in the cleanNLP package is checked.
#' If it has not been found, the option bignlp.corenlp_dir is empty ("").
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
