#' @include bignlp-package.R
NULL

#' @title AnnotationPipeline Class.
#' 
#' @description Worker behind the higher-level `StanfordCoreNLP` class that
#'   allows fine-tuned configuration of an annotation pipeline, see the
#'   [documentation of CoreNLP
#'   Pipelines](https://stanfordnlp.github.io/CoreNLP/pipelines.html). The
#'   `$annotate()` method supports processing annotations in parallel. Unlike
#'   the `StanfordCoreNLP$process_files()` method for processing the content of
#'   files in parallel, it offers an in-memory operation. As annotations consume
#'   a lot of memory, there are limitations to allocating sufficient heap space
#'   required for the parallel in-memory processing of larger corpora. If heap
#'   space is insufficient, the process may run endless without a telling
#'   warning message or an error. So use the `$annotate()` method with
#'   appropriate care.
#' @examples 
#' A <- AnnotationPipeline$new()
#' a <- c("This is a sentences.", "Yet another sentence.")
#' s <- A$annotate(a)
#' result <- s$as.data.table()
#' 
#' reuters_txt <- readLines(
#'   system.file(package = "bignlp", "extdata", "txt", "reuters.txt")
#' )
#' B <- AnnotationPipeline$new()
#' r <- B$annotate(reuters_txt)
#' result <- r$as.data.table()
#' 
#' \dontrun{
#' # this will NOT work with 512GB heap space - 4 GB required
#' library(polmineR)
#' gparl_by_date <- corpus("GERMAPARL") %>%
#'   subset(year %in% 1998) %>%
#'   split(s_attribute = "date") %>% 
#'   get_token_stream(p_attribute = "word", collapse = " ") %>%
#'   as.character()
#' C <- AnnotationPipeline$new()
#' anno <- C$annotate(gparl_by_date, 4L)
#' result <- anno$as.data.table(anno)
#' }
#' @export AnnotationPipeline
#' @importFrom rJava .jarray
AnnotationPipeline <- R6Class(
  
  classname = "AnnotationPipeline",

  public = list(
    
    #' @field pipeline AnnotationPipeline
    pipeline = NULL,

    #' @description Initialize AnnotationPipeline
    #' @param corenlp_dir Directory where StanfordCoreNLP resides.
    initialize = function(corenlp_dir = getOption("bignlp.corenlp_dir")){
      stanford_path <- Sys.glob(paste0(corenlp_dir,"/*.jar"))
      .jaddClassPath(stanford_path)
      self$pipeline <- .jnew("edu.stanford.nlp.pipeline.AnnotationPipeline")
      
      self$pipeline$addAnnotator(.jnew("edu.stanford.nlp.pipeline.TokenizerAnnotator"))
      self$pipeline$addAnnotator(.jnew("edu.stanford.nlp.pipeline.WordsToSentencesAnnotator"))
      
      invisible(self)
    },

    #' @description Annotate a list of strings.
    #' @param x A list of `character` vectors to annotate, an `AnnotationList`
    #'   class object or an ArrayList with Annotation objects.
    #' @param threads If `NULL`, all available threads are used, otherwise an
    #'   `integer` value with number of threads to use.
    #' @param purge A `logical` value, whether to purge text first.
    #' @param verbose A `logical` value, whether to show progress messages.
    #' @return A Java object .
    annotate = function(x, purge = FALSE, threads = NULL, verbose = TRUE){
      if (!is.null(threads)) stopifnot(is.numeric(threads))
      if (verbose) message("... create Java Annotation objects")
      x <- if (is.character(x)){
        AnnotationList$new(x, purge = purge)
      } else if (is(x)[[1]] == "AnnotationList"){
        x
      }
      if (verbose) message("... annotate it")
      if (is.null(threads)){
        self$pipeline$annotate(x$obj)
      } else {
        self$pipeline$annotate(x$obj, as.integer(threads))
      }
      invisible(x)
    }
  )
)
