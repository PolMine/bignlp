#' @include bignlp.R
NULL

#' @title AnnotationPipeline Class.
#' 
#' @description Worker behind the higher-level `StanfordCoreNLP` class that allows fine-tuned
#' configuration of an annotation pipeline, see the [documentation of CoreNLP
#' Pipelines](https://stanfordnlp.github.io/CoreNLP/pipelines.html). The
#' `$annotate()` method supports processing annotations in parallel. Unlike the
#' `StanfordCoreNLP$process_files()` method for processing the content of files
#' in parallel, it is a very efficient in-memory operation  and the fastest
#' option for processsing medium-sized corpora. But as annotations consume a lot
#' of memory, there are limitations to allocating sufficient heap space required
#' for the parallel in-memory processing of larger corpora. If heap space is
#' insufficient, the process may run endless without a telling warning message
#' or an error. So use the `$annotate()` method with appropriate care.
#' @examples 
#' A <- AnnotationPipeline$new()
#' a <- list(
#'   "This is a sentences.",
#'   "Yet another sentence."
#' )
#' s <- A$annotate(a)
#' result <- A$as.matrix(s)
#' 
#' reuters_txt <- readLines(system.file(package = "bignlp", "extdata", "txt", "reuters.txt"))
#' B <- AnnotationPipeline$new()
#' r <- B$annotate(reuters_txt)
#' result <- B$as.matrix(r)
#' 
#' \dontrun{
#' library(polmineR)
#' gparl_by_date <- corpus("GERMAPARL") %>%
#'   subset(year %in% 1998) %>%
#'   split(s_attribute = "date") %>% 
#'   get_token_stream(p_attribute = "word", collapse = " ")
#' C <- AnnotationPipeline$new()
#' anno <- C$annotate(gparl_by_date, 4L)
#' result <- C$as.matrix(anno)
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
    #' @param x A list of `character` vectors to annotate.
    #' @param threads If `NULL`, all available threads are used, otherwise an
    #'   `integer` value with number of threads to use.
    #' @param verbose A `logical` value, whether to show progress messages.
    #' @return A Java object .
    annotate = function(x, threads = NULL, verbose = TRUE){
      if (!is.null(threads)) stopifnot(is.numeric(threads))
      if (verbose) message("... create Java Annotation objects")
      anno_array <- .jarray(lapply(
        x,
        function(chunk){
          S <- .jnew("java.lang.String", chunk)
          .jnew("edu/stanford/nlp/pipeline/Annotation", S)
        }
      ))
      annotations <- .jnew("java.util.Arrays")$asList(anno_array)
      if (verbose) message("... annotate it")
      if (is.null(threads)){
        self$pipeline$annotate(annotations)
      } else {
        self$pipeline$annotate(annotations, as.integer(threads))
      }
      annotations
    },
    
    #' @description Experimental method that will parse annotation output and
    #'   return a `matrix`.
    #' @param x A rJava AnnotationList object resulting from `$annotations()`.
    as.matrix = function(x){
      conll_outputter <- rJava::.jnew("edu.stanford.nlp.pipeline.CoNLLOutputter")
      do.call(rbind, lapply(
        0L:(x$size() - 1L),
        function(i){
          do.call(rbind, strsplit(strsplit(conll_outputter$print(x$get(i)), "\n")[[1]], "\t"))
        }
      ))
    }
  )
)
