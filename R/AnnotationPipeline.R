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
#' A$annotate(a)
#' result <- A$as.matrix()
#' 
#' reuters_txt <- readLines(system.file(package = "bignlp", "extdata", "txt", "reuters.txt"))
#' B <- AnnotationPipeline$new()
#' B$annotate(reuters_txt)
#' result <- B$as.matrix()
#' 
#' \dontrun{
#' library(polmineR)
#' gparl_by_date <- corpus("GERMAPARL") %>%
#'   subset(year %in% 1998) %>%
#'   split(s_attribute = "date") %>% 
#'   get_token_stream(p_attribute = "word", collapse = " ")
#' C <- AnnotationPipeline$new()
#' C$annotate(gparl_by_date, 4L)
#' result <- C$as.matrix()
#' }
#' @export AnnotationPipeline
#' @importFrom rJava .jarray
AnnotationPipeline <- R6Class(
  
  classname = "AnnotationPipeline",

  public = list(
    
    #' @field pipeline AnnotationPipeline
    #' @field annotations A `List` of `Annotation` (Java) objects.
    pipeline = NULL,
    annotations = NULL,
    
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
    #' @return A `List` of `Annotation` objects.
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
      self$annotations <- .jnew("java.util.Arrays")$asList(anno_array)
      if (verbose) message("... annotate it")
      if (is.null(threads)){
        self$pipeline$annotate(self$annotations)
      } else {
        self$pipeline$annotate(self$annotations, as.integer(threads))
      }
      invisible(self)
    },
    
    #' @description Experimental method that will parse annotation output and
    #'   return a `matrix`.
    as.matrix = function(){
      conll_outputter <- rJava::.jnew("edu.stanford.nlp.pipeline.CoNLLOutputter")
      do.call(rbind, lapply(
        0L:(self$annotations$size() - 1L),
        function(i){
          # print(i)
          # read.table(text = conll_outputter$print(self$annotations$get(i)))
          do.call(rbind, strsplit(strsplit(conll_outputter$print(self$annotations$get(i)), "\n")[[1]], "\t"))
        }
      ))
    }
  )
)
