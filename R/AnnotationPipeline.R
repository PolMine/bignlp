#' @include bignlp.R
NULL

#' AnnotationPipeline Class.
#' 
#' @examples 
#' A <- AnnotationPipeline$new()
#' a <- list(
#'   "This is a sentences.",
#'   "Yet another sentence."
#' )
#' A$annotate(a)
#' anno <- A$annotations$get(0L)
#' conll_outputter <- rJava::.jnew("edu.stanford.nlp.pipeline.CoNLLOutputter")
#' y <- do.call(rbind, lapply(
#'   0L:(A$annotations$size() - 1L),
#'   function(i){
#'     read.table(text = conll_outputter$print(A$annotations$get(i)))
#'   }
#' ))
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
    #' @return A `List` of `Annotation` objects.
    annotate = function(x){
      anno_array <- .jarray(lapply(
        x,
        function(chunk){
          S <- .jnew("java.lang.String", chunk)
          .jnew("edu/stanford/nlp/pipeline/Annotation", S)
        }
      ))
      self$annotations <- .jnew("java.util.Arrays")$asList(anno_array)
      self$pipeline$annotate(self$annotations)
      invisible(self)
    }
  )
)
