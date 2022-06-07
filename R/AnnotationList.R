#' @title AnnotationList Class.
#' 
#' @description Generate and process an Java ArrayList with Annotation objects.
#' @export AnnotationList
#' @importFrom rJava .jarray
#' @examples
#' if (getOption("bignlp.corenlp_dir") == "") corenlp_install(lang = NULL)
#' 
#' docs <- c("Das ist das erste Dokument.", "Ein weiteres Dokument!")
#' annoli <- AnnotationList$new(docs)
#' Pipe <- AnnotationPipeline$new()
#' Pipe$annotate(annoli)
#' annoli$as.data.table()
AnnotationList <- R6Class(
  
  classname = "AnnotationList",

  public = list(
    
    #' @field obj rJava object
    obj = NULL,

    #' @description Initialize AnnotationPipeline
    #' @param x A `character` vector with text documents.
    #' @param purge A `logical` value, whether to remove invalid characters as defined by 
    #'   `corenlp_preprocessing_replacements` before running annotators.
    initialize = function(x, purge = TRUE){
      if (!missing(x)){
        if (is.character(x) || is.list(x)){
          anno_array <- .jarray(lapply(
            x,
            function(doc){
              if (isTRUE(purge)){
                doc <- purge(
                  doc,
                  replacements = corenlp_preprocessing_replacements,
                  progress = FALSE
                )
              }
              .jnew("edu/stanford/nlp/pipeline/Annotation", .jnew("java.lang.String", doc))
            }
          ))
          self$obj <- .jnew("java.util.Arrays")$asList(anno_array)
        } else if (is(s) == "jobjRef"){
          self$obj <- x
        }
      } else {
        
      }
      self
    },


    #' @description Parse annotation output and return `data.table`.
    as.data.table = function(){
      conll_outputter <- rJava::.jnew("edu.stanford.nlp.pipeline.CoNLLOutputter")
      rbindlist(lapply(
        0L:(self$obj$size() - 1L),
        function(i){
          dt <- corenlp_parse_conll(conll_outputter$print(self$obj$get(i)))
          dt[, "doc_id" := i + 1L]
          setcolorder(dt, "doc_id")
          dt
        }
      ))
    }
  )
)
