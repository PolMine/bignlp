#' @include bignlp.R
NULL

#' Stanford CoreNLP Annotator Class.
#' 
#' @field tagger Class from Stanford CoreNLP to annotate text
#' @field xmlifier Class from Stanford CoreNLP to generate XML output
#' @field jsonifier Class from Stanford CoreNLP to generate JSON output
#' @field writer Class from Stanford CoreNLP to generate TXT output
#' @field append logical, whether to append output to destfile
#' @field method whith output format to use
#' @field cols_to_keep columns from output to keep
#' @field destfile filename
#' 
#' @export AnnotatorCoreNLP
#' @rdname AnnotatorCoreNLP
#' @importFrom jsonlite fromJSON
#' @importFrom stringi stri_match
#' @importFrom coreNLP getToken
#' @importFrom data.table as.data.table
#' @examples 
#' Sys.setenv("_JAVA_OPTIONS" = "")
#' options(java.parameters = "-Xmx4g")
#' if (getOption("bignlp.corenlp_dir") == "") corenlp_install(lang = "de")
#' 
#' txt <- "Das ist ein Satz. Und das ist ein zweiter Satz."
#' 
#' CNLP <- AnnotatorCoreNLP$new(
#'   method = "json",
#'   corenlp_dir = getOption("bignlp.corenlp_dir"),
#'   properties_file = corenlp_get_properties_file(lang = "de")
#'   )
#' CNLP$annotate(txt = txt)
#' CNLP$annotate(txt = txt, id = 15L)
#' 
#' CNLP <- AnnotatorCoreNLP$new(
#'   method = "xml",
#'   properties_file = corenlp_get_properties_file(lang = "de")
#' )
#' CNLP$annotate(txt = txt)
#' 
#' \dontrun{
#' # Java parallellization
#' options(java.parameters = "-Xmx4g")
#' library(polmineR)
#' library(bignlp)
#' properties <- list(
#'   "threads" = "6",
#'   "annotators" = "tokenize, ssplit, pos, lemma, ner",
#'   "tokenize.language" = "de",
#'   "tokenize.postProcessor" = "edu.stanford.nlp.international.german.process.GermanTokenizerPostProcessor",
#'   "pos.model" = "edu/stanford/nlp/models/pos-tagger/german-ud.tagger",
#'   "ner.model" = "edu/stanford/nlp/models/ner/german.distsim.crf.ser.gz",
#'   "ner.applyNumericClassifiers" = "false",
#'   "ner.applyFineGrained" = "false",
#'   "ner.useSUTime" = "false",
#'   "ner.nthreads" = "6"
#' )
#' CNLP <- AnnotatorCoreNLP$new(method = "json", properties_file = properties)
#' merkel <- corpus("GERMAPARL") %>%
#'   subset(speaker == "Angela Merkel" & interjection == "FALSE") %>%
#'   get_token_stream(beautify = TRUE, collapse = " ")
#' system.time(foo <- CNLP$annotate(merkel))
#' }
AnnotatorCoreNLP <- R6Class(
  
  classname = "CoreNLP",

  public = list(
    
    tagger = NULL,
    xmlifier = NULL,
    jsonifier = NULL,
    writer = NULL,
    append = NULL,
    method = NULL,
    cols_to_keep = NULL,
    destfile = NULL,
    logfile = NULL,
    target = NULL, # The total number of annotation tasks to perform
    current = NULL, # The current annotation task
    report_interval = NULL,
    gc_interval = NULL,


    initialize = function(
      corenlp_dir = getOption("bignlp.corenlp_dir"),
      properties_file, 
      method = c("txt", "json", "xml"),
      cols_to_keep = c("sentence", "id", "token", "pos", "ner"),
      destfile = NULL,
      logfile = NULL,
      target = NULL,
      report_interval = 1L,
      gc_interval = 100L
    ){
      
      self$cols_to_keep <- cols_to_keep
      
      jvm_status <- rJava::.jinit(force.init = TRUE) # does it harm when called again?
      java_version <- rJava::.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
      message("Status of the Java Virtual Machine: ", jvm_status)
      message("Java version: ", java_version)
      
      if (as.numeric(gsub("^(\\d+\\.\\d+)\\..*?$", "\\1", java_version)) != 1.8)
        warning("java version is not 1.8, but ", java_version, ". This may violate CoreNLP requirements.")

      stanford_path <- Sys.glob(paste0(corenlp_dir,"/*.jar"))
      rJava::.jaddClassPath(stanford_path)
      if (is.character(properties_file)){
        if (!file.exists(properties_file)) stop("Argument 'properties_file' does not refer to existing file.")
        rJava::.jaddClassPath(dirname(properties_file))
        self$tagger <- rJava::.jnew("edu.stanford.nlp.pipeline.StanfordCoreNLP", basename(properties_file))
      } else if (is.list(properties_file)){
        props <- rJava::.jnew("java.util.Properties")
        lapply(names(properties_file), function(property) props$put(property, properties_file[[property]]))
        self$tagger <- rJava::.jnew("edu.stanford.nlp.pipeline.StanfordCoreNLP", props)
      }

      self$method <- method
      if (self$method == "xml") self$xmlifier <- rJava::.jnew("edu.stanford.nlp.pipeline.XMLOutputter")
      if (self$method == "json") self$jsonifier <- rJava::.jnew("edu.stanford.nlp.pipeline.JSONOutputter")
      if (self$method == "txt") {
        self$writer <- new(J("java.io.PrintWriter"), rJava::.jnew("java.io.FileOutputStream", rJava::.jnew("java.io.File", destfile), TRUE))
      }

      self$append <- if (is.null(destfile)) FALSE else TRUE
      if (!is.null(destfile)) self$destfile <- destfile
      if (!is.null(logfile)) self$logfile <- logfile
      
      if (!is.null(target)) self$target <- target
      self$report_interval <- report_interval
      self$gc_interval <- gc_interval

      invisible( self )
    },
    
    annotation_to_xml = function(anno){
      doc <- rJava::.jcall(self$xmlifier, "Lnu/xom/Document;", "annotationToDoc", anno, self$tagger)
      xml <- rJava::.jcall(doc, "Ljava/lang/String;", "toXML")
      df <- coreNLP::getToken(coreNLP:::parseAnnoXML(xml))
      colnames(df) <- tolower(colnames(df))
      as.data.table(df[, self$cols_to_keep])
    },
    
    annotation_to_json = function(anno, id = NULL){
      json_string <- rJava::.jcall(self$jsonifier, "Ljava/lang/String;", "print", anno)
      json_string <- gsub("\\s+", " ", json_string)
      if (!is.null(id)){
        stopifnot(is.integer(id))
        json_string <- sprintf('{"id": %d, %s', id, substr(json_string, 2, nchar(json_string)))
      }
      if (!is.null(self$destfile)) cat(json_string, "\n", file = self$destfile, append = self$append)
      if (self$append == FALSE) json_string else NULL # return value
    },
    
    annotation_to_txt = function(anno){
      if (self$append == FALSE){
        self$writer <- rJava::.jnew("java.io.PrintWriter", filename <- tempfile())
      }
      rJava::.jcall(self$tagger, "V", "prettyPrint", anno, self$writer)
      if (self$append == FALSE) parse_pretty_print(filename) else NULL
    },

    annotate = function(txt, id = NULL, current = -1L, purge = TRUE){
      if (purge) txt <- purge(txt, replacements = corenlp_preprocessing_replacements, progress = FALSE)
      
      anno <- rJava::.jcall(self$tagger, "Ledu/stanford/nlp/pipeline/Annotation;", "process", txt)
      
      # output message upon every nth chunk processed
      share <- current / self$report_interval
      if (share - trunc(share) == 0){
        jvm_runtime <- rJava::J("java/lang/Runtime")$getRuntime()
        log_msg <- sprintf(
          "Time: %s | Chunks processed: %d/%d (%.1f %%)| Memory used: %d | Memory free: %d",
          format(Sys.time()), current, self$target, current / self$target * 100, jvm_runtime$totalMemory(), jvm_runtime$getRuntime()$freeMemory()
        )
        if (!is.null(self$logfile)){
          cat(log_msg, file = self$logfile, sep = "\n", append = TRUE)
        } else {
          message(log_msg)
        }
      }
      
      # garbage collection upon every nth chunk processed
      gc_div <- current / self$gc_interval
      if (gc_div - trunc(gc_div) == 0){
        message("... triggering JVM garbagee collection")
        rJava::J("java/lang/Runtime")$getRuntime()$gc()
      }

      switch(
        self$method,
        xml = self$annotation_to_xml(anno),
        json = self$annotation_to_json(anno, id = id),
        txt = self$annotation_to_txt(anno)
      )
    }
    

  )
)
