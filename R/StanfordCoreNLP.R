#' @include bignlp.R
NULL

#' StanfordCoreNLP Annotator Class.
#' 
#' The StanfordCoreNLP class is main pipeline of StanfordCoreNLP for processing
#' text. Its main functionality is exposed to R by way of an R6 class. The
#' special focus of this implementation is to use the multithreading capacities of 
#' StanfordCoreNLP from R.
#' 
#' See https://stackoverflow.com/questions/51636158/what-is-the-default-number-of-threads-in-stanford-corenlp
#' XML output could be parsed using `coreNLP:::parseAnnoXML(xml) %>% coreNLP::getToken()`.
#' 
#' @field pipeline Instance of the StanfordCoreNLP class.
#' @field outputter An outputter (JSON, CoNLL, XML) to generate string output
#'   from annotations.
#' @field writer Class from Stanford CoreNLP to generate TXT output
#' @field output_format Which output format to use ("json", "xml", "conll").
#' @field logfile Where to write logs.
#' 
#' @export StanfordCoreNLP
#' @rdname StanfordCoreNLP
#' @importFrom jsonlite fromJSON
#' @importFrom stringi stri_match
#' @importFrom data.table as.data.table
#' @importFrom rJava .jnew J .jcall
#' @examples 
#' Sys.setenv("_JAVA_OPTIONS" = "")
#' options(java.parameters = "-Xmx4g")
#' if (getOption("bignlp.corenlp_dir") == "") corenlp_install(lang = "de")
#' 
#' txt <- "Das ist ein Satz. Und das ist ein zweiter Satz."
#' 
#' CNLP <- StanfordCoreNLP$new(
#'   method = "json",
#'   corenlp_dir = getOption("bignlp.corenlp_dir"),
#'   properties = corenlp_get_properties_file(lang = "de")
#'   )
#' CNLP$annotate(txt = txt)
#' CNLP$annotate(txt = txt, id = 15L)
#' 
#' CNLP <- StanfordCoreNLP$new(
#'   method = "xml",
#'   properties = corenlp_get_properties_file(lang = "de")
#' )
#' CNLP$annotate(txt = txt)
#' 
#' \dontrun{
#' # Java parallellization
#' options(java.parameters = "-Xmx4g")
#' library(polmineR)
#' library(bignlp)
#' properties <- list(
#'   "annotators" = "tokenize, ssplit, pos, lemma, ner",
#'   "tokenize.language" = "de",
#'   "tokenize.postProcessor" = "edu.stanford.nlp.international.german.process.GermanTokenizerPostProcessor",
#'   "pos.model" = "edu/stanford/nlp/models/pos-tagger/german-ud.tagger",
#'   "pos.nthreads" = "7",
#'   "ner.model" = "edu/stanford/nlp/models/ner/german.distsim.crf.ser.gz",
#'   "ner.applyNumericClassifiers" = "false",
#'   "ner.applyFineGrained" = "false",
#'   "ner.useSUTime" = "false",
#'   "ner.nthreads" = "7"
#' )
#' CNLP <- StanfordCoreNLP$new(method = "json", properties_file = properties)
#' 
#' merkel <- corpus("GERMAPARL") %>%
#'   subset(speaker == "Angela Merkel" & interjection == "FALSE") %>%
#'   get_token_stream(beautify = TRUE, collapse = " ")
#'   
#' system.time(foo <- CNLP$annotate(merkel))
#' }
StanfordCoreNLP <- R6Class(
  
  classname = "StanfordCoreNLP",

  public = list(
    
    pipeline = NULL, # Instance of StanfordCoreNLP will be here
    outputter = NULL,
    output_format = NULL,
    logfile = NULL,


    #' @param corenlp_dir Directory where StanfordCoreNLP resides.
    #' @param properties Either the filename of a properties file or a Java
    #'   properties object.
    #' @param output_format Either "json", "xml", "conll" or "txt", defaults to NULL.
    #' @param logfile Where to write logs.
    initialize = function(
      corenlp_dir = getOption("bignlp.corenlp_dir"),
      properties, 
      output_format = NULL,
      logfile = NULL
    ){
      
      if (startsWith(.jvm_name(), "OpenJDK")){
        cli_alert(sprintf("JVM runtime name: %s", .jvm_name()))
        cli_alert(sprintf("JVM version: %s", .jvm_version()))
        cli_alert_warning("Recommended: Oracle Java 8")
      } else {
        cli_alert_success(sprintf("JVM runtime name: %s", .jvm_name()))
        if (as.numeric(gsub("^(\\d+\\.\\d+)\\..*?$", "\\1", .jvm_version())) != 1.8)
          cli_alert_warning("Java version is %s - recommended: 1.8", .jvm_version())
      }
      cli_alert(sprintf("JVM runtime name: ", .jvm_name()))
      message(
        ,
        ,
         "(recommended: Oracle Java)" else ""
      )
      
      cli_alert("JVM maximum heap space: %s", .jvm_heap_space())

      stanford_path <- Sys.glob(paste0(corenlp_dir,"/*.jar"))
      rJava::.jaddClassPath(stanford_path)
      if (is.character(properties)){
        if (!file.exists(properties)) stop("Argument 'properties' does not refer to existing file.")
        rJava::.jaddClassPath(dirname(properties))
        self$pipeline <- rJava::.jnew("edu.stanford.nlp.pipeline.StanfordCoreNLP", basename(properties))
      } else if (is.list(properties)){
        props <- rJava::.jnew("java.util.Properties")
        lapply(names(properties), function(property) props$put(property, properties[[property]]))
        self$pipeline <- rJava::.jnew("edu.stanford.nlp.pipeline.StanfordCoreNLP", props)
      } else if (is_properties(properties)){
        self$pipeline <- rJava::.jnew("edu.stanford.nlp.pipeline.StanfordCoreNLP", properties)
      }

      # Instantiate output method -------------------------------------------
      
      self$method <- method
      if (!is.null(self$method)){
        if (self$method == "xml") self$xmlifier <- .jnew("edu.stanford.nlp.pipeline.XMLOutputter")
        if (self$method == "json") self$jsonifier <- .jnew("edu.stanford.nlp.pipeline.JSONOutputter")
        if (self$method == "txt") {
          self$writer <- new(J("java.io.PrintWriter"), .jnew("java.io.FileOutputStream", .jnew("java.io.File", destfile), TRUE))
        }
      }
      
      # Fill fields ------------------------------------------------------

      self$append <- if (is.null(destfile)) FALSE else TRUE
      if (!is.null(destfile)) self$destfile <- destfile
      if (!is.null(logfile)) self$logfile <- logfile
      
      if (!is.null(target)) self$target <- target
      self$report_interval <- report_interval
      self$gc_interval <- gc_interval

      invisible( self )
    },

    
    #' @description Get information on memory usage within the JVM and 
    #'   show it as a message or append it to logfile.
    #' @param logfile Name of a logfile.
    show_memory_usage = function(logfile = NULL){
      runtime <- rJava::J("java/lang/Runtime")$getRuntime()
      msg <- sprintf(
        "Time: %s | Memory used: %f | Memory free: %f",
        format(Sys.time()), runtime$totalMemory(), runtime$getRuntime()$freeMemory()
      )
      if (is.null(self$logfile)){
        message(log_msg)
      } else {
        cat(msg, file = self$logfile, sep = "\n", append = TRUE)
      }
    },
    
    #' @description Trigger garbage collection in JVM.
    collect_garbage = function(){
      rJava::J("java/lang/Runtime")$getRuntime()$gc()
    },
    
    #' @description Annotate a string.
    #' @param txt A (length-one) `character` vector to process.
    #' @param id An ID to prepend.
    #' @param current Where process stands.
    #' @param purge Whether to postprocess output.
    annotate = function(txt, id = NULL, purge = TRUE){
      if (purge){
        txt <- purge(txt, replacements = corenlp_preprocessing_replacements, progress = FALSE)
      }
      
      anno <- .jcall(self$pipeline, "Ledu/stanford/nlp/pipeline/Annotation;", "process", txt)
      
      if (self$method == "json"){
        return(.jcall(self$jsonifier, "Ljava/lang/String;", "print", anno))
      } else if (self$method == "xml"){
        doc <- .jcall(self$xmlifier, "Lnu/xom/Document;", "annotationToDoc", anno, self$pipeline)
        return(.jcall(doc, "Ljava/lang/String;", "toXML"))
        # This result can be parsed to a data.frame using:
        # coreNLP:::parseAnnoXML(xml) %>% coreNLP::getToken()
      } else if (self$method == "txt"){
        writer <- rJava::.jnew("java.io.PrintWriter", filename <- tempfile())
        .jcall(self$pipeline, "V", "prettyPrint", anno, writer)
        return(readLines(filename))
      } else if (self$method == "conll"){
        outputter <- .jnew("edu.stanford.nlp.pipeline.CoNLLOutputter")
        y <- outputter$print(anno)
        a <- strsplit(x = y, split = "\n", fixed = TRUE)[[1L]]
        b <- strsplit(a, split = "\t", fixed = TRUE)
        d <- data.frame(do.call(rbind, b))
        d[[1]] <- as.integer(d[[1]])
      }
    },
    
    #' @description Process all files in the stated directory (argument `dir`).
    #'   Parallel processing is possible if a 'threads' key the properties
    #'   object is defined and sets a number of cores to use.
    #' @return The method returns (invisibly) the files expected to result from
    #'   the tagging exercise.
    #' @param dir Directory with files to process (in parallel).
    process_files = function(dir){
      
      file_collection <- .jnew(
        "edu/stanford/nlp/io/FileSequentialCollection",
        .jnew("java/io/File", file.path(dir)),
        .jnew("java/lang/String", "txt"),
        FALSE
      )
      
      self$pipeline$getProperties()$put("outputDirectory", file.path(dir))
      
      self$pipeline$processFiles(
        file_collection,
        6L, # no effect
        FALSE,
        J("java/util/Optional")$empty()
      )
      invisible(paste(Sys.glob(paste0(dir, "/*.txt")), "json", sep = "."))
    },
    
    #' @description Set whether calls of the class shall be verbose.
    #' @param x A `logical` value. If `TRUE`, all status messages are shown, if
    #'   `FALSE`, only error messages are displayed.
    #' @return The class is returned invisibly
    verbose = function(x){
      stopifnot(length(x) == 1L, is.logical(x))
      if (x){
        redwood_config <- .jnew("edu/stanford/nlp/util/logging/RedwoodConfiguration")
        redwood_config$standard()$apply()
      } else {
        redwood_config <- .jnew("edu/stanford/nlp/util/logging/RedwoodConfiguration")
        redwood_config$errorLevel()$apply()
      }
      invisible(self)
    }
  )
)
