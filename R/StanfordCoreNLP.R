#' @include bignlp.R
NULL

#' StanfordCoreNLP Annotator Class.
#' 
#' The StanfordCoreNLP class exposes the pipeline of StanfordCoreNLP for
#' processing text. Its main functionality is exposed to R by way of an R6
#' class. The special focus of this implementation is to use the multithreading
#' capacities of StanfordCoreNLP from R.
#' 
#' The StanfordCorenNLP pipeline uses multithreading (a) by processing files in
#' parallel. This requires that chunks of text are present as files in one
#' directory. The `$processFiles()` method exposes this functionality. The
#' number of threads to be used is controlled by setting the property "threads"
#' accordingly, see examples and vignette. This approach is fast and memory
#' efficient, as it allows effectively a line-by-line approach.
#' 
#' The second approach to multithreading is (b) to process sentences in
#' parallel, i.e. after tokenization and sentence segmentation further annotation
#' tasks such as POS annotation and NER recognition are carried out in parallel.
#' Whether this parallelization is used is controlled by setting the properties
#' "pos.nthreads", "ner.nthreads" and alike. See examples.
#' 
#' @field pipeline Instance of the StanfordCoreNLP class.
#' @field outputter An outputter (JSON, CoNLL, XML) to generate string output
#'   from annotations.
#' @field output_format Which output format to use ("json", "xml", "conll").
#' @field properties A Properties Java object to control multithreading.
#' 
#' @export StanfordCoreNLP
#' @rdname StanfordCoreNLP
#' @importFrom R6 R6Class
#' @importFrom rJava .jnew J .jcall .jaddClassPath
#' @importFrom cli cli_alert cli_alert_success cli_alert_warning
#' @examples 
#' Sys.setenv("_JAVA_OPTIONS" = "")
#' options(java.parameters = "-Xmx4g")
#' if (getOption("bignlp.corenlp_dir") == "") corenlp_install(lang = "de")
#' 
#' txt <- "Das ist ein Satz. Und das ist ein zweiter Satz."
#' 
#' props_file <- corenlp_get_properties_file(lang = "de")
#' CNLP <- StanfordCoreNLP$new(output_format = "json", properties = props_file)
#' j <- CNLP$annotate(txt = txt)
#' 
#' CNLP <- StanfordCoreNLP$new(output_format = "xml", properties = props_file)
#' x <- CNLP$annotate(txt = txt)
#' 
#' CNLP <- StanfordCoreNLP$new(output_format = "conll", properties = props_file)
#' c <- CNLP$annotate(txt = txt)
#' 
#' 
#' # Java parallellization - processing sentences in parallel
#' 
#' library(data.table)
#' reuters_txt <- readLines(system.file(package = "bignlp", "extdata", "txt", "reuters.txt"))
#' dt <- data.table(id = 1L:length(reuters_txt), text = reuters_txt)
#' 
#' options(java.parameters = "-Xmx4g")
#' 
#' n_cores <- as.character(parallel::detectCores() - 1L)
#' properties_file <- corenlp_get_properties_file(lang = "en", fast = TRUE)
#' props <- properties(properties_file)
#' props$put("pos.nthreads", as.character(parallel::detectCores() - 1L))
#' props$put("ner.nthreads", as.character(parallel::detectCores() - 1L))
#' 
#' CNLP <- StanfordCoreNLP$new(output_format = "conll", properties = props)
#' 
#' y <- CNLP$annotate(dt[1][["text"]])
StanfordCoreNLP <- R6Class(
  
  classname = "StanfordCoreNLP",

  public = list(
    
    pipeline = NULL, # Instance of StanfordCoreNLP will be here
    outputter = NULL,
    output_format = NULL,
    properties = NULL,


    #' @param corenlp_dir Directory where StanfordCoreNLP resides.
    #' @param properties Either the filename of a properties file or a Java
    #'   properties object.
    #' @param output_format Either "json", "xml", "conll".
    initialize = function(
      corenlp_dir = getOption("bignlp.corenlp_dir"),
      properties, 
      output_format = c("xml", "json", "conll")
    ){
      
      # Check that Java runtime meets requirements ------------------------------
      
      if (startsWith(jvm_name(), "OpenJDK")){
        cli_alert(sprintf("JVM runtime name: %s", jvm_name()))
        cli_alert(sprintf("JVM version: %s", jvm_version()))
        cli_alert_warning("Recommended: Oracle Java 8")
      } else {
        cli_alert_success(sprintf("JVM runtime name: %s", jvm_name()))
        if (as.numeric(gsub("^(\\d+\\.\\d+)\\..*?$", "\\1", jvm_version())) != 1.8){
          cli_alert_warning("Java version: %s - recommended: 1.8", jvm_version())
        } else {
          cli_alert_success("Java version: %s", jvm_version())
        }
      }
      
      if (as.numeric(gsub("^(\\d+)\\s.*$", "\\1", jvm_heap_space(units = "Gb"))) < 4){
        cli_alert_warning(
          sprintf(
            "JVM maximum heap space: %s - recommended: 4 GB",
            jvm_heap_space()
          )
        )
      } else {
        cli_alert_success(sprintf("JVM maximum heap space: %s", jvm_heap_space()))
      }
      
      # Initialize StanfordCoreNLP class ----------------------------------------------

      stanford_path <- Sys.glob(paste0(corenlp_dir,"/*.jar"))
      .jaddClassPath(stanford_path)
      if (is.character(properties) || is.list(properties)){
        self$properties <- properties(x = properties)
      } else if (is_properties(properties)){
        self$properties <- properties
      }
      self$pipeline <- .jnew("edu.stanford.nlp.pipeline.StanfordCoreNLP", self$properties)
      

      # Instantiate outputter -------------------------------------------
      
      if (length(output_format) != 1L){
        stop("output_format required to be a length-one character vector")
      }
      if (!output_format %in% c("xml", "json", "conll")){
        stop("output_format required to be one of the following: xml | json | conll")
      }
      
      self$output_format <- output_format
      self$outputter <- switch(
        self$output_format,
        "xml" = .jnew("edu.stanford.nlp.pipeline.XMLOutputter"),
        "json" = .jnew("edu.stanford.nlp.pipeline.JSONOutputter"),
        "conll" = .jnew("edu.stanford.nlp.pipeline.CoNLLOutputter")
      )

      invisible( self )
    },

    
    #' @description Annotate a string.
    #' @param txt A (length-one) `character` vector to process.
    #' @param id An ID to prepend.
    #' @param purge Whether to postprocess output.
    #' @return If output_format is "json" or "xml", a string is returned, if output_format is 
    #'   "conll", a `data.frame`.
    annotate = function(txt, purge = TRUE){
      if (purge){
        txt <- purge(txt, replacements = corenlp_preprocessing_replacements, progress = FALSE)
      }
      
      anno <- .jcall(self$pipeline, "Ledu/stanford/nlp/pipeline/Annotation;", "process", txt)
      
      if (self$output_format == "json"){
        return(.jcall(self$outputter, "Ljava/lang/String;", "print", anno))
      } else if (self$output_format == "xml"){
        doc <- .jcall(self$outputter, "Lnu/xom/Document;", "annotationToDoc", anno, self$pipeline)
        return(.jcall(doc, "Ljava/lang/String;", "toXML"))
      } else if (self$output_format == "conll"){
        conll_str <- self$outputter$print(anno)
        conll_lines <- strsplit(x = conll_str, split = "\n", fixed = TRUE)[[1L]]
        cols <- strsplit(conll_lines, split = "\t", fixed = TRUE)
        df <- data.frame(do.call(rbind, cols))
        df[[1]] <- as.integer(df[[1]])
        return(df)
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
      
      self$properties$put("outputDirectory", file.path(dir))
      
      self$pipeline$processFiles(
        file_collection,
        6L, # no effect
        FALSE,
        J("java/util/Optional")$empty()
      )
      invisible(paste(Sys.glob(paste0(dir, "/*.txt")), self$output_format, sep = "."))
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
