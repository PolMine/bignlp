#' Instantiate and manage properties.
#' 
#' StanfordCoreNLP uses a
#' [Properties](https://docs.oracle.com/javase/7/docs/api/java/util/Properties.html)
#' class object for the settings to configure the parser. This includes the
#' number of threads to use, an output directory etc. The `properties` function
#' instantiates this Java object from a properties file or a `list`. A set of
#' auxiliary functions can be used to set and get properties relevant for the
#' workflow envisaged by this package.
#' 
#' @param x Path of a propeties file or named list of properties.
#' @examples
#' # Instantiate properties from properties file
#' 
#' props_german <- system.file(
#'   package = "bignlp",
#'   "extdata", "properties_files",
#'   "corenlp-german-fast.properties"
#' )
#' plist <- parse_properties_file(props_german)
#' props <- properties(plist)
#' 
#' 
#' # Instantiate properties from list
#' 
#' properties <- list(
#'   "annotators" = "tokenize, ssplit, pos, lemma, ner",
#'   "tokenize.language" = "de",
#'   "pos.model" = "edu/stanford/nlp/models/pos-tagger/german-ud.tagger",
#'   "ner.model" = "edu/stanford/nlp/models/ner/german.distsim.crf.ser.gz",
#'   "ner.applyNumericClassifiers" = "false",
#'   "ner.applyFineGrained" = "false",
#'   "ner.useSUTime" = "false",
#'   "outputFormat" = "json",
#'   "outputDirectory" = "/Users/andreasblaette/Lab/tmp/corenlp/json"
#' )
#' props
#' @export properties
#' @details The `properties()` function will instantiate a properties object
#'   from a (named) `list` or a `character` vector with the path to a properties
#'   file.
#' @rdname properties
properties <- function(x){
  props <- rJava::.jnew("java.util.Properties")
  if (is.character(x)){
    if (length(x) != 1L){
      stop("If argument x is a character vector, it is required to have length 1 and to be path to a file.")
    }
    if (!file.exists(x)){
      stop(
        "If argument x is a character vector, it is required to be an a existing file. ",
        "File does not exist."
      )
    }
    x <- parse_properties_file(x)
  }
  lapply(names(x), function(property) props$put(property, x[[property]]))
  props
}

#' @details The `parse_properties_file()` function instantiates a properties
#'   object from a properties file or a named list.
#' @export parse_properties_file
#' @rdname properties
parse_properties_file <- function(x){
  # Using scan() rather than readLines() to remove commented lines and blank
  # lines from the outset
  pf_raw_vec <- scan(
    file = x,
    what = character(),
    sep = "\n",
    blank.lines.skip = TRUE,
    comment.char = "#",
    quiet = TRUE
  )
  pf_raw_list <- strsplit(x = pf_raw_vec, split = "\\s*=\\s*", perl = TRUE)
  setNames(
    object = lapply(pf_raw_list, `[[`, 2L),
    nm = sapply(pf_raw_list, `[[`, 1L)
  )
}

#' @details The number of threads used to process files in parallel is defined
#'   by the property 'threads'. Auxiliary functions `properties_get_threads()`
#'   and `properties_set_threads()` get and set the value.
#' @param k The number of threads used to process files. Needs to be a
#'   reasonable value, but need not be an integer value as value will be coerced
#'   to `character` vector anyway.
#' @param p A properties object.
#' @export properties_set_threads
#' @rdname properties
properties_set_threads <- function(p, k){p$put("threads", as.character(k)); invisible(p)}

#' @export properties_get_threads
#' @rdname properties
properties_get_threads <- function(p) as.integer(p$get("threads"))

#' @param dir The output directory for processed data.
#' @export properties_set_output_directory
#' @rdname properties
properties_set_output_directory <- function(p, dir){p$put("outputDirectory", dir); invisible(p)}

#' @export properties_get_output_directory
#' @rdname properties
properties_get_output_directory <- function(p) p$get("outputDirectory")

#' @param fmt The output format to use, a length-one `character` vector. Use 'json' for
#'   JSON output. 
#' @export properties_set_output_format
#' @rdname properties
properties_set_output_format <- function(p, fmt){p$put("outputFormat", fmt); invisible(p)}

#' @export properties_get_output_format
#' @rdname properties
properties_get_output_format <- function(p) p$get("outputFormat")

#' @export is_properties
#' @rdname properties
#' @importFrom rJava %instanceof%
is_properties <- function(p){
  if (class(p)[1] != "jobjRef") return(FALSE)
  p %instanceof% rJava::.jnew("java.util.Properties")
}