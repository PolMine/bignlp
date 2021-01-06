#' Auxiliary functions to get information on JVM.
#' 
#' Upon loading the bignlp package, a Java Virtual Machine (JVM) is initialized.
#' Some auxiliary functions provide information on the configuration of the JVM
#' and its status. Use the functions to check which Java is installed
#' (recommended: Oracle Java), the Java version and the heap space of the JVM.
#' 
#' @details `jvm_is_initialized` checks whether a JVM has been initialized
#'   already. The return value is a `logical` value, `TRUE` if JVM has been
#'   initialized, `FALSE` if not. As a JVM is initialized upon loading bignlp,
#'   the expected value is always `TRUE` if your installation of Java and rJave
#'   works.
#' @export jvm_is_initialized
#' @rdname jvm
#' @importFrom rJava .jcheck
#' @examples
#' jvm_is_initialized()
#' jvm_name()
#' jvm_version()
#' jvm_heap_space()
#' jvm_memory_usage()
jvm_is_initialized <- function(){
  java_status <- try(.jcheck(), silent = TRUE)
  if (class(java_status)[1] != "try-error") TRUE else FALSE
}

#' @rdname jvm
#' @export
jvm_name <- function(){
  .jcall("java/lang/System", "S", "getProperty", "java.runtime.name")
}


#' @rdname jvm
#' @export
jvm_version <- function(){
  .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
}

#' @rdname jvm
#' @export
jvm_heap_space <- function(units = "MB"){
  jvm_mem <- J("java/lang/Runtime")$getRuntime()$maxMemory()
  class(jvm_mem) <- "object_size"
  format(jvm_mem, units = units)
}


#' @rdname jvm
#' @export
jvm_memory_usage = function(){
  runtime <- rJava::J("java/lang/Runtime")$getRuntime()
  list(totalMemory = runtime$totalMemory(), freeMemory = runtime$getRuntime()$freeMemory())
}

.jvm_garbage_collection = function(){
  rJava::J("java/lang/Runtime")$getRuntime()$gc()
}

