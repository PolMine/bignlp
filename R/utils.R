#' Check whether JVM is initialized.
#' 
#' The \code{jvm_is_initialized} utility function checks whether a JVM has been 
#' initialized already. Parallelisation is likely to fail if this is the case. The 
#' function issues a warning, if a JVM is already running.
#' 
#' @return A logical value, \code{TRUE} if JVM has been initialized, \code{FALSE} if
#' not.
#' @export jvm_is_initialized
jvm_is_initialized <- function(){
  java_status <- try(rJava::.jcheck(), silent = TRUE)
  if (class(java_status)[1] != "try-error"){
    message("JVM already up and running - parallelisation very likely to fail!")
    return(TRUE)
  } else {
    return(FALSE)
  }
}

.jvm_heap_space <- function(){
  jvm_mem <- J("java/lang/Runtime")$getRuntime()$maxMemory()
  class(jvm_mem) <- "object_size"
  format(jvm_mem, units = "MB")
}

.jvm_version <- function(){
  .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
}

.jvm_name <- function(){
  .jcall("java/lang/System", "S", "getProperty", "java.runtime.name")
}