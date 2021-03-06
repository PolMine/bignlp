% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jvm.R
\name{jvm_is_initialized}
\alias{jvm_is_initialized}
\alias{jvm_name}
\alias{jvm_version}
\alias{jvm_heap_space}
\alias{jvm_memory_usage}
\alias{jvm_available_processors}
\title{Auxiliary functions to get information on JVM.}
\usage{
jvm_is_initialized()

jvm_name()

jvm_version()

jvm_heap_space(units = "MB")

jvm_memory_usage()

jvm_available_processors()
}
\arguments{
\item{units}{See documentation of format.object_size, typically either "MB"
or "GB".}
}
\description{
Upon loading the bignlp package, a Java Virtual Machine (JVM) is initialized.
Some auxiliary functions provide information on the configuration of the JVM
and its status. Use the functions to check which Java is installed
(recommended: Oracle Java), the Java version and the heap space of the JVM.
}
\details{
\code{jvm_is_initialized} checks whether a JVM has been initialized
already. The return value is a \code{logical} value, \code{TRUE} if JVM has been
initialized, \code{FALSE} if not. As a JVM is initialized upon loading bignlp,
the expected value is always \code{TRUE} if your installation of Java and rJave
works.
}
\examples{
jvm_is_initialized()
jvm_name()
jvm_version()
jvm_heap_space()
jvm_memory_usage()
}
