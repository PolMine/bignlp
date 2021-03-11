# bignlp v0.1.2.9001 - v0.1.2.9003

* Added lifecylce badge 'maturing'.
* Add repostatus badge 'Active'.
* Method `corenlp_annotate()` for `data.table`: Argument `properties` replaced by argument `pipe`. If value of `pipe` is a `list` or  `properties` object, a pipe will be instantiated, but it is now also possible to pass in an instantiated pipe. Furthermore, the method will call `$annotate()` method of pipe for parallel processing if argument `threads` is larger than 1.
*  New method `corenlp_annotate()` for `xml_document` objects.
* A Dockerfile in ./Docker/annoenv can be used to run bignlp in a dockerized environment.
* Method `corenlp_annotate()` accepts argument `ner` that will turn ner column into ner tags, removing the column.
* The `corenlp_parse_conll()` method will no be robust if the file with annotated output is empty.

# bignlp v0.1.2

* This version adapts the package such that GitHub Actions can be used for CI.
* Downloading CoreNLP in the vignette is performed without downloading a model jar.
* `curl::curl_download()` replaces `download.file()` to get CoreNLP.
* The jars of CoreNLP are added to the Java classpath conditional on their presence upon loading CoreNLP.
* The `corenlp_install()` function has a new argument `verbose`.
* The properties file for English included in the package is limited to the essential minimum (tokenisation, sentence segmentation) to make it useful for testing.
* Packages 'rmarkdown' and 'webshot' have been added to suggests section in DESCRIPTION to ensure that package vignette can be built.
* The check when initializing the `StanfordCoreNLP` class whether sufficient heap space has been allocated would fail when a numeric value was returned (such as "455.5 Gb"). Fixed.

# bignlp v0.1.1

* As a safeguard that the order of segments of text is maintained, names of temporary txt files written by the `segment()` function are padded with leading zeros (#18).
* Whether to call the `purge()` function to preprocess an input string is not exposed by the `corenlp_annotate()` method for `data.table`. Yet the default approach of the `StanfordCoreNLP$annotate()` worker to call `purge()`, leading to results that may differ from results of `StanfordCoreNLP$process_files()` (#20). To harmonize the two approaches, `corenlp_annotate()` for `data.table` objects now has the argument `purge` that will be passed on.
* The option `bignlp.properties_file` is not set upon loading the packqge any more and it is not used as the default value of argument `corenlp_dir` of `corenlp_annotate()` any more. An implicit setting of properties contradicts the logic of the (new) bignlp package that requires an explicit and conscious handling of properties.
* The `$initialize()` method of the `StanfordCoreNLP` class will assign the value of the argument `output_format` to the properties object. It is not necessary to set the output format seperately for the properties (#22).
* The `corenlp_parse_conll()` function now also accepts a list of `character` vectors as input. If `x` is a list, it will be unlisted.
* The `$annotate()` method of the `StanfordCoreNLP` class has been renamed as `$process()` method to reflect that the Java method called is `process`. This avoids confusion with the (Java) method `annotate` that also exists, and is a basis for turning `StanfordCoreNLP` into the superclass of the `AnnotationPipeline` class at a certain stage.
* The `corenlp_parse_json()` function will now add column names in line with the documentation of the `CoNLLOutputter` class (#23).
* The `corenlp_parse_json()` function will now assign a column with the document id as column 'doc' to the `data.table` that is prepared.
* The name of the column "id" of table input is changed to "doc_id" throughout to avoid confusion with the sentence token ids in the output of the CoNLLOutputter.
* The `corenlp_install()` set the option "bignlp.corenlp_dir" to a directory within the 
package, not to the location designated by argument `loc`. Fixed.
* To avoid setting the Java heap space to 4GB upon loading the package, the pipeline used in examples had be reduced to a minimum that works with 512MB (#16).
* The `StanfordCoreNLP` class now inherits from the `AnnotationPipeline` class, exposing the `$annotate()` method for parallel processing.
* A new `AnnotationList` class is introduced to manage annotation objects.
* The `$annotate()` method of the `AnnotationPipeline` class will return an `AnnotationList`, the `$as.matrix()` method of this class has been removed; its functionality is assumed by the `$as.data.table()` method of the `AnnotationList` class.
* The jars of CoreNLP are put on the classpath upon loading the package (experimental still).
* A new, experimental function `as.Annotation()` turns tabular data for tokenized text into an Annotation object (#17).
* Excerpt from REUTERS corpus is included as sample data, and a minimal documentation has been written. The 'data-raw' folder includes a file on data preparation.
* The vignette now includes an explanation of two different workflows how to add an annotation layer to corpus data that has already been tokenized.
* Continuous integration with Travis CI is replaced by CI with GitHub Actions.

# bignlp v0.1.0

Fist release with Java parallelization.

# bignlp v0.0.10

* New function to instantiate a Java `Properties` object, including `properties()`, `parse_properties_file()`.
* Removed package 'jobstatus' from the suggested packages. The status of the package remains to be experimental
and it is not sufficiently mature to rely on it in the long run (see [jobstatus pkg at GitHub](https://github.com/ropenscilabs/jobstatus))
* New function `mince()` for new workflow for parallel processing.
* New method `$process_files()` of `AnnotatorCoreNLP` class.
* New method `$verbose()` of `AnnotatorCoreNLP` class.
* Upon loading the bignlp package, a JVM is initialized using `rJava::.jpackage()`.
* A startup message on the memory allocated to the JVM is issued to give the user a hint whether that is enough.
* Any parallelization using the packages futures or jobstatus, or that relied on creating various parallel JVMs are removed.

# bignlp v0.0.9

* New slots logfile, target (total number of annotation tasks) and current (annotation task) have been added to the AnnotatorCoreNLP class.

# bignlp v0.0.8

* The byline processing mode of `corenlp_annotate()` recognizes whether chunk data is wrapped into quotes, and removes quotes if necessary.


# bignlp v0.0.7

* updated vignette so that annex explains installation of CoreNLP v3.9.2 (2018-10-05)
* Hard coded download paths within the package have been updated, so that CoreNLP v3.9.2 (2018-10-05) is downloaded.
* An error the occurred when multiple files served as input for `corenlp_annotate` has been removed (by wrapping checks whether files already exist into `any`).
* The properties file for German that excludes dependency parsing has been updated to work with CoreNLP v3.9.2


# bignlp 0.0.6

* Unicode character 'non-breaking hyphen' (\u2011) added to corenlp_preprocessing_replacements,
as it throws a warning from CoreNLP
* Checks added to corenlp_annotate()-methods whether output files already exists; existing files
are deleted to avoid that an already existing file is appended unwantedly.

# bignlp 0.0.5

* progess report for multicore operation using jobstatus package
* byline processing for corenlp_annotate,character-method
* `chunk_table_get_nrow`-function
* `chunk_table_split`-function

# bignlp 0.0.4

* Function corenlp_install added to prepre Travis CI checks
* Functions `corenlp_get_jar_dir()` and `corenlp_get_properties_file() added


# bignlp 0.0.3
* The functions `corenlp_annotate` and `corenlp_parse_ndjson` will now return the target files, which may be 
 helpful when using the functions in a pipe.
* Package settings now describe the CoreNLP directory. Upon loading the package, you see whether the coreNLP package is present and includes the CoreNLP jar directory.

 
# bignlp 0.0.2

* Vignette introduced to explain how the package works.


# bignlp 0.0.1

* functionality extracted from ctk
