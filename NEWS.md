# bignlp v0.0.XXX

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