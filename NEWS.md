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