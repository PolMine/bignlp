FROM ubuntu:latest

RUN apt-get update && apt-get upgrade -y && apt-get install -y gnupg

RUN apt install --no-install-recommends -y software-properties-common dirmngr

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN echo 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/' >> /etc/apt/sources.list

RUN apt-get update && \
  apt-get upgrade -y && \
  apt-get install -y openjdk-8-jdk r-base r-base-dev libsodium-dev libglib2.0-dev libxml2-dev && \
  apt-get install -y libcurl4-openssl-dev libssl-dev git libxslt1-dev pandoc-citeproc pandoc

RUN R CMD javareconf
  
RUN Rscript -e 'install.packages(c("rJava", "remotes", "RcppCWB", "cwbtools", "polmineR", "pbapply", "xslt", "webshot", "testthat"))'
RUN Rscript -e 'remotes::install_github("PolMine/bignlp", ref = "dev")' && \
  Rscript -e 'remotes::install_github("PolMine/cwbtools", ref = "dev")' && \
  Rscript -e 'remotes::install_github("PolMine/RcppCWB", ref = "dev")'

RUN apt-get install wget

RUN mkdir /opt/stanford-corenlp && \
  cd /opt/stanford-corenlp && \
  wget http://nlp.stanford.edu/software/stanford-corenlp-4.2.0.zip && \
  unzip stanford-corenlp-4.2.0.zip && \
  rm stanford-corenlp-4.2.0.zip

ENV CORENLP_DIR=/opt/stanford-corenlp/stanford-corenlp-4.2.0
RUN echo 'options(java.parameters = "-Xmx4g")' >> ~/.Rprofile
  
RUN cd /opt/stanford-corenlp/stanford-corenlp-4.2.0 && \
  wget http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models-english.jar
  
RUN cd /opt/stanford-corenlp/stanford-corenlp-4.2.0 && \
  wget http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models-german.jar && \
  unzip stanford-corenlp-4.2.0-models-german.jar StanfordCoreNLP-german.properties && \
  zip -d stanford-corenlp-4.2.0-models-german.jar StanfordCoreNLP-german.properties
