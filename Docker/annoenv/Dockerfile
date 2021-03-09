FROM ubuntu:latest

RUN apt-get update && apt-get upgrade -y && apt-get install -y gnupg

RUN apt install --no-install-recommends -y software-properties-common dirmngr

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN echo 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/' >> /etc/apt/sources.list

RUN apt-get update && \
  apt-get upgrade -y && \
  apt-get install -y openjdk-8-jdk
# RUN apt-get r-base r-base-dev 
RUN apt-get install -y libsodium-dev libglib2.0-dev libxml2-dev && \
  apt-get install -y libcurl4-openssl-dev libssl-dev git libxslt1-dev pandoc-citeproc pandoc


RUN apt-get install -y wget build-essential gfortran
RUN apt-get install -y libreadline-dev
RUN apt-get install -y libbz2-dev
RUN apt-get install -y liblzma-dev

RUN cd ~ && \
  mkdir Lab && \
  mkdir Lab/R && \
  cd ~/Lab/R && \
  wget --no-check-certificate https://stat.ethz.ch/CRAN/src/base/R-4/R-4.0.4.tar.gz && \
  tar xfv R-4.0.4.tar.gz && \
  cd R-4.0.4 && \
  ./configure \
    --prefix=/opt/R/R-4.0.4 \
    --enable-memory-profiling \
    --enable-R-shlib \
    --with-blas \
    --with-lapack && \
  make && \
  make install

ENV PATH="/opt/R/R-4.0.4/bin:${PATH}"

RUN R CMD javareconf
  
RUN Rscript -e 'install.packages(c("rJava", "remotes", "RcppCWB", "cwbtools", "polmineR", "pbapply", "xslt", "webshot", "testthat"), repos = "https://cran.uni-muenster.de")'
RUN Rscript -e 'remotes::install_github("PolMine/bignlp", ref = "dev")'
RUN Rscript -e 'remotes::install_github("PolMine/RcppCWB", ref = "dev")'
RUN Rscript -e 'remotes::install_github("PolMine/cwbtools", ref = "dev")'
RUN Rscript -e 'remotes::install_github("PolMine/GermaParl", ref = "dev")'

RUN apt-get install wget

RUN apt-get install zip unzip

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
