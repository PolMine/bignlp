FROM ubuntu:latest

# We use an Ubuntu image due to the availability of Java 8;
# it is not available for Debian (as far as I can see)

# We build R ourselves (and do not use rocker/r-base or apt-get install r-base)
# to ensure that this image can also be build on Apple Silicon
# (arm64 builds may be around, but I could not find it)


RUN apt-get update && \
  apt-get upgrade -y && \
  apt-get install -y gnupg # maybe installing gnupg is not necessary when building R 

RUN apt install --no-install-recommends -y software-properties-common dirmngr # potentially not necessary

RUN apt-get update && apt-get upgrade -y && apt-get install -y openjdk-8-jdk  # Java 8, required by rJava and bignlp
RUN apt-get install -y git wget zip unzip libsodium-dev libglib2.0-dev libxml2-dev libcurl4-openssl-dev libssl-dev  libxslt1-dev pandoc-citeproc pandoc

RUN apt-get install -y build-essential gfortran # compilers required to build R
RUN apt-get install -y libreadline-dev libbz2-dev liblzma-dev # R dependencies

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
  
RUN Rscript -e 'install.packages(c("rJava", "remotes", "pbapply", "xslt", "webshot", "testthat"), repos = "https://cran.uni-muenster.de")'

# Install RcppCWB first because the other PolMine packages depend on it
RUN Rscript -e 'remotes::install_github("PolMine/RcppCWB", ref = "dev")' && \
  Rscript -e 'remotes::install_github("PolMine/bignlp", ref = "dev")' && \
  Rscript -e 'remotes::install_github("PolMine/cwbtools", ref = "dev")' && \
  Rscript -e 'remotes::install_github("PolMine/GermaParl", ref = "dev")'

RUN mkdir /opt/stanford-corenlp && \
  cd /opt/stanford-corenlp && \
  wget http://nlp.stanford.edu/software/stanford-corenlp-4.2.0.zip && \
  unzip stanford-corenlp-4.2.0.zip && \
  rm stanford-corenlp-4.2.0.zip

# jars with models for specific languages could be put into another Dockerfile extending this one 
RUN cd /opt/stanford-corenlp/stanford-corenlp-4.2.0 && \
  wget http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models-english.jar
  
RUN cd /opt/stanford-corenlp/stanford-corenlp-4.2.0 && \
  wget http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models-german.jar && \
  unzip stanford-corenlp-4.2.0-models-german.jar StanfordCoreNLP-german.properties && \
  zip -d stanford-corenlp-4.2.0-models-german.jar StanfordCoreNLP-german.properties

# Settings required by bignlp
ENV CORENLP_DIR=/opt/stanford-corenlp/stanford-corenlp-4.2.0
RUN echo 'options(java.parameters = "-Xmx4g")' >> ~/.Rprofile

RUN apt-get -y install locales
RUN locale-gen en_US.UTF-8
RUN update-locale LANG=en_US.UTF-8
RUN echo "LANG=en_US.UTF-8" >> ~/.Renviron

RUN Rscript -e 'install.packages(c("remotes", "pbapply", "xml2", "xslt", "magrittr"),  repos = "https://cran.uni-muenster.de")'
RUN Rscript -e 'remotes::install_github("PolMine/RcppCWB", ref = "dev")'
RUN Rscript -e 'remotes::install_github("PolMine/cwbtools", ref = "dev")'
RUN Rscript -e 'remotes::install_github("PolMine/bignlp", ref = "ner")'
RUN Rscript -e 'remotes::install_github("PolMine/polmineR", ref = "dev")'
RUN Rscript -e 'remotes::install_github("PolMine/GermaParl", ref = "dev")'

RUN mkdir -p /xml
RUN mkdir -p /vrt
