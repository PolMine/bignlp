# We use an Ubuntu image due to the availability of Java 8;
# it is not available for Debian (as far as I can see)

FROM ubuntu:latest

RUN apt-get update && \
  apt-get upgrade -y && \
  apt-get install -y gnupg # maybe installing gnupg is not necessary when building R 

RUN apt install --no-install-recommends -y software-properties-common dirmngr # potentially not necessary

RUN apt-get update && \
  apt-get upgrade -y && \
  apt-get install -y openjdk-8-jdk && \ # Java 8, required by rJava and bignlp
  apt-get install -y git && \
  apt-get install -y libsodium-dev libglib2.0-dev libxml2-dev && \
  apt-get install -y libcurl4-openssl-dev libssl-dev  libxslt1-dev pandoc-citeproc pandoc

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
