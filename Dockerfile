FROM reg.jalgos.ai/r-jalgos:4.0.2

WORKDIR /build
COPY ci /build/ci

RUN install2.r \
  --skipinstalled \
  --error \
  --deps TRUE \
  --repos https://cran.rstudio.com \
  RJSONIO \
  data.table \
  Matrix \
  MASS \
  track

RUN rm -rf lib && \
  mkdir -p lib && \
  Rscript ci/dependencies.R

COPY . /build
