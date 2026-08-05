FROM rocker/r-ver:4.3.3

ENV DEBIAN_FRONTEND=noninteractive \
    PHYSIKOS_PROJECT_DIR=/app \
    HOST=0.0.0.0 \
    PORT=8000 \
    PLUMBER_DEBUG=false \
    PLUMBER_SWAGGER=false

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      build-essential \
      ca-certificates \
      libcurl4-openssl-dev \
      libsodium-dev \
      libssl-dev \
      libxml2-dev \
      texlive-fonts-recommended \
      texlive-latex-base \
      texlive-latex-extra \
      texlive-latex-recommended \
      texlive-pictures \
      texlive-plain-generic \
      texlive-science \
      zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "paquetes <- c('plumber', 'knitr', 'zip', 'jsonlite', 'callr'); install.packages(paquetes, repos='https://cloud.r-project.org', Ncpus=parallel::detectCores()); stopifnot(all(vapply(paquetes, requireNamespace, logical(1), quietly=TRUE)))"

WORKDIR /app

COPY api ./api
COPY R ./R
COPY recursos ./recursos
COPY prueba*.Rnw solucion*.Rnw ./

RUN mkdir -p /app/trabajos

EXPOSE 8000

CMD ["Rscript", "api/iniciar_api.R"]
