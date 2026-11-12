# fortuneCougar on a real R server (Hugging Face Spaces, Render, Fly.io, Cloud Run,
# any Docker host). Unlike the GitHub Pages build, all players share one SQLite
# database: leaderboard, community clusters and linked jackpots are global.
#   docker build -t fortunecougar . && docker run -p 7860:7860 fortunecougar
FROM rocker/r-ver:4.4.2

# binary packages from Posit Package Manager (set up by rocker/r-ver)
RUN install2.r --error --skipinstalled shiny bslib htmltools jsonlite DBI RSQLite \
    && rm -rf /tmp/downloaded_packages

RUN useradd -m -u 1000 app && mkdir -p /data && chown app /data
WORKDIR /home/app
COPY --chown=app DESCRIPTION NAMESPACE app.R ./
COPY --chown=app R ./R
COPY --chown=app inst ./inst
USER app

# Mount a volume on /data to keep players between restarts
ENV FC_DB_PATH=/data/fortuneCougar.sqlite PORT=7860
EXPOSE 7860
CMD ["R", "--no-init-file", "--quiet", "-e", "options(fortuneCougar.storage = 'sqlite', fortuneCougar.db_path = Sys.getenv('FC_DB_PATH')); shiny::runApp('/home/app', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT')), launch.browser = FALSE)"]
