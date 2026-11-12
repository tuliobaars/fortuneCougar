# Deploying fortuneCougar for free

There are two kinds of free hosting:

* **Static site (Shinylive)**: R runs inside each visitor's browser (webR), the host only serves
  files. Unlimited visitors, nothing to maintain. Each visitor's profile (name, coins, cards,
  statistics) is saved in their own browser. The shared-database features (leaderboard, community
  k-means clusters, jackpots shared between players) are per visitor.
* **R server**: the normal Shiny app with one SQLite database shared by all players. Free tiers
  sleep when idle and reset their disk when they restart.

| # | option | kind | how |
|---|---|---|---|
| 1 | **GitHub Pages** (recommended) | static | push; the included workflow builds and publishes |
| 2 | Netlify | static | drag the built site folder onto Netlify Drop |
| 3 | Cloudflare Pages | static | `npx wrangler pages deploy _site` |
| 4 | GitLab Pages | static | push to GitLab; the included `.gitlab-ci.yml` builds and publishes |
| 5 | Vercel | static | `npx vercel deploy _site --prod` |
| 6 | Posit Connect Cloud | R server | publish from the GitHub repository |
| 7 | shinyapps.io | R server | `rsconnect::deployApp()` |
| 8 | Render | R server (Docker) | Blueprint from the included `render.yaml` |
| 9 | Google Cloud Run | R server (Docker) | `gcloud run deploy --source .` |
| 10 | Hugging Face Spaces | R server (Docker) | push to a Docker Space |

---

## The static site

### Build it

Always build with the script, never with `shinylive::export(".", "docs")` on the repository:
that packs the *whole* folder (videos, renv library, tests...) into one `app.json` of 120+ MB,
which GitHub refuses and every visitor would have to download. The script exports only the app
(about 0.5 MB of app files) and compiles the Bootstrap theme once, so browsers don't run Sass.

```r
install.packages(c("shinylive", "shiny", "bslib", "httpuv"))
system("Rscript --no-init-file tools/build_site.R _site")   # or run it in a terminal
httpuv::runStaticServer("_site")                              # preview at the printed address
```

The folder `_site/` (roughly 60–90 MB, almost all of it R itself compiled to WebAssembly; the
largest file is 18 MB) is what every static host below serves. It refuses to build if `app.json` grows past 5 MB.

**Load time.** A first visit downloads R (about 40 MB) and the Shiny packages, then starts
the app: typically 10–30 seconds on a good connection. Later visits reuse the browser cache and are
much faster. Most of the time is webR itself, which every Shinylive app pays.

### 1. GitHub Pages (with GitHub Actions, recommended)

Nothing built is committed: GitHub builds the site on every push.

1. Create the repository from this folder (the one with `DESCRIPTION`) and push:
   ```sh
   git init && git add . && git commit -m "fortuneCougar"
   git branch -M main
   git remote add origin https://github.com/<you>/fortuneCougar.git
   git push -u origin main
   ```
2. **Settings → Pages → Build and deployment → Source: GitHub Actions.**
3. The workflow `.github/workflows/pages.yml` publishes
   `https://<you>.github.io/fortuneCougar/` after 3–5 minutes (progress under **Actions**). Each run
   also attaches the built site as a zip (`fortuneCougar-site`) for the other static hosts.

`docs/` and `_site/` are in `.gitignore`, so a build can't end up in a commit by accident.

**Prefer "Deploy from a branch → /docs"?** Build with `Rscript --no-init-file tools/build_site.R docs`,
delete the `docs/` line from `.gitignore`, commit `docs/`, and choose *Source: Deploy from a branch,
main, /docs*. It works (no file over 100 MB), but every rebuild adds tens of MB of binaries to the
repository history, so the Actions route is better.

#### If a push was already rejected because of `docs/app.json`

The big file is in a commit, so deleting it is not enough: remove it from the history.

* It was added in the **last commit**:
  ```sh
  git rm -r --cached docs
  git commit --amend -C HEAD
  git push -u origin main
  ```
* It was added **earlier** (nothing pushed yet): squash your local commits into one that has no `docs/`:
  ```sh
  git rm -r --cached docs
  git commit -m "Remove the Shinylive export from the repository"
  git reset --soft $(git rev-list --max-parents=0 HEAD)    # keep your files, drop the old commits
  git commit --amend -m "fortuneCougar"
  git push -u origin main --force
  ```
* Or rewrite the history with git-filter-repo (`pip install git-filter-repo`):
  `git filter-repo --path docs --invert-paths --force`, then add the remote again and push.

### 2. Netlify

Free plan, 100 GB bandwidth a month. Open <https://app.netlify.com/drop> and drag the `_site`
folder onto the page (or the unzipped `fortuneCougar-site` artifact from a GitHub Actions run). It's
live at `https://<random-name>.netlify.app` in a minute; rename it in *Site configuration*.
From a terminal: `npx netlify-cli deploy --dir _site --prod`.

### 3. Cloudflare Pages

Free plan with unlimited bandwidth (25 MB per file: the largest file here is 18 MB).
```sh
npx wrangler login
npx wrangler pages deploy _site --project-name fortunecougar
```
The site is at `https://fortunecougar.pages.dev`. (The dashboard also has *Upload assets* for a
drag-and-drop upload.)

### 4. GitLab Pages

Push the repository to GitLab. The included `.gitlab-ci.yml` installs R packages in a
`rocker/r-ver` container, builds the site into `public/` and publishes it at
`https://<user>.gitlab.io/fortuneCougar/` (see *Deploy → Pages*).

### 5. Vercel

Free Hobby plan.
```sh
npx vercel deploy _site --prod
```
Answer the prompts (no build command, output directory `.`); the site is at
`https://<project>.vercel.app`.

---

## An R server with a shared database

All of these run `app.R` (it loads the package sources and calls `run_app()`); the Docker ones use
the included `Dockerfile` (R 4.4, packages from Posit Package Manager, SQLite in `/data`, port taken
from `$PORT`, default 7860). Test the image locally with
`docker build -t fortunecougar . && docker run -p 7860:7860 fortunecougar`.

### 6. Posit Connect Cloud

<https://connect.posit.cloud>: sign in with GitHub, *Publish → Shiny*, choose the repository and
`app.R`. It needs a `manifest.json`: run `rsconnect::writeManifest()` once and commit it.

### 7. shinyapps.io

Free plan: 5 apps, 25 active hours a month.
```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<account>", token = "<token>", secret = "<secret>")
rsconnect::deployApp(".", appName = "fortuneCougar")   # .rscignore leaves out media, tests, tools
```

### 8. Render

Free web service (sleeps after 15 minutes without visitors; the first request then takes about a
minute). *New → Blueprint*, pick the repository: `render.yaml` creates a Docker web service on the
free plan. Render sets `$PORT`, which the `Dockerfile` uses.

### 9. Google Cloud Run

Generous free tier (2 million requests a month; a billing account is required but not charged within
it).
```sh
gcloud run deploy fortunecougar --source . --region us-central1 --allow-unauthenticated
```
Cloud Run builds the `Dockerfile` and passes `$PORT`. Add `--max-instances 1` so all players share
one database.

### 10. Hugging Face Spaces

1. Create a Space with SDK **Docker** (template *Blank*).
2. Put this block at the very top of the README.md pushed to the Space:
   ```yaml
   ---
   title: fortuneCougar
   emoji: 🐆
   colorFrom: pink
   colorTo: purple
   sdk: docker
   app_port: 7860
   ---
   ```
3. `git remote add space https://huggingface.co/spaces/<you>/fortuneCougar` and `git push space main`.

For data that survives restarts on the Docker hosts, mount a persistent volume on `/data` (paid on
most free tiers).
