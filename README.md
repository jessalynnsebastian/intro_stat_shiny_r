# intro_stat_shiny_r

This repository serves a Shinylive app on GitHub Pages from the `docs/` folder.

## Update workflow

- Source app code lives in `app/app.R`.
- Published Pages artifacts live in `docs/` (especially `docs/app.json`).
- A GitHub Actions workflow now auto-rebuilds `docs/` whenever files in `app/` change on `main`.

## Rebuild locally (optional)

If you want to preview or force a rebuild before pushing:

Rscript -e "shinylive::export('app', 'docs')"