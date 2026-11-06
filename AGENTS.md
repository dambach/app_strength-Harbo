# AGENTS.md

Static web page comparing isokinetic and isometric peak torque of 18 muscle functions with
the Harbo et al. 2012 reference equations: % of predicted and z-score against the lower
limit of normal. Deployed by GitHub Pages from `master`: <https://dambach.github.io/app_strength-Harbo/>

## Commands

- Run: open `index.html` in a browser (must work from `file://`).
- Test: `node --test tests/` — run before every commit.

## Rules

- Plain HTML, CSS and JavaScript: no build step, no dependency, classic `<script src>` (no ES modules).
- `js/equations.js` (equations, calculations) and `js/export.js` (CSV) have no DOM access. Any change to
  them needs a test and an update of `docs/references.md`, which records sources and design decisions.
- Every value comes from the original article, with its table or page; never invent or estimate one.
- Ankle: Harbo's "extension" is dorsiflexion, "flexion" is plantar flexion. Show the clinical name.
- UI copy in US English, neutral (no "you"). The result is the main emphasis; orange means
  "below the lower limit of normal" only. Use the color tokens in `css/style.css`.
- No patient data leaves the browser; no third-party request except Bunny Fonts (no Google Fonts).
  Never commit the articles (PDF, DOCX).
- `legacy/` holds the previous Shiny app: do not edit it.
