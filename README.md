# Muscle strength norms (Harbo et al. 2012)

A small web page that compares **isokinetic and isometric peak torque of 18 muscle
functions** (shoulder, elbow, wrist, hip, knee, ankle) with the sex-specific reference
equations of healthy adults from Harbo et al. 2012. For each function tested it gives the
**% of predicted** and the **z-score** against the lower limit of normal (LLN, 5th
percentile), shown together as a strength profile.

Use it online: <https://dambach.github.io/app_strength-Harbo/>

Reference: Harbo T, Brincks J, Andersen H. Maximal isokinetic and isometric muscle strength
of major muscle groups related to age, body mass, height, and sex in 178 healthy subjects.
*Eur J Appl Physiol*. 2012;112:267–275.
[doi:10.1007/s00421-011-1975-3](https://doi.org/10.1007/s00421-011-1975-3)

Sources, checks and design decisions (ankle naming, LLN, equations that barely narrow the
normal range) are documented in [docs/references.md](docs/references.md).

## Use

Open `index.html` in a browser. There is no build step, no server and no data leaves the
browser. Enter sex, age, height and weight, then the peak torque (N·m) of the functions
tested only; each filled function adds a row to the profile.

## Export

- **Save as PDF**: opens the browser's print window with the file name already set
  (choose "Save as PDF" as destination); an A4 report with the patient ID and measurement
  date, the inputs, the profile, the table of values with notes and the reference protocol.
- **Download CSV**: one row per function tested (inputs, function, contraction, speed,
  predicted value, LLN, measured value, % of predicted, z-score, r², status, notes). Comma
  separator, dot decimals, UTF-8 with BOM. Read it with
  `read.csv("file.csv", fileEncoding = "UTF-8-BOM")` in R or
  `pandas.read_csv("file.csv", encoding="utf-8-sig")` in Python.

The patient ID (optional) and the measurement date are used only in the exported files. They
are never stored or sent, and the patient ID is not put in the file name.

## Privacy

- No cookies, no tracking, no analytics, no storage of the values entered.
- All calculations run in the browser; values entered are never sent anywhere. The PDF and
  CSV exports are created on the user's device. Use a pseudonymous patient ID, not a name.
- Fonts are served by [Bunny Fonts](https://fonts.bunny.net) (EU, no logging) instead of
  Google Fonts.
- The host, GitHub Pages, logs visitors' IP addresses for security
  ([GitHub privacy statement](https://docs.github.com/en/site-policy/privacy-policies/github-general-privacy-statement)).

## Project layout

```
index.html            page markup
css/style.css         styles (light and dark)
js/equations.js       Harbo et al. 2012 equations and calculations (no DOM, also loaded by the tests)
js/export.js          CSV export and result status wording (no DOM)
js/app.js             form, profile chart, table, tooltip and export buttons
tests/                Node tests
docs/references.md    sources and decisions
legacy/               previous Shiny (R) version and its coefficient files
AGENTS.md             rules for coding agents and contributors
```

## Tests

Requires Node.js 18 or later, no dependencies:

```
node --test tests/
```

The tests check the coefficients against the legacy CSV files, the worked example and the
age-group means of the article, the z-score at the LLN, the cautions, the input guards and
the CSV export.

## Previous version

The original Shiny app (2017) is kept in [legacy/](legacy/). It can still be run in R after cloning,
from the `legacy/` folder (it reads the CSV files from the working directory):
`setwd("legacy"); shiny::runApp("app_strength-Harbo.R")`.

## Disclaimer

For information purposes only. Not a substitute for professional medical advice,
diagnosis or treatment.

## Author and license

[Damien Bachasson](https://github.com/dambach). Released under the [MIT License](LICENSE).
