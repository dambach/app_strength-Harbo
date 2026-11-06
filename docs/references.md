# Reference equations: sources and decisions

## Source

Harbo T, Brincks J, Andersen H. Maximal isokinetic and isometric muscle strength of major
muscle groups related to age, body mass, height, and sex in 178 healthy subjects.
*Eur J Appl Physiol*. 2012;112(1):267–75. doi:[10.1007/s00421-011-1975-3](https://doi.org/10.1007/s00421-011-1975-3)

## What comes from where

| Value in `js/equations.js` | Source in the article |
|---|---|
| Coefficients (intercept, age, height in m, body mass), by sex | Tables 1 (upper limb) and 2 (lower limb) |
| Mean peak torque, SD, residual SD (RSD), r², by sex | Tables 3 (upper limb) and 4 (lower limb) |
| Sample size and age, height, weight ranges (`SAMPLE`) | Results, "Explanatory variables" |
| Protocol (device, side, speeds, ROM, contractions) | Methods, "Test protocol" |

Checks (see `tests/equations.test.js`):

- The coefficients match the legacy CSV files of the Shiny app (`legacy/coeff_*.csv`), 36 rows.
- The worked example of the article (isokinetic knee extension, man, 40 yr, 1.80 m, 80 kg)
  gives 198 N·m.
- At the group characteristics of Table 5, the equations reproduce the age-group means
  within a few N·m.

## Protocol

- Biodex System 3 PRO, seated, shoulder and abdominal straps, joint axis aligned with the
  dynamometer axis by inspection and palpation.
- Dominant arm and non-dominant leg (self-reported dominance).
- Isokinetic: 8 maximal concentric reciprocal contractions, 15 s apart. Knee 80° ROM at 90°/s,
  ankle 48° at 60°/s, hip 60° at 60°/s, elbow 110° at 60°/s, wrist 45° at 90°/s, shoulder
  abduction and adduction 70° at 60°/s. Gravity correction applied.
- Isometric: 3 maximal contractions of 5 s, 40 s apart, at "neutral joint positions"; the
  angles are not reported. Functions: knee extension, ankle extension (dorsiflexion), hip,
  elbow and wrist flexion, shoulder abduction.
- Highest peak torque kept. A test was repeated or discarded when the coefficient of variation
  exceeded 10 % (knee, ankle) or 15 % (hip, shoulder, elbow, wrist).
- 178 healthy non-athletic Danish adults: 93 men aged 15–83 yr, 85 women aged 21–75 yr.

## Decisions

**Ankle naming.** The article calls the weak ankle movement "extension" (mean about 33 N·m in
men) and the strong one "flexion" (about 112 N·m). These are dorsiflexion and plantar flexion.
The app shows the clinical name with the article's term, e.g. "Ankle dorsiflexion
(Harbo: extension)", and the CSV keeps the article's term in `harbo_label`.

**Lower limit of normal.** LLN = predicted − 1.645 × RSD (5th percentile, one-sided), and
z = (measured − predicted) / RSD with the sex-specific RSD. The article gives
prediction intervals as predicted ± 1.96 × RSD (footnotes of Tables 3–4). Its worked example
reports "198 N·m ± 30.1 (168–228)", which is ± 1 RSD; the tables' ± 1.96 × RSD is taken as
the intended definition. No fixed % predicted cutoff (such as 80 %) is used.

**Equations that barely narrow the normal range.** The article reports that the wrist
(extension and flexion) and ankle extension models do not narrow the prediction interval in a
clinically relevant way (SD − RSD < 1 N·m, Results p. 270 and Discussion). These five
functions carry a note: their z-score mostly compares with the group mean. Applying the
SD − RSD < 1 rule to the rounded table values would also flag shoulder adduction and elbow
functions in women; the article does not name them, so they are not flagged.

**Outside the reference sample.** When age, height or weight is outside the sex-specific
range of the sample, the result is still computed, with a note.

**Plausible input ranges.** Age 15–100 yr, height 100–230 cm, weight 25–250 kg, torque
0.5–600 N·m. Values outside are not computed (this also covers partial typing).
