// Harbo et al. 2012 prediction equations and pure calculations. No DOM access, so
// this file can be loaded by the page (classic <script>) and by Node for the tests.
//
// Source: Harbo T, Brincks J, Andersen H. Maximal isokinetic and isometric muscle
// strength of major muscle groups related to age, body mass, height, and sex in
// 178 healthy subjects. Eur J Appl Physiol. 2012;112:267–275.
//
// Predicted peak torque (N·m) = intercept + a × age (yr) + h × height (m) + w × body mass (kg),
// with sex-specific coefficients (Tables 1 and 2: upper and lower limb). Residual SD, r²,
// mean and SD of the measured torque come from Tables 3 and 4. Each array is [men, women].

const SOURCE = {
  citation: 'Harbo T, Brincks J, Andersen H. Maximal isokinetic and isometric muscle strength of major muscle groups related to age, body mass, height, and sex in 178 healthy subjects. Eur J Appl Physiol. 2012;112(1):267–75.',
  doi: '10.1007/s00421-011-1975-3'
};

// Sample characteristics (Results: median and range), used to flag inputs outside the
// reference sample. Height in cm.
const SAMPLE = {
  male: { n: 93, age: [15, 83], height: [167, 189], weight: [52, 115] },
  female: { n: 85, age: [21, 75], height: [153, 183], weight: [46, 105] }
};

// coef: [intercept, age, height (m), weight]; rsd, r2, mean, sd in N·m (Tables 1–4).
const FUNCTIONS = [
  { id: 'shoulder-abd-ik', joint: 'Shoulder', movement: 'Abduction', mode: 'isokinetic', speed: '60°/s', rom: '70°', limb: 'arm',
    coef: [[-47.7, -0.16, 42, 0.51], [-9.3, -0.20, 24, 0.28]], rsd: [9.94, 6.65], r2: [0.35, 0.32], mean: [59.5, 37.4], sd: [12.0, 7.88] },
  { id: 'shoulder-add-ik', joint: 'Shoulder', movement: 'Adduction', mode: 'isokinetic', speed: '60°/s', rom: '70°', limb: 'arm',
    coef: [[-72.9, -0.06, 72, 0.38], [-24.5, -0.15, 40, 0.18]], rsd: [16.6, 8.23], r2: [0.14, 0.21], mean: [83.1, 45.7], sd: [17.7, 9.1] },
  { id: 'shoulder-abd-im', joint: 'Shoulder', movement: 'Abduction', mode: 'isometric', limb: 'arm',
    coef: [[-30.8, -0.24, 36, 0.49], [-28.2, -0.21, 35, 0.29]], rsd: [12.3, 6.95], r2: [0.28, 0.38], mean: [61.1, 38.0], sd: [14.1, 8.66] },

  { id: 'elbow-ext-ik', joint: 'Elbow', movement: 'Extension', mode: 'isokinetic', speed: '60°/s', rom: '110°', limb: 'arm',
    coef: [[-4.4, -0.07, 12, 0.42], [6.4, -0.10, 5, 0.27]], rsd: [8.92, 4.78], r2: [0.24, 0.28], mean: [46.7, 27.2], sd: [9.99, 5.53] },
  { id: 'elbow-flex-ik', joint: 'Elbow', movement: 'Flexion', mode: 'isokinetic', speed: '60°/s', rom: '110°', limb: 'arm',
    coef: [[-37.2, -0.18, 36, 0.41], [-16.5, -0.12, 27, 0.09]], rsd: [10.7, 5.15], r2: [0.26, 0.24], mean: [51.0, 27.8], sd: [12.2, 5.79] },
  { id: 'elbow-flex-im', joint: 'Elbow', movement: 'Flexion', mode: 'isometric', limb: 'arm',
    coef: [[-23.8, -0.13, 22, 0.51], [6.9, -0.13, 9, 0.18]], rsd: [12.3, 4.99], r2: [0.23, 0.23], mean: [50.9, 26.5], sd: [13.6, 5.58] },

  { id: 'wrist-ext-ik', joint: 'Wrist', movement: 'Extension', mode: 'isokinetic', speed: '90°/s', rom: '45°', limb: 'arm',
    coef: [[-11.1, -0.02, 12, 0.03], [-9.0, -0.01, 9, 0.01]], rsd: [3.05, 1.56], r2: [0.08, 0.17], mean: [11.4, 6.01], sd: [3.12, 1.68] },
  { id: 'wrist-flex-ik', joint: 'Wrist', movement: 'Flexion', mode: 'isokinetic', speed: '90°/s', rom: '45°', limb: 'arm',
    coef: [[1.7, -0.07, 4, 0.21], [-2.3, -0.02, 8, 0.04]], rsd: [5.00, 2.73], r2: [0.21, 0.09], mean: [22.1, 13.1], sd: [5.51, 2.81] },
  { id: 'wrist-flex-im', joint: 'Wrist', movement: 'Flexion', mode: 'isometric', limb: 'arm',
    coef: [[-6.9, -0.07, 10, 0.21], [-22.2, -0.04, 23, 0.00]], rsd: [7.05, 3.60], r2: [0.14, 0.18], mean: [24.7, 14.4], sd: [7.45, 3.90] },

  { id: 'hip-ext-ik', joint: 'Hip', movement: 'Extension', mode: 'isokinetic', speed: '60°/s', rom: '60°', limb: 'leg',
    coef: [[-49.8, -0.97, 103, 1.14], [-108.7, -0.87, 147, 0.50]], rsd: [41.0, 25.9], r2: [0.23, 0.30], mean: [178.1, 128.7], sd: [45.6, 30.3] },
  { id: 'hip-flex-ik', joint: 'Hip', movement: 'Flexion', mode: 'isokinetic', speed: '60°/s', rom: '60°', limb: 'leg',
    coef: [[-79.9, -0.75, 107, 0.79], [-72.3, -0.42, 96, 0.38]], rsd: [24.6, 12.5], r2: [0.35, 0.40], mean: [137.6, 91.2], sd: [29.6, 15.8] },
  { id: 'hip-flex-im', joint: 'Hip', movement: 'Flexion', mode: 'isometric', limb: 'leg',
    coef: [[37.6, -0.72, 36, 1.12], [-72.4, -0.20, 78, 0.89]], rsd: [28.6, 15.1], r2: [0.27, 0.40], mean: [156.2, 104.4], sd: [32.8, 19.2] },

  { id: 'knee-ext-ik', joint: 'Knee', movement: 'Extension', mode: 'isokinetic', speed: '90°/s', rom: '80°', limb: 'leg',
    coef: [[81.8, -1.44, 41, 1.25], [-96.7, -1.00, 137, 0.62]], rsd: [30.1, 15.2], r2: [0.43, 0.63], mean: [185.4, 121.5], sd: [39.2, 24.5] },
  { id: 'knee-flex-ik', joint: 'Knee', movement: 'Flexion', mode: 'isokinetic', speed: '90°/s', rom: '80°', limb: 'leg',
    coef: [[-90.8, -0.49, 80, 0.82], [-40.2, -0.48, 57, 0.46]], rsd: [22.9, 10.3], r2: [0.26, 0.47], mean: [95.1, 59.3], sd: [26.0, 13.9] },
  { id: 'knee-ext-im', joint: 'Knee', movement: 'Extension', mode: 'isometric', limb: 'leg',
    coef: [[-107.9, -1.40, 158, 1.75], [-201.0, -0.96, 216, 0.88]], rsd: [46.7, 30.1], r2: [0.35, 0.41], mean: [246.6, 166.6], sd: [56.3, 38.2] },

  // Harbo's "ankle extension" is the weak movement (dorsiflexion) and "ankle flexion"
  // the strong one (plantar flexion), as the mean torques show.
  { id: 'ankle-df-ik', joint: 'Ankle', movement: 'Dorsiflexion', harbo: 'ankle extension', mode: 'isokinetic', speed: '60°/s', rom: '48°', limb: 'leg',
    coef: [[-13.4, -0.07, 23, 0.10], [-11.4, -0.10, 16, 0.17]], rsd: [6.29, 3.81], r2: [0.12, 0.35], mean: [33.0, 21.3], sd: [6.56, 4.62] },
  { id: 'ankle-pf-ik', joint: 'Ankle', movement: 'Plantar flexion', harbo: 'ankle flexion', mode: 'isokinetic', speed: '60°/s', rom: '48°', limb: 'leg',
    coef: [[97.7, -0.72, 4, 0.52], [33.2, -0.68, 24, 0.59]], rsd: [22.3, 17.2], r2: [0.25, 0.29], mean: [111.8, 76.4], sd: [25.0, 20.0] },
  { id: 'ankle-df-im', joint: 'Ankle', movement: 'Dorsiflexion', harbo: 'ankle extension', mode: 'isometric', limb: 'leg',
    coef: [[-88.9, -0.04, 68, 0.16], [-37.8, -0.04, 35, 0.16]], rsd: [9.30, 5.20], r2: [0.22, 0.31], mean: [43.5, 27.5], sd: [10.3, 6.11] }
];

const JOINTS = ['Shoulder', 'Elbow', 'Wrist', 'Hip', 'Knee', 'Ankle'];
const LLN_Z = -1.645; // 5th percentile
// Equations that the article reports as not narrowing the prediction interval in a
// clinically relevant way (SD − RSD < 1 N·m): wrist and ankle extension (Results, p. 270).
const isWeak = f => f.joint === 'Wrist' || f.harbo === 'ankle extension';
// Plausible ranges. Values outside are not computed (this also covers partial typing).
const LIMITS = {
  age: [15, 100, 'yr'], height: [100, 230, 'cm'], weight: [25, 250, 'kg'],
  torque: [0.5, 600, 'N·m']
};
const NEED_LABELS = { age: 'age', height: 'height', weight: 'weight' };

const rangeText = ([lo, hi, u]) => `${lo}–${hi} ${u}`;
const fnName = f => `${f.joint} ${f.movement.toLowerCase()}`;
const fnMode = f => f.mode === 'isokinetic' ? `Isokinetic ${f.speed}` : 'Isometric';

// Parses a raw input string: NaN when empty or outside [lo, hi]; `invalid` only when out of range.
function parseField(raw, [lo, hi]) {
  const s = String(raw).trim(), v = parseFloat(s);
  const invalid = s !== '' && !(Number.isFinite(v) && v >= lo && v <= hi);
  return { value: invalid || s === '' ? NaN : v, invalid };
}

function predict(f, p) {
  const [i, a, h, w] = f.coef[p.male ? 0 : 1];
  return i + a * p.age + h * (p.height / 100) + w * p.weight;
}

// Applies one function's equation to the patient `p` ({ male, age, height, weight, invalid })
// and the measured peak torque (N·m). When it cannot be computed, `field` names the input
// to enter or fix.
function evaluate(f, p, measured) {
  const bad = ['age', 'height', 'weight'].find(k => p.invalid[k]);
  if (bad) return { ok: false, field: bad, invalid: true, reason: `Check ${NEED_LABELS[bad]} (${rangeText(LIMITS[bad])})` };
  const missing = ['age', 'height', 'weight'].filter(k => Number.isNaN(p[k]));
  if (missing.length) return { ok: false, field: missing[0], reason: `Enter ${missing.map(k => NEED_LABELS[k]).join(', ')}` };
  const s = p.male ? 0 : 1;
  const predicted = predict(f, p);
  const rsd = f.rsd[s];
  if (!(predicted > 0)) return { ok: false, reason: 'Predicted value ≤ 0 for these inputs' };
  const lln = predicted + LLN_Z * rsd;
  if (Number.isNaN(measured)) return { ok: false, predicted, lln, field: 'torque', reason: 'Enter measured torque' };
  const warnings = [];
  const sample = SAMPLE[p.male ? 'male' : 'female'];
  const outside = ['age', 'height', 'weight'].filter(k => p[k] < sample[k][0] || p[k] > sample[k][1]);
  if (outside.length) {
    warnings.push(`Outside the reference sample range: ${outside.map(k => `${NEED_LABELS[k]} (${sample[k][0]}–${sample[k][1]})`).join(', ')}`);
  }
  const weak = isWeak(f);
  if (weak) warnings.push(`Equation barely narrows the normal range (r² = ${f.r2[s].toFixed(2)}): the z-score mostly compares with the group mean`);
  return {
    ok: true, predicted, lln, rsd, measured, warnings, outside: outside.length > 0, weak,
    r2: f.r2[s], z: (measured - predicted) / rsd, pct: measured / predicted * 100
  };
}

if (typeof module !== 'undefined' && module.exports) {
  module.exports = { SOURCE, SAMPLE, FUNCTIONS, JOINTS, LLN_Z, LIMITS, rangeText, fnName, fnMode, parseField, predict, evaluate };
}
