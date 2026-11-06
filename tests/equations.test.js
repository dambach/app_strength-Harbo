// Run with: node --test tests/
// Checks the Harbo et al. 2012 equations against the article and the legacy CSV files.
const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { FUNCTIONS, JOINTS, PROTOCOL, fnMode, LLN_Z, LIMITS, parseField, predict, evaluate } = require('../js/equations.js');

const fn = id => FUNCTIONS.find(f => f.id === id);
const close = (actual, expected, tol, msg) =>
  assert.ok(Math.abs(actual - expected) <= tol, `${msg}: got ${actual.toFixed(2)}, expected ${expected} ± ${tol}`);
const patient = p => ({ invalid: {}, ...p });

test('18 functions over 6 joints, each with men and women values', () => {
  assert.equal(FUNCTIONS.length, 18);
  assert.deepEqual([...new Set(FUNCTIONS.map(f => f.joint))], JOINTS);
  for (const f of FUNCTIONS) {
    assert.equal(f.coef.length, 2, f.id);
    for (const k of ['rsd', 'r2', 'mean', 'sd']) assert.equal(f[k].length, 2, `${f.id} ${k}`);
  }
});

test('worked example of the article: isokinetic knee extension, man 40 yr, 1.80 m, 80 kg = 198 N·m', () => {
  close(predict(fn('knee-ext-ik'), { male: 1, age: 40, height: 180, weight: 80 }), 198, 0.5, 'predicted');
});

test('equations reproduce Table 5 age-group means at the group characteristics', () => {
  // [function, sex, age, height cm, weight kg, Table 5 mean, tolerance]
  const cases = [
    ['knee-ext-ik', 1, 45, 179, 82, 192, 3],
    ['knee-ext-ik', 0, 44, 167, 61, 127, 3],
    ['knee-ext-im', 1, 64, 179, 81, 230, 5],
    ['hip-flex-ik', 0, 63, 163, 63, 82, 5],
    ['elbow-flex-ik', 1, 34, 180, 82, 55, 3]
  ];
  for (const [id, male, age, height, weight, mean, tol] of cases) {
    close(predict(fn(id), { male, age, height, weight }), mean, tol, `${id} ${male ? 'men' : 'women'}`);
  }
});

test('coefficients match the legacy CSV files of the Shiny app', () => {
  const legacyName = f => `${f.joint} ${f.harbo ? f.harbo.split(' ')[1] : f.movement.toLowerCase()} ${f.mode}`.toLowerCase();
  for (const [s, file] of [[0, 'coeff_men.csv'], [1, 'coeff_women.csv']]) {
    const lines = fs.readFileSync(path.join(__dirname, '..', 'legacy', file), 'utf8').trim().split('\n').slice(1);
    const csv = Object.fromEntries(lines.map(l => { const [n, ...v] = l.split(';'); return [n.trim().toLowerCase(), v.map(Number)]; }));
    for (const f of FUNCTIONS) assert.deepEqual(f.coef[s], csv[legacyName(f)], `${f.id} ${file}`);
  }
});

test('ankle: Harbo "extension" is the weak movement (dorsiflexion)', () => {
  assert.equal(fn('ankle-df-ik').harbo, 'ankle extension');
  assert.equal(fn('ankle-pf-ik').harbo, 'ankle flexion');
  assert.ok(fn('ankle-df-ik').mean[0] < fn('ankle-pf-ik').mean[0]);
});

test('z-score is −1.645 at the lower limit of normal, with the sex-specific residual SD', () => {
  const f = fn('knee-ext-ik'), p = patient({ male: 0, age: 50, height: 165, weight: 65 });
  const lln = predict(f, p) + LLN_Z * f.rsd[1];
  close(evaluate(f, p, lln).z, -1.645, 1e-9, 'z at LLN');
});

test('cautions: weak equations and patients outside the reference sample', () => {
  const p = patient({ male: 1, age: 50, height: 178, weight: 80 });
  assert.equal(evaluate(fn('wrist-ext-ik'), p, 11).weak, true);
  assert.equal(evaluate(fn('knee-ext-ik'), p, 180).weak, false);
  // Only the functions named in the article (Results, p. 270): wrist and ankle extension.
  const weakIds = FUNCTIONS.filter(f => evaluate(f, p, 20).weak).map(f => f.id);
  assert.deepEqual(weakIds, ['wrist-ext-ik', 'wrist-flex-ik', 'wrist-flex-im', 'ankle-df-ik', 'ankle-df-im']);
  assert.equal(evaluate(fn('knee-ext-ik'), p, 180).outside, false);
  assert.equal(evaluate(fn('knee-ext-ik'), { ...p, age: 90 }, 120).outside, true);
});

test('missing or invalid patient values are not computed', () => {
  assert.match(evaluate(fn('knee-ext-ik'), patient({ male: 1, age: NaN, height: 178, weight: 80 }), 180).reason, /Enter age/);
  const r = evaluate(fn('knee-ext-ik'), patient({ male: 1, age: 50, height: NaN, weight: 80, invalid: { height: true } }), 180);
  assert.equal(r.ok, false);
  assert.match(r.reason, /Check height/);
  assert.equal(parseField('1', LIMITS.height).invalid, true);
  assert.ok(Number.isNaN(parseField('', LIMITS.torque).value));
});

test('test positions: one per joint, and an angle for every isometric function', () => {
  assert.deepEqual(Object.keys(PROTOCOL), JOINTS);
  for (const j of JOINTS) for (const k of ['setup', 'axis', 'rom', 'isometric', 'gravity']) assert.ok(PROTOCOL[j][k], `${j} ${k}`);
  for (const f of FUNCTIONS) assert.equal(Boolean(f.angle), f.mode === 'isometric', f.id);
  // Supplementary material: isometric knee extension at 70° of flexion, not 90°.
  assert.equal(fnMode(fn('knee-ext-im')), 'Isometric, knee 70° flexion');
});
