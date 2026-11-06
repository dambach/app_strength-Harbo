// Run with: node --test tests/
// Checks the CSV export built from the same results as the page.
const test = require('node:test');
const assert = require('node:assert/strict');
const { FUNCTIONS, evaluate } = require('../js/equations.js');
const { CSV_COLUMNS, statusOf, buildCsv, fileBase, csvFilename } = require('../js/export.js');

const meta = { patientId: 'P-001', measurementDate: '2026-11-06', generatedAt: '2026-11-06T08:00:00.000Z' };
const p = { male: 1, age: 50, height: 178, weight: 80, invalid: {} };
const fn = id => FUNCTIONS.find(f => f.id === id);
const rowsFor = entries => entries.map(([id, t]) => ({ f: fn(id), r: evaluate(fn(id), p, t) }));

// Minimal RFC 4180 parser for the checks.
function parseCsv(text) {
  const rows = [];
  let row = [], field = '', quoted = false;
  for (let i = 0; i < text.length; i++) {
    const c = text[i];
    if (quoted) {
      if (c === '"' && text[i + 1] === '"') { field += '"'; i++; }
      else if (c === '"') quoted = false;
      else field += c;
    } else if (c === '"') quoted = true;
    else if (c === ',') { row.push(field); field = ''; }
    else if (c === '\r' && text[i + 1] === '\n') { row.push(field); rows.push(row); row = []; field = ''; i++; }
    else field += c;
  }
  return rows;
}
const toObjects = csv => {
  const [header, ...rows] = parseCsv(csv.replace(/^﻿/, ''));
  return { header, rows: rows.map(r => Object.fromEntries(header.map((h, i) => [h, r[i]]))) };
};

test('CSV: BOM, header and one row per function entered', () => {
  const csv = buildCsv(p, rowsFor([['knee-ext-ik', 150], ['ankle-df-ik', 30]]), meta);
  assert.ok(csv.startsWith('﻿'));
  const { header, rows } = toObjects(csv);
  assert.deepEqual(header, CSV_COLUMNS);
  assert.equal(rows.length, 2);
  assert.equal(rows[0].function, 'Knee extension');
  assert.equal(rows[0].contraction, 'isokinetic');
  assert.equal(rows[0].speed, '90°/s');
  assert.equal(rows[1].harbo_label, 'ankle extension');
});

test('CSV values use dot decimals and match the result', () => {
  const rows = rowsFor([['knee-ext-ik', 150]]);
  const { rows: out } = toObjects(buildCsv(p, rows, meta));
  assert.equal(out[0].measured_nm, '150.0');
  assert.equal(out[0].z_score, rows[0].r.z.toFixed(2));
  assert.equal(out[0].status, statusOf(rows[0].r));
  assert.equal(out[0].patient_id, 'P-001');
});

test('quoting of fields with commas or quotes', () => {
  const { rows } = toObjects(buildCsv(p, rowsFor([['wrist-ext-ik', 11]]), { ...meta, patientId: 'Doe, "J"' }));
  assert.equal(rows[0].patient_id, 'Doe, "J"');
  assert.match(rows[0].notes, /barely narrows/);
});

test('status wording and file names', () => {
  assert.equal(statusOf({ ok: false }), 'Not computed');
  assert.equal(statusOf({ ok: true, z: -2 }), 'Below LLN');
  assert.equal(statusOf({ ok: true, z: 0 }), 'Within normal');
  assert.equal(fileBase(meta), 'strength-profile_2026-11-06');
  assert.equal(csvFilename({}), 'strength-profile_undated.csv');
});
