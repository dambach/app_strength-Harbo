// Export helpers: result status wording and CSV building. No DOM access, so the
// tests can load this file. Wrapped in one namespace to stay clear of the other
// scripts' globals.
const StrengthExport = (() => {
  const eqs = typeof FUNCTIONS !== 'undefined' ? null : require('./equations.js');
  const LLN = eqs ? eqs.LLN_Z : LLN_Z;
  const nameOf = eqs ? eqs.fnName : fnName;

  const CSV_COLUMNS = [
    'patient_id', 'measurement_date', 'generated_at',
    'sex', 'age_yr', 'height_cm', 'weight_kg',
    'function', 'contraction', 'speed', 'isometric_angle', 'harbo_label',
    'predicted_nm', 'lln_nm', 'measured_nm', 'pct_predicted', 'z_score', 'r2',
    'status', 'notes'
  ];

  // Same wording as the chips on the page.
  function statusOf(r) {
    if (!r.ok) return 'Not computed';
    return r.z < LLN ? 'Below LLN' : 'Within normal';
  }

  const notesOf = r => r.ok ? r.warnings.join('; ') : r.reason;
  const nm = v => Number.isFinite(v) ? v.toFixed(1) : '';
  const raw = v => Number.isFinite(v) ? String(v) : '';

  // RFC 4180: quote fields holding a comma, a quote or a line break; double the quotes.
  function cell(v) {
    const s = v === null || v === undefined ? '' : String(v);
    return /[",\r\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
  }

  // One row per function with a measured torque. `rows` = [{ f, r }];
  // `meta` = { patientId, measurementDate, generatedAt }.
  function buildCsv(p, rows, meta) {
    const lines = [CSV_COLUMNS.join(',')];
    for (const { f, r } of rows) {
      const row = [
        meta.patientId || '', meta.measurementDate || '', meta.generatedAt || '',
        p.male ? 'male' : 'female', raw(p.age), raw(p.height), raw(p.weight),
        nameOf(f), f.mode, f.speed || '', f.angle || '', f.harbo || '',
        r.predicted > 0 ? nm(r.predicted) : '', r.predicted > 0 ? nm(r.lln) : '',
        r.ok ? nm(r.measured) : '', r.ok ? r.pct.toFixed(1) : '', r.ok ? r.z.toFixed(2) : '',
        r.ok ? r.r2.toFixed(2) : '', statusOf(r), notesOf(r)
      ];
      lines.push(row.map(cell).join(','));
    }
    // BOM so spreadsheet software detects UTF-8.
    return '﻿' + lines.join('\r\n') + '\r\n';
  }

  // File names carry the measurement date, never the patient ID.
  const fileBase = meta => `strength-profile_${meta.measurementDate || 'undated'}`;
  const csvFilename = meta => `${fileBase(meta)}.csv`;

  return { CSV_COLUMNS, statusOf, notesOf, buildCsv, fileBase, csvFilename };
})();

if (typeof module !== 'undefined' && module.exports) module.exports = StrengthExport;
