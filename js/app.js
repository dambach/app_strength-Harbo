// Page logic: builds the torque fields, reads the form, renders the summary, chart,
// table, reference card, tooltip and the phone result bar. Calculations live in
// equations.js, CSV building in export.js.

// The z axis never extends beyond these bounds; more extreme values are pinned to the edge.
const Z_FLOOR = -8, Z_CEIL = 4;
// Last computed inputs and results, read by the export.
let lastState = null;
// Rows whose detail panel is open, by function id (kept across re-renders).
const openRows = new Set();
const FIELD_LABELS = { age: 'age', height: 'height', weight: 'weight', torque: 'torque' };

const $ = id => document.getElementById(id);
const fmtNm = v => (Math.abs(v) < 20 ? v.toFixed(1) : v.toFixed(0)) + ' N·m';
const fmtZ = z => (z < 0 ? '−' : '+') + Math.abs(z).toFixed(2);
const listText = items => items.length < 2 ? items.join('') : `${items.slice(0, -1).join(', ')} and ${items[items.length - 1]}`;
const label = f => `${fnName(f)} (${f.mode})`;

const ICON = {
  down: '<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="3" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M12 5v14"/><path d="m19 12-7 7-7-7"/></svg>',
  check: '<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="3" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M20 6 9 17l-5-5"/></svg>',
  info: '<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><circle cx="12" cy="12" r="9"/><path d="M12 11v5"/><path d="M12 8h.01"/></svg>',
  pencil: '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M12 20h9"/><path d="M16.5 3.5a2.1 2.1 0 0 1 3 3L7 19l-4 1 1-4Z"/></svg>',
  alert: '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><circle cx="12" cy="12" r="9"/><path d="M12 8v5"/><path d="M12 16h.01"/></svg>',
  chevron: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="m6 9 6 6 6-6"/></svg>'
};

// One torque field per function, grouped by joint.
function renderForm() {
  $('torque-fields').innerHTML = JOINTS.map(joint => {
    const fns = FUNCTIONS.filter(f => f.joint === joint);
    const side = fns[0].limb === 'arm' ? 'dominant arm' : 'non-dominant leg';
    return `<fieldset class="fn-group"><legend>${joint} <small>${side}</small></legend>` + fns.map(f => `
      <div class="field">
        <label for="t-${f.id}">${f.movement}<small>${fnMode(f)}${f.harbo ? ` · Harbo: ${f.harbo}` : ''}</small></label>
        <div><div class="unit-input"><input id="t-${f.id}" type="number" inputmode="decimal" step="0.1" placeholder="—"><span>N·m</span></div><p class="field-error" id="t-${f.id}-err" hidden></p></div>
      </div>`).join('') + '</fieldset>';
  }).join('');
}

// Reads one numeric field; flags it and shows the expected range when out of range.
function readField(id, range) {
  const el = $(id), err = $(`${id}-err`);
  const f = parseField(el.value, range);
  el.setAttribute('aria-invalid', f.invalid ? 'true' : 'false');
  if (f.invalid) {
    err.textContent = `Expected ${rangeText(range)}`;
    err.hidden = false;
    el.setAttribute('aria-describedby', err.id);
  } else {
    err.hidden = true;
    el.removeAttribute('aria-describedby');
  }
  return f;
}

function readInputs() {
  const p = {
    male: document.querySelector('input[name="sex"]:checked').value === 'male' ? 1 : 0,
    invalid: {}
  };
  for (const k of ['age', 'height', 'weight']) {
    const f = readField(k, LIMITS[k]);
    p[k] = f.value;
    if (f.invalid) p.invalid[k] = true;
  }
  // Only the functions with something typed in become result rows.
  const rows = [];
  for (const f of FUNCTIONS) {
    const t = readField(`t-${f.id}`, LIMITS.torque);
    if (t.invalid) rows.push({ f, r: { ok: false, field: 'torque', invalid: true, reason: `Check torque (${rangeText(LIMITS.torque)})` } });
    else if (!Number.isNaN(t.value)) rows.push({ f, r: evaluate(f, p, t.value) });
  }
  return { p, rows };
}

// One sentence above the chart: the verdict first, then what is still missing.
function summarize(rows) {
  if (!rows.length) return { text: 'Enter the peak torque of at least one function (step 2).', tone: 'na', low: 0, scored: 0 };
  const scored = rows.filter(x => x.r.ok);
  const low = scored.filter(x => x.r.z < LLN_Z);
  const waiting = rows.filter(x => !x.r.ok);
  const parts = [];
  let tone = 'na';
  if (scored.length) {
    if (low.length) {
      tone = 'low';
      parts.push(`${low.length} of ${scored.length} function${scored.length > 1 ? 's' : ''} below the lower limit of normal: ${listText(low.map(x => label(x.f).toLowerCase()))}.`);
    } else {
      tone = 'ok';
      parts.push(scored.length > 1 ? 'No function below the lower limit of normal.' : 'Within the normal range.');
    }
  }
  if (waiting.length) {
    const invalid = [...new Set(waiting.filter(x => x.r.invalid).map(x => FIELD_LABELS[x.r.field]))];
    const missing = [...new Set(waiting.filter(x => !x.r.invalid && x.r.field).map(x => FIELD_LABELS[x.r.field]))];
    if (invalid.length) parts.push(`Check ${listText(invalid)}.`);
    else if (missing.length) parts.push(`Enter ${listText(missing)}.`);
  }
  return { text: parts.join(' '), tone, low: low.length, scored: scored.length };
}

// Each row is a button that shows or hides its detail panel.
const rowAttrs = id => `role="button" tabindex="0" aria-expanded="${openRows.has(id)}" aria-controls="detail-${id}"`;

// Values and notes of one function: used by the row detail panel and the mouse tooltip.
function detailHtml({ r }) {
  if (!r.ok) return `<div class="tt-row">${r.reason}</div>`;
  return `<div class="tt-row">Predicted <b>${fmtNm(r.predicted)}</b></div>
    <div class="tt-row">LLN (5th pct) <b>${fmtNm(r.lln)}</b></div>
    <div class="tt-row">Measured <b>${fmtNm(r.measured)}</b></div>
    <div class="tt-row">% predicted <b>${r.pct.toFixed(0)} %</b></div>
    <div class="tt-row">z-score <b>${fmtZ(r.z)}</b></div>
    <div class="tt-row">Variance explained (r²) <b>${r.r2.toFixed(2)}</b></div>` +
    r.warnings.map(w => `<div class="tt-note">${w}</div>`).join('');
}

function trackHtml(r, x, zMin, zMax) {
  let html = `<div class="zone" style="width:${x(LLN_Z)}%"></div>`;
  for (let t = Math.ceil(zMin); t <= zMax; t++) {
    if (t !== 0) html += `<div class="gl" style="left:${x(t)}%"></div>`;
  }
  html += `<div class="ln lln" style="left:${x(LLN_Z)}%"></div><div class="ln pred" style="left:${x(0)}%"></div>`;
  const x0 = x(0), xz = x(r.z);
  html += `<div class="stem" style="left:${Math.min(x0, xz)}%;width:${Math.abs(xz - x0)}%"></div>`;
  html += `<div class="pt${r.z < zMin || r.z > zMax ? ' clipped' : ''}" style="left:${xz}%"></div>`;
  return html;
}

function rowHtml({ f, r }, x, zMin, zMax) {
  const id = f.id;
  const head = `<p class="ref-name">${fnName(f)}<span class="row-toggle" aria-hidden="true">${ICON.chevron}</span></p>
    <p class="proto">${fnMode(f)}${f.harbo ? ` · Harbo: ${f.harbo}` : ''}</p>`;
  const note = text => `<span class="note">${ICON.info}${text}</span>`;
  if (!r.ok) {
    const icon = r.invalid ? ICON.alert : ICON.pencil;
    return `<div class="chart-row" data-id="${id}" ${rowAttrs(id)} aria-label="${label(f)}: not computed. ${r.reason}">
      <div class="name">${head}<div class="chips"><span class="chip na">Not computed</span></div></div>
      <div class="plot"><div class="track"><div class="msg missing${r.invalid ? ' invalid' : ''}">${icon}${r.reason}</div></div>
      <div class="vals"><p class="pct empty">—</p><p class="z">&nbsp;</p></div></div></div>`;
  }
  const notes = [r.outside && note('Outside the reference sample range'), r.weak && note('Equation barely narrows the normal range')].filter(Boolean).join('');
  const low = StrengthExport.statusOf(r) === 'Below LLN';
  const chip = low
    ? `<span class="chip low">${ICON.down}Below LLN</span>`
    : `<span class="chip ok">${ICON.check}Within normal</span>`;
  return `<div class="chart-row" data-id="${id}" ${rowAttrs(id)}
      aria-label="${label(f)}: ${r.pct.toFixed(0)} percent of predicted, z-score ${r.z.toFixed(2)}${low ? ', below the lower limit of normal' : ''}">
    <div class="name">${head}<div class="chips">${chip}${notes}</div></div>
    <div class="plot"><div class="track">${trackHtml(r, x, zMin, zMax)}</div>
    <div class="vals"><p class="pct">${r.pct.toFixed(0)} %</p><p class="z">z ${fmtZ(r.z)}</p></div></div></div>`;
}

function render() {
  const { p, rows } = readInputs();
  const zs = rows.filter(x => x.r.ok).map(x => x.r.z);
  const zMin = Math.max(Z_FLOOR, Math.min(-4, Math.floor(Math.min(...zs, 0) - 0.5)));
  const zMax = Math.min(Z_CEIL, Math.max(2, Math.ceil(Math.max(...zs, 0) + 0.5)));
  const x = z => (Math.min(Math.max(z, zMin), zMax) - zMin) / (zMax - zMin) * 100;

  $('measured-summary').textContent = rows.length ? `${rows.length} function${rows.length > 1 ? 's' : ''} entered` : '';
  const sum = summarize(rows);
  $('summary').innerHTML = `<span class="dot-flag ${sum.tone}" aria-hidden="true"></span>${sum.text}`;

  // Chart header labels and axis
  $('head-labels').innerHTML =
    `<span class="lbl-lln" style="left:${x(LLN_Z)}%">LLN</span><span style="left:${x(0)}%">Predicted</span>`;
  let axis = '';
  for (let t = zMin; t <= zMax; t++) {
    const txt = t === 0 ? '0' : (t < 0 ? '−' : '+') + Math.abs(t);
    const pos = t === zMin ? 'left:0;transform:none' : t === zMax ? 'left:auto;right:0;transform:none' : `left:${x(t)}%`;
    axis += `<span class="${t % 2 ? 'odd' : ''}" style="${pos}">${txt}</span>`;
  }
  $('axis').innerHTML = axis;

  $('rows').innerHTML = rows.length
    ? rows.map(row => `<div class="chart-item">${rowHtml(row, x, zMin, zMax)}
        <div class="row-detail" id="detail-${row.f.id}"${openRows.has(row.f.id) ? '' : ' hidden'}>${detailHtml(row)}</div></div>`).join('')
    : '<p class="empty-rows">No function entered yet.</p>';

  // Table view
  $('table-body').innerHTML = rows.map(({ f, r }) => {
    const pred = r.predicted > 0 ? fmtNm(r.predicted) : '—';
    const lln = r.predicted > 0 ? fmtNm(r.lln) : '—';
    return `<tr><td>${fnName(f)}<br><small>${fnMode(f)}</small></td>
      <td class="num">${pred}</td><td class="num">${lln}</td><td class="num">${r.ok ? fmtNm(r.measured) : '—'}</td>
      <td class="num">${r.ok ? `${r.pct.toFixed(0)} %` : '—'}</td><td class="num">${r.ok ? fmtZ(r.z) : '—'}</td>
      <td class="notes">${StrengthExport.notesOf(r)}</td></tr>`;
  }).join('');

  // Phone result bar
  $('sticky-summary').innerHTML = sum.scored
    ? `<span class="dot-flag ${sum.tone}" aria-hidden="true"></span>${sum.low ? `${sum.low} of ${sum.scored} below the LLN` : 'None below the LLN'}`
    : `<span class="dot-flag na" aria-hidden="true"></span>Result pending`;

  window.__rows = rows;
  lastState = { p, rows };
  fillPrintInputs(p, rows);
  const canExport = rows.some(x => x.r.ok);
  $('btn-print').disabled = !canExport;
  $('btn-csv').disabled = !canExport;
}

function renderRefs() {
  const eqRows = FUNCTIONS.map(f => [0, 1].map(s => {
    const [i, a, h, w] = f.coef[s];
    return `<tr>${s === 0 ? `<td rowspan="2">${fnName(f)}<br><small>${fnMode(f)}</small></td>` : ''}<td>${s === 0 ? 'Men' : 'Women'}</td>
      <td class="num">${i}</td><td class="num">${a}</td><td class="num">${h}</td><td class="num">${w}</td>
      <td class="num">${f.rsd[s]}</td><td class="num">${f.r2[s].toFixed(2)}</td></tr>`;
  }).join('')).join('');
  $('ref-card').innerHTML = `
    <h3>Harbo et al. 2012</h3>
    <div class="pills"><span class="pill accent">Biodex System 3 PRO</span><span class="pill">Peak torque, N·m</span></div>
    <dl>
      <dt>Position</dt><dd>Seated, trunk strapped, dynamometer axis aligned with the joint axis</dd>
      <dt>Side</dt><dd>Dominant arm, non-dominant leg</dd>
      <dt>Isokinetic</dt><dd>Concentric, 8 reciprocal contractions; knee and wrist 90°/s, shoulder, elbow, hip and ankle 60°/s; gravity corrected</dd>
      <dt>Isometric</dt><dd>3 contractions of 5 s at neutral joint positions (angles not reported)</dd>
      <dt>Value kept</dt><dd>Highest peak torque</dd>
      <dt>Reference group</dt><dd>178 healthy non-athletic Danish adults: ${SAMPLE.male.n} men aged ${SAMPLE.male.age.join('–')} yr, ${SAMPLE.female.n} women aged ${SAMPLE.female.age.join('–')} yr</dd>
      <dt>Limitation</dt><dd>Age, height and weight explain 8–63 % of the variance; the wrist and ankle dorsiflexion equations barely narrow the normal range</dd>
    </dl>
    <p class="eq">Predicted peak torque (N·m) = intercept + a × age (yr) + h × height (m) + w × weight (kg), sex-specific</p>
    <details class="eq-details">
      <summary>Show the 18 equations</summary>
      <div class="table-wrap"><table>
        <thead><tr><th>Function</th><th>Sex</th><th class="num">Intercept</th><th class="num">a</th><th class="num">h</th><th class="num">w</th><th class="num">RSD</th><th class="num">r²</th></tr></thead>
        <tbody>${eqRows}</tbody>
      </table></div>
    </details>
    <p class="cite">${SOURCE.citation} <a href="https://doi.org/${SOURCE.doi}" target="_blank" rel="noopener">doi:${SOURCE.doi}</a></p>`;
}

// Tooltip on mouse hover
const tip = $('tooltip');
function showTip(row, clientX, clientY) {
  const res = window.__rows.find(x => x.f.id === row.dataset.id);
  if (!res) return;
  tip.innerHTML = `<div class="tt-title">${fnName(res.f)} · ${fnMode(res.f)}</div>` + detailHtml(res);
  const pad = 14, rect = tip.getBoundingClientRect();
  let left = clientX + pad, top = clientY + pad;
  if (left + rect.width > window.innerWidth - 8) left = clientX - rect.width - pad;
  if (top + rect.height > window.innerHeight - 8) top = clientY - rect.height - pad;
  // Keep the whole tooltip on screen.
  left = Math.max(8, Math.min(left, window.innerWidth - rect.width - 8));
  top = Math.max(8, Math.min(top, window.innerHeight - rect.height - 8));
  tip.style.left = left + 'px';
  tip.style.top = top + 'px';
  tip.classList.add('show');
}
const rowsEl = $('rows');
const hideTip = () => tip.classList.remove('show');

// The floating tooltip is for a mouse only. Touch input is ignored here, so iOS does
// not treat a tap as a hover (which would swallow the click).
rowsEl.addEventListener('pointermove', e => {
  if (e.pointerType !== 'mouse') return;
  const row = e.target.closest('.chart-row');
  if (row && !openRows.has(row.dataset.id)) showTip(row, e.clientX, e.clientY);
  else hideTip();
});
rowsEl.addEventListener('pointerleave', e => { if (e.pointerType === 'mouse') hideTip(); });
window.addEventListener('scroll', hideTip, { passive: true });
document.addEventListener('keydown', e => { if (e.key === 'Escape') hideTip(); });

// Tap, click, Enter or Space shows or hides the row detail.
function toggleRow(row) {
  const id = row.dataset.id;
  const open = !openRows.has(id);
  open ? openRows.add(id) : openRows.delete(id);
  row.setAttribute('aria-expanded', String(open));
  $(`detail-${id}`).hidden = !open;
  hideTip();
}
rowsEl.addEventListener('click', e => {
  const row = e.target.closest('.chart-row');
  if (row) toggleRow(row);
});
rowsEl.addEventListener('keydown', e => {
  const row = e.target.closest('.chart-row');
  if (!row) return;
  if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); toggleRow(row); }
});

// Export: printable report and CSV. Patient ID and date are read only here and
// never stored or sent.
const pad2 = n => String(n).padStart(2, '0');
const localDate = d => `${d.getFullYear()}-${pad2(d.getMonth() + 1)}-${pad2(d.getDate())}`;
const localDateTime = d => `${localDate(d)} ${pad2(d.getHours())}:${pad2(d.getMinutes())}`;
const exportMeta = () => ({
  patientId: $('patient-id').value.trim(),
  measurementDate: $('measurement-date').value,
  generatedAt: new Date().toISOString()
});

function fillPrintHead() {
  const meta = exportMeta();
  $('print-patient').textContent = meta.patientId || '—';
  $('print-date').textContent = meta.measurementDate || '—';
  $('print-generated').textContent = localDateTime(new Date());
}

function fillPrintInputs(p, rows) {
  const v = (x, u) => Number.isFinite(x) ? `${x} ${u}` : '—';
  const items = [
    ['Sex', p.male ? 'Male' : 'Female'], ['Age', v(p.age, 'yr')],
    ['Height', v(p.height, 'cm')], ['Weight', v(p.weight, 'kg')]
  ];
  $('print-inputs').innerHTML = items.map(([k, val]) => `<div><dt>${k}</dt><dd>${val}</dd></div>`).join('') +
    `<div><dt>Functions tested</dt><dd>${rows.length}</dd></div>`;
}

function downloadCsv() {
  if (!lastState) return;
  const meta = exportMeta();
  const csv = StrengthExport.buildCsv(lastState.p, lastState.rows, meta);
  const url = URL.createObjectURL(new Blob([csv], { type: 'text/csv;charset=utf-8' }));
  const a = document.createElement('a');
  a.href = url;
  a.download = StrengthExport.csvFilename(meta);
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 0);
}

// The table is part of the report: open it for printing, then restore it.
const tableView = document.querySelector('details.table-view');
let tableWasOpen = false;
window.addEventListener('beforeprint', () => {
  fillPrintHead();
  tableWasOpen = tableView.open;
  tableView.open = true;
});
window.addEventListener('afterprint', () => { tableView.open = tableWasOpen; });

// Browsers use the page title as the default PDF file name.
const pageTitle = document.title;
$('btn-print').addEventListener('click', () => {
  document.title = StrengthExport.fileBase(exportMeta());
  window.print();
});
window.addEventListener('afterprint', () => { document.title = pageTitle; });
$('measurement-date').value = localDate(new Date());
$('patient-id').addEventListener('input', fillPrintHead);
$('measurement-date').addEventListener('input', fillPrintHead);
$('btn-csv').addEventListener('click', downloadCsv);

// Hide the phone result bar while the full result is on screen.
if ('IntersectionObserver' in window) {
  new IntersectionObserver(([entry]) => {
    $('sticky-bar').classList.toggle('is-hidden', entry.isIntersecting);
  }, { threshold: 0.25 }).observe($('result'));
}

renderForm();
renderRefs();
$('inputs').addEventListener('input', render);
$('inputs').addEventListener('change', render);
render();
fillPrintHead();
