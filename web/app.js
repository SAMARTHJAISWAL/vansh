// vansh browser demo. The analysis itself is window.vansh (OCaml compiled to
// JavaScript); this file is only the editor, the layout and the interaction.
(function () {
  "use strict";

  const $ = (id) => document.getElementById(id);
  const esc = (s) => String(s).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c]);
  const key = (t, c) => t + "|" + c.toLowerCase();

  let files = [];
  let active = 0;
  let result = null;
  let selected = null;

  // ------------------------------------------------------------ files

  function loadSample(name) {
    files = window.VANSH_SAMPLES[name].map((f) => ({ ...f }));
    active = 0;
    selected = null;
    renderTabs();
    showFile();
    analyze();
  }

  function renderTabs() {
    const errFiles = new Set((result?.errors || []).map((e) => e.file));
    $("file-tabs").innerHTML = files
      .map((f, i) => `<button role="tab" aria-selected="${i === active}" data-i="${i}" class="${errFiles.has(f.name) ? "has-error" : ""}">${esc(f.name.split("/").pop())}</button>`)
      .join("");
    $("del-file").hidden = files.length <= 1;
  }

  function showFile() {
    const f = files[active];
    $("sql").value = f ? f.source : "";
    $("file-name").value = f ? f.name : "";
  }

  $("file-tabs").addEventListener("click", (e) => {
    const b = e.target.closest("button");
    if (!b) return;
    active = Number(b.dataset.i);
    renderTabs();
    showFile();
  });
  $("sql").addEventListener("input", () => {
    files[active].source = $("sql").value;
    schedule();
  });
  $("sql").addEventListener("keydown", (e) => {
    if (e.key === "Tab" && !e.shiftKey) {
      e.preventDefault();
      const t = e.target, s = t.selectionStart;
      t.setRangeText("    ", s, t.selectionEnd, "end");
      files[active].source = t.value;
      schedule();
    }
  });
  $("file-name").addEventListener("change", () => {
    let n = $("file-name").value.trim() || "model.sql";
    if (!n.endsWith(".sql")) n += ".sql";
    files[active].name = n;
    $("file-name").value = n;
    renderTabs();
    analyze();
  });
  $("add-file").addEventListener("click", () => {
    let i = files.length + 1;
    while (files.some((f) => f.name === `models/model_${i}.sql`)) i++;
    files.push({ name: `models/model_${i}.sql`, source: "select\n    \nfrom " });
    active = files.length - 1;
    renderTabs();
    showFile();
    $("sql").focus();
    analyze();
  });
  $("del-file").addEventListener("click", () => {
    if (files.length <= 1) return;
    files.splice(active, 1);
    active = Math.max(0, active - 1);
    renderTabs();
    showFile();
    analyze();
  });
  $("sample").addEventListener("change", (e) => loadSample(e.target.value));

  // ------------------------------------------------------------ analysis

  let timer = 0;
  function schedule() {
    clearTimeout(timer);
    timer = setTimeout(analyze, 180);
  }

  function analyze() {
    const t0 = performance.now();
    const json = window.vansh.analyze(files.map((f) => f.name), files.map((f) => f.source));
    result = JSON.parse(json);
    result.ms = performance.now() - t0;
    if (result.fatal) {
      $("problems").innerHTML = `<div class="problem err"><b>error</b><span>${esc(result.fatal)}</span></div>`;
      return;
    }
    if (selected && !nodeExists(selected)) selected = null;
    renderTabs();
    renderProblems();
    renderStats();
    renderGraph();
    renderTable();
    $("mermaid").textContent = window.vansh.mermaid(files.map((f) => f.name), files.map((f) => f.source));
  }

  function nodeExists(k) {
    return result.models.some((m) => m.columns.some((c) => key(m.name, c.name) === k)) ||
      result.sources.some((s) => s.columns.some((c) => key(s.name, c) === k));
  }

  function renderProblems() {
    const errs = result.errors.map((e) =>
      `<div class="problem err"><b>error</b><span class="loc" data-file="${esc(e.file)}" data-line="${e.line}">${esc(e.file.split("/").pop())}:${e.line}</span><span>${esc(e.message)}</span></div>`);
    const warns = result.models.flatMap((m) => m.warnings.map((w) =>
      `<div class="problem warn"><b>warn</b><span class="loc" data-model="${esc(m.name)}" data-file="${esc(m.file)}">${esc(m.name)}</span><span>${esc(w)}</span></div>`));
    $("problems").innerHTML = errs.concat(warns).join("");
  }

  $("problems").addEventListener("click", (e) => {
    const loc = e.target.closest(".loc");
    if (!loc) return;
    const i = files.findIndex((f) => f.name === loc.dataset.file);
    if (i < 0) return;
    active = i;
    renderTabs();
    showFile();
    const line = Number(loc.dataset.line || 0);
    if (line > 0) {
      const ta = $("sql");
      const lines = ta.value.split("\n");
      const start = lines.slice(0, line - 1).reduce((n, l) => n + l.length + 1, 0);
      ta.focus();
      ta.setSelectionRange(start, start + (lines[line - 1] || "").length);
      ta.scrollTop = Math.max(0, (line - 5) * 20);
    }
  });

  function renderStats() {
    const cols = result.models.reduce((n, m) => n + m.columns.length, 0);
    const warns = result.models.reduce((n, m) => n + m.warnings.length, 0);
    const parts = [
      [result.models.length, result.models.length === 1 ? "model" : "models"],
      [result.sources.length, result.sources.length === 1 ? "source" : "sources"],
      [cols, "columns"],
      [result.edges.length, "edges"],
      [result.unused.length, "unused"],
    ];
    let html = parts.map(([n, l]) => `<span><b>${n}</b> ${l}</span>`).join("");
    if (warns) html += `<span class="w"><b>${warns}</b> warning${warns === 1 ? "" : "s"}</span>`;
    if (result.errors.length) html += `<span class="e"><b>${result.errors.length}</b> error${result.errors.length === 1 ? "" : "s"}</span>`;
    html += `<span>${result.ms.toFixed(1)} ms</span>`;
    $("stats").innerHTML = html;
  }

  // ------------------------------------------------------------ graph

  const CARD_W = 214, HEAD_H = 40, ROW_H = 22, PAD = 8, COL_GAP = 118, CARD_GAP = 22, MARGIN = 20;

  function buildRelations() {
    const rels = new Map();
    for (const s of result.sources) rels.set(s.name, { name: s.name, kind: "source", cols: s.columns.slice(), depth: 0 });
    for (const m of result.models) rels.set(m.name, { name: m.name, kind: "model", cols: m.columns.map((c) => c.name), depth: 1, reads: m.reads });
    // longest path from a source; models arrive in dependency order
    for (const m of result.models) {
      const r = rels.get(m.name);
      let d = 0;
      for (const up of m.reads) if (rels.has(up) && up !== m.name) d = Math.max(d, rels.get(up).depth + 1);
      for (const c of m.columns) for (const s of c.sources) if (rels.has(s.table) && s.table !== m.name) d = Math.max(d, rels.get(s.table).depth + 1);
      r.depth = Math.max(1, d);
    }
    return rels;
  }

  function layout(rels) {
    const byDepth = [];
    for (const r of rels.values()) (byDepth[r.depth] ||= []).push(r);
    const place = () => {
      byDepth.forEach((col, d) => {
        let y = MARGIN;
        for (const r of col || []) {
          r.x = MARGIN + d * (CARD_W + COL_GAP);
          r.y = y;
          r.h = HEAD_H + Math.max(1, r.cols.length) * ROW_H + PAD;
          y += r.h + CARD_GAP;
        }
      });
    };
    const rowY = (r, c) => r.y + HEAD_H + Math.max(0, r.cols.findIndex((x) => x.toLowerCase() === c.toLowerCase())) * ROW_H + ROW_H / 2;
    place();
    // Barycenter ordering, a few sweeps each way, to reduce crossings.
    for (let sweep = 0; sweep < 4; sweep++) {
      for (let d = 1; d < byDepth.length; d++) {
        const col = byDepth[d];
        if (!col) continue;
        for (const r of col) {
          const ys = result.edges.filter((e) => e.to.table === r.name && rels.has(e.from.table)).map((e) => rowY(rels.get(e.from.table), e.from.column));
          r.bc = ys.length ? ys.reduce((a, b) => a + b, 0) / ys.length : r.y;
        }
        col.sort((a, b) => a.bc - b.bc);
        place();
      }
      for (let d = byDepth.length - 2; d >= 0; d--) {
        const col = byDepth[d];
        if (!col) continue;
        for (const r of col) {
          const ys = result.edges.filter((e) => e.from.table === r.name && rels.has(e.to.table)).map((e) => rowY(rels.get(e.to.table), e.to.column));
          r.bc = ys.length ? ys.reduce((a, b) => a + b, 0) / ys.length : r.y;
        }
        col.sort((a, b) => a.bc - b.bc);
        place();
      }
    }
    // Vertically centre short columns against the tallest.
    const heights = byDepth.map((col) => (col || []).reduce((h, r) => h + r.h + CARD_GAP, 0));
    const tallest = Math.max(...heights, 0);
    byDepth.forEach((col, d) => (col || []).forEach((r) => (r.y += (tallest - heights[d]) / 2)));
    const width = MARGIN * 2 + byDepth.length * CARD_W + (byDepth.length - 1) * COL_GAP;
    return { width, height: tallest + MARGIN, rowY };
  }

  function renderGraph() {
    const svg = $("graph");
    const rels = buildRelations();
    const { width, height, rowY } = layout(rels);
    const unused = new Set(result.unused.map((n) => key(n.table, n.column)));
    let edges = "";
    result.edges.forEach((e, i) => {
      const a = rels.get(e.from.table), b = rels.get(e.to.table);
      if (!a || !b) return;
      const x1 = a.x + CARD_W, y1 = rowY(a, e.from.column), x2 = b.x, y2 = rowY(b, e.to.column);
      const dx = Math.max(40, (x2 - x1) / 2);
      edges += `<path class="edge ${e.kind}" data-i="${i}" d="M${x1},${y1} C${x1 + dx},${y1} ${x2 - dx},${y2} ${x2},${y2}"/>`;
    });
    let cards = "";
    for (const r of rels.values()) {
      const title = r.name.length > 26 ? r.name.slice(0, 25) + "…" : r.name;
      cards += `<g class="card" transform="translate(${r.x},${r.y})">
        <rect class="card-bg" width="${CARD_W}" height="${r.h}" rx="10"/>
        <path class="card-head ${r.kind}" d="M0,10 a10,10 0 0 1 10,-10 h${CARD_W - 20} a10,10 0 0 1 10,10 v${HEAD_H - 14} h-${CARD_W} z"/>
        <text class="card-kind" x="12" y="15">${r.kind}</text>
        <text class="card-title" x="12" y="31"><title>${esc(r.name)}</title>${esc(title)}</text>`;
      r.cols.forEach((c, i) => {
        const k = key(r.name, c);
        const label = c.length > 27 ? c.slice(0, 26) + "…" : c;
        cards += `<g class="row${unused.has(k) ? " unused" : ""}" data-k="${esc(k)}" transform="translate(0,${HEAD_H + i * ROW_H})">
          <rect class="row-bg" x="4" width="${CARD_W - 8}" height="${ROW_H - 2}" rx="5"/>
          <text x="12" y="15">${esc(label)}</text></g>`;
      });
      if (!r.cols.length) cards += `<text class="card-kind" x="12" y="${HEAD_H + 14}">no columns read</text>`;
      cards += `</g>`;
    }
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
    svg.dataset.w = width;
    svg.dataset.h = height;
    svg.innerHTML = `<g>${edges}</g><g>${cards}</g>`;
    applyZoom();
    applySelection();
  }

  // null = fit the width of the pane (never below 55%, so text stays legible)
  let zoom = null;
  function applyZoom() {
    const svg = $("graph");
    const w = Number(svg.dataset.w || 0), h = Number(svg.dataset.h || 0);
    const avail = $("graph-scroll").clientWidth - 4;
    const scale = zoom ?? Math.min(1, Math.max(0.55, avail / w));
    svg.setAttribute("width", Math.round(w * scale));
    svg.setAttribute("height", Math.round(h * scale));
    $("zoom-fit").textContent = zoom === null ? "Fit" : Math.round(scale * 100) + "%";
  }
  const currentScale = () => Number($("graph").getAttribute("width")) / Number($("graph").dataset.w || 1);
  $("zoom-in").addEventListener("click", () => { zoom = Math.min(2, currentScale() * 1.2); applyZoom(); });
  $("zoom-out").addEventListener("click", () => { zoom = Math.max(0.3, currentScale() / 1.2); applyZoom(); });
  $("zoom-fit").addEventListener("click", () => { zoom = null; applyZoom(); });
  window.addEventListener("resize", () => { if (zoom === null) applyZoom(); });

  // Upstream / downstream closure over the edge list.
  function closure(start, forward) {
    const seen = new Map([[start, null]]);
    const queue = [start];
    while (queue.length) {
      const k = queue.shift();
      result.edges.forEach((e, i) => {
        const from = key(e.from.table, e.from.column), to = key(e.to.table, e.to.column);
        const [here, next] = forward ? [from, to] : [to, from];
        if (here === k && !seen.has(next)) {
          seen.set(next, { edge: i, kind: e.kind, depth: (seen.get(k)?.depth || 0) + 1 });
          queue.push(next);
        }
      });
    }
    seen.delete(start);
    return seen;
  }

  function applySelection() {
    const svg = $("graph");
    svg.querySelectorAll(".row").forEach((g) => g.classList.remove("sel", "up", "down", "on"));
    svg.querySelectorAll(".edge").forEach((p) => p.classList.remove("up", "down"));
    if (!selected) {
      svg.classList.remove("focus");
      $("inspector").hidden = true;
      return;
    }
    const up = closure(selected, false), down = closure(selected, true);
    svg.classList.add("focus");
    const mark = (k, cls) => {
      const g = svg.querySelector(`.row[data-k="${CSS.escape(k)}"]`);
      if (g) g.classList.add(cls, "on");
    };
    mark(selected, "sel");
    up.forEach((_, k) => mark(k, "up"));
    down.forEach((_, k) => mark(k, "down"));
    // highlight every edge between nodes on the path (not just the BFS tree)
    const upSet = new Set([selected, ...up.keys()]), downSet = new Set([selected, ...down.keys()]);
    result.edges.forEach((e, i) => {
      const f = key(e.from.table, e.from.column), t = key(e.to.table, e.to.column);
      const p = svg.querySelector(`.edge[data-i="${i}"]`);
      if (!p) return;
      if (upSet.has(f) && upSet.has(t)) p.classList.add("up");
      else if (downSet.has(f) && downSet.has(t)) p.classList.add("down");
    });
    renderInspector(up, down);
  }

  // Bring a row into view inside the scrolling graph, clear of the inspector.
  function scrollToRow(k) {
    const g = $("graph").querySelector(`.row[data-k="${CSS.escape(k)}"]`);
    if (!g) return;
    const box = g.getBoundingClientRect(), view = $("graph-scroll").getBoundingClientRect();
    const sc = $("graph-scroll");
    sc.scrollTop += box.top - view.top - view.height * 0.3;
    sc.scrollLeft += box.left - view.left - 40;
  }

  function renderInspector(up, down) {
    const [table, column] = selected.split("|");
    const model = result.models.find((m) => m.name === table);
    const realCol = model ? (model.columns.find((c) => c.name.toLowerCase() === column)?.name || column) : column;
    const label = (k) => k.replace("|", ".");
    const list = (m, cls) => [...m.entries()].sort((a, b) => a[1].depth - b[1].depth)
      .map(([k, v]) => `<li class="${cls}"><span>${esc(label(k))}</span><span class="kd">${v.kind}</span></li>`).join("");
    const filtered = result.models.filter((m) => m.row_filters.some((f) => f.table === table && f.column.toLowerCase() === column)).map((m) => m.name);
    const isUnused = result.unused.some((n) => key(n.table, n.column) === selected);
    let html = `<button class="ghost small close" id="close-insp" aria-label="Close">✕</button><h3>${esc(table)}.${esc(realCol)}</h3>`;
    if (isUnused) html += `<p class="note">Produced here but never used by any downstream model.</p>`;
    html += `<h4>Computed from · ${up.size}</h4>${up.size ? `<ul>${list(up, "up-i")}</ul>` : `<p class="note">${model ? "constants only" : "a source column"}</p>`}`;
    html += `<h4>Feeds · ${down.size}</h4>${down.size ? `<ul>${list(down, "down-i")}</ul>` : `<p class="note">nothing downstream</p>`}`;
    if (filtered.length) html += `<h4>Also filters or groups rows in</h4><ul>${filtered.map((m) => `<li><span>${esc(m)}</span></li>`).join("")}</ul>`;
    $("inspector").innerHTML = html;
    $("inspector").hidden = false;
    $("close-insp").onclick = () => { selected = null; applySelection(); };
  }

  $("graph").addEventListener("click", (e) => {
    const row = e.target.closest(".row");
    if (!row) { selected = null; applySelection(); return; }
    selected = selected === row.dataset.k ? null : row.dataset.k;
    applySelection();
  });

  // ------------------------------------------------------------ table

  function renderTable() {
    const rows = result.models.flatMap((m) => m.columns.map((c, i) => `<tr>
      <td class="m">${i === 0 ? esc(m.name) : ""}</td><td class="c">${esc(c.name)}</td>
      <td>${c.sources.length ? c.sources.map((s) => `<span class="src">${esc(s.table)}.${esc(s.column)}${s.kind === "direct" ? "" : `<i class="${s.kind}">${s.kind}</i>`}</span>`).join("") : '<span class="muted">constant</span>'}</td></tr>`));
    $("table").innerHTML = `<table><thead><tr><th>model</th><th>column</th><th>computed from</th></tr></thead><tbody>${rows.join("")}</tbody></table>`;
  }

  document.querySelectorAll(".view-tabs button").forEach((b) =>
    b.addEventListener("click", () => {
      document.querySelectorAll(".view-tabs button").forEach((x) => x.setAttribute("aria-selected", String(x === b)));
      for (const v of ["graph", "table", "mermaid"]) $("view-" + v).hidden = v !== b.dataset.view;
    }));

  $("copy-mermaid").addEventListener("click", async () => {
    await navigator.clipboard.writeText($("mermaid").textContent);
    $("copy-mermaid").textContent = "Copied";
    setTimeout(() => ($("copy-mermaid").textContent = "Copy"), 1200);
  });

  loadSample("dbt");
  // Start with something interesting selected.
  selected = window.innerWidth > 860 ? key("raw.payments", "amount") : null;
  if (selected && !nodeExists(selected)) selected = null;
  applySelection();
  if (selected) scrollToRow(selected);
})();
