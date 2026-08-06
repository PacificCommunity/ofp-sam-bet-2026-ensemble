#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite to build the interactive viewer.", call. = FALSE)
}

series <- readRDS("data/ensemble/ensemble-timeseries.rds")
fit <- read.csv("data/ensemble/fit-diagnostics.csv", check.names = FALSE)
design <- read.csv("data/ensemble/successful-model-design.csv", check.names = FALSE)
management <- read.csv("data/ensemble/management-quantities.csv", check.names = FALSE)

ids <- sort(unique(series$ensemble_id))
if (
  length(ids) != 88L ||
  !identical(ids, sort(fit$ensemble_id)) ||
  !identical(ids, sort(design$ensemble_id)) ||
  !identical(ids, sort(management$ensemble_id))
) {
  stop("The viewer requires the validated 88-model public payload.", call. = FALSE)
}

design <- design[match(ids, design$ensemble_id), ]
fit <- fit[match(ids, fit$ensemble_id), ]
management <- management[match(ids, management$ensemble_id), ]
series <- series[order(series$ensemble_id, series$year), ]

short_reporting <- ifelse(design$tag_reporting == "inclusion", "include", "exclude")
model_meta <- data.frame(
  id = ids,
  h = round(design$steepness, 6),
  tau = round(design$tag_tau, 1),
  K = round(design$tag_mixing_k_cutoff, 2),
  reporting = short_reporting,
  M0 = round(design$m_age40_quarterly, 6),
  creep_primary = round(100 * design$effort_creep_primary, 2),
  creep_secondary = round(100 * design$effort_creep_secondary, 3),
  mgc = signif(fit$maximum_gradient, 7),
  hessian = ifelse(fit$positive_definite_hessian, "PDH", "Near-PDH"),
  objective = round(fit$objective_function, 3),
  depletion_recent = round(management$sb_recent_sb0, 6),
  sb_sbmsy_recent = round(management$sb_recent_sbmsy, 6),
  f_fmsy_recent = round(management$f_recent_fmsy, 6)
)

series_payload <- lapply(ids, function(id) {
  value <- series[series$ensemble_id == id, ]
  list(
    id = id,
    year = as.integer(value$year),
    depletion = round(value$depletion, 7),
    recruitment = round(value$recruitment, 5),
    spawning = round(value$spawning_potential, 5),
    fishing = round(value$fishing_mortality, 7)
  )
})

payload <- list(models = model_meta, series = series_payload)
json <- jsonlite::toJSON(payload, dataframe = "rows", auto_unbox = TRUE, digits = 9)
json <- gsub("</", "<\\/", json, fixed = TRUE)
output_dir <- Sys.getenv("REPORT_OUTPUT_DIR", "results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(output_dir, "bet-2026-ensemble-interactive-viewer.html")

html <- paste0(
"<!doctype html><html lang='en'><head><meta charset='utf-8'>",
"<meta name='viewport' content='width=device-width,initial-scale=1'>",
"<title>BET 2026 ensemble model results</title><style>",
":root{--ink:#17364a;--blue:#075b73;--teal:#16899a;--orange:#d66b00;--paper:#fff;--wash:#edf4f6;--line:#d7e2e7}",
"*{box-sizing:border-box}body{margin:0;background:var(--wash);color:var(--ink);font:15px/1.45 system-ui,-apple-system,Segoe UI,sans-serif}",
"header{background:#0b2b3d;color:white;padding:20px 28px;border-bottom:5px solid #d65d32}header h1{margin:0;font-size:1.55rem}header p{margin:.35rem 0 0;color:#cce0e8}",
"main{max-width:1500px;margin:auto;padding:18px}.toolbar,.card{background:var(--paper);border:1px solid #c9d9df;border-radius:10px;box-shadow:0 2px 8px #17364a12}",
".toolbar{display:grid;grid-template-columns:minmax(210px,1fr) 2fr auto;gap:14px;align-items:end;padding:14px 16px;margin-bottom:14px}",
"label{display:block;font-size:.76rem;font-weight:750;text-transform:uppercase;letter-spacing:.045em;color:#547083;margin-bottom:4px}select,input,button{font:inherit}",
"select,input{width:100%;padding:8px 10px;border:1px solid #aec4ce;border-radius:6px;background:white}button{padding:8px 12px;border:0;border-radius:6px;background:var(--blue);color:white;font-weight:700;cursor:pointer}",
".details{display:grid;grid-template-columns:repeat(12,minmax(95px,1fr));gap:1px;background:#cbdce3;border:1px solid #cbdce3;border-radius:9px;overflow:hidden;margin-bottom:14px}.detail{background:white;padding:9px}.detail b{display:block;font-size:.88rem}.detail span{font-size:.68rem;color:#607988;text-transform:uppercase}.detail.status-ok{box-shadow:inset 0 4px #3b8d69}.detail.status-alert{box-shadow:inset 0 4px #d66b00}",
".plots{display:grid;grid-template-columns:1fr 1fr;gap:14px}.card{padding:10px}.panel-label{font:700 1rem Georgia,serif;margin:2px 7px;color:#163e55}canvas{display:block;width:100%;height:330px}",
".legend{display:flex;gap:18px;justify-content:center;flex-wrap:wrap;padding:12px;font-size:.86rem}.key{display:inline-flex;align-items:center;gap:7px}.line{width:30px;height:0;border-top:3px solid}.faint{border-color:#98aab2;border-top-width:1px}.selected{border-color:#d33a2c}.median{border-color:#075b73}",
".table-card{margin-top:14px;padding:15px}.table-wrap{max-height:360px;overflow:auto;border:1px solid #d7e2e7}table{width:100%;border-collapse:collapse;font-size:.82rem}th{position:sticky;top:0;background:#0b586d;color:white}th,td{padding:7px 8px;border-bottom:1px solid #e1eaee;text-align:right;white-space:nowrap}th:first-child,td:first-child{text-align:left}tbody tr{cursor:pointer}tbody tr:hover,tbody tr.active{background:#e7f5f7}",
"@media(max-width:1100px){.details{grid-template-columns:repeat(6,1fr)}}@media(max-width:900px){.toolbar{grid-template-columns:1fr}.plots{grid-template-columns:1fr}.details{grid-template-columns:repeat(3,1fr)}}",
"</style></head><body><header><h1>BET 2026 ensemble model results</h1><p>88 completed assessment configurations; select a row to inspect its settings, diagnostics, status quantities and annual trajectories.</p></header><main>",
"<section class='toolbar'><div><label for='model'>Model</label><select id='model'></select></div><div><label for='search'>Filter table</label><input id='search' placeholder='e.g. tau 1.3, Near-PDH, K 0.20'></div><button id='reset'>Show first model</button></section>",
"<section class='details' id='details'></section><section class='plots'>",
"<div class='card'><div class='panel-label'>a</div><canvas id='depletion'></canvas></div>",
"<div class='card'><div class='panel-label'>b</div><canvas id='recruitment'></canvas></div>",
"<div class='card'><div class='panel-label'>c</div><canvas id='spawning'></canvas></div>",
"<div class='card'><div class='panel-label'>d</div><canvas id='fishing'></canvas></div></section>",
"<div class='legend'><span class='key'><i class='line faint'></i>Other ensemble models</span><span class='key'><i class='line median'></i>Ensemble median</span><span class='key'><i class='line selected'></i>Selected model</span></div>",
"<section class='card table-card'><label>Model settings, diagnostics and status quantities</label><div class='table-wrap'><table><thead><tr><th>Model</th><th>h</th><th>&tau;</th><th>K</th><th>Pre-mixing reports</th><th>M<sub>0</sub> quarter<sup>&minus;1</sup></th><th>Effort creep (%)</th><th>MGC</th><th>Hessian</th><th>Objective</th><th>SB<sub>recent</sub>/SB<sub>F=0</sub></th><th>SB<sub>recent</sub>/SB<sub>MSY</sub></th><th>F<sub>recent</sub>/F<sub>MSY</sub></th></tr></thead><tbody id='rows'></tbody></table></div></section>",
"</main><script>const DATA=", json, ";",
"const specs={depletion:{label:'SB(t) / SB(F=0,t)',lrp:.2},recruitment:{label:'Recruitment (millions of fish)'},spawning:{label:'Spawning potential (10³ MT)'},fishing:{label:'F (year⁻¹)'}};",
"const byId=new Map(DATA.series.map(x=>[x.id,x]));const meta=new Map(DATA.models.map(x=>[x.id,x]));const select=document.getElementById('model');",
"DATA.models.forEach(m=>{const o=document.createElement('option');o.value=m.id;o.textContent=`${m.id} | h=${m.h.toFixed(3)} | τ=${m.tau.toFixed(1)} | K=${m.K.toFixed(2)} | RR=${m.reporting} | M₀=${m.M0.toFixed(4)}`;select.appendChild(o)});",
"function median(v){const x=v.slice().sort((a,b)=>a-b),n=x.length;return n%2?x[(n-1)/2]:(x[n/2-1]+x[n/2])/2}",
"function fmt(v,d=3){return Number(v).toFixed(d)}function sci(v){return Number(v).toExponential(2).replace('e-',' × 10⁻').replace('e+',' × 10⁺')}",
"function draw(key){const c=document.getElementById(key),r=c.getBoundingClientRect(),d=devicePixelRatio||1;c.width=r.width*d;c.height=r.height*d;const x=c.getContext('2d');x.scale(d,d);const w=r.width,h=r.height,m={l:72,r:18,t:12,b:48},pw=w-m.l-m.r,ph=h-m.t-m.b;const chosen=byId.get(select.value),all=DATA.series,years=chosen.year;let ymax=0;all.forEach(s=>s[key].forEach(v=>{if(v>ymax)ymax=v}));ymax*=1.05;if(key==='depletion')ymax=Math.max(1.03,ymax);const X=i=>m.l+pw*i/(years.length-1),Y=v=>m.t+ph*(1-v/ymax);x.fillStyle='white';x.fillRect(0,0,w,h);x.strokeStyle='#dce5e9';x.lineWidth=.7;for(let k=0;k<=4;k++){const yy=m.t+ph*k/4;x.beginPath();x.moveTo(m.l,yy);x.lineTo(w-m.r,yy);x.stroke();const val=ymax*(1-k/4);x.fillStyle='#456273';x.font='12px system-ui';x.textAlign='right';x.fillText(val<2?val.toFixed(2):Math.round(val).toLocaleString(),m.l-8,yy+4)}[1960,1980,2000,2020].forEach(y=>{const i=years.indexOf(y);if(i>=0){const xx=X(i);x.strokeStyle='#e3eaed';x.beginPath();x.moveTo(xx,m.t);x.lineTo(xx,h-m.b);x.stroke();x.fillStyle='#456273';x.textAlign='center';x.fillText(y,xx,h-m.b+19)}});if(specs[key].lrp){x.setLineDash([6,5]);x.strokeStyle='#b83232';x.lineWidth=1.2;x.beginPath();x.moveTo(m.l,Y(specs[key].lrp));x.lineTo(w-m.r,Y(specs[key].lrp));x.stroke();x.setLineDash([]);x.fillStyle='#b83232';x.textAlign='left';x.fillText('LRP',m.l+7,Y(specs[key].lrp)-5)}function path(values,col,lw,a){x.strokeStyle=col;x.globalAlpha=a;x.lineWidth=lw;x.beginPath();values.forEach((v,i)=>i?x.lineTo(X(i),Y(v)):x.moveTo(X(i),Y(v)));x.stroke();x.globalAlpha=1}all.forEach(s=>path(s[key],'#96a9b1',.55,.22));const med=years.map((_,i)=>median(all.map(s=>s[key][i])));path(med,'#075b73',2.4,1);path(chosen[key],'#d13228',2.7,1);x.strokeStyle='#2c4554';x.lineWidth=.8;x.strokeRect(m.l,m.t,pw,ph);x.fillStyle='#17364a';x.font='bold 13px Georgia';x.textAlign='center';x.fillText('Year',m.l+pw/2,h-9);x.save();x.translate(18,m.t+ph/2);x.rotate(-Math.PI/2);x.fillText(specs[key].label,0,0);x.restore()}",
"function details(){const m=meta.get(select.value);const vals=[['Model',m.id,''],['Steepness, h',fmt(m.h,4),''],['Tag overdispersion, τ',fmt(m.tau,1),''],['Mixing cutoff, K',fmt(m.K,2),''],['Pre-mixing reports',m.reporting,''],['M₀ (quarter⁻¹)',fmt(m.M0,4),''],['Effort creep',`${fmt(m.creep_primary,1)} / ${fmt(m.creep_secondary,2)}%`, ''],['MGC',Number(m.mgc).toExponential(2),''],['Hessian',m.hessian,''],['SBrecent/SBF=0 · 2021–2024',fmt(m.depletion_recent,3),m.depletion_recent>=.2?'status-ok':'status-alert'],['SBrecent/SBMSY · 2021–2024',fmt(m.sb_sbmsy_recent,3),m.sb_sbmsy_recent>=1?'status-ok':'status-alert'],['Frecent/FMSY · 2020–2023',fmt(m.f_fmsy_recent,3),m.f_fmsy_recent<=1?'status-ok':'status-alert']];document.getElementById('details').innerHTML=vals.map(v=>`<div class='detail ${v[2]}'><span>${v[0]}</span><b>${v[1]}</b></div>`).join('')}",
"function render(){details();Object.keys(specs).forEach(draw);document.querySelectorAll('#rows tr').forEach(r=>r.classList.toggle('active',r.dataset.id===select.value))}",
"function table(q=''){const s=q.trim().toLowerCase();document.getElementById('rows').innerHTML=DATA.models.filter(m=>!s||JSON.stringify(m).toLowerCase().includes(s)).map(m=>`<tr data-id='${m.id}'><td>${m.id}</td><td>${fmt(m.h,4)}</td><td>${fmt(m.tau,1)}</td><td>${fmt(m.K,2)}</td><td>${m.reporting}</td><td>${fmt(m.M0,4)}</td><td>${fmt(m.creep_primary,1)} / ${fmt(m.creep_secondary,2)}</td><td>${Number(m.mgc).toExponential(2)}</td><td>${m.hessian}</td><td>${Number(m.objective).toLocaleString()}</td><td>${fmt(m.depletion_recent,3)}</td><td>${fmt(m.sb_sbmsy_recent,3)}</td><td>${fmt(m.f_fmsy_recent,3)}</td></tr>`).join('');document.querySelectorAll('#rows tr').forEach(r=>r.onclick=()=>{select.value=r.dataset.id;render()})}",
"select.onchange=render;document.getElementById('search').oninput=e=>table(e.target.value);document.getElementById('reset').onclick=()=>{select.selectedIndex=0;render()};window.onresize=()=>Object.keys(specs).forEach(draw);table();render();",
"</script></body></html>"
)

writeLines(html, output_file, useBytes = TRUE)
cat("Wrote ", output_file, " with ", length(ids), " models.\n", sep = "")
