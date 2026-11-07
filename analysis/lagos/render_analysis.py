"""Render the standalone Lagos HTML from aggregate outputs; no respondent data."""
from pathlib import Path
import argparse, base64, csv, hashlib, html, json, math, re
import xml.etree.ElementTree as ET

ap = argparse.ArgumentParser()
ap.add_argument('--data-dir', required=True)
ap.add_argument('--reference-html', required=True)
ap.add_argument('--output', required=True)
ap.add_argument('--model-dir')
ap.add_argument('--pca-dir')
a = ap.parse_args()
root, ref, output = Path(a.data_dir), Path(a.reference_html), Path(a.output)
reference_hash = hashlib.sha256(ref.read_bytes()).hexdigest()
source = ref.read_text()
p = json.loads((root / 'report_data.json').read_text())
esc = lambda v: html.escape(str(v), quote=True)
pct = lambda v: 'Not estimated' if v is None else f'{v:.1f}%'
canon = lambda v: re.sub('[^A-Z0-9]', '', v.upper())
NS = 'http://www.w3.org/2000/svg'
ET.register_namespace('', NS)
coverage_svg = re.search(r'<svg[^>]*id="map-coverage".*?</svg>', source, re.S).group()
category_svg = re.search(r'<svg[^>]*id="map-median".*?</svg>', source, re.S).group()
base_css = next(s for s in re.findall(r'<style[^>]*>(.*?)</style>', source, re.S) if '--ink:' in s)
names = {}
for m in re.finditer(r'<g class="lga-region" data-key="([^"]+)"[^>]*aria-label="([^"]+)"', coverage_svg):
    names[m[1]] = html.unescape(m[2]).split('.')[0]
assert len(names) == 20
D, S = p['demand_variables'], p['supply_variables']
DL, SL = dict(zip(D, p['demand_labels'])), dict(zip(S, p['supply_labels']))
colnames = dict(DL, **SL)
cols = dict(both='#78549b', demand='#047f8b', supply='#cd7729', neither='#bacad2', unlinked='#edf0f3')
cats = dict(both='Both', demand='Demand only', supply='Supply only', neither='Neither flagged', unlinked='Not assessed')
R = {r['geo']: r for r in p['rates']}
B = {r['field']: r for r in p['bars']}
interval_note = ('Whiskers are 95% confidence intervals using explicitly supplied confirmed survey-design fields.' if p['audit']['design_fields_confirmed'] else 'Confidence intervals are withheld because official EA/PSU and stratum identifiers remain unconfirmed; clusters are not inferred from household IDs.')
preference_other_n = B['user']['n'] - B['wanted_start']['n'] - B['other_start']['n']
method_other_n = B['user']['n'] - B['modern']['n'] - B['traditional']['n']
C = {r['geo']: r for r in p['coverage']}
meta = {r['geo']: r for r in p['area_metadata']}
HM = {(r['geo'], r['side'], r['variable']): r for r in p['heatmap']}
scores = {(r['method'], r['geo']): r for r in p['scores']}
trace = {(r['geo'], r['side'], r['variable']): r for r in p['component_trace']}
planning = {r['geo']: r for r in p['planning']}
thresholds = {r['method']: r for r in p['thresholds']}
areas = sorted(names, key=lambda g: names[g])
assert set(R).issubset(names) and len(R) == 17 and sum(r['n'] for r in R.values()) == 2159
DATA = {}
for g in areas:
    linked = g in R
    q = scores.get(('zscore', g))
    DATA[g] = dict(name=names[g], linked=linked, women=C[g]['women'], facilities=C[g]['facilities'],
                   n=R[g]['n'] if linked else None, cpr=100*R[g]['cpr']['estimate'] if linked else None,
                   mcpr=100*R[g]['mcpr']['estimate'] if linked else None,
                   category=cats[q['category']] if linked else cats['unlinked'],
                   demand=q['demand_score'] if linked else None, supply=q['supply_score'] if linked else None)

def map_svg(text, mode):
    el = ET.fromstring(text)
    el.set('id', 'map-' + mode)
    for node in el.iter():
        g = node.attrib.get('data-key')
        if g not in DATA:
            continue
        node.set('data-rule', mode)
        d = DATA[g]
        description = f'{d["name"]}. ' + (f'{d["category"]}. Any use {d["cpr"]:.1f}%, modern use {d["mcpr"]:.1f}%, among SURGO FPN2=1 women; n={d["n"]}.' if d['linked'] else 'Facility data only; no SURGO need-group estimate.')
        if 'lga-region' in node.attrib.get('class', ''):
            node.set('aria-label', description)
            title = node.find(f'{{{NS}}}title')
            if title is not None:
                title.text = description
            if mode == 'zscore':
                color = cols[scores['zscore', g]['category']] if d['linked'] else cols['unlinked']
                for child in node:
                    if child.tag.endswith('path'):
                        child.set('fill', color)
    return ET.tostring(el, encoding='unicode')

def mapbox(svg, mode):
    return f'<div class="mapbox">{svg}<div class="map-toolbar"><span>Hover, tap or focus an LGA for weighted use estimates and sample size</span><button class="expand-map" type="button">⤢ Enlarge map</button></div></div>'

def legend():
    counts = {k: sum(r['category'] == k for r in p['scores'] if r['method'] == 'zscore') for k in cols if k != 'unlinked'}
    counts['unlinked'] = 3
    return '<div class="map-legend">' + ''.join(f'<span><i style="background:{cols[k]}"></i>{cats[k]} <b>{counts[k]}</b></span>' for k in cols) + '</div>'

def download(name, label):
    b = base64.b64encode((root/name).read_bytes()).decode()
    return f'<a class="download" download="{esc(name)}" href="data:text/csv;base64,{b}">{esc(label)} ↓</a>'

def chart_svg():
    w, h, left, bw = 1500, 955, 390, 720
    rows = p['bars']
    if rows[-1]['field'] != 'unresolved':
        h = 865
    svg = [f'<svg class="use-chart" viewBox="0 0 {w} {h}" xmlns="{NS}" role="img" aria-label="Weighted contraceptive use and intention among 2159 women with SURGO FPN2=1">']
    svg += ['<rect width="1500" height="955" fill="white"/>']
    for r in rows:
        y = 50 + r['y'] * 57
        v = 100*r['estimate']
        assert r['denominator'] == 2159
        name = r['label']
        svg.append(f'<text x="{left-15}" y="{y+24}" text-anchor="end" class="chart-label" style="font-size:{16 if r["indent"] else 18}px">{esc(name)}</text>')
        svg.append(f'<rect x="{left}" y="{y}" width="{bw}" height="39" rx="1" fill="#f1f5f6"/>')
        svg.append(f'<rect x="{left}" y="{y}" width="{bw*v/100:.3f}" height="39" fill="{r["color"]}"/>')
        if r['lo'] is not None and r['hi'] is not None:
            lx, hx, cy = left+bw*r['lo'], left+bw*r['hi'], y+19.5
            svg.append(f'<path d="M{lx:.2f},{cy:.2f} H{hx:.2f} M{lx:.2f},{cy-6:.2f} V{cy+6:.2f} M{hx:.2f},{cy-6:.2f} V{cy+6:.2f}" stroke="#263f4b" stroke-width="1.8" fill="none"/>')
        svg.append(f'<text x="{left+bw+24}" y="{y+24}" class="chart-number">n = {r["n"]:,} · {v:.1f}%</text>')
    axis_y = h-70
    for t in range(0, 101, 20):
        svg.append(f'<text x="{left+bw*t/100}" y="{axis_y}" text-anchor="middle" class="chart-tick">{t}%</text>')
    svg.append(f'<text x="{left+bw/2}" y="{axis_y+35}" text-anchor="middle" class="chart-axis">Weighted share of all women with FPN2 = 1 (N = 2,159)</text></svg>')
    return ''.join(svg)

def mix(a, b, t):
    return '#'+''.join(f'{round(int(a[i:i+2],16)*(1-t)+int(b[i:i+2],16)*t):02x}' for i in (1,3,5))

def heatmap():
    headers = ''.join(f'<th scope="col">{esc(colnames[k])}</th>' for k in D)
    headers += '<th class="hm-gap"></th>' + ''.join(f'<th scope="col">{esc(colnames[k])}</th>' for k in S)
    rows = []
    for g in ['LAGOS']+areas:
        m = meta[g]
        name = 'Linked Lagos' if g == 'LAGOS' else names[g]
        sub = f'n = {m["n_need"]:,} women in need' if m['n_need'] else 'Facility data only'
        if m['n_need'] and m['n_need'] < 30:
            name += ' †'
        cells = [f'<th scope="row">{esc(name)}<small>{esc(sub)}</small></th>']
        for side, fields in [('Demand', D), ('Supply', S)]:
            if side == 'Supply': cells.append('<td class="hm-gap"></td>')
            for k in fields:
                r = HM[g, side, k]
                if r['value'] is None:
                    cells.append('<td class="hm-missing" title="No valid estimate">—</td>')
                    continue
                t = r['value']/100 if side == 'Demand' else 1-r['value']/100
                color = mix('#eff8f8', '#057483', t) if side == 'Demand' else mix('#fff6e9', '#bf570e', t)
                tip = f'{colnames[k]}: {r["value"]:.3f}%. Included values: {r["n_valid"]}/{r["n_total"]}.'
                if side == 'Supply' and k == 'free' and g in ('LAGOS','IBEJULEKKI'):
                    tip += ' Includes one explicitly authorized Urban no-provision zero exception.'
                cells.append(f'<td title="{esc(tip)}" style="background:{color};color:{"white" if t>.67 else "#233e4c"}">{r["value"]:.1f}%</td>')
        rows.append('<tr'+(' class="lagos-row"' if g=='LAGOS' else '')+'>'+''.join(cells)+'</tr>')
    return '<div class="table-wrap hm-wrap"><table class="joint-heatmap"><thead><tr><th rowspan="2">Area</th><th colspan="6" class="demand-head">Demand · SURGO<small>Weighted women with FPN2 = 1</small></th><th class="hm-gap"></th><th colspan="6" class="supply-head">Supply · Manitoba<small>All facilities within each LGA · Rural + Urban</small></th></tr><tr>'+headers+'</tr></thead><tbody>'+''.join(rows)+'</tbody></table></div>'

method_order = ['rank','raw','robust','median_count','zscore']
method_labels = ['Rank average','Mean percentages','Median/IQR','Median count','Z-score average']
comparison = []
comparison_csv = []
for g in areas:
    vals = [cats[scores[mid,g]['category']] if g in R else cats['unlinked'] for mid in method_order]
    consistent = len(set(vals)) == 1
    comparison.append('<tr><th scope="row">'+esc(names[g])+'</th>'+''.join(f'<td class="category-cell {scores[mid,g]["category"] if g in R else "unlinked"}">{esc(v)}</td>' for mid,v in zip(method_order,vals))+'</tr>')
    comparison_csv.append(dict(LGA=names[g], **dict(zip(method_labels,vals))))
with (root/'lga_classification_comparison.csv').open('w',newline='') as f:
    cw=csv.DictWriter(f,fieldnames=list(comparison_csv[0]));cw.writeheader();cw.writerows(comparison_csv)
compare_table = '<div class="table-wrap"><table class="comparison"><thead><tr><th>LGA</th>'+''.join(f'<th>{t}</th>' for t in method_labels)+'</tr></thead><tbody>'+''.join(comparison)+'</tbody></table></div>'

def priorities(g, side, fields):
    if not fields: return '<span class="empty">—</span>'
    return '<ol class="domain-list">'+''.join(f'<li title="Component z-score {trace[g,side,k]["zscore"]:.3f}"><span>{esc(colnames[k])}</span><b>{HM[g,side,k]["value"]:.1f}%</b></li>' for k in fields)+'</ol>'

plan_rows = []
plan_csv = []
for g in areas:
    linked = g in planning
    q = planning.get(g)
    category = q['category'] if linked else 'unlinked'
    df = q['demand_flag'] if linked else False
    sf = q['supply_flag'] if linked else False
    ds = f'{q["demand_score"]:.3f}' if linked else '—'
    ss = f'{q["supply_score"]:.3f}' if linked else '—'
    yes = lambda flag: ('Yes' if flag else 'No') if linked else 'Not assessed'
    dq = priorities(g,'Demand',q['top_demand']) if df else '<span class="empty">—</span>'
    sq = priorities(g,'Supply',q['top_supply']) if sf else '<span class="empty">—</span>'
    mp = HM[g,'Supply','popular_stock']
    coverage = f'{mp["n_valid"]}/{mp["n_total"]} with popular-product stock status'
    n = str(q['n']) if linked else '—'
    flag_html = lambda flag: f'<span class="flag {"yes" if flag else "no"}">{yes(flag)}</span>' if linked else '<span class="empty">Not assessed</span>'
    category_color = '#687f8c' if category in ('neither', 'unlinked') else cols[category]
    plan_rows.append(f'<tr data-key="{g}"><th scope="row">{esc(names[g])}<small class="category-text" style="color:{category_color}">{cats[category]}</small></th><td>{flag_html(df)}</td><td>{dq}</td><td>{flag_html(sf)}</td><td>{sq}</td></tr>')
    plan_csv.append(dict(LGA=names[g], n_need=q['n'] if linked else '', demand_z_score=q['demand_score'] if linked else '', supply_readiness_z_score=q['supply_score'] if linked else '', category=cats[category], demand_flag=yes(df), top_demand='; '.join(colnames[k] for k in q['top_demand']) if df else '', supply_flag=yes(sf), top_supply='; '.join(colnames[k] for k in q['top_supply']) if sf else '', popular_stock_coverage=coverage))
with (root/'zscore_lga_planning_table.csv').open('w',newline='') as f:
    cw=csv.DictWriter(f,fieldnames=list(plan_csv[0]));cw.writeheader();cw.writerows(plan_csv)
plan_table = '<div class="table-wrap"><table class="planning"><thead><tr><th>LGA</th><th>Demand<br>challenges</th><th>Top 3 demand domains</th><th>Supply<br>challenges</th><th>Top 3 supply domains</th></tr></thead><tbody>'+''.join(plan_rows)+'</tbody></table></div>'
zcut=thresholds['zscore']
changed=sum(len({scores[mid,g]['category'] for mid in method_order})>1 for g in R)
simple = [('rank','Rank average','Rank each LGA separately within each component, use average ranks for ties, then average six ranks per side. Easy to explain; discards differences in magnitude.'),
          ('raw','Mean percentages','Average six original percentages per side. Retains percentage-point differences; components with wider variation can contribute more to differences between LGAs.'),
          ('robust','Median/IQR','For each component, subtract its LGA median and divide by its IQR, then average six values per side. Measures distance from typical conditions using a robust spread.'),
          ('median_count','Median count','Count demand components above their own medians and supply components below theirs. Flag a side at 4 or more of 6. This is a coarse majority rule; it does not use a final-score median.'),
          ('zscore','Z-score average · main approach','For each component, subtract the mean across the 17 LGAs and divide by its sample standard deviation; average six values per side. Retains differences in magnitude on a common variability scale.')]
method_text = '<div class="method-notes">'+''.join(f'<p><b>{title}.</b> {text}'+(f' Demand &gt; {thresholds[mid]["demand_cutoff"]:.6f}; supply readiness &lt; {thresholds[mid]["supply_cutoff"]:.6f}.' if mid!='median_count' else '')+'</p>' for mid,title,text in simple)+'</div>'

styles = base_css + '''
.model-figure{margin:18px 0}.intro{font-size:14px;color:#426171;max-width:1100px;margin:0 0 18px}.callout{padding:15px 18px;background:#eef7f8;border-left:4px solid #087f8b;font-size:13px;margin:16px 0}.mini-metrics{display:flex;flex-wrap:wrap;gap:10px 30px;margin:18px 0 0;color:#446575;font-size:12px}.mini-metrics b{display:block;color:#17374a;font-size:24px}.use-chart{width:100%;height:auto;display:block}.chart-label,.chart-number,.chart-tick,.chart-axis{fill:#344f5e;font-family:Arial,sans-serif}.chart-number{font-size:17px}.chart-tick{font-size:15px}.chart-axis{font-size:17px}.hm-wrap{border:0}.joint-heatmap{width:100%;min-width:1200px;table-layout:fixed;border-collapse:separate;border-spacing:4px;font-size:12px;line-height:1.35}.joint-heatmap th,.joint-heatmap td{padding:9px 4px;border:0;text-align:center}.joint-heatmap thead th{font-size:11px;color:#355463;vertical-align:bottom}.joint-heatmap th:first-child{width:170px;text-align:left;padding-left:0}.joint-heatmap tbody th{font-weight:650;font-size:12px}.joint-heatmap small{display:block;font-weight:450;font-size:10px;color:#6a818e;margin-top:3px}.joint-heatmap td{font-weight:750;font-variant-numeric:tabular-nums}.joint-heatmap .hm-gap{width:16px;padding:0;background:white}.joint-heatmap .demand-head{background:#eaf5f5;color:#086b78;font-size:14px}.joint-heatmap .supply-head{background:#fff0df;color:#9d4c16;font-size:14px}.hm-missing{background:#f0f3f5;color:#8ca0aa}.lagos-row td,.lagos-row th{border-bottom:6px solid white}.comparison{width:100%;min-width:850px;border-collapse:collapse;font-size:12px}.comparison th,.comparison td{padding:10px 12px;border-bottom:1px solid #e1eaee;text-align:left}.comparison thead{background:#edf3f6}.category-cell.both{background:#f1ebf6;color:#69488b}.category-cell.demand{background:#e8f4f5;color:#056f7e}.category-cell.supply{background:#fcf0e2;color:#a95917}.category-cell.neither,.category-cell.unlinked{color:#70838e}.comparison th:last-child{background:#dceef0}.comparison td:last-child{font-weight:750}.method-notes{font-size:12px;color:#506c7c;line-height:1.7;margin:14px 0 23px}.method-notes p{margin:8px 0}.new-planning{min-width:1200px}.new-planning tbody th,.new-planning tbody td,.new-planning thead th{width:auto!important;text-align:left!important;padding:11px!important}.new-planning tbody td:nth-child(6),.new-planning tbody td:nth-child(8){min-width:230px}.new-planning small{display:block;font-weight:450;font-size:10px;color:#6c8491}.new-planning .domain-list li span{max-width:190px}.definitions{font-size:12px;width:100%;border-collapse:collapse}.definitions td,.definitions th{padding:10px;border-bottom:1px solid #e2eaee;text-align:left;vertical-align:top}.definitions th{background:#eef4f7}.details-note{font-size:12px;color:#536f7e;line-height:1.75}.section-heading{margin-top:0}.section-heading+ h3{margin-top:20px}.heatmap-key{display:flex;gap:25px;flex-wrap:wrap;font-size:11px;color:#5e7987;margin:10px 0}.heatmap-key i{display:inline-block;width:70px;height:10px;margin-right:6px}.wide-break{height:22px}@media(max-width:900px){.joint-heatmap{min-width:1200px}.comparison{min-width:850px}.mini-metrics b{font-size:21px}}@media print{.use-chart{break-inside:avoid}.comparison,.joint-heatmap,.new-planning{min-width:0;font-size:7pt}.joint-heatmap th:first-child{width:95px}.joint-heatmap thead th,.joint-heatmap small{font-size:6pt}.new-planning tbody td:nth-child(6),.new-planning tbody td:nth-child(8){min-width:0}.method-notes{font-size:8pt}}
'''
header = '''<header><div><span class="eyebrow">SURGO × UNIVERSITY OF MANITOBA</span><h1>Lagos analysis</h1><p>Women's reported concerns and local family-planning services</p><div class="definition-update">SURGO FPN2 = 1 · Main Lagos sample · LGA planning summaries</div><nav aria-label="Contents"><a href="#coverage">01 · Data coverage</a><a href="#use">02 · Use and intention</a><a href="#joint">03 · Demand and supply</a><a href="#comparison">Scoring comparison</a><a href="#zscore-map">Classification map</a><a href="#priorities">Planning priorities</a><a href="#models">04 · Score models</a><a href="#individual-models">05 · Individual concerns</a><a href="#definitions">Definitions</a></nav></div></header>'''
coverage = f'''<section id="coverage"><div class="section-heading"><div><span class="eyebrow">01 · DATA COVERAGE</span><h2>Where the data overlap</h2></div><p>4,555 women · 1,741 facility records</p></div><div class="map-legend"><span><i style="background:#c3e0e3"></i>Women + facilities <b>17 LGAs</b></span><span><i style="background:#e6b485"></i>Facilities only <b>3 LGAs</b></span><span class="dot-key">● Dot area shows facility count</span></div>{mapbox(map_svg(coverage_svg,'coverage'),'coverage')}<p class="footnote">Coverage geometry, shading, labels and facility-count dots are retained from summary_lagos. Source coverage uses the full files; women's counts now refer to the 4,555-woman main sample. Dots represent LGA counts, not facility coordinates.</p></section>'''
use = f'''<section id="use"><div class="section-heading"><div><span class="eyebrow">02 · CONTRACEPTIVE USE</span><h2>Use, intention and method choice</h2></div><p>2,159 women classified in need by SURGO</p></div><p class="intro">From this section onward, the women's analysis includes only the supplied <b>FPN2 = 1</b> group. Current users and nonusers are retained; no additional fertility, sexual-recency or sterilization recoding is applied.</p>{chart_svg()}<p class="footnote">Every bar and sub-bar uses the same denominator: all 2,159 women with FPN2 = 1. Percentages use final SURGO weights; n is an unweighted respondent count. Modern, traditional and preference bars are overlapping subsets, not categories that sum to 100%. {interval_note} Among current users, {method_other_n} have another/unresolved method classification and {preference_other_n} have another/unresolved preference response. *Preference refers to method choice at initiation. The type of the alternative wanted method is not established. Named future modern switching is a separate measure.</p><div class="table-heading">{download('use_distribution_fpn2.csv','Download bar values and intervals')}{download('lga_use_rates_fpn2.csv','Download LGA use estimates')}</div></section>'''
joint = f'''<section id="joint"><div class="section-heading"><div><span class="eyebrow">03 · JOINT DEMAND AND SUPPLY CONDITIONS</span><h2>Reported concerns alongside recorded services</h2></div><p>LGA rows · six demand and six supply components</p></div><p class="intro">Read across each LGA: survey-weighted concerns among women with FPN2 = 1 on the left, and the facility environment on the right. Supply combines all included Rural and Urban facility records in each LGA; it is not averaged across women.</p>{heatmap()}<div class="heatmap-key"><span><i style="background:linear-gradient(90deg,#eff8f8,#057483)"></i>Demand: fewer → more reported concerns</span><span><i style="background:linear-gradient(90deg,#fff6e9,#bf570e)"></i>Supply: stronger → weaker recorded provision</span></div><p class="footnote">Hover over a cell for its denominator. Demand percentages use valid responses within the FPN2 = 1 group. All displayed supply measures run higher = stronger provision; darker orange therefore means lower values. The Linked Lagos row pools women and facilities across the same 17 linked LGAs. Three facility-only LGAs are displayed but do not enter scoring. † Fewer than 30 women in need. “Low method knowledge” retains the MC5 confidence definition. Most-popular-product stock is conditional on known TRUE/FALSE stock status, not all facility records. Free provision includes the single documented Ibeju-Lekki–Urban zero exception before LGA aggregation.</p><div class="table-heading">{download('heatmap_values_and_denominators.csv','Download percentages and denominators')}</div><div class="wide-break"></div><h3 id="comparison">How classification changes with the scoring approach</h3><p class="details-note">The same six demand and six supply inputs are used in every column. {changed} of 17 linked LGAs change category under at least one alternative; the map below uses the final column.</p>{compare_table}{method_text}<p class="footnote">For the four continuous approaches, each side is flagged relative to the median of its own final-score distribution: higher demand, lower supply readiness. Exact median ties are not flagged. Normalization and cutoffs give each of the 17 LGAs equal contribution; these are relative thresholds, not externally validated service standards. Each component receives one-sixth weight. Median count instead uses its explicit 4-of-6 rule.</p><div class="table-heading">{download('lga_classification_comparison.csv','Download five-method comparison')}{download('classification_thresholds.csv','Download exact thresholds')}</div><div class="wide-break"></div><h3 id="zscore-map">LGA planning profiles · z-score average</h3><div class="rule-strip"><span>Demand: higher = more concern · Supply: higher = stronger provision</span><span>Demand &gt; <b>{zcut['demand_cutoff']:.6f}</b> · Supply &lt; <b>{zcut['supply_cutoff']:.6f}</b></span></div>{legend()}{mapbox(map_svg(category_svg,'zscore'),'zscore')}<p class="footnote">Hover for weighted any-method use and modern use among women with FPN2 = 1, sample size and the two continuous scores. “Neither flagged” means neither relative cutoff is met; it does not establish adequate services or absence of barriers.</p><div class="table-heading"><h3 id="priorities">LGA planning priorities</h3>{download('zscore_lga_planning_table.csv','Download planning table')}</div>{plan_table}<p class="footnote">Top demand components have the largest component z-scores; top service weaknesses have the smallest favorable component z-scores. Raw percentages are displayed, and hovering shows the component z-score. Lists appear only for the side flagged by the map. These are relative component priorities, not regression effects or a causal allocation of nonuse. Both map and table use identical scores, cutoffs and classifications.</p></section>'''
defs_d = [('Healthcare permission barriers','HS7c: permission to seek general healthcare; not a direct FP decision-control item.'),('Anticipated social stigma','PB7b/c: very/quite afraid of being seen seeking FP or worried about others\' reactions.'),('Health / side-effect beliefs','PB2a-c: health problems, reduced sexual desire or fetal-health concerns; excludes bleeding and future-fertility items.'),('Future-fertility concerns','MP4a-f: concern about future childbearing after stopping a nonpermanent method; coherent response patterns only.'),('Low method knowledge','MC5a/b: low confidence choosing a suitable method or managing side effects/switching. This is subjective confidence, not an objective knowledge test.'),('Opposes family planning','FP1: mostly/completely against versus mostly/completely in favor; knowledge-uncertain/no-opinion responses unresolved.')]
defs_s = [('Short + long-acting offering','Mean of UoM one_larc_3_sa_offer; percentage of included facilities meeting the ≥1 long-acting plus ≥3 short-acting product/category rule.'),('Free-product provision','Mean of UoM p_free_prod among valid facility proportions, plus the one Ibeju-Lekki Urban analysis exception. A facility-average product proportion, not percentage of facilities offering something free.'),('Trained-provider share','Mean of UoM p_train: average facility proportion of recorded providers classified as trained under the original eligibility/topic rules; not the percentage of facilities with a trained provider.'),('FP counselor availability','Q702 Yes/(Yes+No), with unknowns excluded. Counselor availability, not observed counseling receipt or quality.'),('Three-LARC stock breadth','Mean of UoM three_larc_stockin: percentage meeting the original ≥3 long-acting product/category in-stock rule; not a measure of interruption duration.'),('Most-popular product in stock','Mean of UoM mp_in_stock over known TRUE/FALSE responses: facilities with most-popular product in stock divided by those with known stock status. Product differs across facilities and is not women\'s preferred method.')]
defs = '<section id="definitions"><div class="section-heading"><div><span class="eyebrow">METHODS AND DEFINITIONS</span><h2>What the measures mean</h2></div></div><details open><summary>Demand components · labels retained from the previous summary</summary><table class="definitions">'+''.join(f'<tr><th>{esc(t)}</th><td>{esc(v)}</td></tr>' for t,v in defs_d)+'</table></details><details open><summary>Supply components · all included facilities within LGA</summary><table class="definitions">'+''.join(f'<tr><th>{esc(t)}</th><td>{esc(v)}</td></tr>' for t,v in defs_s)+'</table></details><details><summary>Weights, uncertainty and reproducibility</summary><div class="details-note"><p>All SURGO point estimates use the original final weight field. The need subgroup is selected after constructing the positive-weight survey frame. Official enumeration-area/PSU and stratum identifiers remain unconfirmed in the available files, so the overview bars withhold full-design confidence intervals. The model sections use separately labeled geographic LGA-clustered inference, which does not establish the official survey design. Household-ID digits and selection probabilities are not treated as cluster identifiers. The analysis script accepts explicitly confirmed PSU/stratum fields when they become available and then calculates Taylor-linearized logit intervals; no finite-population correction is assumed.</p><p>Facility summaries are unweighted because facility sampling weights were not supplied. Each component uses its valid denominator. Most-popular-product stock coverage can be low; the denominator download should accompany interpretation. The only free-provision exception is the one previously authorized Urban facility in Ibeju-Lekki. Original input files and the previous summary HTML remain unchanged.</p><p>Z-score averaging is a transparent working choice, not a uniquely optimal or validated constraint index. It retains distance information but is sensitive to extreme/noisy component estimates, and equal averaging allows stronger components to offset weaker ones. Comparisons with ranks, original percentages, median/IQR and median counts make methodological sensitivity visible. Correlation, small LGA samples and facility response coverage remain relevant to future model specification.</p><p>Normalization and weighting references: <a href="https://knowledge-for-policy.ec.europa.eu/composite-indicators/toolkit_en/navigation-page/10-step-guide_en/step-5-normalisation_en">European Commission JRC: normalization</a>; <a href="https://knowledge-for-policy.ec.europa.eu/composite-indicators/toolkit_en/navigation-page/10-step-guide_en/step-6-weighting_en">JRC: weighting</a>. These support transparent construction and sensitivity checks, not causal interpretation of these particular scores.</p></div></details></section>'
model_sections = ''
if a.model_dir:
    from modern_model_sections import render_model_sections
    model_sections = render_model_sections(a.model_dir)
pca_section = ''
if a.pca_dir:
    if not a.model_dir:
        raise ValueError('Supply --model-dir for the primary-model comparison')
    from pca_appendix import render_pca_appendix
    pca_section = render_pca_appendix(a.pca_dir, a.model_dir)
    header = header.replace('</nav>', '<a href="#appendix-pca">Appendix · PCA</a></nav>')
js = '''
const DATA=__DATA__;const tip=document.querySelector('.tooltip');let active=null;
function hide(){tip.classList.remove('show');if(active)active.classList.remove('selected');active=null;}
function show(el,e){const d=DATA[el.dataset.key];if(!d)return;const mode=el.dataset.rule;const id=mode==='coverage'?'coverage':'zscore';if(active)active.classList.remove('selected');active=document.querySelector('#map-'+id+' .lga-region[data-key="'+el.dataset.key+'"]');if(active)active.classList.add('selected');const pc=v=>v==null?'Not estimated':v.toFixed(1)+'%';tip.innerHTML='<div class="tip-name">'+d.name+'</div><div class="tip-cat">'+(mode==='coverage'?(d.linked?'Women + facility data':'Facility data only'):d.category)+'</div><div class="tip-rates"><div><b>'+pc(d.cpr)+'</b><span>Any-method use</span></div><div><b>'+pc(d.mcpr)+'</b><span>Modern-method use</span></div></div><div class="tip-denom">'+(d.linked?'Weighted shares among SURGO FPN2 = 1 women · n = '+d.n:'No SURGO need-group estimate')+'</div>'+(mode!=='coverage'&&d.linked?'<div class="tip-score"><span>Demand z-score</span><b>'+d.demand.toFixed(3)+'</b></div><div class="tip-score"><span>Supply-readiness z-score</span><b>'+d.supply.toFixed(3)+'</b></div>':'')+'<div class="tip-note">'+d.facilities+' facility records · '+d.women+' women in the full main sample</div>'+(d.n&&d.n<30?'<div class="tip-alert">Small need-group sample: fewer than 30 women.</div>':'');tip.classList.add('show');move(e,el);}
function move(e,el){const b=el.getBoundingClientRect();let x=e&&e.clientX!=null?e.clientX:b.x+b.width/2,y=e&&e.clientY!=null?e.clientY:b.y+b.height/2;let l=x+16,t=y+16;if(l+tip.offsetWidth>innerWidth-10)l=x-tip.offsetWidth-16;if(t+tip.offsetHeight>innerHeight-10)t=y-tip.offsetHeight-16;tip.style.left=Math.max(10,l)+'px';tip.style.top=Math.max(10,t)+'px';}
document.querySelectorAll('[data-key][data-rule]').forEach(el=>{el.addEventListener('pointerenter',e=>show(el,e));el.addEventListener('pointermove',e=>move(e,el));el.addEventListener('pointerleave',hide);el.addEventListener('focus',e=>show(el,e));el.addEventListener('blur',hide);el.addEventListener('click',e=>show(el,e));});
document.querySelectorAll('.expand-map').forEach(b=>b.addEventListener('click',()=>{const box=b.closest('.mapbox');box.classList.toggle('enlarged');document.body.classList.toggle('modal-open',!!document.querySelector('.mapbox.enlarged'));b.textContent=box.classList.contains('enlarged')?'Close enlarged map':'⤢ Enlarge map';hide();}));document.addEventListener('keydown',e=>{if(e.key==='Escape'){document.querySelectorAll('.mapbox.enlarged').forEach(b=>{b.classList.remove('enlarged');b.querySelector('.expand-map').textContent='⤢ Enlarge map';});document.body.classList.remove('modal-open');hide();}});
'''.replace('__DATA__',json.dumps(DATA,separators=(',',':')))
page = '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Lagos_analysis | SURGO × Manitoba</title><style>'+styles+'</style></head><body id="top">'+header+'<main>'+coverage+use+joint+model_sections+defs+pca_section+'</main><div class="tooltip" role="status" aria-live="polite"></div><footer style="max-width:1430px;margin:auto;padding:0 25px 30px">Lagos main-sample analysis · SURGO FPN2 definition · Initial LGA planning results. All categories are relative, not causal diagnoses of binding constraints.</footer><script>'+js+'</script></body></html>'
output.parent.mkdir(parents=True,exist_ok=True)
output.write_text(page)
assert hashlib.sha256(ref.read_bytes()).hexdigest()==reference_hash
def visual_signature(svg):
    e=ET.fromstring(svg)
    return [(n.tag,tuple(sorted((k,v) for k,v in n.attrib.items() if k in ('d','fill','cx','cy','r','x','y','points'))),''.join(n.itertext()) if n.tag.endswith(('text','tspan')) else '') for n in e.iter() if n.tag.endswith(('path','circle','polyline','rect','text','tspan'))]
new_cov=re.search(r'<svg[^>]*id="map-coverage".*?</svg>',page,re.S).group()
assert visual_signature(coverage_svg)==visual_signature(new_cov)
for g,q in planning.items():
    assert q['category']==scores['zscore',g]['category']
    for side,field,reverse in [('Demand','top_demand',True),('Supply','top_supply',False)]:
        variables=D if side=='Demand' else S
        ordered=sorted(variables,key=lambda k:((-1 if reverse else 1)*trace[g,side,k]['zscore'],variables.index(k)))[:3]
        assert ordered==q[field]
assert '2,084' not in page and '4,846' not in page
check=dict(reference_html_unchanged=True,coverage_map_visual_signature_identical=True,
           n_need=2159,main_method='zscore',comparison_methods=method_order,
           map_planning_categories_identical=True,top_three_use_same_component_zscores=True,
           all_bar_denominators_2159=True,old_need_counts_absent=True,
           planning_display_columns=5,
           model_sections_included=bool(a.model_dir),
           pca_appendix_included=bool(a.pca_dir),
           no_random_effect_models=True,
           verified_design_fields=p['audit']['design_fields_confirmed'],
           unverified_cluster_inference_absent=True,
           output_file=output.name,output_sha256=hashlib.sha256(output.read_bytes()).hexdigest())
(root/'render_verification.json').write_text(json.dumps(check,indent=2))
print('Rendered',output.name,'with 20 LGA rows, five scoring methods, and z-score map/priorities.')
