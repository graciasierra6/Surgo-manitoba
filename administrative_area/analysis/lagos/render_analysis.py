"""Render the standalone Lagos HTML from aggregate outputs; no respondent data."""
from pathlib import Path
import argparse, base64, csv, hashlib, html, json, math, re
from collections import Counter
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
            if mode in ('zscore','sensitivity'):
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


planning_labels = dict(
    agency='Needs permission to seek healthcare',
    stigma='Expects social disapproval of family planning',
    health='Concerns about health and side effects',
    fertility='Concerns about future fertility',
    confidence='Low confidence choosing a method or managing side effects',
    opposes='Personally opposes family planning',
    offering='Facilities offering short- and long-acting options together',
    free='Products provided free (facility average)',
    training='Trained-provider share (facility average)',
    counselor='Facilities with an FP counselor',
    larc_stock='Facilities meeting the three-LARC stock criterion',
    popular_stock='Facilities with their most-popular product in stock',
)
# All displayed summaries remain aggregate; respondent rows and fitted objects
# are never embedded in the report.
load = lambda file: json.loads((root/file).read_text())
m = load('models_zscore/model_results.json')
sp = load('complete_facility_sensitivity/report_data.json')
sm = load('complete_facility_sensitivity/models_zscore/model_results.json')
dp = load('complete_demand_sensitivity/report_data.json')
pc = load('pca_appendix/pca_results.json')
pm = load('pca_appendix/models/model_results.json')
verification = load('checks/verification.json')
linked_areas = sorted(R, key=lambda g:names[g])
labels = dict(colnames)
labels.update(planning_labels)
number = lambda v: '—' if v is None else f'{v:+.3f}'
fp = lambda v: '&lt;0.001' if v < .001 else f'{v:.3f}'

def table(headers, rows):
    return '<div class="table-wrap"><table class="planning"><thead><tr>'+''.join('<th>'+esc(h)+'</th>' for h in headers)+'</tr></thead><tbody>'+''.join('<tr><th scope="row">'+r[0]+'</th>'+''.join('<td>'+v+'</td>' for v in r[1:])+'</tr>' for r in rows)+'</tbody></table></div>'

def fig(folder, file, alt):
    data=base64.b64encode((root/folder/file).read_bytes()).decode()
    return f'<figure class="model-figure"><img src="data:image/svg+xml;base64,{data}" alt="{esc(alt)}"></figure>'

def area(g):
    return esc(names[g])+(f'<small>† Small sample · n={R[g]["n"]}</small>' if R[g]['n']<50 else '')

def tag(k):
    return f'<span style="color:{cols.get(k,"#667d8b")};font-weight:700">{cats[k]}</span>'

def score_dict(payload):
    return {r['geo']:r for r in payload['scores'] if r['method']=='zscore'}

main_scores,case_scores,demand_scores=map(score_dict,[p,sp,dp])
case_heat={(r['geo'],r['side'],r['variable']):r for r in sp['heatmap']}
demand_heat={(r['geo'],r['side'],r['variable']):r for r in dp['heatmap']}
case_changes=[g for g in linked_areas if case_scores[g]['category']!=main_scores[g]['category']]
demand_changes=[g for g in linked_areas if demand_scores[g]['category']!=main_scores[g]['category']]
style_module=Path(__file__).resolve().parents[3]/'analysis/common/report.css'
styles=base_css+style_module.read_text()+'''
section{padding:28px;border:1px solid #dfe8ed;border-radius:10px;margin:24px 0;background:white}.intro,.details-note{line-height:1.7}.table-wrap{overflow:auto}.planning{width:100%;border-collapse:collapse}.planning th,.planning td{padding:12px 10px;font-size:13px;line-height:1.5;text-align:left;vertical-align:top;border-bottom:1px solid #e1e9ed}.planning thead{background:#eef5f7}.planning small{display:block;font-size:10px;color:#97622f;margin-top:4px}.priority-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:22px}.priority-grid article{min-width:0}h3{margin-top:28px}h4{padding:14px;background:#eef7f7}.supply-summary h4{background:#fff5e8;color:#9b551f}.demand-summary h4{color:#087d89}.model-figure img{width:100%;height:auto}.callout{padding:15px 18px;border-left:4px solid #16818c;background:#eef8f8;line-height:1.7;font-size:14px}.warning{border-left-color:#bd7528;background:#fff7eb}.footnote{font-size:12px;line-height:1.7;color:#617c89}.use-chart{width:100%}.chart-label,.chart-number,.chart-tick,.chart-axis{font-family:Arial,sans-serif;fill:#294958}.chart-number{font-size:17px}.chart-tick{font-size:15px}.chart-axis{font-size:17px}.joint-heatmap{width:100%;min-width:1250px;border-collapse:separate;border-spacing:4px;table-layout:fixed}.joint-heatmap td,.joint-heatmap th{padding:12px 5px;text-align:center;font-size:12px}.joint-heatmap th:first-child{width:175px;text-align:left}.joint-heatmap small{display:block;font-size:10px}.demand-head{background:#eef7f7}.supply-head{background:#fff5e8}.hm-gap{width:12px}.hm-missing{background:#edf0f3}.model-meta,.model-note{line-height:1.7;font-size:14px}.model-figure{margin:22px 0}.table-heading{display:flex;gap:18px;flex-wrap:wrap;margin:14px 0}.download{font-size:12px}.method-notes{line-height:1.7;font-size:13px}.country-note{font-size:13px;color:#546f7d}@media(max-width:950px){.priority-grid{grid-template-columns:1fr}}@media print{.model-figure,.mapbox{break-inside:avoid}}
'''
header='''<header><div><span class="eyebrow">SURGO × UNIVERSITY OF MANITOBA</span><h1>Lagos · LGA-wide supply analysis</h1><p>Women’s reported concerns alongside recorded family-planning provision</p><p class="country-note">Each woman receives the facility averages for her LGA. Rural/Urban residence remains an adjustment variable.</p><nav aria-label="Contents"><a href="#coverage">1 · Coverage</a><a href="#use">2 · Use and intention</a><a href="#joint">3 · Conditions and priorities</a><a href="#models">4 · Models</a><a href="#definitions">Definitions and missingness</a><a href="#appendix-scoring">Appendix · Scoring</a><a href="#appendix-pca">Appendix · PCA</a></nav></div></header>'''
coverage=f'''<section id="coverage"><span class="eyebrow">01 · COVERAGE AND LINKAGE</span><h2>Women and facilities overlap in 17 LGAs</h2><p>Retain all <b>4,555 main-sample women</b>. Manitoba has <b>1,741 facility records in 20 LGAs</b>; 1,447 are in the 17 LGAs represented by SURGO. Three LGAs have facility data only.</p><div class="map-legend"><span><i style="background:#c3e0e3"></i>Women + facilities · 17 LGAs</span><span><i style="background:#e6b485"></i>Facilities only · 3 LGAs</span><span>● Dot area reflects facility count</span></div>{mapbox(map_svg(coverage_svg,'coverage'),'coverage')}<p class="footnote">Coverage geometry, shading, labels and facility-count dots are retained from the existing Lagos report. Dots show LGA totals, not facility coordinates. The coverage map precedes selection of the need group.</p><h3>How women receive supply measures</h3>{table(['Woman','Facility characteristics assigned'],[['LGA A · Rural','Valid-response facility averages across all of LGA A.'],['LGA A · Urban','The same LGA A averages.'],['LGA B · Rural or Urban','LGA B’s own averages.']])}<p>The checked source LGA mapping is retained. The documented Ifako-Ijaiye/IFAKO-IJAYE spelling alias is reconciled explicitly. This describes the recorded local service environment; it does not identify the facility attended.</p></section>'''
use=f'''<section id="use"><span class="eyebrow">02 · USE, INTENTION AND METHOD CHOICE</span><h2>Women SURGO classifies as in need of family planning</h2><p class="callout">Section 2 onward uses original <b>fpn2 = 1</b>: <b>2,159 women</b>. No reconstructed need definition or additional fertility, sexual-activity or sterilization recoding.</p>{chart_svg()}<p class="footnote">Every bar uses all 2,159 women as its denominator. Percentages use final <b>weight</b>; n is unweighted. Subsets overlap. {interval_note} Preference is the method wanted at initiation, not necessarily current preference. Future intention is for any method without a specified 12-month initiation window.</p>{download('use_distribution_fpn2.csv','Use estimates')}</section>'''

def frequency(category,keys):
    group=[r for r in p['planning'] if r['category']==category]
    count=Counter(k for r in group for k in r['top_'+category])
    ordered=sorted(keys,key=lambda k:(-count[k],keys.index(k)))
    return len(group),table(['Component','LGAs where this is among the top three','Share of this group'],[[esc(labels[k]),f'{count[k]} of {len(group)}',f'{100*count[k]/len(group):.1f}%'] for k in ordered])

def priority_detail(category):
    rows=[]
    for g in linked_areas:
        r=planning[g]
        if r['category']!=category:continue
        row=[area(g)]
        for k in r['top_'+category]:
            q=HM[g,category.title(),k]
            row.append(f'<span title="Valid responses {q["n_valid"]}/{q["n_total"]}">{esc(labels[k])}<br><b>{q["value"]:.1f}%</b></span>')
        rows.append(row)
    return table(['LGA','Priority 1','Priority 2','Priority 3'],rows)

sn,st=frequency('supply',S);dn,dt=frequency('demand',D)
priorities=f'''<div id="priority-summaries"><h3>Which priorities recur across LGAs?</h3><p>Count appearances among each LGA’s three lowest supply component z-scores or highest demand component z-scores. Percentages describe LGAs, not women or facilities. Both-flagged LGAs are excluded from these group summaries.</p><div class="priority-grid"><article id="supply-priority-summary" class="supply-summary"><h4>Supply priorities · {sn} supply-only LGAs</h4>{st}</article><article id="demand-priority-summary" class="demand-summary"><h4>Demand priorities · {dn} demand-only LGAs</h4>{dt}</article></div><p class="footnote">Each LGA contributes three priorities; percentages do not sum to 100%. These are relative component priorities, not causal reasons for nonuse.</p></div><div id="supply-department-priorities"><h3>The {sn} supply-only LGAs · three service priorities</h3>{priority_detail('supply')}</div><div id="demand-department-priorities"><h3>The {dn} demand-only LGAs · three concern priorities</h3>{priority_detail('demand')}<p class="footnote">Displayed values are original percentages, ordered by component z-scores. Demand rates use weight and valid-domain answers among women with fpn2 = 1. Supply rates retain facility-valid denominators; free provision and training are averages of within-facility proportions. † Fewer than 50 women in need. Hover for valid/total response coverage.</p></div>'''
fullrows=[]
for g in linked_areas:
    r=planning[g]
    entries=[]
    for side,key in [('Demand','demand'),('Supply','supply')]:
        entries.extend(['Yes' if r[key+'_flag'] else 'No','<br>'.join(esc(labels[k])+f' <b>{HM[g,side,k]["value"]:.1f}%</b>' for k in r['top_'+key]) if r[key+'_flag'] else '—'])
    fullrows.append([area(g)]+entries)
zcut=thresholds['zscore']
joint=f'''<section id="joint"><span class="eyebrow">03 · JOINT CONDITIONS AND PLANNING PRIORITIES</span><h2>Reported concerns alongside recorded services</h2><p>Six weighted concern percentages among women in need appear beside six unweighted facility measures. Facility measures combine Rural and Urban records within each LGA.</p>{heatmap()}<p class="footnote">Darker teal means more concern; darker orange means weaker provision. Higher raw supply values mean stronger provision. Unknowns are not zero. The single authorized Ibeju-Lekki Urban free-product zero is retained before aggregation; no other missing measure is recoded. The UoM three-LARC criterion refers to product/category breadth, rather than three distinct clinical LARC methods.</p><h3>LGA planning profiles · z-score average</h3><p>Standardize each component using its mean and sample SD across the 17 linked LGAs, then average six components separately by side. Flag demand above its final-score median <b>{zcut['demand_cutoff']:.6f}</b> and favorable supply below its median <b>{zcut['supply_cutoff']:.6f}</b>. Exact ties are unflagged. Equal LGA references differ from the individual demand score used in models.</p>{legend()}{mapbox(map_svg(category_svg,'zscore'),'zscore')}<p class="footnote">Hover for weighted CPR and mCPR among women in need. Neither flagged does not establish adequate services or absence of barriers.</p>{priorities}<h3>LGA planning priorities · all 17</h3>{table(['LGA','Demand challenges','Top three concerns','Supply challenges','Top three service weaknesses'],fullrows)}</section>'''

def model_section(payload,folder,pca=False):
    a=payload['audit'];co={(r['model'],r['term']):r for r in payload['coefficients']}
    tests=[r for r in payload['tests'] if r['model'] in ('score_joint','components_joint')]
    ort=lambda r:f'{r["odds_ratio"]:.2f} ({r["or_lo"]:.2f}–{r["or_hi"]:.2f})'
    title='PCA sensitivity models' if pca else 'Modern use in relation to personal demand and LGA services'
    text=f'<h2>{title}</h2><p class="model-meta">Same <b>{a["common_n"]:,} women</b>, including {a["common_events"]:,} modern users, across 17 LGAs. Outcome: current modern use = 1; confirmed traditional-only use or nonuse = 0. Adjust for age + age², education, partnership, fertility goal and Rural/Urban residence. Original weight; LGA-clustered CR2/Satterthwaite intervals. No random effects or interactions.</p>'
    text+=table(['Model','Predictors alongside the same covariates'],[['1','Personal demand score'],['2','Assigned LGA supply risk score'],['3','Both scores'],['4','Six personal demand indicators separately'],['5','Six personal indicators + supply risk score']])
    if not pca:text+='<p class="model-note">Each woman’s demand score averages her six responses standardized with final-weighted means and population SDs among valid women in need. Supply risk is the negative average of her six LGA-wide facility measures standardized against the 17-LGA planning references. Each woman retains personal demand variation, while women in one LGA share a supply score. All six responses are required; scores enter continuously in their original units.</p>'
    d,su=co['score_joint','demand_score'],co['score_joint','supply_risk']
    text+=fig(folder,'01_score_models.svg','Adjusted demand, supply and joint score odds ratios with LGA CR2 confidence intervals.')
    text+=f'<p class="callout"><strong>Joint model.</strong> Demand OR <b>{ort(d)}</b>; supply OR <b>{ort(su)}</b> per one score point. Interpret the intervals alongside the estimates. These associations do not allocate a percentage of nonuse to each side or establish a causal binding constraint.</p>'
    text+=fig(folder,'02_score_probabilities.svg','Weighted adjusted modern-use probabilities across observed demand and LGA supply scores.')
    text+='<p class="model-note">Curves average predictions over the same women and observed covariates. The other score stays observed. These are adjusted cross-sectional comparisons, not expected effects of an intervention.</p>'
    text+=fig(folder,'03_component_models.svg','Six individual concerns alongside the LGA supply risk score.')
    text+=fig(folder,'04_component_probability_gaps.svg','Adjusted modern-use percentage-point differences for each concern, with confidence intervals.')
    text+='<p class="model-note">For each concern, predict every woman with that response present and absent, retaining other variables. Average using weight and subtract; intervals account for covariance. The MC5 indicator measures confidence, despite the retained knowledge label in older figures.</p>'
    text+=table(['Joint model','Block','Restrictions','Effective denominator df','Raw p','Holm p'],[[r['model'],r['block'],str(r['df_num']),f'{r["df_denom"]:.2f}',fp(r['p']),fp(r['holm_p'])] for r in tests])
    text+='<p class="footnote">Two block tests per joint model; Holm correction is within that pair. With only 17 contextual supply values, woman count does not determine supply precision. These are geographic clustered intervals, not a reconstruction of unconfirmed official EA/strata. Geographic Taylor comparison intervals are unavailable when design residual degrees of freedom are nonpositive.</p>'
    return text

models='<section id="models"><span class="eyebrow">04 · ADJUSTED ASSOCIATIONS</span>'+model_section(m,'models_zscore')+'</section>'
missing_d=list(csv.DictReader((root/'demand_missingness.csv').open()))
missing_s=list(csv.DictReader((root/'supply_missingness.csv').open()))
missing_tables=table(['Demand concern','Unresolved / 2,159','Unweighted share','Weighted share'],[[esc(labels[r['variable']]),r['missing'],f'{float(r["missing_pct"]):.1f}%',f'{float(r["weighted_missing_pct"]):.1f}%'] for r in missing_d])+table(['Facility measure','Valid / 1,447','Undefined','Undefined share'],[[esc(labels[r['variable']]),f'{r["valid"]} / {r["total"]}',r['missing'],f'{float(r["missing_pct"]):.1f}%'] for r in missing_s])
validrows=[[area(g),str(meta[g]['facility_n'])]+[str(HM[g,'Supply',k]['n_valid']) for k in S] for g in linked_areas]
definitions=f'''<section id="definitions"><h2>Definitions, response coverage and model sample</h2><h3>Missing or unresolved demand and supply measures</h3>{missing_tables}<p class="footnote">Demand percentages summarize unanswered, don’t-know, inconsistent or otherwise unclassifiable responses after the agreed coding. A known positive can establish a composite concern, but zero requires all constituent negatives. Supply unknowns include undefined ratios, applicability and missing stock flags; they do not establish absent services or stockouts. Zero missingness for cleaned offering/stock-breadth flags does not certify complete answers to every underlying item.</p><p><b>1,891 of 2,159 women</b> have all six demand indicators; <b>614 of 1,447 linked facilities</b> have all six supply indicators. The main analysis keeps valid responses separately by measure. Every LGA has a full aggregate supply profile. Models use 1,865 women after also requiring known outcome and covariates: 294 exclusions (13.6%).</p><details><summary>Valid facility responses by LGA</summary>{table(['LGA','Total']+[labels[k] for k in S],validrows)}</details><h3>What each supply measure represents</h3>{table(['Measure','Interpretation'],[[labels['offering'],'Share meeting the original ≥1 long-acting plus ≥3 short-acting category offering rule.'],[labels['free'],'Mean facility fraction of recorded products provided free; not share of facilities offering anything free.'],[labels['training'],'Mean facility fraction of recorded personnel classified as trained under original UoM eligibility/topic rules.'],[labels['counselor'],'Q702 Yes divided by Yes + No; unknown/special answers excluded. Availability is not receipt or quality.'],[labels['larc_stock'],'Share meeting the original UoM three_larc_stockin product/category criterion; not three distinct clinical LARC methods.'],[labels['popular_stock'],'Stock present divided by known TRUE/FALSE stock flags, for each facility’s own most-popular product.']])}<h3>Demand coding</h3>{table(['Domain','Source questions'],[[labels['agency'],'HS7C: permission to seek general healthcare, not FP-specific partner approval.'],[labels['stigma'],'PB7B/C: fear of being seen seeking FP or others’ reactions.'],[labels['health'],'PB2A–C: health problems, reduced sexual desire and birth-defect concerns. Excludes bleeding and future fertility.'],[labels['fertility'],'MP4_a–f reversible-method future-fertility beliefs; require coherent selections and an explicit answer.'],[labels['confidence'],'MC5A/B: confidence choosing a suitable method or managing side effects/switching; subjective confidence, not an objective knowledge test.'],[labels['opposes'],'FP1: mostly/completely against versus mostly/completely in favor. Special responses remain unresolved.']])}<p>All six domains use the existing Lagos definitions and original fpn2. Scores summarize selected concerns and provision; equal weighting permits offsetting strengths and weaknesses. Official survey EA/strata are unconfirmed, so overview confidence intervals are withheld rather than inferred from household identifiers. Woman weights remain applied throughout; facilities are unweighted.</p><p class="footnote">Independent verification passed {verification['check_count']} checks. These checks validate reproduction, not nonresponse assumptions, adequate power or causality.</p>{download('demand_missingness.csv','Demand missingness')}{download('supply_missingness.csv','Supply missingness')}</section>'''
method_order=['rank','raw','robust','median_count','zscore'];method_names=['Rank average','Mean percentages','Median/IQR','Median count','Z-score average']
scoring='<section id="appendix-scoring"><h2>Appendix A · Classification under five scoring approaches</h2>'+table(['LGA']+method_names,[[area(g)]+[tag(scores[mid,g]['category']) for mid in method_order] for g in linked_areas])+table(['Approach','Construction','Threshold'],[['Rank average','Average six component ranks on each side.','Separate final-score medians.'],['Mean percentages','Average six original percentages.','Separate final-score medians.'],['Median/IQR','Subtract each component median, divide by its IQR, then average.','Separate final-score medians.'],['Median count','Count unfavorable comparisons against component medians.','Four or more of six.'],['Z-score average · main','Subtract component means, divide by sample SDs and average six.','Demand above its final-score median; favorable supply below its median.']])+'</section>'
fc={r['geo']:r for r in csv.DictReader((root/'facility_complete_case_coverage.csv').open())}
case_component_rows=[[area(g)]+[f'{HM[g,"Supply",k]["value"]:.1f}% → <b>{case_heat[g,"Supply",k]["value"]:.1f}%</b>' for k in S] for g in linked_areas]
case_plan={r['geo']:r for r in sp['planning']}
case_rows=[[area(g),f'{fc[g]["complete_facilities"]}/{fc[g]["total_facilities"]}',tag(main_scores[g]['category']),tag(case_scores[g]['category']),number(main_scores[g]['supply_score'])+' → '+number(case_scores[g]['supply_score']),'<br>'.join(esc(labels[k]) for k in case_plan[g]['top_supply'])] for g in linked_areas]
original_scores=scores.copy()
for g in linked_areas:scores['zscore',g]=case_scores[g]
case_map=map_svg(category_svg,'sensitivity')
scores=original_scores
facilities=f'''<section id="appendix-facilities"><h2>Appendix B · Complete-facility sensitivity</h2><p>Restrict all six supply indicators, including counseling, to the same <b>614 complete facilities</b>. Main results use all 1,447 linked facilities with separate valid-response denominators. Hold primary component means, SDs and classification cutoffs fixed; retain the same women, demand inputs and covariates. Every linked LGA retains at least one complete facility.</p><p class="callout"><b>{len(case_changes)} of 17 classifications change; {17-len(case_changes)} remain unchanged.</b> Changed LGAs: {', '.join(names[g] for g in case_changes)}. This check evaluates selection of complete facilities, not whether missing facilities lack services. A score can move substantially while a binary category remains stable.</p><h3>Component values · primary → complete facilities</h3>{table(['LGA']+[labels[k] for k in S],case_component_rows)}{mapbox(case_map,'sensitivity')}<p class="footnote">Hover for the complete-facility category and score. Use estimates still describe the same women in need.</p>{table(['LGA','Complete / total','Main category','Complete category','Supply readiness: main → complete','Three weakest complete-facility components'],case_rows)}<details><summary>Same adjusted models on the same women</summary>{model_section(sm,'complete_facility_sensitivity/models_zscore')}</details></section>'''
dc={r['geo']:r for r in csv.DictReader((root/'demand_complete_case_coverage.csv').open())}
demand_rows=[[area(g),f'{dc[g]["complete_demand"]}/{dc[g]["women_in_need"]}',number(main_scores[g]['demand_score']),number(demand_scores[g]['demand_score']),tag(main_scores[g]['category']),tag(demand_scores[g]['category'])] for g in linked_areas]
demand_components=[[area(g)]+[f'{HM[g,"Demand",k]["value"]:.1f}% → <b>{demand_heat[g,"Demand",k]["value"]:.1f}%</b>' for k in D] for g in linked_areas]
demand=f'''<section id="appendix-demand"><h2>Appendix C · Common-woman demand sensitivity</h2><p>Use the same <b>1,891 women complete on all six demand indicators</b> for every LGA concern percentage, retaining weight. Main percentages use valid responses separately by domain. Hold primary normalization, thresholds and all supply inputs fixed.</p><p class="callout"><b>{len(demand_changes)} of 17 classifications change; {17-len(demand_changes)} remain unchanged.</b> Changed LGAs: {', '.join(names[g] for g in demand_changes) or 'none'}. Supply flags remain fixed. The individual models already require complete demand data; a second complete-demand model would repeat that restriction.</p>{table(['LGA']+[labels[k] for k in D],demand_components)}{table(['LGA','Complete demand / need','Main demand score','Complete-demand score','Main category','Complete-demand category'],demand_rows)}<p>Existing weights do not automatically correct selection related to unknown or unanswered concerns.</p></section>'''
negative=pc['audit']['negative_supply_PC1_loadings']
if isinstance(negative,str):negative=[negative]
pca=f'''<section id="appendix-pca"><h2>Appendix B · PCA scores and model sensitivity</h2><p>Separate weighted PCAs use each woman’s six personal demand responses and her six LGA-wide supply values. The supply construction is equivalent to 17 profiles weighted by the represented women’s summed weight; these are not facility weights.</p>{fig('pca_appendix','01_pca_loadings.svg','PCA coefficients for demand and LGA supply.')}{fig('pca_appendix','02_pca_variance.svg','Variance captured and exploratory permutation references.')}<p class="callout">Demand PC1 retains <b>{pc['audit']['demand_PC1_variance']:.1f}%</b> of input variation; supply PC1 <b>{pc['audit']['supply_PC1_variance']:.1f}%</b>. Supply PC1 has mixed signs ({', '.join(labels[k] for k in negative)} loads oppositely), so it describes a pattern rather than uniformly worse provision. The equal-weight z-score index remains primary.</p>{table(['Side','Component','PC1 coefficient'],[[r['side'],esc(labels[r['variable']]),f'{r["loading"]:+.3f}'] for r in pc['loadings'] if r['PC']==1])}{model_section(pm,'pca_appendix/models',True)}<p class="footnote">Permutation and LGA-deletion checks examine construction stability. They are not full survey-design significance tests or measures of causal importance. Regression intervals condition on the estimated scores.</p></section>'''
for g in linked_areas:
    DATA[g]['alternative_category']=cats[case_scores[g]['category']]
    DATA[g]['alternative_supply']=case_scores[g]['supply_score']
js='''const DATA=__DATA__;const tip=document.querySelector('.tooltip');
function hide(){tip.classList.remove('show')}
function move(e,el){const b=el.getBoundingClientRect();let x=e&&e.clientX!=null?e.clientX:b.x+b.width/2,y=e&&e.clientY!=null?e.clientY:b.y+b.height/2;let l=x+16,t=y+16;if(l+tip.offsetWidth>innerWidth-10)l=x-tip.offsetWidth-16;if(t+tip.offsetHeight>innerHeight-10)t=y-tip.offsetHeight-16;tip.style.left=Math.max(10,l)+'px';tip.style.top=Math.max(10,t)+'px'}
function show(el,e){const d=DATA[el.dataset.key];if(!d)return;const mode=el.dataset.rule;const pc=v=>v==null?'Not estimated':v.toFixed(1)+'%';const category=mode==='coverage'?(d.linked?'Women + facility data':'Facility data only'):mode==='sensitivity'&&d.linked?d.alternative_category:d.category;tip.innerHTML='<div class="tip-name">'+d.name+'</div><div class="tip-cat">'+category+'</div><div class="tip-rates"><div><b>'+pc(d.cpr)+'</b><span>Any-method use</span></div><div><b>'+pc(d.mcpr)+'</b><span>Modern use</span></div></div><div class="tip-denom">'+(d.linked?'Weighted among women with fpn2=1 · n='+d.n:'No SURGO need-group estimate')+'</div>'+(mode!=='coverage'&&d.linked?'<div class="tip-score">Demand: '+d.demand.toFixed(3)+'<br>Supply readiness: '+(mode==='sensitivity'?d.alternative_supply:d.supply).toFixed(3)+'</div>':'')+'<div class="tip-note">'+d.facilities+' source facilities · '+d.women+' source women</div>'+(d.n&&d.n<50?'<div class="tip-alert">Small need-group sample: n='+d.n+'</div>':'');tip.classList.add('show');move(e,el)}
document.querySelectorAll('[data-key][data-rule]').forEach(el=>{el.addEventListener('pointerenter',e=>show(el,e));el.addEventListener('pointermove',e=>move(e,el));el.addEventListener('pointerleave',hide);el.addEventListener('focus',e=>show(el,e));el.addEventListener('blur',hide);el.addEventListener('click',e=>show(el,e))});document.querySelectorAll('.expand-map').forEach(b=>b.addEventListener('click',()=>{const box=b.closest('.mapbox');box.classList.toggle('enlarged');document.body.classList.toggle('modal-open',!!document.querySelector('.mapbox.enlarged'));b.textContent=box.classList.contains('enlarged')?'Close enlarged map':'⤢ Enlarge map';hide()}));document.addEventListener('keydown',e=>{if(e.key==='Escape'){hide();document.querySelectorAll('.mapbox.enlarged').forEach(b=>b.classList.remove('enlarged'));document.body.classList.remove('modal-open')}});'''.replace('__DATA__',json.dumps(DATA,separators=(',',':')))
page='<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Lagos LGA analysis | SURGO × Manitoba</title><style>'+styles+'</style></head><body>'+header+'<main>'+coverage+use+joint+models+definitions+scoring+pca+'</main><div class="tooltip" role="status" aria-live="polite"></div><footer>Lagos · LGA-wide supply assignment · original SURGO need classification · cross-sectional associations.</footer><script>'+js+'</script></body></html>'
output.parent.mkdir(parents=True,exist_ok=True);output.write_text(page)
assert hashlib.sha256(ref.read_bytes()).hexdigest()==reference_hash
print('Rendered',output.name,'with LGA-wide supply, priority tables and appendix checks.')
