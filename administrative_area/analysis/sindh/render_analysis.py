"""Render the standalone Sindh HTML from aggregate outputs; no respondent data."""
from pathlib import Path
import argparse, base64, csv, hashlib, html, json, math, re
from collections import Counter
import xml.etree.ElementTree as ET

ap = argparse.ArgumentParser()
ap.add_argument('--data-dir', required=True)

ap.add_argument('--output', required=True)
ap.add_argument('--model-dir')
a = ap.parse_args()
root, output = Path(a.data_dir), Path(a.output)
ref=Path(__file__).resolve().parents[3]/'analysis/assets/lagos_map_template.html'
reference_hash = hashlib.sha256(ref.read_bytes()).hexdigest()
source = ref.read_text()
p = json.loads((root / 'report_data.json').read_text())
esc = lambda v: html.escape(str(v), quote=True)
pct = lambda v: 'Not estimated' if v is None else f'{v:.1f}%'
canon = lambda v: re.sub('[^A-Z0-9]', '', v.upper())
NS = 'http://www.w3.org/2000/svg'
ET.register_namespace('', NS)
coverage_svg=category_svg=None
base_css = next(t for t in re.findall(r'<style[^>]*>(.*?)</style>', source, re.S) if '--ink:' in t)
layout=json.loads((root/'map_layout.json').read_text())['features']
def titlecase(value):return value.title().replace('District ','').replace(' District','')
names={r['geo']:titlecase(r['district']) for r in p['profiles']}
assert len(names)==30 and {r['geo'] for r in layout}==set(names)
D, S = p['demand_variables'], p['supply_variables']
DL, SL = dict(zip(D, p['demand_labels'])), dict(zip(S, p['supply_labels']))
colnames = dict(DL, **SL)
cols = dict(both='#78549b', demand='#047f8b', supply='#cd7729', neither='#bacad2', unlinked='#edf0f3')
cats = dict(both='Both', demand='Demand only', supply='Supply only', neither='Neither flagged', unlinked='Not assessed')
R = {r['geo']: r for r in p['rates']}
B = {r['field']: r for r in p['bars']}
interval_note='Whiskers are 95% Taylor-linearized intervals using final Normalizedweights and recorded clusterCode, with no documented strata or finite-population correction. The full positive-weight consenting design is created before need-group subsetting.'
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
assert set(R).issubset(names) and len(R) == 30 and sum(r['n'] for r in R.values()) == 3274
DATA = {}
for g in areas:
    linked = g in R
    q = scores.get(('zscore', g))
    DATA[g] = dict(name=names[g], linked=linked, women=C[g]['women'], facilities=C[g]['facilities'],
                   n=R[g]['n'] if linked else None, cpr=100*R[g]['cpr']['estimate'] if linked else None,
                   mcpr=100*R[g]['mcpr']['estimate'] if linked else None,
                   category=cats[q['category']] if linked else cats['unlinked'],
                   demand=q['demand_score'] if linked else None, supply=q['supply_score'] if linked else None)

def map_svg(text,mode):
    pieces=[f'<svg xmlns="http://www.w3.org/2000/svg" class="sindh-map" viewBox="0 0 1100 690" role="img" aria-label="Sindh district {mode} map"><rect width="1100" height="690" fill="#f8fbfc"/>']
    def draw(features,box,inset=False):
        points=[xy for f in features for ring in f['rings'] for xy in ring]
        xmin=min(v[0] for v in points);xmax=max(v[0] for v in points);ymin=min(v[1] for v in points);ymax=max(v[1] for v in points)
        bx,by,bw,bh=box;scale=min(bw/(xmax-xmin),bh/(ymax-ymin))
        ox=bx+(bw-scale*(xmax-xmin))/2;oy=by+(bh-scale*(ymax-ymin))/2
        def xy(v):return ox+(v[0]-xmin)*scale,oy+(ymax-v[1])*scale
        labels_here=[]
        for f in features:
            g=f['geo'];d=DATA[g];q=scores['zscore',g]
            fill=("#c3e0e3" if d['linked'] else "#e6b485") if mode=='coverage' else cols[q['category']]
            path=' '.join('M'+' L'.join(f'{x:.2f},{y:.2f}' for x,y in map(xy,ring))+' Z' for ring in f['rings'])
            sample_note=f'; small need-group sample, n={d["n"]}' if (d['n']<50) else ''
            pieces.append(f'<path class="district" data-key="{g}" data-rule="{mode}" data-small="{str((d["n"]<50)).lower()}" d="{path}" fill="{fill}" fill-rule="evenodd" stroke="white" stroke-width="1.3" tabindex="0"><title>{esc(d["name"])}: modern use {d["mcpr"]:.1f}%{sample_note}</title></path>')
            cx,cy=xy(f['center'])
            if mode=='coverage':
                radius=3.2+math.sqrt(d['facilities'])*.60
                pieces.append(f'<circle data-key="{g}" data-rule="{mode}" class="facility-dot" cx="{cx:.2f}" cy="{cy:.2f}" r="{radius:.2f}" fill="#123d50" fill-opacity=".65" stroke="white" stroke-width="1.2"><title>{d["facilities"]} included facilities</title></circle>')
            if not f['karachi'] or inset:
                name=d['name']
                if inset:name=name.replace('Karachi ','').replace('Karachi','South' if 'SOUTH' in g else '').replace('Kemari','Keamari')
                if (d['n']<50):name+=' †'
                labels_here.append([cx,cy,cx,cy,name])
        # Separate text labels slightly; leader lines preserve the location reference.
        for _ in range(120):
            for i,l in enumerate(labels_here):
                for j in range(i):
                    o=labels_here[j];dx=l[2]-o[2];dy=l[3]-o[3]
                    if abs(dx)<max(47,(len(l[4])+len(o[4]))*2.6) and abs(dy)<18:
                        push=(18-abs(dy))*.30;sgn=1 if dy>=0 else -1;l[3]+=sgn*push;o[3]-=sgn*push
                l[2]=min(bx+bw-25,max(bx+25,l[2]));l[3]=min(by+bh-7,max(by+10,l[3]))
        for cx,cy,lx,ly,name in labels_here:
            if abs(ly-cy)>6:pieces.append(f'<path d="M{cx:.1f},{cy:.1f} L{lx:.1f},{ly:.1f}" stroke="#667f8c" stroke-width=".6" fill="none" pointer-events="none"/>')
            pieces.append(f'<text x="{lx:.1f}" y="{ly:.1f}" class="map-label" text-anchor="middle" pointer-events="none">{esc(name)}</text>')
    draw(layout,(18,18,665,625))
    pieces+=['<rect x="716" y="82" width="365" height="375" rx="12" fill="white" stroke="#d9e5e9"/>',
             '<text x="738" y="111" class="inset-heading">Karachi division · enlarged</text>']
    draw([f for f in layout if f['karachi']],(730,128,335,310),True)
    pieces+=['<text x="740" y="499" class="map-caption">Hover a district for weighted use rates</text>',
             '<text x="740" y="523" class="map-caption">and survey / facility coverage.</text>',
             '<text x="740" y="563" class="map-caption">† Need-group sample below 50 women</text>',
             '<text x="740" y="590" class="map-caption">N ↑</text>',
             '<text x="38" y="670" class="map-caption">District boundaries: WFP Geoportal · display geography</text>','</svg>']
    return ''.join(pieces)

def mapbox(svg, mode):
    return f'<div class="mapbox">{svg}<div class="map-toolbar"><span>Hover, tap or focus a district for weighted use estimates and sample size</span><button class="expand-map" type="button">⤢ Enlarge map</button></div></div>'

def legend():
    counts = {k: sum(r['category'] == k for r in p['scores'] if r['method'] == 'zscore') for k in cols if k != 'unlinked'}
    counts['unlinked'] = sum(g not in R for g in names)
    return '<div class="map-legend">' + ''.join(f'<span><i style="background:{cols[k]}"></i>{cats[k]} <b>{counts[k]}</b></span>' for k in cols if counts[k]) + '</div>'

def display_download_name(value):
    return str(value).replace("priorities", "components").replace("priority", "component")

def download(name, label):
    b = base64.b64encode((root/name).read_bytes()).decode()
    return f'<a class="download" download="{esc(display_download_name(name))}" href="data:text/csv;base64,{b}">{esc(label)} ↓</a>'

def chart_svg():
    w, h, left, bw = 1500, 955, 390, 720
    rows = p['bars']
    if rows[-1]['field'] != 'unresolved':
        h = 865
    svg = [f'<svg class="use-chart" viewBox="0 0 {w} {h}" xmlns="{NS}" role="img" aria-label="Weighted contraceptive use and intention among 3274 women with authorized fpn2_derived=1">']
    svg += ['<rect width="1500" height="955" fill="white"/>']
    for r in rows:
        y = 50 + r['y'] * 57
        v = 100*r['estimate']
        assert r['denominator'] == 3274
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
    svg.append(f'<text x="{left+bw/2}" y="{axis_y+35}" text-anchor="middle" class="chart-axis">Weighted share of all women with fpn2_derived = 1 (N = 3,274)</text></svg>')
    return ''.join(svg)

def mix(a, b, t):
    return '#'+''.join(f'{round(int(a[i:i+2],16)*(1-t)+int(b[i:i+2],16)*t):02x}' for i in (1,3,5))

def heatmap():
    headers = ''.join(f'<th scope="col">{esc(colnames[k])}</th>' for k in D)
    headers += '<th class="hm-gap"></th>' + ''.join(f'<th scope="col">{esc(colnames[k])}</th>' for k in S)
    rows = []
    for g in ['SINDH']+areas:
        m = meta[g]
        name = 'Linked Sindh' if g == 'SINDH' else names[g]
        sub = f'n = {m["n_need"]:,} women in need' if m['n_need'] else 'Facility data only'
        if m['n_need'] and m['n_need'] < 50:
            name += ' †'
        cells = [f'<th scope="row">{esc(name)}<small>{esc(sub)}</small></th>']
        for side, fields in [('Demand', D), ('Supply', S)]:
            if side == 'Supply': cells.append('<td class="hm-gap"></td>')
            for k in fields:
                r = HM[g, side, k]
                if r['value'] is None:
                    cells.append('<td class="hm-missing" title="No valid estimate">—</td>')
                    continue
                t = r['value']/100 if side == 'Demand' or k == 'stockout_burden' else 1-r['value']/100
                color = mix('#eff8f8', '#057483', t) if side == 'Demand' else mix('#fff6e9', '#bf570e', t)
                tip = f'{colnames[k]}: {r["value"]:.3f}%. Included values: {r["n_valid"]}/{r["n_total"]}.'
                cells.append(f'<td title="{esc(tip)}" style="background:{color};color:{"white" if t>.67 else "#233e4c"}">{r["value"]:.1f}%</td>')
        rows.append('<tr'+(' class="sindh-row"' if g=='SINDH' else '')+'>'+''.join(cells)+'</tr>')
    return '<div class="table-wrap hm-wrap"><table class="joint-heatmap"><thead><tr><th rowspan="2">Area</th><th colspan="6" class="demand-head">Demand · SURGO<small>Weighted women with fpn2_derived = 1</small></th><th class="hm-gap"></th><th colspan="6" class="supply-head">Supply · Manitoba<small>Confirmed FP providers · within district</small></th></tr><tr>'+headers+'</tr></thead><tbody>'+''.join(rows)+'</tbody></table></div>'


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
    daily_provision='Offered modern methods provided daily (facility average)',
    larc_stock='Facilities with at least one LARC category in stock',
    stockout_burden='Product groups with a coded stock interruption (facility average)',
)
# All displayed summaries remain aggregate; respondent rows and fitted objects
# are never embedded in the report.
load = lambda file: json.loads((root/file).read_text())
m = load('models_zscore/model_results.json')
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

main_scores=score_dict(p)
style_module=Path(__file__).resolve().parents[3]/'analysis/common/report.css'
styles=base_css.replace('.lagos-map','.sindh-map')+style_module.read_text()+'''
.sindh-map{width:100%;height:auto;display:block}.district{cursor:pointer;outline:none}.district:hover,.district:focus{stroke:#153448;stroke-width:2}.map-label{font-size:11px;font-weight:600;fill:#254453;paint-order:stroke;stroke:white;stroke-width:2.5px}.map-caption{font:12px Arial;fill:#607985}.inset-heading{font:600 15px Arial;fill:#17374a}section{padding:28px;border:1px solid #dfe8ed;border-radius:10px;margin:24px 0;background:white}.intro,.details-note{line-height:1.7}.table-wrap{overflow:auto}.planning{width:100%;border-collapse:collapse}.planning th,.planning td{padding:12px 10px;font-size:13px;line-height:1.5;text-align:left;vertical-align:top;border-bottom:1px solid #e1e9ed}.planning thead{background:#eef5f7}.planning small{display:block;font-size:10px;color:#97622f;margin-top:4px}.component-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:22px}.component-grid article{min-width:0}h3{margin-top:28px}h4{padding:14px;background:#eef7f7}.supply-summary h4{background:#fff5e8;color:#9b551f}.demand-summary h4{color:#087d89}.model-figure img{width:100%;height:auto}.callout{padding:15px 18px;border-left:4px solid #16818c;background:#eef8f8;line-height:1.7;font-size:14px}.warning{border-left-color:#bd7528;background:#fff7eb}.footnote{font-size:12px;line-height:1.7;color:#617c89}.use-chart{width:100%}.chart-label,.chart-number,.chart-tick,.chart-axis{font-family:Arial,sans-serif;fill:#294958}.chart-number{font-size:17px}.chart-tick{font-size:15px}.chart-axis{font-size:17px}.joint-heatmap{width:100%;min-width:1250px;border-collapse:separate;border-spacing:4px;table-layout:fixed}.joint-heatmap td,.joint-heatmap th{padding:12px 5px;text-align:center;font-size:12px}.joint-heatmap th:first-child{width:175px;text-align:left}.joint-heatmap small{display:block;font-size:10px}.demand-head{background:#eef7f7}.supply-head{background:#fff5e8}.hm-gap{width:12px}.hm-missing{background:#edf0f3}.model-meta,.model-note{line-height:1.7;font-size:14px}.model-figure{margin:22px 0}.table-heading{display:flex;gap:18px;flex-wrap:wrap;margin:14px 0}.download{font-size:12px}.method-notes{line-height:1.7;font-size:13px}.country-note{font-size:13px;color:#546f7d}@media(max-width:950px){.component-grid{grid-template-columns:1fr}}@media print{.model-figure,.mapbox{break-inside:avoid}}
'''
styles=styles.replace('lagos-','sindh-')
header='''<header><div><span class="eyebrow">SURGO × UNIVERSITY OF MANITOBA</span><h1>Sindh · updated district-wide supply analysis</h1><p>Women’s reported concerns alongside recorded family-planning provision</p><p class="country-note">Each woman receives FP-provider averages for her own district. Supply is district-wide; the women’s file has no Rural/Urban field.</p><nav aria-label="Contents"><a href="#coverage">1 · Coverage</a><a href="#use">2 · Use and intention</a><a href="#joint">3 · Conditions and concerns</a><a href="#models">4 · Models</a><a href="#definitions">Definitions and missingness</a></nav></div></header>'''
coverage=f'''<section id="coverage"><span class="eyebrow">01 · COVERAGE AND LINKAGE</span><h2>Women and facilities overlap in all 30 districts</h2><p>Retain all <b>10,364 source women</b>. The coverage map uses all <b>4,308 source facility records</b>. Later supply summaries use the <b>2,823 facilities answering Q506 = Yes</b>, all assigned to a surveyed district.</p><div class="map-legend"><span><i style="background:#c3e0e3"></i>Women + facilities · 30 districts</span><span>● Dot area reflects source facility count</span></div>{mapbox(map_svg(coverage_svg,'coverage'),'coverage')}<p class="footnote">District facility-count dots are not facility coordinates. Boundaries and Karachi inset are retained from the existing Sindh display geography. Coverage precedes selection of the need group.</p><h3>How women receive supply measures</h3>{table(['Woman','Facility characteristics assigned'],[['District A','Valid-response averages among confirmed FP providers in District A.'],['District B','District B’s own FP-provider averages.']])}<p>The checked geography uses the facility survey’s named <b>district</b> field and explicit supplied spelling aliases. All original matched assignments agree with this field; it also locates 881 facilities omitted from the older source crosswalk. There is no Rural/Urban split and no individual facility-attendance linkage.</p></section>'''

use=f'''<section id="use"><span class="eyebrow">02 · USE, INTENTION AND METHOD CHOICE</span><h2>Women in the agreed family-planning need group</h2><p class="callout">Section 2 onward uses authorized <b>fpn2_derived = 1</b>: <b>3,274 women</b>. Use the previously authorized instrument-based need reconstruction. The original fpn2 field is entirely code 95. No new need coding or permanent-protection additions are introduced.</p>{chart_svg()}<p class="footnote">Every bar uses all 3,274 women as its denominator. Percentages use final <b>Normalizedweights</b>; n is unweighted. Subsets overlap. {interval_note} Preference is the method wanted at initiation, not necessarily current preference. Future intention is for any method without a specified 12-month initiation window.</p>{download('use_distribution.csv','Use estimates')}</section>'''

def frequency(category,keys):
    group=[r for r in p['planning'] if r[category+'_flag']]
    count=Counter(k for r in group for k in r['top_'+category])
    ordered=sorted(keys,key=lambda k:(-count[k],keys.index(k)))
    return len(group),table(['Component','districts where this is among the top three','Share of this group'],[[esc(labels[k]),f'{count[k]} of {len(group)}',f'{100*count[k]/len(group):.1f}%'] for k in ordered])

def priority_detail(category):
    rows=[]
    for g in linked_areas:
        r=planning[g]
        if not r[category+'_flag']:continue
        row=[area(g)]
        for k in r['top_'+category]:
            q=HM[g,category.title(),k]
            row.append(f'<span title="Valid responses {q["n_valid"]}/{q["n_total"]}">{esc(labels[k])}<br><b>{q["value"]:.1f}%</b></span>')
        rows.append(row)
    headers=['Weakest service component','Second-weakest component','Third-weakest component'] if category=='supply' else ['Most elevated concern','Second-most elevated concern','Third-most elevated concern']
    ordering='lowest to third-lowest favorable-supply' if category=='supply' else 'highest to third-highest demand'
    explanation=f'<p class="footnote">Ordered from the {ordering} component z-score, comparing each component with its own geographic reference mean and SD. Displayed percentages are the original measures. This ordering does not identify the most effective interventions.</p>'
    return explanation+table(['District']+headers,rows)

sn,st=frequency('supply',S);dn,dt=frequency('demand',D)
priorities=f'''<div id="component-summaries"><h3>Which service weaknesses and reported concerns recur across districts?</h3><p>Count appearances among each district’s three lowest supply component z-scores or highest demand component z-scores. Percentages describe districts, not women or facilities. Both-flagged districts are included in each relevant side; the two groups overlap.</p><div class="component-grid"><article id="supply-component-summary" class="supply-summary"><h4>Service weaknesses · {sn} districts with supply challenges</h4>{st}</article><article id="demand-component-summary" class="demand-summary"><h4>Reported concerns · {dn} districts with demand challenges</h4>{dt}</article></div><p class="footnote">Each district contributes three selected components; percentages do not sum to 100%. These are relative component comparisons, not causal reasons for nonuse.</p></div><div id="supply-department-components"><h3>The {sn} districts with supply challenges · three weakest service components</h3>{priority_detail('supply')}</div><div id="demand-department-components"><h3>The {dn} districts with demand challenges · three most elevated reported concerns</h3>{priority_detail('demand')}<p class="footnote">Displayed values are original percentages, ordered by component z-scores. Demand rates use Normalizedweights and valid-domain answers among women with fpn2_derived = 1. Supply rates retain facility-valid denominators; free provision and training are averages of within-facility proportions. † Fewer than 50 women in need. Hover for valid/total response coverage.</p></div>'''
fullrows=[]
for g in linked_areas:
    r=planning[g]
    entries=[]
    for side,key in [('Demand','demand'),('Supply','supply')]:
        entries.extend(['Yes' if r[key+'_flag'] else 'No','<br>'.join(esc(labels[k])+f' <b>{HM[g,side,k]["value"]:.1f}%</b>' for k in r['top_'+key]) if r[key+'_flag'] else '—'])
    fullrows.append([area(g)]+entries)
zcut=thresholds['zscore']
joint=f'''<section id="joint"><span class="eyebrow">03 · JOINT CONDITIONS, SERVICE WEAKNESSES AND REPORTED CONCERNS</span><h2>Reported concerns alongside recorded services</h2><p>Six weighted concern percentages among women in need appear beside six unweighted facility measures. Facility measures summarize confirmed FP providers within each district. Each indicator uses its own valid-response denominator.</p>{heatmap()}<p class="footnote">Darker teal means more concern; darker orange means weaker provision. Higher stockout burden is worse; higher values of the other five supply measures are better. Undefined responses remain missing. Stockouts become continuity (1 − burden) before scoring. LARC stock uses the original UoM one_larc_stockin criterion.</p><p class="callout warning"><b>Training coverage:</b> only 278 of 2,823 FP facilities (9.8%) have a defined p_train proportion. District means describe those valid reporters; they do not establish the share of all providers trained across the district. Undefined ratios can reflect routing or unusable denominators as well as unanswered items, and are not recoded as zero.</p><h3>District planning profiles · z-score average</h3><p>Standardize each component using its mean and sample SD across the 30 linked districts, then average six components separately by side. The average final score is <b>zero</b>. Flag demand <b>above zero</b> and supply readiness <b>below zero</b>. Values within 1e-10 of zero are unflagged ties. Positive demand means above-average concerns; positive readiness means above-average recorded provision across the selected measures. These are relative Sindh planning signals, not absolute adequacy standards. Equal district references differ from the personal demand score used in models.</p>{legend()}{mapbox(map_svg(category_svg,'zscore'),'zscore')}<p class="footnote">Hover for weighted CPR and mCPR among women in need. Neither flagged does not establish adequate services or absence of barriers.</p>{priorities}<h3>District service weaknesses and reported concerns · all 30</h3>{table(['District','Demand challenges','Top three reported concerns','Supply challenges','Top three service weaknesses'],fullrows)}</section>'''

def model_section(payload,folder,pca=False):
    a=payload['audit'];co={(r['model'],r['term']):r for r in payload['coefficients']}
    tests=[r for r in payload['tests'] if r['model'] in ('score_joint','components_joint')]
    ort=lambda r:f'{r["odds_ratio"]:.2f} ({r["or_lo"]:.2f}–{r["or_hi"]:.2f})'
    title='PCA sensitivity models' if pca else 'Modern use in relation to personal demand and district services'
    text=f'<h2>{title}</h2><p class="model-meta">Same <b>{a["common_n"]:,} women</b>, including {a["common_events"]:,} modern users, across 30 districts. Outcome: current modern use = 1; confirmed traditional-only use or nonuse = 0. Adjust for age + age², education and fertility goal. Partnership is constant in the modeled group; Rural/Urban is unavailable. Original Normalizedweights; district-clustered CR2/Satterthwaite intervals. No random effects or interactions.</p>'
    text+=table(['Model','Predictors alongside the same covariates'],[['1','Personal demand score'],['2','Assigned district supply risk score'],['3','Both scores'],['4','Six personal demand indicators separately'],['5','Six personal indicators + supply risk score']])
    if not pca:text+='<p class="model-note">Each woman’s demand score averages her six responses standardized with final-weighted means and population SDs among valid women in need. Supply readiness averages her six district-wide favorable measures standardized against the 30-district planning references, after stockout burden becomes continuity = 1 − burden. Model supply risk is negative readiness; higher risk means weaker provision. Each woman retains personal demand variation, while women in one district share a supply score. All six responses are required; scores enter continuously in their original units.</p>'
    d,su=co['score_joint','demand_score'],co['score_joint','supply_risk']
    text+=fig(folder,'01_score_models.svg','Adjusted demand, supply and joint score odds ratios with district CR2 confidence intervals.')
    text+=f'<p class="callout"><strong>Joint model.</strong> Demand OR <b>{ort(d)}</b>; supply OR <b>{ort(su)}</b> per one score point. Interpret the intervals alongside the estimates. These associations do not allocate a percentage of nonuse to each side or establish a causal binding constraint.</p>'
    if not pca:
        text+='<p class="model-note"><b>Planning implication:</b> Personal demand concerns are associated with modern use after adjustment. The selected supply index has an interval spanning no association; this does not show that improving supply has no value or determine an investment split. Limited training coverage is an additional reason for caution.</p>'
    text+=fig(folder,'02_score_probabilities.svg','Weighted adjusted modern-use probabilities across observed demand and district supply scores.')
    text+='<p class="model-note">Curves average predictions over the same women and observed covariates. The other score stays observed. These are adjusted cross-sectional comparisons, not expected effects of an intervention.</p>'
    text+=fig(folder,'03_component_models.svg','Six individual concerns alongside the district supply risk score.')
    if not pca:
        supported=[r for r in payload['coefficients'] if r['model']=='components_joint' and r['term'] in D and r['or_hi']<1]
        supported.sort(key=lambda r:r['odds_ratio'])
        if supported:text+='<p class="callout"><b>Individual concerns:</b> '+ '; '.join(esc(labels[r['term']])+f' — adjusted OR {ort(r)}' for r in supported)+'. These are associations, not estimated intervention effects.</p>'
    positives=[r for r in payload['coefficients'] if r['model']=='components_joint' and r['term'] in D and r['or_lo']>1]
    if positives:text+='<p class="callout warning"><b>A result needing care:</b> '+ '; '.join(esc(labels[r['term']])+f' has a positive adjusted association (OR {ort(r)})' for r in positives)+'. This does not mean a barrier improves use. General healthcare permission, unmeasured circumstances and selection may differ from FP-specific access; these data are cross-sectional.</p>'
    text+=fig(folder,'04_component_probability_gaps.svg','Adjusted modern-use percentage-point differences for each concern, with confidence intervals.')
    text+='<p class="model-note">For each concern, predict every woman with that response present and absent, retaining other variables. Average using Normalizedweights and subtract; intervals account for covariance. The MC5 indicator measures confidence, despite the retained knowledge label.</p>'
    text+=table(['Joint model','Block','Restrictions','Effective denominator df','Raw p','Holm p'],[[r['model'],r['block'],str(r['df_num']),f'{r["df_denom"]:.2f}',fp(r['p']),fp(r['holm_p'])] for r in tests])
    text+='<p class="footnote">Two block tests per joint model; Holm correction is within that pair. With only 30 contextual supply values, woman count does not determine supply precision. Primary intervals cluster by district. A separate survey-GLM inference check uses recorded clusterCode with the same weights; original strata and FPC remain undocumented. Neither approach reconstructs every element of the original design.</p>'
    if not pca:
        text+='<details><summary>Model diagnostics and limits</summary>'+table(['Model','Women','Modern users','Parameters','districts','Maximum VIF','Minimum coefficient df'],[[r['model'],str(r['n']),str(r['events']),str(r['parameters']),str(r['districts']),f'{r["max_vif"]:.2f}',f'{r["minimum_df"]:.2f}'] for r in payload['samples']])+'<p>All models converged with full-rank designs. These checks do not establish statistical power. Low effective degrees of freedom, only 30 supply contexts, and uncertainty in constructing scores limit inference.</p></details>'
    return text

models='<section id="models"><span class="eyebrow">04 · ADJUSTED ASSOCIATIONS</span>'+model_section(m,'models_zscore')+'</section>'
missing_d=list(csv.DictReader((root/'demand_missingness.csv').open()))
missing_s=list(csv.DictReader((root/'supply_missingness.csv').open()))
need_n=p['audit']['need_n'];fp_n=p['audit']['linked_fp_facilities'];common_n=m['audit']['common_n']
complete_demand=p['audit']['complete_demand_women']
missing_tables=table(['Demand concern',f'Unresolved / {need_n:,}','Unweighted share','Weighted share'],[[esc(labels[r['variable']]),r['missing'],f'{float(r["missing_pct"]):.1f}%',f'{float(r["weighted_missing_pct"]):.1f}%'] for r in missing_d])+table(['Facility measure',f'Valid / {fp_n:,} FP providers','Undefined','Undefined share'],[[esc(labels[r['variable']]),f'{r["valid"]} / {r["total"]}',r['missing'],f'{float(r["missing_pct"]):.1f}%'] for r in missing_s])
validrows=[[area(g),str(meta[g]['facility_n'])]+[str(HM[g,'Supply',k]['n_valid']) for k in S] for g in linked_areas]
supply_definitions=[
 [labels['offering'],'Mean of original one_larc_3_sa_offer: share meeting ≥1 long-acting plus ≥3 short-acting offering categories.'],
 [labels['free'],'Mean of original p_free_prod: average facility fraction of recorded products provided free. Not the percentage of facilities providing a free method.'],
 [labels['training'],'Mean of original p_train: average facility fraction of recorded personnel classified as trained under UoM topic/eligibility rules.'],
 [labels['daily_provision'],'Within each FP facility: count of modern-method classes offered Daily / count offered Daily, Weekly, Fortnightly or Monthly; average valid facility ratios. Seven classes; IUD interval/post-abortion/postpartum contexts and injectable variants count once using the most frequent recorded offered variant. The entirely blank hormonal-IUD and Noristerat columns do not count as No responses.'],
 [labels['larc_stock'],'Mean of original one_larc_stockin: share meeting the ≥1 LARC stock criterion. This is a current stock/readiness indicator, distinct from service frequency or stockout history.'],
 [labels['stockout_burden'],'Mean of original p_any_stockout: average facility share of coded product groups with a current/recent interruption under the UoM rule. Higher burden is worse. Not the share of facilities with any stockout.']]
demand_definitions=[
 [labels['agency'],'HS7C: Yes to needing permission to seek general healthcare; No is zero. Not FP-specific partner approval.'],
 [labels['stigma'],'PB7B/C: concern about being seen seeking FP or others’ reactions (codes 1/2). Any known concern = 1; both known nonconcerns = 0; otherwise unresolved.'],
 [labels['health'],'PB2A only: agrees that contraceptive methods can cause health problems (1/2); disagrees (3/4) = 0; special or unanswered responses unresolved. PB2B/C are excluded.'],
 [labels['fertility'],'MP4_1–6: any selected reversible-method future-fertility concern, among coherent MP4_1–9 responses. No selected reversible method = 0 only with coherent recorded selections.'],
 [labels['confidence'],'MC5A/B: low confidence choosing a method or managing side effects/switching (3/4). Any low response = 1; both known high-confidence responses = 0; otherwise unresolved. This measures confidence, despite the retained knowledge label.'],
 [labels['opposes'],'FP1: mostly/completely against FP (3/4) versus mostly/completely in favor (1/2). Other responses unresolved.']]
corr=list(csv.DictReader((root/'component_correlations.csv').open()))
corr_rows=[]
for side in ('Demand','Supply'):
    group=sorted((r for r in corr if r['side']==side),key=lambda r:-abs(float(r['correlation'])))[:3]
    corr_rows.extend([[r['side'],esc(labels[r['first']]),esc(labels[r['second']]),f"{float(r['correlation']):+.3f}",r['paired_records']] for r in group])
correlation_note='<details><summary>Component overlap and correlations</summary>'+table(['Side','First component','Second component','Correlation','Paired records'],corr_rows)+'<p>Demand correlations use women in need, final weights and valid pairs. Supply correlations use one unweighted district profile per district. These are the three largest absolute correlations on each side. Equal-weight indices can emphasize related components; model VIF checks do not establish independent supply domains. Stockout correlations here use raw burden.</p>'+download('component_correlations.csv','All component correlations')+'</details>'
definitions=f'''<section id="definitions"><h2>Definitions, response coverage and model sample</h2><h3>Missing or unresolved demand and supply measures</h3>{missing_tables}<p class="footnote">Demand unknowns include unanswered, don’t-know or incoherent responses. Unknown facility proportions can reflect routing, undefined denominators or unanswered items; they are not assumed to be zero. Valid numeric zeros and FALSE remain in the mean.</p><p><b>{complete_demand:,} of {need_n:,} women</b> have all six demand indicators. Supply summaries use valid responses separately for each measure among <b>{fp_n:,} confirmed FP providers</b>; no common complete-facility restriction. Each of the 30 districts has a defined mean for every component; valid facility counts differ substantially. The common model sample has <b>{common_n:,} women</b>; {need_n-common_n:,} are excluded ({100*(need_n-common_n)/need_n:.1f}%) after requiring all model variables.</p><details><summary>Valid FP-facility responses by district</summary>{table(['District','FP facilities']+[labels[k] for k in S],validrows)}</details><h3>What each supply measure represents</h3>{table(['Measure','Construction and interpretation'],supply_definitions)}<p class="footnote">The original cleaned stockout measure retains UoM product-group and missing-answer rules; numeric completeness does not establish that every underlying stock question was answered. No upstream stockout recoding is introduced. Q506 Yes is required before averaging. Each included facility contributes equally to its measure; no facility weights are used. No undefined facility value is zero-filled in this update.</p><h3>Need definition retained</h3><p>The authorized derived flag excludes the existing pregnancy, fertility, sexual-activity, postpartum and permanent-infertility categories. Its SA1 policy also excludes codes 95/97, although these special responses do not establish sexual inactivity. That approved rule is preserved here; no sterilization-based need_n additions are made. This is an instrument-based reconstruction, not a populated original SURGO FPN2 flag or the DHS unmet-need algorithm.</p><h3>Demand coding</h3>{table(['Domain','Source questions and coding'],demand_definitions)}<p>Every later section uses the authorized fpn2_derived = 1 among consenting positive-weight women. Final Normalizedweights are retained. Overview intervals use recorded clusterCode; main models use district-clustered CR2 inference. Original strata and FPC remain undocumented, so a complete original-design reconstruction is not claimed.</p><p class="footnote">Independent verification passed {verification['check_count']} checks. These verify implementation, not nonresponse assumptions, adequate power or causality.</p>{download('demand_missingness.csv','Demand response coverage')}{download('supply_missingness.csv','Supply response coverage')}{correlation_note}</section>'''
js='''const DATA=__DATA__;const tip=document.querySelector('.tooltip');
function hide(){tip.classList.remove('show')}
function move(e,el){const b=el.getBoundingClientRect();let x=e&&e.clientX!=null?e.clientX:b.x+b.width/2,y=e&&e.clientY!=null?e.clientY:b.y+b.height/2;let l=x+16,t=y+16;if(l+tip.offsetWidth>innerWidth-10)l=x-tip.offsetWidth-16;if(t+tip.offsetHeight>innerHeight-10)t=y-tip.offsetHeight-16;tip.style.left=Math.max(10,l)+'px';tip.style.top=Math.max(10,t)+'px'}
function show(el,e){const d=DATA[el.dataset.key];if(!d)return;const mode=el.dataset.rule;const pc=v=>v==null?'Not estimated':v.toFixed(1)+'%';const category=mode==='coverage'?(d.linked?'Women + facility data':'Facility data only'):d.category;tip.innerHTML='<div class="tip-name">'+d.name+'</div><div class="tip-cat">'+category+'</div><div class="tip-rates"><div><b>'+pc(d.cpr)+'</b><span>Any-method use</span></div><div><b>'+pc(d.mcpr)+'</b><span>Modern use</span></div></div><div class="tip-denom">'+(d.linked?'Weighted among women with fpn2_derived=1 · n='+d.n:'No SURGO need-group estimate')+'</div>'+(mode!=='coverage'&&d.linked?'<div class="tip-score">Demand: '+d.demand.toFixed(3)+'<br>Supply readiness: '+d.supply.toFixed(3)+'</div>':'')+'<div class="tip-note">'+d.facilities+' source facilities · '+d.women+' source women</div>'+(d.n&&d.n<50?'<div class="tip-alert">Small need-group sample: n='+d.n+'</div>':'');tip.classList.add('show');move(e,el)}
document.querySelectorAll('[data-key][data-rule]').forEach(el=>{el.addEventListener('pointerenter',e=>show(el,e));el.addEventListener('pointermove',e=>move(e,el));el.addEventListener('pointerleave',hide);el.addEventListener('focus',e=>show(el,e));el.addEventListener('blur',hide);el.addEventListener('click',e=>show(el,e))});document.querySelectorAll('.expand-map').forEach(b=>b.addEventListener('click',()=>{const box=b.closest('.mapbox');box.classList.toggle('enlarged');document.body.classList.toggle('modal-open',!!document.querySelector('.mapbox.enlarged'));b.textContent=box.classList.contains('enlarged')?'Close enlarged map':'⤢ Enlarge map';hide()}));document.addEventListener('keydown',e=>{if(e.key==='Escape'){hide();document.querySelectorAll('.mapbox.enlarged').forEach(b=>b.classList.remove('enlarged'));document.body.classList.remove('modal-open')}});'''.replace('__DATA__',json.dumps(DATA,separators=(',',':')))
page='<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Sindh updated district analysis | SURGO × Manitoba</title><style>'+styles+'</style></head><body>'+header+'<main>'+coverage+use+joint+models+definitions+'</main><div class="tooltip" role="status" aria-live="polite"></div><footer>Sindh · district-wide supply assignment · authorized instrument-based need classification · cross-sectional associations.</footer><script>'+js+'</script></body></html>'
output.parent.mkdir(parents=True,exist_ok=True);output.write_text(page)
assert hashlib.sha256(ref.read_bytes()).hexdigest()==reference_hash
print('Rendered',output.name,'with district-wide supply, component tables and appendix checks.')
