"""Render the Côte d’Ivoire department-only analysis from verified aggregate outputs."""
from pathlib import Path
from collections import Counter
import argparse, base64, csv, html, json, math, re

ap = argparse.ArgumentParser()
ap.add_argument('--data-dir', required=True)
ap.add_argument('--output', required=True)
a = ap.parse_args()
root, output = Path(a.data_dir), Path(a.output)
load = lambda f: json.loads((root / f).read_text())
p = load('report_data.json')
m = load('models_zscore/model_results.json')
pc = load('pca_appendix/pca_results.json')
pm = load('pca_appendix/models/model_results.json')
verification = load('checks/verification.json')
layout = load('map_layout.json')['features']
sens_scores = {}
fc = {}

AUD, MA = p['audit'], m['audit']
D, S = p['demand_variables'], p['supply_variables']
labels = dict(zip(D + S, p['demand_labels'] + p['supply_labels']))
labels.update(demand_score='Demand risk score', supply_risk='Supply risk score')
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
    daily_provision='Daily provision of offered modern methods (facility average)',
    larc_stock='Facilities with at least one LARC in stock',
    stockout_burden='Stockout burden across product groups (higher = worse)',
)
rates = {r['geo']: r for r in p['rates']}
cov = {r['geo']: r for r in p['coverage']}
hm = {(r['geo'], r['side'], r['variable']): r for r in p['heatmap']}
scores = {(r['method'], r['geo']): r for r in p['scores']}
planning = {r['geo']: r for r in p['planning']}
bars = {r['field']: r for r in p['bars']}
cuts = {r['method']: r for r in p['thresholds']}
assign = {r['geo']: r for r in p['assignment_summary']}
areas = sorted(rates, key=lambda g: rates[g]['name'])
for feature in layout:
    cov.setdefault(feature['geo'],dict(geo=feature['geo'],name=feature['name'],women=0,all_source_women=0,n_need=0,facilities=0))
all_areas = sorted(g for g in cov if g != 'CDI')
small = {g for g in areas if rates[g]['n'] < 50}
n, model_n = AUD['need_n'], MA['common_n']
assert n == 4292 and AUD['eligible_mapped_facilities'] == 2370
assert len(areas) == 82 and len(layout) == len(all_areas) == 111
assert pm['audit']['common_n'] == model_n and verification['all_passed']
esc = lambda s: html.escape(str(s), quote=True)
name = lambda g: cov[g]['name'].title()
def french_terms(value):
    return re.sub(r'\bdepartments?\b',lambda m:('Département' if m[0][0].isupper() else 'département')+('s' if m[0].lower().endswith('s') else ''),value,flags=re.I)
FP_LINKED=AUD['eligible_mapped_facilities']
DAILY_VALID=AUD['daily_valid_linked']
DAILY_UNDEFINED=AUD['daily_undefined_linked']
pct = lambda v, digits=1: '—' if v is None else f'{v:.{digits}f}%'
fp = lambda v: '&lt;0.001' if v < .001 else f'{v:.3f}'
ort = lambda r: f'{r["odds_ratio"]:.2f} ({r["or_lo"]:.2f}–{r["or_hi"]:.2f})'
colors = dict(both='#78549b', demand='#047f8b', supply='#cd7729', neither='#bacad2',
              unassessed='#edf0f3', supply_only='#eee4ce', no_sources='#fafafa')
catnames = dict(both='Both', demand='Demand only', supply='Supply only', neither='Neither flagged',
                unassessed='Supply unassessed', supply_only='Facility data only', no_sources='Neither source recorded')
coverage_colors = dict(both='#6aadb5', supply_only='#d69545', demand_only='#8669a6', neither='#edf0f3')
coverage_labels = dict(both='Women + facility data', supply_only='Facility data only',
                       demand_only='Women’s data only', neither='Neither source')
methods = ['rank', 'raw', 'robust', 'median_count', 'zscore']
method_names = ['Rank average', 'Mean percentages', 'Median/IQR', 'Median count', 'Z-score average']

def table(headers, rows):
    return '<div class="table-wrap"><table class="planning"><thead><tr>' + ''.join(
        f'<th>{v}</th>' for v in headers) + '</tr></thead><tbody>' + ''.join(
        '<tr><th scope="row">' + row[0] + '</th>' + ''.join(f'<td>{v}</td>' for v in row[1:]) + '</tr>'
        for row in rows) + '</tbody></table></div>'

def display_download_name(value):
    return str(value).replace("priorities", "components").replace("priority", "component")

def download(file, title):
    data = base64.b64encode((root / file).read_bytes()).decode()
    return f'<a class="download" download="cdi_{display_download_name(Path(file).name)}" href="data:text/csv;base64,{data}">{esc(title)} ↓</a>'

def downloads(items):
    return '<div class="table-heading">' + ''.join(download(f, t) for f, t in items) + '</div>'

def figure(file, alt):
    data = base64.b64encode(french_terms((root / file).read_text()).encode()).decode()
    return f'<figure class="model-figure"><img src="data:image/svg+xml;base64,{data}" alt="{esc(alt)}"></figure>'

def note(text):
    return '<p class="model-key">' + text + '</p>'

def tag(category):
    color = colors[category]
    return f'<span class="category-tag" style="background:{color}25;color:{color if category != "unassessed" else "#627783"}">{catnames[category]}</span>'

def area_label(g):
    label = esc(name(g))
    if g in small:
        label += f' †<small class="sample-badge">Small sample · n={rates[g]["n"]}</small>'
    return label

DATA = {}
for g in all_areas:
    c = cov[g]
    has_d, has_s = c['all_source_women'] > 0, c['facilities'] > 0
    group = 'both' if has_d and has_s else 'demand_only' if has_d else 'supply_only' if has_s else 'neither'
    r, q = rates.get(g), scores.get(('zscore', g))
    DATA[g] = dict(name=name(g), women=c['women'], source_rows=c['all_source_women'], n=c['n_need'],
        facilities=c['facilities'], fp_facilities=c.get('fp_facilities',0), cpr=100*r['cpr']['estimate'] if r else None,
        mcpr=100*r['mcpr']['estimate'] if r else None, category=catnames[q['category']] if q else ('Facility data only' if has_s else 'Neither source recorded'),
        demand=q['demand_score'] if q else None, supply=q['supply_score'] if q else None,
        small_sample=g in small, coverage_group=group, coverage_label=coverage_labels[group],
        supply_issue_need=sum(q['need_women'] for q in p['assignment_issues'] if q['geo']==g),
        alternative_category=catnames[sens_scores[g]['category']] if g in sens_scores else ('Facility data only' if has_s else 'Neither source recorded'),
        alternative_supply=sens_scores[g]['supply_score'] if g in sens_scores else None,
        complete_facilities=int(fc[g]['complete_facilities']) if g in fc else None)
with (root / 'department_data_coverage.csv').open('w', newline='') as f:
    w = csv.writer(f)
    w.writerow(['department', 'source_women', 'consenting_positive_weight_women', 'women_in_need', 'facilities', 'coverage'])
    for g in all_areas:
        r = DATA[g]
        w.writerow([r['name'], r['source_rows'], r['women'], r['n'], r['facilities'], r['coverage_group']])
with (root / 'department_sample_flags.csv').open('w', newline='') as f:
    w = csv.writer(f)
    w.writerow(['department', 'women_in_need', 'below_50', 'below_25'])
    for g in areas:
        w.writerow([name(g), rates[g]['n'], g in small, rates[g]['n'] < 25])

def map_svg(mode):
    out = [f'<svg xmlns="http://www.w3.org/2000/svg" class="country-map" viewBox="0 0 1200 1000" role="img" aria-label="Côte d’Ivoire department {mode} map"><rect width="1200" height="1000" fill="#f8fbfc"/>']
    out.append(f'<defs><pattern id="unassessed-{mode}" patternUnits="userSpaceOnUse" width="7" height="7"><rect width="7" height="7" fill="#f2f3f5"/><path d="M-1,1 L6,8 M3,-3 L10,4" stroke="#bdc8ce" stroke-width="1"/></pattern></defs>')
    points=[xy for f in layout for ring in f['rings'] for xy in ring]
    xmin,xmax=min(v[0] for v in points),max(v[0] for v in points)
    ymin,ymax=min(v[1] for v in points),max(v[1] for v in points)
    scale=min(1130/(xmax-xmin),915/(ymax-ymin));ox=(1200-scale*(xmax-xmin))/2;oy=25
    def xy(v):return ox+(v[0]-xmin)*scale,oy+(ymax-v[1])*scale
    texts=[]
    for f in layout:
        g,d=f['geo'],DATA[f['geo']]
        category=(sens_scores[g]['category'] if mode=='sensitivity' else scores['zscore',g]['category']) if g in rates else 'supply_only' if d['facilities'] else 'no_sources'
        fill=coverage_colors[d['coverage_group']] if mode=='coverage' else f'url(#unassessed-{mode})' if category=='unassessed' else colors[category]
        path=' '.join('M'+' L'.join(f'{x:.2f},{y:.2f}' for x,y in map(xy,ring))+' Z' for ring in f['rings'])
        title=d['name']+': '+(f'modern use {d["mcpr"]:.1f}%' if d['mcpr'] is not None else 'no SURGO need-group estimate')
        out.append(f'<path class="district" data-key="{g}" data-mode="{mode}" d="{path}" fill="{fill}" fill-rule="evenodd" stroke="white" stroke-width="1" tabindex="0"><title>{esc(title)}</title></path>')
        cx,cy=xy(f['center'])
        if mode=='coverage' and d['facilities']:
            radius=2.6+math.sqrt(d['facilities'])*.52
            out.append(f'<circle class="facility-dot" data-key="{g}" data-mode="{mode}" cx="{cx:.2f}" cy="{cy:.2f}" r="{radius:.2f}" fill="#123d50" fill-opacity=".62" stroke="white" stroke-width="1"><title>{d["facilities"]} facility records</title></circle>')
        if f['label']:
            label=d['name']+(' †' if mode in ('classification','sensitivity') and d['small_sample'] else '')
            texts.append([cx,cy,cx,cy,label])
    for _ in range(180):
        for i,t in enumerate(texts):
            for o in texts[:i]:
                dx,dy=t[2]-o[2],t[3]-o[3]
                if abs(dx)<max(50,(len(t[4])+len(o[4]))*3.4) and abs(dy)<18:
                    step=(18-abs(dy))*.25;sgn=1 if dy>=0 else -1;t[3]+=sgn*step;o[3]-=sgn*step
            t[3]=min(925,max(35,t[3]))
    for cx,cy,lx,ly,label in texts:
        if abs(ly-cy)>6:out.append(f'<path d="M{cx:.1f},{cy:.1f} L{lx:.1f},{ly:.1f}" stroke="#7e929d" stroke-width=".6" fill="none" pointer-events="none"/>')
        out.append(f'<text x="{lx:.1f}" y="{ly:.1f}" class="map-label" font-family="Arial" font-size="12.5" font-weight="600" fill="#203d4d" text-anchor="middle" pointer-events="none">{esc(label)}</text>')
    out.append('<text x="1100" y="70" class="map-caption">N ↑</text>')
    out.append('<text x="35" y="972" class="map-caption">Labels are selective for legibility. Hover every department for its name, need-group use estimates and coverage.</text></svg>')
    return '<div class="mapbox">'+''.join(out)+'</div>'

def bar_chart():
    out = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1200 570" class="bar-chart" role="img" aria-label="Weighted use and intention among 4292 women with original SURGO FPN2 equals one">']
    x0, length = 405, 615
    for tick in range(0,101,20):
        x = x0+length*tick/100
        out.append(f'<line x1="{x}" y1="18" x2="{x}" y2="505" stroke="#e5edf0"/><text x="{x}" y="530" text-anchor="middle" class="axis-label">{tick}%</text>')
    for i,r in enumerate(p['bars']):
        y, h = 42+i*47, 18 if r['indent'] else 25
        out.append(f'<text x="385" y="{y+5}" text-anchor="end" class="bar-label {"sub" if r["indent"] else ""}">{esc(r["label"])}</text>')
        out.append(f'<rect x="{x0}" y="{y-h/2}" width="{length*r["estimate"]:.2f}" height="{h}" rx="2" fill="{r["color"]}"/>')
        if r['lo'] is not None and r['hi'] is not None:
            lo, hi = x0+length*r['lo'], x0+length*r['hi']
            out.append(f'<path d="M{lo:.2f},{y-5} V{y+5} M{lo:.2f},{y} H{hi:.2f} M{hi:.2f},{y-5} V{y+5}" stroke="#183847" stroke-width="1.3"/>')
        out.append(f'<text x="1040" y="{y+5}" class="bar-value">{r["estimate"]*100:.1f}% <tspan fill="#69808c">· n={r["n"]:,}</tspan></text>')
    out.append('<text x="710" y="562" text-anchor="middle" class="axis-label">Weighted percentage of all 4,292 women with SURGO FPN2 = 1</text></svg>')
    return ''.join(out)

def mix(a,b,t):
    ac,bc = ([int(s[i:i+2],16) for i in (1,3,5)] for s in [a,b])
    return '#'+''.join(f'{round(x+(y-x)*min(1,max(0,t))):02x}' for x,y in zip(ac,bc))

def heatmap():
    out = ['<div class="table-wrap"><table class="paired-heatmap"><thead><tr><th rowspan="2">Department</th><th colspan="6" class="demand-group">Women’s reported concerns · SURGO</th><th colspan="6" class="supply-group">Recorded provision · Manitoba</th></tr><tr>']
    out += [f'<th>{esc(labels[k])}'+('<br><small>↓ better</small>' if k=='stockout_burden' else '<br><small>↑ better</small>' if k in S else '')+'</th>' for k in D+S]
    out += ['</tr></thead><tbody>']
    for g in ['CDI']+areas:
        c = cov[g]
        title = 'Côte d’Ivoire · linked departments' if g == 'CDI' else name(g)
        mark = ' †' if g in small else ''
        out.append(f'<tr class="{"overall" if g == "CDI" else ""}"><th>{esc(title)}{mark}<small>n={c["n_need"]:,} women in need · {c.get("fp_facilities",0):,} FP facilities</small></th>')
        for side, ks in [('Demand',D),('Supply',S)]:
            for k in ks:
                r = hm[g,side,k]; v = r['value']
                den = f'Valid responses {r["n_valid"]:,} / {r["n_total"]:,}'
                if v is None:
                    out.append(f'<td class="unknown-cell" title="{esc(den)}; unknown, not zero">—</td>'); continue
                t = v/100 if side == 'Demand' or k=='stockout_burden' else 1-v/100
                bg = mix('#eef8f8','#047888',t) if side == 'Demand' else mix('#fff8ed','#bc570f',t)
                fg = 'white' if t > .62 else '#233d4b'
                out.append(f'<td style="background:{bg};color:{fg}" title="{esc(title+": "+labels[k]+" · "+den)}">{v:.0f}%</td>')
        out.append('</tr>')
    return ''.join(out)+'</tbody></table></div>'

def models(data, folder, pca=False):
    au = data['audit']
    local_labels=dict(labels)
    mixed = pca and bool(pc['audit'].get('negative_supply_PC1_loadings'))
    if pca:local_labels.update(demand_score='Demand score (PCA)',supply_risk='Supply pattern score (PCA)' if mixed else 'Supply risk score (PCA)')
    co = {(r['model'],r['term']):r for r in data['coefficients']}
    tests = {(r['model'],r['block']):r for r in data['tests']}
    gaps = {r['term']:r for r in data['gaps']}
    units = 'one oriented PC1 unit' if pca else 'one original average-z index point'
    s = '<h2>Modern use in relation to personal concerns and local services</h2>' if not pca else '<h3>The same five modern-use models using PCA scores</h3>'
    s += f'<p class="model-meta">All models: <b>{au["common_n"]:,} women with original SURGO FPN2 = 1</b>, including {au["common_events"]:,} modern users across {au["districts"]} departments. Outcome: current modern use = 1; confirmed traditional-only use or nonuse = 0. Adjust for age + age², Côte d’Ivoire-specific education, partnership, fertility goal and Rural/Urban residence. Use WEIGHT, department-clustered CR2/Satterthwaite intervals, and no random effects or interactions.</p>'
    if not pca:
        s += table(['Score assigned to each woman','Construction','Observed model variation'],[
            ['Demand risk score','Standardize her own six demand responses using weighted item means and population SDs among valid FPN2 = 1 women; average all six. Her department or residence does not determine this score.',f'{au["demand_distinct_values"]} distinct values across {au["common_n"]:,} women.'],
            ['Supply risk score','Convert stockout burden to continuity = 1 − burden; standardize her six favorable department-wide measures against the common complete-supply reference means/SDs. Average and reverse the sign: higher = weaker provision.',f'{au["supply_distinct_values"]} distinct numeric scores across {au["assigned_context_profiles"]} department-equivalent areas; no geographic fallback is applied.']])
    s += table(['Model','Predictors alongside the same covariates'],[
        ['1 · Demand score','Personal demand score'],['2 · Supply score','Assigned supply risk score'],
        ['3 · Both scores','Personal demand score + assigned supply risk score'],
        ['4 · Six demand indicators','Each woman’s six demand indicators separately'],
        ['5 · Six indicators + supply','Six personal demand indicators + assigned supply risk score']])
    s += figure(folder+'/01_score_models.svg','Adjusted odds ratios for demand-only, supply-only and joint modern-use models, with 95% department-clustered confidence intervals.')
    s += note(f'<strong>Key finding.</strong> In Model 3, the demand odds ratio is <b>{ort(co["score_joint","demand_score"])}</b> and the supply score odds ratio <b>{ort(co["score_joint","supply_risk"])}</b> per {units}. These are adjusted associations on the same women. Read the intervals against 1 and consult the joint tests below; overlap with 1 means the direction is not resolved at that confidence level. These do not allocate a percentage of nonuse to either side.')
    s += figure(folder+'/02_score_probabilities.svg','Weighted average adjusted modern-use probabilities across observed demand and assigned supply score ranges.')
    s += note('<strong>Planning interpretation.</strong> The curves show how predicted modern use varies with each score for the same women, holding other predictors at their observed values. Their levels and uncertainty are easier to interpret than odds ratios. They are adjusted cross-sectional comparisons, not expected gains from removing barriers. An uncertain supply slope does not establish adequate services.')
    s += '<h3>Which reported concerns accompany lower use?</h3><p class="model-note">Models 4 and 5 retain personal responses separately. Model 5 also includes the same contextual supply score. The supply-only model is Model 2, shown again for reference.</p>'
    s += figure(folder+'/03_component_models.svg','Individual reported concerns and assigned supply risk alongside covariates in modern-use models.')
    s += note(f'<strong>Key finding.</strong> In Model 5, opposition to family planning has OR <b>{ort(co["components_joint","opposes"])}</b>; health beliefs have OR <b>{ort(co["components_joint","health"])}</b>; low method confidence has OR <b>{ort(co["components_joint","confidence"])}</b>. These individual intervals are pointwise. The six-indicator joint test has Holm p {fp(tests["components_joint","Demand"]["holm_p"])} with only {tests["components_joint","Demand"]["df_denom"]:.2f} effective denominator degrees of freedom, so its joint evidence must be assessed alongside the effective degrees of freedom. Use the heatmap to locate these concerns alongside service weaknesses; these associations do not establish which intervention would be most effective.')
    s += figure(folder+'/04_component_probability_gaps.svg','Adjusted modern-use percentage-point differences for each concern, present versus absent, with paired confidence intervals.')
    s += note(f'<strong>What this means for planning.</strong> The adjusted present-versus-absent difference is <b>{100*gaps["opposes"]["estimate"]:.1f} percentage points</b> for opposition and <b>{100*gaps["health"]["estimate"]:.1f} points</b> for health beliefs. The model predicts the same women under each response setting, retains their supply and other characteristics, and averages using WEIGHT. These gaps identify patterns to investigate; they are not intervention-effect estimates.')
    coefrows = [[esc(local_labels[k])]+[ort(co[mid,k]) if (mid,k) in co else 'Not included' for mid in ['components_demand','supply','components_joint']] for k in D+['supply_risk']]
    probrows = [[esc(local_labels[k]),pct(100*gaps[k]['probability_absent']),pct(100*gaps[k]['probability_present']),f'{100*gaps[k]["estimate"]:+.1f} ({100*gaps[k]["lo"]:+.1f} to {100*gaps[k]["hi"]:+.1f})'] for k in D]
    testrows = [[label,b,str(tests[mid,b]['df_num']),f"{tests[mid,b]['df_denom']:.2f}",fp(tests[mid,b]['p']),fp(tests[mid,b]['holm_p'])] for mid,label in [('score_joint','Both scores'),('components_joint','Six indicators + supply')] for b in ['Demand','Supply']]
    if mixed:
        s += note('<strong>PCA interpretation.</strong> Supply PC1 has mixed signed coefficients despite favorable input alignment; inspect the updated loadings below. Its negative is labeled a supply pattern score. It is not a monotonic supply-risk index, and its association cannot be interpreted as the effect of worsening every service domain.')
    elif not pca:
        s += note(f'<strong>Interpret this item cautiously.</strong> Healthcare permission barriers have OR {ort(co["components_joint","agency"])} and an adjusted difference of {100*gaps["agency"]["estimate"]:+.1f} points. This does not mean restrictions improve use: the item concerns general healthcare permission, and confounding, selection and reverse direction remain possible.')
    s += '<details class="model-detail"><summary>Exact estimates, adjusted probabilities and joint tests</summary>'
    s += table(['Predictor','Six indicators','Supply only','Six indicators + supply · OR (95% CI)'],coefrows)
    s += table(['Concern','Predicted use: absent','Predicted use: present','Difference, pp (95% CI)'],probrows)
    s += table(['Joint model','Block','Restrictions','Effective denominator df','Raw p','Holm p'],testrows)
    s += '<p>Each joint model has two tests. Demand tests its one score coefficient or all six concern coefficients together; supply tests its one score coefficient. Holm correction applies within those two tests per model. These tests do not establish whether demand matters more than supply, equality between them, or causal binding constraints.</p>'
    s += downloads([(folder+'/model_coefficients.csv','All coefficients, including covariates'),(folder+'/model_block_tests.csv','Joint tests'),(folder+'/score_predicted_probabilities.csv','Probability curves'),(folder+'/individual_demand_probability_gaps.csv','Predicted probabilities and differences')])+'</details>'
    return s

css = (Path(__file__).resolve().parents[3]/'analysis/common/report.css').read_text() + '''
body{margin:0;background:#f5f8fa;color:#233e4e;font-family:Arial,Helvetica,sans-serif}main{max-width:1480px;margin:auto;padding:0 30px}.report-header{max-width:1420px;margin:32px auto 24px;padding:34px 36px;background:#143e51;color:white;border-radius:14px}.report-header h1{font-size:38px;line-height:1.12;margin:8px 0 16px;color:white}.report-header p{max-width:1110px;color:#e0edf2;line-height:1.65}.report-header .eyebrow{color:#b3dcdd}.report-header nav{display:flex;flex-wrap:wrap;gap:9px;margin-top:22px}.report-header nav a{color:white;border:1px solid #71909e;border-radius:20px;padding:7px 13px;text-decoration:none;font-size:13px}section{margin:26px 0;padding:30px;background:white;border-radius:12px;border:1px solid #dce6ea}h2{font-size:28px;line-height:1.25}h3{color:#164b5b;margin:28px 0 13px}.eyebrow{letter-spacing:.1em;font-size:12px;font-weight:700;color:#24717c}.intro,.model-note{line-height:1.65}.metrics{display:grid;grid-template-columns:repeat(4,1fr);gap:16px;margin:22px 0}.metric{padding:18px;border:1px solid #dce6ea;border-radius:9px}.metric b{display:block;font-size:29px;color:#125967}.metric span{font-size:13px;color:#677f8c}.report-warning{background:#fff8ed;border-left:4px solid #c48035;padding:16px 18px;line-height:1.65;border-radius:3px;font-size:14px}.mapbox{background:#f8fbfc;border:1px solid #e0e9ec;border-radius:9px;overflow:hidden}.country-map{display:block;width:100%;height:auto}.district{cursor:pointer}.district:hover,.district:focus{filter:brightness(.85);stroke:#173b50;stroke-width:2;outline:none}.facility-dot{cursor:pointer}.map-label{font-size:11.5px;fill:#203d4d;font-weight:600;paint-order:stroke;stroke:white;stroke-width:3;stroke-linejoin:round}.inset-heading{font-size:16px;fill:#174758;font-weight:700}.map-caption{font-size:12px;fill:#64808b}.legend{display:flex;flex-wrap:wrap;gap:17px;margin:15px 0;font-size:13px}.legend i{display:inline-block;width:14px;height:14px;border-radius:3px;margin-right:6px;vertical-align:middle}.bar-chart{display:block;width:100%}.bar-label{font-size:15px;fill:#244253;font-weight:600}.bar-label.sub{font-size:13px;font-weight:400}.bar-value{font-size:14px;font-weight:600;fill:#203d4c}.axis-label{font-size:13px;fill:#647d8a}.paired-heatmap{width:100%;min-width:1200px;border-collapse:separate;border-spacing:4px;font-size:13px}.paired-heatmap th{padding:10px 4px;text-align:center;vertical-align:bottom;font-size:12px}.paired-heatmap tbody th{text-align:left;min-width:205px;font-size:13px}.paired-heatmap small{display:block;font-size:10px;font-weight:400;color:#6e8793;margin-top:4px}.paired-heatmap td{text-align:center;font-size:14px;font-weight:700;padding:14px 5px;border-radius:2px}.demand-group{color:#087c88;background:#eef8f8}.supply-group{color:#b66b20;background:#fff6e8}.overall th{color:#086d78}.overall td{outline:1px solid #234a5c}.unknown-cell{background:#edf0f3;color:#7d8d96}.table-wrap{overflow:auto}.planning{width:100%;border-collapse:collapse}.planning th,.planning td{padding:12px 11px;vertical-align:top;text-align:left;line-height:1.5;border-bottom:1px solid #e4ecef}.planning thead{background:#f0f6f8}.planning th,.planning td{font-size:13px}.planning td ul{margin:0;padding-left:18px}.planning td li{margin:0 0 5px}.sample-badge{display:block;margin-top:5px;font-size:10px;font-weight:600;color:#98581a}.category-tag{display:inline-block;border-radius:4px;padding:5px 8px;white-space:nowrap;font-size:12px;font-weight:700}.model-figure{margin:23px 0 8px}.model-figure img{display:block;width:100%;height:auto}.model-meta{font-size:14px;line-height:1.7}.model-key{margin:10px 0 24px;background:#edf7f7;padding:15px 18px;border-left:4px solid #148591;font-size:14px;line-height:1.7}.model-detail{margin:18px 0;border:1px solid #dce7eb;border-radius:7px;padding:14px 17px}.model-detail summary{font-weight:700;color:#164b5b;cursor:pointer}.model-detail p{font-size:13px;line-height:1.7}.table-heading{display:flex;flex-wrap:wrap;gap:13px;margin:15px 0}.download{font-size:12px}.footnote{font-size:12px;color:#637d8b;line-height:1.7}.score-steps{font-size:14px;line-height:1.7;padding-left:22px}.score-steps li{margin:10px 0}.wide-break{height:1px;background:#dce7eb;margin:30px 0}.tooltip{display:none;position:fixed;background:#173d50;color:white;border-radius:9px;padding:16px;width:300px;z-index:1000;box-shadow:0 9px 35px #173d5040;font-size:13px;pointer-events:none}.tooltip.show{display:block}.tooltip strong{display:block;font-size:17px;color:white;margin-bottom:7px}.tooltip p{margin:7px 0;color:#e0edf1}.tooltip .two{display:flex;justify-content:space-between;margin:12px 0}.tooltip b{font-size:20px}.tooltip small{color:#c8dbe3}.tooltip .sample-alert{background:#fff0cb;color:#5c421b;padding:8px;border-radius:4px}@media(max-width:850px){main{padding:0 10px}section{padding:18px}.report-header{margin:12px;padding:24px}.report-header h1{font-size:28px}.metrics{grid-template-columns:repeat(2,1fr)}}@media print{body{background:white}.report-header,section{border-radius:0}details{display:block}nav,.download{display:none}.mapbox,.model-figure{break-inside:avoid}}
'''
demand_defs = [
 ['Healthcare permission barriers','HS7C: 1 = permission to see a doctor is a challenge; 2 = no. General healthcare permission, not FP-specific partner approval.'],
 ['Anticipated social stigma','PB7B/C: 1/2 on either item about being seen or what others would say. Zero requires both valid 3/4 responses.'],
 ['Health / side-effect beliefs','PB2A only: 1/2 indicates perceived health problems; 3/4 indicates no concern; other responses remain unresolved. PB2B/C are not components. This is a belief, not personally experienced side effects.'],
 ['Future-fertility concerns','MP4_a–f reversible-method concerns, requiring coherent MP4_a–i 0/1 selections, no none-plus-method contradiction, an explicit selection or none, and MP4_99 = 0. Sterilization items MP4_g/h do not create a concern.'],
 ['Low method knowledge · retained label','MC5A/B: low confidence choosing a suitable method or handling side effects/switching (3/4 on either). This is confidence/self-efficacy, not objective knowledge. Zero requires both valid 1/2 answers.'],
 ['Opposes family planning','FP1: against (3/4) versus in favor (1/2). Unfamiliarity (5), no opinion (99) and other special responses remain unresolved.']]
supply_defs = [
 ['Short + long-acting offering','Mean of UoM one_larc_3_sa_offer, joined as share_offer_larc_and_3_sa: share meeting the original ≥1 long-acting plus ≥3 short-acting product/category rule.'],
 ['Free-product provision','Mean of valid facility p_free_prod, joined as mean_p_free_prod. A value of 60% means the average reporting facility provides 60% of its recorded products free; not that 60% of facilities offer any free method.'],
 ['Trained-provider share','Mean of valid facility p_train proportions, using original UoM personnel/topic eligibility. Not the percentage of facilities with at least one trained provider.'],
 ['Daily provision of offered modern methods','Use s5_2a, b2m/b3m, c, d, e, f, g3a/g5a. Count seven categories: pills, injectables, male/female condoms, emergency contraception, IUD and implants. Combine injectable/implant variants using the most frequent recorded offered variant, counting each method class once. Within each FP facility, Daily categories (code 1) / categories recorded as Daily, Weekly, Bi-monthly or Monthly (codes 1–4). Not at all (5) is excluded from the offered denominator; -9/unknown is not zero. A zero denominator is undefined. Average valid facility ratios equally. This is reported frequency, not proof of daily access or clinical adequacy.'],
 ['At least one LARC in stock','Mean of one_larc_stockin, joined as share_stocked_1plus_larc. The country-wide three-LARC indicator is constant FALSE and cannot be standardized; this existing joined alternative is explicitly used.'],
 ['Stockout burden','Average original cleaned p_any_stockout among confirmed FP providers. It is the average facility product-group share classified as having current or recent stockouts under UoM coding, not the percentage of facilities with a stockout. Retain the supplied definition. Higher raw burden = worse; use 1 − burden as continuity before scoring/PCA. Numerical completeness does not establish complete raw responses.']]
question_text = dict(HS7C='Is getting permission to see a doctor usually a challenge?',PB7B='How afraid would you be of someone you know seeing you at a facility for FP services?',PB7C='How worried would you be about what people would say if they learned you used FP services?',PB2A='How likely are women using methods such as implants or injectables to develop health problems?',PB2B='How likely are women using these methods to experience reduced sexual desire?',PB2C='How likely are women using these methods to have babies with birth defects?',MP4_99='Which methods could affect later childbearing after stopping? MP4_a–f record reversible methods; MP4_99 records “don’t know”.',MC5A='How confident are you that you could choose a suitable FP method?',MC5B='How confident are you that you could manage bothersome side effects or switch methods?',FP1='Are you in favor of or against FP to delay or avoid pregnancy?')
unknown = list(csv.DictReader((root/'checks/demand_unresolved_summary.csv').open()))
unknown_rows = [[esc(labels[q['variable']]),q['n_unresolved'],pct(float(q['weighted_pct']))] for q in unknown]
items = list(csv.DictReader((root/'checks/unresolved_item_responses.csv').open()))
question_rows = [[esc(q['variable']),esc(question_text[q['variable']]),q['blank_n'],q['n'],pct(float(q['weighted_pct']))] for q in items]
flow = list(csv.DictReader((root/'checks/model_sample_flow.csv').open()))
flow_rows = [[esc(q['step']),q['excluded_at_step'],q['remaining']] for q in flow]
sample_rows = [[q['model'],str(q['n']),str(q['events']),str(q['parameters']),f'{q["max_vif"]:.2f}'] for q in m['samples']]
sample_compare = list(csv.DictReader((root/'checks/model_sample_comparison.csv').open()))
comparison_samples = [[esc(q['group']),q['n'],pct(100*float(q['weighted_share_of_need'])),pct(100*float(q['modern_use_among_known'])),pct(100*float(q['urban_share']))] for q in sample_compare]
den_rows = [[area_label(g),str(cov[g].get('fp_facilities',0))]+[str(hm[g,'Supply',k]['n_valid']) for k in S] for g in areas]
correlations = list(csv.DictReader((root/'checks/component_correlations.csv').open()))
cor_rows=[]
for side in ['Demand','Supply']:
    pairs=sorted([q for q in correlations if q['side']==side and q['item1']<q['item2']],key=lambda q:abs(float(q['correlation'])),reverse=True)[:3]
    cor_rows += [[side,esc(labels[q['item1']]),esc(labels[q['item2']]),f'{float(q["correlation"]):+.3f}',q['n_pair']] for q in pairs]
weight_concentration = list(csv.DictReader((root/'checks/model_weight_concentration.csv').open()))
weight_rows = [[esc(name(q['geo'])),q['n'],pct(100*float(q['weight_share']))] for q in weight_concentration[:6]]

num=lambda v:'—' if v is None else f'{v:+.3f}'
pair=lambda a,b:f'{pct(a)} → <b>{pct(b)}</b>'
threshold=cuts['zscore']
counts={k:sum(scores['zscore',g]['category']==k for g in areas) for k in ['both','demand','supply','neither','unassessed']}
legend='<div class="legend">'+''.join(f'<span><i style="background:{colors[k]}"></i>{catnames[k]}: <b>{counts[k]}</b></span>' for k in counts)+'</div>'
coverage_legend='<div class="legend">'+''.join(f'<span><i style="background:{coverage_colors[k]}"></i>{coverage_labels[k]}: <b>{sum(v["coverage_group"]==k for v in DATA.values())}</b></span>' for k in coverage_colors)+'</div>'
comparison_rows=[[area_label(g)]+[tag(scores[k,g]['category']) for k in methods] for g in areas]
method_rows=[['Rank average','Rank each component across departments and average six ranks.','Separate score medians; average ranks for ties.'],['Mean percentages','Average six original percentages on each side.','Separate score medians.'],['Median/IQR','Subtract the component median, divide by its IQR and average.','Separate score medians.'],['Median count','Count unfavorable component comparisons with their medians.','Flag four or more of six.'],['Z-score average · main','Subtract component mean, divide by sample SD and average.','Demand > 0; favorable supply readiness < 0. Zero is the final-score mean within each stated reference set.']]

def priority_frequency(category, keys):
    group = [r for r in planning.values() if r[category+'_flag'] is True]
    frequency = Counter(k for r in group for k in r['top_' + category])
    ordered = sorted(keys, key=lambda k: (-frequency[k], keys.index(k)))
    rows = [[esc(planning_labels[k]), f'{frequency[k]} of {len(group)}',
             pct(100 * frequency[k] / len(group)) if group else '—'] for k in ordered]
    return len(group), table(['Component', 'Departments with this among their top three',
                             'Share of this group'], rows)

supply_group_n, supply_frequency_table = priority_frequency('supply', S)
demand_group_n, demand_frequency_table = priority_frequency('demand', D)
priority_summaries = f'''<div id="component-summaries"><h3>Which service weaknesses and reported concerns recur across departments?</h3><p>Count how often each component appears among a department’s three selected components: the lowest standardized provision components for supply, and the highest standardized concern components for demand. These are counts of departments, not percentages of women or estimated causes of nonuse.</p><div class="component-summary-grid"><article id="supply-component-summary"><h4>Service weaknesses · {supply_group_n} supply-flagged departments</h4>{supply_frequency_table}</article><article id="demand-component-summary"><h4>Reported concerns · {demand_group_n} demand-flagged departments</h4>{demand_frequency_table}</article></div><p class="footnote">Both-flagged departments are included on each flagged side, so groups overlap. Demand totals also include known concern flags in supply-unassessed areas; those areas are not classified as demand-only. A department contributes to three components, so percentages do not sum to 100%. These are relative component patterns, not service-standard failures. Stockout burden is higher = worse; other raw supply measures are higher = better. These rules are reversed as needed before standardized component ordering.</p><p class="footnote">Short- and long-acting options together means at least one long-acting and three short-acting categories at the same facility. Free provision and training average within-facility proportions; they are not shares of facilities offering any free product or having any trained provider. The confidence indicator is unchanged: it measures confidence choosing a method or managing side effects, not objective knowledge.</p></div>'''

def department_priority_table(category):
    side = category.title()
    rows = []
    for g in areas:
        q = planning[g]
        if q[category+'_flag'] is not True:
            continue
        cells = []
        for k in q['top_' + category]:
            r = hm[g, side, k]
            coverage = f'Valid responses: {r["n_valid"]:,} / {r["n_total"]:,}'
            cells.append(f'<span title="{esc(coverage)}">{esc(planning_labels[k])}'
                         f'<br><b>{pct(r["value"])}</b></span>')
        rows.append([area_label(g)] + cells)
    headers = ['Weakest service component', 'Second-weakest component', 'Third-weakest component'] if category == 'supply' else ['Most elevated concern', 'Second-most elevated concern', 'Third-most elevated concern']
    return table(['Department'] + headers, rows)

department_priorities = f'''<div id="supply-department-components"><h3>The {supply_group_n} supply-flagged departments · three weakest service components</h3><p>The supply columns run from the lowest to the third-lowest favorable-supply component z-score. Displayed percentages are original measures: higher stockout burden indicates weaker provision; lower values indicate weaker provision for the other five components. Ordering is based on standardized scores, not the raw percentages.</p>{department_priority_table('supply')}<p class="footnote">Facilities contribute equally; each measure uses its own valid-response denominator. Free provision and training are averages of within-facility proportions. Hover a value for response coverage. Daily provision describes the frequency of recorded offered methods; keep method breadth separate. Cleaned stockout burden retains its documented upstream group definitions.</p></div><div id="demand-department-components"><h3>The {demand_group_n} demand-flagged departments · three most elevated reported concerns</h3><p>The concern columns run from the highest to the third-highest demand component z-score. Displayed percentages are weighted shares of women with original SURGO FPN2 = 1 who report each concern, among valid answers. Ordering is based on standardized scores, not the raw percentages.</p>{department_priority_table('demand')}<p class="footnote">Each concern uses its own valid-response denominator; unanswered or unresolved items are not coded as no concern. Hover a value for response coverage. † Fewer than 50 women in need in the department; subgroup percentages may be imprecise. Both-flagged departments appear in both relevant component lists and in the complete planning table below.</p></div>'''
css += '''.component-summary-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:24px}.component-summary-grid article{min-width:0;border:1px solid #dce6ea;border-radius:8px;overflow:hidden}.component-summary-grid h4{margin:0;padding:16px 13px;line-height:1.5;font-size:15px}#supply-component-summary h4{color:#a75a18;background:#fff6e8}#demand-component-summary h4{color:#087c88;background:#eef8f8}.component-summary-grid .planning th,.component-summary-grid .planning td{font-size:12px}@media(max-width:1000px){.component-summary-grid{grid-template-columns:1fr}}'''
css += '''#supply-department-components .planning thead{background:#fff6e8}#demand-department-components .planning thead{background:#eef8f8}#supply-department-components h3{color:#a75a18}#demand-department-components h3{color:#087c88}'''
priority_rows=[]
for g in areas:
    q=planning[g]
    flag=lambda v:'Unassessed' if v is None else 'Yes' if v else 'No'
    tops=lambda side,ks:'<ul>'+''.join(f'<li>{esc(planning_labels[k])} <b>{pct(hm[g,side,k]["value"])}</b></li>' for k in ks)+'</ul>'
    priority_rows.append([area_label(g),flag(q['demand_flag']),tops('Demand',q['top_demand']) if q['demand_flag'] else '—',flag(q['supply_flag']),tops('Supply',q['top_supply']) if q['supply_flag'] else 'Incomplete inputs' if q['supply_flag'] is None else '—'])
header='''<header class="report-header"><span class="eyebrow">SURGO × UNIVERSITY OF MANITOBA · CÔTE D’IVOIRE</span><h1>Demand, services and modern contraceptive use</h1><p><strong>Updated FP-provider-only department supply assignment.</strong> Each woman receives the recorded facility averages for her own department-equivalent area. Eligibility uses s2_5_sp = 1 for FP services generally; it does not additionally require on-site provision (s5_1). Daily provision replaces counseling, cleaned stockout burden replaces popular-product stock, and health beliefs use PB2A only. Main classifications use zero-centered average z-scores. Demand indicators remain personal in models; Rural/Urban residence remains a covariate.</p><p>Joint evidence helps identify where reported concerns coincide with service weaknesses. Associations and relative planning categories do not establish causal binding constraints or budget allocations.</p><nav><a href="#coverage">1 · Coverage</a><a href="#use">2 · Use and intention</a><a href="#joint">3 · Joint conditions</a><a href="#models">4 · Models</a><a href="#definitions">Definitions</a><a href="#appendix-scoring">Appendix · Scoring</a><a href="#appendix-stock">Appendix · Stock data</a><a href="#pca">Appendix · PCA</a></nav></header>'''
unmatched_rows=[[esc(name(g)),str(assign[g]['source_women']),str(assign[g]['need_women'])] for g in areas if assign[g]['unmatched_women']]
coverage=f'''<section id="coverage"><span class="eyebrow">01 · COVERAGE AND LINKAGE</span><h2>Women and facilities overlap in 78 department-equivalent areas</h2><p>The join retains <b>10,859 source records</b>; 10,856 have geography. The map shows all geographically identified women before selecting family-planning need. Manitoba has <b>3,078 facilities in 100 areas</b>, including 2,712 facilities in the 78 areas shared with SURGO.</p><div class="metrics"><div class="metric"><b>10,859</b><span>Source records retained</span></div><div class="metric"><b>78 / 82</b><span>Surveyed areas with facilities</span></div><div class="metric"><b>2,712</b><span>Facilities in linked areas</span></div><div class="metric"><b>4,292</b><span>Original FPN2 = 1 women</span></div></div>{coverage_legend}{map_svg('coverage')}<p class="footnote">Dots show department totals, not facility coordinates. There are 22 facility-only areas and seven display areas with neither source. Archived <a href="https://data.humdata.org/dataset/cod-ab-civ">CNTIG/OCHA boundaries</a> include documented, area-conserving display updates for Bonon, Gohitafla and Ouelle; polygons do not assign women or facilities.</p><h3>How each woman receives supply measures</h3>{table(['Woman','Supply assigned'],[['Department A · Rural','Confirmed FP providers (s2_5_sp = 1) in Department A, with valid denominators per measure.'],['Department A · Urban','The same Department A profile.'],['Department B · Rural or Urban','Department B’s profile.']])}<p>The checked health-district crosswalk and documented Abidjan, Bouake, Gagnoa and Korhogo rollups are retained. The coverage map retains all source facilities; subsequent supply summaries use 2,370 confirmed FP providers in the linked areas, without facility weights. This is a contextual environment, not a link to the facility attended. No residence-specific profile or geographic fallback is used.</p><div class="report-warning">Bonon, Facobly, Gohitafla and Taabo contain <b>153 geographically identified source women without facility matches, including 49 in need</b>. Three further records lack geography, consent and weight. All records remain in the join. An unmatched area does not establish absence of services.</div>{table(['Department','Source women','Women in need'],unmatched_rows)}{downloads([('department_data_coverage.csv','Coverage'),('assigned_supply_profiles.csv','Assignments'),('boundary_update_checks.csv','Boundary checks')])}</section>'''
use=f'''<section id="use"><span class="eyebrow">02 · USE, INTENTION AND METHOD CHOICE</span><h2>Among women SURGO classifies as in need of family planning</h2><div class="report-warning">From this section onward: original <b>FPN2 = 1</b>, with consent and WEIGHT, gives <b>4,292 women</b>. No historical need_n reconstruction or permanent-protection additions are applied.</div>{bar_chart()}<p class="footnote">All bars use 4,292 women in need as the denominator; subgroups overlap. Percentages use WEIGHT; n is unweighted. Capped whiskers use recorded IDZD census-district clusters, with the need subgroup selected after constructing the full consenting positive-weight design. Original strata and FPC are unavailable. Preference concerns the wanted method at initiation; intention concerns any method without a specified future initiation window.</p>{note(f'<strong>Initial pattern.</strong> Current use is {100*bars["user"]["estimate"]:.1f}%, including {100*bars["modern"]["estimate"]:.1f}% modern use. Nonusers with intention account for {100*bars["intender"]["estimate"]:.1f}%, and nonusers without stated intention {100*bars["no_intent"]["estimate"]:.1f}%. These are concurrent groups, not observed transitions.')}{downloads([('use_distribution.csv','Use distribution'),('department_use_rates.csv','Department CPR and mCPR')])}</section>'''
stock_warning='<div class="report-warning"><strong>Supply profiles remain incomplete in five surveyed areas.</strong> Bonon, Facobly, Gohitafla and Taabo have no linked facility records. Daoukro has general FP providers but undefined free-product and daily-provision proportions. They remain supply-unassessed; missing values are not zeros. The stock appendix documents the retained UoM stockout definition.</div>'
joint=f'''<section id="joint"><span class="eyebrow">03 · JOINT CONDITIONS, SERVICE WEAKNESSES AND REPORTED CONCERNS</span><h2>Women’s concerns alongside recorded services</h2><p>Six weighted demand percentages among women in need appear on the left; six unweighted FP-provider measures, including daily provision and stockout burden, appear on the right. Planning summaries and model supply exposures both use entire departments.</p>{heatmap()}<p class="footnote">Darker teal means more concern; darker orange means weaker provision. The five provision percentages run higher = better; stockout burden runs higher = worse. Orange intensity accounts for the difference. Hover for valid/total denominators. Missing cells are unknown, not zero. † Fewer than 50 women in need.</p>{stock_warning}<details open class="model-detail"><summary>Main score and classification</summary><ol class="score-steps"><li>Calculate six concern prevalences using valid-domain answers and WEIGHT; restrict facilities to raw s2_5_sp = 1 and average valid facility values separately by measure, without facility weights. Valid zeros/FALSE remain included.</li><li>Align supply direction first: stock continuity = 1 − stockout burden; other five inputs remain favorable. Standardize each component against the mean and sample SD of its side’s complete-department reference set. Departments contribute equally.</li><li>Average six standardized components separately on each side. A missing component prevents a complete score. Higher demand means more concern; higher favorable supply means stronger provision.</li><li>Flag demand strictly above <b>{threshold['demand_cutoff']:.6f}</b>; favorable supply strictly below <b>{threshold['supply_cutoff']:.6f}</b>. Both cutoffs equal zero, the final-score mean. Ties are unflagged; model supply risk is negative readiness. Scores are averages of six standardized components, not percentage-point differences.</li></ol><p>Demand references use 82 departments with all six demand percentages; supply references use the same 77 complete departments for every supply component. Thus each final score has mean zero over its own stated reference set. These are equal-department references, not the overall weighted-woman or pooled-facility heatmap row. The four unmatched areas and Daoukro’s undefined free-product and daily-provision measures remain supply-unassessed. Categories are relative planning signals.</p></details>{legend}{map_svg('classification')}{note(f'<strong>Planning signal.</strong> {counts["both"]} areas have both sides flagged, {counts["demand"]} demand only, {counts["supply"]} supply only, {counts["neither"]} neither and {counts["unassessed"]} supply-unassessed. Investigate component comparisons and response coverage before selecting an intervention.')}{priority_summaries}{department_priorities}<h3>Department service weaknesses and reported concerns</h3>{table(['Department','Demand challenges','Top three reported concerns','Supply challenges','Top three service weaknesses'],priority_rows)}<p class="footnote">Component lists use the highest demand and lowest favorable-supply component z-scores, not estimated effects on nonuse. Neither flag does not establish adequate services or absence of barriers.</p>{downloads([('planning_priorities.csv','Service weaknesses and reported concerns'),('component_score_trace.csv','Score calculations'),('department_sample_flags.csv','Sample flags')])}</section>'''
model='<section id="models"><span class="eyebrow">04 · ADJUSTED ASSOCIATIONS</span>'+models(m,'models_zscore')+'</section>'
missing=list(csv.DictReader((root/'models_zscore/model_missingness.csv').open()))
missing_rows=[[esc(labels.get(r['variable'],r['variable'])),r['missing_in_need']] for r in missing if int(r['missing_in_need'])]
definitions=f'''<section id="definitions"><span class="eyebrow">DEFINITIONS AND VERIFICATION</span><h2>Measures, denominators and interpretation</h2><details class="model-detail"><summary>Demand domains and unknown responses</summary>{table(['Domain','Question and coding'],demand_defs)}<p>A known positive establishes an any-concern flag; zero requires valid negatives on every included item. Remaining combinations stay unresolved. These questions are not restricted to married women or nonusers. The retained knowledge label measures subjective confidence.</p>{table(['Item','Shortened question','Blank','Unclassifiable','Weighted percentage'],question_rows)}{table(['Domain','Unresolved women','Weighted unresolved share'],unknown_rows)}</details><details open class="model-detail"><summary>Supply definitions and response coverage</summary>{table(['Measure','Calculation and interpretation'],supply_defs)}<p>Valid zero/FALSE values are included; unknown and undefined values are excluded from each measure’s denominator. Within-facility proportions are averaged equally, rather than replaced by pooled product/personnel ratios. All averages restrict to raw <b>s2_5_sp = 1</b> (general FP provision, not an on-site requirement): <b>{FP_LINKED:,} confirmed FP providers</b> in 78 surveyed departments. Daily provision has {DAILY_VALID:,} defined facility ratios and {DAILY_UNDEFINED} undefined. Original cleaned stockout burden has {AUD["stockout_valid_linked"]:,} numeric values. Counseling and popular-product stock are not selected components.</p>{table(['Department','Confirmed FP facilities']+[esc(labels[k])+': valid' for k in S],den_rows)}<p>Daoukro has 19 general FP-service providers among 21 source records; all 19 report No to on-site provision (s5_1). Their product questions are largely Not applicable, and both free-product and daily-provision proportions are undefined. Their recorded free/nonfree product counts yield undefined p_free_prod; no zero filling or unapproved repair is applied. The other measures can still be displayed. Daily ratios are conditional on recorded offered categories, including partial method responses; higher daily provision does not by itself establish broad choice or adequate access.</p></details><details open class="model-detail"><summary>Survey design, adjustment and model sample</summary><p>All models use original WEIGHT and department-clustered CR2 covariance with Satterthwaite intervals. Recorded IDZD census districts support overview intervals and an inference sensitivity; original strata/FPC are unavailable. Adjust for age, age², education, partnership, fertility goal and Rural/Urban residence. No random effects or interactions are fitted.</p><p>The model comparison uses <b>{MA['common_n']:,} women</b>, including {MA['common_events']:,} modern users. The overview retains all 4,292 women in need. Department supply is complete for 4,200; 92 lack a complete profile. Models additionally require known demand, outcome and covariates. All five primary models and the five PCA models share the same eligible women. Health uses PB2A only; the remaining demand definitions are retained. Women lacking a complete supply profile remain in the overview but are excluded from these common models.</p>{table(['Model','Women','Modern users','Parameters','Maximum VIF'],sample_rows)}{table(['Sample-selection step','Excluded here','Women remaining'],flow_rows)}{table(['Group','Women','Share of need-group weight','Modern use among known outcomes','Urban share'],comparison_samples)}{table(['Variable','Missing/unresolved women'],missing_rows)}<p>There are {MA['supply_distinct_values']} supply scores across {MA['districts']} modeled departments; personal demand varies within departments. Intervals condition on estimated scores. VIF and convergence checks do not establish adequate power or causal identification.</p>{downloads([('models_zscore/model_vifs.csv','Weighted VIF'),('models_zscore/model_missingness.csv','Missingness'),('models_zscore/individual_demand_score_reference.csv','Demand-score references'),('models_zscore/supply_score_reference.csv','Supply-score references')])}</details><details class="model-detail"><summary>Correlation and independent verification</summary>{table(['Side','Component 1','Component 2','Correlation','Paired records'],cor_rows)}<p>Demand correlations use personal responses and WEIGHT; supply correlations use department profiles. Independent verification passed {verification['check_count']} checks, including source preservation, assignments, score calculations, coefficients, CR2 covariance, weighted-SVD PCA, FP eligibility and risk-direction equivalence.</p>{downloads([('checks/audit_checks.csv','Verification checklist'),('checks/component_correlations.csv','Correlations')])}</details></section>'''
appendix_scoring=f'''<section id="appendix-scoring"><span class="eyebrow">APPENDIX A · SCORING SENSITIVITY</span><h2>Classification under five scoring approaches</h2>{table(['Department']+method_names,comparison_rows)}{table(['Method','Construction','Threshold'],method_rows)}<p>Missing components remain unassessed; no partial score is substituted. Reverse burden to continuity before every scoring method. References use complete departments consistently within each side. The primary z-score cutoff is zero; rank, percentage and Median/IQR alternatives retain separate final-score medians. These equal-weight comparisons are relative to observed departments, not absolute service standards.</p>{downloads([('classification_comparison.csv','Classifications'),('classification_thresholds.csv','Thresholds'),('all_method_scores_and_classifications.csv','Scores')])}</section>'''
appendix_stock=f'''<section id="appendix-stock"><span class="eyebrow">APPENDIX B · STOCK MEASURE AND SOURCE QUALIFICATIONS</span><h2>How stockout burden contributes to the supply score</h2><p>The selected measure is the mean of cleaned <b>p_any_stockout</b> among FP providers: {AUD['stockout_valid_linked']:,} numeric records in the linked departments. A value of 30% means the average facility has 30% of its assessed product groups classified as having current or recent stockouts under the UoM definition. It is not the percentage of facilities with any stockout, and 70% continuity is not simply 70% current availability.</p><p>Before scoring, continuity = 1 − burden. Standardize continuity alongside the five other favorable measures; average all six for readiness. The negative is supply risk. Greater burden therefore lowers readiness and raises risk.</p><p>Original cleaned values and product-group definitions are retained and verified against the supplied UoM export. Numeric completeness does not prove complete raw responses. The existing CDI extraction uses lettered stock items and does not include the unlettered male-condom current-stock question; unknown-response rules also affect derived classifications. The indicator describes assessed groups in the supplied code, rather than a newly verified all-method stock measure. No upstream repair is silently introduced.</p>{downloads([('service_regularity_coverage.csv','Daily-provision coverage'),('supply_input_reconciliation.csv','Independent facility aggregation checks')])}</section>'''
load_rows=[[r['side'],esc('Stock continuity (1 − stockout burden)' if r['side']=='Supply' and r['variable']=='stockout_burden' else labels[r['variable']]),f"{r['loading']:+.3f}"] for r in pc['loadings'] if r['PC']==1]
variance_rows=[[r['side'],f"PC{r['PC']}",pct(r['variance_pct']),pct(r['cumulative_pct'])] for r in pc['variance']]
negative_supply=pc['audit'].get('negative_supply_PC1_loadings',[])
if isinstance(negative_supply,str):negative_supply=[negative_supply]
pca_sign_note=('Supply PC1 has mixed signs: '+', '.join('Stock continuity' if k=='stockout_burden' else labels[k] for k in negative_supply)+' load negatively. Its negative is a pattern score, not uniformly worse provision.' if negative_supply else 'Supply PC1 is oriented toward stronger provision and negated for modeling.')
supply_pc1=next(r for r in pc['variance'] if r['side']=='Supply' and r['PC']==1)
pca_reference_note=('Supply PC1 does not exceed the exploratory 95th-percentile permutation reference; it remains a prespecified sensitivity score, not a validated one-dimensional supply scale.' if not supply_pc1['above_permutation_95'] else 'The permutation reference is exploratory and does not validate a causal supply scale.')
appendix_pca=f'''<section id="pca"><span class="eyebrow">APPENDIX C · PCA AND MODEL ROBUSTNESS</span><h2>Alternative scores from the same personal and department inputs</h2><p>Separate weighted PCAs use six personal demand and six department supply components, including daily provision and favorable stock continuity. Demand PCA uses {pc['audit']['demand_PCA_n']:,} women; supply uses {pc['audit']['supply_PCA_women_n']:,} women across {pc['audit']['supply_profiles']} complete department profiles, weighted by represented women’s summed WEIGHT.</p>{figure('pca_appendix/01_pca_loadings.svg','Signed PCA coefficients for personal demand and department supply.')}{note(f'<strong>Interpretation.</strong> Demand PC1 captures {pc["audit"]["demand_PC1_variance"]:.1f}% of input variation; supply PC1 {pc["audit"]["supply_PC1_variance"]:.1f}%. {pca_sign_note} {pca_reference_note} Coefficients describe input variation, not causal effects or investment decisions.')}{figure('pca_appendix/02_pca_variance.svg','PCA variance and exploratory permutation reference.')}<details class="model-detail"><summary>Coefficients and stability</summary>{table(['Side','Input','PC1 coefficient'],load_rows)}{table(['Side','Principal component','Variance retained','Cumulative'],variance_rows)}<p>Standardize with final-weighted means and population SDs, eigendecompose each correlation matrix and project onto PC1. Two thousand exploratory permutations and department-deletion checks assess construction stability, not causal or full survey-design inference.</p>{downloads([('pca_appendix/pca_loadings.csv','PCA coefficients'),('pca_appendix/pca_variance.csv','Variance checks'),('pca_appendix/pca_leave_one_department_out.csv','PCA stability')])}</details>{models(pm,'pca_appendix/models',pca=True)}<h3>Geographic leverage and uncertainty</h3><p>Unequal department contributions mean nominal geographic count does not guarantee precise inference. Effective denominator degrees of freedom are reported for every joint test. Very low degrees of freedom make a block test weak; lack of joint evidence does not establish no demand role or invalidate every pointwise association.</p>{table(['Largest weighted department contributions','Modeled women','Share of model weight'],weight_rows)}{downloads([('models_zscore/leave_one_department_out.csv','Department-deletion coefficients'),('models_zscore/sampling_cluster_model_sensitivity.csv','Sampling-cluster inference'),('checks/block_test_diagnostics.csv','Block-test effective degrees of freedom')])}</section>'''
js='''const DATA=__DATA__;const tip=document.querySelector('.tooltip');
const pct=v=>v==null?'Not available':v.toFixed(1)+'%';const val=v=>v==null?'Unassessed':v.toFixed(3);
function hide(){tip.classList.remove('show')}
function move(e){let x=e.clientX+17,y=e.clientY+17;if(x+tip.offsetWidth>innerWidth-10)x=e.clientX-tip.offsetWidth-17;if(y+tip.offsetHeight>innerHeight-10)y=e.clientY-tip.offsetHeight-17;tip.style.left=Math.max(10,x)+'px';tip.style.top=Math.max(10,y)+'px'}
function show(el,e){const d=DATA[el.dataset.key];if(!d)return;const sensitivity=el.dataset.mode==='sensitivity';const label=el.dataset.mode==='coverage'?d.coverage_label:sensitivity?d.alternative_category:d.category;tip.innerHTML='<strong>'+d.name+'</strong><p>'+label+'</p><div class="two"><div><b>'+pct(d.cpr)+'</b><br><small>Any-method use</small></div><div><b>'+pct(d.mcpr)+'</b><br><small>Modern use</small></div></div><p>'+d.n+' women with original FPN2=1.</p><p>'+d.source_rows+' source records · '+d.facilities+' facility records · '+d.fp_facilities+' confirmed FP providers</p>'+(d.demand!=null&&el.dataset.mode!=='coverage'?'<p>Demand score: '+val(d.demand)+'<br>Favorable supply: '+val(sensitivity?d.alternative_supply:d.supply)+'</p>':'')+(sensitivity&&d.complete_facilities!=null?'<p>'+d.complete_facilities+' complete facilities retained.</p>':'')+(d.small_sample?'<p class="sample-alert">Small need-group sample: n='+d.n+'. Interpret cautiously.</p>':'')+'<small>Weighted use among women in need; department-wide supply.</small>';tip.classList.add('show');move(e)}
document.querySelectorAll('[data-key][data-mode]').forEach(el=>{el.addEventListener('pointerenter',e=>show(el,e));el.addEventListener('pointermove',move);el.addEventListener('pointerleave',hide);el.addEventListener('focus',()=>{let r=el.getBoundingClientRect();show(el,{clientX:r.x+r.width/2,clientY:r.y+r.height/2})});el.addEventListener('blur',hide);el.addEventListener('click',e=>show(el,e))});document.addEventListener('keydown',e=>{if(e.key==='Escape')hide()});'''.replace('__DATA__',json.dumps(DATA,separators=(',',':')))
page='<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Côte d’Ivoire department analysis | SURGO × Manitoba</title><style>'+css+'</style></head><body>'+header+'<main>'+coverage+use+joint+model+definitions+appendix_scoring+appendix_stock+appendix_pca+'</main><div class="tooltip" role="status" aria-live="polite"></div><footer style="max-width:1430px;margin:20px auto;padding:0 30px;font-size:12px;color:#647d88">Côte d’Ivoire · department-wide supply assignment · original FPN2=1 · cross-sectional associations with stock-indicator qualifications.</footer><script>'+js+'</script></body></html>'
output.parent.mkdir(parents=True,exist_ok=True);output.write_text(french_terms(page),encoding='utf-8')
print('Rendered',output.name,'with main results, scoring comparisons, stock review and PCA.')

