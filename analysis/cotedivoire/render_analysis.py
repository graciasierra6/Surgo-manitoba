"""Render the Côte d’Ivoire hybrid-linkage analysis from verified aggregate outputs."""
from pathlib import Path
import argparse, base64, csv, html, json, math

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
AUD, MA = p['audit'], m['audit']
D, S = p['demand_variables'], p['supply_variables']
labels = dict(zip(D + S, p['demand_labels'] + p['supply_labels']))
labels.update(demand_score='Demand risk score', supply_risk='Supply risk score')
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
assert n == 4292 and model_n == 3768 and MA['common_events'] == 2164
assert len(areas) == 82 and len(layout) == len(all_areas) == 111
assert pm['audit']['common_n'] == model_n and verification['all_passed']
esc = lambda s: html.escape(str(s), quote=True)
name = lambda g: cov[g]['name'].title()
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

def download(file, title):
    data = base64.b64encode((root / file).read_bytes()).decode()
    return f'<a class="download" download="cdi_{Path(file).name}" href="data:text/csv;base64,{data}">{esc(title)} ↓</a>'

def downloads(items):
    return '<div class="table-heading">' + ''.join(download(f, t) for f, t in items) + '</div>'

def figure(file, alt):
    data = base64.b64encode((root / file).read_bytes()).decode()
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
        facilities=c['facilities'], cpr=100*r['cpr']['estimate'] if r else None,
        mcpr=100*r['mcpr']['estimate'] if r else None, category=catnames[q['category']] if q else ('Facility data only' if has_s else 'Neither source recorded'),
        demand=q['demand_score'] if q else None, supply=q['supply_score'] if q else None,
        small_sample=g in small, coverage_group=group, coverage_label=coverage_labels[group],
        supply_issue_need=sum(q['need_women'] for q in p['assignment_issues'] if q['geo']==g))
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
        category=scores['zscore',g]['category'] if g in rates else 'supply_only' if d['facilities'] else 'no_sources'
        fill=coverage_colors[d['coverage_group']] if mode=='coverage' else f'url(#unassessed-{mode})' if category=='unassessed' else colors[category]
        path=' '.join('M'+' L'.join(f'{x:.2f},{y:.2f}' for x,y in map(xy,ring))+' Z' for ring in f['rings'])
        title=d['name']+': '+(f'modern use {d["mcpr"]:.1f}%' if d['mcpr'] is not None else 'no SURGO need-group estimate')
        out.append(f'<path class="district" data-key="{g}" data-mode="{mode}" d="{path}" fill="{fill}" fill-rule="evenodd" stroke="white" stroke-width="1" tabindex="0"><title>{esc(title)}</title></path>')
        cx,cy=xy(f['center'])
        if mode=='coverage' and d['facilities']:
            radius=2.6+math.sqrt(d['facilities'])*.52
            out.append(f'<circle class="facility-dot" data-key="{g}" data-mode="{mode}" cx="{cx:.2f}" cy="{cy:.2f}" r="{radius:.2f}" fill="#123d50" fill-opacity=".62" stroke="white" stroke-width="1"><title>{d["facilities"]} facility records</title></circle>')
        if f['label']:
            label=d['name']+(' †' if mode=='classification' and d['small_sample'] else '')
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
    out += [f'<th>{esc(labels[k])}</th>' for k in D+S]
    out += ['</tr></thead><tbody>']
    for g in ['CDI']+areas:
        c = cov[g]
        title = 'Côte d’Ivoire · linked departments' if g == 'CDI' else name(g)
        mark = ' †' if g in small else ''
        out.append(f'<tr class="{"overall" if g == "CDI" else ""}"><th>{esc(title)}{mark}<small>n={c["n_need"]:,} women in need · {c["facilities"]:,} facilities</small></th>')
        for side, ks in [('Demand',D),('Supply',S)]:
            for k in ks:
                r = hm[g,side,k]; v = r['value']
                den = f'Valid responses {r["n_valid"]:,} / {r["n_total"]:,}'
                if v is None:
                    out.append(f'<td class="unknown-cell" title="{esc(den)}; unknown, not zero">—</td>'); continue
                t = v/100 if side == 'Demand' else 1-v/100
                bg = mix('#eef8f8','#047888',t) if side == 'Demand' else mix('#fff8ed','#bc570f',t)
                fg = 'white' if t > .62 else '#233d4b'
                out.append(f'<td style="background:{bg};color:{fg}" title="{esc(title+": "+labels[k]+" · "+den)}">{v:.0f}%</td>')
        out.append('</tr>')
    return ''.join(out)+'</tbody></table></div>'

def models(data, folder, pca=False):
    au = data['audit']
    local_labels=dict(labels)
    if pca:local_labels.update(demand_score='Demand score (PCA)',supply_risk='Supply pattern score (PCA)')
    co = {(r['model'],r['term']):r for r in data['coefficients']}
    tests = {(r['model'],r['block']):r for r in data['tests']}
    gaps = {r['term']:r for r in data['gaps']}
    units = 'one oriented PC1 unit' if pca else 'one original average-z index point'
    s = '<h2>Modern use in relation to personal concerns and local services</h2>' if not pca else '<h3>The same five modern-use models using PCA scores</h3>'
    s += f'<p class="model-meta">All models: <b>{au["common_n"]:,} women with original SURGO FPN2 = 1</b>, including {au["common_events"]:,} modern users across {au["districts"]} departments. Outcome: current modern use = 1; confirmed traditional-only use or nonuse = 0. Adjust for age + age², Côte d’Ivoire-specific education, partnership, fertility goal and Rural/Urban residence. Use WEIGHT, department-clustered CR2/Satterthwaite intervals, and no random effects or interactions.</p>'
    if not pca:
        s += table(['Score assigned to each woman','Construction','Observed model variation'],[
            ['Demand risk score','Standardize her own six demand responses using weighted item means and population SDs among valid FPN2 = 1 women; average all six. Her department or residence does not determine this score.',f'{au["demand_distinct_values"]} distinct values across {au["common_n"]:,} women.'],
            ['Supply risk score','Standardize her assigned six exact department-residence measures against the valid department planning means/SDs. Average and reverse the sign: higher = weaker provision.',f'{au["supply_distinct_values"]} distinct numeric scores across {au["assigned_context_profiles"]} department–residence contexts; No geographic fallback is applied.']])
    s += table(['Model','Predictors alongside the same covariates'],[
        ['1 · Demand score','Personal demand score'],['2 · Supply score','Assigned supply risk score'],
        ['3 · Both scores','Personal demand score + assigned supply risk score'],
        ['4 · Six demand indicators','Each woman’s six demand indicators separately'],
        ['5 · Six indicators + supply','Six personal demand indicators + assigned supply risk score']])
    s += figure(folder+'/01_score_models.svg','Adjusted odds ratios for demand-only, supply-only and joint modern-use models, with 95% department-clustered confidence intervals.')
    s += note(f'<strong>Key finding.</strong> In Model 3, the demand odds ratio is <b>{ort(co["score_joint","demand_score"])}</b> and the supply score odds ratio <b>{ort(co["score_joint","supply_risk"])}</b> per {units}. The demand association remains after accounting for recorded supply and covariates. The supply interval includes 1, so its independent association remains uncertain. These do not allocate a percentage of nonuse to either side.')
    s += figure(folder+'/02_score_probabilities.svg','Weighted average adjusted modern-use probabilities across observed demand and assigned supply score ranges.')
    s += note('<strong>Planning interpretation.</strong> The curves show how predicted modern use varies with each score for the same women, holding other predictors at their observed values. Their levels and uncertainty are easier to interpret than odds ratios. They are adjusted cross-sectional comparisons, not expected gains from removing barriers. An uncertain supply slope does not establish adequate services.')
    s += '<h3>Which reported concerns accompany lower use?</h3><p class="model-note">Models 4 and 5 retain personal responses separately. Model 5 also includes the same contextual supply score. The supply-only model is Model 2, shown again for reference.</p>'
    s += figure(folder+'/03_component_models.svg','Individual reported concerns and assigned supply risk alongside covariates in modern-use models.')
    s += note(f'<strong>Key finding.</strong> In Model 5, opposition to family planning has OR <b>{ort(co["components_joint","opposes"])}</b>; health beliefs have OR <b>{ort(co["components_joint","health"])}</b>; low method confidence has OR <b>{ort(co["components_joint","confidence"])}</b>. These individual intervals are pointwise. The six-indicator joint test has Holm p {fp(tests["components_joint","Demand"]["holm_p"])} with only {tests["components_joint","Demand"]["df_denom"]:.2f} effective denominator degrees of freedom, so that block test is particularly weak. Use the heatmap to locate these concerns alongside service weaknesses; these associations do not establish which intervention would be most effective.')
    s += figure(folder+'/04_component_probability_gaps.svg','Adjusted modern-use percentage-point differences for each concern, present versus absent, with paired confidence intervals.')
    s += note(f'<strong>What this means for planning.</strong> The adjusted present-versus-absent difference is <b>{100*gaps["opposes"]["estimate"]:.1f} percentage points</b> for opposition and <b>{100*gaps["health"]["estimate"]:.1f} points</b> for health beliefs. The model predicts the same women under each response setting, retains their supply and other characteristics, and averages using WEIGHT. These gaps identify patterns to investigate; they are not intervention-effect estimates.')
    coefrows = [[esc(local_labels[k])]+[ort(co[mid,k]) if (mid,k) in co else 'Not included' for mid in ['components_demand','supply','components_joint']] for k in D+['supply_risk']]
    probrows = [[esc(local_labels[k]),pct(100*gaps[k]['probability_absent']),pct(100*gaps[k]['probability_present']),f'{100*gaps[k]["estimate"]:+.1f} ({100*gaps[k]["lo"]:+.1f} to {100*gaps[k]["hi"]:+.1f})'] for k in D]
    testrows = [[label,b,str(tests[mid,b]['df_num']),f"{tests[mid,b]['df_denom']:.2f}",fp(tests[mid,b]['p']),fp(tests[mid,b]['holm_p'])] for mid,label in [('score_joint','Both scores'),('components_joint','Six indicators + supply')] for b in ['Demand','Supply']]
    if pca:
        s += note('<strong>PCA interpretation.</strong> Supply PC1 has negative coefficients for trained-provider share and popular-product stock, alongside positive coefficients for other provision measures. Its negative is labeled a supply pattern score. It is not a monotonic supply-risk index, and its association cannot be interpreted as the effect of worsening every service domain.')
    else:
        s += note(f'<strong>Unexpected direction.</strong> Healthcare permission barriers have OR {ort(co["components_joint","agency"])} and an adjusted difference of {100*gaps["agency"]["estimate"]:+.1f} points. This does not mean restrictions improve use: the item concerns general healthcare permission, and confounding, selection and reverse direction remain possible.')
    s += '<details class="model-detail"><summary>Exact estimates, adjusted probabilities and joint tests</summary>'
    s += table(['Predictor','Six indicators','Supply only','Six indicators + supply · OR (95% CI)'],coefrows)
    s += table(['Concern','Predicted use: absent','Predicted use: present','Difference, pp (95% CI)'],probrows)
    s += table(['Joint model','Block','Restrictions','Effective denominator df','Raw p','Holm p'],testrows)
    s += '<p>Each joint model has two tests. Demand tests its one score coefficient or all six concern coefficients together; supply tests its one score coefficient. Holm correction applies within those two tests per model. These tests do not establish whether demand matters more than supply, equality between them, or causal binding constraints.</p>'
    s += downloads([(folder+'/model_coefficients.csv','All coefficients, including covariates'),(folder+'/model_block_tests.csv','Joint tests'),(folder+'/score_predicted_probabilities.csv','Probability curves'),(folder+'/individual_demand_probability_gaps.csv','Predicted probabilities and differences')])+'</details>'
    return s

css = (Path(__file__).resolve().parent.parent/'common/report.css').read_text() + '''
body{margin:0;background:#f5f8fa;color:#233e4e;font-family:Arial,Helvetica,sans-serif}main{max-width:1480px;margin:auto;padding:0 30px}.report-header{max-width:1420px;margin:32px auto 24px;padding:34px 36px;background:#143e51;color:white;border-radius:14px}.report-header h1{font-size:38px;line-height:1.12;margin:8px 0 16px;color:white}.report-header p{max-width:1110px;color:#e0edf2;line-height:1.65}.report-header .eyebrow{color:#b3dcdd}.report-header nav{display:flex;flex-wrap:wrap;gap:9px;margin-top:22px}.report-header nav a{color:white;border:1px solid #71909e;border-radius:20px;padding:7px 13px;text-decoration:none;font-size:13px}section{margin:26px 0;padding:30px;background:white;border-radius:12px;border:1px solid #dce6ea}h2{font-size:28px;line-height:1.25}h3{color:#164b5b;margin:28px 0 13px}.eyebrow{letter-spacing:.1em;font-size:12px;font-weight:700;color:#24717c}.intro,.model-note{line-height:1.65}.metrics{display:grid;grid-template-columns:repeat(4,1fr);gap:16px;margin:22px 0}.metric{padding:18px;border:1px solid #dce6ea;border-radius:9px}.metric b{display:block;font-size:29px;color:#125967}.metric span{font-size:13px;color:#677f8c}.report-warning{background:#fff8ed;border-left:4px solid #c48035;padding:16px 18px;line-height:1.65;border-radius:3px;font-size:14px}.mapbox{background:#f8fbfc;border:1px solid #e0e9ec;border-radius:9px;overflow:hidden}.country-map{display:block;width:100%;height:auto}.district{cursor:pointer}.district:hover,.district:focus{filter:brightness(.85);stroke:#173b50;stroke-width:2;outline:none}.facility-dot{cursor:pointer}.map-label{font-size:11.5px;fill:#203d4d;font-weight:600;paint-order:stroke;stroke:white;stroke-width:3;stroke-linejoin:round}.inset-heading{font-size:16px;fill:#174758;font-weight:700}.map-caption{font-size:12px;fill:#64808b}.legend{display:flex;flex-wrap:wrap;gap:17px;margin:15px 0;font-size:13px}.legend i{display:inline-block;width:14px;height:14px;border-radius:3px;margin-right:6px;vertical-align:middle}.bar-chart{display:block;width:100%}.bar-label{font-size:15px;fill:#244253;font-weight:600}.bar-label.sub{font-size:13px;font-weight:400}.bar-value{font-size:14px;font-weight:600;fill:#203d4c}.axis-label{font-size:13px;fill:#647d8a}.paired-heatmap{width:100%;min-width:1200px;border-collapse:separate;border-spacing:4px;font-size:13px}.paired-heatmap th{padding:10px 4px;text-align:center;vertical-align:bottom;font-size:12px}.paired-heatmap tbody th{text-align:left;min-width:205px;font-size:13px}.paired-heatmap small{display:block;font-size:10px;font-weight:400;color:#6e8793;margin-top:4px}.paired-heatmap td{text-align:center;font-size:14px;font-weight:700;padding:14px 5px;border-radius:2px}.demand-group{color:#087c88;background:#eef8f8}.supply-group{color:#b66b20;background:#fff6e8}.overall th{color:#086d78}.overall td{outline:1px solid #234a5c}.unknown-cell{background:#edf0f3;color:#7d8d96}.table-wrap{overflow:auto}.planning{width:100%;border-collapse:collapse}.planning th,.planning td{padding:12px 11px;vertical-align:top;text-align:left;line-height:1.5;border-bottom:1px solid #e4ecef}.planning thead{background:#f0f6f8}.planning th,.planning td{font-size:13px}.planning td ul{margin:0;padding-left:18px}.planning td li{margin:0 0 5px}.sample-badge{display:block;margin-top:5px;font-size:10px;font-weight:600;color:#98581a}.category-tag{display:inline-block;border-radius:4px;padding:5px 8px;white-space:nowrap;font-size:12px;font-weight:700}.model-figure{margin:23px 0 8px}.model-figure img{display:block;width:100%;height:auto}.model-meta{font-size:14px;line-height:1.7}.model-key{margin:10px 0 24px;background:#edf7f7;padding:15px 18px;border-left:4px solid #148591;font-size:14px;line-height:1.7}.model-detail{margin:18px 0;border:1px solid #dce7eb;border-radius:7px;padding:14px 17px}.model-detail summary{font-weight:700;color:#164b5b;cursor:pointer}.model-detail p{font-size:13px;line-height:1.7}.table-heading{display:flex;flex-wrap:wrap;gap:13px;margin:15px 0}.download{font-size:12px}.footnote{font-size:12px;color:#637d8b;line-height:1.7}.score-steps{font-size:14px;line-height:1.7;padding-left:22px}.score-steps li{margin:10px 0}.wide-break{height:1px;background:#dce7eb;margin:30px 0}.tooltip{display:none;position:fixed;background:#173d50;color:white;border-radius:9px;padding:16px;width:300px;z-index:1000;box-shadow:0 9px 35px #173d5040;font-size:13px;pointer-events:none}.tooltip.show{display:block}.tooltip strong{display:block;font-size:17px;color:white;margin-bottom:7px}.tooltip p{margin:7px 0;color:#e0edf1}.tooltip .two{display:flex;justify-content:space-between;margin:12px 0}.tooltip b{font-size:20px}.tooltip small{color:#c8dbe3}.tooltip .sample-alert{background:#fff0cb;color:#5c421b;padding:8px;border-radius:4px}@media(max-width:850px){main{padding:0 10px}section{padding:18px}.report-header{margin:12px;padding:24px}.report-header h1{font-size:28px}.metrics{grid-template-columns:repeat(2,1fr)}}@media print{body{background:white}.report-header,section{border-radius:0}details{display:block}nav,.download{display:none}.mapbox,.model-figure{break-inside:avoid}}
'''
co = {(r['model'],r['term']):r for r in m['coefficients']}

coverage_legend = '<div class="legend">'+''.join(f'<span><i style="background:{coverage_colors[k]}"></i>{coverage_labels[k]}: <b>{sum(v["coverage_group"]==k for v in DATA.values())}</b></span>' for k in ['both','demand_only','supply_only','neither'])+'<span><i style="background:#123d50;border-radius:50%"></i>Department facility-count dots</span></div>'
unmatched_rows = [[esc(name(g)),str(assign[g]['source_women']),str(assign[g]['need_women']),str(assign[g]['unmatched_women']),str(assign[g]['unmatched_need'])] for g in areas if assign[g]['unmatched_women']]
issue_rows = [[esc(name(q['geo'])),esc(q['residence']),str(q['n_facilities']),str(q['need_women']),esc(q['missing_components'].replace('_',' '))] for q in p['assignment_issues']]
header = '''<header class="report-header"><span class="eyebrow">SURGO × UNIVERSITY OF MANITOBA · CÔTE D’IVOIRE</span><h1>Demand, services and modern contraceptive use</h1><p>Women’s reported concerns are examined alongside the recorded facility environment in their own department and Rural/Urban area. Joint analysis helps identify places where service improvements, information and social support—or integrated responses—warrant investigation.</p><p><strong>Initial evidence:</strong> higher personal demand scores are associated with lower modern use after adjustment. The selected supply-risk association remains uncertain. Geographic profiles identify relative concerns and service weaknesses; they do not establish causal binding constraints.</p><nav><a href="#coverage">1 · Coverage and linkage</a><a href="#use">2 · Use and intention</a><a href="#joint">3 · Joint conditions and priorities</a><a href="#models">4 · Adjusted models</a><a href="#definitions">Definitions and checks</a><a href="#pca">Appendix · PCA</a></nav></header>'''
coverage = f'''<section id="coverage"><span class="eyebrow">01 · DATA COVERAGE AND LINKAGE</span><h2>Department–residence profiles attach local services to women</h2><p class="intro">The source join retains <b>10,859 records</b>: 10,856 have geographic identifiers, including 103 nonconsenting respondents; three records lack geography, weights and consent. The coverage map uses all geographically identified source records, before selecting women in need. Manitoba contains <b>3,078 facilities in 100 department-equivalent areas</b>. Of 82 surveyed departments, 78 have both sources and four have women’s data only.</p><div class="metrics"><div class="metric"><b>10,859</b><span>Original source records retained</span></div><div class="metric"><b>78</b><span>Departments with both sources</span></div><div class="metric"><b>3,078</b><span>Facility records across 100 areas</span></div><div class="metric"><b>4,292</b><span>Women with original SURGO FPN2 = 1</span></div></div>{coverage_legend}{map_svg('coverage')}<p class="footnote">Dots show department facility counts, not actual facility coordinates. The 22 facility-only departments contain 366 records; 2,712 facility records contribute to the 78 linked departments. Seven display areas have neither source recorded. Boundaries use the <a href="https://data.humdata.org/dataset/cod-ab-civ">CNTIG/OCHA archived 2018 layer</a>, with Bonon, Gohitafla and Ouelle reconstructed from named subprefectures following official <a href="https://www.gouv.ci/doc/1601493593COMMUNIQUE-DU-CONSEIL-DES-MINISTRES-DU-MERCREDI-30-SEPTEMBRE-2020.pdf">30 September 2020</a> and <a href="https://www.presidence.ci/wp-content/uploads/2020/09/CCM-du-23-09-2020.pdf">23 September 2020</a> decisions. These display updates conserve parent areas and do not change any woman/facility assignment.</p>{downloads([('department_data_coverage.csv','Coverage counts'),('map_name_crosswalk.csv','Boundary name crosswalk'),('boundary_update_checks.csv','Documented boundary updates')])}<h3>How the assignment works</h3>{table(['Woman’s recorded residence','Assigned supply environment'],[['Department X · Rural','Aggregate the valid facility indicators among Rural facilities in Department X, then attach that profile to Rural women in Department X.'],['Department X · Urban','Aggregate among Urban facilities in Department X, then attach that profile to Urban women in Department X.']])}<p class="model-note">Binary facility indicators become valid-response proportions; facility-level proportions become averages of valid facility proportions. Each facility contributes equally; there are no facility weights. This is <b>aggregation before the woman-level join</b>, not an average of profiles repeated across surveyed women. Facility residence is directly recorded in <b>s1_5</b>, rather than inferred from health-district type.</p><div class="report-warning"><strong>Unmatched areas:</strong> 153 women in Bonon, Facobly, Gohitafla and Taabo have no facility profile in the available crosswalk, including <b>49 women in need</b>. Three additional source records have no geography. No Rural/Urban mismatch is repaired by borrowing another area, assigning zero or applying Senegal’s department fallback. These source gaps do not establish absence of services.</div>{table(['Department','Source women','Women in need','Unmatched source women','Unmatched women in need'],unmatched_rows)}<p class="footnote">Raw health-district codes 1–13 are restored using the supplied companion codebook. Explicit Abidjan, Bouake, Gagnoa and Korhogo rollups are independently checked. Health districts and administrative departments are not necessarily identical catchments; the approved crosswalk remains a contextual assignment, not a facility-attendance or GPS link.</p>{downloads([('department_residence_assignment_summary.csv','Residence-specific assignment counts'),('assigned_supply_profiles.csv','Assigned profiles')])}</section>'''

unknown_method = bars['user']['n']-bars['modern']['n']-bars['traditional']['n']
unknown_preference = bars['user']['n']-bars['wanted_start']['n']-bars['other_start']['n']
use = f'''<section id="use"><span class="eyebrow">02 · USE, INTENTION AND METHOD CHOICE</span><h2>Among women SURGO classifies as in need of family planning</h2><div class="report-warning"><strong>Population from this section onward:</strong> original SURGO <b>FPN2 = 1</b>, unchanged, gives <b>4,292 women</b>. No older script’s need_n recoding or sterilization/hysterectomy additions are applied. Women remain in overview results even when a model predictor is unresolved or a facility profile is unavailable.</div>{bar_chart()}<p class="footnote">Every bar and sub-bar uses all 4,292 women in need as its denominator. Percentages use <b>WEIGHT</b>; n is an unweighted respondent count. Capped whiskers are 95% logit intervals using the documented <b>IDZD census-district clusters</b>, constructed before need-domain selection. Original sampling strata and finite-population corrections are unavailable. Sub-bars overlap with current users and do not sum to 100%. {unknown_method} users have unresolved modern/traditional classification; {unknown_preference} have another or unresolved initiation-preference response. *Preference concerns the wanted method at initiation; the alternative method type is not established. Future intention concerns any method, without a specified 12-month initiation window.</p>{note(f'<strong>Initial pattern.</strong> Weighted current contraceptive use is <b>{100*bars["user"]["estimate"]:.1f}%</b>, including <b>{100*bars["modern"]["estimate"]:.1f}%</b> confirmed modern use. Another <b>{100*bars["intender"]["estimate"]:.1f}%</b> are nonusers with intention and <b>{100*bars["no_intent"]["estimate"]:.1f}%</b> nonusers without stated intention. These are concurrent groups, not observed transitions from intention to adoption.')}{downloads([('use_distribution.csv','Use and intention estimates'),('department_use_rates.csv','Department use estimates')])}</section>'''

comparison_rows = [[area_label(g)]+[tag(scores[k,g]['category']) for k in methods] for g in areas]
method_rows = [
 ['Rank average','Rank departments separately for each component, using average ranks for ties; average the six ranks.','Demand above its final-score median; favorable supply below its median.'],
 ['Mean percentages','Average the six original percentages on each side.','Same strict final-score median rule.'],
 ['Median/IQR','Subtract each component’s department median, divide by its IQR, then average six transformed values.','Same strict final-score median rule.'],
 ['Median count','Count components above demand medians or below favorable supply medians.','Flag at least four of six unfavorable comparisons.'],
 ['Z-score average · primary','Subtract each component’s department mean, divide by its sample SD, then average six component z-scores.','Same strict final-score median rule.']]
threshold = cuts['zscore']
count_category = {k:sum(scores['zscore',g]['category']==k for g in areas) for k in ['both','demand','supply','neither','unassessed']}
legend = '<div class="legend">'+''.join(f'<span><i style="background:{colors[k]}"></i>{catnames[k]}: <b>{count_category[k]}</b></span>' for k in count_category)+'<span><i style="background:#eee4ce"></i>Facility data only: <b>22</b></span><span><i style="background:#fafafa;border:1px solid #ccc"></i>Neither source recorded: <b>7</b></span></div>'
priority_rows=[]
for g in areas:
    q=planning[g]
    def tops(side,fields):return '<ul>'+''.join(f'<li>{esc(labels[k])} <b>{pct(hm[g,side,k]["value"])}</b></li>' for k in fields)+'</ul>'
    def flag(v):return 'Unassessed' if v is None else 'Yes' if v else 'No'
    priority_rows.append([area_label(g),'<b style="color:#087c88">'+flag(q['demand_flag'])+'</b>',tops('Demand',q['top_demand']) if q['demand_flag'] else '—',
        '<b style="color:#b66b20">'+flag(q['supply_flag'])+'</b>',tops('Supply',q['top_supply']) if q['supply_flag'] else 'Incomplete supply inputs' if q['supply_flag'] is None else '—'])
joint = f'''<section id="joint"><span class="eyebrow">03 · JOINT CONDITIONS AND PLANNING PRIORITIES</span><h2>Reported concerns alongside recorded services</h2><p class="intro">Demand percentages use <b>FPN2 = 1 women with valid responses for each domain</b>, weighted by WEIGHT. Supply summarizes all included facilities in each department, combining Rural and Urban records. This keeps the planning heatmap readable; the models use residence-specific supply profiles.</p>{heatmap()}<p class="footnote">Darker teal means more concern; darker orange means weaker provision. Raw supply percentages all run higher = stronger provision. Hover cells for denominators. The overall row pools all women in need and facilities in the linked departments; its two sides have different coverage because four surveyed departments lack facilities. Gray means unknown, not zero. The stock measure uses the already-joined at-least-one-LARC indicator because the three-LARC variable is constant in Côte d’Ivoire. † Fewer than 50 women in need.</p>{downloads([('heatmap_values_and_denominators.csv','Heatmap values and denominators'),('demand_missingness_by_department.csv','Demand response coverage')])}<div class="wide-break"></div><h3>How classifications compare across five approaches</h3>{table(['Department']+method_names,comparison_rows)}<details class="model-detail"><summary>How the comparison methods work</summary>{table(['Approach','Score construction','Threshold'],method_rows)}<p>Every continuous score requires six component summaries. Demand has 82 complete department scores; supply has 77. Component references use valid department values, with 78 departments for most supply components and 77 for free provision. Exact median ties are unflagged. No reduced-component score or outcome-informed threshold is substituted.</p></details>{downloads([('classification_comparison.csv','Five-method comparison'),('classification_thresholds.csv','Exact thresholds'),('component_score_trace.csv','Component transformations')])}<div class="wide-break"></div><h3>Department planning profiles · z-score average</h3><details open class="model-detail"><summary>How the scores and thresholds are constructed</summary><ol class="score-steps"><li><b>Summarize the two sides by department.</b> Demand is weighted valid-domain concern prevalence among women in need. Supply is unweighted facility summaries across all residence types.</li><li><b>Standardize each component.</b> Calculate (department value − mean of valid department values) ÷ sample SD. Departments contribute equally to this normalization reference; these are not survey weights.</li><li><b>Average all six components.</b> Higher demand means greater concern; higher raw supply readiness means stronger provision. The map uses department demand scores; models use each woman’s personal demand responses.</li><li><b>Apply separate final-score medians.</b> Flag demand strictly above <b>{threshold['demand_cutoff']:.6f}</b>. Flag supply readiness strictly below <b>{threshold['supply_cutoff']:.6f}</b>, equivalent to supply risk above <b>{-threshold['supply_cutoff']:.6f}</b>. Exact ties are unflagged.</li></ol><p>Both flags → Both; one flag → Demand only or Supply only; no flags → Neither flagged. Missing supply gives <b>Supply unassessed</b>, rather than Demand only or Neither. These are relative planning profiles, not causal diagnoses or validated adequacy standards.</p></details>{legend}{map_svg('classification')}{note(f'<strong>Planning signal.</strong> The z-score approach flags <b>{count_category["both"]}</b> departments on both sides, <b>{count_category["demand"]}</b> on demand only, <b>{count_category["supply"]}</b> on supply only and <b>{count_category["neither"]}</b> on neither. Supply is unassessed in five departments: four without facility profiles and Daoukro with undefined free provision. Use component evidence and local validation before selecting interventions.')}<p class="footnote"><b>Small samples:</b> {len(small)} departments have fewer than 50 women in need; {sum(rates[g]['n']<25 for g in areas)} have fewer than 25. The smallest sample is {min(rates[g]['n'] for g in areas)}. These exploratory estimates are retained with flags; rankings and classifications can be unstable. The 524 common-model exclusions are not applied to the map: each domain uses all available valid responses among women in need.</p>{downloads([('department_sample_flags.csv','Department sample-size flags')])}<h3>Department planning priorities</h3>{table(['Department','Demand challenges','Top three demand components','Supply challenges','Top three service weaknesses'],priority_rows)}<p class="footnote">Top concerns have the largest component z-scores; top service weaknesses the smallest favorable component z-scores. Lists appear for flagged sides only, with original percentages. Missing supply remains unassessed rather than being ranked using a partial index. The department-wide planning map/table and residence-specific model exposures serve different purposes.</p>{downloads([('planning_priorities.csv','Planning priorities'),('all_method_scores_and_classifications.csv','All scores and classifications')])}</section>'''
model = '<section id="models"><span class="eyebrow">04 · ADJUSTED ASSOCIATIONS</span>'+models(m,'models_zscore')+'</section>'

demand_defs = [
 ['Healthcare permission barriers','HS7C: 1 = permission to see a doctor is a challenge; 2 = no. General healthcare permission, not FP-specific partner approval.'],
 ['Anticipated social stigma','PB7B/C: 1/2 on either item about being seen or what others would say. Zero requires both valid 3/4 responses.'],
 ['Health / side-effect beliefs','PB2A–C: 1/2 on any item about health problems, reduced sexual desire or babies with birth defects. Zero requires all valid 3/4 responses. Bleeding and later fertility are excluded. Beliefs are not personally experienced side effects.'],
 ['Future-fertility concerns','MP4_a–f reversible-method concerns, requiring coherent MP4_a–i 0/1 selections, no none-plus-method contradiction, an explicit selection or none, and MP4_99 = 0. Sterilization items MP4_g/h do not create a concern.'],
 ['Low method knowledge · retained label','MC5A/B: low confidence choosing a suitable method or handling side effects/switching (3/4 on either). This is confidence/self-efficacy, not objective knowledge. Zero requires both valid 1/2 answers.'],
 ['Opposes family planning','FP1: against (3/4) versus in favor (1/2). Unfamiliarity (5), no opinion (99) and other special responses remain unresolved.']]
supply_defs = [
 ['Short + long-acting offering','Mean of UoM one_larc_3_sa_offer, joined as share_offer_larc_and_3_sa: share meeting the original ≥1 long-acting plus ≥3 short-acting product/category rule.'],
 ['Free-product provision','Mean of valid facility p_free_prod, joined as mean_p_free_prod. A value of 60% means the average reporting facility provides 60% of its recorded products free; not that 60% of facilities offer any free method.'],
 ['Trained-provider share','Mean of valid facility p_train proportions, using original UoM personnel/topic eligibility. Not the percentage of facilities with at least one trained provider.'],
 ['FP counselor availability','s7_2: Yes/(Yes+No); 1 = Yes, 2 = No, unknown/special responses excluded. Availability is not counseling receipt or quality.'],
 ['At least one LARC in stock','Mean of one_larc_stockin, joined as share_stocked_1plus_larc. The country-wide three-LARC indicator is constant FALSE and cannot be standardized; this existing joined alternative is explicitly used.'],
 ['Most-popular product in stock','n_mp_in_stock_true/(true+false), among facilities with known status. Each facility refers to its own most-popular product, not the woman’s preferred method.']]
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
den_rows = [[area_label(g),str(cov[g]['facilities'])]+[str(hm[g,'Supply',k]['n_valid']) for k in S] for g in areas]
correlations = list(csv.DictReader((root/'checks/component_correlations.csv').open()))
cor_rows=[]
for side in ['Demand','Supply']:
    pairs=sorted([q for q in correlations if q['side']==side and q['item1']<q['item2']],key=lambda q:abs(float(q['correlation'])),reverse=True)[:3]
    cor_rows += [[side,esc(labels[q['item1']]),esc(labels[q['item2']]),f'{float(q["correlation"]):+.3f}',q['n_pair']] for q in pairs]
weight_concentration = list(csv.DictReader((root/'checks/model_weight_concentration.csv').open()))
weight_rows = [[esc(name(q['geo'])),q['n'],pct(100*float(q['weight_share']))] for q in weight_concentration[:6]]
defs = f'''<section id="definitions"><span class="eyebrow">DEFINITIONS, MISSINGNESS AND VERIFICATION</span><h2>What the measures mean and what needs checking</h2><details open class="model-detail"><summary>Demand definitions and uncertainty</summary>{table(['Domain','Source and coding'],demand_defs)}<p>A known positive establishes an any-concern flag even if another item is unknown. Zero requires valid negatives on every included item. Otherwise the domain remains unresolved. For health, raw 1/2 becomes concern, 3/4 becomes no concern and 99 remains unknown. These modules have responses among users and nonusers and are not restricted to married women.</p>{table(['Question','Shortened wording','Blank responses','Don’t know / unclassifiable, n','Weighted percentage'],question_rows)}{table(['Combined demand domain','Unresolved women, n','Weighted percentage'],unknown_rows)}<p>PB2A alone has 30 unresolved responses (0.9% weighted), compared with 144 (3.8%) for the current PB2A–C composite. This report retains the broader definition; no PB2A-only model is substituted. Uncertainty is potentially informative, not necessarily a data error. Counts overlap across domains and the complete-case model may select more informed respondents.</p>{downloads([('checks/demand_unresolved_summary.csv','Unresolved domain counts'),('checks/unresolved_item_responses.csv','Underlying uncertainty responses')])}</details><details open class="model-detail"><summary>Supply definitions and valid-response coverage</summary>{table(['Measure','Calculation and interpretation'],supply_defs)}<p>Apply these same facility definitions within department–residence cells for models and across all facilities in a department for planning. Facility aggregation is unweighted; missing responses are not automatically No. The source UoM definitions and their valid-response denominators are preserved.</p>{table(['Department','Facility records','Offering','Free provision','Training','Counselor','LARC stock','Popular stock'],den_rows)}<p>Most-popular-product stock has substantial nonresponse: {sum(hm[g,'Supply','popular_stock']['n_valid'] for g in areas):,} of {AUD['mapped_facility_records']:,} facilities in linked departments have known status. Read the percentages alongside these denominators; reported provision need not describe nonreporting facilities.</p>{downloads([('supply_input_reconciliation.csv','All 1,278 profile/component checks'),('department_supply_profiles.csv','Department-wide supply profiles'),('assigned_supply_profiles.csv','Residence-specific supply profiles')])}</details><details open class="model-detail"><summary>Facility issues requiring source clarification</summary>{table(['Department','Residence','Matched facilities','Women in need affected','Missing components'],issue_rows)}<div class="report-warning"><strong>Daoukro:</strong> all 21 facilities have zero recorded free and nonfree product counts, so p_free_prod is undefined. However, 19 report FP provision in the raw s2_5_sp question. Check the source questions and cleaning before interpreting this as no offering or no free provision. No zero repair is made.</div><p>Duekoue–Rural, Ouangolodougou–Rural and Gueyo–Urban have no known popular-product stock status. Four other departments have no facility profile. A department-average fallback for a known component could be tested separately, but it is not applied here. Entirely missing department provision cannot be recovered from a department fallback. Unmatched does not mean zero provision.</p>{downloads([('supply_assignment_issues.csv','Affected supply contexts and counts')])}</details><details class="model-detail"><summary>Original need flag, weights and available survey design</summary><p>Original <b>FPN2 = 1</b> identifies 4,292 women among 10,753 consenting positive-weight respondents, a weighted share of {100*AUD['weighted_need_share']:.1f}%. The original file records 6,461 as FPN2 = 0, 103 as 95 and three missing. No old researcher need_n or permanent-protection additions are used. All original source fields and rows are preserved.</p><p>Use final <b>WEIGHT</b>. The documented <b>IDZD</b> identifier is a unique census district/enumeration zone; {AUD['survey_psus']} clusters are available among consenting weighted respondents, each nested within one department and residence type. Overview confidence intervals use that available cluster design, created before need-domain selection. Original sampling strata and FPC are not documented, so the full original complex design cannot be claimed. Department rates remain visible as weighted point estimates; logit intervals are not calculated where boundary proportions or zero design degrees of freedom prevent estimation.</p><p>All outcome models use modern use, retain WEIGHT, adjust for age + age², education, partnership, fertility goal and the woman’s Rural/Urban status. Primary uncertainty is department-clustered CR2 with coefficient-specific Satterthwaite intervals. Sampling-IDZD survey-GLM intervals are exported as a sensitivity; weighted point estimates agree. No random effects or interactions are fitted.</p></details><details open class="model-detail"><summary>Complete-case sample and model diagnostics</summary>{table(['Model','Women','Modern users','Parameters','Maximum VIF'],sample_rows)}{table(['Sequential selection step','Additional exclusions','Women remaining'],flow_rows)}<p>The common sample excludes <b>524 women (12.2% of respondents)</b>, representing {100*(1-MA['eligible_weight_share']):.1f}% of need-group survey weight. Demand domains are unresolved for 338 women; 197 have incomplete supply, including 21 also unresolved on demand. The sequence removes each woman only once. A demand-only or supply-only specification could use a larger available sample; the primary comparison deliberately holds women constant across predictor blocks.</p>{table(['Group','Women','Share of need-group weight','Modern use among known outcomes','Urban share'],comparison_samples)}<p>All ten z-score/PCA specifications converge, have full-rank designs and finite coefficients. Maximum weighted VIF is {max(q['max_vif'] for q in m['samples']):.2f}. This is not a formal power guarantee or proof of construct validity. There are 121 complete residence-specific supply profiles across 77 modeled departments; all women in each profile share their supply measures. Personal demand scores retain women’s own responses, with 62 observed values in the model sample. Scores enter continuously, without dichotomization or IQR scaling. Intervals condition on estimated indices.</p>{downloads([('checks/model_sample_flow.csv','Selection flow'),('checks/model_sample_comparison.csv','Included versus excluded women'),('models_zscore/model_missingness.csv','All model missingness'),('models_zscore/model_vifs.csv','Weighted VIF'),('models_zscore/model_samples.csv','Sample and convergence'),('models_zscore/leave_one_department_out.csv','Department-deletion checks'),('models_zscore/sampling_cluster_model_sensitivity.csv','Sampling-cluster inference sensitivity'),('models_zscore/individual_demand_score_reference.csv','Personal-score normalization'),('models_zscore/supply_score_reference.csv','Supply-score normalization')])}</details><details open class="model-detail"><summary>Low effective degrees of freedom and concentrated information</summary><p><b>Abidjan accounts for {100*float(weight_concentration[0]['weight_share']):.1f}% of the common model’s survey weight.</b> Information is unevenly distributed across departments. Despite 77 nominal department clusters, the six-demand-coefficient joint HTZ test has only {next(q['df_denom'] for q in m['tests'] if q['model']=='components_joint' and q['block']=='Demand'):.2f} effective denominator degrees of freedom. Its high p-value is not evidence that demand has no association. Pointwise coefficient intervals and the single demand-score test address different hypotheses and also have limited effective degrees of freedom.</p>{table(['Largest represented department','Modeled women','Share of model survey weight'],weight_rows)}<p>Retain this as an exploratory analysis. Review department-deletion sensitivity and original sampling documentation before strong inferential claims. Do not switch to a more optimistic covariance or large-sample test merely to obtain significance. The available sampling-cluster sensitivity is supplied transparently; department CR2 remains primary for shared supply environments.</p>{downloads([('checks/model_weight_concentration.csv','Department weight concentration'),('checks/block_test_diagnostics.csv','Effective degrees of freedom by test')])}</details><details class="model-detail"><summary>Component overlap, verification and interpretation</summary>{table(['Side','Component 1','Component 2','Correlation','Paired records'],cor_rows)}<p>Demand correlations use personal FPN2 = 1 responses and WEIGHT; supply correlations use one unweighted department profile. Correlated offering and stock features can receive overlapping weight in a six-component index. Equal weighting is explicit, not proof of equal intervention importance.</p><p>Independent verification passes {verification['check_count']} checks: every original survey field, all 1,278 department/context component values, all 65,154 individual assigned supply values including expected missingness, all ten weighted fits and CR2 covariance matrices, joint tests and PCA projections independently reproduced using weighted singular-value decomposition. Numerical correctness does not establish causal identification. The data are cross-sectional; attitudes can reflect contraceptive experience, and women can obtain care outside their assigned geographic environment.</p>{downloads([('checks/component_correlations.csv','All correlations'),('checks/audit_checks.csv','Independent verification checklist')])}</details></section>'''

load_rows = [[q['side'],esc(labels[q['variable']]),f'{q["loading"]:+.3f}'] for q in pc['loadings'] if q['PC']==1]
var_rows = [[q['side'],f'PC{q["PC"]}',pct(q['variance_pct']),pct(q['cumulative_pct'])] for q in pc['variance']]
pca = f'''<section id="pca"><span class="eyebrow">APPENDIX · PCA SENSITIVITY</span><h2>Alternative scores from the same personal and assigned service inputs</h2><p class="intro">Separate weighted correlation PCAs use the same six personal demand indicators and six residence-specific supply measures among original FPN2 = 1 women. Demand PCA uses {pc['audit']['demand_PCA_n']:,} complete women. Supply PCA uses {pc['audit']['supply_PCA_women_n']:,} women with complete supply, equivalent to {pc['audit']['supply_profiles']} profiles weighted by their represented women’s summed WEIGHT. These are not facility weights.</p><div class="report-warning"><strong>Supply PC1 is a contrast, not a general risk scale.</strong> Its coefficients are positive for offering, free provision, counseling and LARC stock, but negative for training and popular-product stock. The model uses its negative and labels it a <b>Supply pattern score (PCA)</b>. Increasing it does not mean every supply condition worsens. The monotonic equal-weight z-score index remains primary.</div>{figure('pca_appendix/01_pca_loadings.svg','Signed PCA scoring coefficients for personal demand and assigned supply inputs.')}{note(f'<strong>What PCA retains.</strong> Demand PC1 captures {pc["audit"]["demand_PC1_variance"]:.1f}% of variation in the demand inputs; supply PC1 captures {pc["audit"]["supply_PC1_variance"]:.1f}% of the assigned supply variation. These are not percentages of modern use explained. The negative training/popular-stock coefficients must remain visible rather than being forced positive.')}{figure('pca_appendix/02_pca_variance.svg','Variance captured by each component and exploratory permutation benchmarks.')}<details class="model-detail"><summary>Coefficients, variance and construction checks</summary>{table(['Side','Input','PC1 coefficient'],load_rows)}{table(['Side','Component','Variance retained','Cumulative'],var_rows)}<p>Final-weighted input means/population SDs and an eigendecomposition define the components. Each woman receives her personal demand projection and the projection of her exact department-residence supply values. PC1 is prespecified as a sensitivity. PC2 is reported but not entered into the current models; no PCA geographic classification is constructed.</p><p>Exploratory permutation and department-deletion checks describe construction stability, not formal survey-design tests. Supply PC1 does not exceed its exploratory permutation benchmark, reinforcing caution about a strong one-dimensional interpretation. The same 3,768 women enter all ten models. PCA units differ from average-z units, so odds-ratio magnitudes cannot be compared as measures of which approach or side matters more. Model intervals do not propagate score-estimation uncertainty.</p>{downloads([('pca_appendix/pca_loadings.csv','All PCA coefficients'),('pca_appendix/pca_centers_scales.csv','PCA means and SDs'),('pca_appendix/pca_variance.csv','Variance and exploratory checks'),('pca_appendix/pca_leave_one_department_out.csv','PCA department-deletion checks')])}</details>{models(pm,'pca_appendix/models',pca=True)}<p class="footnote">Method references: <a href="https://knowledge-for-policy.ec.europa.eu/composite-indicators/toolkit_en/navigation-page/10-step-guide_en/step-5-normalisation_en">JRC normalization</a>, <a href="https://knowledge-for-policy.ec.europa.eu/composite-indicators/toolkit_en/navigation-page/10-step-guide_en/step-6-weighting_en">JRC weighting/PCA</a>, and <a href="https://jepusto.github.io/clubSandwich/reference/vcovCR.glm.html">CR2 covariance for weighted GLMs</a>. All figures are calculated from Côte d’Ivoire data. Proposed uncertainty-category or geographic-fallback sensitivities are not implemented in this version.</p></section>'''

js = '''const DATA=__DATA__;const tip=document.querySelector('.tooltip');
const fmt=v=>v==null?'Not surveyed':v.toFixed(1)+'%';
function hide(){tip.classList.remove('show')}
function move(e){let x=e.clientX+17,y=e.clientY+17;if(x+tip.offsetWidth>innerWidth-10)x=e.clientX-tip.offsetWidth-17;if(y+tip.offsetHeight>innerHeight-10)y=e.clientY-tip.offsetHeight-17;tip.style.left=Math.max(10,x)+'px';tip.style.top=Math.max(10,y)+'px'}
function show(el,e){const d=DATA[el.dataset.key];if(!d)return;tip.innerHTML='<strong>'+d.name+'</strong><p>'+(el.dataset.mode==='coverage'?d.coverage_label:d.category)+'</p><div class="two"><div><b>'+fmt(d.cpr)+'</b><br><small>Any-method use</small></div><div><b>'+fmt(d.mcpr)+'</b><br><small>Modern use</small></div></div><p>'+d.n+' women with original FPN2 = 1.</p><p>'+d.source_rows+' source women · '+d.facilities+' included facility records</p>'+(el.dataset.mode==='classification'&&d.demand!=null?'<p>Department demand score: '+d.demand.toFixed(3)+'<br>Supply readiness: '+(d.supply==null?'Unassessed':d.supply.toFixed(3))+'</p>':'')+(d.supply_issue_need?'<p>'+d.supply_issue_need+' women in need have incomplete assigned supply inputs.</p>':'')+(d.small_sample?'<p class="sample-alert">† Small need-group sample: n='+d.n+'. Interpret estimates and classifications cautiously.</p>':'')+'<small>Weighted use among women in need. Missing facility records do not establish absence of services.</small>';tip.classList.add('show');move(e)}
document.querySelectorAll('[data-key][data-mode]').forEach(el=>{el.addEventListener('pointerenter',e=>show(el,e));el.addEventListener('pointermove',move);el.addEventListener('pointerleave',hide);el.addEventListener('focus',()=>{let r=el.getBoundingClientRect();show(el,{clientX:r.x+r.width/2,clientY:r.y+r.height/2})});el.addEventListener('blur',hide);el.addEventListener('click',e=>show(el,e))});document.addEventListener('keydown',e=>{if(e.key==='Escape')hide()});
'''.replace('__DATA__',json.dumps(DATA,separators=(',',':')))
page = '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Cote_dIvoire_analysis | SURGO × Manitoba</title><style>'+css+'</style></head><body>'+header+'<main>'+coverage+use+joint+model+defs+pca+'</main><div class="tooltip" role="status" aria-live="polite"></div><footer style="max-width:1430px;margin:20px auto 35px;padding:0 30px;color:#6a8290;font-size:12px">Côte d’Ivoire · exact department–residence supply · original SURGO FPN2 = 1 · initial cross-sectional evidence.</footer><script>'+js+'</script></body></html>'
output.parent.mkdir(parents=True,exist_ok=True)
output.write_text(page)
print('Rendered',output.name,'with 111 display areas, 82 surveyed departments, original FPN2 need group and ten modern-use model specifications.')
