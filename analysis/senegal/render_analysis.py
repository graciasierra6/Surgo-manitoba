"""Render the Senegal hybrid-linkage analysis from verified aggregate outputs."""
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
all_areas = sorted(g for g in cov if g != 'SENEGAL')
small = {g for g in areas if rates[g]['n'] < 50}
n, model_n = AUD['need_n'], MA['common_n']
assert n == 1905 and model_n == 1445 and MA['common_events'] == 737
assert len(areas) == 33 and len(layout) == len(all_areas) == 46
assert pm['audit']['common_n'] == model_n and verification['all_passed']
esc = lambda s: html.escape(str(s), quote=True)
name = lambda g: cov[g]['name'].title()
pct = lambda v, digits=1: '—' if v is None else f'{v:.{digits}f}%'
fp = lambda v: '&lt;0.001' if v < .001 else f'{v:.3f}'
ort = lambda r: f'{r["odds_ratio"]:.2f} ({r["or_lo"]:.2f}–{r["or_hi"]:.2f})'
colors = dict(both='#78549b', demand='#047f8b', supply='#cd7729', neither='#bacad2',
              unassessed='#edf0f3', supply_only='#eee4ce')
catnames = dict(both='Both', demand='Demand only', supply='Supply only', neither='Neither flagged',
                unassessed='Unassessed', supply_only='Facility data only')
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
    return f'<a class="download" download="senegal_{Path(file).name}" href="data:text/csv;base64,{data}">{esc(title)} ↓</a>'

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
        mcpr=100*r['mcpr']['estimate'] if r else None, category=catnames[q['category']] if q else 'Facility data only',
        demand=q['demand_score'] if q else None, supply=q['supply_score'] if q else None,
        small_sample=g in small, coverage_group=group, coverage_label=coverage_labels[group],
        fallback_need=assign[g]['fallback_need'] if g in assign else 0)
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
    out = [f'<svg xmlns="http://www.w3.org/2000/svg" class="senegal-map" viewBox="0 0 1250 700" role="img" aria-label="Senegal department {mode} map"><rect width="1250" height="700" fill="#f8fbfc"/>']
    def draw(features, box, inset=False):
        points = [xy for f in features for ring in f['rings'] for xy in ring]
        xmin, xmax = min(v[0] for v in points), max(v[0] for v in points)
        ymin, ymax = min(v[1] for v in points), max(v[1] for v in points)
        bx, by, bw, bh = box
        scale = min(bw/(xmax-xmin), bh/(ymax-ymin))
        ox, oy = bx+(bw-scale*(xmax-xmin))/2, by+(bh-scale*(ymax-ymin))/2
        def xy(v):
            return ox+(v[0]-xmin)*scale, oy+(ymax-v[1])*scale
        text = []
        for f in features:
            g, d = f['geo'], DATA[f['geo']]
            category = scores['zscore', g]['category'] if g in rates else 'supply_only'
            fill = coverage_colors[d['coverage_group']] if mode == 'coverage' else colors[category]
            path = ' '.join('M'+' L'.join(f'{x:.2f},{y:.2f}' for x,y in map(xy,ring))+' Z' for ring in f['rings'])
            title = d['name'] + ': ' + (f'modern use {d["mcpr"]:.1f}%' if d['mcpr'] is not None else 'facility data only; no SURGO sample')
            out.append(f'<path class="district" data-key="{g}" data-mode="{mode}" d="{path}" fill="{fill}" fill-rule="evenodd" stroke="white" stroke-width="1.1" tabindex="0"><title>{esc(title)}</title></path>')
            cx, cy = xy(f['center'])
            if mode == 'coverage':
                radius = 3 + math.sqrt(d['facilities']) * .55
                out.append(f'<circle class="facility-dot" data-key="{g}" data-mode="{mode}" cx="{cx:.2f}" cy="{cy:.2f}" r="{radius:.2f}" fill="#123d50" fill-opacity=".62" stroke="white" stroke-width="1"><title>{d["facilities"]} facility records</title></circle>')
            if not f['inset'] or inset:
                label = d['name'] + (' †' if mode == 'classification' and d['small_sample'] else '')
                text.append([cx, cy, cx, cy, label])
        for _ in range(160):
            for i, t in enumerate(text):
                for o in text[:i]:
                    dx, dy = t[2]-o[2], t[3]-o[3]
                    if abs(dx) < max(50, (len(t[4])+len(o[4]))*3.6) and abs(dy) < 17:
                        step = (17-abs(dy))*.26
                        sign = 1 if dy >= 0 else -1
                        t[3] += sign*step; o[3] -= sign*step
                t[2] = min(bx+bw-30, max(bx+30, t[2]))
                t[3] = min(by+bh-8, max(by+11, t[3]))
        for cx, cy, lx, ly, label in text:
            if abs(ly-cy) > 6:
                out.append(f'<path d="M{cx:.1f},{cy:.1f} L{lx:.1f},{ly:.1f}" stroke="#7e929d" stroke-width=".6" fill="none" pointer-events="none"/>')
            out.append(f'<text x="{lx:.1f}" y="{ly:.1f}" class="map-label" font-family="Arial" font-size="11.5" font-weight="600" fill="#203d4d" text-anchor="middle" pointer-events="none">{esc(label)}</text>')
    draw(layout, (25, 20, 890, 615))
    out += ['<rect x="940" y="80" width="290" height="290" rx="10" fill="white" stroke="#d9e5e9"/>',
            '<text x="958" y="108" class="inset-heading">Dakar region · enlarged</text>']
    draw([f for f in layout if f['inset']], (950, 125, 270, 230), True)
    out += ['<text x="953" y="420" class="map-caption">Hover for department details,</text>',
            '<text x="953" y="440" class="map-caption">weighted CPR and mCPR.</text>',
            ('<text x="953" y="482" class="map-caption">† Fewer than 50 women in need</text>' if mode=='classification' else ''),
            '<text x="953" y="518" class="map-caption">N ↑</text>',
            ('<text x="30" y="674" class="map-caption">Government of Senegal / OCHA boundaries · dots show department totals, not facility coordinates</text>' if mode=='coverage' else '<text x="30" y="674" class="map-caption">Department-wide planning scores · facility-only departments remain outside the demand/supply classification</text>'), '</svg>']
    return '<div class="mapbox">' + ''.join(out) + '</div>'

def bar_chart():
    out = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1200 570" class="bar-chart" role="img" aria-label="Weighted use and intention among 1905 women with original SURGO FPN2 equals one">']
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
    out.append('<text x="710" y="562" text-anchor="middle" class="axis-label">Weighted percentage of all 1,905 women with SURGO FPN2 = 1</text></svg>')
    return ''.join(out)

def mix(a,b,t):
    ac,bc = ([int(s[i:i+2],16) for i in (1,3,5)] for s in [a,b])
    return '#'+''.join(f'{round(x+(y-x)*min(1,max(0,t))):02x}' for x,y in zip(ac,bc))

def heatmap():
    out = ['<div class="table-wrap"><table class="paired-heatmap"><thead><tr><th rowspan="2">Department</th><th colspan="6" class="demand-group">Women’s reported concerns · SURGO</th><th colspan="6" class="supply-group">Recorded provision · Manitoba</th></tr><tr>']
    out += [f'<th>{esc(labels[k])}</th>' for k in D+S]
    out += ['</tr></thead><tbody>']
    for g in ['SENEGAL']+areas:
        c = cov[g]
        title = 'Senegal · linked departments' if g == 'SENEGAL' else name(g)
        mark = ' †' if g in small else ''
        out.append(f'<tr class="{"overall" if g == "SENEGAL" else ""}"><th>{esc(title)}{mark}<small>n={c["n_need"]:,} women in need · {c["facilities"]:,} facilities</small></th>')
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
    co = {(r['model'],r['term']):r for r in data['coefficients']}
    tests = {(r['model'],r['block']):r for r in data['tests']}
    gaps = {r['term']:r for r in data['gaps']}
    units = 'one oriented PC1 unit' if pca else 'one original average-z index point'
    s = '<h2>Modern use in relation to personal concerns and local services</h2>' if not pca else '<h3>The same five modern-use models using PCA scores</h3>'
    s += f'<p class="model-meta">All models: <b>{au["common_n"]:,} women with original SURGO FPN2 = 1</b>, including {au["common_events"]:,} modern users across {au["districts"]} departments. Outcome: current modern use = 1; confirmed traditional-only use or nonuse = 0. Adjust for age + age², Senegal-specific education, partnership, fertility goal and Rural/Urban residence. Use WEIGHT, department-clustered CR2/Satterthwaite intervals, and no random effects or interactions.</p>'
    if not pca:
        s += table(['Score assigned to each woman','Construction','Observed model variation'],[
            ['Demand risk score','Standardize her own six demand responses using weighted item means and population SDs among valid FPN2 = 1 women; average all six. Her department or residence does not determine this score.',f'{au["demand_distinct_values"]} distinct values across {au["common_n"]:,} women.'],
            ['Supply risk score','Standardize her assigned six residence-specific or flagged department-fallback measures against the fixed 33-department planning means/SDs. Average and reverse the sign: higher = weaker provision.',f'{au["supply_distinct_values"]} distinct numeric scores across {au["assigned_context_profiles"]} department–residence contexts; {au["fallback_model_women"]} modeled women receive fallbacks.']])
    s += table(['Model','Predictors alongside the same covariates'],[
        ['1 · Demand score','Personal demand score'],['2 · Supply score','Assigned supply risk score'],
        ['3 · Both scores','Personal demand score + assigned supply risk score'],
        ['4 · Six demand indicators','Each woman’s six demand indicators separately'],
        ['5 · Six indicators + supply','Six personal demand indicators + assigned supply risk score']])
    s += figure(folder+'/01_score_models.svg','Adjusted odds ratios for demand-only, supply-only and joint modern-use models, with 95% department-clustered confidence intervals.')
    s += note(f'<strong>Key finding.</strong> In Model 3, the demand odds ratio is <b>{ort(co["score_joint","demand_score"])}</b> and the supply risk odds ratio <b>{ort(co["score_joint","supply_risk"])}</b> per {units}. The demand association remains after accounting for recorded supply and covariates. The supply interval includes 1, so its independent association remains uncertain. These do not allocate a percentage of nonuse to either side.')
    s += figure(folder+'/02_score_probabilities.svg','Weighted average adjusted modern-use probabilities across observed demand and assigned supply score ranges.')
    s += note('<strong>Planning interpretation.</strong> The curves show how predicted modern use varies with each score for the same women, holding other predictors at their observed values. Their levels and uncertainty are easier to interpret than odds ratios. They are adjusted cross-sectional comparisons, not expected gains from removing barriers. An uncertain supply slope does not establish adequate services.')
    s += '<h3>Which reported concerns accompany lower use?</h3><p class="model-note">Models 4 and 5 retain personal responses separately. Model 5 also includes the same contextual supply score. The supply-only model is Model 2, shown again for reference.</p>'
    s += figure(folder+'/03_component_models.svg','Individual reported concerns and assigned supply risk alongside covariates in modern-use models.')
    s += note(f'<strong>Key finding.</strong> In Model 5, opposition to family planning has OR <b>{ort(co["components_joint","opposes"])}</b>; anticipated stigma has OR <b>{ort(co["components_joint","stigma"])}</b>. The six demand coefficients are jointly associated with use (Holm p {fp(tests["components_joint","Demand"]["holm_p"])}). Use the heatmap to locate these concerns alongside service weaknesses; these associations do not establish which intervention would be most effective.')
    s += figure(folder+'/04_component_probability_gaps.svg','Adjusted modern-use percentage-point differences for each concern, present versus absent, with paired confidence intervals.')
    s += note(f'<strong>What this means for planning.</strong> The adjusted present-versus-absent difference is <b>{100*gaps["opposes"]["estimate"]:.1f} percentage points</b> for opposition and <b>{100*gaps["stigma"]["estimate"]:.1f} points</b> for anticipated stigma. The model predicts the same women under each response setting, retains their supply and other characteristics, and averages using WEIGHT. These gaps identify patterns to investigate; they are not intervention-effect estimates.')
    coefrows = [[esc(labels[k])]+[ort(co[mid,k]) if (mid,k) in co else 'Not included' for mid in ['components_demand','supply','components_joint']] for k in D+['supply_risk']]
    probrows = [[esc(labels[k]),pct(100*gaps[k]['probability_absent']),pct(100*gaps[k]['probability_present']),f'{100*gaps[k]["estimate"]:+.1f} ({100*gaps[k]["lo"]:+.1f} to {100*gaps[k]["hi"]:+.1f})'] for k in D]
    testrows = [[label,b,str(tests[mid,b]['df_num']),fp(tests[mid,b]['p']),fp(tests[mid,b]['holm_p'])] for mid,label in [('score_joint','Both scores'),('components_joint','Six indicators + supply')] for b in ['Demand','Supply']]
    s += '<details class="model-detail"><summary>Exact estimates, adjusted probabilities and joint tests</summary>'
    s += table(['Predictor','Six indicators','Supply only','Six indicators + supply · OR (95% CI)'],coefrows)
    s += table(['Concern','Predicted use: absent','Predicted use: present','Difference, pp (95% CI)'],probrows)
    s += table(['Joint model','Block','Restrictions','Raw p','Holm p'],testrows)
    s += '<p>Each joint model has two tests. Demand tests its one score coefficient or all six concern coefficients together; supply tests its one score coefficient. Holm correction applies within those two tests per model. These tests do not establish whether demand matters more than supply, equality between them, or causal binding constraints.</p>'
    s += downloads([(folder+'/model_coefficients.csv','All coefficients, including covariates'),(folder+'/model_block_tests.csv','Joint tests'),(folder+'/score_predicted_probabilities.csv','Probability curves'),(folder+'/individual_demand_probability_gaps.csv','Predicted probabilities and differences')])+'</details>'
    return s

css = (Path(__file__).resolve().parent.parent/'common/report.css').read_text() + '''
body{margin:0;background:#f5f8fa;color:#233e4e;font-family:Arial,Helvetica,sans-serif}main{max-width:1480px;margin:auto;padding:0 30px}.report-header{max-width:1420px;margin:32px auto 24px;padding:34px 36px;background:#143e51;color:white;border-radius:14px}.report-header h1{font-size:38px;line-height:1.12;margin:8px 0 16px;color:white}.report-header p{max-width:1110px;color:#e0edf2;line-height:1.65}.report-header .eyebrow{color:#b3dcdd}.report-header nav{display:flex;flex-wrap:wrap;gap:9px;margin-top:22px}.report-header nav a{color:white;border:1px solid #71909e;border-radius:20px;padding:7px 13px;text-decoration:none;font-size:13px}section{margin:26px 0;padding:30px;background:white;border-radius:12px;border:1px solid #dce6ea}h2{font-size:28px;line-height:1.25}h3{color:#164b5b;margin:28px 0 13px}.eyebrow{letter-spacing:.1em;font-size:12px;font-weight:700;color:#24717c}.intro,.model-note{line-height:1.65}.metrics{display:grid;grid-template-columns:repeat(4,1fr);gap:16px;margin:22px 0}.metric{padding:18px;border:1px solid #dce6ea;border-radius:9px}.metric b{display:block;font-size:29px;color:#125967}.metric span{font-size:13px;color:#677f8c}.report-warning{background:#fff8ed;border-left:4px solid #c48035;padding:16px 18px;line-height:1.65;border-radius:3px;font-size:14px}.mapbox{background:#f8fbfc;border:1px solid #e0e9ec;border-radius:9px;overflow:hidden}.senegal-map{display:block;width:100%;height:auto}.district{cursor:pointer}.district:hover,.district:focus{filter:brightness(.85);stroke:#173b50;stroke-width:2;outline:none}.facility-dot{cursor:pointer}.map-label{font-size:11.5px;fill:#203d4d;font-weight:600;paint-order:stroke;stroke:white;stroke-width:3;stroke-linejoin:round}.inset-heading{font-size:16px;fill:#174758;font-weight:700}.map-caption{font-size:12px;fill:#64808b}.legend{display:flex;flex-wrap:wrap;gap:17px;margin:15px 0;font-size:13px}.legend i{display:inline-block;width:14px;height:14px;border-radius:3px;margin-right:6px;vertical-align:middle}.bar-chart{display:block;width:100%}.bar-label{font-size:15px;fill:#244253;font-weight:600}.bar-label.sub{font-size:13px;font-weight:400}.bar-value{font-size:14px;font-weight:600;fill:#203d4c}.axis-label{font-size:13px;fill:#647d8a}.paired-heatmap{width:100%;min-width:1200px;border-collapse:separate;border-spacing:4px;font-size:13px}.paired-heatmap th{padding:10px 4px;text-align:center;vertical-align:bottom;font-size:12px}.paired-heatmap tbody th{text-align:left;min-width:205px;font-size:13px}.paired-heatmap small{display:block;font-size:10px;font-weight:400;color:#6e8793;margin-top:4px}.paired-heatmap td{text-align:center;font-size:14px;font-weight:700;padding:14px 5px;border-radius:2px}.demand-group{color:#087c88;background:#eef8f8}.supply-group{color:#b66b20;background:#fff6e8}.overall th{color:#086d78}.overall td{outline:1px solid #234a5c}.unknown-cell{background:#edf0f3;color:#7d8d96}.table-wrap{overflow:auto}.planning{width:100%;border-collapse:collapse}.planning th,.planning td{padding:12px 11px;vertical-align:top;text-align:left;line-height:1.5;border-bottom:1px solid #e4ecef}.planning thead{background:#f0f6f8}.planning th,.planning td{font-size:13px}.planning td ul{margin:0;padding-left:18px}.planning td li{margin:0 0 5px}.sample-badge{display:block;margin-top:5px;font-size:10px;font-weight:600;color:#98581a}.category-tag{display:inline-block;border-radius:4px;padding:5px 8px;white-space:nowrap;font-size:12px;font-weight:700}.model-figure{margin:23px 0 8px}.model-figure img{display:block;width:100%;height:auto}.model-meta{font-size:14px;line-height:1.7}.model-key{margin:10px 0 24px;background:#edf7f7;padding:15px 18px;border-left:4px solid #148591;font-size:14px;line-height:1.7}.model-detail{margin:18px 0;border:1px solid #dce7eb;border-radius:7px;padding:14px 17px}.model-detail summary{font-weight:700;color:#164b5b;cursor:pointer}.model-detail p{font-size:13px;line-height:1.7}.table-heading{display:flex;flex-wrap:wrap;gap:13px;margin:15px 0}.download{font-size:12px}.footnote{font-size:12px;color:#637d8b;line-height:1.7}.score-steps{font-size:14px;line-height:1.7;padding-left:22px}.score-steps li{margin:10px 0}.wide-break{height:1px;background:#dce7eb;margin:30px 0}.tooltip{display:none;position:fixed;background:#173d50;color:white;border-radius:9px;padding:16px;width:300px;z-index:1000;box-shadow:0 9px 35px #173d5040;font-size:13px;pointer-events:none}.tooltip.show{display:block}.tooltip strong{display:block;font-size:17px;color:white;margin-bottom:7px}.tooltip p{margin:7px 0;color:#e0edf1}.tooltip .two{display:flex;justify-content:space-between;margin:12px 0}.tooltip b{font-size:20px}.tooltip small{color:#c8dbe3}.tooltip .sample-alert{background:#fff0cb;color:#5c421b;padding:8px;border-radius:4px}@media(max-width:850px){main{padding:0 10px}section{padding:18px}.report-header{margin:12px;padding:24px}.report-header h1{font-size:28px}.metrics{grid-template-columns:repeat(2,1fr)}}@media print{body{background:white}.report-header,section{border-radius:0}details{display:block}nav,.download{display:none}.mapbox,.model-figure{break-inside:avoid}}
'''
co = {(r['model'],r['term']):r for r in m['coefficients']}
header = '''<header class="report-header"><span class="eyebrow">SURGO × UNIVERSITY OF MANITOBA · SENEGAL</span><h1>Demand, services and modern contraceptive use</h1><p>Women’s own reported concerns are examined alongside the recorded service environment where they live. Looking at both can identify places where social support, service improvements or integrated responses warrant attention.</p><p><strong>Initial evidence:</strong> personal concern profiles, especially opposition and anticipated stigma, are associated with lower modern use after adjustment. The selected supply score’s independent association remains uncertain. Geographic profiles identify relative concerns and service weaknesses; they do not establish causal binding constraints.</p><nav><a href="#coverage">1 · Coverage and linkage</a><a href="#use">2 · Use and intention</a><a href="#joint">3 · Joint conditions and priorities</a><a href="#models">4 · Adjusted models</a><a href="#definitions">Definitions and checks</a><a href="#pca">Appendix · PCA</a></nav></header>'''
coverage_legend = '<div class="legend">' + ''.join(f'<span><i style="background:{coverage_colors[k]}"></i>{coverage_labels[k]}: <b>{sum(v["coverage_group"] == k for v in DATA.values())}</b></span>' for k in ['both','supply_only','demand_only']) + '<span><i style="background:#123d50;border-radius:50%"></i>Department facility-count dots</span></div>'
fallback_rows = [[esc(name(g)),str(assign[g]['rural_classified_facilities']),str(assign[g]['urban_classified_facilities']),str(assign[g]['fallback_women']),str(assign[g]['fallback_need'])] for g in areas if assign[g]['fallback_women']]
coverage = f'''<section id="coverage"><span class="eyebrow">01 · COVERAGE AND LINKAGE</span><h2>Women and facilities overlap in 33 departments</h2><p class="intro">The individual-level join retains all <b>7,169 original women</b>. Manitoba contains 2,043 facility records across 46 departments: 33 also have SURGO respondents, and 13 have facilities only. The map shows source coverage before restricting the analytical population.</p><div class="metrics"><div class="metric"><b>7,169</b><span>Original SURGO women retained</span></div><div class="metric"><b>33</b><span>Departments with both sources</span></div><div class="metric"><b>2,043</b><span>Facility records across 46 departments</span></div><div class="metric"><b>1,905</b><span>Women with original SURGO FPN2 = 1</span></div></div>{coverage_legend}{map_svg('coverage')}<p class="footnote">Dots indicate department totals, not actual facility coordinates. All 33 surveyed departments have a department-wide facility profile; 1,648 facility records contribute to those profiles. The other 395 records are in 13 departments without SURGO respondents. Boundaries: <a href="https://data.humdata.org/dataset/cod-ab-sen">Government of Senegal / OCHA, archived department layer</a>. Health districts are assigned using the documented completed-geography crosswalk; qualified Diakhao, Sangalkam and Yeumbeul assignments remain disclosed in the input audit.</p>{downloads([('department_data_coverage.csv','Coverage counts'),('map_name_crosswalk.csv','Boundary crosswalk')])}<div class="wide-break"></div><h3>How each woman receives her supply measures</h3>{table(['Available facility profile','Assignment to the woman'],[['Her department + Rural','A Rural woman receives summaries of Rural-labelled facilities in her own department.'],['Her department + Urban','An Urban woman receives summaries of Urban-labelled facilities in her own department.'],['No profile for her department–residence combination','Use all included facilities in her own department; flag this as a department-wide fallback.']])}<div class="report-warning"><strong>Residence-classification limitation and decision.</strong> Facility Rural/Urban labels come from a <b>health-district lookup</b>, not a directly verified settlement classification for each facility. In Mbacke, all <b>64 facility records are Urban-labelled</b>, while SURGO contains 529 Urban and 253 Rural women. Urban women receive the Urban-labelled facility profile; Rural women receive a flagged department-wide fallback. Among women with FPN2 = 1, this is <b>136 Urban women and 56 Rural women</b>. The department-wide and Urban-labelled profiles are numerically identical here. This does not prove that rural services are absent or that Rural women attended Urban facilities.</div><p class="model-note">This rule applies to <b>1,815 source women across 16 departments</b>, including <b>522 women in need</b>. The remaining 1,383 women in need have exact department–residence profiles. No unmatched measure is coded zero. Existing profiles with an unanswered component would retain that missing component; the fallback rule is triggered by an absent geographic profile.</p><details class="model-detail"><summary>Departments receiving fallbacks and affected women</summary>{table(['Department','Rural-labelled facilities','Urban-labelled facilities','Source women receiving fallback','Women in need receiving fallback'],fallback_rows)}{downloads([('department_residence_assignment_summary.csv','Residence assignment and fallback counts'),('assigned_supply_profiles.csv','Assigned supply profiles')])}</details></section>'''

unknown_method = bars['user']['n'] - bars['modern']['n'] - bars['traditional']['n']
unknown_preference = bars['user']['n'] - bars['wanted_start']['n'] - bars['other_start']['n']
use = f'''<section id="use"><span class="eyebrow">02 · USE, INTENTION AND METHOD CHOICE</span><h2>Among women SURGO classifies as in need of family planning</h2><div class="report-warning"><strong>Population used from this section onward:</strong> original SURGO <b>FPN2 = 1</b>, exactly as recorded in the source survey. This gives <b>1,905 women</b>. No older script’s need_n recoding or sterilization/hysterectomy additions are applied. Eleven nonconsenting records are outside estimation and have FPN2 = 95.</div>{bar_chart()}<p class="footnote">Every bar and sub-bar has the same denominator: all 1,905 women with FPN2 = 1. Percentages use <b>WEIGHT</b>; n is an unweighted respondent count. Capped whiskers are 95% logit intervals using the available RB0H census-district design, constructed before domain selection. Original sampling strata and finite-population corrections are unavailable. Subsets overlap: modern users are included in current users, so these bars do not sum to 100%. {unknown_method} users have unresolved modern/traditional classification; {unknown_preference} have another or unresolved initiation-preference response. *Preference concerns the method wanted at initiation; the type of alternative wanted is not established. Future intention concerns any method, without a specified 12-month initiation window.</p>{note(f'<strong>Initial pattern.</strong> Weighted current use is <b>{100*bars["user"]["estimate"]:.1f}%</b>, including <b>{100*bars["modern"]["estimate"]:.1f}%</b> modern use. Another <b>{100*bars["intender"]["estimate"]:.1f}%</b> are nonusers with intention and <b>{100*bars["no_intent"]["estimate"]:.1f}%</b> nonusers without stated intention. These groups help distinguish existing use from stated future interest without treating this cross-sectional survey as a transition study.')}{downloads([('use_distribution.csv','Use and intention estimates'),('department_use_rates.csv','Department use estimates')])}</section>'''

comparison_rows = [[area_label(g)]+[tag(scores[k,g]['category']) for k in methods] for g in areas]
method_rows = [
 ['Rank average','Rank departments separately for each component, using average ranks for ties; average six ranks on each side.','Demand above its final-score median; favorable supply below its median.'],
 ['Mean percentages','Average the six original percentages separately on each side.','Same strict final-score median rule.'],
 ['Median/IQR','For each component, subtract its department median and divide by its IQR; average six transformed values.','Same strict final-score median rule.'],
 ['Median count','Count components above the demand component medians, or below the favorable supply component medians.','Flag at least four of six unfavorable comparisons.'],
 ['Z-score average · primary','For each component, subtract its department mean and divide by its sample SD; average six component z-scores.','Same strict final-score median rule.']]
threshold = cuts['zscore']
count_category = {k:sum(scores['zscore',g]['category']==k for g in areas) for k in ['both','demand','supply','neither']}
legend = '<div class="legend">'+''.join(f'<span><i style="background:{colors[k]}"></i>{catnames[k]}: <b>{count_category.get(k,13)}</b></span>' for k in ['both','demand','supply','neither','supply_only'])+'</div>'
priority_rows = []
for g in areas:
    q = planning[g]
    def tops(side, fields):
        return '<ul>'+''.join(f'<li>{esc(labels[k])} <b>{pct(hm[g,side,k]["value"])}</b></li>' for k in fields)+'</ul>'
    priority_rows.append([area_label(g),'<b style="color:#087c88">'+('Yes' if q['demand_flag'] else 'No')+'</b>',
        tops('Demand',q['top_demand']) if q['demand_flag'] else '—',
        '<b style="color:#b66b20">'+('Yes' if q['supply_flag'] else 'No')+'</b>',
        tops('Supply',q['top_supply']) if q['supply_flag'] else '—'])
joint = f'''<section id="joint"><span class="eyebrow">03 · JOINT CONDITIONS AND PLANNING PRIORITIES</span><h2>Reported concerns alongside recorded services</h2><p class="intro">Read across a department: women’s reported concerns on the left and recorded facility provision on the right. Demand percentages use <b>only women with FPN2 = 1</b> and valid domain responses. Supply summarizes all included facilities in that department, combining residence types, so the planning heatmap is comparable across departments.</p>{heatmap()}<p class="footnote">Darker teal means more concern; darker orange means weaker provision. Displayed supply percentages all run higher = stronger provision. Hover cells for valid-response denominators. The Senegal row pools women in need and the 1,648 facilities in linked departments. Facility summaries are unweighted and preserve original UoM definitions. The three-LARC stock measure is constant in Senegal; the already-joined <b>at-least-one-LARC stock</b> measure is used instead. Counseling has sparse reporting and must be read with its valid denominators. † Fewer than 50 women in need.</p>{downloads([('heatmap_values_and_denominators.csv','Heatmap values and denominators'),('demand_missingness_by_department.csv','Demand response coverage')])}<div class="wide-break"></div><h3>How classifications compare across five scoring approaches</h3>{table(['Department']+method_names,comparison_rows)}<details class="model-detail"><summary>How each comparison method works</summary>{table(['Approach','Score construction','Flagging threshold'],method_rows)}<p>These are relative planning classifications, not validated service-adequacy thresholds. Every component receives equal weight. Exact median ties are unflagged. Rankings order the final scores; the map uses the continuous z-score averages and their score medians, not an additional ranking cutoff.</p></details>{downloads([('classification_comparison.csv','Five-method comparison'),('classification_thresholds.csv','Exact thresholds'),('component_score_trace.csv','Component values and transformations')])}<div class="wide-break"></div><h3>Department planning profiles · z-score average</h3><details open class="model-detail"><summary>How the scores and map thresholds are constructed</summary><ol class="score-steps"><li><b>Summarize each department.</b> Demand inputs are six WEIGHT-weighted concern percentages among valid FPN2 = 1 women. Supply inputs are six unweighted facility summaries using all facilities within each department.</li><li><b>Standardize each component across the 33 linked departments.</b> Calculate (department value − department mean) ÷ sample SD. Each department contributes equally to this reference. Higher demand values mean more concern; higher raw supply values mean stronger provision.</li><li><b>Average six standardized components on each side.</b> This gives department demand and favorable supply-readiness scores. These contextual demand scores are used for the map; the models instead use each woman’s own responses.</li><li><b>Apply separate final-score medians.</b> Flag demand strictly above <b>{threshold['demand_cutoff']:.6f}</b>. Flag supply readiness strictly below <b>{threshold['supply_cutoff']:.6f}</b>, equivalent to supply risk above <b>{-threshold['supply_cutoff']:.6f}</b>. Exact ties are not flagged.</li></ol><p>Both flags → Both; one flag → Demand only or Supply only; no flags → Neither flagged. The 13 facility-only departments are outside the demand/supply comparison. “Neither flagged” does not establish adequate services or no barriers. Small samples do not change these relative scores or thresholds.</p></details>{legend}{map_svg('classification')}{note(f'<strong>Planning signal.</strong> Among the 33 linked departments, <b>{count_category["both"]}</b> have both sides flagged, <b>{count_category["demand"]}</b> demand only, <b>{count_category["supply"]}</b> supply only, and <b>{count_category["neither"]}</b> neither. Use the component priorities below to identify what should be investigated in each place; a category alone is insufficient to choose an intervention.')}<p class="footnote"><b>Small samples:</b> † marks fewer than 50 unweighted women in need. {sum(rates[g]['n'] < 25 for g in areas)} departments have fewer than 25. These exploratory estimates are retained with explicit flags; department categories and rankings can be unstable. Consult component-specific denominators and use modeled uncertainty rather than treating these maps as precise population rankings.</p>{downloads([('department_sample_flags.csv','Department sample-size flags')])}<h3>Department planning priorities</h3>{table(['Department','Demand challenges','Top three demand components','Supply challenges','Top three service weaknesses'],priority_rows)}<p class="footnote">Top concerns have the largest component z-scores; top service weaknesses have the smallest favorable component z-scores. Lists appear for flagged sides only. The percentages describe recorded conditions, not estimated intervention effects. This table and map use all-facility department summaries; residence-specific or fallback exposure is reserved for the woman-level models.</p>{downloads([('planning_priorities.csv','Planning priorities'),('all_method_scores_and_classifications.csv','All scores and categories')])}</section>'''
model = '<section id="models"><span class="eyebrow">04 · ADJUSTED ASSOCIATIONS</span>'+models(m,'models_zscore')+'</section>'

demand_defs = [
 ['Healthcare permission barriers','HS7C: 1 = permission to seek a doctor is a challenge; 2 = no. General healthcare permission, not FP-specific partner approval.'],
 ['Anticipated social stigma','PB7B/C: 1/2 on either item, concerning being seen by someone known or what people would say. Zero requires both valid negatives (3/4); unresolved responses remain missing unless the other item establishes concern.'],
 ['Health / side-effect beliefs','PB2A–C: 1/2 on any item about health problems, reduced sexual desire or malformed babies; zero requires all valid 3/4 responses. Bleeding is excluded. These are perceived beliefs, not clinically verified effects.'],
 ['Future-fertility concerns','Any MP4_a–f selected for later childbearing concerns about reversible methods. Require coherent MP4_a–i 0/1 responses, at least one selection or explicit none, no none-plus-method contradiction, and MP4_99 = 0. Sterilization items MP4_g/h do not create a fertility concern; unknown responses are not zero.'],
 ['Low method knowledge · retained label','MC5A/B: 3/4 on either item, measuring low confidence choosing a suitable method or handling side effects/switching. This is self-efficacy/confidence, not an objective knowledge test. Zero requires both valid 1/2 answers.'],
 ['Opposes family planning','FP1: 3/4 = against versus 1/2 = in favor. Unfamiliarity, no opinion and other special codes remain unresolved.']]
supply_defs = [
 ['Short + long-acting offering','Average one_larc_3_sa_offer, joined as share_offer_larc_and_3_sa. Share meeting the original ≥1 long-acting plus ≥3 short-acting product/category rule. Higher = stronger recorded provision.'],
 ['Free-product provision','Average valid facility p_free_prod proportions, joined as mean_p_free_prod. A value of 60% means the average reporting facility provides 60% of its recorded products free; it is not the percentage of facilities offering any free method.'],
 ['Trained-provider share','Average valid facility p_train proportions, joined as mean_p_train. Describes the average reporting facility’s trained-personnel proportion under the original UoM eligibility; not the share of facilities with at least one trained provider.'],
 ['FP counselor availability','Senegal item s7_2: Yes/(Yes+No), joined as share_fp_counselor. Codes 1 = Yes; 0/2 = No; unknown responses excluded. Availability is not counseling receipt or quality. Reporting coverage is sparse.'],
 ['At least one LARC in stock','Average one_larc_stockin, joined as share_stocked_1plus_larc. Share meeting the original at-least-one-long-acting-product stock rule. The three-LARC measure is constant FALSE and cannot be standardized.'],
 ['Most-popular product in stock','n_mp_in_stock_true/(true+false). Percentage among facilities with known status, each facility referring to its own most-popular product. This does not measure each woman’s preferred method.']]
question_text = {
 'HS7C':'Is getting permission to visit a doctor usually a challenge when seeking care?',
 'PB7B':'How afraid would you be of someone you know seeing you at a facility for FP services?',
 'PB7C':'How worried would you be about what people would say if they learned you used FP services?',
 'PB2A':'How likely are women using methods such as implants or injectables to develop health problems?',
 'PB2B':'How likely are women using these methods to experience reduced sexual desire?',
 'PB2C':'How likely are women using these methods to have malformed babies?',
 'MP4_99':'Which methods could affect later childbearing after stopping? MP4_a–f identify reversible methods; MP4_99 records “don’t know”.',
 'MC5A':'How confident are you that you could choose a suitable FP method?',
 'MC5B':'How confident are you that you could manage bothersome side effects or switch methods?',
 'FP1':'Are you in favor of or against FP to delay or avoid pregnancy?'
}
item_unknowns = list(csv.DictReader((root/'checks/unresolved_item_responses.csv').open()))
question_rows = [[esc(r['variable']),esc(question_text[r['variable']]),r['blank_n'],r['n'],pct(float(r['weighted_pct']))] for r in item_unknowns]
unknowns = list(csv.DictReader((root/'checks/demand_unresolved_summary.csv').open()))
unknown_rows = [[esc(labels[r['variable']]),r['n_unresolved'],pct(float(r['weighted_pct']))] for r in unknowns]
missing = list(csv.DictReader((root/'models_zscore/model_missingness.csv').open()))
missrows = [[esc(labels.get(r['variable'],r['variable'])),r['missing_in_need']] for r in missing if int(r['missing_in_need'])]
sample_rows = [[r['model'],str(r['n']),str(r['events']),str(r['parameters']),f'{r["max_vif"]:.2f}'] for r in m['samples']]
sample_flow = list(csv.DictReader((root/'checks/model_sample_flow.csv').open()))
flow_rows = [[esc(r['step']),r['excluded_at_step'],r['remaining']] for r in sample_flow]
sample_compare = list(csv.DictReader((root/'checks/model_sample_comparison.csv').open()))
sample_compare_rows = [[esc(r['group']),r['n'],pct(100*float(r['weighted_share_of_need'])),pct(100*float(r['modern_use_among_known'])),pct(100*float(r['urban_share']))] for r in sample_compare]
den_rows = [[area_label(g),str(cov[g]['facilities'])]+[str(hm[g,'Supply',k]['n_valid']) for k in ['training','counselor','popular_stock']] for g in areas]
correlations = list(csv.DictReader((root/'checks/component_correlations.csv').open()))
cor_rows = []
for side in ['Demand','Supply']:
    pairs = sorted([r for r in correlations if r['side']==side and r['item1']<r['item2']],key=lambda r:abs(float(r['correlation'])),reverse=True)[:3]
    cor_rows += [[side,esc(labels[r['item1']]),esc(labels[r['item2']]),f'{float(r["correlation"]):+.3f}',r['n_pair']] for r in pairs]
defs = f'''<section id="definitions"><span class="eyebrow">DEFINITIONS AND VERIFICATION</span><h2>How to interpret the measures and their limits</h2><details open class="model-detail"><summary>Demand variables and missing-response rules</summary>{table(['Indicator','Source and interpretation'],demand_defs)}<p>These modules have responses among current users and nonusers and are not restricted to married women. Special codes are excluded from valid item denominators. A known positive can establish an any-concern flag; zero requires valid negatives on every included item. Do not interpret unresolved responses as absence of concern.</p><h3>Questions and uncertainty responses among women in need</h3>{table(['Item','Question · shortened wording','Blank responses','Don’t know / unclassifiable, n','Weighted percentage'],question_rows)}<p>Questions are shortened from the supplied Senegal SURGO dictionary. Counts cover original FPN2 = 1 women. FP1’s unclassifiable responses comprise unfamiliarity (5) and no opinion (99), rather than only “don’t know”. MP4 is one multiple-response question; its “don’t know” option governs interpretation of the method selections.</p><h3>Where uncertainty affects the combined demand indicators</h3>{table(['Demand indicator','Unresolved women, n','Weighted unresolved percentage'],unknown_rows)}<p>Health and fertility account for most unresolved domains. PB2B (reduced sexual desire) has 403 “don’t know” answers; PB2C (malformed babies) has 385; PB2A (health problems) has 113. A known concern on another item can still establish a positive health flag, leaving 264 health domains unresolved overall. MP4_99 is explicitly “don’t know which methods could affect later fertility”: 244 women select it, including two who also select a method. Confidence has 34 unresolved domains. FP1 has 17 unfamiliarity and 9 no-opinion responses, which cannot be classified as either opposition or support.</p><p>These are largely substantive uncertainty responses, not necessarily data errors. The binary concern scores do not assign uncertainty a risk value. Coding unknowns as no concern is unsupported; excluding them may select a more informed subset. Department percentages use valid domain responses, while the primary models require a complete personal profile. Counts overlap across domains; the percentages above use WEIGHT.</p>{downloads([('checks/demand_unresolved_summary.csv','Unresolved domain counts and weighted percentages'),('checks/unresolved_item_responses.csv','Underlying uncertainty responses')])}</details><details open class="model-detail"><summary>Supply variables and denominators</summary>{table(['Indicator','Calculation and interpretation'],supply_defs)}<p>No facility sampling weights are constructed. Valid-response aggregation follows the existing UoM facility definitions. A missing answer is not automatically evidence of no provision. The same definitions are applied within departments and within department–residence cells, before assigning profiles to women.</p>{table(['Department','Facility records','Training responses','Counselor responses','Known popular-stock status'],den_rows)}<p>Among the 1,648 facilities in linked departments, {sum(hm[g,'Supply','counselor']['n_valid'] for g in areas)} have valid counselor responses. The indicator describes those reporting facilities; sparse response limits inference about all facilities. Nine of the 2,043 country facility records have no residence lookup label; they contribute to department-wide summaries, not residence-specific profiles.</p>{downloads([('supply_input_reconciliation.csv','All 492 facility-profile checks'),('department_supply_profiles.csv','Department-wide profiles'),('assigned_supply_profiles.csv','Assigned residence/fallback profiles')])}</details><details class="model-detail"><summary>Original SURGO need classification and analytical population</summary><p>Sections 2 onward use <b>original FPN2 = 1</b>, with original WEIGHT and consent. The source records 1,905 women as in need, 5,253 as not in need and 11 nonconsenting records with code 95. Among the 7,158 consenting positive-weight respondents, the weighted in-need share is {100*AUD['weighted_need_share']:.1f}%. This report does not reconstruct that flag, use an older researcher’s need_n, add permanent-protection cases, or apply DHS unmet-need coding. All original survey fields, including FPN2, are independently verified as unchanged.</p></details><details open class="model-detail"><summary>Survey design, adjustment and common model sample</summary><p>Use <b>WEIGHT</b> from the Senegal source. The available survey design uses <b>RB0H</b>, explicitly labelled census district, with {AUD['survey_psus']} recorded clusters among consenting respondents; cluster nesting in departments and residence is verified. Construct the full consenting positive-weight design before selecting FPN2 = 1. Original sampling strata and FPC are not documented in the supplied materials, so complete reconstruction of the original complex design cannot be claimed.</p><p>Models retain these weights and use department-clustered CR2 covariance with coefficient-specific Satterthwaite degrees of freedom. Sampling-census-district survey-GLM intervals are exported as an inference sensitivity; weighted point estimates agree. The models adjust for age + age², education (primary or less; middle/secondary; tertiary/professional), partnership, fertility goal (five categories including after marriage and uncertain), and FV_MILIEU Rural/Urban. There are no random effects or interactions.</p>{table(['Model','Women','Modern users','Parameters','Maximum VIF'],sample_rows)}<p>All ten z-score/PCA models use the same <b>1,445 women</b> and converge with full-rank designs and finite coefficients. All 1,905 women have complete assigned supply inputs after geographic fallbacks. The 460 common-model exclusions arise from unresolved demand, outcome or covariate values; they are not removed from the source join or overview. The common sample represents {100*MA['eligible_weight_share']:.1f}% of need-group survey weight. Missingness may be informative, so complete-case results need not represent all women in need.</p>{table(['Sample selection step','Excluded at this step','Women remaining'],flow_rows)}<p>Demand-item missingness counts overlap; the flow removes each woman once. Complete-case selection excludes 24.1% of respondents. The supply-only model could use a larger sample, but the primary five-model comparison deliberately uses the same women to isolate changes in predictor blocks.</p>{table(['Group','Women','Share of need-group weight','Modern use among known outcomes','Urban share'],sample_compare_rows)}{table(['Variable','Missing/unresolved among 1,905 women'],missrows)}<p>There are <b>39 distinct numeric supply scores across 49 woman-occupied department–residence contexts</b>, and 33 departments for clustered inference. Fallbacks can equal an Urban-labelled profile numerically; this does not create new independent supply information. The joint z-score supply coefficient has {co['score_joint','supply_risk']['df']:.1f} effective degrees of freedom. Diagnostics and VIF checks are not a formal power guarantee. Cross-sectional associations do not identify causal mechanisms, and current experience can influence reported beliefs. Confidence intervals condition on the estimated scores.</p>{downloads([('models_zscore/model_samples.csv','Sample and convergence'),('models_zscore/model_missingness.csv','Missingness'),('models_zscore/model_vifs.csv','Weighted VIF'),('models_zscore/leave_one_department_out.csv','Department-deletion checks'),('models_zscore/sampling_cluster_model_sensitivity.csv','Sampling-cluster inference sensitivity'),('models_zscore/individual_demand_score_reference.csv','Personal-score means and SDs'),('models_zscore/supply_score_reference.csv','Supply-score normalization references'),('checks/model_sample_comparison.csv','Included versus excluded women'),('checks/model_sample_flow.csv','Model sample-selection flow')])}</details><details class="model-detail"><summary>Indicator overlap and independent checks</summary>{table(['Side','Component 1','Component 2','Correlation','Paired records'],cor_rows)}<p>Demand correlations use personal responses among women in need and final WEIGHT. Supply correlations use one unweighted department-wide profile per department. Correlated availability features can receive overlapping weight in the index; equal weighting does not imply independent domains or equal policy importance. Low model VIFs do not validate the composite’s content.</p><p>Independent verification passes {verification['check_count']} checks, including all {verification['source_raw_columns_verified']} raw survey columns, 492 facility/profile components, 43,014 woman-level supply assignments, all ten modern-use fits and covariance matrices, and PCA scores reproduced using weighted singular-value decomposition.</p>{downloads([('checks/component_correlations.csv','All component correlations'),('checks/audit_checks.csv','Independent verification checklist')])}</details></section>'''

load_rows = [[r['side'],esc(labels[r['variable']]),f'{r["loading"]:+.3f}'] for r in pc['loadings'] if r['PC']==1]
var_rows = [[r['side'],f'PC{r["PC"]}',pct(r['variance_pct']),pct(r['cumulative_pct'])] for r in pc['variance']]
pca = f'''<section id="pca"><span class="eyebrow">APPENDIX · PCA SENSITIVITY</span><h2>Alternative scores using the same individual and contextual inputs</h2><p class="intro">Separate weighted correlation PCAs summarize the same six personal demand indicators and six assigned supply measures. Both are restricted to original FPN2 = 1 women. Demand PCA uses {pc['audit']['demand_PCA_n']:,} complete women; supply PCA uses {pc['audit']['supply_PCA_women_n']:,} women across {pc['audit']['supply_profiles']} residence/fallback source profiles, weighted by their represented women’s summed WEIGHT. These are not facility weights.</p>{figure('pca_appendix/01_pca_loadings.svg','Signed PCA scoring coefficients for personal demand and assigned supply inputs.')}{note(f'<strong>What PCA captures.</strong> Demand PC1 retains {pc["audit"]["demand_PC1_variance"]:.1f}% of demand variation; supply PC1 retains {pc["audit"]["supply_PC1_variance"]:.1f}%. In this Senegal fit, all PC1 coefficients are positive after orientation. Demand PC1 emphasizes opposition, low confidence and stigma; supply PC1 emphasizes offering and LARC stock. The model negates favorable supply PC1 to obtain supply risk. These coefficients describe variation in inputs, not their effects on modern use.')}{figure('pca_appendix/02_pca_variance.svg','Variance retained by each principal component with exploratory permutation references.')}<details class="model-detail"><summary>PCA construction, coefficients and stability</summary>{table(['Side','Input','PC1 coefficient'],load_rows)}{table(['Side','Component','Variance retained','Cumulative'],var_rows)}<p>Use final-weighted input means/population SDs, eigendecompose the standardized correlation matrix, and project each woman’s own demand responses or assigned supply inputs onto PC1. PC1 is prespecified as a sensitivity; no outcome-informed coefficients or PCA geographic categories are constructed. Demand PC2 also retains variation, so PC1 is not a complete summary of every domain.</p><p>Exploratory 2,000-permutation and leave-one-department-out construction checks are available. Repeated fallback/Urban-equivalent profiles do not provide independent new areas; permutation references are construction diagnostics, not survey-design tests. Model intervals condition on the estimated scores. PCA units differ from average-z units, so odds-ratio magnitudes across methods cannot establish which fit is stronger.</p>{downloads([('pca_appendix/pca_loadings.csv','All PCA coefficients'),('pca_appendix/pca_centers_scales.csv','PCA centers and scales'),('pca_appendix/pca_variance.csv','Variance and permutation checks'),('pca_appendix/pca_leave_one_department_out.csv','PCA department-deletion checks')])}</details>{models(pm,'pca_appendix/models',pca=True)}<p class="footnote">Method references: <a href="https://knowledge-for-policy.ec.europa.eu/composite-indicators/toolkit_en/navigation-page/10-step-guide_en/step-5-normalisation_en">JRC normalization</a>; <a href="https://knowledge-for-policy.ec.europa.eu/composite-indicators/toolkit_en/navigation-page/10-step-guide_en/step-6-weighting_en">JRC weighting/PCA</a>; <a href="https://jepusto.github.io/clubSandwich/reference/vcovCR.glm.html">CR2 covariance for weighted GLMs</a>. All estimates shown are calculated from Senegal data. The department-only exposure comparison is a separate planned sensitivity, not a result claimed here.</p></section>'''

js = '''const DATA=__DATA__;const tip=document.querySelector('.tooltip');
const fmt=v=>v==null?'Not surveyed':v.toFixed(1)+'%';
function hide(){tip.classList.remove('show')}
function move(e){let x=e.clientX+17,y=e.clientY+17;if(x+tip.offsetWidth>innerWidth-10)x=e.clientX-tip.offsetWidth-17;if(y+tip.offsetHeight>innerHeight-10)y=e.clientY-tip.offsetHeight-17;tip.style.left=Math.max(10,x)+'px';tip.style.top=Math.max(10,y)+'px'}
function show(el,e){const d=DATA[el.dataset.key];if(!d)return;tip.innerHTML='<strong>'+d.name+'</strong><p>'+(el.dataset.mode==='coverage'?d.coverage_label:d.category)+'</p><div class="two"><div><b>'+fmt(d.cpr)+'</b><br><small>Any-method use</small></div><div><b>'+fmt(d.mcpr)+'</b><br><small>Modern use</small></div></div><p>'+d.n+' women with original FPN2 = 1.</p><p>'+d.source_rows+' source women · '+d.facilities+' facility records</p>'+(el.dataset.mode==='classification'&&d.demand!=null?'<p>Department demand score: '+d.demand.toFixed(3)+'<br>Department supply readiness: '+d.supply.toFixed(3)+'</p>':'')+(d.fallback_need?'<p>'+d.fallback_need+' women in need receive department-level fallbacks in the models.</p>':'')+(d.small_sample?'<p class="sample-alert">† Small need-group sample: n='+d.n+'. Interpret department estimates and categories cautiously.</p>':'')+'<small>Weighted use among women in need; department-wide supply for planning.</small>';tip.classList.add('show');move(e)}
document.querySelectorAll('[data-key][data-mode]').forEach(el=>{el.addEventListener('pointerenter',e=>show(el,e));el.addEventListener('pointermove',move);el.addEventListener('pointerleave',hide);el.addEventListener('focus',()=>{let r=el.getBoundingClientRect();show(el,{clientX:r.x+r.width/2,clientY:r.y+r.height/2})});el.addEventListener('blur',hide);el.addEventListener('click',e=>show(el,e))});document.addEventListener('keydown',e=>{if(e.key==='Escape')hide()});
'''.replace('__DATA__',json.dumps(DATA,separators=(',',':')))
page = '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Senegal_analysis | SURGO × Manitoba</title><style>'+css+'</style></head><body>'+header+'<main>'+coverage+use+joint+model+defs+pca+'</main><div class="tooltip" role="status" aria-live="polite"></div><footer style="max-width:1430px;margin:20px auto 35px;padding:0 30px;color:#6a8290;font-size:12px">Senegal · residence-specific supply with flagged department fallbacks · original SURGO FPN2 = 1 · initial cross-sectional evidence.</footer><script>'+js+'</script></body></html>'
output.parent.mkdir(parents=True,exist_ok=True)
output.write_text(page)
print('Rendered',output.name,'with 46 department boundaries, 33 planning profiles, original FPN2 need group and ten modern-use specifications.')
