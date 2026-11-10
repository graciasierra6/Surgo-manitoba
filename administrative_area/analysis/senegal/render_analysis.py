"""Render department-only Senegal evidence and appendix sensitivity checks."""
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
facility_coverage = {}

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
all_areas = sorted(g for g in cov if g != 'SENEGAL')
small = {g for g in areas if rates[g]['n'] < 50}
n, model_n = AUD['need_n'], MA['common_n']
assert n == 1905 and model_n == MA["common_n"] and AUD["eligible_mapped_facilities"] == 1568
assert len(areas) == 33 and len(layout) == len(all_areas) == 46
assert pm['audit']['common_n'] == model_n and verification['all_passed']
esc = lambda s: html.escape(str(s), quote=True)
name = lambda g: cov[g]['name'].title()
def french_terms(value):
    return re.sub(r'\bdepartments?\b', lambda m: ('Département' if m[0][0].isupper() else 'département') + ('s' if m[0].lower().endswith('s') else ''), value, flags=re.I)
FP_LINKED = AUD['eligible_mapped_facilities']
DAILY_VALID = AUD['daily_valid_linked']
DAILY_UNDEFINED = AUD['daily_undefined_linked']
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

def display_download_name(value):
    return str(value).replace("priorities", "components").replace("priority", "component")

def download(file, title):
    data = base64.b64encode((root / file).read_bytes()).decode()
    return f'<a class="download" download="senegal_{display_download_name(Path(file).name)}" href="data:text/csv;base64,{data}">{esc(title)} ↓</a>'

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
        mcpr=100*r['mcpr']['estimate'] if r else None, category=catnames[q['category']] if q else 'Facility data only',
        demand=q['demand_score'] if q else None, supply=q['supply_score'] if q else None,
        small_sample=g in small, coverage_group=group, coverage_label=coverage_labels[group],
        fallback_need=0, alternative_category=catnames[sens_scores[g]['category']] if g in sens_scores else 'Facility data only', alternative_supply=sens_scores[g]['supply_score'] if g in sens_scores else None, complete_facilities=int(facility_coverage[g]['complete_facilities']) if g in facility_coverage else None)
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
            category = (sens_scores[g]['category'] if mode == 'sensitivity' else scores['zscore', g]['category']) if g in rates else 'supply_only'
            fill = coverage_colors[d['coverage_group']] if mode == 'coverage' else colors[category]
            path = ' '.join('M'+' L'.join(f'{x:.2f},{y:.2f}' for x,y in map(xy,ring))+' Z' for ring in f['rings'])
            title = d['name'] + ': ' + (f'modern use {d["mcpr"]:.1f}%' if d['mcpr'] is not None else 'facility data only; no SURGO sample')
            out.append(f'<path class="district" data-key="{g}" data-mode="{mode}" d="{path}" fill="{fill}" fill-rule="evenodd" stroke="white" stroke-width="1.1" tabindex="0"><title>{esc(title)}</title></path>')
            cx, cy = xy(f['center'])
            if mode == 'coverage':
                radius = 3 + math.sqrt(d['facilities']) * .55
                out.append(f'<circle class="facility-dot" data-key="{g}" data-mode="{mode}" cx="{cx:.2f}" cy="{cy:.2f}" r="{radius:.2f}" fill="#123d50" fill-opacity=".62" stroke="white" stroke-width="1"><title>{d["facilities"]} facility records</title></circle>')
            if not f['inset'] or inset:
                label = d['name'] + (' †' if mode in ('classification','sensitivity') and d['small_sample'] else '')
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
            ('<text x="953" y="482" class="map-caption">† Fewer than 50 women in need</text>' if mode in ('classification','sensitivity') else ''),
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
    out += [f'<th>{esc(labels[k])}'+('<br><small>↓ better</small>' if k=='stockout_burden' else '<br><small>↑ better</small>' if k in S else '')+'</th>' for k in D+S]
    out += ['</tr></thead><tbody>']
    for g in ['SENEGAL']+areas:
        c = cov[g]
        title = 'Senegal · linked departments' if g == 'SENEGAL' else name(g)
        mark = ' †' if g in small else ''
        out.append(f'<tr class="{"overall" if g == "SENEGAL" else ""}"><th>{esc(title)}{mark}<small>n={c["n_need"]:,} women in need · {c["fp_facilities"]:,} FP facilities</small></th>')
        for side, ks in [('Demand',D),('Supply',S)]:
            for k in ks:
                r = hm[g,side,k]; v = r['value']
                den = f'Valid responses {r["n_valid"]:,} / {r["n_total"]:,}'
                if v is None:
                    out.append(f'<td class="unknown-cell" title="{esc(den)}; unknown, not zero">—</td>'); continue
                t = v/100 if side == 'Demand' or k == 'stockout_burden' else 1-v/100
                bg = mix('#eef8f8','#047888',t) if side == 'Demand' else mix('#fff8ed','#bc570f',t)
                fg = 'white' if t > .62 else '#233d4b'
                out.append(f'<td style="background:{bg};color:{fg}" title="{esc(title+": "+labels[k]+" · "+den)}">{v:.0f}%</td>')
        out.append('</tr>')
    return ''.join(out)+'</tbody></table></div>'

def models(data, folder, pca=False):
    au = data['audit']
    component_count = au['supply_components']
    mixed = pca and bool(pc['audit'].get('negative_supply_PC1_loadings'))
    co = {(r['model'],r['term']):r for r in data['coefficients']}
    tests = {(r['model'],r['block']):r for r in data['tests']}
    gaps = {r['term']:r for r in data['gaps']}
    units = 'one PC1 pattern unit' if pca else 'one original average-z index point'
    s = '<h2>Modern use in relation to personal concerns and local services</h2>' if not pca else '<h3>The same five modern-use models using PCA scores</h3>'
    s += f'<p class="model-meta">All models: <b>{au["common_n"]:,} women with original SURGO FPN2 = 1</b>, including {au["common_events"]:,} modern users across {au["districts"]} departments. Outcome: current modern use = 1; confirmed traditional-only use or nonuse = 0. Adjust for age + age², Senegal-specific education, partnership, fertility goal and Rural/Urban residence. Use WEIGHT, department-clustered CR2/Satterthwaite intervals, and no random effects or interactions.</p>'
    if not pca:
        s += table(['Score assigned to each woman','Construction','Observed model variation'],[
            ['Demand risk score','Standardize her own six demand responses using weighted item means and population SDs among valid FPN2 = 1 women; average all six. Her department or residence does not determine this score.',f'{au["demand_distinct_values"]} distinct values across {au["common_n"]:,} women.'],
            ['Supply risk score',f'Standardize her assigned {component_count} department-wide measures against the fixed 33-department planning means/SDs. Average and reverse the sign: higher = weaker provision.',f'{au["supply_distinct_values"]} distinct numeric scores across {au["districts"]} departments. Rural and Urban women in one department receive the same score.']])
    if mixed:
        s += '<div class="report-warning"><strong>Supply PCA is a pattern score.</strong> One or more coefficients have the opposite sign from the favorable orientation of the inputs. A higher negative PC1 is not uniformly weaker supply; interpret its signed coefficients. The equal-weight z-score remains primary.</div>'
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
    if mixed:
        s = s.replace('Supply risk score', 'Supply pattern score (PCA)').replace('supply risk odds ratio', 'supply-pattern odds ratio').replace('local supply risk','local supply pattern')
    return s

css = (Path(__file__).resolve().parents[3]/'analysis/common/report.css').read_text() + '''
body{margin:0;background:#f5f8fa;color:#233e4e;font-family:Arial,Helvetica,sans-serif}main{max-width:1480px;margin:auto;padding:0 30px}.report-header{max-width:1420px;margin:32px auto 24px;padding:34px 36px;background:#143e51;color:white;border-radius:14px}.report-header h1{font-size:38px;line-height:1.12;margin:8px 0 16px;color:white}.report-header p{max-width:1110px;color:#e0edf2;line-height:1.65}.report-header .eyebrow{color:#b3dcdd}.report-header nav{display:flex;flex-wrap:wrap;gap:9px;margin-top:22px}.report-header nav a{color:white;border:1px solid #71909e;border-radius:20px;padding:7px 13px;text-decoration:none;font-size:13px}section{margin:26px 0;padding:30px;background:white;border-radius:12px;border:1px solid #dce6ea}h2{font-size:28px;line-height:1.25}h3{color:#164b5b;margin:28px 0 13px}.eyebrow{letter-spacing:.1em;font-size:12px;font-weight:700;color:#24717c}.intro,.model-note{line-height:1.65}.metrics{display:grid;grid-template-columns:repeat(4,1fr);gap:16px;margin:22px 0}.metric{padding:18px;border:1px solid #dce6ea;border-radius:9px}.metric b{display:block;font-size:29px;color:#125967}.metric span{font-size:13px;color:#677f8c}.report-warning{background:#fff8ed;border-left:4px solid #c48035;padding:16px 18px;line-height:1.65;border-radius:3px;font-size:14px}.mapbox{background:#f8fbfc;border:1px solid #e0e9ec;border-radius:9px;overflow:hidden}.senegal-map{display:block;width:100%;height:auto}.district{cursor:pointer}.district:hover,.district:focus{filter:brightness(.85);stroke:#173b50;stroke-width:2;outline:none}.facility-dot{cursor:pointer}.map-label{font-size:11.5px;fill:#203d4d;font-weight:600;paint-order:stroke;stroke:white;stroke-width:3;stroke-linejoin:round}.inset-heading{font-size:16px;fill:#174758;font-weight:700}.map-caption{font-size:12px;fill:#64808b}.legend{display:flex;flex-wrap:wrap;gap:17px;margin:15px 0;font-size:13px}.legend i{display:inline-block;width:14px;height:14px;border-radius:3px;margin-right:6px;vertical-align:middle}.bar-chart{display:block;width:100%}.bar-label{font-size:15px;fill:#244253;font-weight:600}.bar-label.sub{font-size:13px;font-weight:400}.bar-value{font-size:14px;font-weight:600;fill:#203d4c}.axis-label{font-size:13px;fill:#647d8a}.paired-heatmap{width:100%;min-width:1200px;border-collapse:separate;border-spacing:4px;font-size:13px}.paired-heatmap th{padding:10px 4px;text-align:center;vertical-align:bottom;font-size:12px}.paired-heatmap tbody th{text-align:left;min-width:205px;font-size:13px}.paired-heatmap small{display:block;font-size:10px;font-weight:400;color:#6e8793;margin-top:4px}.paired-heatmap td{text-align:center;font-size:14px;font-weight:700;padding:14px 5px;border-radius:2px}.demand-group{color:#087c88;background:#eef8f8}.supply-group{color:#b66b20;background:#fff6e8}.overall th{color:#086d78}.overall td{outline:1px solid #234a5c}.unknown-cell{background:#edf0f3;color:#7d8d96}.table-wrap{overflow:auto}.planning{width:100%;border-collapse:collapse}.planning th,.planning td{padding:12px 11px;vertical-align:top;text-align:left;line-height:1.5;border-bottom:1px solid #e4ecef}.planning thead{background:#f0f6f8}.planning th,.planning td{font-size:13px}.planning td ul{margin:0;padding-left:18px}.planning td li{margin:0 0 5px}.sample-badge{display:block;margin-top:5px;font-size:10px;font-weight:600;color:#98581a}.category-tag{display:inline-block;border-radius:4px;padding:5px 8px;white-space:nowrap;font-size:12px;font-weight:700}.model-figure{margin:23px 0 8px}.model-figure img{display:block;width:100%;height:auto}.model-meta{font-size:14px;line-height:1.7}.model-key{margin:10px 0 24px;background:#edf7f7;padding:15px 18px;border-left:4px solid #148591;font-size:14px;line-height:1.7}.model-detail{margin:18px 0;border:1px solid #dce7eb;border-radius:7px;padding:14px 17px}.model-detail summary{font-weight:700;color:#164b5b;cursor:pointer}.model-detail p{font-size:13px;line-height:1.7}.table-heading{display:flex;flex-wrap:wrap;gap:13px;margin:15px 0}.download{font-size:12px}.footnote{font-size:12px;color:#637d8b;line-height:1.7}.score-steps{font-size:14px;line-height:1.7;padding-left:22px}.score-steps li{margin:10px 0}.wide-break{height:1px;background:#dce7eb;margin:30px 0}.tooltip{display:none;position:fixed;background:#173d50;color:white;border-radius:9px;padding:16px;width:300px;z-index:1000;box-shadow:0 9px 35px #173d5040;font-size:13px;pointer-events:none}.tooltip.show{display:block}.tooltip strong{display:block;font-size:17px;color:white;margin-bottom:7px}.tooltip p{margin:7px 0;color:#e0edf1}.tooltip .two{display:flex;justify-content:space-between;margin:12px 0}.tooltip b{font-size:20px}.tooltip small{color:#c8dbe3}.tooltip .sample-alert{background:#fff0cb;color:#5c421b;padding:8px;border-radius:4px}@media(max-width:850px){main{padding:0 10px}section{padding:18px}.report-header{margin:12px;padding:24px}.report-header h1{font-size:28px}.metrics{grid-template-columns:repeat(2,1fr)}}@media print{body{background:white}.report-header,section{border-radius:0}details{display:block}nav,.download{display:none}.mapbox,.model-figure{break-inside:avoid}}
'''
co = {(r['model'],r['term']):r for r in m['coefficients']}
header = '<header class="report-header"><span class="eyebrow">SURGO × UNIVERSITY OF MANITOBA · SENEGAL</span><h1>Demand, services and modern contraceptive use</h1><p><strong>Updated department-only supply assignment.</strong> Supply summaries use confirmed FP providers (s5_6 = 1), valid values by measure, and no facility weights. Daily provision replaces counseling; cleaned stockout burden replaces popular-product stock. Stockout burden is reversed to continuity before scoring. Health beliefs use PB2A alone. Main geographic classifications compare final average-z scores with zero.  Every woman receives the average recorded facility characteristics of her own department. Her personal demand responses remain individual; Rural/Urban residence is retained as an adjustment variable.</p><p>Joint evidence helps planners see where reported concerns accompany service weaknesses. The models estimate adjusted associations, while the map identifies relative patterns of service weaknesses and reported concerns. They do not establish causal binding constraints or the budget share to allocate to either side.</p><nav><a href="#coverage">1 · Coverage and linkage</a><a href="#use">2 · Use and intention</a><a href="#joint">3 · Joint conditions and concerns</a><a href="#models">4 · Adjusted models</a><a href="#definitions">Definitions and checks</a><a href="#appendix-scoring">Appendix · Scoring comparisons</a><a href="#pca">Appendix · PCA</a></nav></header>'
coverage_legend = '<div class="legend">' + ''.join(f'<span><i style="background:{coverage_colors[k]}"></i>{coverage_labels[k]}: <b>{sum(v["coverage_group"] == k for v in DATA.values())}</b></span>' for k in ['both','supply_only','demand_only']) + '<span><i style="background:#123d50;border-radius:50%"></i>Department facility-count dots</span></div>'
coverage = f'''<section id="coverage"><span class="eyebrow">01 · COVERAGE AND LINKAGE</span><h2>Women and facilities overlap in 33 departments</h2><p class="intro">The join retains all <b>7,169 original women</b>. Manitoba contains 2,043 facility records across 46 departments. Of these, <b>1,648 facilities in 33 departments</b> overlap with SURGO; 395 facilities are in 13 departments without surveyed women. This map shows all source facility records before FP-provider restriction and all source women before selecting the need group. The updated supply analyses below use <b>1,568 confirmed FP providers</b> in the 33 linked departments.</p><div class="metrics"><div class="metric"><b>7,169</b><span>Original SURGO women retained</span></div><div class="metric"><b>33</b><span>Departments with both sources</span></div><div class="metric"><b>1,648</b><span>Facilities in the linked departments</span></div><div class="metric"><b>1,905</b><span>Women with original FPN2 = 1</span></div></div>{coverage_legend}{map_svg('coverage')}<p class="footnote">Dots show department facility totals, not precise facility locations. Boundaries: <a href="https://data.humdata.org/dataset/cod-ab-sen">Government of Senegal / OCHA</a>. The checked health-district-to-department crosswalk is retained, including the documented qualifications for Diakhao, Sangalkam and Yeumbeul.</p><h3>How supply is assigned to each woman</h3>{table(['Woman’s residence','Supply profile assigned'],[['Department A · Rural','Confirmed FP providers in Department A, using the valid denominator for each measure.'],['Department A · Urban','The same Department A profile.'],['Department B · Rural or Urban','The Department B profile.']])}<p>Each valid facility contributes equally to its department measure; no facility weights are used. All 7,169 women have a department profile. There is no Rural/Urban supply split or geographic fallback in this analysis. A department profile describes the recorded service environment, not the facility a woman actually attended.</p>{downloads([('department_data_coverage.csv','Coverage counts'),('assigned_supply_profiles.csv','Supply assignments'),('map_name_crosswalk.csv','Boundary crosswalk')])}</section>'''

unknown_method = bars['user']['n'] - bars['modern']['n'] - bars['traditional']['n']
unknown_preference = bars['user']['n'] - bars['wanted_start']['n'] - bars['other_start']['n']
use = f'''<section id="use"><span class="eyebrow">02 · USE, INTENTION AND METHOD CHOICE</span><h2>Among women SURGO classifies as in need of family planning</h2><div class="report-warning"><strong>Population used from this section onward:</strong> original SURGO <b>FPN2 = 1</b>, exactly as recorded in the source survey. This gives <b>1,905 women</b>. No older script’s need_n recoding or sterilization/hysterectomy additions are applied. Eleven nonconsenting records are outside estimation and have FPN2 = 95.</div>{bar_chart()}<p class="footnote">Every bar and sub-bar has the same denominator: all 1,905 women with FPN2 = 1. Percentages use <b>WEIGHT</b>; n is an unweighted respondent count. Capped whiskers are 95% logit intervals using the available RB0H census-district design, constructed before domain selection. Original sampling strata and finite-population corrections are unavailable. Subsets overlap: modern users are included in current users, so these bars do not sum to 100%. {unknown_method} users have unresolved modern/traditional classification; {unknown_preference} have another or unresolved initiation-preference response. *Preference concerns the method wanted at initiation; the type of alternative wanted is not established. Future intention concerns any method, without a specified 12-month initiation window.</p>{note(f'<strong>Initial pattern.</strong> Weighted current use is <b>{100*bars["user"]["estimate"]:.1f}%</b>, including <b>{100*bars["modern"]["estimate"]:.1f}%</b> modern use. Another <b>{100*bars["intender"]["estimate"]:.1f}%</b> are nonusers with intention and <b>{100*bars["no_intent"]["estimate"]:.1f}%</b> nonusers without stated intention. These groups help distinguish existing use from stated future interest without treating this cross-sectional survey as a transition study.')}{downloads([('use_distribution.csv','Use and intention estimates'),('department_use_rates.csv','Department use estimates')])}</section>'''

comparison_rows = [[area_label(g)]+[tag(scores[k,g]['category']) for k in methods] for g in areas]
method_rows = [
 ['Rank average','Rank departments separately for each component, using average ranks for ties; average six ranks separately on each side.','Demand above its final-score median; favorable supply below its median.'],
 ['Mean percentages','Average six component percentages separately on each side.','Same strict final-score median rule.'],
 ['Median/IQR','For each component, subtract its department median and divide by its IQR; average the transformed values separately by side.','Same strict final-score median rule.'],
 ['Median count','Count components above the demand component medians, or below the favorable supply component medians.','Flag at least four of six unfavorable components on either side.'],
 ['Z-score average · primary','For each component, subtract its department mean and divide by its sample SD; average six component z-scores on each side.','Demand > 0; favorable supply readiness < 0. Zero is the mean of the final scores.']]
threshold = cuts['zscore']
count_category = {k:sum(scores['zscore',g]['category']==k for g in areas) for k in ['both','demand','supply','neither']}
legend = '<div class="legend">'+''.join(f'<span><i style="background:{colors[k]}"></i>{catnames[k]}: <b>{count_category.get(k,13)}</b></span>' for k in ['both','demand','supply','neither','supply_only'])+'</div>'
def priority_frequency(category, keys):
    group = [r for r in planning.values() if r[category + '_flag']]
    frequency = Counter(k for r in group for k in r['top_' + category])
    ordered = sorted(keys, key=lambda k: (-frequency[k], keys.index(k)))
    rows = [[esc(planning_labels[k]), f'{frequency[k]} of {len(group)}',
             pct(100 * frequency[k] / len(group)) if group else '—'] for k in ordered]
    with (root / f'{category}_priority_frequency.csv').open('w',newline='',encoding='utf-8') as stream:
        writer=csv.writer(stream); writer.writerow(['component','label','flagged_departements','in_top_three','percentage_of_flagged_departements'])
        for k in ordered: writer.writerow([k,planning_labels[k],len(group),frequency[k],100*frequency[k]/len(group) if group else ''])
    return len(group), table(['Component', 'Departments with this among their top three',
                             'Share of this group'], rows)

supply_group_n, supply_frequency_table = priority_frequency('supply', S)
demand_group_n, demand_frequency_table = priority_frequency('demand', D)
priority_summaries = f'''<div id="component-summaries"><h3>Which service weaknesses and reported concerns recur across departments?</h3><p>Count how often each component appears among a department’s three selected components: the lowest standardized provision components for supply, and the highest standardized concern components for demand. These are counts of departments, not percentages of women or estimated causes of nonuse.</p><div class="component-summary-grid"><article id="supply-component-summary"><h4>Service weaknesses · {supply_group_n} supply-flagged departments</h4>{supply_frequency_table}</article><article id="demand-component-summary"><h4>Reported concerns · {demand_group_n} demand-flagged departments</h4>{demand_frequency_table}</article></div><p class="footnote">Both-flagged departments are included on each flagged side. The supply and demand groups therefore overlap. A department contributes to three components, so percentages do not sum to 100%. These are relative component patterns, not service-standard failures. Each supply measure uses its own valid-response denominator. Daily provision is defined for {DAILY_VALID:,} of {FP_LINKED:,} FP facilities; {DAILY_UNDEFINED} have no recorded offered category and remain undefined. Stockout burden is higher = worse, whereas the other five raw supply percentages are higher = better.</p><p class="footnote">Short- and long-acting options together means at least one long-acting and three short-acting categories at the same facility. Free provision and training average within-facility proportions; they are not shares of facilities offering any free product or having any trained provider. The confidence indicator is unchanged: it measures confidence choosing a method or managing side effects, not objective knowledge.</p></div>'''

def department_priority_table(category):
    side = category.title()
    rows = []
    for g in areas:
        q = planning[g]
        if not q[category + '_flag']:
            continue
        cells = []
        for k in q['top_' + category]:
            r = hm[g, side, k]
            coverage = f'Valid responses: {r["n_valid"]:,} / {r["n_total"]:,}'
            sparse = (f'<br><small>{r["n_valid"]} of {r["n_total"]} FP facilities answered</small>'
                      if category == 'supply' and r['n_valid'] < r['n_total'] else '')
            cells.append(f'<span title="{esc(coverage)}">{esc(planning_labels[k])}'
                         f'<br><b>{pct(r["value"])}</b>{sparse}</span>')
        rows.append([area_label(g)] + cells)
    headers = ['Weakest service component', 'Second-weakest component', 'Third-weakest component'] if category == 'supply' else ['Most elevated concern', 'Second-most elevated concern', 'Third-most elevated concern']
    return table(['Department'] + headers, rows)

department_priorities = f'''<div id="supply-department-components"><h3>The {supply_group_n} supply-flagged departments · three weakest service components</h3><p>The supply columns run from the lowest to the third-lowest favorable-supply component z-score. Displayed percentages are original measures. Higher stockout burden indicates weaker provision; lower values indicate weaker provision for the other five measures. Ordering is based on standardized scores, not the raw percentages.</p>{department_priority_table('supply')}<p class="footnote">Facilities contribute equally; each measure uses its own valid-response denominator. Free provision and training are averages of within-facility proportions. Hover a value for response coverage. Stockout burden retains the cleaned UoM definition; it is not the percentage of facilities with any stockout. Daily provision conditions on each facility’s recorded offered modern methods and does not by itself measure method breadth or prove inadequate access.</p></div><div id="demand-department-components"><h3>The {demand_group_n} demand-flagged departments · three most elevated reported concerns</h3><p>The concern columns run from the highest to the third-highest demand component z-score. Displayed percentages are weighted shares of women with original SURGO FPN2 = 1 who report each concern, among valid answers. Ordering is based on standardized scores, not the raw percentages.</p>{department_priority_table('demand')}<p class="footnote">Each concern uses its own valid-response denominator; unanswered or unresolved items are not coded as no concern. Hover a value for response coverage. † Fewer than 50 women in need in the department; subgroup percentages may be imprecise. Both-flagged departments appear in both lists and in the complete planning table below.</p></div>'''
css += '''.component-summary-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:24px}.component-summary-grid article{min-width:0;border:1px solid #dce6ea;border-radius:8px;overflow:hidden}.component-summary-grid h4{margin:0;padding:16px 13px;line-height:1.5;font-size:15px}#supply-component-summary h4{color:#a75a18;background:#fff6e8}#demand-component-summary h4{color:#087c88;background:#eef8f8}.component-summary-grid .planning th,.component-summary-grid .planning td{font-size:12px}@media(max-width:1000px){.component-summary-grid{grid-template-columns:1fr}}'''
css += '''#supply-department-components .planning thead{background:#fff6e8}#demand-department-components .planning thead{background:#eef8f8}#supply-department-components h3{color:#a75a18}#demand-department-components h3{color:#087c88}'''
priority_rows = []
for g in areas:
    q = planning[g]
    def tops(side, fields):
        return '<ul>'+''.join(f'<li>{esc(planning_labels[k])} <b>{pct(hm[g,side,k]["value"])}</b></li>' for k in fields)+'</ul>'
    priority_rows.append([area_label(g),'<b style="color:#087c88">'+('Yes' if q['demand_flag'] else 'No')+'</b>',
        tops('Demand',q['top_demand']) if q['demand_flag'] else '—',
        '<b style="color:#b66b20">'+('Yes' if q['supply_flag'] else 'No')+'</b>',
        tops('Supply',q['top_supply']) if q['supply_flag'] else '—'])
joint = f'''<section id="joint"><span class="eyebrow">03 · JOINT CONDITIONS, SERVICE WEAKNESSES AND REPORTED CONCERNS</span><h2>Reported concerns alongside recorded services</h2><p class="intro">Read across each department: six reported demand concerns on the left and six recorded supply measures, including daily provision and stockout burden, on the right. Demand percentages use women with <b>FPN2 = 1</b>, WEIGHT and valid domain responses. Supply uses valid responses only among facilities with raw s5_6 = 1: confirmed on-site FP providers in the department.</p>{heatmap()}<p class="footnote">Darker teal means more concern; darker orange means weaker provision. The five provision percentages run higher = better; <b>stockout burden runs higher = worse</b>. Colors account for this direction difference. Daily provision uses {DAILY_VALID:,}/{FP_LINKED:,} valid facility ratios; stockout burden has {AUD['stockout_valid_linked']:,} cleaned values. Hover cells for coverage. The existing at-least-one-LARC stock indicator is retained because three-LARC stock is constant in Senegal. † Fewer than 50 women in need.</p>{downloads([('heatmap_values_and_denominators.csv','Heatmap values and denominators'),('demand_missingness_by_department.csv','Demand response coverage')])}<h3>Department planning profiles · z-score average</h3><details open class="model-detail"><summary>How the scores and thresholds work</summary><ol class="score-steps"><li><b>Summarize departments.</b> Calculate six weighted concern percentages among women in need and six unweighted supply summaries using valid facility responses.</li><li><b>Align direction before calculation.</b> Retain the five higher-is-better supply measures. Convert stockout burden to <b>stock continuity = 1 − burden</b>. For example, 40% burden becomes 60% continuity. The heatmap retains the original burden percentage. All six scoring inputs now have higher = stronger provision.</li><li><b>Standardize each component across the 33 departments.</b> Use (department value − component mean) ÷ component sample SD. Each department contributes equally to these reference values.</li><li><b>Average within each side.</b> Average six demand z-scores and six favorable-supply z-scores. Higher demand means more concern; higher favorable supply means stronger recorded provision. Supply risk is the negative of readiness: higher burden lowers readiness and raises risk. This equals first reversing the other five measures to risks, retaining burden as a risk, and averaging their standardized scores. Each component has mean zero; their average also has mean zero, but its SD is not necessarily one. A score of +0.2 is an average of standardized component values, not 20 percentage points.</li><li><b>Compare the final scores with their mean: zero.</b> Flag demand above <b>0</b> and favorable supply readiness below <b>0</b>. Scores equal to zero within numerical tolerance remain unflagged. Every component is centered across the same 33 departments, so the mean of each final average-z score is zero.</li></ol><p><strong>Why this construction?</strong> Standardization gives components comparable dispersion and equal averaging makes the chosen weights explicit. The <a href="https://www.oecd.org/en/publications/handbook-on-constructing-composite-indicators-methodology-and-user-guide_9789264043466-en.html">OECD/JRC handbook</a> describes these composite-indicator methods; it does not endorse our particular variables or zero threshold. Correlated components, extreme values and sparse responses can affect the result. Each department has equal weight in the reference, so zero is the surveyed-department average, not a national population estimate. Averaging six z-scores preserves the ranking of their sum, while keeping the scale interpretable as an average standardized component.</p><p>These are relative planning categories. “Neither flagged” does not establish adequate services or absence of barriers. The map’s contextual demand score is separate from the personal demand score used in models. Alternative scoring methods are shown in the appendix.</p></details>{legend}{map_svg('classification')}{note(f'<strong>Planning signal.</strong> Of 33 departments, <b>{count_category["both"]}</b> have both sides flagged, <b>{count_category["demand"]}</b> demand only, <b>{count_category["supply"]}</b> supply only, and <b>{count_category["neither"]}</b> neither. Use the component comparisons to identify what warrants investigation; the category is not a causal diagnosis.')}<p class="footnote">† Fewer than 50 unweighted women in need; {sum(rates[g]['n'] < 25 for g in areas)} departments have fewer than 25. Small samples and incomplete facility coverage can make rankings unstable. Hover over the map for weighted CPR and mCPR.</p>{priority_summaries}{department_priorities}<h3>Department service weaknesses and reported concerns</h3>{table(['Department','Demand challenges','Top three reported concerns','Supply challenges','Top three service weaknesses'],priority_rows)}<p class="footnote">Top demand concerns have the highest component z-scores; service weaknesses have the lowest favorable component z-scores. These are relative component comparisons, not estimated effects on use. Lists appear for flagged sides only.</p>{downloads([('planning_priorities.csv','Service weaknesses and reported concerns'),('component_score_trace.csv','Score calculations'),('department_sample_flags.csv','Sample-size flags')])}</section>'''
model = '<section id="models"><span class="eyebrow">04 · ADJUSTED ASSOCIATIONS</span>'+models(m,'models_zscore')+'</section>'

demand_defs = [
 ['Healthcare permission barriers','HS7C: 1 = permission to seek a doctor is a challenge; 2 = no. General healthcare permission, not FP-specific partner approval.'],
 ['Anticipated social stigma','PB7B/C: 1/2 on either item, concerning being seen by someone known or what people would say. Zero requires both valid negatives (3/4); unresolved responses remain missing unless the other item establishes concern.'],
 ['Health / side-effect beliefs','PB2A only: 1/2 = health problems are very/somewhat likely; 3/4 = unlikely. Other answers remain unresolved. PB2B/C are excluded. This measures perceived health harm, not experienced side effects.'],
 ['Future-fertility concerns','Any MP4_a–f selected for later childbearing concerns about reversible methods. Require coherent MP4_a–i 0/1 responses, at least one selection or explicit none, no none-plus-method contradiction, and MP4_99 = 0. Sterilization items MP4_g/h do not create a fertility concern; unknown responses are not zero.'],
 ['Low method knowledge · retained label','MC5A/B: 3/4 on either item, measuring low confidence choosing a suitable method or handling side effects/switching. This is self-efficacy/confidence, not an objective knowledge test. Zero requires both valid 1/2 answers.'],
 ['Opposes family planning','FP1: 3/4 = against versus 1/2 = in favor. Unfamiliarity, no opinion and other special codes remain unresolved.']]
supply_defs = [
 ['Short + long-acting offering','Average one_larc_3_sa_offer, joined as share_offer_larc_and_3_sa. Share meeting the original ≥1 long-acting plus ≥3 short-acting product/category rule. Higher = stronger recorded provision.'],
 ['Free-product provision','Average valid facility p_free_prod proportions, joined as mean_p_free_prod. A value of 60% means the average reporting facility provides 60% of its recorded products free; it is not the percentage of facilities offering any free method.'],
 ['Trained-provider share','Average valid facility p_train proportions, joined as mean_p_train. Describes the average reporting facility’s trained-personnel proportion under the original UoM eligibility; not the share of facilities with at least one trained provider.'],
 ['Daily provision of offered modern methods','From raw s5_7a–g: pills, injectables, male/female condoms, emergency contraception, IUD and implants. For each FP facility, count Daily (code 1) divided by categories provided Daily/Weekly/Biweekly/Monthly (codes 1–4). Code 5 Not at all is excluded from the offered denominator; unknown responses enter neither numerator nor denominator. A zero offered denominator is undefined. Average valid facility ratios as mean_daily_modern_provision. A department value of 75% means the average reporting facility provides 75% of its recorded offered modern categories daily; it is not the percentage of facilities providing all methods daily. Partial method responses remain conditional on recorded information. Higher = more frequent reported provision, not proof of adequate access.'],
 ['At least one LARC in stock','Average one_larc_stockin, joined as share_stocked_1plus_larc. Share meeting the original at-least-one-long-acting-product stock rule. The three-LARC measure is constant FALSE and cannot be standardized.'],
 ['Stockout burden','Average the original cleaned p_any_stockout values among confirmed FP facilities, joined as mean_facility_any_stockout. A value of 40% means the average facility has 40% of assessed product groups classified as having current or recent stockouts; it is not 40% of facilities. Senegal history items s6_17 refer to the last three months. Product groups follow the UoM code and are not explicitly restricted to normally offered methods. Higher raw burden = worse. Preserve these values as supplied; use 1 − burden as stock continuity before scoring and PCA. Numeric completeness does not establish complete raw responses or independently validate the upstream unknown-response rules.']]
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
den_rows = [[area_label(g),str(cov[g]['fp_facilities'])]+[str(hm[g,'Supply',k]['n_valid']) for k in S] for g in areas]
correlations = list(csv.DictReader((root/'checks/component_correlations.csv').open()))
cor_rows = []
for side in ['Demand','Supply']:
    pairs = sorted([r for r in correlations if r['side']==side and r['item1']<r['item2']],key=lambda r:abs(float(r['correlation'])),reverse=True)[:3]
    cor_rows += [[side,esc(labels[r['item1']]),esc(labels[r['item2']]),f'{float(r["correlation"]):+.3f}',r['n_pair']] for r in pairs]
defs = f'''<section id="definitions"><span class="eyebrow">DEFINITIONS AND VERIFICATION</span><h2>Measures, denominators and interpretation</h2><details class="model-detail"><summary>Six demand domains: questions and missing-response rules</summary>{table(['Indicator','Source and interpretation'],demand_defs)}<p>These items are not restricted to married women or current nonusers. A known positive can establish an any-concern flag. Zero requires valid negative responses on every included item; other combinations remain unresolved.</p>{table(['Item','Question · shortened wording','Blank responses','Unclassifiable responses, n','Weighted percentage'],question_rows)}{table(['Demand indicator','Unresolved women, n','Weighted unresolved percentage'],unknown_rows)}<p>Uncertainty responses are not automatically data errors or absence of concern. Future-fertility concerns and PB2A health beliefs account for most unresolved demand values. Department heatmaps use valid-domain responses; individual models require complete predictor values.</p></details><details open class="model-detail"><summary>Six supply measures and response coverage</summary>{table(['Indicator','Calculation and interpretation'],supply_defs)}<p>Every facility contributes equally when it has a valid value for the relevant indicator. Recorded zeros and FALSE flags are included. Unknown or undefined values are excluded from that indicator’s denominator. Cleaned flags can be complete even when underlying raw items are not; preserving the upstream definition does not validate every raw missing-data decision.</p>{table(['Department','Confirmed FP facilities']+[labels[k]+': valid' for k in S],den_rows)}<p>All main scores, classifications, component tables, models and the PCA appendix use the updated six components. Main facility summaries use only confirmed FP providers: {FP_LINKED:,} facilities across 33 departments. Daily provision has {DAILY_VALID:,} defined ratios and {DAILY_UNDEFINED} undefined ratios; stockout burden has {AUD['stockout_valid_linked']:,} numeric cleaned values. No missing frequency response is coded as nonprovision. FP counseling and popular-product stock are not index components.</p>{downloads([('service_regularity_coverage.csv','Daily-provision response coverage')])}{downloads([('supply_input_reconciliation.csv','Facility-profile reconciliation'),('department_supply_profiles.csv','Department-wide profiles')])}</details><details class="model-detail"><summary>Geographic assignment qualifications</summary><p>The reviewed health-district crosswalk is retained without new reassignment. Existing review flags remain for Diakhao → Fatick (16 facility records), Yeumbeul → Keur Massar (9), and Sangalkam → Rufisque (19). This eligibility update does not resolve those mapping qualifications or verify facility GPS locations.</p></details><details class="model-detail"><summary>Population, weighting, covariates and model sample</summary><p>Section 1 shows all source women. From Section 2 onward, the population is original SURGO <b>FPN2 = 1</b>: 1,905 women. The flag is unchanged; no historical need_n recoding or permanent-protection additions are applied. Original <b>WEIGHT</b> is retained.</p><p>The available sampling design uses recorded <b>RB0H census districts</b> ({AUD['survey_psus']} among consenting positive-weight respondents), constructed before selecting the need subgroup. Original strata and FPC are unavailable. Models use department-clustered CR2 covariance and Satterthwaite intervals to reflect shared supply exposure; sampling-cluster inference is an appendix sensitivity. No random effects or interactions are fitted.</p><p>All models adjust for <b>age, age², education, partnership, fertility goal and Rural/Urban residence</b>. Both scores enter continuously in their original units. All five primary specifications share <b>{MA['common_n']:,} women</b>, including {MA['common_events']:,} modern users; {MA['excluded_need']} women in need are excluded from these common models because outcome, demand or covariate information is unresolved. They remain in the overview and original joined data.</p>{table(['Model','Women','Modern users','Parameters','Maximum VIF'],sample_rows)}{table(['Sample-selection step','Excluded at this step','Women remaining'],flow_rows)}{table(['Group','Women','Share of need-group weight','Modern use among known outcomes','Urban share'],sample_compare_rows)}{table(['Variable','Missing/unresolved among 1,905 women'],missrows)}<p>There are <b>{MA['supply_distinct_values']} supply score values across 33 departments</b>. All women in one department share a supply score; their personal demand scores vary with their own answers. Supply precision depends on these geographic contexts, not solely the woman count. Intervals condition on estimated scores and do not propagate construction uncertainty. The checks are not a formal power analysis.</p>{downloads([('models_zscore/model_samples.csv','Sample and convergence'),('models_zscore/model_vifs.csv','Weighted VIF'),('models_zscore/model_missingness.csv','Model missingness'),('models_zscore/individual_demand_score_reference.csv','Personal demand-score references'),('models_zscore/supply_score_reference.csv','Supply-score references')])}</details><details class="model-detail"><summary>Correlation and independent verification</summary>{table(['Side','Component 1','Component 2','Correlation','Paired records'],cor_rows)}<p>Demand correlations use personal responses and WEIGHT. Supply correlations use one unweighted department profile per department. A low model VIF does not establish that index components measure separate constructs.</p><p>Independent verification passed <b>{verification['check_count']}</b> checks, including preservation of all {verification['source_raw_columns_verified']} raw survey columns, direct facility-to-department calculations, assignment of each woman’s six supply measures, modern-use coefficients and CR2 covariance, and weighted-SVD reproduction of PCA.</p>{downloads([('checks/component_correlations.csv','All component correlations'),('checks/audit_checks.csv','Verification checklist')])}</details></section>'''

appendix_scoring = f'''<section id="appendix-scoring"><span class="eyebrow">APPENDIX A · SCORING SENSITIVITY</span><h2>How department classifications compare across five methods</h2><p>All methods use the same six demand domains and six supply components. Reverse burden to stock continuity before any supply ranking, averaging or normalization. These comparisons examine dependence on the normalization and decision rule.</p>{table(['Department']+method_names,comparison_rows)}{table(['Approach','Construction','Threshold'],method_rows)}<p>Median ties are unflagged. The median-count method requires a strict majority: four of six unfavorable components on either side. The rank-average, mean-percentage and Median/IQR comparisons retain separate final-score medians. The primary z-score method uses zero, its final-score mean. These classifications are relative to the linked departments, not absolute standards.</p>{downloads([('classification_comparison.csv','Five-method comparison'),('classification_thresholds.csv','Thresholds'),('all_method_scores_and_classifications.csv','Scores and classifications')])}</section>'''

load_rows = [[r['side'],esc('Stock continuity (1 − stockout burden)' if r['side']=='Supply' and r['variable']=='stockout_burden' else labels[r['variable']]),f'{r["loading"]:+.3f}'] for r in pc['loadings'] if r['PC']==1]
var_rows = [[r['side'],f'PC{r["PC"]}',pct(r['variance_pct']),pct(r['cumulative_pct'])] for r in pc['variance']]
negative_supply = pc['audit'].get('negative_supply_PC1_loadings',[])
if isinstance(negative_supply,str): negative_supply=[negative_supply]
pc_note = ('Supply PC1 has mixed signs: '+', '.join(labels[k] for k in negative_supply)+' run opposite to the other provision components. Its negative is a supply pattern score, not uniformly ordered risk.' if negative_supply else 'Supply PC1 is oriented toward favorable provision and reversed for modeling.')
pca = f'''<section id="pca"><span class="eyebrow">APPENDIX B · PCA SENSITIVITY</span><h2>Alternative scores from the same individual and department inputs</h2><p>Separate weighted PCAs use six personal demand indicators and six assigned department supply measures, including daily provision and stock continuity (1 − burden). Demand PCA uses {pc['audit']['demand_PCA_n']:,} complete women; supply PCA uses {pc['audit']['supply_PCA_women_n']:,} women across {pc['audit']['supply_profiles']} department profiles. Repeated supply profiles are weighted by the summed WEIGHT of represented women; these are not facility weights.</p>{figure('pca_appendix/01_pca_loadings.svg','Signed PCA coefficients for personal demand and department supply.')}{note(f'<strong>Interpretation.</strong> Demand PC1 retains {pc["audit"]["demand_PC1_variance"]:.1f}% of demand variation; supply PC1 retains {pc["audit"]["supply_PC1_variance"]:.1f}%. {pc_note} PCA loadings describe variation in the inputs, not their association with modern use or their program importance. The equal-weight z-score remains primary.')}{figure('pca_appendix/02_pca_variance.svg','Variance retained and exploratory permutation references.')}<details class="model-detail"><summary>PCA construction and stability</summary>{table(['Side','Input','PC1 coefficient'],load_rows)}{table(['Side','Component','Variance retained','Cumulative'],var_rows)}<p>Standardize against final-weighted input means and population SDs; eigendecompose each correlation matrix and project onto PC1. Scores require all selected inputs. Two thousand exploratory permutations and leave-one-department-out checks examine construction stability. They are not survey-design significance tests or evidence of a causal constraint.</p>{downloads([('pca_appendix/pca_loadings.csv','PCA coefficients'),('pca_appendix/pca_variance.csv','PCA variance and permutation checks'),('pca_appendix/pca_leave_one_department_out.csv','PCA stability')])}</details>{models(pm,'pca_appendix/models',pca=True)}</section>'''

css += '''#supply-department-components .planning,#demand-department-components .planning{table-layout:fixed;width:100%}#supply-department-components .planning th:first-child,#demand-department-components .planning th:first-child{width:22%}#supply-department-components .planning td,#demand-department-components .planning td{width:26%;white-space:normal}#supply-department-components .planning small{color:#617986;font-size:11px;font-weight:400}'''

js = '''const DATA=__DATA__;const tip=document.querySelector('.tooltip');
const fmt=v=>v==null?'Not surveyed':v.toFixed(1)+'%';
function hide(){tip.classList.remove('show')}
function move(e){let x=e.clientX+17,y=e.clientY+17;if(x+tip.offsetWidth>innerWidth-10)x=e.clientX-tip.offsetWidth-17;if(y+tip.offsetHeight>innerHeight-10)y=e.clientY-tip.offsetHeight-17;tip.style.left=Math.max(10,x)+'px';tip.style.top=Math.max(10,y)+'px'}
function show(el,e){const d=DATA[el.dataset.key];if(!d)return;tip.innerHTML='<strong>'+d.name+'</strong><p>'+(el.dataset.mode==='coverage'?d.coverage_label:(el.dataset.mode==='sensitivity'?d.alternative_category:d.category))+'</p><div class="two"><div><b>'+fmt(d.cpr)+'</b><br><small>Any-method use</small></div><div><b>'+fmt(d.mcpr)+'</b><br><small>Modern use</small></div></div><p>'+d.n+' women with original FPN2 = 1.</p><p>'+d.source_rows+' source women · '+d.facilities+' facility records · '+d.fp_facilities+' confirmed FP providers</p>'+(['classification','sensitivity'].includes(el.dataset.mode)&&d.demand!=null?'<p>Department demand score: '+d.demand.toFixed(3)+'<br>Department supply readiness: '+(el.dataset.mode==='sensitivity'?d.alternative_supply:d.supply).toFixed(3)+'</p>':'')+(el.dataset.mode==='sensitivity'&&d.complete_facilities!=null?'<p>'+d.complete_facilities+' complete facilities retained in this department.</p>':'')+(d.fallback_need?'<p>'+d.fallback_need+' women in need receive department-level fallbacks in the models.</p>':'')+(d.small_sample?'<p class="sample-alert">† Small need-group sample: n='+d.n+'. Interpret department estimates and categories cautiously.</p>':'')+'<small>Weighted use among women in need; department-wide supply for planning.</small>';tip.classList.add('show');move(e)}
document.querySelectorAll('[data-key][data-mode]').forEach(el=>{el.addEventListener('pointerenter',e=>show(el,e));el.addEventListener('pointermove',move);el.addEventListener('pointerleave',hide);el.addEventListener('focus',()=>{let r=el.getBoundingClientRect();show(el,{clientX:r.x+r.width/2,clientY:r.y+r.height/2})});el.addEventListener('blur',hide);el.addEventListener('click',e=>show(el,e))});document.addEventListener('keydown',e=>{if(e.key==='Escape')hide()});
'''.replace('__DATA__',json.dumps(DATA,separators=(',',':')))
page = '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Senegal department analysis | SURGO × Manitoba</title><style>'+css+'</style></head><body>'+header+'<main>'+coverage+use+joint+model+defs+appendix_scoring+pca+'</main><div class="tooltip" role="status" aria-live="polite"></div><footer style="max-width:1430px;margin:20px auto 35px;padding:0 30px;color:#6a8290;font-size:12px">Senegal · department-wide supply assignment for every woman · original SURGO FPN2 = 1 · initial cross-sectional evidence.</footer><script>'+js+'</script></body></html>'
output.parent.mkdir(parents=True,exist_ok=True)
page = french_terms(page)
output.write_text(page,encoding="utf-8")
print('Rendered',output.name,'with 46 department boundaries, 33 planning profiles, original FPN2 need group and ten primary/appendix modern-use fits, six-domain main supply and the retained scoring/PCA appendices.')
