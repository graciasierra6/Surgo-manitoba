"""Export Lagos and Senegal maps with supply constraints = negative readiness.

Uses published aggregate report values and their existing projected SVG boundaries.
No inputs are re-estimated, no geographic assignments change, and all categories
must reconcile before the self-contained HTML is written.

Run from the project root:
    python3 analysis/new_maps.py --output reports/New_maps.html
Use --reports-dir to select another parameterized report directory.
"""
import argparse
from collections import Counter
from html import escape
import json
import math
from pathlib import Path
import re
import xml.etree.ElementTree as ET

ET.register_namespace('', 'http://www.w3.org/2000/svg')
CATEGORIES = ['Demand only', 'Supply only', 'Both', 'Neither flagged', 'Supply data only']


def load_map(report, country, expected_assessed):
    text = report.read_text(encoding='utf-8')
    match = re.search(r'const DATA=(\{)', text)
    if not match: raise ValueError('Stored area map data missing')
    start = match.start(1)
    original, _ = json.JSONDecoder().raw_decode(text[start:])
    constraints_oriented = 'data-supply-score-orientation="constraints"' in text
    selected = None
    for match in re.finditer(r'<svg\b[\s\S]*?</svg>', text):
        svg = ET.fromstring(match.group())
        paths = [p for p in svg.iter() if p.tag.rsplit('}', 1)[-1] == 'path']
        if country == 'lagos' and svg.get('id') == 'map-zscore':
            selected = svg
            for group in svg.iter():
                if 'lga-region' not in group.get('class', '').split():
                    continue
                group.attrib.pop('tabindex', None)
                group.attrib.pop('role', None)
                for path in group:
                    if path.tag.rsplit('}', 1)[-1] == 'path':
                        path.set('data-area', group.get('data-key'))
        elif country == 'senegal' and any(p.get('data-mode') == 'classification' for p in paths):
            selected = svg
            for path in paths:
                if path.get('data-mode') == 'classification':
                    path.set('data-area', path.get('data-key'))
        if selected is not None:
            break
    if selected is None:
        raise ValueError('Classification geometry not found: ' + report.name)

    areas = {}
    for key, row in original.items():
        assessed = all(isinstance(row.get(k), (int, float)) and math.isfinite(row[k])
                       for k in ['demand', 'supply'])
        category = 'Supply data only'
        if assessed:
            df = row['demand'] > 1e-10
            constraints = row['supply'] if constraints_oriented else -row['supply']
            sf_original = constraints > 1e-10
            sf_reversed = -(-constraints) > 1e-10
            assert sf_original == sf_reversed
            category = 'Both' if df and sf_reversed else 'Demand only' if df else 'Supply only' if sf_reversed else 'Neither flagged'
            assert category == row['category'], (country, key, category, row['category'])
        areas[key] = dict(name=row['name'], assessed=assessed, demand=row.get('demand'),
                          readiness=(-row['supply'] if constraints_oriented else row['supply']) if assessed else None, constraints=(row['supply'] if constraints_oriented else -row['supply']) if assessed else None,
                          category=category, n=row.get('n', 0), cpr=row.get('cpr'), mcpr=row.get('mcpr'))
    assessed_n = sum(row['assessed'] for row in areas.values())
    assert assessed_n == expected_assessed
    palette = {}
    observed = set()
    for node in selected.iter():
        key = node.get('data-area')
        if key is None:
            continue
        observed.add(key)
        category = areas[key]['category']
        color = node.get('fill')
        if category in palette:
            assert palette[category] == color
        palette[category] = color
        node.set('tabindex', '0')
        node.set('role', 'button')
        node.set('aria-label', areas[key]['name'])
    assert observed == set(areas)
    assert set(palette) == set(CATEGORIES)
    selected.set('id', 'map-' + country)
    selected.set('class', selected.get('class', '') + ' geo-map')
    selected.set('aria-label', country.title() + ': demand and supply-constraints classification')
    selected.set('role', 'group')
    return dict(areas=areas, palette=palette, assessed=assessed_n,
                counts=dict(Counter(row['category'] for row in areas.values()))), ET.tostring(selected, encoding='unicode')


TEMPLATE = r'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>New maps</title>
<style>
:root{--ink:#17374a;--muted:#59717f;--line:#dce6eb;--teal:#047f8b}*{box-sizing:border-box}body{margin:0;background:#f3f6f8;color:var(--ink);font:15px/1.5 Arial,Helvetica,sans-serif}header,main{max-width:1400px;margin:auto;padding:25px 28px}header{padding-bottom:8px}h1{margin:0 0 8px;font-size:30px}h2{margin:0;font-size:24px}p{margin:7px 0}nav{display:flex;gap:18px;margin-top:15px}a{color:var(--teal)}.direction{display:flex;gap:8px;flex-wrap:wrap;margin-top:17px}button,select{font:inherit;border:1px solid #c7d6de;border-radius:5px;background:white;color:var(--ink);padding:8px 12px}button{cursor:pointer}button[aria-pressed="true"]{background:var(--ink);color:white;border-color:var(--ink)}button:focus-visible,select:focus-visible{outline:3px solid #75b9c2;outline-offset:2px}section{background:white;border:1px solid var(--line);border-radius:9px;padding:24px;margin-bottom:25px}.section-head{display:flex;justify-content:space-between;gap:15px;flex-wrap:wrap;align-items:center}.section-head label{display:flex;align-items:center;gap:8px}.check{font-size:13px;color:var(--muted)}.scores{display:flex;gap:28px;flex-wrap:wrap;margin:18px 0 12px}.score{min-width:120px}.score span{display:block;font-size:13px;color:var(--muted)}.score strong{font-size:21px;font-weight:600;font-variant-numeric:tabular-nums}.geo-map{width:100%;height:auto;display:block}.geo-map [data-area]{cursor:pointer;vector-effect:non-scaling-stroke;stroke:white;stroke-width:1.2}.geo-map [data-area]:hover,.geo-map [data-area]:focus{stroke:var(--ink);stroke-width:2;outline:none}.geo-map [data-area].selected{stroke:var(--ink);stroke-width:2.2}.map-label{fill:#254453;paint-order:stroke;stroke:white;stroke-width:3;stroke-linejoin:round;font-family:Arial,Helvetica,sans-serif}.label-bg{fill:white;fill-opacity:.95;stroke:#d4dfe4;stroke-width:.6}.leader{stroke:#93a8b3;stroke-width:1;fill:none}.inset-heading{font-size:16px;fill:var(--ink);font-weight:600}.map-caption,.orientation{font-size:12px;fill:var(--muted)}.legend{display:flex;gap:10px 19px;flex-wrap:wrap;margin-top:14px;font-size:13px}.legend span{display:inline-flex;align-items:center;gap:6px}.swatch{width:14px;height:14px;border:1px solid #d5dfe5;border-radius:2px}.context{font-size:13px;color:var(--muted);margin-top:14px}.tooltip{display:none;position:fixed;z-index:50;pointer-events:none;max-width:300px;padding:12px 15px;background:#17374a;color:white;border-radius:6px;box-shadow:0 3px 12px #17374a35;font-size:13px}.tooltip strong{font-size:15px}.tooltip p{margin:5px 0}.pill{display:inline-block;padding:3px 9px;background:#edf4f6;border-radius:4px;font-size:13px}.source{font-size:12px;color:var(--muted)}@media(max-width:650px){header,main{padding:18px 12px}section{padding:15px}.scores{gap:17px}.score strong{font-size:18px}.section-head label{width:100%}.section-head select{max-width:100%}.tooltip{max-width:260px}.geo-map .lga-name,.geo-map .map-label{display:none}}@media print{body{background:white}header,main{max-width:none;padding:12px}.direction,.tooltip{display:none}section{break-inside:avoid}.geo-map{max-height:560px}}
</style></head><body>
<header><h1>New maps</h1><p id="direction-summary">Higher demand and supply-constraints scores indicate more challenges. Above zero triggers the corresponding flag.</p><p class="check">Supply constraints = −supply readiness. All 17 Lagos and 33 Senegal classifications remain unchanged.</p><nav aria-label="Maps"><a href="#lagos">Lagos</a><a href="#senegal">Senegal</a></nav><div class="direction" role="group" aria-label="Supply score direction"><button type="button" data-direction="constraints" aria-pressed="true">Supply constraints: higher = worse</button><button type="button" data-direction="readiness" aria-pressed="false">Original readiness: higher = better</button></div></header>
<main>__SECTIONS__<p class="source">Demand values use survey weights among women in need of FP. Supply profiles are whole-area valid-response means among confirmed FP-providing facilities. Categories are relative to the study-area reference, not absolute adequacy standards. Published report boundaries and area assignments are unchanged.</p></main>
<div class="tooltip" role="tooltip" id="map-tooltip"></div>
<script id="map-data" type="application/json">__DATA__</script>
<script>
const DATA=JSON.parse(document.getElementById('map-data').textContent);let direction='constraints';const selected={lagos:'AGEGE',senegal:'MBACKE'};const tip=document.getElementById('map-tooltip');
const number=v=>v==null?'—':(v<0?'−':'+')+Math.abs(v).toFixed(3);const percent=v=>v==null?'—':v.toFixed(1)+'%';
function showTip(country,key,event){const r=DATA[country].areas[key];const score=direction==='constraints'?r.constraints:r.readiness;tip.replaceChildren();const title=document.createElement('strong');title.textContent=r.name;tip.appendChild(title);const lines=[r.category,r.assessed?'Demand score: '+number(r.demand)+' · Supply '+direction+': '+number(score):'No SURGO need-group demand estimate; joint category unassessed.',r.assessed?'CPR '+percent(r.cpr)+' · mCPR '+percent(r.mcpr):'',r.assessed?r.n+' women in need of FP':''];for(const text of lines){if(!text)continue;const p=document.createElement('p');p.textContent=text;tip.appendChild(p)}tip.style.display='block';const box=event.currentTarget.getBoundingClientRect();const x=event.clientX??box.left+box.width/2,y=event.clientY??box.top;tip.style.left=Math.max(8,Math.min(innerWidth-tip.offsetWidth-8,x+15))+'px';tip.style.top=Math.max(8,Math.min(innerHeight-tip.offsetHeight-8,y+15))+'px'}
function update(){document.querySelectorAll('[data-direction]').forEach(b=>b.setAttribute('aria-pressed',String(b.dataset.direction===direction)));document.getElementById('direction-summary').textContent=direction==='constraints'?'Higher demand and supply-constraints scores indicate more challenges. Above zero triggers the corresponding flag.':'Higher demand means more concerns; higher supply readiness means stronger provision. Demand is flagged above zero; supply below zero.';for(const [country,d] of Object.entries(DATA)){const section=document.getElementById(country),r=d.areas[selected[country]],s=direction==='constraints'?r.constraints:r.readiness;section.querySelector('select').value=selected[country];section.querySelector('[data-selected-name]').textContent=r.name;section.querySelector('[data-category]').textContent=r.category;section.querySelector('[data-demand]').textContent=number(r.demand);section.querySelector('[data-supply]').textContent=number(s);section.querySelector('[data-supply-label]').textContent=direction==='constraints'?'Supply constraints':'Supply readiness';section.querySelector('[data-flag]').textContent=r.assessed?((direction==='constraints'?s>1e-10:s< -1e-10)?'Yes':'No'):'Unassessed';section.querySelector('[data-rule]').textContent=direction==='constraints'?'Supply flag: score > 0':'Supply flag: score < 0';section.querySelector('[data-rates]').textContent=r.assessed?'Women in need: n = '+r.n+' · CPR '+percent(r.cpr)+' · mCPR '+percent(r.mcpr):'Supply data only; no joint classification.';section.querySelectorAll('[data-area]').forEach(p=>{const row=d.areas[p.dataset.area];p.classList.toggle('selected',p.dataset.area===selected[country]);p.setAttribute('aria-label',row.name+'. '+row.category+'. Supply '+direction+' '+number(direction==='constraints'?row.constraints:row.readiness));p.setAttribute('fill',d.palette[row.category])})}}
for(const [country,d] of Object.entries(DATA)){const section=document.getElementById(country),select=section.querySelector('select');for(const [key,r] of Object.entries(d.areas).sort((a,b)=>a[1].name.localeCompare(b[1].name))){const option=document.createElement('option');option.value=key;option.textContent=r.name;select.appendChild(option)}select.addEventListener('change',()=>{selected[country]=select.value;tip.style.display='none';update()});section.querySelectorAll('[data-area]').forEach(path=>{path.addEventListener('mouseenter',e=>showTip(country,path.dataset.area,e));path.addEventListener('mousemove',e=>showTip(country,path.dataset.area,e));path.addEventListener('mouseleave',()=>tip.style.display='none');path.addEventListener('focus',e=>showTip(country,path.dataset.area,e));path.addEventListener('blur',()=>tip.style.display='none');const choose=()=>{selected[country]=path.dataset.area;update()};path.addEventListener('click',choose);path.addEventListener('keydown',e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();choose()}})})}
document.querySelectorAll('[data-direction]').forEach(button=>button.addEventListener('click',()=>{direction=button.dataset.direction;tip.style.display='none';update()}));update();
</script></body></html>'''


def main():
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--reports-dir', type=Path, default=root / 'reports')
    parser.add_argument('--output', type=Path, default=root / 'reports/New_maps.html')
    args = parser.parse_args()
    data, sections = {}, []
    for country, title, filename, expected, unit in [
        ('lagos', 'Lagos', 'Lagos_analysis_aggregate.html', 17, 'LGAs'),
        ('senegal', 'Senegal', 'Senegal_analysis_aggregate.html', 33, 'départements')]:
        info, geometry = load_map(args.reports_dir / filename, country, expected)
        data[country] = info
        legend = ''.join('<span><i class="swatch" style="background:'+info['palette'][c]+'"></i>'+escape(c)+' · '+str(info['counts'][c])+'</span>' for c in CATEGORIES)
        sections.append(f'''<section id="{country}"><div class="section-head"><div><h2>{title}</h2><span class="check">{expected} of {expected} assessed {unit} retain their classification</span></div><label>{'LGA' if country=='lagos' else 'Département'}<select aria-label="Select {title} area"></select></label></div><p><strong data-selected-name></strong> <span class="pill" data-category></span></p><div class="scores"><div class="score"><span>Demand concerns</span><strong data-demand></strong></div><div class="score"><span data-supply-label>Supply constraints</span><strong data-supply></strong></div><div class="score"><span>Supply challenge flag</span><strong data-flag></strong></div></div><p class="check" data-rule>Supply flag: score &gt; 0</p>{geometry}<div class="legend">{legend}</div><p class="context" data-rates></p></section>''')
    html = TEMPLATE.replace('__SECTIONS__', '\n'.join(sections)).replace('__DATA__', json.dumps(data, ensure_ascii=False, separators=(',', ':')))
    assert not re.search(chr(47)+'Us'+'ers'+chr(47)+'|'+chr(47)+'ho'+'me'+chr(47)+r'|file:[/][/]|[/]private[/]var[/]', html)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(html, encoding='utf-8')
    print(json.dumps({'unchanged_classifications': {k: v['assessed'] for k, v in data.items()}, 'self_contained': True, 'bytes': len(html.encode('utf-8'))}))


if __name__ == '__main__':
    main()
