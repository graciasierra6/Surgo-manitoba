"""Build country score distributions and maps at zero, +0.5, +1 and +1.5 SD cutoffs.

Published aggregate scores, geography and colors are preserved. Each side retains
its own reference set; unassessed categories remain unassessed at every cutoff.
Run: python3 analysis/build_threshold_map_report.py
"""
import argparse
from collections import Counter
from html import escape
import hashlib
import json
import math
from pathlib import Path
import re
import statistics
import xml.etree.ElementTree as ET

from new_maps import load_map
from regional_sd_maps import load_country, ORDER


CONFIG = [
    ('cotedivoire', 'Côte d’Ivoire', 'Cote_dIvoire_analysis_aggregate.html', 82, 77),
    ('lagos', 'Lagos', 'Lagos_analysis_aggregate.html', 17, 17),
    ('senegal', 'Senegal', 'Senegal_analysis_aggregate.html', 33, 33),
    ('sindh', 'Sindh', 'Sindh_analysis.html', 30, 30),
]
RULES = [('current', None, 'Current map · Above area average'),
         ('half', .5, '+0.5 SD or more above area average'),
         ('one', 1., '+1 SD or more above area average'),
         ('one_half', 1.5, '+1.5 SD or more above area average')]


def category(demand, supply):
    return ('Both' if demand and supply else 'Demand only' if demand else
            'Supply only' if supply else 'Neither flagged')


def load_lagos(path):
    source, svg = load_map(path, 'lagos', 17)
    reference_rows = [r for r in source['areas'].values() if r['assessed']]
    dm = statistics.mean(r['demand'] for r in reference_rows)
    sm = statistics.mean(r['constraints'] for r in reference_rows)
    ds = statistics.stdev(r['demand'] for r in reference_rows)
    ss = statistics.stdev(r['constraints'] for r in reference_rows)
    assert abs(dm) < 1e-10 and abs(sm) < 1e-10
    rows = {}
    for key, r in source['areas'].items():
        rows[key] = dict(name=r['name'], assessed=r['assessed'], current=r['category'],
                         demand_u=(r['demand'] - dm) / ds if r['assessed'] else None,
                         supply_u=(r['constraints'] - sm) / ss if r['assessed'] else None,
                         n=r['n'], cpr=r['cpr'], mcpr=r['mcpr'])
    return dict(areas=rows, palette=source['palette'], assessed=17,
                reference=dict(demand_n=17, supply_n=17, demand_mean=dm, supply_mean=sm,
                               demand_SD=ds, supply_SD=ss),
                source_report=path.name, source_sha256=hashlib.sha256(path.read_bytes()).hexdigest()), svg


def display_map(svg_text, map_id, rows, palette, rule, title):
    svg = ET.fromstring(svg_text)
    ids = {n.get('id'): map_id + '-' + n.get('id') for n in svg.iter() if n.get('id')}
    colors = dict(palette)
    for c, color in colors.items():
        for before, after in ids.items():
            color = color.replace('url(#' + before + ')', 'url(#' + after + ')')
        colors[c] = color
    for node in svg.iter():
        for attr, value in list(node.attrib.items()):
            if attr == 'id':
                node.set(attr, ids[value])
            else:
                for before, after in ids.items():
                    value = value.replace('url(#' + before + ')', 'url(#' + after + ')')
                node.set(attr, value)
        if node.get('data-area'):
            r = rows[node.get('data-area')]
            node.set('fill', colors[r['categories'][rule]])
            node.set('aria-label', r['name'] + ': ' + r['categories'][rule])
        if node.tag.rsplit('}', 1)[-1] == 'text':
            if node.text:
                node.text = node.text.replace('†', '').strip()
    # This edition contains map titles and legends only; sample coverage remains
    # inspectable in area tooltips and the original source reports.
    for parent in svg.iter():
        for child in list(parent):
            if child.get('class') == 'map-caption' and ''.join(child.itertext()).strip() != 'N ↑':
                parent.remove(child)
    svg.set('id', map_id)
    svg.set('aria-label', title)
    return ET.tostring(svg, encoding='unicode'), colors


def legend_swatch(color):
    if color.startswith('url('):
        return ('<svg class="legend-swatch" aria-hidden="true" viewBox="0 0 14 14">'
                '<rect width="14" height="14" fill="' + escape(color) + '"/></svg>')
    return '<i class="legend-swatch" style="background:' + escape(color) + '"></i>'


def distribution_plot(country, title, rows, reference):
    """Show exact score positions; vertical offsets only prevent dot overlap."""
    series = [('demand_u', 'Demand concerns', '#047f8b', reference['demand_n']),
              ('supply_u', 'Supply constraints', '#cd7729', reference['supply_n'])]
    values = [r[field] for field, _, _, _ in series for r in rows.values() if r[field] is not None]
    left = math.floor(min(values) * 2) / 2 - .25
    right = math.ceil(max(values) * 2) / 2 + .25
    x0, x1 = 50, 1150
    def xpos(value):
        return x0 + (value - left) / (right - left) * (x1 - x0)
    packed = []
    max_offset = 0
    for field, label, color, expected in series:
        points = []
        for area, row in sorted(rows.items(), key=lambda item: float('inf') if item[1][field] is None else item[1][field]):
            value = row[field]
            if value is None:
                continue
            x = xpos(value)
            nearby = [p for p in points if abs(p['x'] - x) < 17]
            slots = [0] + [sign * step * 17 for step in range(1, 101) for sign in (1, -1)]
            offset = next(y for y in slots if all(abs(p['offset'] - y) >= 16 for p in nearby))
            max_offset = max(max_offset, abs(offset))
            points.append(dict(area=area, x=x, offset=offset, value=value))
        assert len(points) == expected
        packed.append((field, label, color, points))
    baseline = max(170, max_offset + 135)
    line_top = baseline - max(52, max_offset + 13)
    line_bottom = baseline + max(52, max_offset + 13)
    row_height = line_bottom + 55
    height = 2 * row_height + 65
    parts = [f'<svg class="distribution-svg" viewBox="0 0 1200 {height}" role="img" '
             f'aria-label="{escape(title)} demand and supply score distributions">',
             '<title>Demand and supply scores in final-composite standard-deviation units</title>',
             '<desc>Each dot represents one area. Vertical offsets separate overlapping dots. '
             'Reference lines mark the average, plus half a standard deviation and plus one and plus one-and-a-half standard deviations.</desc>']
    for index, (field, label, color, points) in enumerate(packed):
        top = index * row_height
        above = sum(p['value'] > 1e-10 for p in points)
        half = sum(p['value'] >= .5 for p in points)
        one = sum(p['value'] >= 1 for p in points)
        one_half = sum(p['value'] >= 1.5 for p in points)
        parts.append(f'<g class="distribution-series" data-side="{field}">'
                     f'<text x="40" y="{top + 28}" class="dist-title">{label}</text>'
                     f'<text x="50" y="{top + 60}" class="dist-counts">Above average: {above} / {len(points)}'
                     f'     ≥ +0.5 SD: {half} / {len(points)}     ≥ +1 SD: {one} / {len(points)}     ≥ +1.5 SD: {one_half} / {len(points)}</text>'
                     f'<line x1="{x0}" x2="{x1}" y1="{top + baseline}" y2="{top + baseline}" class="dist-baseline"/>')
        for cutoff, caption in [(0, 'Area average'), (.5, '+0.5 SD'), (1, '+1 SD'), (1.5, '+1.5 SD')]:
            x = xpos(cutoff)
            parts.append(f'<line x1="{x:.3f}" x2="{x:.3f}" y1="{top + line_top}" y2="{top + line_bottom}" '
                         f'class="dist-reference{" dist-cutoff" if cutoff else ""}"/>')
            parts.append(f'<text x="{x:.3f}" y="{top + line_top - 18}" class="dist-threshold" text-anchor="middle">{caption}</text>')
            if cutoff == 0:
                parts.append(f'<text x="{x:.3f}" y="{top + line_top - 1}" class="dist-threshold" text-anchor="middle">0</text>')
        for p in points:
            name = rows[p['area']]['name']
            parts.append(f'<circle cx="{p["x"]:.3f}" cy="{top + baseline + p["offset"]}" r="7.4" '
                         f'fill="{color}" class="distribution-dot" data-area="{escape(p["area"])}" '
                         f'data-score="{p["value"]:.17g}" data-side="{field}" role="button" tabindex="0" '
                         f'aria-label="{escape(name)}: {label} {p["value"]:+.3f} SD"/>')
        parts.append('</g>')
    ticks = sorted(set(range(math.ceil(left), math.floor(right) + 1)) | {.5})
    for tick in ticks:
        value = ('−' + f'{abs(tick):g}') if tick < 0 else f'{tick:g}'
        parts.append(f'<text x="{xpos(tick):.3f}" y="{2 * row_height + 10}" class="dist-tick" text-anchor="middle">{value}</text>')
    parts.append(f'<text x="600" y="{height - 10}" text-anchor="middle" class="dist-axis">'
                 'Distance from the area average (standard deviations)</text></svg>')
    return (f'<article class="distribution-panel" id="{country}-distribution" data-country="{country}">'
            '<h2>Distribution of area scores</h2>'
            '<div class="distribution-key">Each dot represents one area; higher values indicate more measured challenges</div>'
            + ''.join(parts) + '</article>')


TEMPLATE = r'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Demand and supply maps</title>
<style>
:root{--ink:#17374a;--muted:#59717f;--line:#dce6eb;--teal:#047f8b}*{box-sizing:border-box}body{margin:0;background:#f3f6f8;color:var(--ink);font:15px/1.5 Arial,Helvetica,sans-serif}.sidebar{position:fixed;left:0;top:0;bottom:0;width:224px;padding:32px 18px;background:white;border-right:1px solid var(--line)}.sidebar h2{font-size:15px;font-weight:600;margin:0 12px 22px}.country-tabs{display:flex;flex-direction:column;gap:8px}.country-tabs button{text-align:left;font:inherit;min-height:45px;border:0;border-radius:5px;padding:11px 13px;color:var(--ink);background:transparent;cursor:pointer}.country-tabs button[aria-selected="true"]{color:white;background:var(--ink)}button:focus-visible{outline:3px solid #83bdc5;outline-offset:3px}main{margin-left:224px;padding:28px 30px 40px}.country{max-width:1450px;margin:auto}.country[hidden]{display:none}h1{font-size:28px;line-height:1.3;margin:0 0 22px;font-weight:600}h2{font-size:20px;font-weight:600;line-height:1.4;margin:0 0 18px}.map-panel{background:white;border:1px solid var(--line);border-radius:8px;padding:24px;margin-bottom:26px;scroll-margin-top:24px}.geo-map{width:100%;height:auto;display:block}.geo-map [data-area]{cursor:pointer;stroke:white;stroke-width:1.1;vector-effect:non-scaling-stroke}.geo-map [data-area]:hover,.geo-map [data-area]:focus{stroke:var(--ink);stroke-width:2.2;outline:none}.map-label{font-size:11.5px;fill:#254453;font-weight:600;paint-order:stroke;stroke:white;stroke-width:3;stroke-linejoin:round;font-family:Arial,Helvetica,sans-serif;pointer-events:none}.label-bg{fill:white;fill-opacity:.95;stroke:#d4dfe4;stroke-width:.6}.leader{stroke:#93a8b3;stroke-width:1;fill:none;pointer-events:none}.inset-heading{font-size:16px;fill:var(--ink);font-weight:600}.map-caption,.orientation{font-size:12px;fill:var(--muted)}.legend{display:flex;flex-wrap:wrap;gap:9px 20px;margin-top:16px;font-size:13px}.legend span{display:inline-flex;align-items:center;gap:6px}.legend-swatch{display:inline-block;width:14px;height:14px;border:1px solid #d5dfe5;flex:none}.tooltip{display:none;position:fixed;z-index:50;pointer-events:none;background:var(--ink);color:white;padding:12px 15px;border-radius:5px;max-width:310px;font-size:13px}.tooltip strong{font-size:15px}.tooltip p{margin:5px 0}.country-tabs button:hover:not([aria-selected="true"]){background:#edf3f5}@media(max-width:850px){.sidebar{position:static;width:auto;padding:16px;border-right:0;border-bottom:1px solid var(--line)}.sidebar h2{margin:0 6px 10px}.country-tabs{flex-direction:row;flex-wrap:wrap;gap:5px}.country-tabs button{padding:9px 12px}main{margin-left:0;padding:22px 12px}h1{font-size:25px}h2{font-size:18px}.map-panel{padding:16px;margin-bottom:20px}}@media(max-width:600px){.geo-map .lga-name,.geo-map .map-label,.geo-map .leader{display:none}.legend{font-size:12px;gap:8px 13px}.tooltip{max-width:270px}}@media print{body{background:white}.sidebar,.tooltip{display:none}main{margin:0;padding:0}.country[hidden]{display:block}.country{break-before:page}.country:first-child{break-before:auto}.map-panel{break-inside:avoid;border:0;padding:0}.geo-map{max-height:750px}}
.distribution-panel{background:white;border:1px solid var(--line);border-radius:8px;padding:24px;margin-bottom:26px}.distribution-panel h2{margin-bottom:6px}.distribution-key{font-size:14px;color:var(--muted);margin-bottom:18px}.distribution-svg{display:block;width:100%;height:auto}.distribution-svg text{font-family:Arial,Helvetica,sans-serif;fill:var(--ink);white-space:pre}.dist-title{font-size:21px;font-weight:600}.distribution-svg .dist-counts{font-size:15px;fill:var(--muted)}.dist-baseline{stroke:#d9e3e8;stroke-width:1}.dist-reference{stroke:#718795;stroke-width:1}.dist-cutoff{stroke-dasharray:6 6}.dist-threshold,.dist-tick{font-size:15px}.dist-axis{font-size:18px}.distribution-dot{cursor:pointer}.distribution-dot:hover,.distribution-dot:focus{stroke:var(--ink);stroke-width:2;outline:none}@media(max-width:850px){.distribution-panel{padding:16px;margin-bottom:20px}}@media(max-width:600px){.distribution-key{font-size:13px}.dist-title{font-size:28px}.distribution-svg .dist-counts{font-size:20px}.dist-threshold,.dist-tick{font-size:23px}.dist-axis{font-size:25px}.distribution-panel{padding:16px 8px}.distribution-svg{min-height:280px}.distribution-svg .dist-threshold{font-size:20px}}@media print{.distribution-panel{break-inside:avoid;border:0;padding:0}}
</style></head><body>
<aside class="sidebar"><h2>Table of contents</h2><nav class="country-tabs" role="tablist" aria-label="Countries" aria-orientation="vertical">__TABS__</nav></aside>
<main>__COUNTRIES__</main><div id="map-tooltip" class="tooltip" role="tooltip"></div>
<script id="map-data" type="application/json">__DATA__</script>
<script>
const DATA=JSON.parse(document.getElementById('map-data').textContent),tip=document.getElementById('map-tooltip'),tabs=[...document.querySelectorAll('[role="tab"]')];
const fmt=v=>v==null?'—':(v<0?'−':'+')+Math.abs(v).toFixed(3),pct=v=>v==null?'—':v.toFixed(1)+'%';
function choose(country,scroll=true){if(!DATA[country])country='cotedivoire';for(const t of tabs)t.setAttribute('aria-selected',String(t.dataset.country===country));for(const p of document.querySelectorAll('.country'))p.hidden=p.id!==country;tip.style.display='none';if(location.hash!=='#'+country)history.replaceState(null,'','#'+country);if(scroll)window.scrollTo(0,0)}
function tooltip(country,rule,path,event){const r=DATA[country].areas[path.dataset.area];tip.replaceChildren();const title=document.createElement('strong');title.textContent=r.name;tip.appendChild(title);const lines=[r.categories[rule],'Demand: '+fmt(r.demand_u)+' SD','Supply constraints: '+fmt(r.supply_u)+' SD'];if(r.cpr!=null&&r.mcpr!=null)lines.push('CPR '+pct(r.cpr)+' · mCPR '+pct(r.mcpr));if(r.n)lines.push('Women in need of FP: n = '+r.n);for(const text of lines){const p=document.createElement('p');p.textContent=text;tip.appendChild(p)}tip.style.display='block';const b=path.getBoundingClientRect(),x=event.clientX??b.left+b.width/2,y=event.clientY??b.top;tip.style.left=Math.max(8,Math.min(innerWidth-tip.offsetWidth-8,x+14))+'px';tip.style.top=Math.max(8,Math.min(innerHeight-tip.offsetHeight-8,y+14))+'px'}
for(const t of tabs){t.addEventListener('click',()=>choose(t.dataset.country));t.addEventListener('keydown',event=>{const i=tabs.indexOf(t);let next;if(event.key==='ArrowDown'||event.key==='ArrowRight')next=(i+1)%tabs.length;if(event.key==='ArrowUp'||event.key==='ArrowLeft')next=(i+tabs.length-1)%tabs.length;if(event.key==='Home')next=0;if(event.key==='End')next=tabs.length-1;if(next!=null){event.preventDefault();tabs[next].focus();choose(tabs[next].dataset.country)}})}
for(const panel of document.querySelectorAll('.map-panel')){const country=panel.dataset.country,rule=panel.dataset.rule;panel.querySelectorAll('path[data-area]').forEach(path=>{path.addEventListener('mouseenter',e=>tooltip(country,rule,path,e));path.addEventListener('mousemove',e=>tooltip(country,rule,path,e));path.addEventListener('mouseleave',()=>tip.style.display='none');path.addEventListener('focus',e=>tooltip(country,rule,path,e));path.addEventListener('blur',()=>tip.style.display='none');path.addEventListener('click',e=>tooltip(country,rule,path,e))})}
function distributionTip(country,path,event){const r=DATA[country].areas[path.dataset.area];tip.replaceChildren();const title=document.createElement('strong');title.textContent=r.name;tip.appendChild(title);for(const text of ['Demand concerns: '+fmt(r.demand_u)+' SD','Supply constraints: '+fmt(r.supply_u)+' SD']){const p=document.createElement('p');p.textContent=text;tip.appendChild(p)}tip.style.display='block';const b=path.getBoundingClientRect(),x=event.clientX??b.left+b.width/2,y=event.clientY??b.top;tip.style.left=Math.max(8,Math.min(innerWidth-tip.offsetWidth-8,x+14))+'px';tip.style.top=Math.max(8,Math.min(innerHeight-tip.offsetHeight-8,y+14))+'px'}
for(const panel of document.querySelectorAll('.distribution-panel')){panel.querySelectorAll('.distribution-dot').forEach(dot=>{for(const event of ['mouseenter','mousemove','focus','click'])dot.addEventListener(event,e=>distributionTip(panel.dataset.country,dot,e));for(const event of ['mouseleave','blur'])dot.addEventListener(event,()=>tip.style.display='none')})}
window.addEventListener('hashchange',()=>choose(location.hash.slice(1)));window.addEventListener('scroll',()=>tip.style.display='none',{passive:true});document.addEventListener('keydown',e=>{if(e.key==='Escape')tip.style.display='none'});choose(location.hash.slice(1),false);
</script></body></html>'''


def main():
    root = Path(__file__).resolve().parents[2]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--reports-dir', type=Path, default=root / 'reports')
    parser.add_argument('--output', type=Path, default=root / 'reports/Demand_supply_threshold_maps.html')
    parser.add_argument('--countries', default='all')
    parser.add_argument('--workflow', choices=['administrative_area', 'urban_rural'], default='administrative_area')
    parser.add_argument('--source-report', type=Path)
    parser.add_argument('--data-file', type=Path)
    parser.add_argument('--embed-in-source', action='store_true')
    args = parser.parse_args()
    selected = [r[0] for r in CONFIG] if args.countries == 'all' else args.countries.split(',')
    if not set(selected).issubset({r[0] for r in CONFIG}): parser.error('Unknown geography')
    config = [r for r in CONFIG if r[0] in selected]
    if args.source_report and len(config) != 1: parser.error('--source-report requires one country')
    if args.workflow == 'urban_rural':
        names = {'lagos':'Lagos_rural_urban_updated.html', 'senegal':'Senegal_rural_urban_updated.html',
                 'cotedivoire':'CotedIvoire_rural_urban_updated.html', 'sindh':'Sindh_analysis.html'}
        config = [(k,t,names[k],dn,sn) for k,t,_,dn,sn in config]
    statistical = json.loads(args.data_file.read_text(encoding='utf-8')) if args.data_file else None
    data, sections, tabs = {}, [], []
    for key, title, filename, dn, sn in config:
        source_report = args.source_report or args.reports_dir / filename
        if key == 'lagos':
            d, svg = load_lagos(source_report)
        else:
            d, svg, _ = load_country(source_report, key, title, dn, sn, .5)
        rows = d['areas']
        counts = {}
        for r in rows.values():
            r['categories'] = {}
            for rule, threshold, _ in RULES:
                result = r['current']
                if r['assessed'] and threshold is not None:
                    result = category(r['demand_u'] >= threshold, r['supply_u'] >= threshold)
                r['categories'][rule] = result
            if r['assessed']:
                assert category(r['demand_u'] > 1e-10, r['supply_u'] > 1e-10) == r['current']
            for unused in ['alternative', 'demand_band', 'supply_band']:
                r.pop(unused, None)
        if statistical is not None:
            expected = {(r['geo'], r['threshold_sd']):r for r in statistical['sd_thresholds']}
            expected_names = {'both':'Both', 'demand':'Demand only', 'supply':'Supply only',
                              'neither':'Neither flagged', 'unassessed':'Supply unassessed'}
            for area, r in rows.items():
                if not r['assessed']: continue
                for rule, threshold, _ in RULES:
                    rr = expected[area, 0 if threshold is None else threshold]
                    assert r['categories'][rule] == expected_names[rr['category']]
                    assert abs(r['demand_u'] - rr['demand_sd']) < 1e-8
                    assert abs(r['supply_u'] - rr['supply_sd']) < 1e-8
        cards = []
        order = [c for c in ORDER + ['Supply data only'] if c in d['palette']]
        for rule, _, heading in RULES:
            counts[rule] = dict(Counter(r['categories'][rule] for r in rows.values()))
            geometry, palette = display_map(svg, key + '-' + rule, rows, d['palette'], rule, title + ': ' + heading)
            legend = ''.join('<span>' + legend_swatch(palette[c]) + escape(c) + ' · '
                             + str(counts[rule].get(c, 0)) + '</span>' for c in order)
            cards.append(f'<article class="map-panel" id="{key}-{rule}-panel" data-country="{key}" data-rule="{rule}">'
                         f'<h2>{escape(heading)}</h2>{geometry}<div class="legend">{legend}</div></article>')
        data[key] = dict(title=title, areas=rows, reference=d['reference'], counts=counts,
                         source_report=d['source_report'], source_sha256=d['source_sha256'])
        tabs.append(f'<button type="button" id="tab-{key}" role="tab" aria-selected="false" '
                    f'aria-controls="{key}" data-country="{key}">{escape(title)}</button>')
        sections.append(f'<section class="country" id="{key}" role="tabpanel" aria-labelledby="tab-{key}" hidden>'
                        f'<h1>{escape(title)}</h1>' + distribution_plot(key, title, rows, d['reference'])
                        + ''.join(cards) + '</section>')
    html = TEMPLATE.replace('__TABS__', ''.join(tabs)).replace('__COUNTRIES__', ''.join(sections)).replace(
        '__DATA__', json.dumps(data, ensure_ascii=False, separators=(',', ':')))
    assert not re.search(chr(47)+'Us'+'ers'+chr(47)+'|'+chr(47)+'ho'+'me'+chr(47)+r'|file:[/][/]|[/]private[/]var[/]|__\w+__', html)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(html, encoding='utf-8')
    if args.embed_in_source:
        if not args.source_report: parser.error('--embed-in-source requires --source-report')
        page = args.source_report.read_text(encoding='utf-8')
        section = ('<section id="score-thresholds"><h2>Risk score distributions and SD maps</h2>'
                   '<p>Both scores increase with measured constraints. SD cutoffs use each side’s distribution '
                   'of final area scores; they are descriptive alternatives to the primary above-zero flags.</p>'
                   '<details><summary>View distributions and maps: above average, +0.5, +1 and +1.5 SD</summary>'
                   '<iframe title="Area risk distributions and threshold maps" loading="lazy" '
                   'style="width:100%;height:1100px;border:1px solid #dce6eb" srcdoc="' + escape(html, quote=True) + '"></iframe>'
                   '</details></section>')
        assert 'id="score-thresholds"' not in page
        page = page.replace('</main>', section + '</main>', 1)
        args.source_report.write_text(page, encoding='utf-8')
    print(json.dumps({k: d['counts'] for k, d in data.items()}, ensure_ascii=False))


if __name__ == '__main__':
    main()
