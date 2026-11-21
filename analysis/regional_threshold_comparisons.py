"""Compare the published zero-cutoff maps with a final-index SD cutoff.

Uses stored aggregate scores and boundaries without refitting indicators or models.
Run: python3 analysis/regional_threshold_comparisons.py --threshold 0.5
"""
import argparse
from html import escape
import json
import math
from pathlib import Path
import re
import xml.etree.ElementTree as ET

from lagos_threshold_comparison import TEMPLATE, table
from regional_sd_maps import load_country, fmt, ORDER


CONFIG = [
    ('cotedivoire', 'Côte d’Ivoire', 'Cote_dIvoire_analysis_aggregate.html',
     'départements', 'Département', 82, 77, 'ABIDJAN', 'Cote_dIvoire_threshold_comparison.html'),
    ('sindh', 'Sindh', 'Sindh_analysis.html',
     'districts', 'District', 30, 30, 'SHIKARPUR', 'Sindh_threshold_comparison.html'),
]


def clone_map(svg_text, mode):
    """Keep geometry fixed and give each map its own pattern identifiers."""
    svg = ET.fromstring(svg_text)
    ids = {n.get('id'): mode + '-' + n.get('id') for n in svg.iter() if n.get('id')}
    for node in svg.iter():
        for attr, value in list(node.attrib.items()):
            if attr == 'id':
                node.set(attr, ids[value])
            else:
                for before, after in ids.items():
                    value = value.replace('url(#' + before + ')', 'url(#' + after + ')')
                node.set(attr, value)
    svg.set('id', 'map-' + mode)
    return svg, ids


def legend_swatch(color):
    if color.startswith('url('):
        return ('<svg width="12" height="12" aria-hidden="true">'
                '<rect width="12" height="12" fill="' + escape(color) + '"/></svg>')
    return '<i style="background:' + escape(color) + '"></i>'


def build_report(reports_dir, output_dir, config, threshold):
    key, title, filename, units, singular, dn, sn, default_area, output = config
    d, svg_text, changes = load_country(reports_dir / filename, key, title, dn, sn, threshold)
    categories = [c for c in ORDER if c in d['palette']]
    panels, palettes = [], {}
    for mode, heading, rule, counts in [
        ('current', 'Prior: above zero', 'Flag demand > 0; flag supply constraints > 0', d['current_counts']),
        ('alternative', f'SD cutoff: +{threshold:g} SD or higher',
         f'Flag demand ≥ {threshold:g} SD; flag supply constraints ≥ {threshold:g} SD', d['alternative_counts']),
    ]:
        svg, ids = clone_map(svg_text, mode)
        palette = dict(d['palette'])
        for category, color in palette.items():
            for before, after in ids.items():
                color = color.replace('url(#' + before + ')', 'url(#' + after + ')')
            palette[category] = color
        palettes[mode] = palette
        svg.set('aria-label', title + ': ' + heading)
        for node in svg.iter():
            area = node.get('data-area')
            if area:
                node.set('fill', palette[d['areas'][area][mode]])
        legend = ''.join('<span>' + legend_swatch(palette[c]) + escape(c) + ' · '
                         + str(counts.get(c, 0)) + '</span>' for c in categories)
        panels.append(f'<section class="panel" id="{mode}"><h2>{heading}</h2>'
                      f'<p class="muted">{rule}</p><p class="category" data-category></p>'
                      '<div class="scores"><div>Demand · SD units<strong data-demand></strong></div>'
                      '<div>Supply constraints · SD units<strong data-supply></strong></div></div>'
                      + ET.tostring(svg, encoding='unicode') + '<div class="legend">' + legend + '</div></section>')

    base = TEMPLATE.replace('Lagos', title).replace('LGAs', units).replace('LGA', singular)
    for before, after in [('17 assessed', f"{d['assessed']} assessed"), ('of 17', f"of {d['assessed']}"),
                          ('All 17', f"All {d['assessed']}")]:
        base = base.replace(before, after)
    base = base.replace('current classification versus', 'prior classification versus')
    base = base.replace('Current', 'Prior').replace('Proposed', 'SD cutoff')
    base = base.replace('calculated equally across the same 17 assessed ' + units,
                        f'calculated equally across {dn} areas with valid demand scores and {sn} with valid supply scores')
    unknown_note = ('Five surveyed areas with unassessed supply remain unassessed; '
                    'demand standardization retains all 82 valid demand scores. '
                    'Areas with facility data only or neither source also retain their prior status.'
                    if key == 'cotedivoire' else 'All 30 survey districts retain complete area-level scores.')
    base = base.replace('Three ' + units + ' with supply data only remain unassessed under both rules.', unknown_note)
    base = base.replace("let selected='IBEJULEKKI'", 'let selected=' + json.dumps(default_area))
    base = base.replace("DATA.palette[r[mode]]", "DATA.palettes[mode][r[mode]]")
    base = base.replace(
        "const lines=[r[mode],r.assessed?'Demand: '+fmt(r.demand_u)+' SD · '+r.demand_band:'No complete demand/supply classification',r.assessed?'Supply constraints: '+fmt(r.supply_u)+' SD · '+r.supply_band:'Supply data only',r.assessed?'CPR '+r.cpr.toFixed(1)+'% · mCPR '+r.mcpr.toFixed(1)+'%':''];",
        "const lines=[r[mode],r.demand_u!=null?'Demand: '+fmt(r.demand_u)+' SD · '+r.demand_band:'Demand: unassessed',r.supply_u!=null?'Supply constraints: '+fmt(r.supply_u)+' SD · '+r.supply_band:'Supply constraints: unassessed',r.cpr!=null&&r.mcpr!=null?'CPR '+r.cpr.toFixed(1)+'% · mCPR '+r.mcpr.toFixed(1)+'%':''];")
    base = base.replace("path.classList.toggle('selected',path.dataset.area===selected);",
                        "path.classList.toggle('selected',path.dataset.area===selected);"
                        "path.classList.toggle('changed-area',document.getElementById('outline-changes').checked&&r.assessed&&r.current!==r.alternative);")
    base = base.replace("select.addEventListener('change',",
                        "document.getElementById('outline-changes').addEventListener('change',update);select.addEventListener('change',")
    base = base.replace('<div class="grid" id="comparison-maps">',
                        f'<label class="outline-control"><input type="checkbox" id="outline-changes" checked>'
                        f'Dark outlines mark the {len(changes)} {units} whose category changes</label>'
                        '<div class="grid" id="comparison-maps">')
    extra_css = '''main{max-width:3000px}.map-label{font-size:11.5px;fill:#254453;paint-order:stroke;stroke:white;stroke-width:3;stroke-linejoin:round;font-weight:600}.inset-heading{font-size:16px;fill:#17374a;font-weight:600}.map-caption,.orientation{font-size:12px;fill:#59717f}.geo-map [data-area].changed-area{stroke:#17374a;stroke-width:1.35}.geo-map [data-area].selected{stroke-width:2.7}.outline-control{display:flex;gap:8px;align-items:center;margin:0 0 16px;font-size:13px}.outline-control input{width:16px;height:16px}@media(max-width:850px){.map-label{display:none}}'''
    base = base.replace('</style>', extra_css + '</style>')
    rows = sorted(d['areas'].values(), key=lambda r: r['name'])
    changed_rows = [[r['name'], r['current'], r['alternative'], fmt(r['demand_u']), fmt(r['supply_u'])]
                    for r in sorted(changes, key=lambda r: r['name'])]
    all_rows = [[r['name'], fmt(r['demand_u']), r['demand_band'], fmt(r['supply_u']), r['supply_band'],
                 r['current'], r['alternative']] for r in rows if r['assessed']]
    r = d['reference']
    payload = dict(areas=d['areas'], palette=d['palette'], palettes=palettes, threshold_SD=threshold,
                   means={'demand': r['demand_mean'], 'supply_constraints': r['supply_mean']},
                   SDs={'demand': r['demand_SD'], 'supply_constraints': r['supply_SD']},
                   changed=len(changes), current_counts=d['current_counts'], alternative_counts=d['alternative_counts'],
                   source_report=d['source_report'], source_sha256=d['source_sha256'])
    replacements = {
        '__THRESHOLD__': f'{threshold:g}', '__CHANGED__': str(len(changes)),
        '__UNCHANGED__': str(d['assessed'] - len(changes)), '__PANELS__': ''.join(panels),
        '__COUNTS__': table(['Category', 'Prior', 'SD cutoff'],
                            [[c, d['current_counts'].get(c, 0), d['alternative_counts'].get(c, 0)] for c in categories], (1, 2)),
        '__CHANGES__': table([singular, 'Prior', 'SD cutoff', 'Demand · SD units', 'Supply · SD units'], changed_rows, (3, 4)),
        '__ALL_AREAS__': table([singular, 'Demand · SD', 'Demand band', 'Supply · SD', 'Supply band', 'Prior', 'SD cutoff'], all_rows, (1, 3)),
        '__D_SD__': f"{r['demand_SD']:.6f}", '__S_SD__': f"{r['supply_SD']:.6f}",
        '__D_CUT__': f"{r['original_demand_cutoff']:.6f}", '__S_CUT__': f"{r['original_supply_constraints_cutoff']:.6f}",
        '__DATA__': json.dumps(payload, ensure_ascii=False, separators=(',', ':')),
    }
    for token, value in replacements.items():
        base = base.replace(token, value)
    assert not re.search(chr(47)+'Us'+'ers'+chr(47)+'|'+chr(47)+'ho'+'me'+chr(47)+r'|file:[/][/]|[/]private[/]var[/]|__\w+__', base)
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / output).write_text(base, encoding='utf-8')
    return dict(geography=title, assessed=d['assessed'], changed=len(changes), output=output,
                current=d['current_counts'], alternative=d['alternative_counts'])


def main():
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--reports-dir', type=Path, default=root / 'reports')
    parser.add_argument('--output-dir', type=Path, default=root / 'reports')
    parser.add_argument('--threshold', type=float, default=.5)
    args = parser.parse_args()
    if not math.isfinite(args.threshold) or args.threshold <= 0:
        parser.error('Supply a finite positive --threshold')
    print(json.dumps([build_report(args.reports_dir, args.output_dir, c, args.threshold) for c in CONFIG], ensure_ascii=False))


if __name__ == '__main__':
    main()
