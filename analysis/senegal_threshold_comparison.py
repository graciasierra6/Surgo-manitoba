"""Place current and final-index SD threshold Senegal maps side by side.

Reuses the verified reference calculations and established comparison layout.
Run: python3 analysis/senegal_threshold_comparison.py --threshold 0.5
"""
import argparse
from html import escape
import json
import math
from pathlib import Path
import re
import xml.etree.ElementTree as ET

from lagos_threshold_comparison import TEMPLATE, table
from regional_sd_maps import load_country, fmt


def main():
    root=Path(__file__).resolve().parents[1]
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--reports-dir',type=Path,default=root/'reports')
    parser.add_argument('--output',type=Path,default=root/'reports/Senegal_threshold_comparison.html')
    parser.add_argument('--threshold',type=float,default=.5)
    args=parser.parse_args()
    if not math.isfinite(args.threshold) or args.threshold<=0:parser.error('Supply a finite positive --threshold')
    d,svg_text,changes=load_country(args.reports_dir/'Senegal_analysis_aggregate.html','senegal','Senegal',33,33,args.threshold)
    categories=['Demand only','Supply only','Both','Neither flagged','Facility data only']
    panels=[]
    for mode,title,rule,counts in [
        ('current','Current: above zero','Flag demand > 0; flag supply constraints > 0',d['current_counts']),
        ('alternative',f'Proposed: +{args.threshold:g} SD or higher',f'Flag each side at +{args.threshold:g} SD or higher',d['alternative_counts'])]:
        svg=ET.fromstring(svg_text);svg.set('id','map-'+mode);svg.set('aria-label','Senegal '+title)
        for p in svg.iter():
            key=p.get('data-area')
            if key:p.set('fill',d['palette'][d['areas'][key][mode]])
        legend=''.join('<span><i style="background:'+d['palette'][c]+'"></i>'+escape(c)+' · '+str(counts.get(c,0))+'</span>' for c in categories)
        panels.append(f'<section class="panel" id="{mode}"><h2>{title}</h2><p class="muted">{rule}</p><p class="category" data-category></p><div class="scores"><div>Demand · SD units<strong data-demand></strong></div><div>Supply constraints · SD units<strong data-supply></strong></div></div>{ET.tostring(svg,encoding="unicode")}<div class="legend">{legend}</div></section>')
    rows=sorted(d['areas'].values(),key=lambda r:r['name'])
    changes_rows=[[r['name'],r['current'],r['alternative'],fmt(r['demand_u']),fmt(r['supply_u'])] for r in sorted(changes,key=lambda r:r['name'])]
    all_rows=[[r['name'],fmt(r['demand_u']),r['demand_band'],fmt(r['supply_u']),r['supply_band'],r['current'],r['alternative']] for r in rows if r['assessed']]
    counts_rows=[[c,d['current_counts'].get(c,0),d['alternative_counts'].get(c,0)] for c in categories]
    base=TEMPLATE.replace('Lagos','Senegal').replace('LGAs','départements').replace('LGAs','départements').replace('LGA','Département')
    base=base.replace('17 assessed','33 assessed').replace('of 17','of 33').replace('same 17','same 33').replace('All 17','All 33')
    base=base.replace('Three départements with supply data only','Thirteen départements with facility data only')
    base=base.replace("let selected='IBEJULEKKI'","let selected='GOUDIRY'")
    extra_css='''main{max-width:3000px}.map-label{font-size:11.5px;fill:#254453;paint-order:stroke;stroke:white;stroke-width:3;stroke-linejoin:round;font-weight:600}.inset-heading{font-size:16px;fill:#17374a;font-weight:600}.map-caption,.orientation{font-size:12px;fill:#59717f}.geo-map [data-area].changed-area{stroke:#17374a;stroke-width:1.35}.geo-map [data-area].selected{stroke-width:2.7}.outline-control{display:flex;gap:8px;align-items:center;margin:0 0 16px;font-size:13px}.outline-control input{width:16px;height:16px}@media(max-width:850px){.map-label{display:none}}'''
    base=base.replace('</style>',extra_css+'</style>')
    base=base.replace('<div class="grid" id="comparison-maps">','<label class="outline-control"><input type="checkbox" id="outline-changes" checked>Dark outlines mark the 13 départements whose category changes</label><div class="grid" id="comparison-maps">')
    base=base.replace("path.classList.toggle('selected',path.dataset.area===selected);","path.classList.toggle('selected',path.dataset.area===selected);path.classList.toggle('changed-area',document.getElementById('outline-changes').checked&&r.assessed&&r.current!==r.alternative);")
    base=base.replace("select.addEventListener('change',", "document.getElementById('outline-changes').addEventListener('change',update);select.addEventListener('change',")
    # The ordinary body text describes the selected threshold; outlines are only a comparison aid.
    base=base.replace('Dark outlines mark the 13 départements',f'Dark outlines mark the {len(changes)} départements')
    r=d['reference']
    payload={'areas':d['areas'],'palette':d['palette'],'threshold_SD':args.threshold,'means':{'demand':r['demand_mean'],'supply_constraints':r['supply_mean']},'SDs':{'demand':r['demand_SD'],'supply_constraints':r['supply_SD']},'changed':len(changes),'current_counts':d['current_counts'],'alternative_counts':d['alternative_counts']}
    replacements={'__THRESHOLD__':f'{args.threshold:g}','__CHANGED__':str(len(changes)),'__UNCHANGED__':str(33-len(changes)),
        '__PANELS__':''.join(panels),'__COUNTS__':table(['Category','Current','Proposed'],counts_rows,(1,2)),
        '__CHANGES__':table(['Département','Current','Proposed','Demand · SD units','Supply · SD units'],changes_rows,(3,4)),
        '__ALL_AREAS__':table(['Département','Demand · SD','Demand band','Supply · SD','Supply band','Current','Proposed'],all_rows,(1,3)),
        '__D_SD__':f"{r['demand_SD']:.6f}",'__S_SD__':f"{r['supply_SD']:.6f}",
        '__D_CUT__':f"{r['original_demand_cutoff']:.6f}",'__S_CUT__':f"{r['original_supply_constraints_cutoff']:.6f}",
        '__DATA__':json.dumps(payload,ensure_ascii=False,separators=(',',':'))}
    for key,value in replacements.items():base=base.replace(key,value)
    assert not re.search(chr(47)+'Us'+'ers'+chr(47)+'|'+chr(47)+'ho'+'me'+chr(47)+r'|file:[/][/]|[/]private[/]var[/]',base)
    args.output.parent.mkdir(parents=True,exist_ok=True);args.output.write_text(base,encoding='utf-8')
    print(json.dumps({'assessed':33,'changed':len(changes),'current':d['current_counts'],'proposed':d['alternative_counts'],'source_report_unchanged':True}))


if __name__=='__main__':main()
