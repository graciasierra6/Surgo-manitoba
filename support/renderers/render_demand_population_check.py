"""Present saved aggregate population-comparison results without changing analyses."""
import argparse
import base64
import csv
import html
import json
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument('--results-dir', required=True)
parser.add_argument('--output', required=True)
args = parser.parse_args()
root = Path(args.results_dir)
output = Path(args.output)
labels = {'both': 'Both', 'demand': 'Demand only', 'supply': 'Supply only',
          'neither': 'Neither flagged', 'unassessed': 'Unassessed'}
esc = lambda value: html.escape(str(value))


def tag(value):
    return f'<span class="tag {esc(value)}">{labels[value]}</span>'


def table(headers, rows):
    return '<div class="table-wrap"><table><thead><tr>' + ''.join(
        '<th>' + esc(h) + '</th>' for h in headers) + '</tr></thead><tbody>' + ''.join(
        '<tr>' + ''.join('<td>' + str(v) + '</td>' for v in row) + '</tr>'
        for row in rows) + '</tbody></table></div>'


summaries, sections = [], []
for country in ['lagos', 'senegal', 'cotedivoire', 'sindh']:
    folder = root / country
    if not (folder / 'summary.json').exists():
        continue
    meta = json.loads((folder / 'summary.json').read_text())
    assert meta['current_need_classification_reproduced'] and meta['no_main_report_changes']
    path = folder / 'classification_population_comparison.csv'
    rows = list(csv.DictReader(path.open()))
    changed = [r for r in rows if r['changed'] == 'TRUE']
    assert len(changed) == meta['changed']
    summaries.append([esc(meta['country']), str(meta['areas']), str(meta['changed']),
                      str(meta['unchanged'])])

    def display(selected):
        return [[esc(r['area'].title()), tag(r['all_eligible_category']),
                 tag(r['current_in_need_category']),
                 f'{float(r["all_eligible_demand_score"]):+.3f}',
                 f'{float(r["in_need_demand_score"]):+.3f}'] for r in selected]

    headers = ['Area', 'All eligible women', 'Current: women in need',
               'Demand score: all eligible', 'Demand score: in need']
    encoded = base64.b64encode(path.read_bytes()).decode()
    sections.append('<section><h2>' + esc(meta['country']) + '</h2><p><strong>' +
                    f'{meta["changed"]} of {meta["areas"]} classifications differ.</strong> '
                    'The supply flag is held fixed; these changes reflect the demand-population comparison.</p>' +
                    table(headers, display(changed)) +
                    '<details><summary>All geographic areas</summary>' +
                    table(headers, display(rows)) + '</details>' +
                    f'<p><a download="{country}_demand_population_comparison.csv" '
                    f'href="data:text/csv;base64,{encoded}">Download all area comparisons</a></p></section>')

css = '''*{box-sizing:border-box}body{margin:0;background:#f3f7f8;color:#213e4d;font:15px/1.65 Arial,sans-serif}main{max-width:1200px;margin:32px auto;padding:32px 36px;background:white;border:1px solid #dbe7ec;border-radius:10px}h1,h2{color:#145563;line-height:1.3}h1{font-size:30px}h2{font-size:22px;margin-top:30px}.notice{padding:16px 20px;background:#edf7f7;border-left:4px solid #0c7c88}.note{font-size:13px;color:#566f7b}.table-wrap{overflow:auto}table{width:100%;border-collapse:collapse;margin:18px 0}th,td{padding:12px;text-align:left;border-bottom:1px solid #dce6eb;font-size:13px}th{background:#edf4f6}tbody tr:nth-child(even){background:#f9fbfc}.tag{padding:4px 7px;border-radius:4px;white-space:nowrap;font-weight:bold}.both{color:#755398;background:#f0ebf6}.demand{color:#087b88;background:#eaf6f7}.supply{color:#a56224;background:#fff2e5}.neither{color:#586e7a;background:#edf1f4}.unassessed{color:#7e7353;background:#f5f1e7}details{margin:18px 0}summary{cursor:pointer;font-weight:bold}a{color:#087b88}@media(max-width:750px){main{margin:0;padding:22px 14px}table{min-width:790px}}'''
page = '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Demand population and geographic classification</title><style>' + css + '</style></head><body><main><h1>Does the demand population change geographic classification?</h1><div class="notice"><strong>The current main classifications already use women in need of family planning.</strong> This separate check compares them with a hypothetical classification based on all eligible women. No main report, dataset or classification is overwritten.</div><h2>Controlled comparison</h2><ol><li>Keep the same six demand-domain definitions, valid-answer rules, geographic reference areas and supply measures/flags.</li><li>Calculate weighted concern percentages within each area, first for all consenting women with positive final weights, then for the current need group.</li><li>For each population separately, standardize each component across the same geographic areas using its mean and sample SD, then average six z-scores.</li><li>Use demand score &gt; 0 for the demand flag. Combine it with the unchanged supply-readiness flag to obtain the four categories.</li></ol><p class="note">This changes the demand population and its reference distribution together. It is not a comparison of regression samples. Need-group definitions remain original SURGO FPN2 for Lagos, Senegal and Côte d’Ivoire, and the authorized derived need flag for Sindh. Weighted estimates use positive final weights and consent where required. Areas with unassessed supply remain unassessed; five such Côte d’Ivoire areas are included in its total of 82.</p>' + table(['Geography', 'Areas compared', 'Different categories', 'Unchanged'], summaries) + ''.join(sections) + '<p class="note">Scores are relative planning signals, not causal diagnoses. A change in category can occur near zero and does not establish that an intervention will change use. The need-only calculations independently reproduce the current saved classifications in all four geographies.</p></main></body></html>'
output.parent.mkdir(parents=True, exist_ok=True)
output.write_text(page)
print('Rendered the separate four-geography demand-population comparison.')
