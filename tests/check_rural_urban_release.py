#!/usr/bin/env python3
"""Check the updated residence reports, their source modules and portable assets."""
from pathlib import Path
from html.parser import HTMLParser
import base64
import hashlib
import json
import re
import subprocess
import sys

root = Path(__file__).resolve().parents[1]
subprocess.run([sys.executable, str(root / 'tests/check_aggregate_release.py')],
               cwd=root, check=True)
manifest = json.loads((root/'reports/rural_urban_build_manifest.json').read_text())
assert manifest['workflow'] == 'updated_residence_specific_analysis'
assert set(manifest['reports']) == {'lagos','senegal','cotedivoire'}
assert manifest['validation']['fresh_bundle_rebuild_for_this_release'] is True
assert manifest['validation']['all_three_reports_identical_to_saved_versions'] is True

class Inspect(HTMLParser):
    def __init__(self):
        super().__init__(); self.ids=[]; self.anchors=[]; self.images=[]; self.downloads=[]
    def handle_starttag(self, tag, attrs):
        a=dict(attrs)
        if 'id' in a: self.ids.append(a['id'])
        if tag=='a' and a.get('href','').startswith('#'): self.anchors.append(a['href'][1:])
        if tag=='a' and 'download' in a: self.downloads.append(a.get('href',''))
        if tag=='img': self.images.append(a.get('src',''))

for country, record in manifest['reports'].items():
    module=root/'urban_rural/analysis'/country
    entry=module/record['entry_script']; rmd=entry.with_suffix('.Rmd')
    for name in ['README.md','assign_residence_supply.R','fit_models.R',
                 'build_pca_scores.R','render_analysis.py','verify_analysis.R','verify_html.py']:
        assert (module/name).is_file(), (country,name)
    assert entry.is_file() and rmd.is_file()
    source=entry.read_text()
    assert '--input-zip' in source and 'SURGO_MANITOBA_INPUT_ZIP' in source
    assert 'rmarkdown::render' in source and 'prepare_linkage' in source
    for relative, expected in record['source_files'].items():
        assert hashlib.sha256((root/relative).read_bytes()).hexdigest()==expected, relative
    report=root/'reports'/record['file']; html=report.read_text()
    assert hashlib.sha256(report.read_bytes()).hexdigest()==record['sha256']
    assert html.lower().startswith('<!doctype html>')
    parsed=Inspect(); parsed.feed(html)
    assert len(parsed.ids)==len(set(parsed.ids)), ('duplicate IDs',country)
    assert set(parsed.anchors).issubset(parsed.ids), ('internal links',country)
    assert len(parsed.images)==10, ('figure count',country)
    for uri in parsed.images:
        assert uri.startswith('data:image/svg+xml;base64,')
        assert '<svg' in base64.b64decode(uri.split(',',1)[1]).decode()
    for uri in parsed.downloads:
        assert uri.startswith('data:text/csv;base64,')
        assert len(base64.b64decode(uri.split(',',1)[1]))>0
    assert not re.search(r'<(?:script|link|img)[^>]+(?:src|href)=["\']https?://',html,re.I)
    assert any('colspan="6" class="'+style+'"' in html for style in ["supply-group","supply-head"])
    assert record['numerical_checks_passed'] is True
    assert record['fp_providers_only'] is True and record['health_PB2A_only'] is True
    assert record['primary_demand_cutoff']==record['primary_readiness_cutoff']==0
    assert record['survey_weights_used'] is True and record['random_effects'] is False
    assert record['need_n']=={'lagos':2159,'senegal':1905,'cotedivoire':4292}[country]

checks=(root/'reports/Data_checks.html').read_text()
for identity in ['senegal-residence-fallbacks','cdi-residence-fallbacks',
                 'cdi-fallback-need-summary','cdi-unmatched-need-residence']:
    assert checks.count('id="'+identity+'"')==1,identity
assert (root/'reports/Demand_population_classification_check.html').is_file()
assert (root/'analysis/check_demand_population.R').is_file()
print('Checked all three updated residence reports, source modules, coverage tables and offline assets.')
