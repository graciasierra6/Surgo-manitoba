#!/usr/bin/env python3
"""Check the additional administrative-area reports without changing the base checks."""
from pathlib import Path
import hashlib
import json
import re
import subprocess
import sys

root = Path(__file__).resolve().parents[1]
subprocess.run([sys.executable, str(root / 'tests/check_release.py')],
               cwd=root, check=True)
assert (root / 'R/run_aggregate_reports.R').is_file()
assert (root / 'administrative_area/analysis/report_pipeline.R').is_file()
assert (root / 'administrative_area/analysis/plot_models.R').is_file()
manifest = json.loads((root / 'reports/aggregate_build_manifest.json').read_text())
reports = [('lagos', 'Lagos_analysis_aggregate'),
           ('senegal', 'Senegal_analysis_aggregate'),
           ('cotedivoire', 'Cote_dIvoire_analysis_aggregate')]
for country, name in reports:
    module = root / 'administrative_area/analysis' / country
    assert (module / (name + '.R')).is_file()
    assert (module / (name + '.Rmd')).is_file()
    source = (module / 'fit_models.R').read_text()
    assert 'source(file.path(script_dir,"..","plot_models.R"))' in source
    path = root / 'reports' / (name + '.html')
    text = path.read_text()
    assert text.lower().startswith('<!doctype html>')
    assert len(re.findall(r'data:image/svg\+xml;base64,', text)) == 10
    assert '<section id="appendix-facilities"' not in text
    assert '<section id="appendix-demand"' not in text
    assert 'id="supply-priority-summary"' in text
    assert 'id="demand-priority-summary"' in text
    refs = re.findall(r'(?:href|src)=["\']([^"\']+)', text)
    assert all(x.startswith(('#', 'data:', 'https://', 'http://', 'mailto:'))
               for x in refs), ('local asset dependency', name)
    record = manifest['reports'][country]
    assert record['sha256'] == hashlib.sha256(path.read_bytes()).hexdigest()
    assert record['numerical_checks_passed'] is True
    assert record['counseling_in_main'] is True
    assert record['complete_case_sensitivity_sections_in_report'] is False
print('Checked all three additional aggregate reports, independent source files and build hashes.')
