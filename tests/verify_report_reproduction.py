"""Compare a clean report build with frozen current analytical results."""
import argparse
import json
import math
from pathlib import Path

ap = argparse.ArgumentParser()
ap.add_argument('--work-dir', required=True)
args = ap.parse_args()
root = Path(__file__).resolve().parents[1]
expected = json.loads((root/'tests/report_reference_values.json').read_text())
checks = 0
def compare(a, b, path):
    global checks
    if isinstance(a, dict):
        assert isinstance(b, dict) and set(a) <= set(b), path
        for k in a: compare(a[k], b[k], path+'.'+k)
    elif isinstance(a, list):
        assert isinstance(b, list) and len(a) == len(b), path
        for i, (x, y) in enumerate(zip(a, b)): compare(x, y, f'{path}[{i}]')
    elif isinstance(a, (int, float)) and not isinstance(a, bool):
        assert isinstance(b, (int, float)) and math.isclose(a, b, rel_tol=1e-7, abs_tol=1e-9), (path, a, b)
        checks += 1
    else:
        assert a == b, (path, a, b)
        checks += 1
for country, name, z in [('lagos','Lagos','models_urban_rural'),('sindh','Sindh','models_zscore'),
                          ('senegal','Senegal','models_zscore'),('cotedivoire','Cote_dIvoire','models_zscore')]:
    data = Path(args.work_dir)/country/(name+'_analysis_files')
    report = json.loads((data/'report_data.json').read_text())
    old = expected[country]
    compare(old['need_n'], report['audit']['need_n'], country+'.need_n')
    for field in ['bars','scores','thresholds']: compare(old[field], report[field], country+'.'+field)
    for method, directory in [('zscore',data/z),('pca',data/'pca_appendix/models')]:
        model = json.loads((directory/'model_results.json').read_text())
        for field, value in old['models'][method].items():
            actual = model['audit']['common_n'] if field == 'common_n' else model[field]
            compare(value, actual, country+'.'+method+'.'+field)
    print(country+': need, weighted bars, classifications, thresholds, all coefficients, tests, samples and predictions reproduced')
print(f'Passed {checks:,} saved-result comparisons.')
