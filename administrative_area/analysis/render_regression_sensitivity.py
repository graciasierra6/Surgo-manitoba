"""Append aggregate-only regression sensitivity results to an administrative-area report."""
import argparse
import base64
import html
import json
from pathlib import Path
import re

ap = argparse.ArgumentParser()
ap.add_argument('--data-dir', required=True)
ap.add_argument('--html', required=True)
a = ap.parse_args()
root, report = Path(a.data_dir), Path(a.html)
p = json.loads((root / 'sensitivity_results.json').read_text())
checks = json.loads((root / 'verification.json').read_text())
assert checks['all_passed']
assert p['audit']['supply_resolution'] in ('district only', 'département only')
country = p['audit']['country']
unit = p['audit']['area_singular']
units = p['audit']['area_plural']
clusters = p['audit']['geographic_clusters']
controls = p['audit']['controls']
need_label = ('the approved instrument-derived FPN2 definition' if p['audit']['country_key'] == 'sindh' else 'original SURGO FPN2 = 1')
control_label = ', '.join({'age10':'age','age10_sq':'age squared','education':'education','partnered':'partnership','fertility_goal':'fertility goal','urban':'Rural/Urban residence'}[k] for k in controls)
esc = lambda v: html.escape(str(v), quote=True)
number = lambda v: f'{v:,}'
percent = lambda v: '—' if v is None else f'{v:.1f}%'
ratio = lambda r: f"{r['odds_ratio']:.3f} ({r['or_lo']:.3f}–{r['or_hi']:.3f})"
D = p['demand_variables']
labels = dict(zip(D, p['demand_labels']))
labels.update(supply_risk='Supply risk score', demand_score='Demand risk score',
              y_modern='Modern-use outcome', partnered='Partnership', age10='Age', age10_sq='Age squared', education='Education', fertility_goal='Fertility goal', urban='Rural/Urban residence')

def table(headings, rows):
    return ('<div class="table-wrap"><table class="planning"><thead><tr>' +
            ''.join('<th>' + esc(h) + '</th>' for h in headings) +
            '</tr></thead><tbody>' + ''.join('<tr><th scope="row">' + row[0] +
            '</th>' + ''.join('<td>' + cell + '</td>' for cell in row[1:]) + '</tr>'
            for row in rows) + '</tbody></table></div>')

def figure(name, alt):
    data = base64.b64encode((root / name).read_bytes()).decode()
    return f'<figure class="model-figure"><img src="data:image/svg+xml;base64,{data}" alt="{esc(alt)}"></figure>'

def download(name, label):
    data = base64.b64encode((root / name).read_bytes()).decode()
    return f'<p><a class="download" download="{esc(name)}" href="data:text/csv;base64,{data}">{esc(label)}</a></p>'

audit = p['audit']
w = p['weight_diagnostics']
overview = {r['group']: r for r in p['overview']}
included, excluded = overview['Included'], overview['Excluded']
need, common, base = audit['need_n'], audit['common_n'], audit['base_n']
s = ('<section id="regression-sensitivity"><span class="eyebrow">APPENDIX · REGRESSION ROBUSTNESS</span>'
     f'<h2>{esc(audit["appendix_title"])}</h2>'
     f'<p>These checks examine whether regression associations depend on requiring complete responses to every demand indicator. '
     f'They use women meeting {need_label}, the existing {unit}-wide supply assignments, fixed score references and the same adjustment covariates: '
     f'{esc(control_label)}.</p>')
s += (f'<div class="callout"><b>{number(common)} of {number(need)} women</b> enter the common-sample models. '
      f'<b>{number(audit["excluded_n"])} are excluded ({100*audit["excluded_n"]/need:.1f}%).</b> '
      f'Of those excluded, {number(base-common)} have the outcome, covariates and supply score needed for the broader comparison; '
      f'{number(need-base)} still lack an outcome, an adjustment variable or a complete assigned supply score.</div>')
if p['audit']['country_key'] == 'sindh':
    s += '<p>Rural/Urban residence is unavailable in the Sindh women’s data; partnership is constant in the modeled group. Neither enters its unchanged adjustment set. The corrected Q406 training measure is retained.</p>'
s += '<h3>1 · Who is excluded?</h3>'
s += table(['Measure', 'Included in main models', 'Excluded from main models'], [
    ['Women, n', number(included['n']), number(excluded['n'])],
    ['Known modern-use outcome, n', number(included['known_outcomes']), number(excluded['known_outcomes'])],
    ['Weighted modern use among known outcomes', percent(included['modern_use_pct']), percent(excluded['modern_use_pct'])],
    ['Weighted mean age, years', f"{included['mean_age']:.1f}", f"{excluded['mean_age']:.1f}"],
    ['Urban residence, weighted', percent(included['urban_pct']), percent(excluded['urban_pct'])],
    ['Partnered, weighted among known responses', percent(included['partnered_pct']), percent(excluded['partnered_pct'])],
])
s += ('<p>The difference in recorded modern use motivates checking selection; it does not, by itself, establish bias in adjusted associations.</p>')
missing = {r['variable']: r for r in p['missingness']}
if missing['supply_risk']['n_missing']:
    s += (f'<p>The assigned supply score is unavailable for {number(missing["supply_risk"]["n_missing"])} women in need. '
          'They remain outside regressions requiring that score. Response weighting examines demand-response completeness within the group with a known outcome, covariates and supply score; it does not recover missing geographic supply profiles.</p>')
s += table(['Response or variable', f'Unresolved n / {number(need)}', 'Weighted unresolved share'], [
    [esc(labels[k]), number(missing[k]['n_missing']), percent(missing[k]['weighted_missing_pct'])]
    for k in ['y_modern'] + [k for k in controls if missing[k]['n_missing'] > 0] + D + (['supply_risk'] if missing['supply_risk']['n_missing'] > 0 else [])])
s += ('<p>Missing-response counts overlap across variables. “Unresolved” retains the existing coding of unanswered, special or inconsistent responses; '
      'these checks do not turn unknown beliefs into a known absence of concern.</p>')
s += f'<details><summary>Exclusions by {unit}</summary>' + table(
    [unit.capitalize(), 'Women in need', 'Common sample', 'Excluded', 'Weighted excluded share', 'Broader eligible sample'],
    [[esc(r['area']), number(r['need_n']), number(r['common_n']), number(r['excluded_n']),
      percent(r['excluded_weighted_pct']), number(r['broader_base_n'])] for r in p['area_coverage']]) + '</details>'
category_rows = []
for variable in ['education', 'fertility_goal']:
    levels = sorted({r['level'] for r in p['categories'] if r['variable'] == variable})
    for level in levels:
        values = []
        for group in ['Included', 'Excluded']:
            rows = [r for r in p['categories'] if r['variable'] == variable and r['level'] == level and r['group'] == group]
            values.append(percent(rows[0]['weighted_pct']) if rows else '0.0%')
        category_rows.append([esc(('Education' if variable == 'education' else 'Fertility goal') + ' · ' + level)] + values)
s += '<details><summary>Education and fertility-goal distributions</summary>' + table(
    ['Characteristic', 'Included · weighted', 'Excluded · weighted'], category_rows) + '</details>'

co = {(r['model'], r['sample'], r['term']): r for r in p['available_coefficients']}
samples = {(r['model'], r['sample']): r for r in p['available_samples']}
s += ('<h3>2 · Identical models with broader eligible samples</h3>'
      '<p>The supply-only model requires its own outcome, covariates and supply score. Each of the six single-indicator models includes '
      '<b>one demand indicator + supply score + all covariates</b>. Other demand indicators are absent from these single-indicator models. '
      'Each formula is fitted first in the common sample and then in everyone with the responses that formula requires.</p>')
s += figure('available_case_comparison.svg', 'Common-sample and available-case odds ratios with administrative-area-clustered CR2 and Satterthwaite confidence intervals.')
available_rows = []
for model, term in [('supply', 'supply_risk')] + [('single_' + k, k) for k in D]:
    available_rows.append([esc('Supply only' if model == 'supply' else labels[term]),
        number(samples[model, 'common']['n']), ratio(co[model, 'common', term]),
        number(samples[model, 'available']['n']), ratio(co[model, 'available', term])])
s += table(['Reported coefficient', 'Common n', 'Common OR (95% CI)', 'Available n', 'Available OR (95% CI)'], available_rows)
before, after = co['supply', 'common', 'supply_risk'], co['supply', 'available', 'supply_risk']
largest = max(D, key=lambda k: abs(co['single_' + k, 'available', k]['odds_ratio'] / co['single_' + k, 'common', k]['odds_ratio'] - 1))
relative_change = 100 * (co['single_' + largest, 'available', largest]['odds_ratio'] / co['single_' + largest, 'common', largest]['odds_ratio'] - 1)
s += (f'<p>The supply-only odds ratio changes from {before["odds_ratio"]:.3f} to {after["odds_ratio"]:.3f}. '
      f'Among the single-demand-indicator fits, the largest relative change in the odds ratio is {relative_change:+.1f}% for {esc(labels[largest])}. '
      'These comparisons isolate sample selection within each formula. Comparing a single-indicator model with the six-indicator joint model would additionally change adjustment for other concerns. Similarity of estimates is not a formal equivalence test.</p>')
s += ('<p>Intervals use the same administrative-area-clustered CR2/Satterthwaite method as the main models. Demand-indicator odds ratios compare presence versus absence; '
      'the supply coefficient is per one original average-z point. These are exploratory comparisons, with pointwise intervals.</p>')
s += download('available_case_coefficients.csv', 'All available-case coefficients')
s += download('available_case_samples.csv', 'Model-specific sample sizes')

j = {(r['model'], r['weighting'], r['term']): r for r in p['joint_coefficients']}
s += ('<h3>3 · Joint models with response-adjusted weights</h3>'
      f'<p>Among the {number(base)} women with known outcomes, covariates and supply scores, a weighted logistic model predicts whether all demand responses are complete. '
      'Predictors are observed modern use, the adjustment covariates and administrative-area indicators. The resulting adjustment is applied to the '
      f'<b>{number(common)} complete women</b>: original SURGO weight divided by estimated probability of complete demand information. '
      'This changes the contribution of observed women; it does not fill in the missing answers.</p>'
      f'<p>{len(w["fully_observed_areas"])} {units} have complete demand information for every otherwise eligible woman; their response probability is set to 1. '
      f'The response model is fitted in the remaining {units}. No response weights are trimmed.</p>')
s += figure('response_weighting_comparison.svg', 'Joint-model odds ratios with original and response-adjusted weights; both use two-stage area-jackknife confidence intervals.')
joint_rows = []
for model, terms, model_label in [('score_joint', ['demand_score', 'supply_risk'], 'Scores'),
                                  ('components_joint', D + ['supply_risk'], 'Individual indicators')]:
    for term in terms:
        joint_rows.append([esc(model_label + ' · ' + labels[term]),
                           ratio(j[model, 'survey_only', term]), ratio(j[model, 'response_weighted', term])])
s += table(['Joint model · coefficient', 'SURGO weights · OR (95% CI)', 'SURGO + response weights · OR (95% CI)'], joint_rows)
db, da = j['score_joint', 'survey_only', 'demand_score'], j['score_joint', 'response_weighted', 'demand_score']
sb, sa = j['score_joint', 'survey_only', 'supply_risk'], j['score_joint', 'response_weighted', 'supply_risk']
s += (f'<p>The demand-score odds ratio is {db["odds_ratio"]:.3f} with survey weights and '
      f'{da["odds_ratio"]:.3f} after response adjustment; the supply-score odds ratio is {sb["odds_ratio"]:.3f} versus {sa["odds_ratio"]:.3f}. '
      'The table shows the corresponding intervals and the results when all six demand indicators enter jointly.</p>')
interval_changes = []
for term in D + ['supply_risk']:
    before = j['components_joint', 'survey_only', term]
    after = j['components_joint', 'response_weighted', term]
    before_excludes = before['or_hi'] < 1 or before['or_lo'] > 1
    after_excludes = after['or_hi'] < 1 or after['or_lo'] > 1
    if before_excludes != after_excludes:
        interval_changes.append(esc(labels[term]) + ': ' + ratio(before) + ' versus ' + ratio(after))
if interval_changes:
    s += ('<p><b>Pointwise interval changes in the six-indicator joint model:</b> ' +
          '; '.join(interval_changes) + '. The interval moves across 1 under response adjustment. '
          'This should be considered alongside the size of the estimate change, rather than interpreted as a categorical reversal of the underlying association.</p>')
s += (f'<p>The response-adjusted comparison uses a <b>delete-one-{unit} jackknife</b>: omit each of the {clusters} {units}, re-estimate response probabilities, '
      f'and refit the joint models. Both the survey-only and response-adjusted columns above use this same interval method, with {clusters-1} degrees of freedom. '
      'It accounts for geographic clustering and re-estimation of response weights. The primary models retain their existing CR2/Satterthwaite intervals. '
      'This geographic calculation does not identify the original survey sampling PSUs, and all intervals condition on the fixed score references.</p>')
s += '<details><summary>Response-weight diagnostics and observed balance</summary>'
s += table(['Diagnostic', 'Value'], [
    ['Lowest estimated completeness probability in the broader target', percent(100*w['response_probability_min'])],
    ['Median response-weight multiplier among included women', f"{w['response_adjustment_median']:.2f}×"],
    ['99th percentile of response-weight multiplier', f"{w['response_adjustment_p99']:.2f}×"],
    ['Largest response-weight multiplier', f"{w['response_adjustment_max']:.2f}×"],
    ['Kish effective n · original / adjusted weights', f"{w['kish_effective_n_original']:.0f} / {w['kish_effective_n_adjusted']:.0f}"],
    ['Largest absolute observed standardized difference · before / after adjustment',
     f"{w['maximum_absolute_balance_difference_before']:.3f} / {w['maximum_absolute_balance_difference_after']:.3f}"],
])
s += ('<p>Kish effective n describes weight concentration; it does not account for clustering or establish statistical power. '
      f'Balance compares observed modern use, age, {"residence, " if "urban" in controls else ""}partnership, supply score, education, fertility goal and administrative-area indicators against the broader eligible target.</p>')
s += download('response_weighting_balance.csv', 'Observed balance diagnostics') + '</details>'
s += download('response_weighted_joint_coefficients.csv', 'All response-weighted joint comparisons')
s += ('<h3>How to interpret these comparisons</h3>'
      f'<p>These checks examine sensitivity to eligibility restrictions and response adjustment in {esc(country)}. '
      'They do not establish equivalence or rule out selection related to unrecorded concerns. Response weighting depends on the completeness model being adequate. '
      f'Inference uses {clusters} geographic error clusters and fixed score references.</p>'
      '<p>Some unresolved responses may be substantive “don’t know” answers or incoherent selections. Response weighting treats classifiability as an observation process for this sensitivity; '
      'it does not establish that these responses are ordinary accidental nonresponse or reveal their unobserved demand profiles.</p>'
      '<p>Response weighting follows established missing-data methods; see '
      '<a href="https://pubmed.ncbi.nlm.nih.gov/21220355/">Seaman and White</a>. '
      'The geographic jackknife uses the centered delete-cluster replicate covariance; see '
      '<a href="https://arxiv.org/abs/2406.00650">MacKinnon, Nielsen and Webb on binary-response models</a>.</p>'
      f'<p>Independent verification passed {checks["check_count"]} checks of sample membership, weights, coefficients, response probabilities and refitted jackknife intervals.</p></section>')
page = report.read_text()
page = re.sub(r'<section id="regression-sensitivity">.*?</section>', '', page, flags=re.S)
page = re.sub(r'<a href="#regression-sensitivity">.*?</a>', '', page)
assert '</main>' in page and '</nav>' in page
page = page.replace('</main>', s + '</main>', 1)
page = page.replace('</nav>', '<a href="#regression-sensitivity">Appendix · Sensitivity checks</a></nav>', 1)
report.write_text(page)
print('Appended', country, 'administrative-area sensitivity checks; only aggregate statistics embedded.')
