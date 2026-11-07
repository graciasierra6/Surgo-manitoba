"""Append the current weighted individual/profile PCA and modern-use sensitivity."""
from pathlib import Path
import base64
import html
import json


def render_pca_appendix(directory, primary_model_dir):
    root = Path(directory)
    p = json.loads((root / 'pca_results.json').read_text())
    m = json.loads((root / 'models/model_results.json').read_text())
    primary = json.loads((Path(primary_model_dir) / 'model_results.json').read_text())
    a, ma = p['audit'], m['audit']
    assert ma['score_method'] == 'pca' and ma['common_n'] == primary['audit']['common_n'] == 1865
    assert ma['demand_resolution'] == 'individual' and ma['supply_resolution'] == 'urban_rural'
    assert ma['fallback_model_n'] == primary['audit']['fallback_model_n'] == 51
    esc = lambda v: html.escape(str(v), quote=True)
    fmtp = lambda v: '<0.001' if v < .001 else f'{v:.3f}'
    fmtor = lambda r: f'{r["odds_ratio"]:.2f} ({r["or_lo"]:.2f}–{r["or_hi"]:.2f})'
    labels = dict(m['labels'], offering='Short + long-acting offering', free='Free-product provision',
                  training='Trained-provider share', counselor='FP counselor availability',
                  larc_stock='Three-LARC stock breadth', popular_stock='Most-popular product in stock')
    co = {(r['model'], r['term']): r for r in m['coefficients']}
    tc = {(r['model'], r['block']): r for r in m['tests']}
    load = {(r['side'], r['variable'], r['PC']): r for r in p['loadings']}
    D = ['agency', 'stigma', 'health', 'fertility', 'confidence', 'opposes']
    S = ['offering', 'free', 'training', 'counselor', 'larc_stock', 'popular_stock']

    def figure(file, alt):
        data = base64.b64encode((root / file).read_bytes()).decode()
        return f'<figure class="model-figure"><img src="data:image/svg+xml;base64,{data}" alt="{esc(alt)}"></figure>'

    def download(file, label):
        data = base64.b64encode((root / file).read_bytes()).decode()
        return f'<a class="download" download="{Path(file).name}" href="data:text/csv;base64,{data}">{esc(label)} ↓</a>'

    def table(headers, rows):
        return '<div class="table-wrap"><table class="planning model-table"><thead><tr>' + ''.join(f'<th>{h}</th>' for h in headers) + '</tr></thead><tbody>' + ''.join('<tr><th scope="row">'+r[0]+'</th>'+''.join(f'<td>{c}</td>' for c in r[1:])+'</tr>' for r in rows) + '</tbody></table></div>'

    load_rows = []
    for side, variables in [('Demand', D), ('Supply', S)]:
        for k in variables:
            load_rows.append([side, esc(labels[k]), f'{load[side,k,1]["loading"]:+.3f}',
                              f'{load[side,k,2]["loading"]:+.3f}'])
    variance_rows = [[r['side'], f'PC{r["PC"]}', f'{r["variance_pct"]:.1f}%', f'{r["cumulative_pct"]:.1f}%',
                      f'{r["eigenvalue"]:.3f}', f'{r["permutation_95"]:.3f}'] for r in p['variance']]
    model_rows = [
        ['P1 · Individual demand PC1', 'Individual demand PC1 + covariates'],
        ['P2 · Local supply PC1', 'Risk-oriented supply PC1 from her LGA–rural/urban profile + covariates'],
        ['P3 · Both PCA scores', 'Individual demand PC1 + local supply PC1 + covariates'],
        ['P4 · Six separate concerns', 'Six individual demand indicators + covariates; identical to the primary indicator-only model'],
        ['P5 · Six concerns + supply PC1', 'Six individual demand indicators + local supply PC1 + covariates']]
    comparisons = []
    for k in ['demand_score', 'supply_risk']:
        comparisons.append([esc(labels[k]), fmtor(co['score_joint', k]), fmtp(tc['score_joint', 'Demand' if k=='demand_score' else 'Supply']['p'])])
    component_rows = []
    for k in D + ['supply_risk']:
        component_rows.append([esc(labels[k])] + [fmtor(co[mid,k]) if (mid,k) in co else 'Not included'
                                                   for mid in ['components_demand','supply','components_joint']])
    test_rows = [[title, block, str(int(tc[mid, block]['df_num'])), fmtp(tc[mid, block]['p']), fmtp(tc[mid, block]['holm_p'])]
                 for mid, title in [('score_joint','P3 · Both PC1 scores'),('components_joint','P5 · Six concerns + supply PC1')]
                 for block in ['Demand','Supply']]
    gap = {r['term']: r for r in m['gaps']}
    demand_stability = min(r['score_rank_correlation'] for r in p['stability'] if r['side']=='Demand')
    supply_stability = min(r['score_rank_correlation'] for r in p['stability'] if r['side']=='Supply')
    sample_rows = [[r['model'], f'{r["n"]:,}', str(r['events']), str(r['parameters']), f'{r["max_vif"]:.2f}'] for r in m['samples']]
    return f'''<section id="appendix-pca"><div class="section-heading"><div><span class="eyebrow">APPENDIX · PCA SENSITIVITY</span><h2>Alternative scores from the same individual demand and matched supply measures</h2></div><p>Weighted PCA · continuous scores · modern-use models</p></div>
<p class="intro">This adapts the earlier summary's separate, standardized demand and supply PCAs to the current individual-level analysis. It changes how the two model scores are constructed; the main z-score models, heatmaps, coverage and LGA classifications remain unchanged.</p>
<div class="rule-strip"><span><b>Demand:</b> each woman's own six binary indicators.</span><span><b>Supply:</b> her six assigned LGA–rural/urban facility measures.</span><span>No new demand/supply/both/neither categories are needed for these regressions.</span></div>
<p class="model-note"><strong>How the scores are constructed.</strong> Fit two separate weighted correlation PCAs, without using contraceptive use to choose the loadings. Center and standardize each input using its weighted construction sample, multiply the six standardized values by their first-component coefficients, and sum. Each woman receives her own demand PC1 and the supply PC1 corresponding to her matched facility profile. The favorable supply PC1 is multiplied by −1 for modeling so that larger values indicate weaker provision. All first-component coefficients have the expected sign in these fits.</p>
<p class="model-note"><strong>Construction samples and weights.</strong> Demand PCA uses {a['demand_PCA_n']:,} FPN2 = 1 women with complete demand responses. Supply PCA uses the assigned measures for all {a['supply_PCA_women_n']:,} women in need. SURGO weights are used on both sides. Supply PCA on the woman-level records is numerically identical to PCA on the {a['supply_profiles']} distinct profiles weighted by their combined woman weights. These are not facility sampling weights. The approved popular-product fallback for Ibeju-Lekki–Urban is retained and flagged; its other five measures remain Urban-specific.</p>
{figure('01_pca_loadings.svg', 'Signed first-component scoring coefficients for the six individual demand indicators and six matched supply measures, with explained variance shown separately.')}
<p class="model-key"><strong>What the scores emphasize.</strong> Demand PC1 emphasizes opposition to FP, low method confidence, stigma and health beliefs. Supply PC1 emphasizes counselor availability, free provision, provider training and method offering. Three-LARC stock breadth and most-popular-product stock contribute less to the shared supply pattern. These coefficients describe variation in the inputs; they do not measure which intervention would change modern use most.</p>
{figure('02_pca_variance.svg', 'Observed variance explained by each demand and supply principal component, alongside exploratory permutation eigenvalue benchmarks.')}
<p class="model-key"><strong>How much is retained.</strong> Demand PC1 captures <b>{a['demand_PC1_variance']:.1f}%</b> of variation across the six demand indicators; supply PC1 captures <b>{a['supply_PC1_variance']:.1f}%</b>. A single demand PC therefore summarizes only part of the information. PC2 adds 18.6% of demand variation. Keep the six-indicator models and the main equal-weight scores alongside this sensitivity.</p>
<details class="model-detail"><summary>PCA coefficients, dimensionality and stability checks</summary>
{table(['Side','Input','PC1 coefficient','PC2 coefficient'],load_rows)}
<p>Coefficients multiply standardized inputs. Their squared values sum to one within each component; their signs are oriented for interpretation. The all-positive first-component coefficients support the stated direction here. PCA weights maximize captured input variation rather than program relevance, and need not match the main equal weights.</p>
{table(['Side','Component','Variance','Cumulative','Eigenvalue','Permutation 95%'],variance_rows)}
<p>The permutation benchmarks are exploratory: demand items are independently permuted within LGAs; supply items are permuted across the 18 profiles. They are not complex-survey significance tests or a rule establishing that PC1 is a complete barrier measure. PC2 and the borderline demand PC3 benchmark indicate additional structure. No outcome-informed component selection was performed.</p>
<p>Leaving one whole LGA out and refitting both normalization and PCA gives a minimum full-versus-refit PC1 rank correlation of {demand_stability:.3f} for demand and {supply_stability:.3f} for supply. This checks construction stability; it does not provide sampling confidence intervals. The original LGA classifications are retained without PCA cutoffs.</p>
<div class="table-heading">{download('pca_loadings.csv','All PCA coefficients')}{download('pca_centers_scales.csv','Centers and scales')}{download('pca_variance.csv','Variance and permutation checks')}{download('pca_leave_one_lga_out.csv','PCA construction stability')}{download('pca_matched_supply_profiles.csv','Matched profiles and woman weights')}</div></details>
<div class="wide-break"></div><h3 id="pca-models">Modern-use models using the PCA scores</h3>
<p class="model-meta"><strong>Same model sample:</strong> 1,865 complete-case women with FPN2 = 1, including 943 modern users. All models adjust for age, age², education, partnership, fertility goal and urban/rural residence. Original SURGO weights · 17 LGA clusters · CR2/Satterthwaite inference · no random effects or interactions.</p>
{table(['Specification','Predictors in addition to covariates'],model_rows)}
<p class="model-note">P2 is repeated in the second coefficient panel for comparison. Both PCA scores enter as continuous values, per one original PC1 score unit. Their units differ from the average z-score units; odds-ratio magnitudes across the two scoring approaches are not directly comparable. The four-way LGA labels are not predictors.</p>
{figure('models/01_score_models.svg','Modern-use models with individual demand PC1 alone, matched supply PC1 alone and both PC1 scores, adjusted for the same covariates and women.')}
<p class="model-key"><strong>Key finding.</strong> With both scores included, higher individual demand PC1 is associated with lower modern-use odds: <b>{fmtor(co['score_joint','demand_score'])}</b>. The local supply PC1 estimate is <b>{fmtor(co['score_joint','supply_risk'])}</b>, with an interval spanning 1. This is consistent with the main analysis's direction of findings; it does not establish that supply is adequate or causally unimportant.</p>
{table(['Score in P3','Adjusted OR (95% CI)','Raw p'],comparisons)}
{figure('models/02_score_probabilities.svg','Weighted adjusted modern-use probability curves across individual demand PC1 and matched supply PC1, showing single-score and joint-score models.')}
<p class="model-key"><strong>Planning interpretation.</strong> The individual demand-pattern curve slopes downward; the estimated supply-pattern curve is nearly flat with substantial uncertainty at sparsely represented exposures. The result supports examining reported concerns alongside services, while retaining the component heatmap to identify service weaknesses that a single PC may downweight.</p>
{figure('models/03_component_models.svg','Modern-use coefficient models with six separate individual concerns and a matched PCA supply score, including the demand-only and supply-only comparisons.')}
<p class="model-key"><strong>Component-level finding.</strong> Retaining the six concerns separately gives results close to the main model: opposition to FP has an adjusted odds ratio of <b>{fmtor(co['components_joint','opposes'])}</b>, low method confidence <b>{fmtor(co['components_joint','confidence'])}</b>, and health/side-effect beliefs <b>{fmtor(co['components_joint','health'])}</b>. The demand-only component model is unchanged because its inputs and women are unchanged.</p>
{figure('models/04_component_probability_gaps.svg','Adjusted modern-use probability gaps for each individual concern from the six-indicator plus PCA supply model, with paired 95% intervals.')}
<p class="model-key"><strong>Probability differences.</strong> In P5, the adjusted difference is <b>{100*gap['opposes']['estimate']:.1f} percentage points</b> for opposition and <b>{100*gap['confidence']['estimate']:.1f} points</b> for low confidence. These are weighted standardized associations, not expected gains from removing a barrier.</p>
<details class="model-detail"><summary>Exact model results, tests and limitations</summary>
{table(['Predictor','P4 · Six concerns','P2 · Supply PC1','P5 · Both'],component_rows)}
{table(['Joint model','Block','Restrictions','Raw p','Holm p'],test_rows)}
<p>The tests assess whether a score coefficient, or all six individual demand coefficients, are zero conditional on other predictors. Holm correction is within the two block tests in each joint model. They do not test equal demand/supply importance or attribute nonuse to one side.</p>
{table(['Model','Women','Modern users','Parameters','Maximum VIF'],sample_rows)}
<p>All five models converged and had full-rank designs. Maximum VIF is {max(r['max_vif'] for r in m['samples']):.2f}; point estimates agree with the corresponding survey-weighted GLM calculation. Probability calculations pass direct prediction and numerical-gradient checks. LGA-clustered intervals condition on the estimated PCA scores and do not propagate PCA-construction uncertainty. Official survey EA/stratum identifiers remain unconfirmed; these are geographic cluster intervals, not a reconstruction of the full complex survey design. Supply inference remains limited by 17 LGAs and about 2.6 effective degrees of freedom for the supply coefficients.</p>
<p>The first PCs are exploratory compression, particularly with binary demand indicators and only 18 supply profiles. Neither PCA nor regression establishes a causal binding constraint. The main equal-weight analysis remains primary. Normalization/PCA references: <a href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/prcomp.html">R PCA documentation</a>; <a href="https://knowledge-for-policy.ec.europa.eu/composite-indicators/toolkit_en/navigation-page/10-step-guide_en/step-6-weighting_en">JRC composite-indicator weighting guidance</a>.</p>
<div class="table-heading">{download('models/model_coefficients.csv','PCA-model coefficients')}{download('models/model_block_tests.csv','PCA-model block tests')}{download('models/model_samples.csv','Model checks')}{download('models/score_predicted_probabilities.csv','PCA-score probability curves')}{download('models/individual_demand_probability_gaps.csv','Component probability gaps')}{download('models/leave_one_lga_out.csv','Model influence checks')}</div></details></section>'''
