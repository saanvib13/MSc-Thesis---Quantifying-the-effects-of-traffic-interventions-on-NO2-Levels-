# MSc-Thesis: Quantifying-the-effects-of-traffic-interventions-on-NO2-Levels-
Code for a data-driven framework to quantify how short-term traffic interventions change urban NO₂ in Cork City. Integrates NO₂, meteorology, traffic and calendar data with changepoint-derived features (segment mean NO₂). Compares Random Forest and LSTM; detects localized reductions and improves predictions.

Local traffic is a dominant urban source of NO₂, yet the real-world efficacy of short, local interventions is often under-quantified. This repo operationalizes a method to  detect, attribute, and predict intervention-driven shifts in NO₂—useful for policy and planning.
### Key Ideas:

1. Changepoint-aware features: Detect structural shifts in NO₂ and feed the segment mean NO₂ into predictors—capturing regime changes without sparse “event day” dummies. 

2. Rolling-window changepoints: A sliding two-week window improves localization of short-term shifts tied to events and weather transitions.

3. Dual modeling techniques:

            i. Random Forest (RF) benefits most from structural context (changepoint features).

            ii. LSTM excels at complex temporal dynamics and delayed rebounds after interventions.


### Findings:
1. Short-term interventions produce detectable, localized reductions in NO₂, strongest near the intervention locations.

2. Adding changepoint-derived context consistently improves predictive accuracy.

3. Feature importance is location-specific (meteorology, dispersion, traffic mix, canyon effects).


### Data & coverage

1. NO₂: Low-cost Air Node sensors (2-min, resampled hourly) at St Patrick’s St, Oliver Plunkett St, Grand Parade Rd, Beasley St (Aug 2023–Jun/Jul 2024). EPA stations (UCC–Distillery Fields; Lower Glanmire Rd) provide hourly reference context (2020/2022–2024). 

2. Meteorology: Met Éireann (Cork Airport)—temperature, wind speed/direction, precipitation and more variables (hourly, TZ-aware).

3. Traffic: TII counters on 4 nearby roads; PCA condenses 4 car + 4 HGV series to 1 car + 1 HGV component.

4. Calendar: School terms + weekends/public holidays to capture activity cycles. 

Note: All timestamps use Europe/Dublin and handle DST correctly; 2-min sensor data are aggregated to hourly for comparability. Missing-data handling follows a daily threshold (remove if >25% missing), then linear interpolation + local smoothing + hourly medians as needed. 

### Methods Used

1. Changepoints: PELT (and Binary Segmentation/GA for sensitivity) with MBIC/BIC penalties; ARIMA residualization reduces autocorrelation issues; rolling two-week windows merged within ±1h tolerance. Outputs include regime IDs and segment mean NO₂ per timestamp.

2. Features: Meteorology (+ wind dir as sin/cos), PCA traffic (car/HGV), hour/day/DoY, school-term flag, and changepoint features.

3. Models:

          i. Random Forest (scikit-learn), permutation importance for interpretability.

         ii. LSTM (Keras/TensorFlow) with look-back windows (24/48/72h), Adam + MSE, early stopping.
4. Evaluation: Held-out windows around interventions (e.g., St Patrick’s Day, Open Streets, Car-Free Day), removing the day before/after from training to test generalization to unseen disruptions.
