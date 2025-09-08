# MSc-Thesis: Quantifying-the-effects-of-traffic-interventions-on-NO2-Levels-
Code for a data-driven framework to quantify how short-term traffic interventions change urban NO₂ in Cork City. Integrates NO₂, meteorology, traffic and calendar data with changepoint-derived features (segment mean NO₂). Compares Random Forest and LSTM; detects localized reductions and improves predictions.

Local traffic is a dominant urban source of NO₂, yet the real-world efficacy of short, local interventions is often under-quantified. This repo operationalizes a method to  detect, attribute, and predict intervention-driven shifts in NO₂—useful for policy and planning.
### Key Ideas:

1. Changepoint-aware features: Detect structural shifts in NO₂ and feed the segment mean NO₂ into predictors—capturing regime changes without sparse “event day” dummies. 

2. Rolling-window changepoints: A sliding two-week window improves localization of short-term shifts tied to events and weather transitions.

3. Dual modeling track:

      i. Random Forest (RF) benefits most from structural context (changepoint features).

      ii. LSTM excels at complex temporal dynamics and delayed rebounds after interventions.
