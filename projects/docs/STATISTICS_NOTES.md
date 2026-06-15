Statistical Notes for Data Projects

This document outlines the methodological standards applied in this repository to ensure high-quality, reproducible data analysis.


Core Principles


1. Correlation vs. Causation

Correlation indicates a statistical association between variables, not a causal relationship. We must be cautious not to over-interpret associations, as many factors can lead to high correlation without any underlying causation (e.g., spurious correlations).


2. Statistical Integrity & P-Hacking

We are committed to avoiding "data dredging" (also known as p-hacking or data snooping). This means:

We do not search through the dataset repeatedly until we find a random, statistically significant result.

We do not fit multiple models post-hoc just to artificially force a p-value below $0.05$.

All analyses are grounded in logical, domain-specific hypotheses (e.g., urban planning principles).


3. The Influence of Outliers

A single extreme data point (caused by typos, sensor errors, or rare extreme events) can artificially inflate correlation coefficients or mask a true underlying relationship.

Action: We visually inspect the data for outliers before drawing conclusions.

Solution: When strong outliers are suspected, we utilize robust statistical measures such as the Spearman rank correlation instead of the classic Pearson correlation.

Zusammenfassung (German Summary)

Diese Richtlinien dienen als methodisches Fundament für alle datengestützten Analysen in diesem Repository. Durch den bewussten Verzicht auf "Data Dredging" (P-Hacking) und die Berücksichtigung von Ausreißern – beispielsweise durch den systematischen Einsatz der robusten Spearman-Rangkorrelation anstelle der klassischen Pearson-Korrelation bei unregelmäßigen Datenstrukturen – stellen wir sicher, dass gefundene Zusammenhänge wissenschaftlichen Standards entsprechen und keine reinen Zufallsprodukte (Scheinkorrelationen) darstellen.

Reference: Based on the HarvardX Data Science Course sections on "Correlation is not Causation" and "Outliers".


4. Reversing Cause and Effect (Reverse Causality)

An association can be easily confounded with causation when the true direction of cause and effect is reversed. A statistical model can be mathematically flawless, yielding highly significant p-values and perfect estimates, yet the human interpretation remains entirely incorrect if the independent and dependent variables are swapped.

Example 1 (Tutoring & Homework): Studies showing that intensive tutoring or parental homework help correlates with poorer student performance often mistake the effect for the cause.Children receive regular help because they are underperforming, not vice versa.

[cite_start]Example 2 (Galton's Height Data): Fitting a linear model to predict a father's height using their son's height (lm(father ~ son)) works perfectly in R and is statistically valid, but interpreting it as the son's growth causing the father's height violates biological reality[cite: 23, 24, 25, 26].