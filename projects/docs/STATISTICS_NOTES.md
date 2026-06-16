Statistical Notes for Data Projects

This document outlines the methodological standards applied in this repository to ensure high-quality, reproducible data analysis.


Core Principles


1. Correlation vs. Causation

Correlation indicates a statistical association between variables, not a causal relationship. We must be cautious not to over-interpret associations, as many factors can lead to high correlation without any underlying causation (e.g., spurious correlations).


2. Statistical Integrity & P-Hacking

We are committed to avoiding "data dredging" (also known as p-hacking or data snooping). This means:

We do not search through the dataset repeatedly until we find a random, statistically significant result.

We do not fit multiple models post-hoc just to artificially force a p-value below 0.05.

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

Example 2 (Galton's Height Data): Fitting a linear model to predict a father's height using their son's height (lm(father ~ son)) works perfectly in R and is statistically valid, but interpreting it as the son's growth causing the father's height violates biological reality.



5. Confounders and Simpson's Paradox

A confounder (Z) is a third variable that is correlated with both the predictor (X) and the outcome (Y). If we fail to control for Z, we might observe a strong correlation between X and Y that is entirely misleading.
When a confounder completely reverses the direction of an association depending on whether you analyze the data as a whole or split it into subgroups, this is known as Simpson's Paradox.

The UC Berkeley Admission Case Study:

The Apparent Bias (X \rightarrow Y): When looking at the aggregate admissions data, men had a significantly higher acceptance rate than women, suggesting systemic gender discrimination.

The Confounder (Z): The major (academic department) to which students applied.

The Reality: Women applied in much larger numbers to highly competitive majors with very low acceptance rates (e.g., English). Men applied more frequently to less competitive majors with high acceptance rates (e.g., Engineering). When the data is stratified by major, the apparent gender bias disappears or even slightly reverses.



Zusammenfassung (German Summary)

Diese Richtlinien dienen als das methodische Fundament für alle datengestützten Analysen in diesem Repository. Durch den bewussten Verzicht auf "Data Dredging" (P-Hacking) und die Berücksichtigung von Ausreißern (z. B. durch den systematischen Einsatz der robusten Spearman-Rangkorrelation anstelle der klassischen Pearson-Korrelation bei unregelmäßigen Datenstrukturen) stellen wir sicher, dass gefundene Zusammenhänge wissenschaftlichen Standards entsprechen.

Zudem beachten wir strikt das Risiko der Kausalitätsumkehr (Reverse Causality) sowie das Auftreten von Störvariablen (Confounders). Letztere können das sogenannte Simpson-Paradoxon hervorrufen: Ein statistischer Zusammenhang (wie eine scheinbare Benachteiligung von Frauen bei den UC Berkeley Zulassungen) kann sich komplett ins Gegenteil verkehren oder verschwinden, sobald man die Daten nach der entscheidenden Störvariable (hier: dem gewählten Studienfach) aufteilt. Ohne die Berücksichtigung von Confoundern führen statistische Analysen oft zu fatalen Fehlinterpretationen.

Reference: Based on the HarvardX Data Science Course sections on "Correlation is not Causation", "Outliers", "Reversing Cause and Effect", and "Confounders".