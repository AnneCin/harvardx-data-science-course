Dealing with Outliers and Robust Correlation

Outliers are data points that deviate significantly from other observations. They are one of the most common causes of distorted statistical analyses.

The Problem: Leverage of Outliers

In classic Pearson correlation ($r$), actual metric values are used directly in the calculation. A single point that lies extremely far from the rest of the data acts like a lever, pulling the regression line toward itself.

Spurious Correlation: Two completely uncorrelated variables can show a correlation close to $1.0$ due to a single extreme outlier.

Misinterpretation: Removing this single point often drops the correlation back to nearly $0$ instantly.

The Solution: Spearman Rank Correlation

Spearman rank correlation is a robust alternative that is highly resilient to outliers.

How It Works

Instead of calculating with raw values, the Spearman method converts all data points into ranks (positions from $1$ to $N$):

The smallest value gets rank 1.

The largest value gets rank $N$.

The correlation is then calculated using these ranks.

This completely eliminates the extreme mathematical leverage of outliers, since an outlier value of $1,000,000$ (with $100$ data points) is simply assigned rank $100$, placing it adjacent to rank $99$ instead of pulling the entire distribution.

Implementation in R

In R, Spearman correlation can be easily calculated by adjusting the method argument:

# Classic Pearson correlation (sensitive to outliers)
cor(x, y, method = "pearson")

# Robust Spearman rank correlation (resilient to outliers)
cor(x, y, method = "spearman")


Zusammenfassung (German Summary)

Ausreißer können statistische Zusammenhänge massiv verzerren und Scheinkorrelationen erzeugen. Während die klassische Pearson-Korrelation sehr anfällig für solche Extremwerte ist, bietet die Spearman-Rangkorrelation eine robuste Alternative. Indem sie echte Messwerte in Ränge (Platzierungen) übersetzt, nimmt sie Ausreißern ihre mathematische Hebelwirkung und sorgt für verlässliche Analyseergebnisse.

Reference: Based on the HarvardX Data Science Course section on "Outliers".