# Project Overview

This project examines the relationship between perceived stress, sleep (duration and quality), and subjective wellbeing. The main objective is to analyze key associations and to test whether stress is a significant predictor of wellbeing.
The dataset was artificially generated using ChatGPT for practice purposes. The analysis, as well as parts of the structure and wording, were supported by ChatGPT.
The purpose of this project is to understand the workflow of a data analysis project and to gain experience using GitHub as a portfolio tool.

## Methodology
### Data Cleaning
- Checked for missing values
- Identified potential outliers using boxplots

No missing values were found. Some outliers were detected but remained within a plausible range and were therefore retained.

### Exploratory Analysis
- Descriptive statistics (overview of variables)
- Correlation analysis between key variables

### Regression Analysis
A multiple linear regression was conducted to examine predictors of wellbeing.
- **Dependent variable:** Wellbeing
- **Independent variables:** Stress, sleep duration, sleep quality

### Moderation Analysis
It was further tested whether the relationship between stress and wellbeing is moderated by:
- Gender
- Age
Interaction terms were included in the regression models.

## Results
### Correlations
- Strong negative relationship between stress and wellbeing
![Stress und Wohlbefinden](plots/stress_wellbeing.png)

- Moderate positive relationship between sleep quality and wellbeing
![Schlafqualität und Wohlbefinden](plots/sleep_quality_wellbeing.png)

- Weaker positive relationship between sleep duration and wellbeing

Visualizations support these relationships.

### Regression
- Stress is a significant negative predictor of wellbeing
- Sleep duration and sleep quality are significant positive predictors
- The model explains a substantial proportion of variance (R² ≈ 0.62)

### Moderation
- No significant moderation by gender
- No significant moderation by age

→ The relationship between stress and wellbeing appears stable across groups.

## Model Diagnostics
Model assumptions were evaluated using graphical diagnostics.
- Residuals showed no systematic patterns → linearity assumption met
- Q-Q plot indicated approximately normal distribution of residuals
- Only minor indications of heteroskedasticity
- No evidence of influential outliers

Overall, model assumptions were considered adequately satisfied.
![Diagnostik](plots/diagnostic_plots.png)

## Standardization
To improve comparability of effects, all continuous variables were standardized (z-transformation).
This transformation results in variables with:
- Mean = 0
- Standard deviation = 1
This allows for direct comparison of effect sizes:
- Stress (β = -0.63) → strongest effect
- Sleep quality (β = 0.34) → moderate effect
- Sleep duration (β = 0.30) → slightly smaller effect

## Conclusion
The results suggest that stress has a strong negative impact on subjective wellbeing, while sleep—both in terms of duration and quality—contributes positively.
These relationships remain stable across age and gender, indicating robust effects within the dataset.

## Limitations
- Simulated dataset
- No causal conclusions can be drawn
- No item-based scales (no reliability analysis)

## Future Directions
Future projects could include:
- Analysis of item-level data
- Scale construction and reliability analysis (e.g., Cronbach’s alpha)
- Factor analysis (EFA / CFA)

## Tools Used
- R
