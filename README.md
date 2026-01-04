# life-expectancy-happiness-analysis
Analyzing life expectancy and happiness data using R for statistical insights
# Life Expectancy and Happiness: Statistical Analysis of Global Health Disparities

## Project Overview

This project looks at global health patterns using 2015 data from the WHO Life Expectancy dataset and the World Happiness Report. I was interested in understanding why life expectancy varies so much across countries - ranging from 51 to 88 years - and what factors contribute to these differences.

Working as part of a three-person team, I focused on the hypothesis testing part of the analysis, examining whether health disparities between regions and country groups are statistically significant.

## What I Found

After analyzing data from 133 countries, several clear patterns emerged:

African countries face significant health challenges. The mean life expectancy in Africa is 62.54 years, which is 9.46 years lower than the global average of 72 years. This difference is statistically significant (p < .001) with a large effect size (Cohen's d = 1.36).

OECD membership is associated with better health outcomes. Countries belonging to the OECD have a mean life expectancy of 81.57 years compared to 69.33 years for non-member countries - a difference of 12.24 years (p < .001, Cohen's d = 1.78).

The gap between Western Europe and Africa is stark. Western Europe averages 82.18 years while Africa averages 62.54 years. This 19.64-year difference represents one of the largest health disparities in the data (p < .001, Cohen's d = 3.34).

Income distribution matters more than overall wealth. When looking at what predicts life expectancy, income composition of resources emerged as the strongest single predictor, explaining 84% of the variance on its own. This suggests that how resources are distributed across a population may be more important than a country's total GDP.

## My Role in the Project

I worked with two teammates on this analysis. My specific contributions included:

- Coming up with the three research questions about global health disparities
- Running the statistical tests in R (one-sample and two-sample t-tests)
- Creating the visualizations showing distribution plots, jitter plots, and boxplots
- Checking statistical assumptions like normality and variance
- Calculating Cohen's d to understand the practical significance of our findings
- Writing parts of the final report and interpreting what the results mean

## Methods

### Data Preparation

The original datasets were merged by country name. I filtered the life expectancy data to just 2015 to match the happiness data. After removing 20 countries that had too much missing information, the final dataset included 133 countries with complete data on all variables.

### Statistical Testing

I formulated three specific research questions and tested them using t-tests:

Question 1: Is Africa's mean life expectancy significantly different from the global mean?
- Used a one-sample t-test comparing African countries to μ = 72 years
- Checked normality using the Shapiro-Wilk test (p = .108, so the assumption was met)

Question 2: Do OECD and non-OECD countries differ in life expectancy?
- Used Welch's two-sample t-test to account for unequal variances
- Large sample sizes (n = 29 and n = 104) made the test robust

Question 3: How does Western Europe compare to Africa?
- Another Welch's two-sample t-test
- This comparison showed the most extreme difference in the data

For each test, I calculated Cohen's d to measure effect size, which tells us whether the differences are meaningful in real-world terms, not just statistically significant.

### Regression Analysis

The team also ran regression models to see which factors predict life expectancy when considered together. We started with eight predictors and used backward stepwise selection. The final model included four variables (Economy, Happiness Score, Income Composition, and Adult Mortality) and explained 92% of the variance in life expectancy.

## Tools and Technologies

- R for all statistical analysis
- tidyverse and ggplot2 for data manipulation and visualization
- Additional packages: magrittr, ggrepel, flextable, psych

## Visualizations

### Distribution of Life Expectancy
<img width="503" height="342" alt="Life Expectancy Distribution" src="https://github.com/user-attachments/assets/623c6125-3d3b-4dfe-bd88-3536057e28bf" />

### Question 1: Africa vs Global Mean
<img width="2400" height="1500" alt="q1_africa_vs_global" src="https://github.com/user-attachments/assets/44ef958d-1042-4614-be48-83f6c7e87114" />

African countries show significantly lower life expectancy compared to the global average.

### Question 2: OECD vs Non-OECD
<img width="2400" height="1500" alt="q2_oecd_vs_nonoecd" src="https://github.com/user-attachments/assets/a8a7f8f9-43a8-4e8d-a4de-01806ef89acb" />

OECD member countries demonstrate notably higher life expectancy.

### Question 3: Western Europe vs Africa
<img width="2400" height="1500" alt="q3_weurope_vs_africa" src="https://github.com/user-attachments/assets/dfd56f48-a724-4391-8e7c-dc372077cdb6" />


The regional comparison reveals nearly a 20-year gap in life expectancy.

## Data Sources

- WHO Global Health Observatory Life Expectancy data (2015): https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who
- World Happiness Report data (2015): https://github.com/PhilippeCodes/World-Happiness-Report-Data-Analysis

## Repository Structure
```
life-expectancy-happiness-analysis/
├── README.md
├── life_expectancy_analysis.R
├── Life Expectancy Data.csv
├── World Happiness Report 1.csv
├── life_expectancy_distribution.png
├── q1_africa_vs_global.png
├── q2_oecd_vs_nonoecd.png
└── q3_weurope_vs_africa.png
```

## Running the Analysis

If you want to reproduce this analysis:

1. Install the required R packages:
```R
install.packages(c("tidyverse", "ggplot2", "magrittr", 
                   "ggrepel", "gridExtra", "flextable", 
                   "psych", "broom"))
```
2. Download the repository and make sure the CSV files are in the same folder as the R script

3. Open life_expectancy_analysis.R in RStudio and run the code

The script is organized into sections, so you can run it all at once or step through each analysis separately.

## Summary of Results

| Test | Comparison | Difference | t-value | p-value | Cohen's d |
|------|-----------|------------|---------|---------|-----------|
| One-sample t-test | Africa vs Global Mean | -9.46 years | -8.62 | <.001 | 1.36 |
| Two-sample t-test | OECD vs Non-OECD | +12.24 years | 13.49 | <.001 | 1.78 |
| Two-sample t-test | Western Europe vs Africa | +19.64 years | 17.29 | <.001 | 3.34 |

## What This Means

These findings suggest potential policy directions for countries facing health disparities. Reducing adult mortality rates had a significant negative effect on life expectancy in the regression model (β = -0.024, p < .001), suggesting that investments in healthcare infrastructure could have a substantial impact. The importance of income composition over GDP alone indicates that how resources are distributed matters more than total national wealth.

## Limitations

There are some important limitations to keep in mind. This is cross-sectional data from just 2015, so we can't make claims about causation. There were also multicollinearity issues in the regression model - the economy variable actually flipped from a positive correlation when examined alone to a negative coefficient in the multiple regression, which is a classic sign of collinearity problems. Country-level data also hides inequality within countries, and we had to remove 20 countries due to missing data.

## References

Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Lawrence Erlbaum Associates.

Field, A. (2013). Discovering statistics using IBM SPSS statistics (4th ed.). SAGE Publications.

Marmot, M. (2005). Social determinants of health inequalities. The Lancet, 365(9464), 1099-1104.

World Health Organization. (2015). World Health Statistics 2015.
