# Global Income Distribution and Poverty Dynamics  
<!-- Use an H1 for the main title :contentReference[oaicite:0]{index=0} -->

## Overview  
<!-- H2 for main sections :contentReference[oaicite:1]{index=1} -->

This repository contains a comprehensive analysis exploring the relationship between income distribution patterns and poverty reduction effectiveness across countries. The research examines whether economic growth alone drives poverty reduction or if the distribution of that growth plays an equally important role in successful development strategies.  
<!-- Paragraph text separated from headings :contentReference[oaicite:2]{index=2} -->

Using statistical analysis in R, this project integrates six complementary global datasets spanning 1970–2024 to analyze how different inequality metrics correlate with poverty reduction outcomes. Our findings reveal that countries achieving both economic growth and more equitable distribution were most successful at poverty reduction, suggesting that development strategies should address both aspects simultaneously.  
<!-- Line breaks for readability :contentReference[oaicite:3]{index=3} -->

## Data Description  

The analysis integrates six complementary datasets:  
<!-- Bulleted list :contentReference[oaicite:4]{index=4} -->

- **Mean Income/Consumption Per Day:** Country-level average daily income values (2,705 observations)  
- **Median Income/Consumption Per Day:** Country-level median daily income values (2,705 observations)  
- **Poorest Decile Threshold:** Income threshold marking the poorest 10% by country (2,705 observations)  
- **Richest Decile Threshold:** Income threshold marking the richest 10% by country (2,705 observations)  
- **Number Below Poverty Lines:** Absolute counts of people below eight poverty thresholds (2,705 observations)  
- **Share Below Poverty Lines:** Percentage of population below poverty thresholds (1,468 observations)  
<!-- Bold syntax for emphasis :contentReference[oaicite:5]{index=5} -->

## Methodology  

Our analysis employs several statistical techniques implemented in R:  
<!-- H2 + bullet list :contentReference[oaicite:6]{index=6} :contentReference[oaicite:7]{index=7} -->

1. Calculation of inequality metrics (mean-to-median ratio, richest-to-poorest decile ratio)  
2. Time series analysis of poverty reduction trajectories  
3. Correlation analysis between income levels, inequality measures, and poverty rates  
4. Regression modeling to quantify relationships between income, inequality, and poverty  
5. Growth incidence analysis comparing different income segments across countries  
6. Case-study comparisons between successful and less successful countries  
<!-- Numbered list :contentReference[oaicite:8]{index=8} -->

## Key Findings  

### Primary Results
Our comprehensive analysis of global income distribution and poverty dynamics (1970-2024) reveals several critical insights:

**1. Economic Growth Alone is Insufficient**
- Higher mean income strongly predicts lower poverty rates (r = -0.481, p < 0.001)
- However, the effectiveness of economic growth is significantly moderated by income distribution patterns
- Countries with both growth and equity show the most successful poverty reduction

**2. Inequality Dramatically Affects Poverty Reduction**
- Significant interaction between income and inequality in our regression model (β = -0.983, p < 0.001)
- A 10% increase in mean income reduces poverty by 0.78 percentage points in countries with moderate inequality (richest-to-poorest ratio of 5)
- The same income increase reduces poverty by only 0.29 percentage points in highly unequal countries (ratio of 10)

**3. Regional Success Patterns**
- **East Asia**: Most dramatic poverty reduction (from ~90% to near 0%) while maintaining moderate inequality
- **Europe**: Consistently low poverty rates with stable, low inequality measures
- **Latin America**: High inequality persistence despite moderate progress
- **Sub-Saharan Africa**: Highest poverty rates with highest inequality levels

**4. Success Case Studies**
Countries achieving over 96% poverty reduction typically combined strong economic growth with stable/decreasing inequality:
- Malaysia: 100% reduction (2.68% → 0%)
- South Korea: 100% reduction (0.25% → 0%)
- Chile: 97.4% reduction (15.4% → 0.40%)
- Costa Rica: 96.6% reduction (25.9% → 0.88%)

**5. Statistical Model Performance**
- Our most comprehensive regression model explains 57.9% of variance in poverty rates
- Model includes interaction effects between income and inequality measures
- Gini coefficient shows strongest correlation with poverty (r = 0.531)

### Policy Implications
- Development strategies must address both economic growth and distributional patterns
- Regional differences require tailored approaches
- Monitoring both income levels and inequality measures is crucial
- Evidence-based policy decisions should consider the income-inequality-poverty nexus

**Data Source**: All analyses based on datasets from [Our World in Data](https://ourworldindata.org/poverty)


## Repository Structure 
```  
global-income-distribution-poverty-dynamics/
├── data/                     # Data files
│   ├── meanincomeorconsumptionperday.csv
│   ├── medianincomeorconsumptionperday.csv
│   ├── thepoorestdecile.csv
│   ├── therichestdecile.csv
│   ├── numberofpeoplelivingbelowarangeofpovertylines.csv
│   └── shareofpopulationlivingbelowarangeofpovertylines.csv
├── scripts/                  # R code files
│   └── poverty_analysis.R    # Main analysis script
├── output/                   # Generated figures and tables
├── docs/                     # Project report and documentation
├── LICENSE                   # MIT License
└── README.md                 # This file
``` 

## License

This project is licensed under the MIT License – see the [LICENSE](LICENSE) file for details.
