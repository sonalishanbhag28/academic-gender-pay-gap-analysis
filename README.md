# Gender Discrimination Analysis in Academic Salaries 👩‍🏫

## Overview
This project performs a comprehensive **statistical analysis of gender discrimination in academic salaries** using regression models and machine learning. The analysis is based on lawsuit data from **261 university faculty members**, revealing a persistent and significant gender pay gap.

---

## Key Findings 💸
The analysis provides robust statistical evidence of a gender-based pay gap that is not explained by differences in qualifications or experience.

### The Gender Pay Gap
* **Significant Difference**: T-tests show a highly significant salary difference between genders. In 1994, the average male salary was **$58,467 higher** than the average female salary, a gap that grew to **$64,037 by 1995**.
* **Persists Across Ranks**: The gender pay gap exists across all academic ranks (Assistant, Associate, and Full Professor), and across all departments.

### The "Glass Ceiling" Effect 📊
* **Promotion Disparity**: Logistic regression analysis reveals that **females are 58.3% less likely to achieve the rank of Full Professor**.
* **Unequal Rank Distribution**: A striking disparity exists in academic rank distribution. **Only 15% of female faculty reach Full Professor**, compared to **45% of male faculty**.

### Statistical & Machine Learning Validation ✅
* **Linear Regression**: The regression model has a **91.2% model fit**, confirming that gender is a statistically significant predictor of salary.
* **Random Forest**: Machine learning models independently validate these findings, ranking **gender as a significant predictor** for both salary and promotion outcomes.

---

## Methodology 🔬
This analysis uses a multi-layered approach to ensure robust and reliable findings:
1.  **Exploratory Data Analysis**: Visualizations to explore salary distributions by gender, rank, and department.
2.  **Hypothesis Testing**: T-tests and chi-square tests to confirm statistical significance.
3.  **Regression Analysis**: Linear and logistic regression models to quantify the impact of gender on salary and promotion.
4.  **Machine Learning**: Random Forest models to validate findings and determine variable importance.

---

## How to Use 💻
This project is fully reproducible. To run the analysis, you can simply clone the repository and execute the main R script.

1.  **Install Required Packages**:
    ```R
    install.packages(c("data.table", "ggplot2", "car", "dplyr", "tidyr", "scales", "randomForest"))
    ```

2.  **Run the Analysis**:
    ```R
    source("gender_analysis.R")
    ```

---

## Conclusion
This project provides strong, data-driven evidence of gender discrimination in academic salaries and promotion. The consistent results from multiple analytical methods offer a robust foundation for informing policy decisions and legal efforts to address gender inequality in academia.
