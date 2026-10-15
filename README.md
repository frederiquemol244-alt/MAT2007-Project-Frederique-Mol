# Ramen Ratings Analysis

This project analyses a public dataset of instant-noodle reviews published by *The Ramen Rater* and hosted on Kaggle.

## Research question
**Which country produces the best-rated instant ramen, and is the top country's lead statistically meaningful?**

## Files in this repository
| File | What it is |
|------|------------|
| `README.md` | This file |
| `Ramen project code.r` | The analysis script (code) |
| `ramen-ratings.csv` | The dataset |
| `report/report.pdf`, `report/report.tex` | The short report and its LaTeX source |
| `ramen_top_countries.png`, `ramen_rating_vs_reviews.png`, `ramen_country_ranking.csv` | Output of the script |

## Dataset
- Source: [Kaggle: Ramen Ratings](https://www.kaggle.com/datasets/residentmario/ramen-ratings) (reviews by The Ramen Rater, theramenrater.com)
- File used: `ramen-ratings.csv` (2,580 reviews, ~155 KB; a copy is included in this repository)
- Columns: `Review #`, `Brand`, `Variety`, `Style`, `Country`, `Stars` (0–5), `Top Ten`

The raw CSV does **not** need manual cleaning. The script handles the following itself:
- Converts `Stars` to numbers and drops the 3 reviews marked `Unrated` (2,577 remain)
- Merges country-name variants: `Holland` → `Netherlands`, `Dubai` → `United Arab Emirates`, `United States` → `USA`, `Sarawak` → `Malaysia`

## Software dependencies
- [R](https://cran.r-project.org/) ≥ 4.0 (tested with R 4.6.1). Only base R is used, so no packages need to be installed.
- Optional: [VS Code](https://code.visualstudio.com/) with the *R* extension (REditorSupport) to run the script from the editor.

## How to run
1. Install R from <https://cran.r-project.org/>.
2. Download or clone this repository.
3. Open a terminal in the repository folder and run:

```bash
Rscript "Ramen project code.r"
```

Or open `Ramen project code.r` in VS Code and run it with the R extension.

**Condition:** `ramen-ratings.csv` must be in the working directory. In an interactive R session, check `getwd()` first and use `setwd()` if necessary. If the file is not found, the script stops with an error message that shows the folder it looked in.

The script takes about a second to run.

## What the script does
The script is built from small helper functions (loading, cleaning, statistics, printing, plotting). The main analysis at the bottom of the file calls them in this order:

1. Tests the helper functions on small hand-made examples (`stopifnot`), and stops if one of them gives a wrong answer
2. Loads and checks the CSV
3. Cleans the data (see above)
4. Computes the number of reviews, the mean rating, the standard deviation and the standard error for each country
5. Keeps only countries with at least 10 reviews (21 of 35 countries) and ranks them by mean rating
6. Compares the top country with the runner-up, with Japan, with the lowest-ranked country and with Myanmar (the widest error bar in the top 10): difference of the means ± combined standard error
7. Cross-checks the uncertainty of the overall mean with the subsampling method (10 subsamples)
8. Plots the top 10 countries with standard-error bars
9. Plots mean rating against number of reviews for all 35 countries (scatter plot)

The settings (minimum number of reviews, number of countries in the bar chart, significance threshold, number of subsamples) are at the top of the script.

## Output
- Console: the dataset summary, the ranked table of countries and the comparisons
- `ramen_country_ranking.csv`: the ranked table (rank, country, number of reviews, mean, standard deviation, standard error)
- `ramen_top_countries.png`: bar chart of the 10 best-rated countries with standard-error bars
- `ramen_rating_vs_reviews.png`: scatter plot of mean rating vs number of reviews (log scale) for every country, showing which ones the 10-review cutoff excludes

## Main result
| Rank | Country   | n   | Mean stars | SE   |
|------|-----------|-----|------------|------|
| 1    | Malaysia  | 158 | 4.16       | 0.06 |
| 2    | Singapore | 109 | 4.13       | 0.07 |
| 3    | Indonesia | 126 | 4.07       | 0.07 |

Malaysia ranks first, but its lead over Singapore (0.03 ± 0.10 stars, 0.3σ) is well within the uncertainty. The two countries are statistically tied. Malaysia is above Japan by 0.18 ± 0.08 stars (2.2σ) and above last-placed Canada by 1.91 ± 0.21 stars (9σ). The mean over all products is 3.65 ± 0.02 stars.

## Report
The short LaTeX report (title page plus 2 pages) is `report/report.pdf`, and its source is `report/report.tex`. It follows the course Overleaf template. To rebuild it, upload `report.tex`, `ramen_top_countries.png` and `ramen_rating_vs_reviews.png` to an Overleaf project made from the template, or run `pdflatex report.tex` inside `report/`. The logo on the title page (`figures/MSP-logo.jpg`) comes with the template and is left out if that file is not present.
