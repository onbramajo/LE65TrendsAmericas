# Trends in life expectancy at age 65 in the Americas, 2000-2019

R code and data for:

> Bramajo O, Mehta NK, Myrskylä M (2025). Catching up with stagnation: Cause-specific dynamics of change in life expectancy at age 65 in the United States, Canada, and Latin America, 2000-2019. *The Gerontologist* 65(11). https://doi.org/10.1093/geront/gnaf214

This repository replaces the original OSF repository.

## What the analysis does

We compare life expectancy at age 65 (LE65) in the United States, Canada and seven Latin American countries (Argentina, Brazil, Chile, Colombia, Costa Rica, Mexico, Peru), by sex, for 2000, 2005, 2010, 2015 and 2019.

1. We take all-cause death rates by 5-year age group from UN World Population Prospects (WPP) life tables, and split them by cause using the cause-of-death distribution from the WHO Mortality Database (ICD-10).
2. We compute age-standardized death rates by cause for ages 65+.
3. We build abridged life tables (65-69 to 95+) and estimate LE65 for each country, plus a Latin American average with and without Mexico.
4. We decompose LE65 change in 2000-2010 and 2010-2019 by age and cause (Horiuchi line-integral method, `DemoDecomp`).
5. We measure dispersion between countries with the coefficient of variation, and fit segmented linear models of LE65 with a change in slope after 2010.

Cause groups (by first letter of the ICD-10 code): cardiovascular (I), cancer (C), metabolic (E), respiratory (J), nervous system (G), external (V-Y), and the rest. Ill-defined deaths (R) are redistributed proportionally across causes. Canada reports causes only up to age 85, so for ages 90+ we assume the cause shares of the 85-89 group.

## Files

### Code

| File | Content |
|---|---|
| `Aging in the Americas Part 1 Code OSF (Data Wrangling).R` | Reads WPP life tables, WPP population and WHO cause-of-death files. Builds deaths and cause shares by country, year, sex and age. |
| `Aging in the Americas Part 2 Code OSF 4825 (LE65 estimates and standardized rates).R` | Age-standardized cause-specific death rates, life tables and LE65, Latin American averages, coefficients of variation, figures. |
| `Aging in the Americas Part 3 Code OSF (Decomposition).R` | Age- and cause-specific decomposition of LE65 change, 2000-2010 and 2010-2019, and decomposition figures. |
| `Extra code Part 4 LE65 for revised version (Linear Models).R` | Segmented linear models of annual LE65 (2000-2019) by country and sex, with a slope change after 2010. Added during revision. |

### Data

| File | Content | Source |
|---|---|---|
| `LE65bySextxt.txt` | Abridged life tables, ages 65 to 100+, by sex, country and year, 2000-2019 (tab-separated) | UN WPP **[TO ADD: revision, e.g. 2022]** |
| `LE65bysexUN20002019.xlsx` | Same life tables in Excel format | UN WPP |
| `Excel Pop Americas.xlsx` | Population by sex and 5-year age group, in thousands | UN WPP |
| `mortality who database zip/Mortality WHO database zip.zip` | WHO Mortality Database, ICD-10 files `Morticd10_part1` to `part5` | WHO Mortality Database, downloaded March 2024 |

### Figures

| File | Content |
|---|---|
| `Plot1LEAmericas.png` | LE65 by country and sex (Figure 1) |
| `PlotBW1Americas.png` | Age-standardized death rates: cardiovascular, cancer, rest |
| `PlotBW2Americas.png` | Age-standardized death rates: respiratory, metabolic, nervous, external |

`Bibliographic References` is the reference list of the paper.

## How to run

1. Unzip `Mortality WHO database zip.zip` and put the five `Morticd10_part*` files in your working directory.
2. Set the working directory, and replace `route/` with your own path where `LE65bySextxt.txt` is read (part 1, line 10, and part 4, line 7).
3. Run parts 1, 2 and 3 **in this order, in the same R session**. Part 2 uses objects created in part 1 (`DeathData`, `ShareofMortjoin`, `AllPop`), and part 3 uses objects created in part 2 (the country-sex-year data frames such as `ARGMale2000`).
4. Part 4 runs on its own. It only needs `LE65bySextxt.txt` and the tidyverse.

## Software

R, with these packages:

`tidyverse`, `readr`, `readxl`, `DemoDecomp`, `gridExtra`, `viridis`, `ggrepel`, `forcats`, `RColorBrewer`

```r
install.packages(c("tidyverse", "readxl", "DemoDecomp", "gridExtra",
                   "viridis", "ggrepel", "RColorBrewer"))
```

## Contact

Octavio Bramajo, **[TO ADD: email]**

## License

GNU General Public License v3.0. See `LICENSE`.
