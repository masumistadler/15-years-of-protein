# Environmental Impact of Dietary Protein: A 15-Year Cumulative Analysis

This repository contains the R code and methodology used to estimate the cumulative greenhouse gas (GHG) and water footprint of a 15-year dietary history. The project compares a Lacto-Ovo Vegetarian diet (mine) against various national baselines and flexitarian example models.

---

## Overview

Countries included were: Austria, Canada, Japan and U.S. for all the countries where I have lived parts of my adult life. To move beyond global average life cycle assessment (LCA) data, I incorporated country-specific **AWARE (Available Water Remaining)** factors to reflect the locally sourced nature of diary, a more geographically accurate representation of water stress associated with food production. However, I have left the globally average AWARE for other protein-sources as these proteins can be sourced globally.

## Key results

- **GHG Savings:** ~76,000 kg CO2eq (equivalent to 76 round-trip passenger flights between Tokyo and New York).
- **Water Savings:** ~87 million liters of scarcity-weighted water (equivalent to 35 Olympic-sized swimming pools).
- **Flexitarian Efficiency:** The model demonstrates that a 5-day meat-free diet captures nearly 90 percent of the environmental benefits of a full vegetarian diet.

## Data Sources

The analysis integrates several datasets:

- **Our World in Data (OWID):** Environmental footprints for various protein types ([GHG emissions](https://ourworldindata.org/grapher/ghg-per-protein-poore) and [water use](https://ourworldindata.org/grapher/scarcity-water-protein-poore)). Original data from Joseph Poore and Thomas Nemecek (2018).
- **FAOSTAT:** National food supply data for Austria, Japan, Canada, and the USA to establish omnivore baselines ([FAOSTAT Data](https://www.fao.org/faostat/en/#data)).
- **AWARE:** Regional water-stress characterization factors used to weight water consumption by local scarcity ([WULCA](https://wulca-waterlca.org/aware/download-aware-factors/)).
- **Beyond Meat LCA:** Manufacturing footprint data for plant-based meat alternatives ([Report](https://investors.beyondmeat.com/Beyond-Burger-IV-Life-Cycle-Assessment)).

## Methodology
The script `food.footprint_estimate.R` performs the following operations:

1. **Protein Normalization:** All diets are anchored to the CDC recommended minimum intake of 20.44 kg of protein per year to ensure a fair comparison across different food groups.
2. **Regional Weighting:** Dairy impacts are adjusted using country-specific AWARE factors (e.g., USA: 9.51 vs. Austria: 1.19) to reflect the reality of local sourcing for perishable goods.
3. **Counterfactual Modeling:** Creates 15-year projections for four dietary profiles: Omnivore, 2-day Flexitarian, 5-day Flexitarian, and Lacto-Ovo Vegetarian.
4. **Visualization:** Generates static ggplot2 visuals and interactive Plotly chart for longitudinal impact analysis.


---

## *Share, adapt, attribute*

<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons Licence" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Attribution-NonCommercial 4.0 International License</a>.
