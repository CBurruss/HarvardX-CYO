# Predicting Violent Incidents in New Orleans

## Overview

This repository contains an R-based analysis pipeline that explores the relationship between urban disorder indicators and violent incidents in New Orleans neighborhoods. The analysis uses geospatial data from multiple public datasets to identify correlations and build predictive models that help understand how factors such as code violations, streetlight outages, abandoned vehicles, and lot abatements may predict neighborhood violence.

## Key Features

- Automated acquisition and processing of multiple open data sources from the City of New Orleans
- Geospatial analysis to identify spatial patterns of violent incidents
- Statistical analysis of correlations between urban disorder indicators and violent incidents
- Machine learning models to predict violent incidents based on urban disorder indicators
- Comparative evaluation of model performance
- Data visualization of spatial patterns and statistical relationships

## Data Sources

The analysis uses the following datasets from the City of New Orleans open data portal:

1. **Calls for Service (2023)**: Police response data including incident type, location, and time.
2. **Code Violations**: Records of property maintenance and zoning violations.
3. **311 Calls**: Citizen reports of issues including streetlight outages and abandoned vehicles.
4. **Chapter 66 Lot Abatements**: Records of city-initiated lot clean-ups for properties in violation.
5. **Neighborhood Statistical Areas**: Geographic boundaries defining New Orleans neighborhoods.

Data will be automatically downloaded during the first run and cached for subsequent runs.

## Analysis Pipeline

### 1. Environment Setup
- Installation of required packages
- Configuration of directory structure
- Resolution of function conflicts (e.g., dplyr::filter vs stats::filter)

### 2. Data Acquisition and Processing
- Automated download of datasets from New Orleans open data portal
- Geocoding of addresses where coordinates are missing
- Spatial data transformations and validations
- Identification of violent incidents based on call types

### 3. Spatial Analysis
- Mapping of neighborhoods and incident locations
- Spatial joins to aggregate data by neighborhood
- Visualization of spatial patterns

### 4. Statistical Analysis
- Correlation analysis between disorder indicators and violent incidents
- Testing of statistical significance
- Visualization of relationships

### 5. Predictive Modeling
- Data preparation and feature selection
- Training and tuning of multiple model types:
  - Linear Regression
  - Random Forest
  - Elastic Net
  - Multivariate Adaptive Regression Splines (MARS)
- Model evaluation and comparison
- Feature importance analysis

### 6. Results Interpretation
- Comparative analysis of model performance
- Identification of key predictors
- Implications for urban policy and public safety

## Key Findings

The analysis revealed:

1. Strong correlations between urban disorder indicators (particularly code violations and streetlight outages) and violent incidents in New Orleans neighborhoods.

2. Random Forest models achieved the best predictive performance (R² ≈ 0.76), suggesting non-linear relationships between the predictors and violent incidents.

3. Spatial clustering of both disorder indicators and violent incidents in specific neighborhoods, suggesting that targeted interventions could be effective.

4. Infrastructure maintenance (particularly streetlight repairs) may be a worthwhile focus for violence reduction strategies based on the predictive strength of this indicator.

## Limitations

- The analysis uses correlation methods which cannot establish causation
- Data quality depends on reporting rates which may vary by neighborhood
- Temporal dynamics are not fully captured in this cross-sectional analysis
- Socioeconomic factors not included in the current models may influence results

## Future Work

Potential extensions of this work include:

- Longitudinal analysis examining changes over time
- Incorporation of socioeconomic variables from Census data
- Analysis at more granular geographic levels (census blocks)
- Integration of additional urban disorder indicators
- Deployment of predictive models in operational contexts

## Contact

For questions or collaboration opportunities, please contact [Connor](cburru2@gmail.com).
