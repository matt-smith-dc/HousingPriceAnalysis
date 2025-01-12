# HousingPriceAnalysis
Analysis and visualizations of housing prices based on zip code-level data

Based on Zillow research data, in particular Zillow Home Value Index (ZHVI) - All Homes Time Series, smoothed, seasonally adjusted, at the zip code level, as well as FRED data.
https://www.zillow.com/research/data/

I am unaffiliated with Zillow. This script is not meant to be used, and has not been used by me, professionally.

The Zillow data was downloaded from the site above and saved locally. Other data was accessed through APIs.

To use this script, you will need a FRED API key. You will also need to update 'data.path' variable with the correct path. Downloaded data is saved to a subfolder created by the script, to limit repeat API calls in future runs.
