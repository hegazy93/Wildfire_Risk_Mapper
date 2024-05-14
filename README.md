# Wildfire Risk Mapper

## Project overview
Wildfires are recognised as an increasing risk to natural ecosystems, property, and human lives. The objective of this project was to develop an online risk mapping tool to assist West Yorkshire Fire and Rescue Service (WYFRS).

The tool was developed using a case study covering Marsden Moor in the Peak District by employing various topographic, meteorological, and geological factors that influence wildfire spread.

## Data and methods 

We defined risk as the potential severity of spread if a fire starts. Wildfire spread is influenced by several topographic, meteorological and landcover factors. For example, wildfires tend to travel faster under stronger wind, uphill or when the vegetation is dry. Our objective was to build a tool that can model the effects of those factors and visualise the overall spread risk over discrete time intervals.
The data layers were obtained from multiple sources and aggregated to a 25-meter grid for cell-based computation and analysis. Data used and their respective sources are as follows:

* **Rainfall volume:** Environment Agency Rainfall API (EA, 2024)
* **Wind speed and direction temperature and humidity:** MetOffice Datapoint API (MO, 2024)
* **Landcover:** UK Land Cover Map (CEH, 2021)
* **Peaty soil:** Peaty Soils Location (Natural England 2023)
* **LIDAR:** Composite DTM	EDINA LIDAR (Digimap, 2023)

We used R Shiny for the tool development and Leaflet.JS for interactive map visualisation. The tool includes two components:

1.	**Propagation risk map** to visualise the risk of fire propagation across the landscape. This was generated from a classic weighted linear combination of user-defined weights for each factor and landcover class. 
2.	**Wildfire spread map** to visualise the potential extent of fire when a user clicks on an ignition location, after having set wind direction and speed, and relative humidity. Cost Distance Analysis method was used to create an accumulated cost surface where costs represent the time required for the fire to cross the cell.

![Figure 1](https://github.com/hegazy93/wildfire_mapping/assets/16538975/0a2021ce-14be-41bb-a1f1-8940cc3dce15)

