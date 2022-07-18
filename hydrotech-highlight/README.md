# A pipeline for creating a prototype visualization highlighting USGS hydrotechs

This pipeline uses publicly available data to generate 3 different figures exploring the relationship between USGS streamgages and the USGS hydrologic technicians who visit them. Running `targets::tar_make()` will build the full pipeline, including creating 3 distinct figures and a single HTML file with all three figures in one. Prior to being able to succesfully run the full pipeline, you first need an API key from the US Census Bureau to download the population data. See the documentation for `pull_population_data()` in `fxns.R`.

The visualization creates the following three figures and stores them in `out/`:

![bff_bars](https://user-images.githubusercontent.com/13220910/178821686-1516eb14-eb23-482b-85c0-2892d0712f69.png)

![travel_violins](https://user-images.githubusercontent.com/13220910/178821869-b8422e34-8338-46c1-889b-1569858cec58.png)

![visit_clock](https://user-images.githubusercontent.com/13220910/178821969-507f4160-c048-45b3-ab76-afdebee50778.png)

A mock-up visualization of what a final, combined version of these figures into a single "data-viz" was made in PowerPoint:

![prototype](https://user-images.githubusercontent.com/13220910/178822733-f8a381b3-ddc0-44c6-bc40-93bc230a5597.png)
