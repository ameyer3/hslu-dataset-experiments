# Datasets

This repository consists of several scrips that explore, cleanup and visualize the microplastics and the currents datasets.

When working with the Microplastics dataset use the cleaned up version in the `microplastics/` directory called `Microplastics_Cleaned`. This file has the correct coordinates and some unnecessary columns removed. It might make sense to do additionally cleanup to remove more unnecessary columns or rename some of the current columns.

When working with the currents dataset, there are several versions of cleaned up currents. I would recommend using the `currrents/currents_by_bins.csv` dataset. The data was clustered by location into 500 bins. There are these columns that might help in several cases: `bin_id,lon,lat,speed_sum,speed_avg,ve_avg,vn_avg,buoy_count,measurement_count`.

To work with the whole dataset I would recommend downloading the following file (it has the newest data): [buoydata_15001_current.dat.gz](https://www.aoml.noaa.gov/phod/gdp/interpolated/data/all.php)

## Structure

There are several scripts in this repository. In the `currents` and in the `microplastics` directory are scripts that cleanup the respective datasets.

In the `visualizations` directory are scripts that plot the data.

The `correlate.r` script is a script that matches the currents and the microplastics script on the latitude and longitude and then tries to find a correlation between the Measurments and the currents. This definitely needs to be updated and I would also not recommend even executing it as it uses the whole currents dataset and takes way to long. It as a good reference to link the datasets.

## Other

Use the density class in the microplastics and not the measurements. These are in different units and are not comparable.

# Cleanup

The first step was to cleanup the datasets. This was done by exploring the data itself and the summary of the single columns and also by visualizing.
The microplastics dataset could always be double checked when visualised, as there is a visualization on the actual website.

# Linking

There are two different scripts to link the two datasets.

1. link_by_average_current: tries to take microplastics set, then gets all its nearest neighbors and takes its average values (Note: not sure it works)
2. link_closest_microplastic: takes the currents dataset and all the nearest neighbors of the microplastics get microplastic information, these columns on the other rows are empty (Works good for visualisation -> plot/plot_ocean_measurements.r)

With these we can visualise the correlations between the sets: The amount of measurements correlates with the amount of microplastic.

# NEXT STEP

PLEASE CONTINUE HERE DESCRIBING WHAT YOU DID/YOUR STEPS (will help for documentation)
