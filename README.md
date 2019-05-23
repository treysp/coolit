# coolit

coolit is an R package for identify water cooling towers from high-resolution aerial imagery. It consists of functions for:

- Slicing large images into smaller pieces the model can use
- Scoring slices with a trained model

You can install coolit from Github using `remotes::install_github()`. 

# coolit model

coolit identifies images containing a water cooling tower with a convolutional neural network. 

Because we have relatively few tower images, it uses transfer learning instead of training a model from scratch. It uses VGG16 as a base model, trains dense layers on top of that base, then simultaneously fine-tunes the last VGG16 and dense layers.

The model training functions allow the user to specify the model architecture, but as of 2019-05-16 I am using one 256-node and one 128-node layer with dropout of 0.2 for each. 

As of 2019-05-16, the model has been trained on labeled data from New York City, Philadelphia, and Chicago.

# coolit data

coolit requires ortho-rectified aerial imagery at resolution 6 inches per pixel or better. Currently, publicly available satellite imagery does not go below about 1 foot per pixel so cannot be used.

Suitable imagery is available from multiple public sources - the ones we have found or used are described  below.

# Image sources

Some states, counties, and cities collect orthoimagery every few years. If those images are publicly available, they are typically hosted by a city or state GIS unit such as those described above for NYC, Philadelphia, and Chicago. We located those by searching Google to identify the relevant party. 

Unfortunately, many places have publicly available imagery that is not of sufficient resolution for tower identification. We have found some other potential image sources, described below.

## USGS EarthExplorer

[EarthExplorer](https://earthexplorer.usgs.gov/) is an aggregator of geospatial data from many sources, and it hosts imagery covering the entire United States from assorted times. 

To search for appropriate imagery: 

1. Specify the area of interest in the front 'Search Criteria' tab
2. In the 'Data Sets' tab select Aerial Imagery --> High Resolution Orthoimagery
3. In the 'Additional Criteria' tab and select resolutions of .5 Feet or smaller and .16 Meter or smaller
4. Click the 'Results' button at the bottom

The results are returned by image tile (typically 5000px square). You can select the ones you want, then download them after you register for an account. If you want many images, there is a separate 'Bulk Download' application you can use - information can be found at the [help page](https://lta.cr.usgs.gov/EEHelp/ee_help). I am in a restricted environment, and the necessary port is not open in our firewall for using the bulk download tool.

## NOAA 

Todo

  ------------

Please note that the 'coolit' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
