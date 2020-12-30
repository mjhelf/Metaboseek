[![Build Status](https://travis-ci.org/mjhelf/Metaboseek.svg?branch=master)](https://travis-ci.org/mjhelf/Metaboseek)


# Metaboseek

Metaboseek is here to help you analyze your mass spectrometry data!

Visit our website at https://metaboseek.com to test Metaboseek online

<a href = "https://metaboseek.com/doc/index.html">Installation and data analysis documentation</a>

Metaboseek is an interactive tool to analyze and browse your mass spectrometry data.
It is built on functions from the <a href = "https://github.com/sneumann/xcms">xcms</a> package, and provides a web-based graphical user interface built with <a href = "http://shiny.rstudio.com/">shiny</a>.

Metaboseek is currently under development, and new features are constantly added. Visit the [download page](http://metaboseek.com/download) to see the full change log.



## Some background

In Liquid Chromatography/Mass Spectrometry (LC/MS), so called mass spectra are acquired in rapid succession. Each spectrum can contain the mass-to-charge ratios (m/z) of thousands of charged molecules (ions) and their intensity (=relative abundance) at one time point. The molecules are separated through chromatography before entering the mass spectrometer, so that ions are separated in an additional dimension, the retention time (rt). This is particularly important to get separate signals for molecules with the same or very similar m/z value, but different structures (this happens a lot). 

The combination of m/z value and retention time can be defined as a `molecular feature`. Although these `molecular features` typically don't allow unambigous identification of specific compounds, their abundance can be compared between different biological samples. This can be very useful, because a molecular feature might be up- or downregulated in one sample group compared to another. For instance, some molecular features might only be found in a mutant strain of bacteria, but not in the wild-type control. In the approach to metabolomics presented here, we can select such molecular features of interest and then try to get more information about the molecular structure by tandem-MS, a method that breaks molecules into pieces and then reports the m/z of the resulting fragments.

## Features

So what can you do with this app? I would like to highlight some of the things it can do, and invite you to check out the example data that is loaded in the online version (and is also included in the Metaboseek R package). Documentation on how to use the app is available at https://metaboseek.com/doc.

Metaboseek offers a graphical user interface to set up data analysis with the `xcms` package to detect and align molecular features from LC/MS data across multiple samples. You can then load xcms results into the app as a "Feature Table" (using `xcms` and [`MSnbase`](https://bioconductor.org/packages/release/bioc/html/MSnbase.html) packages, [`mzR`](https://bioconductor.org/packages/release/bioc/html/mzR.html)-based) and run statistical analyses to identify molecular features of interest.

1. Filter the xcms results, view and export chromatograms and mass spectra for molecular features of interest.

![browse_2](https://github.com/mjhelf/Metaboseek/raw/master/vignettes/img/browse_2.gif)

2. Generate and view molecular networks based on tandem-MS spectrum similarity between molecular features (using [`MassTools`](https://github.com/mjhelf/MassTools) and [`igraph`](https://github.com/igraph/rigraph) packages). The interactive plot is based on re-rendering plot.igraph() with different parameters depending on selections.

![networking](https://github.com/mjhelf/Metaboseek/raw/master/vignettes/img/networking.gif) 

3. Annotate fragments in tandem-MS spectra with [SIRIUS](https://bio.informatik.uni-jena.de/software/sirius/). Metaboseek sends jobs to the SIRIUS command line interface and retrieves them from the SIRIUS output folder.

![SIRIUS](https://github.com/mjhelf/Metaboseek/raw/master/vignettes/img/SIRIUS.gif) 

## Technical Details

Metaboseek is strictly built from shiny modules, allowing for easy reuse and rearrangement of elements such as the spectral plots 

* Metaboseek is an R package that contains the app code along with some data analysis functions

* For improved code organization, Metaboseek is built from more than 70 different shiny modules, all of which are part of the Metaboseek package and documented. I am considering splitting grouping these modules into sub-packages for easier reuse by others.

## Try Metaboseek

Unfortunately, it was not possible to deploy the app on shinyapps.io. There seems to be a problem with installing [one of the required packages (`mzR`)](https://community.rstudio.com/t/http-599-time-out-error-while-deployapp-with-package-mzr/20644/16). However, there are other options to try the app:

* Here is the [direct link](http://mosaic.bti.cornell.edu/Metaboseek/) to the app (with example data pre-loaded), hosted on a shiny-server instance at Boyce Thompson Institute/Cornell University (on this server, it takes up to a minute to load; when installed locally, it starts within a few seconds)

* Take a look at the documentation [here](https://metaboseek.com/doc/) to see how to get Metaboseek as installer or .zip file for Windows (built using [R-portable](https://sourceforge.net/projects/rportable/)), as a [Docker image](https://hub.docker.com/r/mjhelf/metaboseek) or from [GitHub](https://github.com/mjhelf/Metaboseek).
