# ggforce
*Accelerating ggplot2*

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/ggforce.svg?branch=master)](https://travis-ci.org/thomasp85/ggforce)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thomasp85/ggforce?branch=master&svg=true)](https://ci.appveyor.com/project/thomasp85/ggforce)
[![CRAN_Release_Badge](http://www.r-pkg.org/badges/version-ago/ggforce)](https://CRAN.R-project.org/package=ggforce)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/ggforce)](https://CRAN.R-project.org/package=ggforce)


### About
`ggforce` is a package aimed at providing missing functionality to `ggplot2` 
through the extension system introduced with `ggplot2` v2.0.0. Broadly speaking 
`ggplot2` has been aimed primarily at explorative data visualization in order to 
investigate the data at hand, and less at providing utilities for composing 
custom plots a la [D3.js](https://d3js.org). `ggforce` is mainly an attempt to 
address these "shortcoming" (design choices might be a better description). The 
goal is to provide a repository of geoms, stats, etc. that are as well 
documented and implemented as the official ones found in `ggplot2`.

#### Disclaimer
The inclusion of any geom, stat, position etc in `ggforce` is not necessarily a 
recommendation of their use. `ggplot2` has been successful in being opinionated
about what functionality should be available. This is good as it insulates the
user from making bad decisions when analyzing their data (to a certain degree), 
but it also makes it difficult to develop novel visualizations using the 
`ggplot2` API. `ggforce` on the other hand positions itself closer to the 
"anything goes - the user is responsible for the quality of the output". Be very
aware of this responsibility! Bad visualizations lie about, distorts, and 
obscure the data behind them, both to you and the ones you share your 
visualizations with.

### Installation
`ggforce` is available on CRAN and can be installed in the regular way:

```r
install.packages('ggforce')
```

It is very possible that the GitHub repository contains new experimental 
features not released on CRAN yet. To get a taste of the future use `devtools`
to install the development version:

```r
if (!require(devtools)) {
    install.packages('devtools')
}
devtools::install_github('thomasp85/ggforce')
```

### Features
`ggforce` is by design a collection of features with the only commonality being
their tie to the `ggplot2` API. Because of this an overview of all features 
would get too long for a README. The package does contain a
[vignette](https://CRAN.R-project.org/package=ggforce/vignettes/Visual_Guide.html) 
where every feature is described and justified with examples and plots. There
should be a plot in the README of a visualization package though, so without 
further ado:

```r
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
    geom_point() +
    facet_zoom(x = Species == "versicolor")
```

![facet_zoom](https://www.dropbox.com/s/dpz91x0wp0tkp5t/facet_zoom.png?raw=1)

### Contributions
Pull and feature requests are very welcome. Obviously PR's will lead to faster
implementations than feature requests. I would like to urge requests to include
the following if possible:

#### Pull requests
If a PR is for a new feature, it should be self contained, possibly using 
already implemented functionality if applicable. All exported functions should
be documented following the style from ggplot2 using roxygen2 comment. You can
credit yourself with the implementation in the documentation. If the feature 
concerns a visualization approach invented by others, please link to the article
describing the approach.

#### Feature requests
If a feature is wished, but skill, time or other is lacking to create a full PR,
please file an issue. The feature request should provide a detailed description
of the nature of the feature, with links to relevant literature describing the
visualization type, as well as possible use cases to guide in designing the use
cases.

### Roadmap
For an overview of already requested features, enhancements, and bug fixes 
please consult the issues list. I will try to keep the labels up to date so it
should be easy to identify whether an issue is related to any of the above.

### Related projects
`ggforce` is by no means unique. The `ggplot2` ecosystem is flourishing 
following the release of v2.0.0. To help keep on top of all `ggplot2` extension
packages a web page has been created where developers can submit their packages.
Please explore <https://www.ggplot2-exts.org> to get an overview of the current
state of `ggplot2` extensions.
