# ClimateConnectivity

This repo contains R code underpinning the analyses for our paper, submitted to Global Change Biology (2026):

> ### Climate connectivity of Australia’s marine protected areas is driven by their size, proximity, latitude, and the pace of climate change
>
> *Alice M. Pidd*<sup>*1*</sup>*, David S. Schoeman*<sup>*1,2*</sup>*, Anthony J. Richardson*<sup>*3,4*</sup>*, Eric Treml*<sup>*5*</sup>*, Kristine C. V. Buenafe*<sup>*3,4*</sup>*, Jason Everett*<sup>*3,4,6,7*</sup>*, Kylie L. Scales*<sup>*1*</sup>
>
> ##### <sup>1</sup> Ocean Futures Research Cluster, Global-Change Ecology Research Group, School of Science, Technology and Engineering, University of the Sunshine Coast.
>
> ##### <sup>2</sup> Centre for African Conservation Ecology, Department of Zoology, Nelson Mandela University, Gqeberha, South Africa
>
> ##### <sup>3</sup> Centre for Biodiversity and Conservation Science (CBCS), The University of Queensland, Brisbane, Queensland, Australia
>
> ##### <sup>4</sup> School of the Environment, The University of Queensland, Brisbane, Queensland, Australia
>
> ##### <sup>5</sup> Australian Institute of Marine Science (AIMS) and UWA Oceans Institute, The University of Western Australia, MO96, 35 Stirling Highway, Crawley, WA 6009
>
> ##### <sup>6</sup> Commonwealth Scientific and Industrial Research Organization (CSIRO) Environment, Queensland Biosciences Precinct (QBP), Queensland, Australia
>
> ##### <sup>7</sup> Centre for Marine Science and Innovation (CMSI), The University of New South Wales, Sydney, New South Wales, Australia

## Contents

```         
ClimateConnectivity
├── figures_tables      <--- .pdf files of figures and tables in the main text
├── masks               <--- .RDS files of masks used in computation and spatial plotting
├── helpers             <--- helper files used in computation and spatial plotting
└── supplementary       <--- supplementary materials for the manuscript
```

## Overview

Gradient-based thermal climate velocity trajectory computations, and workflow of the underlying Earth System Model (ESM) outputs used to compute climate velocity, are not included in this repo. Scripts included in this repo reflect the entire code base for computing network analysis metrics using `igraph` (Csárdi et al. 2025), and plotting outputs.

ESMs of sea surface temperature (SST) were obtained from publicly available data nodes via the Earth System Grid Federation MetaGrid (<https://esgf.nci.org.au/search>). Workflow for downloading, wrangling, and processing ESMs can be followed in the `hotrstuff` package and GitHub repo (Buenafe, Schoeman, & Everett 2024) at <https://github.com/SnBuenafe/hotrstuff>. Climate velocity computations followed the workflow found in the `VoCC` R package (Molinos et al. 2019) at <https://github.com/JorGarMol/VoCC>, reworked slightly for `terra`.

Network analysis is underpinned by graph theory, which describes a 'network' of anything using nodes (points) and edges (links between points), and metrics to provide insight into the overall connectivity of the broader network.

Here, we used network analysis metrics to describe the projected climate connectivity among MPAs under AR6 IPCC emissions scenarios. In this sense, MPAs represent nodes, and climate velocity trajectories represent edges linking MPAs through time, indicating pathways of analogous climate throughout the seascape. The connectivity of the overall MPA meta-network describes the climate connectivity. These metrics, and their graph level, used in this analysis include: - Total Strength (node-based metric) - Edge Strength (edge-based metric) - Betweenness Centrality (node-based metric) - Residence time (node-based metric, not a typical network analysis metric)

Background data and shapefiles included relate specifically to the case study region (here, continental Australia).

## Machine specifications

All analyses were run on a machine with the following specifications:

```         
Model Name:     MacBook Pro
Chip:           Apple M3 Max
Cores:          16 (12 performance and 4 efficiency)
Memory:         64 GB
OS:             Tahoe Version 26.3.1 (a) (25D771280a)
R version:      4.5.2 (2025-10-31) -- "[Not] Part in a Rumble"
GitHub:         Version 3.5.8 (arm64)
```

## Questions or feedback?

Please submit an issue, or email your questions to A.Pidd: alicempidd(at)gmail(dot)com

## References

Buenafe, K., Schoeman, D., & Everett, J. (2024). hotrstuff: Facilitate the rapid download, wrangling and processing of Earth System Model (ESM) outputs from the Coupled Model Intercomparison Project (CMIP). R package version 0.0.2. <https://github.com/SnBuenafe/hotrstuff>

Csárdi, G., Nepusz, T., Traag, V., Horvát, S., Zanini, F., Noom, D., Müller, K., Schoch, D., & Salmon, M. (2025). igraph: Network Analysis and Visualization in R. <doi:10.5281/zenodo.7682609>, R package version 2.2.1.9013. <https://CRAN.R-project.org/package=igraph>

Molinos, J. G., Schoeman, D. S., Brown, C. J., & Burrows, M. T. (2019). VoCC: An r package for calculating the velocity of climate change and related climatic metrics. Methods in Ecology and Evolution, 10(12), 2195–2202. <https://doi.org/10.1111/2041-210x.13295>
