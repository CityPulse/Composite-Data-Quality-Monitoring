# Composite Monitoring

The main challenge in evaluating the correctness and information quality of heterogeneous data sources in smart city environments is a missing ground truth for comparing results. If there is no exactly planned infrastructure it is a complex process to determine, which sensor measurements are correct in case of contradictory information. CityPulse aims at predicting the plausibility of events by pursuing a monitoring approach that analyses sensor values of related sensors of different kinds. This is realised in addition to the Atomic Monitoring and called Composite Monitoring. This approach helps to determine if outliers in sensor readings are due to defective sensors or can be explained by similar information from related sensors. 

![SingleAnalysisExample](https://github.com/CityPulse/CompositeMonitoring/blob/master/ExampleFigure1.png)

## Requirements and Dependencies
- PostgreSQL version >= V9.3
- OpenStreetMap data from the GDI
- Gnu R version 3.2.4 (2016-03-10)
- R Libraries: 

|             |Package      |Version(>=) |License                   |
|:------------|:------------|:-----------|:-------------------------|
|reshape2     |reshape2     |1.4.1       |MIT + file LICENSE        |
|ggplot2      |ggplot2      |2.1.0       |GPL-2                     |
|lattice      |lattice      |0.20-33     |GPL (>= 2)                |
|scales       |scales       |0.4.0       |MIT + file LICENSE        |
|RColorBrewer |RColorBrewer |1.1-2       |Apache License 2.0        |
|shinyBS      |shinyBS      |0.61        |GPL-3                     |
|leaflet      |leaflet      |1.0.1       |GPL-3 &#124; file LICENSE |
|V8           |V8           |1.1         |MIT + file LICENSE        |
|rgdal        |rgdal        |1.1-10      |GPL (>= 2)                |
|rjson        |rjson        |0.2.15      |GPL-2                     |
|jsonlite     |jsonlite     |0.9.21      |MIT + file LICENSE        |
|DT           |DT           |0.1         |GPL-3 &#124; file LICENSE |
|knitr        |knitr        |1.13        |GPL                       |
|gdata        |gdata        |2.17.0      |GPL-2                     |
|maptools     |maptools     |0.8-39      |GPL (>= 2)                |
|rgeos        |rgeos        |0.3-19      |GPL (>= 2)                |
|RCurl        |RCurl        |1.95-4.8    |BSD                       |
|bitops       |bitops       |1.0-6       |GPL (>= 2)                |
|dplyr        |dplyr        |0.5.0       |MIT + file LICENSE        |
|plyr         |plyr         |1.8.4       |MIT + file LICENSE        |
|shiny        |shiny        |0.13.2      |GPL-3 &#124; file LICENSE |
|sp           |sp           |1.2-3       |GPL (>= 2)                |
|RPostgreSQL  |RPostgreSQL  |0.4-1       |GPL-2 &#124; file LICENSE |
|DBI          |DBI          |0.4-1       |LGPL (>= 2)               |

## Installation
Detailled installation / execution instructions will follow.

## Contributers
The GDI component was developed as part of the EU project CityPulse. The consortium member University of Applied Sciences Osnabrück provided the main contributions for this component.

## Link
The code of the needed GDI component can be found here: https://github.com/CityPulse/GDI.git
![FullAnalysisExample](https://github.com/CityPulse/CompositeMonitoring/blob/master/ExampleFigure2.png)
