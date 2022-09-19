############################################### #############################
### VU_Colors.R
############################################### #############################
### R code for Student Analytics VU University Amsterdam
### Copyright 2018 VU
### Web Page: http://www.vu.nl
### Contact: Theo Bakker (t.c.bakker@vu.nl)
###
### Filename: VU_Colors.R
### Goal: A data frame with colors of the VU and faculties
###
### Dependencies: Dependency
###
### Datasets used: Datasets
###
### Comments:
### 1) Comments.
### 2) ___
###
############################################### #############################
### History:
### 07/13/2017: TB: Comment creation
### 03-08-2017: JvZ: Tableau colors added
############################################### #############################

VU_faculty_colors <- data.frame(Faculty = c('ALW',
                                            'FGB',
                                            'FSW',
                                            'RCH',
                                            'FEWEB',
                                            'ESB',
                                            'FEW',
                                            'GGL',
                                            'FGW',
                                            'THK',
                                            'ACTA',
                                            'GNK',
                                            'VUmc',
                                            'VUMC'),
                                Color =
                                  c(rgb(112,191,84, maxColorValue=256),
                                    rgb(213,62,41,    maxColorValue=256),
                                    rgb(140,37,35,    maxColorValue=256),
                                    rgb(75,178,178,   maxColorValue=256),
                                    rgb(128,63,152,   maxColorValue=256),
                                    rgb(80,148,151,   maxColorValue=256),
                                    rgb(80,148,151,   maxColorValue=256),
                                    rgb(167,126,37,   maxColorValue=256),
                                    rgb(200,67,129,   maxColorValue=256),
                                    rgb(203,218,237,  maxColorValue=256),
                                    rgb(203,218,237,  maxColorValue=256),
                                    rgb(32,64,154,    maxColorValue=256),
                                    rgb(32,64,154,    maxColorValue=256),
                                    rgb(32,64,154,    maxColorValue=256)
                                  ),
                                Color_light =
                                  c(rgb(112,191,84, maxColorValue=256),
                                    rgb(213,62,41,    maxColorValue=256),
                                    rgb(140,37,35,    maxColorValue=256),
                                    rgb(75,178,178,   maxColorValue=256),
                                    rgb(128,63,152,   maxColorValue=256),
                                    rgb(80,148,151,   maxColorValue=256),
                                    rgb(80,148,151,   maxColorValue=256),
                                    rgb(167,126,37,   maxColorValue=256),
                                    rgb(200,67,129,   maxColorValue=256),
                                    rgb(203,218,237,  maxColorValue=256),
                                    rgb(203,218,237,  maxColorValue=256),
                                    rgb(32,64,154,    maxColorValue=256),
                                    rgb(32,64,154,    maxColorValue=256),
                                    rgb(32,64,154,    maxColorValue=256)
                                  ), stringsAsFactors = F )

VU_Blue       <- rgb(19,71,140,  maxColorValue=256)
VU_BlueDark   <- rgb(19,71,140,  maxColorValue=256)
VU_BlueMiddle <- rgb(35,115,196, maxColorValue=256)
VU_BlueLight  <- rgb(126,178,231,maxColorValue=256)

## Tableau's default color palette
## https://www.tableau.com/about/blog/2016/7/colors-upgrade-tableau-10-56782
Tableau_colors <- c("#4E79A7",
                    "#F28E2C",
                    "#E15759",
                    "#76B7B2",
                    "#59A14F",
                    "#EDC949",
                    "#AF7AA1",
                    "#FF9DA7",
                    "#9C755F",
                    "#BAB0AB")

## Export the colors
usethis::use_data(VU_faculty_colors, internal = T, overwrite = T)
usethis::use_data(VU_Blue, internal = T, overwrite = T)
usethis::use_data(VU_BlueDark, internal = T, overwrite = T)
usethis::use_data(VU_BlueMiddle, internal = T, overwrite = T)
usethis::use_data(VU_BlueLight, internal = T, overwrite = T)
usethis::use_data(Tableau_colors, internal = T, overwrite = T)
