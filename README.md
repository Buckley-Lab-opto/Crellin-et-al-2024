# Repository for Local optogenetic NMYII activation within the zebrafish neural rod results in long-range, asymmetric force propagation

This repository contains the Fiji Macros, MATLAB and R code necessary for the analysis performed in 'Local optogenetic NMYII activation within the zebrafish neural rod results in long-range, asymmetric force propagation', (2024).

Authors: Helena A Crellin; Chengxi Zhu; Guillermo Serrano-Nájera; Amelia Race; Kevin O'Holleran; Martin O Lenz; Clare Buckley

DOI: https://doi.org/10.1101/2024.09.19.613826

Code authors: 

Helena Crellin and Guillermo Serrano-Nájera 

Corresponding author contact: 

Clare Buckley, clare.buckley-2@manchester.ac.uk

## Installation

Install Fiji and Bio-Formats Importer

Install MATLAB and the following add-ons/toolboxes: PIVlab, Parallel Computing Toolbox, Statistics and Machine Learning Toolbox, Image Processing Toolbox, Circular Statistics Toolbox (Directional Statistics)

Install R and RStudio, and the following packages: tidyverse, openxlsx, rstatix, circular, CircStats, signal, ggpubr, devtools, svglite, cmocean, viridis

Clone the Github repository

## Image processing

The input data are TIF timelapses. The Fiji macro will save any TIF timelapses in a folder as an image sequence. The folder of image sequences is the input into first section of the launch_PIV_analysis.m script in MATLAB. The images are filtered and saved as a image sequence of the moving average of the previous, current and subsequent images.

## Particle image velocimetry analysis

Manually input the moving average into the PIVlab GUI. Mask the area of interested and apply to all images. Run the PIV with the default settings. Save the mask and the export the PIV output file with the default filenames.

Input the file paths of the image folder, original TIF, ROI file (as .bmp), PIV mask and PIV output into the next section of the launch_PIV_analysis.m MATLAB script. This processes the velocity vector field with a spatial filter, calculates strain rate fields and averages the ROI, the NSEW neighbouring regions and ROI anterior-posterior column. The outputs of the script are visualisations of the vector fields overlying the original timelapse, .xlsx files and data visualisations (for initial inspection) of the averaged data. 

## Data visualisation

The .xlsx files are the input for the R scripts and produce the data visualisations in Crellin et al. 2024. The R scripts are numbered in running order. These process the data (e.g. threshold and average the data from multiple embryos), producing .xlsx files for subsequent R scripts and data visualisations of individual embryos and averaged data.

## Other analysis

This includes code for the statistical analysis and visualisation of non-PIV data.

