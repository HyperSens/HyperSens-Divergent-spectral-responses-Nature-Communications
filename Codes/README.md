
### These scripts include the main source for the analysis of leaf data, the Feature-weighted Random forest and the spectral clustering algorithms

Code for the analysis of leaf data to reproduce Figure 2e-j <br> 
The feature weighted Random forest algorithm includes the feature importance for the disease prediction into the model <br>
The spectral clustering algorithm is used in the reclassification stage in order to reclassify the "uncertain" trees using the divergent-specific biotic and abiotic spectral traits. 


### Packages requirements

<ul>
  
 <li> These algorithms were implemented in R (v. 3.5.3; R Development Core Team, Vienna, Austria)</li>
  
<li> The code for the leaf measurements analysis requires the following packages: "RColorBrewer", "ggplot2", "gridExtra", "dplyr" and "grid"  </li>
  
<li> The feature weighted random forest algorithm is included in the R package “viRandomForests” proposed by Liu, Y. & Zhao, H (2017) <sup>* </li>
  
<li>The “viRandomForests” package could be obtained from: <a href="http://zhaocenter.org/softwares/">“viRandomForests” R package </a>  </li>

<li> The unsupervised spectral clustering algorithm requires the kernlab package <sup>** </li>  
  
<li> Both algorithms require the Caret Package <sup>*** </li>    
 </ul> 





<sup>*</sup> Liu, Y. & Zhao, H. Variable importance-weighted random forests. Quant Biol 5, 338–351 (2017) <br>

<sup>**</sup> Karatzoglou, A., Smola, A., Hornik, K. & Zeileis, A. kernlab An S4 Package for Kernel Methods in R Journal of Statistical Software 11, 1–20 (2004); https://CRAN.R-project.org/package=kernlab <br>


<sup>***</sup> Kuhn, M. et al. caret: Classification and Regression Training v6.0-78 
(CRAN, 2017); https://CRAN.R-project.org/package=caret  <br>


Additional codes can be provided from the corresponding author upon reasonable request

