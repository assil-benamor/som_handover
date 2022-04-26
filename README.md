
<div id="top"></div>




<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/lilos404/som_handover">
    <img src="https://data.humdata.org/image/2015-11-05-231341.581365REACHlogo_300x125_grey.png" alt="Logo">
  </a>

<h3 align="center">Assil handover - Reach SOM</h3>

  <p align="center">
    Handover for SOM data team 
    <br />
    <br />
    <a href="https://github.com/lilos404/som_handover/issues">Report Bug</a>
    ·
    <a href="https://github.com/lilos404/som_handover/issues">Request Feature</a>
  </p>
</div>



<!-- ABOUT THE PROJECT -->
## About The Project

This repository contains the different tools/scripts/projects created for Reach Somalia. Under each project folder you can find the source code and output examples. Instructions and additional information can be found in the different "README.me" pages.
Input data are not included in this repo for data protection purposes.   


<p align="right">(<a href="#top">back to top</a>)</p>

## Project structure map

* [Link](https://htmlpreview.github.io/?https://github.com/lilos404/som_handover_map/blob/main/Handover%20SOM-d7fbd02a72150451fad82cdc0abbac67.html)


## Built With

* [R](https://www.r-project.org/)
* [Python](https://www.python.org/)

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- GETTING STARTED -->

## Steps to run the projects

1. Clone the repo
   ```sh
   git clone https://github.com/lilos404/som_handover
   ```
   
2. Add the missing input files (files can be found in Synology drive)
   ```sh
   mkdir input/
   cd input/
   cp "[path_where_data_file_is_located]/input_data.zip" .
   unzip input_data.zip
   ```

3. Run the scripts in order. Example from the DSA:
   ```r
   source("src/1_aggregation.R")
   source("src/2_indicators_coding.R")
   source("src/3_lsg_scoring.R")
   source("src/3_lsg_scoring.R")   
   source("src/4_results_table.R")
   source("src/5_data_merge.R")
   ...
   ```

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- ROADMAP -->
## Structure

- [x] DSA
    - [x] Pre Data collection
    - [x] Data collection
    - [x] Data analysis

- [x] JMCNA
   - [x] Analysis
   - [x] PIN files
   - [x] MSNI
   
- [x] H2R
    - [x] Aggregation
    - [x] Analysis

 
    


<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTRIBUTING -->
## Contributing

If you have a suggestion that would make this better or you want to add other projects, please fork the repo and create a pull request. 

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/NewFeature`)
3. Commit your Changes (`git commit -m 'Add some NewFeature'`)
4. Push to the Branch (`git push origin feature/NewFeature`)
5. Open a Pull Request

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Assil BEN AMOR - assil.benamor@reach-initiative.org


<p align="right">(<a href="#top">back to top</a>)</p>

