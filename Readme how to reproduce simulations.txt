There are three scripts that creates three different simulations 

-- 2_sample_simulation_including_uno
The file to use to run a 2-sample simulations for KONP and all compared tests including uno's tests

-- 2_sample_simulation
The file to use to run a 2-sample simulations for KONP and all compared tests, not including uno's tests


-- K_sample_simulation
The file to use to run a K-sample (K>2) simulation for KONP, Logrank and Peto-Peto tests 



In order to run each simulation:
1. install all the packages that are being loaded in the beginning of the script
* To install KONPSURV package use  devtools::install_github("matan-schles/KONPsurv")
2. choose the scenarios function you want and copy them and paste them to the top of the script (instead of the current scenarios)
3. run the entire script except for the for loop in the end of the file
4. if you don't want to run the simulation for all sample sizes examined, change the sample sizes inside the for loop
5. set a working directory in which the csv file will be created
6. run the for loop and wait for the results, when the simulation is done, see the results in the csv file created.
