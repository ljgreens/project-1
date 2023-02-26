# project-1

To reduce the time to run the .R script and .Rmd report, I attempted to upload my generated .RData objects to the github repository instead of having the code regenerate these objects, which is time consuming.  However, the file size of some of the  .RData objects are greater than 100MB that github permits, so reducing runtime by providing the .RData objects was not possible.  Regardless, from implementing if statements, the code is compartmentalized by using variables starting with the name "switch" to control which code chunks are computed, since some of the operations take days to run (i.e. bootstrap loess fitting and predicting).  


The submitted .Rmd and .R files have all of these switches set to 1, which means that all of the code will run,  generate my .RData objects, save them to the local drive that is set by the "save_path" variable, and reload then when needed.  To run the code, the "save_path" variable needs to be changed from my local path to the local path on the machine that will run the code.  This process is also explained at the beggining of the script within the "Important Notes about Running this Script" section of the .R and .Rmd files.
