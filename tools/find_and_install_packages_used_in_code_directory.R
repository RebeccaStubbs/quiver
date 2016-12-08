################################################################################
# Author: Rebecca Stubbs
# Date: Sept 2, 2016
# Purpose:
#   When starting work on a new research team or after updating an R
# environment, it's frustrating to find out when you try to call a library to
# run a piece of code, only to find out that you need to install packages.
# This code looks through a directory for all files ending with .R or .r,
# reads them in as text files, and finds the libraries called. Note that this code
# will only install packages that contain the library(...) syntax, not the
# deprecated require(...) syntax.
# 
# To use this code:
#
# Simply change the variable code_folder to your desired file path, and run this 
# code in its entirety. If everything runs smoothly, it should print a list of
# libraries that failed to install-- these could include character strings that
# were never libraries to begin with, but perhaps existed in a comment somewhere
# in R code within the directory sandwiched between "library(" and ")". This should
# not cause problems and the code should still run, but these erroneous "package 
# names" will appear in your "failed libraries" list that is printed at the end.
# Feel free, of course, to use the get_pkg functions outside of this context as well.
# 
#################################################################################


########## CHANGE THESE INPUTS #################################################
  code_folder<-"C:/Users/stubbsrw/Documents/us_counties_stubbs_gitrepo/"
  n<-NULL # If you set this to an integer value, only libraries that occur n times 
          # or more in your code directory will be installed. If it stays as NULL,
          # all libraries will be installed regardless of frequency.

################################################################################



# Defining function to install a package if it isn't already downloaded into your R environment
# ---------------------------------------------------------------------------------------------------------------------------

  get_pkg<-function(package_name,load=FALSE){
    cat("\n")
    # Give this function the string of the package name you would like to install.
    # the "load" argument, if true, will also 
    if(package_name %in% installed.packages()){cat(" Looks like you already had the ",
                                                   package_name," package installed. ")}else{
      cat("Installing library ",package_name,"; ")
      try(install.packages(package_name))
      if(package_name %in% installed.packages()){
        cat("Successfully installed.")}else{
        cat("Something went wrong with that install !!!")}}
    if(load){
      try(library(package_name,character.only=T))
      if (package_name %in% (.packages())){cat(" Package successfully loaded.")}else{cat(" !!! Package did not load.")}}
    }

# Loading libraries needed for THIS code
# ---------------------------------------------------------------------------------------------------------------------------
  # This piece of code needs a few libraries to operate, 
  # so we need to install and/or load those first.
    packages_needed_for_this_code<-c("data.table","qdapRegex","stringr","readr")
    
    # Installing dependent packages
    for (package in packages_needed_for_this_code){
      get_pkg(package,load=TRUE)
    }

# Finding and installing packages called in code within a certain directory
# ---------------------------------------------------------------------------------------------------------------------------
  # Getting a list of all .r or .R files within the directory of interest
    files<-list.files(code_folder,recursive=T, full.names=T)
    files<-files[str_sub(files, start= -2) %in% c(".r",".R")]

# Making a table of all the libraries within the code with frequencies
# ---------------------------------------------------------------------------------------------------------------------------
  libraries<-list()
  
  for (file in files){
    libraries[[file]]<-rm_between(read_file(file), "library(", ")", extract=TRUE)
  }

  libraries<-data.table(table(rbindlist(libraries)))[order(-N)]
  
    # If a threshold is set, subset the libraries to only those above a certain count.
    if (!is.null(n)){
      cat("Subsetting libraries to only libraries that occur more than ",n," times.")
      libraries<-libraries[N>=n,]
    }else{cat("All libraries will be installed.")}

# Installing packages!
# ---------------------------------------------------------------------------------------------------------------------------
  for (lib_name in libraries$V1){
    get_pkg(lib_name,load=F)
  }

# Printing an explicit list of what packages FAILED
# ---------------------------------------------------------------------------------------------------------------------------
  failed_packages<-libraries$V1[!(libraries$V %in% installed.packages())]
  print("The following packages FAILED to install:")
  print(failed_packages)

