This guide presents an overview for installing **openVA** package, with some common error reports and solutions at the end.

## 1. Overview of openVA package structure
Whenever the **openVA** package is loaded to R, it also requires four other core packages on CRAN for each of the VA coding methods, namely, **InSilicoVA**, **InterVA4**, **Tariff**, and **nbc4va**, and **ggplot2** for visualization. Additionally, the **InSilicoVA** package further requires the dependency of **coda** and **rJava** for its computation. 

Users typically need to take no specific action regarding the dependencies, since R takes care of them automatically. However, sometimes issues with installing **openVA** package can arise because of some of the dependencies, which may require additional configurations and re-installing the package. I find that most of the times, the errors stem from loading **rJava**, and thus is the main focus of this guide.

## 2. Pre-requisites
As the name suggests, to properly load the package **rJava**, you will need two key ingradients: _R_, and _Java_. Here is how you can make sure you have the right combination of the two:
 
### Check R environment 
1. If you do not aleady have it, install from [CRAN](http://cran.r-project.org/). Follow the instructions at the link to choose a mirror that will take you to the download page. After download, double click the file to install.
2. Open R. On the welcome message, there is a line starting with “Platform” and ending with “(32-bit)” or “(64-bit)”. It is very important to know which version (32-bit or 64-bit) of R you use, since the Java JDK should have the same version. 
3. Sometimes multiple versions of R could be installed on the same machine, so you should check the version you wish to use for data analysis. For example, if you prefer using RStudio to run the codes, you should check the default R version of RStudio by reading the welcome message for RStudio, instead of, say, the R version when opening from command line.

### Check Java installation
1. To check if Java in installed on your **Mac** or **Linux** machine, open terminal if you use Mac or Linux and type in ```java -version```. If Java is installed, it will show the Java version number. Version number at least 1.7.x should be sufficient.
1. To check if Java is installed on your **Windows** machine, open **Control Panel**, select **Programs**, then select **Programs and Features**, and see if Java is listed. 
1. If no Java is installed (on Mac OSX, you will see a pop-up notice of installing Java JDK), or version too low, you should download and install a newer Java JDK by following the instructions at http://www.oracle.com/technetwork/java/javase/downloads/index.html. On that page, there are downloads for both “Java SE 8uXX” and “Java SE 7uXX” (XX is the specific most recent re- leased subversion number), which stands for Standard Edition Java 8 and 7. The required download is JDK (Java Development Kit). Click on download link for JDK and choose the appropriate version. You should choose “x86” version if your R version is 32-bit, and “x64” if R is 64-bit. Then follow the instructions to finish download and install Java.
3. After successfully installing Java, try again typing ```java -version``` on terminal or Command Prompt. It should show the correct version number just installed.

## 3. Misc suggestions
1. For users not familiar with R envrionment, see [the official introduction](https://www.r-project.org/about.html).
2. Many people find using R from Rstudio to be more convenient. Rstudio could be downloaded from [its website](https://www.rstudio.com/products/rstudio/download3/). 


## 4. Errors and solutions
Here I include some tips of installing and loading **rJava** package that I found to be useful in the past. Some of those here have been fixed in later version so the solutions might be out-dated.   

If you encounter other errors not listed here, or could not resolve the errors following the steps listed, or would like to propose new solutions, you are more than welcome to contact me (Richard Li, lizehang@uw.edu) or submit issue reports to [this Github repository](https://github.com/richardli/openVA/issues). If you know explicitly the errors are caused by rJava, you could also submit issue reports to the [rJava repository](https://github.com/s-u/rJava/issues) directly.

Here is the things you might see down the rabbit hole - One thing omitted in the tricks is that typically for most of the tricks below (for Mac and Linux system especially, I found Windows machine usually does not need this), at the end, it requires another step of re-compiling rJava from source, i.e., adding the ```type='source'``` as below:

```
install.packages('rJava', type='source')
```

1. Fail to load rJava on Mac OSX (El Capitan 10.11): [original post](http://stackoverflow.com/questions/35179151/cannot-load-r-xlsx-package-on-mac-os-10-11)
  + Example Error Message:
    
         ```
         JavaVM: Failed to load JVM: /bundle/Libraries/libserver.dylib
         Java FATAL: Failed to load the jvm library.
         Error : .onLoad failed in loadNamespace() for 'InSilicoVA', details:
          call: .jinit()
          error: JNI_GetCreatedJavaVMs returned -1
         ```
   
  + Solution: Open terminal and execute the commands:
  
        ```
        sudo R CMD javareconf
        ```
   Then reopen R and run

        ```
        install.packages("rJava", type = "source")
        library(openVA)
        ```
   
1. Fail to install rJava (usually from Rstudio): [original post](http://stackoverflow.com/questions/34212378/installation-of-rjava)
  + Example Error Message:
    
        ```
        ERROR: configuration failed for package ‘rJava’
        * removing ‘....some directory.../rJava’
        Warning in install.packages :
        installation of package ‘rJava’ had non-zero exit status
        ```
   
  + Solution: Open terminal and execute the commands:
  
        ```
        sudo R CMD javareconf
        ```
   
1. Fail to load rJava on Mac: [original post](http://stackoverflow.com/questions/30738974/rjava-load-error-in-rstudio-r-after-upgrading-to-osx-yosemite)
  + Example Error Message:
  
  + Solution: This is a common problem whenever your Mac OS gets updated, especially if you use Rstudio. As of OSX El Capitan 10.11, the trick that seems to work for me is to run the following from terminal:
  
        ```
        sudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib
        ```

1. Failure to load rJava on 64-bit Windows: [original post](http://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r)
  + Example Error Message: 
        
        ```
         Error : .onLoad failed in loadNamespace() for 'rJava', details: 
         call: inDL(x, as.logical(local), as.logical(now), ...)
         ```
  + Solution: This is only useful when **rJava** package can be installed (```install.packages("rJava")```), but not loaded (```library('rJava')```). Sometimes it works by executing the following in R console:
      
         ```
         if (Sys.getenv("JAVA_HOME")!="")
         Sys.setenv(JAVA_HOME="")
         library(rJava)
         ```
1. Failure to install rJava on Windows with ``type = "source"``.
  + Example Error Message:
   
         ```
         Warning: running command 'sh ./configure.win' had status 127
         ERROR: configuration failed for package 'rJava'
         ```

  + Solution: (1) Check java path is in the system environmental variable list (on Win 10, click Windows icon and type "environmental"), (2) Try re-install without ``type = "source"``.
  
1. Failure to install rJava on Linux: [original post](http://stackoverflow.com/questions/3311940/r-rjava-package-install-failing)
  
  + Example Error Message:
   
          ```
          checking JNI data types... configure: error: One or more JNI types differ from the corresponding native type. You may need to use     
          non-standard compiler flags or a different compiler in order to fix this.
          ERROR: configuration failed for package ‘rJava’
          ```
  
  + Solution:
          ```
          apt-get install r-cran-rjava
          ```
  
1. More misc error messages and hacks that I have not tried myself:
  + [OSX + Rstudio 1](http://stackoverflow.com/questions/26948777/how-can-i-make-rjava-use-the-newer-version-of-java-on-osx)
  + [OSX + Rstudio 2](http://spartanideas.msu.edu/2015/06/27/the-rjava-nightmare/) 
  + [OSX + Rstudio 3](https://andrewgoldstone.com/blog/2015/02/03/rjava/)
  + [OSX + Rstudio 4](http://conjugateprior.org/2014/12/r-java8-osx/)
  + [Windows + registry](https://www.r-bloggers.com/how-to-load-the-rjava-package-after-the-error-java_home-cannot-be-determined-from-the-registry/)
  + [No Java run time in OSX](https://github.com/s-u/rJava/issues/37) (with many useful discussions)

