# pupilr: a package to manage a series of eye-tracking experiments

This package is the public version of the one which manages a pupillometry (eye-tracking) project. (The original R package is stored as part of a private repository on Bitbucket for confidentiality reasons.) This modified package is put online to showcase the development work I've done on it.

This package has three primariy uses: 

 * First, it pre-processes the raw data (which has personally identifiable information and is thus stored on a secure remote server) when run on the data server so that the accessible data no longer has participant information in it. 
 
 * Second, it bundles this data together in a nice, portable package. 
 
 * Third, it provides a number of helpful functions to further process the eye-tracking data for analysis and plotting.
 
## Caveats:
 
Although this package was designed with documentation and other user/developer-friendly aspects, there are a few essential things that prevent it from being CRAN-worthy, or even super distributable.  

Most importantly, **I did NOT include `Imports` or `Suggests` in the `DESCRIPTION` file.** I do have parts of the code that check to make sure certain packages are installed, but I had a tramatic incident with making my own R package once have been mentally scarred a little.  This code is actually (sadly) based on `dplyr 0.5`, and not tested on the new versions. I admit all that is dumb, but this package is primarily for my own uses, so I'll get around to fixing all that stuff later.

Also, **this package does not include the actual participant data**. Although the data processing flow of this package removes the most personally identifiable participant information (i.e., participants' Amazon Mechanical Turker IDs), certain demographic information about participants is retained. This is ok if the data on my lab computer, but I want to be sure that I don't cross any IRB lines by publicly sharing that data. You can get a sense of what the data would look like, and how it would be processed with the script `data-raw/data_preprocessing_cleanTurkData.R`.

This also means that the "vignettes" that are included in this package are not rebuildable.  You'll just have to be content with seeing what they _would_ look like had you had access to the participant data.

I have to acknowledge that the way this package processes data was built with advice from [Dave Kleinschmidt](http://www.davekleinschmidt.com/r-packages/). His tutorials were very helpful in establishing a clean workflow.

Finally, although I have a detailed git history of the development of this package on the private repository on Bitbucket, importing that could reveal personally identifiable participant information, so this git history is starting in media res.
