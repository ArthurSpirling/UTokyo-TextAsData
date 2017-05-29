# UTokyo-TextAsData

These materials are for the University of Tokyo 'Text as Data' course, taught June 3--4, 2017.  You will need the statistical environment R to follow along with the exercises.  Please ensure you have an (up to date) copy of R prior to the course starting.

In terms of an overview, on the first day we will cover the process of making text into data, along with some descriptive inference ideas.  On the second day, we will take a look at some popular [supervised](https://en.wikipedia.org/wiki/Supervised_learning) and [unsupervised](https://en.wikipedia.org/wiki/Unsupervised_learning) methods as used in social science.  Given the time constraints, we will take broad but inevitably shallow pass over these areas.  

## Before We Begin: R Packages

Hopefully, everyone will have wireless access on the day, enabling packages to be installed 'on the fly'.  It may be helpful, however, to have some packages installed in advance.  In particular, inputting the following code into your R console prior to the course should save time.

    install.packages("quanteda")
    install.packages("readtext")
    install.packages("lsa")
    install.packages("bursts")
    install.packages("topicmodels")
    install.packages("ldatuning")

It is particularly important that you have the latest version of quanteda, which at the time of writing is 0.9.9.65.  You can check this by typing 

    packageVersion("quanteda")

which should return 

    [1] ‘0.9.9.65’



## Before We Begin: Data

You can make local copies on your computer of the data in the data folder (recommended), or you can access it directly during the class (assuming you have internet access). I have also put all the data required in the course dropbox, [here](https://www.dropbox.com/sh/m49xkc6vano5791/AAD6W0UUZYVHU-8S6fhWDOk9a?dl=0)

## Before We Begin: Code

As with the data, you can make local copies of the code (recommended) or cut-and-paste during the course (not recommended).  You can also grab the code from the course dropbox, [here](https://www.dropbox.com/sh/8uluc1bme385hyq/AAB61UxLJ0dVyW2oPygYjKtDa?dl=0)

