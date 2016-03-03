# EES 4760 Apps
Various shiny apps related to teaching Agent-Based modeling using 
Railsback and Grimm, 
[*Agent-Based and Individual-Based Modeling*](http://www.railsback-grimm-abm-book.com/). 
These apps make life easier than using Excel to analyze submodels and 
behaviorspace output.

## analyze_behaviorspace
Upload a .csv file (table format) from a behaviorspace run and do some simple 
processing and plotting. Can be useful for turning a behaviorspace output file 
into something that's useful to analyze and plot with Excel.

If you check the box "Summary Table"  calculates the mean and standard deviation
of the selected y-variable over all the behaviorspace runs, and prepares
a table that you can export using "Save Table" and then import into Excel
to analyze and plot.

You can also save an image the plot from this app by clicking on "Save Plot".

Before running analyze_behaviorspace for the first time, you must install
several R packages from CRAN. Sourcing the script `install_packages.R` will
automatically install those packages.

Then you only need to source the file `analyze_behaviorspace.R` to run the 
shiny application. I have run into some weirdness when I try to save files
(`/csv` data tables or `.png` image files) from the browser built into 
RStudio. If you run into this problem, just click on the 
"Open in Browserj" tab to open the app in your regular browser and run things
from there.

## contour_plot
Interactive plot of countours of expected utility for Chapter 12, adding 
prediction to the business investor model

## jg-tif
My version of Pierre-Olivier Chasset's 
[Testing is Fun](https://github.com/chasset/tif) unit-testing library for 
NetLogo. To use it, the you should put the following line in your NetLogo model, 
before `globals`:

    includes__ ['jg-tif.nls']

To use the package, in your `to setup` procedure, you should have the line

    initialize-tests

Then to do a test you would put something like

    test-that "Testing that foo = bar"
    expect-that foo equals bar

If `foo = bar`, nothing happens. If `foo != bar`, you will see an error message 
on the NetLogo Command Center. 

Available comparisons for this library are
`x equals y`, `x does-not-equal y`, `x is-greater-than y`,
`x is-greater-or-equal y` `x is-less-than y` `x is-less-or-equal y`,
`x is-true`, `x is-false`, `x is-identical-to y`, and `x is-not-identical-to y`

Finally, when you stop your NetLogo model, or at any other convenient point, 
you can use the line

    resume-all-tests

to print a summary of the number of successful and failed tests.
