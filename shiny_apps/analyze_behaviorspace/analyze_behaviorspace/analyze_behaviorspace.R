library(shiny)

# Find directory where this script resides and set current directory to that path
# see https://support.rstudio.com/hc/en-us/community/posts/200895567-can-user-obtain-the-path-of-current-Project-s-directory-
this.dir <- dirname(sys.frame(1)$ofile)
setwd(this.dir)

runApp(display.mode="normal")
