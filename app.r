library(readr)
library(rvest)
results_2001 <- read_csv("Data/results_2001.csv")
results_2002 <- read_csv("Data/results_2002.csv")
results_2003 <- read_csv("Data/results_2003.csv")
results_2004 <- read_csv("Data/results_2004.csv")
results_2005 <- read_csv("Data/results_2005.csv")
results_2006 <- read_csv("Data/results_2006.csv")
results_2007 <- read_csv("Data/results_2007.csv")
results_2008 <- read_csv("Data/results_2008.csv")
results_2009 <- read_csv("Data/results_2009.csv")
results_2010 <- read_csv("Data/results_2010.csv")
results_2011 <- read_csv("Data/results_2011.csv")
results_2012 <- read_csv("Data/results_2012.csv")
results_2013 <- read_csv("Data/results_2013.csv")
results_2014 <- read_csv("Data/results_2014.csv")
results_2015 <- read_csv("Data/marathon_results_2015.csv")
results_2016 <- read_csv("Data/marathon_results_2016.csv")
results_2017 <- read_csv("Data/marathon_results_2017.csv")

#Not working - can't figure out how to turn it into a table
url <- "http://www.baa.org/races/boston-marathon/boston-marathon-history/participation.aspx"
tables <- url %>%
  read_html() %>%
  html_nodes(css = "tbody") 
participation <- html_table(tables[[2]])
