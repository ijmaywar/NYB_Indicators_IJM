install.packages("rjson")
library("rjson")
json_file <- "/Users/ian/Desktop/*NYB Indicators/nsidc-extratropical-cyclone-tracking-cnect/nsidc-extratropical-cyclone-tracking-cnect.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))
json_data


json_data$`@graph`$