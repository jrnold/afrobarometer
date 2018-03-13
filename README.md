# Merged Afrobarometer Data

This repository contains some R scripts that clean and merge data from 
[Afrobarometer] surveys. Currently this includes only rounds 3--6 and 
the variables that I have used in my work.

## Build

1. Sign up for a username at [afrobarometer.org](http://afrobarometer.org/).
   The Afrobarometer data is free to use, but requires a login to download.
   See the [Data Use Policy](http://afrobarometer.org/data/data-use-policy)
   before using the data.

2. Copy `afrobarometer-example.yml` to `afrobarometer.yml`. Edit 
   Edit the `user` and `password` fields to your Afrobarometer username and
   password.
   
3. Install R dependencies
    ```r
    devtools::install(".")
    ```

3. Run or source the script `download.R` to download the  data and `afrobarometer.R` to merge the data.
    ```console
    Rscript download.R
    Rscript afrobarometer
    ```

## Usage

Running the build script saves an R object with the combined dataset as `data/afrobarometer.rds`:

You can load this data using:
```r
afrobarometer <- readRDS("data/afrobarometer.rds")
```

## LICENSE

All code is MIT licensed.

See the [Afrobarometer Data Use Policy](http://www.afrobarometer.org/data/data-use-policy)
for the copyright on the Afrobarometer data.
