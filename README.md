# Merged Afrobarometer Data



# Build

1. Sign up for a username at [afrobarometer.org](http://afrobarometer.org/).
   The Afrobarometer data is free to use, but requires a login to download.
   See the [Data Use Policy](http://afrobarometer.org/data/data-use-policy)
   before using the data.

2. Copy `afrobarometer-example.yml` to `afrobarometer.yml`. Edit 
   Edit the `user` and `password` fields to your Afrobarometer username and
   password.

3. Run 
    ```r
    Rscript R/download.R
    ```
