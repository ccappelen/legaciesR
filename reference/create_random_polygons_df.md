# Create random overlapping polygons

Create random overlapping polygons

## Usage

``` r
create_random_polygons_df(
  ext,
  n = 5,
  nmap_min = 50,
  nmap_max = 200,
  year_min = 1800,
  year_max = 1900,
  n.pnts = 10,
  progress = TRUE
)
```

## Arguments

- ext:

  An sf object, within which the polygons will be sampled. If `NULL`, it
  will be use a polygon of Africa as the extent extent object (from
  [rnaturalearthdata::countries50](https://docs.ropensci.org/rnaturalearthdata/reference/countries.html)).

- n:

  Integer, the number of groups to create

- nmap_min:

  Integer, minimum number of features within a group

- nmap_max:

  Integer, maximum number of features within a group

- year_min:

  Integer, minimum year

- year_max:

  Integer, maximum year

- n.pnts:

  Integer, number of points to sample for each feature to create a
  polygon

- progress:

  Logical, whether to show progress bar

## Value

An sf dataframe of random overlapping polygons
