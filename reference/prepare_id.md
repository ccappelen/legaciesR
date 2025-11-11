# Prepare ID data

The function prepares the raw ID data for further use by, e.g.,
expanding rows into polity-year observations and extracting information
on capitals and hierarchies.

## Usage

``` r
prepare_id(data, multiple_capital = FALSE)
```

## Arguments

- data:

  Raw ID data to be cleaned and expanded. Typically called
  `legacies_id_coding.xlsx`.

- multiple_capital:

  Logical, whether to keep all capitals if there are multiple capitals
  in the same polity-year. In that case, a polity-year will be expanded
  to one row per capital. Default is `FALSE`, in which case only the
  capital with the longest spell will be included. If two or more
  capitals have the same spell, one capital will be selected at random.

## Value

A data frame with polity-year observations and the variables decribed
below:

- `polity_name`:

  Name of polity

- `COWID`:

  COW code for the polity

- `COWNUM`:

  Numeric COW code for the polity

- `year`:

  Year of observation

- `other_names`:

  Alternative polity names

- `start_year`:

  Start year of a given polity spell

- `end_year`:

  End year of a given polity spell

- `population_10k`:

  Whether the polity-year lives up to the population criterium

- `autonomy`:

  Whether the polity-year is regarded as autonomous

- `capital_name`:

  Name of capital of the polity in a given year. If there are multiple
  capitals, the most prominent capital is recorded here.

- `capital_lon`:

  Longitude of capital city

- `capital_lat`:

  Latitude of capital city

- `capital_raw`:

  Raw capital city information. Can contain information for multiple
  capital cities. The capital variables above contain information on
  only the most prominent capital.

- `hie_COWID`:

  COWID for which the given polity is in a hierarchical relationship.
  Can contain multiple COWIDs if the polity is in multiple hierarchical
  relationships.

- `hie_type`:

  Type of hierarchy (`1 = tributary`, `2 = dependency`). Can contain
  multiple.

- `hie_tributary`:

  Dummy equal to `1` if the polity is in at least one tributary
  relationship.

- `hie_dependency`:

  Dummy equal to `1` if the polity is in at least one dependency
  relationship.

- `independent_dependency`:

  Dummy equal to `1` if the polity is independent, i.e. it is not a
  dependency.

- `independent_tributary`:

  Dummy equal to `1` if the polity is independent, i.e. it is not a
  tributary.

- `independent`:

  Dummy equal to `1` if the polity is independent, i.e. neither
  dependency nor tributary.

- `hie_raw`:

  Raw hierarchy coding.

- `ethnonational_group`:

- `EPR_link`:

- `ACD_actor_link`:

- `end_agent`:

- `destination_states`:

- `destination_state_1-5`:
