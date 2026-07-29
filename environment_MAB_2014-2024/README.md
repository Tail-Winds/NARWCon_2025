# Environmental data — 2014–2024

DOPPIO (ocean) + NDBC (in-situ met/wave) + WAVERYS (waves) for sites
**A-7M, A-2M, T-2M**, at **hourly / three_hourly / daily**.
No ERA5, IMERG, AIS, or GFW (i.e. no wind reanalysis, no precip, no vessel/fishing).
Files are in `cache_2014-2024/`.

## Read it in R

```r
source("collab/read_env.R")     # one-time: install.packages("nanoparquet")

# site, variables, time segment, resolution:
df <- get_env("A-7M",
              vars = c("sst", "ndbc_wspd", "wav_hs"),
              start = "2018-06-01", end = "2018-08-31",
              resolution = "three_hourly")

df <- get_env("A-2M")           # whole series, all variables, hourly
```
`time` is always kept. `start`/`end` are UTC; omit either for an open end.

**Site distances / depth** (static per site) live in `collab/site_distances.csv`.
Attach them to any result with `add_distances(get_env("A-2M"), "A-2M")` — it appends
`dist_shore_km`, `dist_OC_km`, `depth_m`, etc.

## How `get_env()` works

`get_env()` is the only function — `source("collab/read_env.R")` defines it. Given a
site (and optionally a time window, a variable list, and a resolution) it:

1. builds the file name from the site + resolution and reads that `.parquet` into a data.frame;
2. trims to rows within `start`/`end` if given (it matches daily *dates* vs hourly *timestamps* automatically);
3. keeps only the variables you listed, plus `time` (or all of them if you list none);
4. hands the data.frame back.

Defaults: `resolution = "hourly"`, no time limit, all variables. `time` is always kept.

## Variables

| group | columns |
|---|---|
| DOPPIO (ocean) | `sst`, `bottom_temp`, `sss`, `zeta` |
| NDBC (in-situ) | `ndbc_wspd`, `ndbc_wdir`, `ndbc_gst`, `ndbc_wvht`, `ndbc_dpd`, `ndbc_pres`, `ndbc_atmp`, `ndbc_wtmp` |
| WAVERYS (waves) | `wav_hs`, `wav_tp` — *three_hourly & daily only* |

### Variable names 
DOPPIO - ROMS ocean model, nearest ~7 km grid cell to the site:
  - sst  - sea surface temperature (°C) (shallowest sigma level)
  - bottom_temp -  near-bottom temperature (°C) (deepest sigma level)
  - sss - sea surface salinity (PSU)
  - zeta - sea surface height anomaly (m)

  NDBC - buoy 44009 (Delaware Bay entrance, a single fixed buoy ~13–24 km from these sites, not at the
  site):
  - ndbc_wspd - wind speed (m/s)
  - ndbc_wdir - wind direction (° true; circular-mean on resample)
  - ndbc_gst - wind gust speed (m/s)
  - ndbc_wvht - significant wave height (m)
  - ndbc_dpd - dominant wave period (s)
  - ndbc_pres - sea-level pressure (hPa)
  - ndbc_atmp - air temperature (°C)
  - ndbc_wtmp - water temperature (°C)

  WAVERYS - wave reanalysis, nearest grid cell; three_hourly & daily files only (3-hour means):
  - wav_hs - significant wave height (m)
  - wav_tp - peak wave period (s)

  Two notes: NDBC and WAVERYS both report wave height (ndbc_wvht = in-situ buoy point; wav_hs = model),
  so they'll differ. And the hourly files have DOPPIO + NDBC only.  Waves appear once you go to
  three_hourly/daily.

## To (Re)build the cache (Julia)

```
julia --project=/home/robert/env_data collab/build_lean_env.jl
```
Options: `--site A-2M`, `--start/--end` for a shorter range.

Note: NDBC is the one exception. The single
final hour (2024-12-31 23:00) is missing by construction used.
