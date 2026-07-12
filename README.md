# SWATIA: SWAT+ISPSO Adapter

This repository provides a reusable adapter layer for integrating [SWAT+](https://github.com/swat-model/swatplus) with the [Isolated-Speciation-Based Particle Swarm Optimization (ISPSO)](https://github.com/HuidaeCho/ispso) algorithm (Cho et al., 2011).
The goal is to separate model-specific operations from optimization logic, enabling reproducible, transferable calibration workflows across watersheds and projects.

Unlike project-specific scripts, this adapter enforces:
* Separation of concerns between configuration, model execution, and objective evaluation
* Parameterized model interaction (no hardcoded identifiers or paths)
* Reusable workflow structure suitable for multiple study areas
* Clear command-line and R-level interfaces

The adapter is designed to serve as computational infrastructure within the HydroCS ecosystem, supporting method-driven hydrologic modeling rather than one-off case studies.

<!-- vim-markdown-toc GFM -->

* [Installation](#installation)
* [Usage](#usage)
* [Example configuration](#example-configuration)
* [Acknowledgments](#acknowledgments)
* [License](#license)

<!-- vim-markdown-toc -->

## Installation

```R
install.packages("remotes")
remotes::install_git("git@github.com:hydrocslab/swatia.git")

# install CLI
swatia::install_swatia_cli()
```

## Usage

```bash
swatia [options] <command> [args]

Options:
  -c, --config=PATH    Path to configuration file (default: ./config.R)
  -h, --help           Show this help message

Commands:
  run_ispso    [best_par_txt]
  get_best_obj [obj_day_txt]
  get_best_x   [obj_day_txt]
  get_best_par [obj_day_txt]
```

## Example configuration

```R
config <- list(
  par = list(
    cn2 = list(
      # HRU; widen upper (was 30)
      change_type = "pctchg",
      range = c(-30, 40)
    ),
    awc = list(
      # SOL; widen lower (was -20)
      change_type = "pctchg",
      range = c(-40, 20)
    ),
    canmx = list(
      # HRU
      change_type = "pctchg",
      range = c(-50, 50)
    ),
    esco = list(
      # HRU; was abschg (-0.2, 0.2)
      change_type = "absval",
      range = c(0.00, 1.00)
    ),
    alpha = list(
      # AQU
      change_type = "absval",
      range = c(0.01, 0.50)
    ),
    flo_min = list(
      # AQU
      change_type = "absval",
      range = c(0, 10)
    ),
    deep_seep = list(
      # AQU
      change_type = "absval",
      range = c(0.001, 0.05)
    ),
    revap_min = list(
      # AQU
      change_type = "absval",
      range = c(0, 20)
    ),
    lat_ttime = list(
      # HRU
      change_type = "pctchg",
      range = c(-50, 50)
    ),
    revap_co = list(
      # AQU
      change_type = "absval",
      range = c(0.02, 0.20)
    ),
    epco = list(
      # HRU; was abschg (-0.2, 0.2)
      change_type = "absval",
      range = c(0.00, 1.00)
    ),
    snofall_tmp = list(
      # HRU; widen both (was -2, 2)
      change_type = "abschg",
      range = c(-5, 5)
    ),
    snomelt_tmp = list(
      # HRU; widen both (was -2, 2)
      change_type = "abschg",
      range = c(-5, 5)
    ),
    snomelt_max = list(
      # HRU
      change_type = "pctchg",
      range = c(-50, 50)
    ),
    snomelt_min = list(
      # HRU
      change_type = "pctchg",
      range = c(-50, 50)
    ),
    snomelt_lag = list(
      # HRU
      change_type = "pctchg",
      range = c(-50, 50)
    )
  ),

  txtinout = "../TxtInOut",
  chaid = 55, # USGS gage 13010065
  #chaid = 196, # USGS gage 13022500
  #chaid = 143, # USGS gage? Upper Snake outlet
  obs_day_txt = "../obs_day_13010065.txt",
  nobs_day_c = 3288,

  sim_dir = "day",
  obj_day_txt = "obj_day.txt",

  # objective function: 1 - NSE
  calc_obj = function(obs, sim) {
    sum((obs - sim)^2) / sum((obs - mean(obs))^2)
  }
)

config$control <- ispso::ispso_control(
  S = ispso::ispso_swarm_size(config$par),
  maxiter = 30
)
```

## Acknowledgments

The development of the SWAT+ISPSO Adapter (SWATIA) was supported in part by the [NSF award 2308358](https://www.nsf.gov/awardsearch/show-award/?AWD_ID=2308358) and the [USDA award 2025-69012-44233](https://rawcs.nmsu.edu/).

## References

[Cho, Huidae, Kim, Dongkyun, Olivera, Francisco, Guikema, Seth D., 2011. Enhanced Speciation in Particle Swarm Optimization for Multi-Modal Problems. European Journal of Operational Research 213 (1), 15--23](https://doi.org/10.1016/j.ejor.2011.02.026).

## License

Copyright (C) 2023-2026, Huidae Cho <<https://hydro.isnew.info/>>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <<http://www.gnu.org/licenses/>>.
