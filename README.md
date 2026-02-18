# SWAT+-ISPSO Adapter

This repository provides a reusable adapter layer for integrating [SWAT+](https://github.com/swat-model/swatplus) with the [Isolated-Speciation-Based Particle Swarm Optimization (ISPSO)](https://github.com/HuidaeCho/ispso) algorithm.
The goal is to separate model-specific operations from optimization logic, enabling reproducible, transferable calibration workflows across watersheds and projects.

Unlike project-specific scripts, this adapter enforces:
* Separation of concerns between configuration, model execution, and objective evaluation
* Parameterized model interaction (no hardcoded identifiers or paths)
* Reusable workflow structure suitable for multiple study areas
* Clear command-line and R-level interfaces

The adapter is designed to serve as computational infrastructure within the CLAWRIM ecosystem, supporting method-driven hydrologic modeling rather than one-off case studies.
