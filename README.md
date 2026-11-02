# Hardcaml with openXC7

[![FPGA examples](https://github.com/TheCBaH/hardcaml-f4pga/actions/workflows/build.yml/badge.svg?branch=master)](https://github.com/TheCBaH/hardcaml-f4pga/actions/workflows/build.yml)

Generate Verilog with Hardcaml and build Artix-7 bitstreams with
[openXC7](https://github.com/openXC7/toolchain-installer).

## Get started
* [![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://github.com/codespaces/new?hide_repo_select=true&ref=master&repo=628173356)
* run
  * `make rtl` build verilog files
  * `make basys3.design` synthesize FPGA .bit file
  * `make arty_35.design` build the Arty A7-35T design
  * `make basys3.download` or `make arty_35.download` program a connected board

Bitstreams are written to `build/<board>/top.bit`, alongside synthesis,
place-and-route logs and a timing report. Both boards use a 100 MHz clock.

The image includes the OCaml switch and Hardcaml dependencies, Yosys,
nextpnr, Project X-Ray device data, `fpga-as`, `openFPGALoader`, the chip
database and interconnect delay table for both board packages, and pinned Digilent
constraints. Building a design requires no downloads or device database
generation. Both packages use one prebuilt die database, with the package
selected explicitly through nextpnr's native command line.
The image build also assembles a small test design for both packages.

The Dockerfile pins the upstream installer and board constraints. It builds
the toolchain components in separate cached layers, before the OCaml
features, and removes temporary source trees after each installation.
`JOBS` controls image compilation parallelism (default 4).

USB access is needed only for programming. Building bitstreams does not
require the swap helper.
