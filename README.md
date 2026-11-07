# Hardcaml with openXC7

[![FPGA examples](https://github.com/TheCBaH/hardcaml-f4pga/actions/workflows/build.yml/badge.svg?branch=master)](https://github.com/TheCBaH/hardcaml-f4pga/actions/workflows/build.yml)

Generate Verilog with Hardcaml and build Artix-7 bitstreams with
[openXC7](https://github.com/TheCBaH/devcontainer.openxc7).

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
The FPGA tools and databases come from the checksummed
[devcontainer.openxc7 v0.0.3 release](https://github.com/TheCBaH/devcontainer.openxc7/releases/tag/v0.0.3).
That release validates static binaries and bitstream generation offline on
Debian and Alpine. The Dockerfile downloads the native xz tool archive and
shared xc7a50t data in a separate stage and copies only the installed runtime into the OCaml image. It does
not compile FPGA tools or generate chip databases. OCaml dependencies and
the pinned Digilent constraints are also installed while building the image.

The release supports Linux x86_64 and ARM64. CI uses native runners for both
architectures with OCaml 4.14.3 and 5.4.1; each version/architecture pair has
its own cached dependency image. Both board designs are built on every leg.
The OCaml setup uses the shared
[`ocaml-features` feature](https://github.com/TheCBaH/ocaml-features), as in
[`ocaml-devcontainer`](https://github.com/TheCBaH/ocaml-devcontainer).
OCaml 5.4 uses a pinned [Notty compatibility fix](https://github.com/pqwy/notty/pull/58)
for the formatter API change; OCaml 4.14 uses the released Notty package.
The default development compiler is OCaml 4.14.3; set `OCAML_VERSION` before
building the container to select another version. The committed
`.devcontainer/openxc7.sha256` pins both native tool archives and the shared
device data; update it together with `OPENXC7_VERSION` when upgrading the toolchain.

USB access is needed only for programming. Building bitstreams does not
require the swap helper.
