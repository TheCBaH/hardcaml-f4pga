# Devcontainer for hardcaml and F4GPA

[![F4GPA examples](https://github.com/TheCBaH/fpga_example/actions/workflows/build.yml/badge.svg?branch=master)](https://github.com/TheCBaH/hardcaml-f4pga/actions/workflows/build.yml)

Devcontainer to create development environment for hardcaml that uses F4FPGA toolchain.

## Get started
* [![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://github.com/codespaces/new?hide_repo_select=true&ref=master&repo=628173356)
* run
  * `.devcontainer/with_swap.sh` to add swap (needed by fasm)
  * `make rtl` build verilog files
  * `make basys3.design` synthesize FPGA .bit file
