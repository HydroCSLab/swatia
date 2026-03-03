#!/bin/sh
set -e
# curl -L https://github.com/posit-dev/air/releases/latest/download/air-installer.sh | sh
air format . inst/cli/swatia
Rscript -e 'devtools::document()'
pkg="swatia_$(sed '/Version/!d; s/.* //' DESCRIPTION).tar.gz"
R CMD build .
R CMD check $pkg
R CMD INSTALL $pkg
