
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vcardr

<!-- badges: start -->
<!-- badges: end -->

Read/write VCF contact files with R

## Description

Imports data from vcf vcard format into R dataframe, basic manipulations
with contacts data and write back into vsf vcard file format, which can
be imported with various contact apps.

Supports VCARD 3.0 file format, including these fields:

- FN – full name
- N – name fields
- BDAY
- PHOTO (in base64 format)
- TEL – including phone type (HOME/CELL/FAX…) and preferable phone
- ORG
- TITLE

## Installation

You can install the development version of vcardr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("i2z1/vcardr")
```

## Example

Make dataframe from VCF file:

``` r
library(vcardr)

df <- vcardr::import_vcf("path_to_file.vcf")
## basic example code
```

Note that if contact has few phone numbers, record would consist of
coressponding number of rows in dataframe with the same set of columns
but various telephone numbers.
