## code to prepare `npo` dataset goes here

npo <- pdftools::pdf_data("https://open.overheid.nl/documenten/dpc-9225c6ebccfe327bd67c4a5d0013d85d1abdcf22/pdf")

usethis::use_data(npo, overwrite = TRUE)


## code to prepare `cibap` dataset goes here

cibap <- pdftools::pdf_data("https://www.rijksoverheid.nl/binaries/rijksoverheid/documenten/rapporten/2024/09/16/kwaliteitsagenda-2024-2027-cibap/kwaliteitsagenda-cibap.pdf")

usethis::use_data(cibap, overwrite = TRUE)
