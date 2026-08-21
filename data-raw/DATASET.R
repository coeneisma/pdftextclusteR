## code to prepare `npo` dataset goes here

# NOTE (2026-08-21): this download URL is no longer available (the landing page
# https://www.rijksoverheid.nl/documenten/rapporten/2024/11/20/bijlage-3-npo-terugblik-2023
# still exists). The dataset remains available in data/npo.rda.
npo <- pdftools::pdf_data("https://open.overheid.nl/documenten/dpc-9225c6ebccfe327bd67c4a5d0013d85d1abdcf22/pdf")

usethis::use_data(npo, overwrite = TRUE)


## code to prepare `cibap` dataset goes here

cibap <- pdftools::pdf_data("https://www.rijksoverheid.nl/binaries/rijksoverheid/documenten/rapporten/2024/09/16/kwaliteitsagenda-2024-2027-cibap/kwaliteitsagenda-cibap.pdf")

usethis::use_data(cibap, overwrite = TRUE)
