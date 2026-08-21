## Code to prepare the `npo` and `cibap` datasets.
##
## IMPORTANT: the datasets are serialized S7 objects. Whenever the class
## definitions in R/classes.R change (e.g. new properties), rerun this
## script so the stored objects match the class definitions.
## Run with the package loaded, e.g. devtools::load_all().

# NPO Terugblik 2023.
# The original download URL
# (https://open.overheid.nl/documenten/dpc-9225c6ebccfe327bd67c4a5d0013d85d1abdcf22/pdf,
# linked from https://www.rijksoverheid.nl/documenten/rapporten/2024/11/20/bijlage-3-npo-terugblik-2023)
# is no longer available; the Internet Archive copy is used instead.
npo <- pdf_read("https://web.archive.org/web/2025id_/https://open.overheid.nl/documenten/dpc-9225c6ebccfe327bd67c4a5d0013d85d1abdcf22/pdf")
usethis::use_data(npo, overwrite = TRUE)

# Kwaliteitsagenda 2024-2027 Cibap
# (https://www.rijksoverheid.nl/documenten/rapporten/2024/09/16/kwaliteitsagenda-2024-2027-cibap)
cibap <- pdf_read("https://www.rijksoverheid.nl/binaries/rijksoverheid/documenten/rapporten/2024/09/16/kwaliteitsagenda-2024-2027-cibap/kwaliteitsagenda-cibap.pdf")
usethis::use_data(cibap, overwrite = TRUE)
