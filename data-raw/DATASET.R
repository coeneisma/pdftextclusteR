## Code to prepare the bundled datasets.
## Run with the package loaded, e.g. devtools::load_all().
##
## IMPORTANT: the datasets are serialized S7 objects. Whenever the class
## definitions in R/classes.R change (e.g. new properties), rerun this
## script so the stored objects match the class definitions.

# cibap: Kwaliteitsagenda 2024-2027 Cibap
# https://www.rijksoverheid.nl/documenten/rapporten/2024/09/16/kwaliteitsagenda-2024-2027-cibap
cibap <- pdf_read("https://www.rijksoverheid.nl/binaries/rijksoverheid/documenten/rapporten/2024/09/16/kwaliteitsagenda-2024-2027-cibap/kwaliteitsagenda-cibap.pdf")
usethis::use_data(cibap, overwrite = TRUE)

# burgerschap: Rapport wetsevaluatie burgerschap (February 2026)
# https://www.rijksoverheid.nl/documenten/2026/02/12/rapport-wetsevaluatie-burgerschap
burgerschap <- pdf_read("https://open.overheid.nl/documenten/3e3a3f14-a190-4a2e-acb7-70f2499242d0/file")
usethis::use_data(burgerschap, overwrite = TRUE)

# eu2024: The EU in 2024 - General Report on the Activities of the European Union
# https://op.europa.eu/en/publication-detail/-/publication/9d1a7eec-fb41-11ef-b7db-01aa75ed71a1/language-en
eu2024 <- pdf_read("https://op.europa.eu/o/opportal-service/download-handler?identifier=9d1a7eec-fb41-11ef-b7db-01aa75ed71a1&format=pdf&language=en&productionSystem=cellar")
usethis::use_data(eu2024, overwrite = TRUE)

# eurostat: Key figures on Europe - 2025 edition
# https://ec.europa.eu/eurostat/web/products-key-figures/w/ks-01-25-003
eurostat <- pdf_read("https://ec.europa.eu/eurostat/documents/15216629/22447468/KS-01-25-003-EN-N.pdf/0b4b896a-57a9-3348-06d0-319c667b6b08?download=true&version=3.0")
usethis::use_data(eurostat, overwrite = TRUE)
