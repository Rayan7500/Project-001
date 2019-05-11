tes <- function(){

library(data.table)
library(readxl)
library(ggplot2)
library(here)
library(Matrix)

# Table of emissions and value added by sector
# Sector id = index of the sector in the technical coeff matrix (i = 1..N)
# Import data 
CI_naf <- as.data.table(read_excel(here("..", "/data/Etude ratio monetaire/Resultat Evolution (emplois, tes)/va_emissions.xlsx")))
CI_naf[, sector_id := 1:.N]

sectors <- data.table(sector_id = CI_naf$sector_id,
                      emissions = CI_naf$`2014co2e`,
                      value_added = CI_naf$`2014va`) #en millions


# Table of intermediary consumption
# Format : sector of origin / sector of destination / value of intermediary consumption
# Includes intra sector flows

CI <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/INSEE/CI.xlsx"), sheet = "2014"))
CI[, X__1 := NULL]

CI_m <- melt(CI, id="a38")
CI_m <- merge(CI_m, CI_naf[, list(a38, sector_id)], by = "a38")
CI_m <- merge(CI_m, CI_naf[, list(a38, sector_id)], by.x = "variable", by.y = "a38")

intermediary_cons <- data.table(from_sector = CI_m$sector_id.x,
                                to_sector = CI_m$sector_id.y,
                                value = CI_m$value)


# Compute the production by sector
# Production = VA + sum(IC)
sum_ic <- intermediary_cons[, list(sum_ic = sum(value)), by = to_sector]

production <- merge(sectors[, list(sector_id, value_added)], sum_ic,
                    by.x = "sector_id", by.y = "to_sector", all = TRUE)

production[is.na(sum_ic), sum_ic := 0]
production[, prod := value_added + sum_ic]

# Compute the technical coefficients matrix
# which are the intermediary consumption flow values normalized by the sector's total production
# (Intra sector intermediary consumption should be removed from the flows)
technical_coeffs_mat <- merge(intermediary_cons[from_sector != to_sector],
                           production[, list(sector_id, prod)],
                           by.x = "to_sector", by.y = "sector_id")

technical_coeffs_mat[, a := value/prod]

technical_coeffs_mat <- Matrix::sparseMatrix(i = technical_coeffs_mat$to_sector,
                                             j = technical_coeffs_mat$from_sector,
                                             x = - technical_coeffs_mat$a,
                                             dims = c(nrow(sectors), nrow(sectors)))

technical_coeffs_mat <- technical_coeffs_mat + Matrix::Diagonal(nrow(sectors))

# Normalize sectoral emissions by their production
emissions <- merge(sectors[, list(sector_id, emissions)],
                   production[, list(sector_id, prod)],
                   by = "sector_id")

emissions[, b := emissions/prod]

# Compute the FEMs
fem <- solve(technical_coeffs_mat, emissions$b)

sectors$fem <- fem@x*1000
#sectors[, fem := fem*1000]

CI_naf <- merge(CI_naf, sectors[, list(sector_id, fem)], by = "sector_id")

x11()
p <- ggplot(CI_naf)
p <- p + geom_bar(aes(x = a38, y = fem), stat ="identity")
p

}