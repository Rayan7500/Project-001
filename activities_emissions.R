library(readxl)
library(here)
library(data.table)
library(ggplot2)
library(RColorBrewer)

source(here("code", "load_sites_info.R"), encoding = "UTF-8")
source(here("code", "load_sites_energy_consumption.R"), encoding = "UTF-8")
source(here("code", "load_renovation_works.R"), encoding = "UTF-8")

# ----------------------------------------------------
# Load client detailed data
sites_areas <- load_sites_areas(here("data", "poste_immo", "energie", "03 - Contrôle de cohérence client.xlsx"))
sites_energy <- load_sites_energy_consumption(here("data", "poste_immo", "energie", "03 - Contrôle de cohérence client.xlsx"))


# ----------------------------------------------------
# Compute co2 emissions
energy_ef <- as.data.table(read_excel(here("data", "elioth", "hyps.xlsx"), sheet = "energy_ef"))
sites_energy <- merge(sites_energy, energy_ef, by = "energy_vector")
sites_energy[, co2_emissions := consumption*ef]


# ----------------------------------------------------
# Compute co2 emissions ratios
kaya <- sites_energy[, list(consumption = sum(consumption),
                      co2_emissions = sum(co2_emissions)),
               by = list(irsi_id)]

kaya <- merge(kaya, sites_areas[, list(irsi_id, total_floor_area_subl)], by = c("irsi_id"))

kaya[, co2_intensity := co2_emissions/consumption]
kaya[, energy_intensity := consumption/total_floor_area_subl]

# Cutoff extreme values
kaya[energy_intensity < 0, energy_intensity := NA]
kaya[energy_intensity > 500, energy_intensity := NA]
kaya[co2_intensity > 0.4, co2_intensity := NA]
kaya[co2_intensity < 0.05, co2_intensity := NA]

summary(kaya$co2_intensity)
summary(kaya$energy_intensity)

kaya[co2_emissions > 0, share := co2_emissions/sum(co2_emissions)]
kaya <- kaya[order(-share)]

kaya[co2_emissions > 0, p := (1:.N)/.N]
kaya[co2_emissions > 0, cum_share := cumsum(share)]

summary(kaya[co2_emissions > 0]$co2_emissions)

kaya[, co2_area := co2_emissions/total_floor_area_subl]
kaya[, co2_area_bin := round(co2_area)]

kaya_hist <- kaya2017[taille ==" > 50 000 Hab." & co2_area > 0 & co2_area < 100, .N, by = co2_area_bin]

x11()
# ----------------------------------------------------
# Plot distribution of emissions intensity
p <- ggplot(kaya_hist)
p <- p + geom_bar(aes(x = co2_area_bin, y = N),
                  stat = "identity", width=1)
p <- p + theme_minimal(base_size = 16)
p <- p + labs(title="Diffus", x = "\nIntensité carbone [kgCO2e/m²/an]", y = "Nombre de sites\n")
p <- p + scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10))
p

ggsave(plot = p, file = here("plots", "real_estate_emissions.png"), width = 6, height = 4.5, type = "cairo-png")


# ----------------------------------------------------
# Compute correction area ratios by use
extrap_areas <- as.data.table(read_excel(here("data", "elioth", "hyps.xlsx"), sheet = "extrap_areas"))
areas <- sites_areas[, list(area = sum(floor_area)), by = use]
areas <- merge(areas, extrap_areas, by = "use")
areas[, area_corr := area*ratio]

energy <- sites_energy[, list(consumption = sum(consumption),
                        co2_emissions = sum(co2_emissions)),
                 by = list(use, energy_vector)]

energy <- merge(energy, extrap_areas, by = "use")

energy[, co2_emissions := ratio*co2_emissions]
energy[, consumption := ratio*consumption]

# areas[, sum(area_corr)]
# energy[, sum(co2_emissions)]/areas[, sum(area_corr)]
# energy[, sum(consumption)]/areas[, sum(area_corr)]
# energy[, sum(co2_emissions)]/1000
# energy[, sum(co2_emissions)]/energy[, sum(consumption)]

energy[, co2_emissions_use := sum(co2_emissions), by = use]

total_use <- energy[, list(co2_emissions = sum(co2_emissions)), by = use]
total_use <- total_use[order(-co2_emissions)]

energy <- energy[, use := factor(use, total_use$use, unique(use))]


# ----------------------------------------------------
# Plot total emissions per use and energy vector in 2017
energy[, energy_vector_name := factor(energy_vector,
                                      c("district_heating", "electricity", "fuel", "gas", "propane", "wood"),
                                      c("Réseau de chaleur", "Electricité", "Fioul", "Gaz", "Propane", "Biomasse"))]

energy_vector_name <- energy[, unique(energy_vector_name)]
colors <- c("#d32f2f", "#FDD835", "#37474F", "#0097A7", "#26C6DA", "#795548")
names(colors) <- energy_vector_name

p <- ggplot(energy)
p <- p + geom_bar(aes(x = use, y = co2_emissions/1000, fill = energy_vector_name),
                  stat = "identity", width = 0.5)
p <- p + labs(title = "",
              subtitle = "",
              x = "\nMétier", y = "Emissions [tCO2e/an]\n",
              fill = "")
p <- p + scale_fill_manual(values = colors)
p <- p + scale_y_continuous(limits = c(0, 80000), breaks = seq(0, 80000, 20000), expand = c(0, 0))

p <- p + theme_minimal(base_size = 14)
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

ggsave(plot = p, file = here("plots", "building_emissions.png"), width = 8, height = 5, type = "cairo-png")


# ----------------------------------------------------
# Compute primary energy consumption
energy[, ef_to_ep := ifelse(energy_vector == "electricity", 2.58, 1)]
energy[, sum(ef_to_ep*consumption)]/areas[, sum(area_corr)]



# ----------------------------------------------------
# Plot renovation works data
works <- load_renovation_works(here("data", "poste_immo", "travaux", "GPS.xlsx"))
works_year <- works[, list(budget = sum(budget)), by = list(end_works_year)]


# Plot total renovation budget per year
p <- ggplot(works_year)
p <- p + geom_bar(aes(x = end_works_year, y = budget/1e6), stat = "identity", fill= "#009688")
p <- p + theme_minimal(base_size = 14)
p <- p + labs(x = "\nAnnée de fin des travaux", y = "Budget [M€]\n",
              title = "Montant des travaux financés par Poste Immo",
              subtitle = "")
p <- p + scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0, 0))
p

ggsave(plot = p, file = here("results", "works_budgets.png"), width = 3, height = 4.5, type = "cairo-png")


# Plot ratio of renovation cost per m² vs total floor area
works_site <- works[, list(budget = sum(budget)), by = c("irsi_id", "end_works_year")]
areas <- sites_areas[, list(area = sum(floor_area)), by = irsi_id]
works_site <- merge(works_site, areas, by = "irsi_id", all.x = TRUE)
works_site[, budget_ratio := budget/area]

p <- ggplot(works_site)
p <- p + geom_point(aes(x = area, y = budget_ratio), col = "#009688", alpha = 0.5)
p <- p + theme_minimal(base_size = 14)
p <- p + labs(x = "\nSurface locative utile [m²]", y = "Budget des travaux [€/m²]\n",
              title = "Ratios monétaires des travaux financés par Poste Immo",
              subtitle = "")
p <- p + scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000))
p <- p + scale_x_log10(breaks = c(100, 1000, 10000, 100000), limits = c(50, 100000), labels = function(x) format(x, big.mark = " ", scientific = FALSE))
p <- p + annotation_logticks()
p

ggsave(plot = p, file = here("results", "works_ratios.png"), width = 7, height = 4.5, type = "cairo-png")



