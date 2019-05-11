library(readxl)
library(here)
library(data.table)
library(ggplot2)
library(RColorBrewer)

source(here("code", "load_workers.R"), encoding = "UTF-8")
source(here("code", "load_expenses.R"), encoding = "UTF-8")
source(here("code", "load_car_trips.R"), encoding = "UTF-8")
source(here("code", "load_corporate_trips.R"), encoding = "UTF-8")

# ----------------------------------------------------
# Load client detailed data
train_air_trips <- load_corporate_trips(here("data", "poste_immo", "corporate", "Travel - voyages.xlsx"))
car_trips <- load_car_trips(here("data", "poste_immo", "corporate", "Expense - KM.xlsx"))
expenses <- load_expenses(here("data", "poste_immo", "corporate", "Expense - NDF.xlsx"))
workers <- load_workers(here("data", "poste_immo", "corporate", "Tableau effectif par direction.xlsx"))
n_workers <- workers[, sum(n_workers)]
n_workers <- 830

# ----------------------------------------------------
# Aggregate emissions for the 2017 year
long_trips_2017 <- train_air_trips[year(date) == 2017, list(long_trips = sum(co2_emissions)), by = mode]
long_trips_2017[, category := "Déplacements"]

car_trips_2017 <- car_trips[year(date) == 2017, list(car_trips = sum(co2_emissions)), by = type]
car_trips_2017[, category := "Déplacements"]

expenses_2017 <- expenses[year(date) == 2017, list(expenses = sum(co2_emissions, na.rm = TRUE)), by = expense_category_0]
expenses_2017[expense_category_0 == "Transport", category := "Déplacements"]
expenses_2017[expense_category_0 != "Transport", category := "Achats"]

setnames(long_trips_2017, c("category_1", "emissions", "category_0"))
setnames(car_trips_2017, c("category_1", "emissions", "category_0"))
setnames(expenses_2017, c("category_1", "emissions", "category_0"))

corporate_emissions <- rbind(long_trips_2017, expenses_2017, car_trips_2017)
corporate_emissions <- corporate_emissions[emissions != 0]

corporate_emissions[category_1=="Voiture"]$emissions <- 10861 + mean(car_ef$ef)*3720000 #ajout des kilomètres parcourus
bur <- data.frame(category_1 = "Bureau",
                  emissions =230000,#conso bati coporate
                  category_0 ="Consommations")
corporate_emissions <- rbind(corporate_emissions,bur)

corporate_emissions[, emissions_worker := (emissions)/n_workers]
corporate_emissions[, share := emissions/sum(emissions)]

corporate_emissions[, category_1 := factor(corporate_emissions$category_1,
                                           rev(c("Air", "Train", "Transport", "Voiture", "Moto", "Repas et Hébergement", "Z-Autre Dépense", "Animation Equipe")),
                                           rev(c("Avion", "Train", "Autres transports", "Voiture", "Moto","Bureau", "Repas et hébergement", "Autres dépenses", "Animation")))]

x11()
# ----------------------------------------------------
# Plot the emissions per employee, per category of use
p <- ggplot(corporate_emissions)
p <- p + geom_bar(aes(x = category_0, y = emissions_worker, fill = category_1), 
                  stat = "identity", width = 0.5)
p <- p + scale_fill_manual(values = brewer.pal(9, "Set3"))
p <- p + labs(fill = "", x = "", y = "Emissions [kgCO2e/employé]\n",
              title = "",
              subtitle = "")
p <- p + theme_minimal()
p <- p + theme(axis.text.x = element_text(margin = margin(t = 10)))
p

ggsave(plot = p, file = here("results", "corporate_emissionss.png"), width = 6, height = 6, type = "cairo-png")


# ----------------------------------------------------
# Plot the detailed (per employee) long distance travel emissions
x <- trips[year(date) == 2017, list(co2_emissions = sum(co2_emissions)), by = list(traveller_id, mode)]
x[, co2_emissions_tot := sum(co2_emissions), by = traveller_id]

p <- ggplot(x)
p <- p + geom_bar(aes(x = reorder(factor(traveller_id), -co2_emissions_tot), y = co2_emissions, fill = mode),
                  stat = "identity", position = "stack", width = 1)
p <- p + labs(fill = "", x = "\nCollaborateurs", y = "Emissions [kgCO2e/employé]\n",
              title = "",
              subtitle = "")
p <- p + theme_minimal(base_size = 14)
p <- p + theme(axis.text.x = element_blank(),
               panel.grid.major.x = element_blank())
p <- p + scale_y_continuous(limits = c(0, 9000), breaks = seq(0,9000, 1000), expand = c(0, 0))
p

ggsave(plot = p, file = here("plots", "corporate_trips_employees.png"), width = 12, height = 4.5, type = "cairo-png")


# ----------------------------------------------------
# Compute statistics of long distance travel emissions
x <- trips[year(date) == 2017, list(co2_emissions = sum(co2_emissions)), by = list(traveller_id)]
summary(x$co2_emissions)

x <- x[order(co2_emissions)]
x[, cum := cumsum(co2_emissions)/sum(co2_emissions)]
x[, p := (1:.N)/.N]


# ----------------------------------------------------
# Plot the detailed (per employee) car travel emissions
x <- car_trips[year(date) == 2017, list(co2_emissions = sum(co2_emissions)), by = list(user_id)]
summary(x$co2_emissions)

x <- x[order(co2_emissions)]
x[, cum := cumsum(co2_emissions)/sum(co2_emissions)]
x[, p := (1:.N)/.N]

p <- ggplot(x)
p <- p + geom_bar(aes(x = reorder(factor(user_id), -co2_emissions), y = co2_emissions),
                  stat = "identity", position = "stack", width = 1)
p <- p + labs(fill = "", x = "\nCollaborateurs", y = "Emissions [kgCO2e/employé]\n",
              title = "",
              subtitle = "")
p <- p + theme_minimal(base_size = 14)
p <- p + theme(axis.text.x = element_blank(),
               panel.grid.major.x = element_blank())
p <- p + scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, 250), expand = c(0, 0))
p

ggsave(plot = p, file = here("plots", "corporate_car_trips_employees.png"), width = 4, height = 4.5, type = "cairo-png")


# ----------------------------------------------------
# Plot the detailed (per employee) expenses emissions
x <- expenses[year(date) == 2017, list(co2_emissions = sum(co2_emissions, na.rm = TRUE)), by = list(user_id, expense_category_0)]
x[, co2_emissions_tot := sum(co2_emissions, na.rm = TRUE), by = user_id]


p <- ggplot(x)
p <- p + geom_bar(aes(x = reorder(factor(user_id), -co2_emissions_tot), y = co2_emissions, fill = expense_category_0),
                  stat = "identity", position = "stack", width = 1)
p <- p + labs(fill = "", x = "\nCollaborateurs", y = "Emissions [kgCO2e/employé]\n",
              title = "",
              subtitle = "")
p <- p + theme_minimal(base_size = 14)
p <- p + theme(axis.text.x = element_blank(),
               panel.grid.major.x = element_blank())
p <- p + scale_y_continuous(limits = c(0, 2500), breaks = seq(0, 2500, 500), expand = c(0, 0))
p <- p + scale_fill_manual(values = brewer.pal(n = 6, "Set2"))
p

ggsave(plot = p, file = here("plots", "corporate_expenses_employees.png"), width = 13, height = 4.5, type = "cairo-png")


# ----------------------------------------------------
# Compute statistics of expenses emissions
x <- expenses[year(date) == 2017, list(co2_emissions = sum(co2_emissions, na.rm = TRUE)), by = list(user_id)]
x <- x[order(co2_emissions)][co2_emissions != 0]
x[, cum := cumsum(co2_emissions)/sum(co2_emissions)]
x[, p := (1:.N)/.N]

summary(x$co2_emissions)

x[abs(p - 0.8) < 0.01]