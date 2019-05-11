library(data.table)
library(readxl)
library(stringr)
library(doBy)
library(here)
library(weather)
library(ggplot2)
library(quantreg)
library(RColorBrewer)
library(gridExtra)
library(grid)

source(here("code", "load_sites_info.R"), encoding = "UTF-8")
source(here("code", "interpolate_areas.R"), encoding = "UTF-8")
source(here("code", "load_sites_energy_consumption.R"), encoding = "UTF-8")

# Load sites info
sites_dep <- sites[, list(irsi_id, dep)]
sites_dep <- sites_dep[!duplicated(sites_dep)]

sites_use <- sites[, list(irsi_id, use)]
sites_use <- sites_use[!duplicated(sites_use)]

# Load areas of the sites and compute vacancy
areas <- interpolate_areas(here("data", "poste_immo", "patrimoine", "Vacance 12-2013 à 12-2017.xlsx"))
areas[, vacancy_share := 1-rented_area/total_site_area]

# Load energy consumption data for each year
energy_2017 <- load_sites_energy_consumption(2017, here("data", "poste_immo", "energie", "03 - Contrôle de cohérence client.xlsx"))
energy_2016 <- load_sites_energy_consumption(2016, here("data", "poste_immo", "energie", "Reporting environnemental 2016 par IRSI et par Branche.xlsx"))
energy_2015 <- load_sites_energy_consumption(2015, here("data", "poste_immo", "energie", "Reporting environnemental 2015 par IRSI.xls"))
energy_2014 <- load_sites_energy_consumption(2014, here("data", "poste_immo", "energie", "Reporting environnemental 2014.xlsx"))
energy_2013 <- load_sites_energy_consumption(2013, here("data", "poste_immo", "energie", "Reporting environnemental Dec 2012-Nov 2013.xlsx"))
energy <- rbind(energy_2017, energy_2016, energy_2015, energy_2014, energy_2013)



# Find each site's climate zone
stations <- as.data.table(read_excel(here("data", "elioth", "clim_zones.xlsx"), sheet = "stations"))
clim_zones <- as.data.table(read_excel(here("data", "elioth", "clim_zones.xlsx"), sheet = "dep"))
sites_dep <- merge(sites_dep, clim_zones, by = "dep")

# Load weather data for each climate zone
weather <- load_synop("D:/DATA/r.martin/Desktop/Projet_intern/weather/meteo",
                   station_id = unique(stations$station_id),
                   station_name = NULL,
                   date_from = as.Date("2013-01-01"),
                   date_to = as.Date("2017-12-31"),
                   interpolate_to_hourly = TRUE,
                   verbose = FALSE)


# Compute the heating and cooling degree days
weather[, hdd_18 := ifelse(air_temp_2m - 273.15 - 18 < 0, abs(air_temp_2m - 273.15 - 18), 0)]
weather[, cdd_18 := ifelse(air_temp_2m - 273.15 - 18 > 0, abs(air_temp_2m - 273.15 - 18), 0)]
weather <- weather[, list(hdd_18 = sum(hdd_18, na.rm = TRUE)/24,
               cdd_18 = sum(cdd_18, na.rm = TRUE)/24),
        by = list(station_id, year(time))]

weather <- merge(weather, stations, by ="station_id")
weather[, k_hdd_corr := hdd_18/hdd_18[year == 2013], by = station_id] # 
weather[is.na(k_hdd_corr) | hdd_18 < 100, k_hdd_corr := 1] # non utilisé

# Merge sites and weather data
weather_sites <- merge(sites_dep, weather[, list(clim_zone, year, hdd_18, cdd_18)], by = "clim_zone", allow.cartesian = TRUE)


# Merge all dataframes
kaya <- merge(areas, energy, by = c("irsi_id", "year"), all.X = TRUE)
kaya <- merge(kaya, weather_sites[, list(irsi_id, year, hdd_18, cdd_18)], by = c("irsi_id", "year"), all.X = TRUE)
kaya <- merge(kaya, sites_use, by = "irsi_id")


kaya[, consumption_ratio := consumption/total_site_area]



p <- ggplot(kaya[consumption_ratio < 300])
p <- p + geom_point(aes(x = vacancy_share, consumption_ratio))
p <- p + facet_wrap(~energy_vector)
p

p <- ggplot(kaya[consumption_ratio < 300])
p <- p + geom_point(aes(x = hdd_18, consumption_ratio), alpha = 0.2)
p <- p + facet_wrap(~energy_vector)
p



reg <- function(cons, vac, hdd, cdd) {
  
  hdd <- hdd + runif(length(hdd), -1, 1)
  cdd <- cdd + runif(length(hdd), -1, 1)
  
  # Apply quantile regression only if there is sufficient data
  if (length(cons) > 10) {
    
    # Regression only on hdd if there is no vacancy variations
    if (sd(vac) > 0.05) {
      coefs <- as.list(coef(rq(cons ~ vac + hdd + cdd)))
    } else {
      coefs <- as.list(coef(rq(cons ~ hdd + cdd)))
      coefs[["vac"]] <- 0
    }

  } else {
    
    coefs <- list(int = median(cons), vac = 0, hdd = 0, cdd = 0)
    
  }
  
  names(coefs) <- c("median_perf", "vac_coeff", "hdd_coeff", "cdd_coeff")
  
  return(coefs)
  
}


coefs <- kaya[, reg(consumption_ratio, vacancy_share, hdd_18, cdd_18), by = list(use, energy_vector)]

kaya <- merge(kaya, coefs, by = c("use", "energy_vector"))


kaya[, vacancy_effect_ratio := vacancy_share*vac_coeff]
kaya[, heating_effect_ratio := hdd_18*hdd_coeff]
kaya[, cooling_effect_ratio := cdd_18*cdd_coeff]
kaya[, other_effect_ratio := consumption_ratio - median_perf - vacancy_effect_ratio - heating_effect_ratio - cooling_effect_ratio]


kaya[, vacancy_effect := total_site_area*vacancy_effect_ratio]
kaya[, heating_effect := total_site_area*heating_effect_ratio]
kaya[, cooling_effect := total_site_area*cooling_effect_ratio]
kaya[, other_effect := total_site_area*other_effect_ratio]


kaya[, d_vacancy := c(NA, diff(vacancy_effect)), by = list(irsi_id, energy_vector)]
kaya[, d_heating := c(NA, diff(heating_effect)), by = list(irsi_id, energy_vector)]
kaya[, d_cooling := c(NA, diff(cooling_effect)), by = list(irsi_id, energy_vector)]
kaya[, d_other := c(NA, diff(other_effect)), by = list(irsi_id, energy_vector)]


sites_years <- sites[, list(min_year = min(year), max_year = max(year)), by = irsi_id]

kaya <- merge(kaya, sites_years, by = "irsi_id")


x <- kaya[, list(d_vacancy_pos = sum(d_vacancy[d_vacancy > 0], na.rm = TRUE),
                 d_vacancy_neg = sum(d_vacancy[d_vacancy < 0], na.rm = TRUE),
                 d_heating_pos = sum(d_heating[d_heating > 0], na.rm = TRUE),
                 d_heating_neg = sum(d_heating[d_heating < 0], na.rm = TRUE),
                 d_cooling_pos = sum(d_cooling[d_cooling > 0], na.rm = TRUE),
                 d_cooling_neg = sum(d_cooling[d_cooling < 0], na.rm = TRUE),
                 d_other_pos = sum(d_other[d_other > 0], na.rm = TRUE),
                 d_other_neg = sum(d_other[d_other < 0], na.rm = TRUE)),
          by = list(year, energy_vector)]

x <- melt(x, c("year", "energy_vector"))



x_new <- kaya[year == min_year & min_year > 2013, list(d_new_sites = sum(consumption)), by = list(energy_vector, year)]
x_new <- melt(x_new, c("year", "energy_vector"))

x_old <- kaya[year == max_year & max_year < 2017, list(d_exit_sites = -sum(consumption)), by = list(energy_vector, year)]
x_old[, year := year + 1]
x_old <- melt(x_old, c("year", "energy_vector"))

x <- rbind(x, x_old, x_new)


p <- ggplot(x)
p <- p + geom_bar(aes(x = year, y = value, fill = energy_vector), stat = "identity")
p <- p + facet_wrap(~variable)
p

p <- ggplot(x)
p <- p + geom_bar(aes(x = year, y = value, fill = variable), stat = "identity")
p <- p + facet_wrap(~energy_vector)
p

p <- ggplot(x)
p <- p + geom_bar(aes(x = year, y = value, fill = variable), stat = "identity")
p


#Carbone and Rates
kaya <- kaya[!kaya$irsi_id=="270671"]
kaya <- kaya[!kaya$irsi_id=="290117"]

kaya<-kaya[!kaya$carbone_m²=="NA",]

carbone_indice <- read_excel("data/elioth/hyps.xlsx", 
                   sheet = "energy_ef")

names(carbone_indice)=c("energy_vector","indice")

kaya <- merge(kaya,carbone, by="energy_vector")

kaya[, carbone:= consumption*indice]
kaya[, carbone_m²:= carbone/total_site_area]

kaya[carbone_m² < 1, carbone_m² := NA]
kaya[carbone_m² > 500, carbone_m² := NA]


#carbone
d_f <- data.frame(use = kaya$use,
                  val = kaya$carbone)
summary <- summaryBy(val ~ use, d_f, FUN = function(x) {return(c(mean(x), sd(x), length(x)))})
names(summary)=c("use","mean","sd","length")
summary <- summary[order(summary$mean),]
summary$use <- factor(summary$use, levels = summary$use[order(summary$mean)])

data=data.frame(group=summary$use , value=summary$mean , number_of_obs=summary$length)

# Calculate the future positions on the x axis of each bar (left border, central position, right border)
data$right=cumsum(data$number_of_obs) + 30*c(0:(nrow(data)-1))
data$left=data$right - data$number_of_obs 

# Plot
ggplot(data, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, ymax = value, colour = group, fill = group)) +
  xlab("number of obs") + ylab("value")




#Ajout (locatif,patrimoine,taille)

vacancee <- data.frame(juridique=Vacance_12_2013$`VACANCE PAR SITE : M² - AU 1ER DÉCEMBRE 2013`,
                       X__3=Vacance_12_2013$X__3)

for (j in 2013:2017) {
  print(j)
  assign(paste0 ("Vacance_12_", j),read_excel("data/poste_immo/energie/Vacance 12-2013 a 12-2017.xlsx",
                                              sheet = paste0("Vacance par site 12-", j)))
    assign(paste0 ("Vacance_12_", j),get(paste0("Vacance_12_",j))[,c(1,6,11)])
  assign(paste0 ("Vacance_12_", j),get(paste0("Vacance_12_",j))[-(1:3),])                                     
  #assign(get(paste0 ("Vacance_12_",j)[,c("year")],j))
  Vacance_12_2013$year <- 2013
  Vacance_12_2014$year <- 2014
  Vacance_12_2015$year <- 2015
  Vacance_12_2016$year <- 2016
  Vacance_12_2017$year <- 2017
  #assign(names(get(paste0("Vacance_12_",j))),c("juridiction","irsi_id","taille","year"))
  names(Vacance_12_2013)=c("juridiction","irsi_id","taille","year")
  names(Vacance_12_2014)=c("juridiction","irsi_id","taille","year")
  names(Vacance_12_2015)=c("juridiction","irsi_id","taille","year")
  names(Vacance_12_2016)=c("juridiction","irsi_id","taille","year")
  names(Vacance_12_2017)=c("juridiction","irsi_id","taille","year")
  

}
vacance <- rbind(Vacance_12_2013,Vacance_12_2014,Vacance_12_2015,Vacance_12_2016,Vacance_12_2017)

vacance$taille[vacance$taille=="5 - Chef lieu de département"] <- "6 - Unité urbaine > 50 000 Hab."
vacance$taille[vacance$taille=="4 - Grande métropole"] <- "6 - Unité urbaine > 50 000 Hab."
vacance$taille[vacance$taille=="3 - 2ème couronne (Ile de France)"] <- "6 - Unité urbaine > 50 000 Hab."
vacance$taille[vacance$taille=="2 - 1ère couronne (Ile de France)"] <- "6 - Unité urbaine > 50 000 Hab."
vacance$taille[vacance$taille=="13 - Paris ardt 1/2/5/6/7/8/9/16/17"] <- "6 - Unité urbaine > 50 000 Hab."
vacance$taille[vacance$taille=="12 - Paris : arrdt 4/12/13/14/15"] <- "6 - Unité urbaine > 50 000 Hab."
vacance$taille[vacance$taille=="11 - Paris : arrdt 3/10/11/18/19/20"] <- "6 - Unité urbaine > 50 000 Hab."
vacance$taille[vacance$taille=="1 - Paris"] <- "6 - Unité urbaine > 50 000 Hab."

vacance$taille[vacance$taille=="7 - Unité urb. > 20 000 et < 50 M"] <- "8 - Unité urb. de 5 000 à 50 000"
vacance$taille[vacance$taille=="8 - Unité urb. de 5 000 à 20 000"] <- "8 - Unité urb. de 5 000 à 50 000"
vacance$taille[vacance$taille=="6 - Unité urbaine > 50 000 Hab."] <- "6 - Unité urb. > 50 000 Hab."

kaya <- merge(kaya,vacance, by=c("irsi_id","year"))

kaya$taille <- str_sub(kaya$taille,15)

#carbone_m² Patrimonial & Locatif


for (i in 2013:2017) {
  print(i)

  #assign(paste0 ("kaya_patrimoine_", i), kaya[vacancy_share>0 & year==i])
  #assign(paste0 ("kaya_locatif_", i), kaya[vacancy_share==0 & year==i])
  
  d_patri <- data.frame(use = kaya[juridiction=="Patrimonial" & year==i]$use,
                        val = kaya[juridiction=="Patrimonial" & year==i]$carbone_m²) 
  d_loca  <- data.frame(use = kaya[juridiction=="Locatif" & year==i]$use,
                        val = kaya[juridiction=="Locatif" & year==i]$carbone_m²)

summary  <- summaryBy(val ~ use, d_patri, FUN = function(x) {return(c(mean(x), sd(x), length(x)))})
summary1 <- summaryBy(val ~ use, d_loca, FUN = function(x) {return(c(mean(x), sd(x), length(x)))})

names(summary) =c("use","mean","sd","length")
names(summary1)=c("use","mean","sd","length")

summary  <- summary[order(summary$mean),]
summary1 <- summary1[order(summary1$mean),]

summary$use  <- factor(summary$use, levels = summary$use[order(summary$mean)])
summary1$use <- factor(summary1$use, levels = summary1$use[order(summary1$mean)])

assign(paste0 ("data_patri", i),data.frame(group=summary$use , value=summary$mean , number_of_obs=summary$length))
assign(paste0 ("data_loca", i),data.frame(group=summary1$use , value=summary1$mean , number_of_obs=summary1$length))


}


data_p <- data.frame(use = kaya[year==2017 & juridiction=="Patrimonial"]$use,
                     value = kaya[year==2017 & juridiction=="Patrimonial"]$carbone_m²)
data_l <- data.frame(use = kaya[year==2017 & juridiction=="Locatif"]$use,
                     value = kaya[year==2017 & juridiction=="Locatif"]$carbone_m²)
for (j in 2013:2016) {
  print(j)
  
  data_p <- merge(data_p,get(paste0 ("data_patri", j+1)), by="group", all=T)
  data_l <- merge(data_l,get(paste0 ("data_loca", j+1)), by="group", all=T)
  
}

data_p <- data_p[,c(1,2,3,5,7,9)]
data_l <- data_l[,c(1,2,3,5,7,9)]

names(data_p)=c("group","2013","2014","2015","2016","2017")
names(data_l)=c("group","2013","2014","2015","2016","2017") 

data_pp<-data_p[,c("group","2017")]
names(data_pp)=c("group","Patrimonial")
dp <- melt(data_pp)

data_ll<-data_l[,c("group","2017")]
names(data_ll)=c("group","Locatif")
dl <- melt(data_ll)

dd<-rbind(dl,dp)

x11()
data__p<-melt(addd)
qplot(addd$year, value, data = addd, colour = addd$use, group=addd$use) +geom_line(size=2) +labs(y=NULL, x="Evolution des émissions de carbone KgCO2/m² du locatif") + geom_line()+ theme_minimal() + labs(fill = "", x = "", y = "Emissions [kgCO2e/m²]\n",
                                                                                                                                                                                                 title = "Locatif 2017",
                                                                                                                                                                                                 subtitle = "")
#ggsave(plot = p, file = here("results", "patrimoine_carbone_m².png"), width = 4, height = 4, type = "cairo-png")

x11()
data__l<-melt(data_l)
qplot(data__l$variable, value, data = data__l, colour = group, group=group) +geom_line(size=2) +labs(y=NULL, x="Evolution des émissions de carbone KgCO2/m² du locatif") + geom_line()+ theme_minimal() + labs(fill = "", x = "", y = "Emissions [kgCO2e/m²]\n",
                                                                                                                                                                                              title = "Locatif 2017",
                                                                                                                                                                                              subtitle = "")
#ggsave(plot = p, file = here("results", "locatif_carbone_m².png"), width = 4, height = 4, type = "cairo-png")


dd <- data.table(group = kaya[year==2017]$use,
                 juridiction = kaya[year==2017]$juridiction,
                 carbone = kaya[year==2017]$carbone)
dd[juridiction =="Mixte", := NA]
dd<-dd[!dd$juridiction=="NA"]

x11()
p <- ggplot(dd)
p <- p + geom_bar(aes(x = juridiction, y = carbone/1000, fill = group), 
                  stat = "identity", width = 0.5)
p <- p + scale_fill_manual(values = brewer.pal(11, "Set3"))
p <- p + labs(fill = "", x = "", y = "Emissions [tCO2e]\n",
              title = "Emissions 2017",
              subtitle = "")
p <- p + scale_y_continuous(limits = c(0, 100000), breaks = seq(0, 100000, 15000), expand = c(0, 0))
p <- p + theme_minimal()
p <- p + theme(axis.text.x = element_text(margin = margin(t = 10)))
p

ggsave(plot = p, file = here("results", "emissions_locatif_patrimoine_brute.png"), width = 4, height = 4, type = "cairo-png")





erreur_Habitation  <- data.frame( id =       kaya[juridiction=="Locatif" & year==2015 & use=="Habitation"]$irsi_id,
                                  use=       kaya[juridiction=="Locatif" & year==2015 & use=="Habitation"]$use,
                                  conso =    kaya[juridiction=="Locatif" & year==2015 & use=="Habitation"]$consumption,
                                  m2 =       kaya[juridiction=="Locatif" & year==2015 & use=="Habitation"]$total_site_area,
                                  carbone_m2=kaya[juridiction=="Locatif" & year==2015 & use=="Habitation"]$carbone_m²)



pie_loca <- data.frame(carbone = kaya[juridiction=="Locatif" & year==2017]$carbone,
                       group   = kaya[juridiction=="Locatif" & year==2017]$taille)

pie_patri <- data.frame(carbone = kaya[juridiction=="Patrimonial" & year==2017]$carbone,
                        group   = kaya[juridiction=="Patrimonial" & year==2017]$taille)

#pie_patri$group <- factor(pie_patri$group, levels = c(" > 50 000 Hab.","de 5 000 à 50 000","Sociale","< 5 000 Habitants"))


summary <- summaryBy(carbone ~ group, pie_loca, FUN = function(x) {return(c(sum(x), sd(x), length(x)))})
names(summary)=c("use","sum","sd","length")
summary <- summary[order(summary$sum),]

data=data.frame(group=summary$use , value=summary$sum/1000 , number_of_obs=summary$length)
data$right=cumsum(data$number_of_obs) + 30*c(0:(nrow(data)-1))
data$left=data$right - data$number_of_obs 
data1 <- data

p <- ggplot(data, aes(ymin = 0)) 
p <- p + geom_rect(aes(xmin = left, xmax = right, ymax = value, colour = group, fill = group)) 
p <- p + xlab("number of obs") + ylab("value")
p <- p + labs(fill = "", x = "Nombre de sites", y = "Emissions [tCO2e]\n",
              title = "Emissions brute locatif 2017",
              subtitle = "")
p

ggsave(plot = p, file = here("results", "emissions_brute_locatif_hab.png"), width = 5, height = 5, type = "cairo-png")

summary <- summaryBy(carbone ~ pie$groupe....kaya.year....2017..taille, pie, FUN = function(x) {return(c(sum(x), sd(x), length(x)))})
names(summary)=c("use","sum","sd","length")
summary <- summary[order(summary$sum),]

data=data.frame(group=summary$use , value=summary$sum/1000 , number_of_obs=summary$length)
data$right=cumsum(data$number_of_obs) + 30*c(0:(nrow(data)-1))
data$left=data$right - data$number_of_obs 


p <- ggplot(data, aes(ymin = 0)) 
p <- p + geom_rect(aes(xmin = left, xmax = right, ymax = value, colour = group, fill = group)) 
p <- p + xlab("number of obs") + ylab("value")
p <- p + labs(fill = "", x = "Nombre de site", y = "Emissions [tCO2e]\n",
              title = "Emissions brute patrimonial 2017",
              subtitle = "")
p

ggsave(plot = p, file = here("results", "emissions_brute_habb.png"), width = 5, height = 5, type = "cairo-png")



#PIE loca patri
df <- data.frame(Juridiction = c("Locatif", "Patrimonial"), 
                     values = c(0.428, 0.572))
df$Juridiction <- factor(df$Juridiction, levels = rev(df$Juridiction))


p <- ggplot(data = df, mapping = aes(x = factor(1), y = values, fill = Juridiction)) +
  geom_bar(width=1, stat = "identity") +
  coord_polar(theta = "y") + 
  scale_fill_brewer(type = "seq",direction = -1, guide = F) +
  geom_text(aes(x = c(1.1, 1.1), 
                y = values/2 + c(0, cumsum(values)[-length(values)]), 
                label=paste(Juridiction,"\n",values*100, "%")), family = "Consolas") +
  labs(fill = "", x = "", y = "",
       title = "Part des emissions brute 2017",
       subtitle = "")

ggsave(plot = p, file = here("results", "part_emissions_brute.png"), width = 5, height = 5, type = "cairo-png")


#PIE hab

df <- data.frame(Juridiction = c("< 5 000 Hab.", "de 5 000 à 50 000","> 50 000 Hab."), 
                 values = c(0.135, 0.253, 0.621))
df$Juridiction <- factor(df$Juridiction, levels = rev(df$Juridiction))


p <- ggplot(data = df, mapping = aes(x = factor(1), y = values, fill = Juridiction)) +
  geom_bar(width=1, stat = "identity") +
  coord_polar(theta = "y") + 
  scale_fill_brewer(type = "seq",direction = -1, guide = F) +
  geom_text(aes(x = c(1.3,1.3,1), 
                y = values/3 + c(0, cumsum(values)[-length(values)]), 
                label=paste(Juridiction,"\n",values*100, "%")), family = "Consolas") +
  labs(fill = "", x = "", y = "",
       title = "Part des emissions brute 2017",
       subtitle = "")

ggsave(plot = p, file = here("results", "part_emissions_brute_hab.png"), width = 5, height = 5, type = "cairo-png")

#Emissions m² Hab

df <- data.frame(carbone = kaya[year==2017]$carbone_m²,
                 group   = kaya[year==2017]$taille,
                 carbone_b=kaya[year==2017]$carbone,
                 surface = kaya[year==2017]$total_site_area,
                 id = kaya[year==2017]$irsi_id)

summary <- summaryBy(carbone ~ group, df, FUN = function(x) {return(c(mean(x), sd(x), length(x)))})
names(summary)=c("use","sum","sd","length")
summary <- summary[order(summary$sum),]

data=data.frame(group=summary$use , value=summary$sum , number_of_obs=summary$length)
data$right=cumsum(data$number_of_obs) + 30*c(0:(nrow(data)-1))
data$left=data$right - data$number_of_obs 

p <- ggplot(data, aes(ymin = 0)) 
p <- p + geom_rect(aes(xmin = left, xmax = right, ymax = value, colour = group, fill = group)) 
p <- p + xlab("number of obs") + ylab("value")
p <- p + labs(fill = "", x = "Nombre de site", y = "Emissions [tCO2e]\n",
              title = "Emissions brute patrimonial 2017",
              subtitle = "")
p

new_df <- aggregate(as.numeric(consumption_ratio) ~ year, kaya, mean)
new_df
new_1 <- aggregate(as.numeric(carbone_m²) ~ year, kaya, mean)
new_1
new_2 <- aggregate(as.numeric(carbone) ~ year, kaya, sum)
new_2
new_3 <- aggregate(as.numeric(carbone_m²) ~ taille, kaya, mean)
new_3

neww
tab <- tableGrob(neww)
p1 <- dualplot(x1 = neww$Année, y1 = neww$`Intensité énergétique (kWh/m²)`, x2= neww$Année, y2 =neww$`Intensité carbone (KgCO2/m²)`,
         ylab1 = "Intensité Energétique",
         ylab2 = "Intensité Carbone",
         lwd=c(4,4),
         legx = "topright",
         main = "Evolution des intensitées carbone et énergétique")
ggsave(plot = p1, file = here("results", "dualplot.png"), width = 5, height = 5, type = "cairo-png")

grid.arrange(
  tab,
  ncol = 2,
  widths = c(12, 0),
  clip = FALSE
)
ddd<- kaya[year==2017]
ddd <- data.frame(use=kaya[year==2017]$use,
                  juridiction=kaya[year==2017]$juridiction,
                  carbone_m²=kaya[year==2017]$carbone_m²)
ddd<-ddd[!ddd$carbone_m²=="NA",]
ddd <- aggregate(as.numeric(ddd$carbone_m²),by=list( typ1=ddd$use,typ2=ddd$year), mean)



kayaloca<-kaya[juridiction=="Locatif"]
kayaloca<-kayaloca[!kayaloca$carbone_m²=="NA"]
addd_loca <- aggregate(as.numeric(kayaloca$carbone_m²),by=list( typ1=kayaloca$use,typ2=kayaloca$year), mean)
names(addd_loca)=c("use","year","value")

x11()
qplot(addd_loca$year, value, data = addd_loca, colour = addd_loca$use, group=addd_loca$use) +geom_line(size=2) +labs(y=NULL, x="Evolution des émissions de carbone KgCO2/m² du locatif") + scale_y_continuous(limits = c(0, 60))+ theme_minimal() + labs(fill = "", x = "", y = "Emissions [kgCO2e/m²]\n",
                                                                                                                                                                                                           title = "Locatif 2017",
                                                                                                                                                                                                           subtitle = "")



kayapatri<-kaya[juridiction=="Patrimonial"]
kayapatri<-kayapatri[!kayapatri$carbone_m²=="NA"]
addd_patri <- aggregate(as.numeric(kayapatri$carbone_m²),by=list( typ1=kayapatri$use,typ2=kayapatri$year), mean)
names(addd_patri)=c("use","year","value")

x11()
qplot(addd_patri$year, value, data = addd_patri, colour = addd_patri$use, group=addd_patri$use) +geom_line(size=2) +labs(y=NULL, x="Evolution des émissions de carbone KgCO2/m² du patrimoine") + scale_y_continuous(limits = c(0, 60))+ theme_minimal() + labs(fill = "", x = "", y = "Emissions [kgCO2e/m²]\n",
                                                                                                                                                                                                                                                         title = "Patrimonial 2017",
                                                                                                                                                                                                                                                         subtitle = "")



x11()
p <- ggplot(ddd)
p <- p + geom_bar(aes(x = type, y = carbone_m², fill = use), 
                  stat = "identity", width = 0.5)
p <- p + scale_fill_manual(values = brewer.pal(11, "Set3"))
p <- p + labs(fill = "", x = "", y = "Emissions [kgCO2e/m²]\n",
              title = "Emissions 2017",
              subtitle = "")
p <- p + scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 50), expand = c(0, 0))
p <- p + theme_minimal()
p <- p + theme(axis.text.x = element_text(margin = margin(t = 10)))
p

ggsave(plot = p, file = here("results", "emissions_locatif_patrimoine.png"), width = 4, height = 4, type = "cairo-png")

data_p <- data.frame(use = kaya[year==2017 & juridiction=="Patrimonial"]$use,
                     value = kaya[year==2017 & juridiction=="Patrimonial"]$carbone_m²)
data_l <- data.frame(use = kaya[year==2017 & juridiction=="Locatif"]$use,
                     value = kaya[year==2017 & juridiction=="Locatif"]$carbone_m²)

data_p <- aggregate(as.numeric(data_p$value) ~ use, data_p, mean)
data_l <- aggregate(as.numeric(data_l$value) ~ use, data_l, mean)

names(data_l)=c("use","value","type")
names(data_p)=c("use","value","type")
apal <- rbind(data_p,data_l)

x11()
p <- ggplot(apal)
p <- p + geom_bar(aes(x = use, y = value, fill = type),position=position_dodge(), 
                  stat = "identity", width = 0.5)
p <- p + labs(fill = "", x = "", y = "Emissions [kgCO2e/m²]\n",
              title = "Emissions 2017",
              subtitle = "")
p <- p + scale_alpha_continuous((limits = c(0, 40))
p <- p + scale_fill_manual(values=c('#999999','#E69F00'))
p <- p + theme_minimal()
p

ggsave(plot = p, file = here("results", "emissions_locatif_patrimoine3.png"), width = 10, height = 4, type = "cairo-png")


#PIE hab

df <- data.frame(Juridiction = c("s","a","x"), 
                 values = c(0.55, 0.30, 0.15))


p <- ggplot(data = df, mapping = aes(x = factor(1), y = values, fill = Juridiction)) +
  geom_bar(width=1, stat = "identity") +
  coord_polar(theta = "y") + 
  scale_fill_brewer(type = "seq",direction = -1, guide = F, palette = "Reds") +
  geom_text(aes(x = c(1,1,1), 
                y = values/3 + c(0, cumsum(values)[-length(values)]), 
                label=paste(Juridiction,"\n",values*100, "%")), family = "Consolas") +
  labs(fill = "", x = "", y = "",
       title = "Rénovations lourdes",
       subtitle = "")
p
ggsave(plot = p, file = here("results", "part_emissions_m²_priorite1.png"), width = 5, height = 5, type = "cairo-png")


data=data.frame(group=c("Diffus","Ville moyenne","Pôle urbain") , value=c(11000,27000,65000) , value2=c(3100,9500,28000), value3=c(7500,5000,8000), number_of_obs=c(5400,3200,6000))
data$right=cumsum(data$number_of_obs) + 30*c(0:(nrow(data)-1))
data$left=data$right - data$number_of_obs 

# dataframe with value
data1 <- data[-2:-3]
data1$ymin <- 0
data1$ymax <- data$value
data1$group <- paste(data$group, '15-65 KgCO2/m²') # same group but with value2

# dataframe with value2
data2 <- data[-2:-3]
data2$ymin <- data$value
data2$ymax <- data$value + data$value2 
data2$group <- paste(data$group, '< 15 KgCO2/m²') # same group but with value2

# dataframe with value2
data3 <- data[-2:-3]
data3$ymin <- data$value
data3$ymax <- data$value + data$value3 
data3$group <- paste(data$group, '> 65 KgCO2/m²') # same group but with value2

# combine
data <- rbind(data1, data2, data3)
data$variable <-c("A","A","A","B","B","B","C","C","C")
# plot
x11()
p <- ggplot(data)
p <- p + geom_rect(aes(xmin = left, xmax = right, ymin=ymin, ymax = ymax, colour = group, fill =variable)) 
p <- p + xlab("number of obs") + ylab("value") 
p <- p + labs(fill = "", x = "", y = "Emissions [tCO2e]\n",
              title = "Emissions 2017",
              subtitle = "")
p <- p + scale_y_continuous(limits = c(0, 100000), breaks = seq(0, 100000, 25000), expand = c(0, 0))
p <- p + theme_minimal()
p <- p + theme(axis.text.x = element_text(margin = margin(t = 10)))
p
ggsave(plot = p, file = here("results", "emissions_locatif_patrimoine_bruteeer.png"), width = 4, height = 4, type = "cairo-png")

