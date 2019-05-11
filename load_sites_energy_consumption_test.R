library(data.table)
library(here)
library(readxl)




data_path <- "D:/DATA/r.martin/Desktop/Projet_intern/Projet_Poste_Immo"




load_sites_energy_consumption <- function(year) {
  
  sites <- load_consumption_sites_id(year)
  
  if (year == 2017) {
    
    
  # -------------------------------------
  data_path <- here("data", "poste_immo", "energie", "03 - Controle de coherence client.xlsx")
  electricity <- as.data.table(read_excel(data_path, range = "N12:V10147", skip = 0))
  
  electricity <- cbind(sites, electricity)
  electricity <- melt(electricity,
                      c("irsi_id", "irsi_name"),
                      variable.name = "use",
                      value.name = "consumption")
  electricity[, energy_vector := "electricity"]
  electricity <- electricity[!is.na(consumption)]
  
  # -------------------------------------
  gas <- as.data.table(read_excel(data_path, range = "X12:AF10147", skip = 0))
  
  gas <- cbind(sites, gas)
  gas <- melt(gas,
              c("irsi_id", "irsi_name"),
              variable.name = "use",
              value.name = "consumption")
  gas[, energy_vector := "gas"]
  gas <- gas[!is.na(consumption)]
  
  # -------------------------------------
  fuel <- as.data.table(read_excel(here("data", "poste_immo", "energie", "03 - Controle de coherence client.xlsx"),
                                   range = "AH12:AO10147", skip = 0))
  
  fuel <- cbind(sites, fuel)
  fuel <- melt(fuel,
               c("irsi_id", "irsi_name"),
               variable.name = "use",
               value.name = "consumption")
  fuel[, energy_vector := "fuel"]
  fuel <- fuel[!is.na(consumption)]
  
  # -------------------------------------
  district_heating <- as.data.table(read_excel(data_path, range = "AQ12:AY10147", skip = 0))
  
  district_heating <- cbind(sites, district_heating)
  district_heating <- melt(district_heating,
                           c("irsi_id", "irsi_name"),
                           variable.name = "use",
                           value.name = "consumption")
  district_heating[, energy_vector := "district_heating"]
  district_heating <- district_heating[!is.na(consumption)]
  
  # -------------------------------------
  propane <- as.data.table(read_excel(data_path, range = "BA12:BG10147", skip = 0))
  
  propane <- cbind(sites, propane)
  propane <- melt(propane,
                  c("irsi_id", "irsi_name"),
                  variable.name = "use",
                  value.name = "consumption")
  propane[, energy_vector := "propane"]
  propane <- propane[!is.na(consumption)]
  
  # -------------------------------------
  wood <- as.data.table(read_excel(data_path, range = "BS12:BU10147", skip = 0))
  
  wood <- cbind(sites, wood)
  wood <- melt(wood,
               c("irsi_id", "irsi_name"),
               variable.name = "use",
               value.name = "consumption")
  wood[, energy_vector := "wood"]
  wood <- wood[!is.na(consumption)]
  
  
  # -------------------------------------
  energy <- rbind(electricity, gas, fuel, propane, district_heating, wood)
  
  
}

  if (year == 2016) {
  
  # -------------------------------------
  data_path <- here("data", "poste_immo", "energie", "data_frame_2013_2018.xlsx")
  electricity <- as.data.table(read_excel(data_path, range = "O12:X11296", skip = 0, sheet = "2016"))
  
  electricity <- cbind(sites, electricity)
  electricity <- melt(electricity,
                      c("irsi_id", "irsi_name"),
                      variable.name = "use",
                      value.name = "consumption")
  electricity[, energy_vector := "electricity"]
  electricity <- electricity[!is.na(consumption)]
  
  # -------------------------------------
  gas <- as.data.table(read_excel(data_path, range = "Z12:AI11296", skip = 0, sheet = "2016"))
  
  gas <- cbind(sites, gas)
  gas <- melt(gas,
              c("irsi_id", "irsi_name"),
              variable.name = "use",
              value.name = "consumption")
  gas[, energy_vector := "gas"]
  gas <- gas[!is.na(consumption)]
  
  # -------------------------------------
  fuel <- as.data.table(read_excel(data_path, range = "AK12:AT11296", skip = 0, sheet = "2016"))
  
  fuel <- cbind(sites, fuel)
  fuel <- melt(fuel,
               c("irsi_id", "irsi_name"),
               variable.name = "use",
               value.name = "consumption")
  fuel[, energy_vector := "fuel"]
  fuel <- fuel[!is.na(consumption)]
  
  # -------------------------------------
  district_heating <- as.data.table(read_excel(data_path, range = "AV12:BE11296", skip = 0, sheet = "2016"))
  
  district_heating <- cbind(sites, district_heating)
  district_heating <- melt(district_heating,
                           c("irsi_id", "irsi_name"),
                           variable.name = "use",
                           value.name = "consumption")
  district_heating[, energy_vector := "district_heating"]
  district_heating <- district_heating[!is.na(consumption)]
  
  # -------------------------------------
  propane <- as.data.table(read_excel(data_path, range = "BN12:BQ11296", skip = 0, sheet = "2016"))
  
  propane <- cbind(sites, propane)
  propane <- melt(propane,
                  c("irsi_id", "irsi_name"),
                  variable.name = "use",
                  value.name = "consumption")
  propane[, energy_vector := "propane"]
  propane <- propane[!is.na(consumption)]
  

  # -------------------------------------
  energy <- rbind(electricity, gas, fuel, propane, district_heating)
  

  
  }
  
  
  if (year == 2015) {
    
    # -------------------------------------
    data_path <- here("data", "poste_immo", "energie", "Reporting environnemental 2015 par IRSI.xls")
    electricity <- as.data.table(read_excel(data_path, range = "AD1:AD10945", skip = 0))
    use <- as.data.table(read_excel(data_path, range = "H1:H10945", skip = 0))
    
    electricity <- cbind(sites,use,electricity)
    electricity[, energy_vector := "electricity"]
    setnames(electricity, c("irsi_id", "irsi_name", "use" ,"consumption","energy_vector"))
    electricity <- electricity[!is.na(consumption)]
    
    # -------------------------------------
    gaz <- as.data.table(read_excel(data_path, range = "AH1:AH10945", skip = 0))
    
    gaz <- cbind(sites,use, gaz)
    gaz[, energy_vector := "gaz"]
    setnames(gaz, c("irsi_id", "irsi_name", "use" ,"consumption","energy_vector"))
    gaz <- gaz[!is.na(consumption)]
    
    # -------------------------------------
    comb <- as.data.table(read_excel(data_path, range = "V1:V10945", skip = 0))
    
    comb <- cbind(sites,use, comb)
    comb[, energy_vector := "comb"]
    setnames(comb, c("irsi_id", "irsi_name", "use" ,"consumption","energy_vector"))
    comb <- comb[!is.na(consumption)]
    # -------------------------------------
    eau <- as.data.table(read_excel(data_path, range = "Z1:Z10945", skip = 0))
    
    eau <- cbind(sites,use, eau)
    eau[, energy_vector := "eau"]
    setnames(eau, c("irsi_id", "irsi_name", "use" ,"consumption","energy_vector"))
    eau <- eau[!is.na(consumption)]
    
    # -------------------------------------
    propane <- as.data.table(read_excel(data_path, range = "AL1:AL10945", skip = 0))
    
    propane <- cbind(sites,use, propane)
    propane[, energy_vector := "propane"]
    setnames(propane, c("irsi_id", "irsi_name", "use" ,"consumption","energy_vector"))
    propane <- propane[!is.na(consumption)]
    
    # -------------------------------------
    gas <- as.data.table(read_excel(data_path, range = "N1:N10945", skip = 0))
    
    gas <- cbind(sites,use, gas)
    gas[, energy_vector := "gas"]
    setnames(gas, c("irsi_id", "irsi_name", "use" ,"consumption","energy_vector"))
    gas <- gas[!is.na(consumption)]
    
    # -------------------------------------
    district_heating <- as.data.table(read_excel(data_path, range = "R1:R10945", skip = 0))
    
    district_heating <- cbind(sites,use, district_heating)
    district_heating[, energy_vector := "district_heating"]
    setnames(district_heating, c("irsi_id", "irsi_name", "use" ,"consumption","energy_vector"))
    district_heating <- district_heating[!is.na(consumption)]
    
    
    # -------------------------------------
    energy <- rbind(electricity, gaz, comb, propane, eau, propane, gas, district_heating)
    
    
    
  }
  
  if (year == 2014) {
    
    # -------------------------------------
    data_path <- here("data", "poste_immo", "energie", "Reporting environnemental 2014.xlsx")
    electricity <- as.data.table(read_excel(data_path, range = "W3:W11146", skip = 0))
    use <- as.data.table(read_excel(data_path, range = "I3:I11146", skip = 0))
    
    electricity <- cbind(sites,use,electricity)
    electricity[, energy_vector := "electricity"]
    setnames(electricity, c("irsi_id", "irsi_name", "use" ,"consumption","energy_vector"))
    electricity <- electricity[!is.na(consumption)]
    
    # -------------------------------------
    gaz <- as.data.table(read_excel(data_path, range = "AA3:AA11146", skip = 0))
    
    gaz <- cbind(sites,use, gaz)
    gaz[, energy_vector := "gaz"]
    setnames(gaz, c("irsi_id", "irsi_name", "use" ,"consumption","energy_vector"))
    gaz <- gaz[!is.na(consumption)]
    

    # -------------------------------------
    propane <- as.data.table(read_excel(data_path, range = "AE3:AE11146", skip = 0))
    
    propane <- cbind(sites,use, propane)
    propane[, energy_vector := "propane"]
    setnames(propane, c("irsi_id", "irsi_name", "use" ,"consumption","energy_vector"))
    propane <- propane[!is.na(consumption)]
    
    # -------------------------------------
    fuel <- as.data.table(read_excel(data_path, range = "S3:S11146", skip = 0))
    
    fuel <- cbind(sites,use, fuel)
    fuel[, energy_vector := "fuel"]
    setnames(fuel, c("irsi_id", "irsi_name", "use" ,"consumption","energy_vector"))
    fuel <- fuel[!is.na(consumption)]
    
    # -------------------------------------
    district_heating <- as.data.table(read_excel(data_path, range = "O3:O11146", skip = 0))
    
    district_heating <- cbind(sites,use, district_heating)
    district_heating[, energy_vector := "district_heating"]
    setnames(district_heating, c("irsi_id", "irsi_name", "use" ,"consumption","energy_vector"))
    district_heating <- district_heating[!is.na(consumption)]
    
    
    # -------------------------------------
    energy <- rbind(electricity, gaz, propane, fuel, district_heating)
    
    
    
  }
  
  if (year == 2013) {
    
    
    # -------------------------------------
    data_path <- here("data", "poste_immo", "energie", "Reporting environnemental Dec 2012-Nov 2013.xlsx")
    electricity <- as.data.table(read_excel(data_path, range = "M3:U11940", skip = 0))
    
    electricity <- cbind(sites, electricity)
    electricity <- melt(electricity,
                        c("irsi_id", "irsi_name"),
                        variable.name = "use",
                        value.name = "consumption")
    electricity[, energy_vector := "electricity"]
    electricity <- electricity[!is.na(consumption)]
    
    # -------------------------------------
    gaz <- as.data.table(read_excel(data_path, range = "AE3:AM11940", skip = 0))
    
    gaz <- cbind(sites, gaz)
    gaz <- melt(gaz,
                c("irsi_id", "irsi_name"),
                variable.name = "use",
                value.name = "consumption")
    gaz[, energy_vector := "gaz"]
    gaz <- gaz[!is.na(consumption)]
    
    # -------------------------------------
    district_heating <- as.data.table(read_excel(data_path, range = "AW3:BE11940", skip = 0))
    
    district_heating <- cbind(sites, district_heating)
    district_heating <- melt(district_heating,
                             c("irsi_id", "irsi_name"),
                             variable.name = "use",
                             value.name = "consumption")
    district_heating[, energy_vector := "district_heating"]
    district_heating <- district_heating[!is.na(consumption)]
    
    # -------------------------------------

    fuel <- as.data.table(read_excel(data_path, range = "BO3:BW11940", skip = 0))
    
    fuel <- cbind(sites, fuel)
    fuel <- melt(fuel,
                             c("irsi_id", "irsi_name"),
                             variable.name = "use",
                             value.name = "consumption")
    fuel[, energy_vector := "fuel"]
    fuel <- fuel[!is.na(consumption)]
    
    # -------------------------------------
    energy <- rbind(electricity, gaz, district_heating, propane, fuel)
    
    
  }
  
  #energy <- energy[consumption > 0]
  return(energy)
}
