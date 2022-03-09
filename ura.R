# kirjastot
library(data.table)
library(lubridate)
library(ggplot2)
library(gridExtra)



# päivämäärät
ty.alku <- as.Date("2011-09-01")
ty.loppu <- as.Date("2015-12-31")
fourp1.alku <- as.Date("2016-01-01")
fourp1.loppu <- as.Date("2017-12-10")
atp.alku <- as.Date("2017-12-11")
atp.loppu <- as.Date("2020-10-31")
fourp2.alku <- as.Date("2020-11-01")
fourp2.loppu <- today()

# irtisanomisaika
irt.aika <- 14



# kokoa data.tableksi
ura <- rbind(data.table(date = seq(ty.alku, ty.loppu, 1), workplace = "Turun yliopisto Biostatistiikka"),
             data.table(date = seq(fourp1.alku, fourp1.loppu, 1), workplace = "4Pharma"),
             data.table(date = seq(atp.alku, atp.loppu, 1), workplace = "VSSHP Auria tietopalvelu"),
             data.table(date = seq(fourp2.alku, fourp2.loppu + irt.aika, 1), workplace = "4Pharma"))

# numeroi vuoden ja kuukauden mukaan viikkonumerot
ura[, w := rleid(isoweek(date)), list(year(date), month(date))]

# laske työsuhteen kesto
ura[, workplace := paste(workplace, paste0("(", sprintf("%2.1f", interval(min(date), max(date))/years(1)), "v)")), list(rleid(workplace), workplace)]

# merkitse viikonloput
ura[wday(date, week_start = 1) %in% 6:7, workplace := "Viikonloppu"]



# anna värit ero työpaikoille + viikonlopuille
colmap <- colmap.names <- unique(ura$workplace)
colmap[grepl("Auria", colmap, T)] <- "#56B4E9CC"
colmap[grepl("Bio", colmap, T)] <- "#E69F00CC"
colmap[grepl("4P", colmap, T)] <- "#4E79A7"
colmap[grepl("Viikonloppu", colmap, T)] <- "#CCCCCC"
names(colmap) <- colmap.names



# piirrä kuva
ggplot(ura, aes(x = w, y = wday(date, week_start = 1, label = T))) +
  # piirrä laatikko per viikonpäivä per viikko
  geom_tile(aes(fill = workplace, width = 0.9, height = 0.9), size = 1) +
  # käännä y-akselin suunta
  scale_y_discrete(limits = rev) +
  # muunna x-akseli
  scale_x_discrete() +
  # muuta väritys halutunlaiseksi
  scale_fill_manual(values = colmap) +
  # jaa viikonpäivä x viikko -visualisaatio vuoden ja kuukauden mukaiseksi matriisiksi
  facet_grid(year(date) ~ month(date, label = T), scales="free", space="free", switch = 'x') +
  # aseta selitteet
  labs(x = "Vko", y = "") +
  # hienosäädä teema
  theme_bw(base_size = 10) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        legend.justification = "right",
        legend.direction = "horizontal",
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.x = unit(0.2, "cm"))


ggsave("ura.png", device = "png", width = 500, height = 500, units = "px", dpi = 180, scale = 3.9)
