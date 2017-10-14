elements <- readr::read_csv("https://raw.githubusercontent.com/andrejewski/periodic-table/master/data.csv")

names(elements) <- c("atomic_number", "symbol", "name", "atomic_mass", "cpk_hex_color", "electronic_configuration", "electronegativity", "atomic_radius", "ion_radius", "vandelwaals_radius", "ionization_energy", "electron_affinity", "oxidation_states", "standard_state", "bonding_type", "melting_point", "boiling_point", "density", "group_block", "year_discovered")

rowcol <- readr::read_csv("symbol,period,group
H,1,1
He,1,18
Li,2,1
Be,2,2
B,2,13
C,2,14
N,2,15
O,2,16
F,2,17
Ne,2,18
Na,3,1
Mg,3,2
Al,3,13
Si,3,14
P,3,15
S,3,16
Cl,3,17
Ar,3,18
K,4,1
Ca,4,2
Sc,4,3
Ti,4,4
V,4,5
Cr,4,6
Mn,4,7
Fe,4,8
Co,4,9
Ni,4,10
Cu,4,11
Zn,4,12
Ga,4,13
Ge,4,14
As,4,15
Se,4,16
Br,4,17
Kr,4,18
Rb,5,1
Sr,5,2
Y,5,3
Zr,5,4
Nb,5,5
Mo,5,6
Tc,5,7
Ru,5,8
Rh,5,9
Pd,5,10
Ag,5,11
Cd,5,12
In,5,13
Sn,5,14
Sb,5,15
Te,5,16
I,5,17
Xe,5,18
Cs,6,1
Ba,6,2
Hf,6,4
Ta,6,5
W,6,6
Re,6,7
Os,6,8
Ir,6,9
Pt,6,10
Au,6,11
Hg,6,12
Tl,6,13
Pb,6,14
Bi,6,15
Po,6,16
At,6,17
Rn,6,18
Fr,7,1
Ra,7,2
Rf,7,4
Db,7,5
Sg,7,6
Bh,7,7
Hs,7,8
Mt,7,9
Ds,7,10
Rg,7,11
Cn,7,12
Nh,7,13
Fl,7,14
Mc,7,15
Lv,7,16
Ts,7,17
Og,7,18
")



library(dplyr)

a <- left_join(elements, rowcol)
a %>%
  select(symbol, atomic_number, period, group) %>%
  filter(is.na(period)) %>%
  data.frame()

a$period[a$atomic_number %in% c(57:71)] <- "  "
a$group[a$atomic_number %in% c(57:71)] <- 4:18

a$period[a$atomic_number %in% c(89:103)] <- "   "
a$group[a$atomic_number %in% c(89:103)] <- 4:18

a %>%
  select(symbol, atomic_number, period, group) %>%
  filter(is.na(period)) %>%
  data.frame()

a$atomic_number_p <- as.character(a$atomic_number)

new_dat <- data_frame(
  atomic_number_p = c("57-71", "89-103"),
  name = c("Lanthanides", "Actinides"),
  period = c("6", "7"),
  group = c(3, 3),
  group_block = c("lanthanoid", "actinoid"),
  atomic_mass = c("", ""),
  symbol = c("", "")
)

a <- full_join(a, new_dat)

idx <- a$atomic_number %in% c(57:71, 89:103)

a$group_offset <- rep(0, nrow(a))
a$group_offset[idx] <- -0.5
a$sym_offset <- rep(-0.4, nrow(a))
a$sym_offset[idx] <- -0.9

a$symbol[a$name == "Lanthanides"] <- "\u2193"
a$symbol[a$name == "Actinides"] <- "\u2193"

elements <- a
use_data(elements, overwrite = TRUE)

# a$periody <- as.character(a$period)
# a$massy <- paste0(a$period, ":0.15")
# a$namey <- paste0(a$period, ":0.3")
# a$numbery <- paste0(a$period, ":0.8")

# a$periody <- as.character(a$period)
# a$massy <- paste0(a$period, ":0.15")
# a$namey <- paste0(a$period, ":0.3")
# a$numbery <- paste0(a$period, ":0.8")
# a$periody[idx] <- paste0(a$periody[idx], ":0.2")
# a$numbery[idx] <- paste0(a$period[idx], ":0.5")
# a$massy[idx] <- paste0(a$period[idx], ":-0.15")
# a$namey[idx] <- paste0(a$period[idx], ":0.0")


figure(title = "Periodic Table", data = elements,
  xgrid = FALSE, ygrid = FALSE, xlab = "", ylab = "",
  xlim = as.character(1:18), ylim = c("   ", "  ", " ", as.character(7:1)),
  height = 700, width = 1200) %>%
  ly_crect(ctg(group, group_offset), period, 0.9, 0.9,
    color = group_block, fill_alpha = 0.6,
    hover = list(name, atomic_number, group_block, atomic_mass,
      electronic_configuration)) %>%
  ly_text(ctg(group, sym_offset), period, text = symbol,
    font_style = "bold", font_size = "15pt",
    align = "left", baseline = "middle") %>%
  ly_text(ctg(group, sym_offset), ctg(period, 0.3), text = atomic_number_p,
    font_size = "9pt", align = "left", baseline = "middle") %>%
  ly_text(ctg(group, sym_offset), ctg(period, -0.2), text = name,
    font_size = "6pt",
    align = "left", baseline = "middle") %>%
  ly_text(ctg(group, sym_offset), ctg(period, -0.35), text = atomic_mass,
    font_size = "6pt", align = "left", baseline = "middle") %>%
  x_axis(axis = axis_spec(visible = FALSE)) %>%
  y_axis(axis = axis_spec(visible = FALSE))

