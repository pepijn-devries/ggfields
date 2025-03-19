library(ggplot2)
library(ggfields)
library(basemaps)
library(ggspatial)

data(seawatervelocity)

temp <-
  basemap_geotif(
    st_bbox(seawatervelocity),
    map_service = "esri", map_type = "world_imagery")

topo <- stars::read_stars(temp)

p <-
  ggplot() +
  annotation_spatial(topo) +
  geom_fields(
    data          = seawatervelocity,
    aes(radius    = as.numeric(v),
        linewidth = as.numeric(v),
        angle     = as.numeric(angle),
        colour    = as.numeric(v)),
    max_radius    = grid::unit(0.6, "cm"),
    arrow         = grid::arrow(
      type   = "closed",
      angle  = 30,
      length = grid::unit(0.1, "cm"))) +
  labs(colour     = "v[m/s]",
       linewidth  = "v[m/s]",
       radius     = "v[m/s]") +
  scale_radius_binned(breaks = seq(0, 0.3, by = 0.05)) +
  scale_linewidth_binned(breaks = seq(0, 0.3, by = 0.05), range = c(.5, 4)) +
  scale_colour_viridis_b(breaks = seq(0, 0.3, by = 0.05),
                         option = "magma", guide = guide_bins()) +
  theme(legend.key.size = unit(0.8, "cm"))

ggsave("gallery/pepijn-devries-ggfields.png", p, width = 350, height = 300, units = "px",
       scale = 5)
