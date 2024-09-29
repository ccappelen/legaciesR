df <- legaciesr::shape_raw_polygons

get_contours <- function(shp, grp_id,
                         by_period = FALSE, interval,
                         nmap_threshold = 5,
                         progress = F,
                         parallel = F,
                         returnList = F,
                         ...)

df_list <- split(df, df$COWID)

df_contour_combine <- lapply(df_list,
                             FUN = function(x) contour_polygons(x, invalid_geom = "fix"))

df_contour_combine2 <- bind_rows(df_contour_combine)
