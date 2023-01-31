if (!require('upstartr')) install.packages('upstartr'); library('upstartr')
if (!require('waapihktheme')) install_github('monkeycycle/waapihktheme', force=TRUE, ref = "0f7e109"); library('waapihktheme');

run_config()
run_process()
run_analyze()
run_visualize()
run_notebook()

source(dir_src("theme.R"))
source(dir_src("daylight.R"))
sunrise_sunset <- grid::grid.draw(daylight(49.8954, -97.1385, "Winnipeg, Manitoba", "2023-01-01", 365, tz="America/Winnipeg"))


# daylight_srss <- daylight(49.8954, -97.1385, "Winnipeg, MB", "2023-01-01", 365, tz="America/Winnipeg")
# View(daylight_srss)
