
library(gridExtra)

# CLUSTER 1---------------------------------------------------------------------


a <- TFMevt::qq_gev(fit_cluster_1,incendios_cluster_1$superficie)

b <- TFMevt::qq_gumbel(fit_cluster_1_Gumbel, incendios_cluster_1$superficie)

c <- TFMevt::qq_gpd(fit_cluster_1_GP, incendios_cluster_1$superficie,threshold = 3)



grid.arrange(a, b, c)

# CLUSTER 2-------------------------------------------------------------------

a <- TFMevt::qq_gev(fit_cluster_2,incendios_cluster_2$superficie)

b <- TFMevt::qq_gumbel(fit_cluster_2_Gumbel, incendios_cluster_2$superficie)

c <- TFMevt::qq_gpd(fit_cluster_2_GP, incendios_cluster_2$superficie,threshold = 5)
c


grid.arrange(a, b, c)
# CLUSTER 3---------------------------------------------------------------------

a <- TFMevt::qq_gev(fit_cluster_3,incendios_cluster_3$superficie)

b <- TFMevt::qq_gumbel(fit_cluster_3_Gumbel, incendios_cluster_3$superficie)

c <- TFMevt::qq_gpd(fit_cluster_3_GP, incendios_cluster_3$superficie,threshold = 3.2)
a
b
c


grid.arrange(a, b, c)

# CLUSTER 4---------------------------------------------------------------------



a <- TFMevt::qq_gev(fit_cluster_4,incendios_cluster_4$superficie)

b <- TFMevt::qq_gumbel(fit_cluster_4_Gumbel, incendios_cluster_4$superficie)

c <- TFMevt::qq_gpd(fit_cluster_4_GP, incendios_cluster_4$superficie,threshold = 3)
a
b
c


grid.arrange(a, b, c)






























































