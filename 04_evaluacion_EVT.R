

library(gridExtra)

# CLUSTER 1---------------------------------------------------------------------


a <- TFMevt::qq_gev(fit_cluster_1,terremotos_cluster_1$mag)

b <- TFMevt::qq_gumbel(fit_cluster_1_Gumbel, terremotos_cluster_1$mag)

c <- TFMevt::qq_gpd(fit_cluster_1_GP, terremotos_cluster_1$mag,threshold = 3)



grid.arrange(a, b, c)

# CLUSTER 2-------------------------------------------------------------------

a <- TFMevt::qq_gev(fit_cluster_2,terremotos_cluster_2$mag)

b <- TFMevt::qq_gumbel(fit_cluster_2_Gumbel, terremotos_cluster_2$mag)

c <- TFMevt::qq_gpd(fit_cluster_2_GP, terremotos_cluster_2$mag,threshold = 5)
c


grid.arrange(a, b, c)
# CLUSTER 3---------------------------------------------------------------------

a <- TFMevt::qq_gev(fit_cluster_3,terremotos_cluster_3$mag)

b <- TFMevt::qq_gumbel(fit_cluster_3_Gumbel, terremotos_cluster_3$mag)

c <- TFMevt::qq_gpd(fit_cluster_3_GP, terremotos_cluster_3$mag,threshold = 3.2)
a
b
c


grid.arrange(a, b, c)

# CLUSTER 4---------------------------------------------------------------------



a <- TFMevt::qq_gev(fit_cluster_4,terremotos_cluster_4$mag)

b <- TFMevt::qq_gumbel(fit_cluster_4_Gumbel, terremotos_cluster_4$mag)

c <- TFMevt::qq_gpd(fit_cluster_4_GP, terremotos_cluster_4$mag,threshold = 3)
a
b
c


grid.arrange(a, b, c)






