simout_zinfglmm_pois_p1 <- simulateResiduals(fittedModel = zinfglmm_pois_p1, plot = T)


simout_zinfglmm_pois_p2 <- simulateResiduals(fittedModel = zinfglmm_pois_p2, plot = T)


simout_zinfglmm_pois_p3 <- simulateResiduals(fittedModel = zinfglmm_pois_p3, plot = T)
testZeroInflation(simout_zinfglmm_pois_p3)
testDispersion(simout_zinfglmm_pois_p3)
