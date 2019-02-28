
library(tacmagic)
library(kinfitr)

kinfitr_refLogan <- function(tm_tac, target, ref, k2_prime, t_star) {
  
  tac <- convert_tacmagic_kinfitr(tm_tac, target, ref)

  logan <- kinfitr::refLogan(tac$time, tac$ref, tac$roi, k2_prime, t_star, tac$weights)

  return(logan)
}


convert_tacmagic_kinfitr <- function(tm_tac, target, ref) {
	
	time <- c(0, (tm_tac$start + tm_tac$end / 2))
	if (attributes(tm_tac)$time_unit == "seconds") time <- time/60

	roi_tac <- c(0, tm_tac[,target])
    ref_tac <- c(0, tm_tac[,ref])

    frame_weights <- c(0, (tm_tac$end - tm_tac$start))

    return(list(time=time, roi=roi_tac, ref=ref_tac, 
                weights=frame_weights))
    
}



# Example

f <- system.file("extdata", "AD06.tac", package="tacmagic")
fv <- system.file("extdata", "AD06_TAC.voistat", package="tacmagic")

AD06_tac <- load_tac(f, format="PMOD")
AD06_volume <- load_vol(fv, format="voistat")
AD06 <- tac_roi(tac=AD06_tac, volumes=AD06_volume, ROI_def=roi_ham_pib(), 
                merge=FALSE, PVC=FALSE)  


# tacmagic Logan ref model
AD06_DVR <- dvr(AD06, target="frontal", ref="cerebellum", 
                k2prime=0.2, t_star=20)

# kinfitr Logan ref model
kinfitr_refLogan(AD06, "frontal", "cerebellum", 0.2, 20)