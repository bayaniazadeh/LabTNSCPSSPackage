# Empty list
.weights <- list()

for (w in names(.maps)) {
  if (grepl(pattern = "charlson", x = w)) {
    # Charlson-compatible weights:
    # Original Charlson weights
    .weights[[w]][["charlson"]] <- c(
      mi = 1,
      chf = 1,
      pvd = 1,
      cevd = 1,
      dementia = 1,
      cpd = 1,
      rheumd = 1,
      pud = 1,
      mld = 1,
      diab = 1,
      diabwc = 2,
      hp = 2,
      rend = 2,
      canc = 2,
      msld = 3,
      metacanc = 6,
      aids = 6
    )
    # Quan (2011)
    .weights[[w]][["quan"]] <- c(
      mi = 0,
      chf = 2,
      pvd = 0,
      cevd = 0,
      dementia = 2,
      cpd = 1,
      rheumd = 1,
      pud = 0,
      mld = 2,
      diab = 0,
      diabwc = 1,
      hp = 2,
      rend = 1,
      canc = 2,
      msld = 4,
      metacanc = 6,
      aids = 4
    )
  } else {
    # Elixhauser-compatible weights:
    
    
    # hcup mortality weights mortality
    .weights[[w]][["mortality_elix_hcup"]] <- c(
      chf = 15,
      carit = 4,
      valv = 0,
      pcd = 4,
      pvd = 3,
      hypunc = 0,
      hypc = 1,
      para = 4,
      ond = 23,
      cpd = 2,
      diabunc = 0,
      diabc = -2,
      hypothy = -3,
      rf = 0, #not mentioned
      ld = 17,
      pud = 0,
      aids = -4,
      lymph = 6,
      metacanc = 23,
      solidtum = 10,
      rheumd = 0,#not mentioned
      coag = 15,
      obes = -7,
      wloss = 14,
      fed = 0,# not mentioned
      blane = -4,
      dane = -3,
      alcohol = -1,
      drug = -7,
      psycho = -9,
      depre = -9
    )
    
    # binary weights
    .weights[[w]][["elix_binary"]] <- c(
      chf = 1,
      carit = 1,
      valv = 1,
      pcd = 1,
      pvd = 1,
      hypunc = 1,
      hypc = 1,
      para = 1,
      ond = 1,
      cpd = 1,
      diabunc = 1,
      diabc = 1,
      hypothy = 1,
      rf = 1, #not mentioned
      ld = 1,
      pud = 1,
      aids = 1,
      lymph = 1,
      metacanc = 1,
      solidtum = 1,
      rheumd = 1,#not mentioned
      coag = 1,
      obes = 1,
      wloss = 1,
      fed = 1,# not mentioned
      blane = 1,
      dane = 1,
      alcohol = 1,
      drug = 1,
      psycho = 1,
      depre = 1
    )
    
    
    # hcup readmission weights
    .weights[[w]][["readmission_elix_hcup"]] <- c(
      chf = 7,
      carit = 0,# not mentioned
      valv = 0,
      pcd = 3,
      pvd = 1,
      hypunc = 0,
      hypc = 0,
      para = 3,
      ond = 2,
      cpd = 4,
      diabunc = 0,
      diabc = 4,
      hypothy = 0,
      rf = 0, #Not mentioned
      ld = 10,
      pud = 2,
      aids = 5,
      lymph = 7,
      metacanc = 0,#not  mentioned
      solidtum = 7,
      rheumd = 0,#not mentioned
      coag = 3,
      obes = -2,
      wloss = 6,
      fed = 0,#not mentioned
      blane = 2,
      dane = 5,
      alcohol = 3,
      drug = 6,
      psycho = 6,
      depre = 2
    )
    
    # van Walraven
    .weights[[w]][["vw"]] <- c(
      chf = 7,
      carit = 5,
      valv = -1,
      pcd = 4,
      pvd = 2,
      hypunc = 0,
      hypc = 0,
      para = 7,
      ond = 6,
      cpd = 3,
      diabunc = 0,
      diabc = 0,
      hypothy = 0,
      rf = 5,
      ld = 11,
      pud = 0,
      aids = 0,
      lymph = 9,
      metacanc = 12,
      solidtum = 4,
      rheumd = 0,
      coag = 3,
      obes = -4,
      wloss = 6,
      fed = 5,
      blane = -2,
      dane = -2,
      alcohol = 0,
      drug = -7,
      psycho = 0,
      depre = -3
    )
    # Swiss weights
    .weights[[w]][["swiss"]] <- c(
      chf = 13,
      carit = 6,
      valv = -1,
      pcd = 6,
      pvd = 3,
      hypunc = -4,
      hypc = -3,
      para = 11,
      ond = 10,
      cpd = 3,
      diabunc = 1,
      diabc = -1,
      hypothy = -3,
      rf = 8,
      ld = 16,
      pud = 0,
      aids = 0,
      lymph = 9,
      metacanc = 17,
      solidtum = 10,
      rheumd = -1,
      coag = 9,
      obes = -6,
      wloss = 6,
      fed = 5,
      blane = -5,
      dane = -7,
      alcohol = -3,
      drug = -5,
      psycho = -4,
      depre = -3
    )
  }
  usethis::ui_done(x = "Done with score: '{w}'!")
}
