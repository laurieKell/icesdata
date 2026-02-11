for (san in c("san.sa.1r","san.sa.2r","san.sa.3r","san.sa.4"))
  icesdata[[san]]=qapply(icesdata[[san]], function(x) {
    dimnames(x)$unit="unique"
    x})