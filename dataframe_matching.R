find.rowidx<-function(d1row, d1cols, d2, d2cols) {
  pos<-which(mapply(function(d2idx) {
    Reduce(function(a,b) {a & b}, d1row[1, d1cols]==d2[d2idx, d2cols])
  }, 1:nrow(d2)), arr.ind=TRUE)
  ifelse(length(pos)>0, pos, NA)
}

match.rowidx<-function(d1, d1cols, d2, d2cols) {
  unlist(lapply(1:nrow(d1), function(d1idx) {
    find.rowidx(d1[d1idx,], d1cols, d2, d2cols)
  }))
}
