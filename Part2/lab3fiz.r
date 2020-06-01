ck<- function(m1, m2, t1, t2, tk){
  cw = 4190
  mk = 0.765
  (cw / mk) * (m2*((t2-tk)/(tk-t1)) - m1)
}
ubck<- function(m1, m2, t1, t2, tk){
  cw = 4190
  mk = 0.765

  ubm1 = 0.02887
  ubm2 = 0.02887
  ubt1 = 0.00005773
  ubtk = 0.00005773
  ubt2 = 0.57735

  A = (-cw/mk)^2 * (ubm1)^2
  B = ((cw/mk) * ((t2-tk)/(tk-t1)))^2 * (ubm2)^2
  C = ((cw/mk) *m2* ((t2-tk)/(tk-t1)^2 ))^2 * (ubt1)^2
  D = ((cw/mk) *m2* ((t1-t2)/(tk-t1)^2 ))^2 * (ubtk)^2
  E = ((cw/mk) *m2* ((1)/(tk-t1) ))^2 * (ubt2)^2

  print(A)
  print(B)
  print(C)
  print(D)
  print(E)

  sqrt(A + B + C + D + E)
}
m1 <- seq(0.3, 1.5, length.out = 5)
m2 <- rep(0.3, 5)
t1 = c(22, 29.0631, 34.9634, 38.2693, 40.3835)
tk = c(29.0631, 34.9634, 38.2693, 40.3835, 43.3791)
t2 = c(40, 50, 50, 50, 60)

res <- ck(m1,m2,t1,t2,tk)
resb <- ubck(m1,m2,t1,t2,tk)
