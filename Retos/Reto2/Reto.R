library("plot3D")
library("bezier")

x_0 <-
  c(0,
    0.136562,
    0.267877,
    0.388898,
    0.494974,
    0.582028,
    0.646715,
    0.68655,
    0.7)
y_0 <-
  c(0.7,
    0.68655,
    0.646716,
    0.582029,
    0.494975,
    0.3889,
    0.267879,
    0.136564,
    0.000001)
z_0 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
x_1 <-
  c(0,
    0.136562,
    0.267877,
    0.388898,
    0.494974,
    0.582028,
    0.646715,
    0.68655,
    0.7)
y_1 <-
  c(0.7,
    0.68655,
    0.646716,
    0.582029,
    0.494975,
    0.3889,
    0.267879,
    0.136564,
    0.000001)
z_1 <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
x_2 <-
  c(0,
    0.152353,
    0.298853,
    0.433868,
    0.552209,
    0.64933,
    0.721497,
    0.765937,
    0.780943)
y_2 <-
  c(
    0.780943,
    0.765937,
    0.721497,
    0.649331,
    0.552211,
    0.433869,
    0.298855,
    0.152355,
    0.000001
  )
z_2 <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
x_3 <-
  c(
    0,
    0,
    0.170516,
    0.170516,
    0.33448,
    0.33448,
    0.485591,
    0.485591,
    0.61804,
    0.61804,
    0.726738,
    0.726738,
    0.807509,
    0.807509,
    0.857247,
    0.857247,
    0.874042,
    0.874042
  )
y_3 <-
  c(
    0.874042,
    0.874042,
    0.857247,
    0.857247,
    0.80751,
    0.80751,
    0.72674,
    0.72674,
    0.618041,
    0.618041,
    0.485592,
    0.485592,
    0.334482,
    0.334482,
    0.170518,
    0.170518,
    0.000001,
    0.000001
  )
z_3 <-
  c(0.3,
    0.3,
    0.3,
    0.3,
    0.3,
    0.3,
    0.3,
    0.3,
    0.3,
    0.3,
    0.3,
    0.3,
    0.3,
    0.3,
    0.3,
    0.3,
    0.3,
    0.3)
x_4 <-
  c(0,
    0.183517,
    0.359983,
    0.522614,
    0.665162,
    0.782149,
    0.869077,
    0.922608,
    0.940683)
y_4 <-
  c(
    0.940683,
    0.922608,
    0.869078,
    0.78215,
    0.665164,
    0.522616,
    0.359985,
    0.183519,
    0.000001
  )
z_4 <- c(0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4)
x_5 <-
  c(0,
    0.193306,
    0.379186,
    0.550493,
    0.700645,
    0.823872,
    0.915438,
    0.971824,
    0.990864)
y_5 <-
  c(
    0.990863,
    0.971825,
    0.915439,
    0.823874,
    0.700647,
    0.550495,
    0.379188,
    0.193309,
    0.000001
  )
z_5 <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
x_6 <-
  c(0,
    0.199207,
    0.39076,
    0.567297,
    0.722033,
    0.849021,
    0.943382,
    1.001489,
    1.02111)
y_6 <-
  c(1.021109,
    1.00149,
    0.943383,
    0.849022,
    0.722034,
    0.567299,
    0.390763,
    0.19921,
    0.000001)
z_6 <- c(0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6)
x_7 <-
  c(0,
    0.202048,
    0.396334,
    0.575388,
    0.732331,
    0.86113,
    0.956837,
    1.015773,
    1.035673)
y_7 <-
  c(
    1.035673,
    1.015773,
    0.956838,
    0.861132,
    0.732332,
    0.57539,
    0.396336,
    0.202051,
    0.000001
  )
z_7 <- c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7)
x_8 <-
  c(0,
    0.203214,
    0.398621,
    0.578708,
    0.736557,
    0.866099,
    0.962358,
    1.021635,
    1.04165)
y_8 <-
  c(
    1.04165,
    1.021635,
    0.962359,
    0.866101,
    0.736558,
    0.578711,
    0.398623,
    0.203217,
    0.000001
  )
z_8 <- c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
x_9 <-
  c(0,
    0.203214,
    0.398621,
    0.578708,
    0.736557,
    0.866099,
    0.962358,
    1.021635,
    1.04165)
y_9 <-
  c(
    1.04165,
    1.021635,
    0.962359,
    0.866101,
    0.736558,
    0.578711,
    0.398623,
    0.203217,
    0.000001
  )
z_9 <- c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9)
x_10 <-
  c(0,
    0.203214,
    0.398621,
    0.578708,
    0.736557,
    0.866099,
    0.962358,
    1.021635,
    1.04165)
y_10 <-
  c(
    1.04165,
    1.021635,
    0.962359,
    0.866101,
    0.736558,
    0.578711,
    0.398623,
    0.203217,
    0.000001
  )
z_10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
x_11 <-
  c(0,
    0.200002,
    0.392319,
    0.56956,
    0.724913,
    0.852408,
    0.947145,
    1.005484,
    1.025183)
y_11 <-
  c(
    1.025182,
    1.005484,
    0.947146,
    0.852409,
    0.724914,
    0.569562,
    0.392321,
    0.200004,
    0.000001
  )
z_11 <- c(1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1)
x_12 <-
  c(0,
    0.180002,
    0.353087,
    0.512604,
    0.652421,
    0.767167,
    0.85243,
    0.904935,
    0.922664)
y_12 <-
  c(
    0.922664,
    0.904936,
    0.852431,
    0.767168,
    0.652423,
    0.512606,
    0.353089,
    0.180004,
    0.000001
  )
z_12 <- c(1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2)
x_13 <-
  c(0,
    0.162001,
    0.317778,
    0.461343,
    0.587179,
    0.69045,
    0.767187,
    0.814442,
    0.830398)
y_13 <-
  c(
    0.830398,
    0.814442,
    0.767188,
    0.690451,
    0.587181,
    0.461345,
    0.31778,
    0.162003,
    0.000001
  )
z_13 <- c(1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3)
x_14 <-
  c(0,
    0.145801,
    0.286001,
    0.415209,
    0.528461,
    0.621405,
    0.690468,
    0.732998,
    0.747358)
y_14 <-
  c(
    0.747358,
    0.732998,
    0.690469,
    0.621406,
    0.528463,
    0.415211,
    0.286002,
    0.145803,
    0.000001
  )
z_14 <- c(1.4, 1.4, 1.4, 1.4, 1.4, 1.4, 1.4, 1.4, 1.4)
x_15 <-
  c(0,
    0.131221,
    0.257401,
    0.373688,
    0.475615,
    0.559264,
    0.621422,
    0.659698,
    0.672622)
y_15 <-
  c(
    0.672622,
    0.659698,
    0.621422,
    0.559265,
    0.475616,
    0.37369,
    0.257402,
    0.131223,
    0.000001
  )
z_15 <- c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5)
x_16 <-
  c(0,
    0.144343,
    0.283141,
    0.411057,
    0.523177,
    0.615191,
    0.683564,
    0.725668,
    0.739885)
y_16 <-
  c(
    0.739884,
    0.725668,
    0.683564,
    0.615192,
    0.523178,
    0.411059,
    0.283142,
    0.144345,
    0.000001
  )
z_16 <- c(1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6)
x_17 <-
  c(0,
    0.158778,
    0.311455,
    0.452163,
    0.575494,
    0.67671,
    0.75192,
    0.798235,
    0.813873)
y_17 <-
  c(
    0.813873,
    0.798235,
    0.751921,
    0.676711,
    0.575496,
    0.452164,
    0.311457,
    0.15878,
    0.000001
  )
z_17 <- c(1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7)
x_18 <-
  c(0,
    0.174655,
    0.3426,
    0.497379,
    0.633044,
    0.744381,
    0.827112,
    0.878058,
    0.89526)
y_18 <-
  c(
    0.89526,
    0.878058,
    0.827113,
    0.744382,
    0.633045,
    0.497381,
    0.342602,
    0.174657,
    0.000001
  )
z_18 <- c(1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8)

# lines3D(x_0, y_0, z_0, colvar=NULL, xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(0, 2), theta = 0, phi = 30, col=19, lwd=3)
# lines3D(x_1, y_1, z_1, colvar=NULL, add=TRUE, col=1, lwd=3)
# lines3D(x_2, y_2, z_2, colvar=NULL, add=TRUE, col=2, lwd=3)
# lines3D(x_3, y_3, z_3, colvar=NULL, add=TRUE, col=3, lwd=3)
# lines3D(x_4, y_4, z_4, colvar=NULL, add=TRUE, col=4, lwd=3)
# lines3D(x_5, y_5, z_5, colvar=NULL, add=TRUE, col=5, lwd=3)
# lines3D(x_6, y_6, z_6, colvar=NULL, add=TRUE, col=6, lwd=3)
# lines3D(x_7, y_7, z_7, colvar=NULL, add=TRUE, col=7, lwd=3)
# lines3D(x_8, y_8, z_8, colvar=NULL, add=TRUE, col=8, lwd=3)
# lines3D(x_9, y_9, z_9, colvar=NULL, add=TRUE, col=9, lwd=3)
# lines3D(x_10, y_10, z_10, colvar=NULL, add=TRUE, col=10, lwd=3)
# lines3D(x_11, y_11, z_11, colvar=NULL, add=TRUE, col=11, lwd=3)
# lines3D(x_12, y_12, z_12, colvar=NULL, add=TRUE, col=12, lwd=3)
# lines3D(x_13, y_13, z_13, colvar=NULL, add=TRUE, col=13, lwd=3)
# lines3D(x_14, y_14, z_14, colvar=NULL, add=TRUE, col=14, lwd=3)
# lines3D(x_15, y_15, z_15, colvar=NULL, add=TRUE, col=15, lwd=3)
# lines3D(x_16, y_16, z_16, colvar=NULL, add=TRUE, col=16, lwd=3)
# lines3D(x_17, y_17, z_17, colvar=NULL, add=TRUE, col=17, lwd=3)
# lines3D(x_18, y_18, z_18, colvar=NULL, add=TRUE, col=18, lwd=3)
#
# lines3D(x_0, y_0*-1, z_0, colvar=NULL, add=TRUE, col=19, lwd=3)
# lines3D(x_1, y_1*-1, z_1, colvar=NULL, add=TRUE, col=1, lwd=3)
# lines3D(x_2, y_2*-1, z_2, colvar=NULL, add=TRUE, col=2, lwd=3)
# lines3D(x_3, y_3*-1, z_3, colvar=NULL, add=TRUE, col=3, lwd=3)
# lines3D(x_4, y_4*-1, z_4, colvar=NULL, add=TRUE, col=4, lwd=3)
# lines3D(x_5, y_5*-1, z_5, colvar=NULL, add=TRUE, col=5, lwd=3)
# lines3D(x_6, y_6*-1, z_6, colvar=NULL, add=TRUE, col=6, lwd=3)
# lines3D(x_7, y_7*-1, z_7, colvar=NULL, add=TRUE, col=7, lwd=3)
# lines3D(x_8, y_8*-1, z_8, colvar=NULL, add=TRUE, col=8, lwd=3)
# lines3D(x_9, y_9*-1, z_9, colvar=NULL, add=TRUE, col=9, lwd=3)
# lines3D(x_10, y_10*-1, z_10, colvar=NULL, add=TRUE, col=10, lwd=3)
# lines3D(x_11, y_11*-1, z_11, colvar=NULL, add=TRUE, col=11, lwd=3)
# lines3D(x_12, y_12*-1, z_12, colvar=NULL, add=TRUE, col=12, lwd=3)
# lines3D(x_13, y_13*-1, z_13, colvar=NULL, add=TRUE, col=13, lwd=3)
# lines3D(x_14, y_14*-1, z_14, colvar=NULL, add=TRUE, col=14, lwd=3)
# lines3D(x_15, y_15*-1, z_15, colvar=NULL, add=TRUE, col=15, lwd=3)
# lines3D(x_16, y_16*-1, z_16, colvar=NULL, add=TRUE, col=16, lwd=3)
# lines3D(x_17, y_17*-1, z_17, colvar=NULL, add=TRUE, col=17, lwd=3)
# lines3D(x_18, y_18*-1, z_18, colvar=NULL, add=TRUE, col=18, lwd=3)
#
#
# lines3D(x_0*-1, y_0, z_0, colvar=NULL, add=TRUE, col=19, lwd=3)
# lines3D(x_1*-1, y_1, z_1, colvar=NULL, add=TRUE, col=1, lwd=3)
# lines3D(x_2*-1, y_2, z_2, colvar=NULL, add=TRUE, col=2, lwd=3)
# lines3D(x_3*-1, y_3, z_3, colvar=NULL, add=TRUE, col=3, lwd=3)
# lines3D(x_4*-1, y_4, z_4, colvar=NULL, add=TRUE, col=4, lwd=3)
# lines3D(x_5*-1, y_5, z_5, colvar=NULL, add=TRUE, col=5, lwd=3)
# lines3D(x_6*-1, y_6, z_6, colvar=NULL, add=TRUE, col=6, lwd=3)
# lines3D(x_7*-1, y_7, z_7, colvar=NULL, add=TRUE, col=7, lwd=3)
# lines3D(x_8*-1, y_8, z_8, colvar=NULL, add=TRUE, col=8, lwd=3)
# lines3D(x_9*-1, y_9, z_9, colvar=NULL, add=TRUE, col=9, lwd=3)
# lines3D(x_10*-1, y_10, z_10, colvar=NULL, add=TRUE, col=10, lwd=3)
# lines3D(x_11*-1, y_11, z_11, colvar=NULL, add=TRUE, col=11, lwd=3)
# lines3D(x_12*-1, y_12, z_12, colvar=NULL, add=TRUE, col=12, lwd=3)
# lines3D(x_13*-1, y_13, z_13, colvar=NULL, add=TRUE, col=13, lwd=3)
# lines3D(x_14*-1, y_14, z_14, colvar=NULL, add=TRUE, col=14, lwd=3)
# lines3D(x_15*-1, y_15, z_15, colvar=NULL, add=TRUE, col=15, lwd=3)
# lines3D(x_16*-1, y_16, z_16, colvar=NULL, add=TRUE, col=16, lwd=3)
# lines3D(x_17*-1, y_17, z_17, colvar=NULL, add=TRUE, col=17, lwd=3)
# lines3D(x_18*-1, y_18, z_18, colvar=NULL, add=TRUE, col=18, lwd=3)
#
# lines3D(x_0*-1, y_0*-1, z_0, colvar=NULL, add=TRUE, col=19, lwd=3)
# lines3D(x_1*-1, y_1*-1, z_1, colvar=NULL, add=TRUE, col=1, lwd=3)
# lines3D(x_2*-1, y_2*-1, z_2, colvar=NULL, add=TRUE, col=2, lwd=3)
# lines3D(x_3*-1, y_3*-1, z_3, colvar=NULL, add=TRUE, col=3, lwd=3)
# lines3D(x_4*-1, y_4*-1, z_4, colvar=NULL, add=TRUE, col=4, lwd=3)
# lines3D(x_5*-1, y_5*-1, z_5, colvar=NULL, add=TRUE, col=5, lwd=3)
# lines3D(x_6*-1, y_6*-1, z_6, colvar=NULL, add=TRUE, col=6, lwd=3)
# lines3D(x_7*-1, y_7*-1, z_7, colvar=NULL, add=TRUE, col=7, lwd=3)
# lines3D(x_8*-1, y_8*-1, z_8, colvar=NULL, add=TRUE, col=8, lwd=3)
# lines3D(x_9*-1, y_9*-1, z_9, colvar=NULL, add=TRUE, col=9, lwd=3)
# lines3D(x_10*-1, y_10*-1, z_10, colvar=NULL, add=TRUE, col=10, lwd=3)
# lines3D(x_11*-1, y_11*-1, z_11, colvar=NULL, add=TRUE, col=11, lwd=3)
# lines3D(x_12*-1, y_12*-1, z_12, colvar=NULL, add=TRUE, col=12, lwd=3)
# lines3D(x_13*-1, y_13*-1, z_13, colvar=NULL, add=TRUE, col=13, lwd=3)
# lines3D(x_14*-1, y_14*-1, z_14, colvar=NULL, add=TRUE, col=14, lwd=3)
# lines3D(x_15*-1, y_15*-1, z_15, colvar=NULL, add=TRUE, col=15, lwd=3)
# lines3D(x_16*-1, y_16*-1, z_16, colvar=NULL, add=TRUE, col=16, lwd=3)
# lines3D(x_17*-1, y_17*-1, z_17, colvar=NULL, add=TRUE, col=17, lwd=3)
# lines3D(x_18*-1, y_18*-1, z_18, colvar=NULL, add=TRUE, col=18, lwd=3)

bezierCurvaNivel <- function(x_v, y_v, z_v) {
  t <- seq(0, 1, 0.01)
  x_t = x_v
  y_t = y_v
  x_c <- c()
  y_c <- c()
  for (i in 1:length(x_t)) {
    if (i != length(x_t)) {
      x_c <- c(x_c, (x_t[i] + x_t[i + 1]) / 2)
      y_c <- c(y_c, (y_t[i] + y_t[i + 1]) / 2)
    }
  }
  x_f <-
    c(x_t, x_c)[order(c(seq_along(x_t) * 2 - 1, seq_along(x_c) * 2))]
  y_f <-
    c(y_t, y_c)[order(c(seq_along(y_t) * 2 - 1, seq_along(y_c) * 2))]
  matriz <- cbind(x_f, y_f)
  indice_control <- seq(1, length(x_f), 2)
  puntos <- matrix(rep(0, 2), ncol = 2)
  for (i in seq(1, length(indice_control) - 1, 1)) {
    b <- bezier(t, matriz[indice_control[i]:indice_control[i + 1], ])
    puntos <- rbind(puntos, b)
  }
  puntos <- cbind(puntos, rep(z_v[1], nrow(puntos)))
  colnames(puntos) <- c("x", "y", "z")
  return(puntos[2:nrow(puntos),])
}

# plot(bezierCurvaNivel(x_0, y_0, z_0)[,1:2], type = "l", col = 19, xlim= c(0,1.1), ylim = c(0,1.1))
# lines(bezierCurvaNivel(x_1, y_1, z_1)[,1:2], type = "l", col = 1)
# lines(bezierCurvaNivel(x_2, y_2, z_2)[,1:2], type = "l", col = 2)
# lines(bezierCurvaNivel(x_3, y_3, z_3)[,1:2], type = "l", col = 3)
# lines(bezierCurvaNivel(x_4, y_4, z_4)[,1:2], type = "l", col = 4)
# lines(bezierCurvaNivel(x_5, y_5, z_5)[,1:2], type = "l", col = 5)
# lines(bezierCurvaNivel(x_6, y_6, z_6)[,1:2], type = "l", col = 6)
# lines(bezierCurvaNivel(x_7, y_7, z_7)[,1:2], type = "l", col = 7)
# lines(bezierCurvaNivel(x_8, y_8, z_8)[,1:2], type = "l", col = 8)
# lines(bezierCurvaNivel(x_9, y_9, z_9)[,1:2], type = "l", col = 9)
# lines(bezierCurvaNivel(x_10, y_10, z_10)[,1:2], type = "l", col = 10)
# lines(bezierCurvaNivel(x_11, y_11, z_11)[,1:2], type = "l", col = 11)
# lines(bezierCurvaNivel(x_12, y_12, z_12)[,1:2], type = "l", col = 12)
# lines(bezierCurvaNivel(x_13, y_13, z_13)[,1:2], type = "l", col = 13)
# lines(bezierCurvaNivel(x_14, y_14, z_14)[,1:2], type = "l", col = 14)
# lines(bezierCurvaNivel(x_15, y_15, z_15)[,1:2], type = "l", col = 15)
# lines(bezierCurvaNivel(x_16, y_16, z_16)[,1:2], type = "l", col = 16)
# lines(bezierCurvaNivel(x_17, y_17, z_17)[,1:2], type = "l", col = 17)
# lines(bezierCurvaNivel(x_18, y_18, z_18)[,1:2], type = "l", col = 18)

nivel_0 <- bezierCurvaNivel(x_0, y_0, z_0)
nivel_1 <- bezierCurvaNivel(x_1, y_1, z_1)
nivel_2 <- bezierCurvaNivel(x_2, y_2, z_2)
nivel_3 <- bezierCurvaNivel(x_3, y_3, z_3)
nivel_4 <- bezierCurvaNivel(x_4, y_4, z_4)
nivel_5 <- bezierCurvaNivel(x_5, y_5, z_5)
nivel_6 <- bezierCurvaNivel(x_6, y_6, z_6)
nivel_7 <- bezierCurvaNivel(x_7, y_7, z_7)
nivel_8 <- bezierCurvaNivel(x_8, y_8, z_8)
nivel_9 <- bezierCurvaNivel(x_9, y_9, z_9)
nivel_10 <- bezierCurvaNivel(x_10, y_10, z_10)
nivel_11 <- bezierCurvaNivel(x_11, y_11, z_11)
nivel_12 <- bezierCurvaNivel(x_12, y_12, z_12)
nivel_13 <- bezierCurvaNivel(x_13, y_13, z_13)
nivel_14 <- bezierCurvaNivel(x_14, y_14, z_14)
nivel_15 <- bezierCurvaNivel(x_15, y_15, z_15)
nivel_16 <- bezierCurvaNivel(x_16, y_16, z_16)
nivel_17 <- bezierCurvaNivel(x_17, y_17, z_17)
nivel_18 <- bezierCurvaNivel(x_18, y_18, z_18)

lines3D(nivel_0[,1:1], nivel_0[,2:2], nivel_0[,3:3], colvar=NULL, xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(0, 2), theta = 90, phi = 0, col=19, lwd=3)
lines3D(nivel_1[,1:1], nivel_1[,2:2], nivel_1[,3:3], colvar=NULL, col=1, lwd=3, add = TRUE)
lines3D(nivel_2[,1:1], nivel_2[,2:2], nivel_2[,3:3], colvar=NULL, col=2, lwd=3, add = TRUE)
lines3D(nivel_3[,1:1], nivel_3[,2:2], nivel_3[,3:3], colvar=NULL, col=3, lwd=3, add = TRUE)
lines3D(nivel_4[,1:1], nivel_4[,2:2], nivel_4[,3:3], colvar=NULL, col=4, lwd=3, add = TRUE)
lines3D(nivel_5[,1:1], nivel_5[,2:2], nivel_5[,3:3], colvar=NULL, col=5, lwd=3, add = TRUE)
lines3D(nivel_6[,1:1], nivel_6[,2:2], nivel_6[,3:3], colvar=NULL, col=6, lwd=3, add = TRUE)
lines3D(nivel_7[,1:1], nivel_7[,2:2], nivel_7[,3:3], colvar=NULL, col=7, lwd=3, add = TRUE)
lines3D(nivel_8[,1:1], nivel_8[,2:2], nivel_8[,3:3], colvar=NULL, col=8, lwd=3, add = TRUE)
lines3D(nivel_9[,1:1], nivel_9[,2:2], nivel_9[,3:3], colvar=NULL, col=9, lwd=3, add = TRUE)
lines3D(nivel_10[,1:1], nivel_10[,2:2], nivel_10[,3:3], colvar=NULL, col=10, lwd=3, add = TRUE)
lines3D(nivel_11[,1:1], nivel_11[,2:2], nivel_11[,3:3], colvar=NULL, col=11, lwd=3, add = TRUE)
lines3D(nivel_12[,1:1], nivel_12[,2:2], nivel_12[,3:3], colvar=NULL, col=12, lwd=3, add = TRUE)
lines3D(nivel_13[,1:1], nivel_13[,2:2], nivel_13[,3:3], colvar=NULL, col=13, lwd=3, add = TRUE)
lines3D(nivel_14[,1:1], nivel_14[,2:2], nivel_14[,3:3], colvar=NULL, col=14, lwd=3, add = TRUE)
lines3D(nivel_15[,1:1], nivel_15[,2:2], nivel_15[,3:3], colvar=NULL, col=15, lwd=3, add = TRUE)
lines3D(nivel_16[,1:1], nivel_16[,2:2], nivel_16[,3:3], colvar=NULL, col=16, lwd=3, add = TRUE)
lines3D(nivel_17[,1:1], nivel_17[,2:2], nivel_17[,3:3], colvar=NULL, col=17, lwd=3, add = TRUE)
lines3D(nivel_18[,1:1], nivel_18[,2:2], nivel_18[,3:3], colvar=NULL, col=18, lwd=3, add = TRUE)
