prob_neg = 0.75
prob_pos = 0.25

x <- seq(-10, 10, 0.1)

x_neg_prob =  (exp(-((x + 1) ^ 2) / 4)) / (2*sqrt(pi))
x_pos_prob =  (sqrt(2) * exp(-2 * (x - 3) ^ 2)) / sqrt(pi)

px = (x_neg_prob * prob_neg) + (x_pos_prob * prob_pos)

py_pos_given_x <- x_pos_prob * prob_pos / px
py_neg_given_x <- x_neg_prob * prob_neg / px

plot(x, py_neg_given_x,type = "l", col="red", xlab = "x Values", ylab = "p(y|x)",
     main = "p(y=-1|x) vs. p(y=+1|x)", cex.lab=1.2, cex.axis = 1.2, cex=1.2, xaxt="n")
lines(x, py_pos_given_x, col="blue")
axis(side = 1, at = seq(-10, 10))
