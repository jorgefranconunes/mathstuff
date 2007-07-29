NPOINTS = 1000;







function result = getCoinTosses(nPoints, weight)

  result = -1 + 2*(rand(1, nPoints) < weight);

endfunction






a = [ 1 -1];
b = [ 0  1];

x = getCoinTosses(NPOINTS, 0.5);
y = filter(b, a, x);

plot(y);
#hist(y, 100);
