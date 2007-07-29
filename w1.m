# Orthogonal Daubechies coefficients (normalized to have sum 2).
# Obtained from http://en.wikipedia.org/wiki/Daubechies_wavelet

D4 = [
0.6830127
1.1830127
0.3169873
-0.1830127
];

D6 = [
0.47046721
1.14111692
0.650365
-0.19093442
-0.12083221
0.0498175
];

D8 = [
0.32580343
1.01094572
0.8922014
-0.03957503
-0.26450717
0.0436163
0.0465036
-0.01498699
];

D10 = [
0.22641898
0.85394354
1.02432694
0.19576696
-0.34265671
-0.04560113
0.10970265
-0.00882680
-0.01779187
4.71742793e-3
];





%
% Shring lines but keep the same number of points in each line.
%
function result = shrinkBy2(values)

  [lineCount, colCount] = size(values);
  sourceColIndexes      = 1:colCount;
  destColIndexes        = 1:(colCount/2);

  result                    = zeros(lineCount, colCount);
  result(:, destColIndexes) = values(:, 2*destColIndexes-1);

endfunction

