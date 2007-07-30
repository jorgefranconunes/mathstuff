# Orthogonal Daubechies coefficients (normalized to have sum 2).
# Obtained from http://en.wikipedia.org/wiki/Daubechies_wavelet

D4 = [
0.6830127
1.1830127
0.3169873
-0.1830127
];

%D4 = [
%0.482962913144
%0.836516303737
%0.224143868042
%-0.129409522551
%];

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

D12 = [
0.15774243
0.69950381
1.06226376
0.44583132
-0.31998660
-0.18351806
0.13788809
0.03892321
-0.04466375
7.83251152e-4
6.75606236e-3
-1.52353381e-3
];





%
% Shrink lines but keep the same number of points in each line.
%
function result = shrinkBy2(values)

  [lineCount, colCount] = size(values);
  sourceColIndexes      = 1:colCount;
  destColIndexes        = 1:(colCount/2);

  result                    = zeros(lineCount, colCount);
  result(:, destColIndexes) = values(:, 2*destColIndexes-1);

endfunction





%
%
%
function result = translateLine(x, d)

  pointCount               = length(x);
  result                   = zeros(1, pointCount);
  result((d+1):pointCount) = x(1:(pointCount-d));

endfunction





%
% The "d" line vector is expected to contain as many elements as rows in "m".
%
function result = translate(m, d)

  [lineCount, rowCount] = size(m);
  result = zeros(lineCount, rowCount);
  
  for line = 1:lineCount
    result(line, :) = translateLine(m(line,:), d(line));
  endfor

endfunction





%
%
%
function result = waveletStep(phi0, h, unitSize)

  coefCount = length(h);
  mPhi0 = phi0(ones(1,coefCount), :);
  mPhi0 = shrinkBy2(mPhi0);
  mPhi0 = translate(mPhi0, 0.5*unitSize*[0:(coefCount-1)]);

  result = h * mPhi0;

endfunction





%
%
%
function [time, phi] = wavelet(h, unitSize, depth)

  coefCount = length(h);
  maxT      = coefCount - 1;

  phi             = zeros(1, maxT*unitSize);
  phi(1:unitSize) = ones(1, unitSize);

  for i = 1:depth
    phi = waveletStep(phi, h, unitSize);
  endfor

  time = 0:(1/unitSize):maxT;
  time = time(1:length(phi));

endfunction

