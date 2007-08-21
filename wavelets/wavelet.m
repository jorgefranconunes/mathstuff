# 
# 
#
1;





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

