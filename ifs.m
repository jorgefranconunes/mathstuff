#
# Iterated function systems.
#





%
% Sierpinsky gasket
%
T0 = [ 1/2 0 0 1/2 0 0];
T1 = [ 1/2 0 0 1/2 1/2 0];
T2 = [ 1/2 0 0 1/2 1/4 1/2];
Ta = [ T0; T1; T2 ];





%
%
%
function indT = iff2ind(T, m, n)

  a = T(1);
  b = T(2);
  c = T(3);
  d = T(4);
  e = n * T(5);
  f = m * T(6);
  A = [ a, b; c, d];
  invA = inv(A);
  aInv = invA(1,1);
  bInv = invA(1,2);
  cInv = invA(2,1);
  dInv = invA(2,2);

  xx1Line = [0 : m-1];
  vTildeXLine = xx1Line - e;

  yy1Line = [0 : n-1];
  vTildeYLine = yy1Line - f;

  vTildeX = vTildeXLine(ones(1,m), :);
  vTildeY = vTildeYLine(ones(1,n), :)';

  xx0 = aInv*vTildeX + bInv*vTildeY;
  yy0 = cInv*vTildeX + dInv*vTildeY;

  indT = 1 + floor(xx0 .+ (n*yy0));
  indT(xx0>=n | xx0<0) = 0;
  indT(yy0>=m | yy0<0) = 0;
  indT = reshape(indT, 1, m*n);

endfunction





%
%
%
function ifsIndexes = buildIfsIndexes(Tdata, m, n)

  [Tcount, dymmy] = size(Tdata);
  ifsIndexes = zeros(Tcount, m*n);

  for line = 1:Tcount
    T = Tdata(line, :);
    indT = iff2ind(T, m, n);
    ifsIndexes(line, :) = indT;
  endfor

endfunction





%
%
%
function u1 = ifsStep(ifsIndexes, u0)

  [affineCount, dummy] = size(ifsIndexes);
  [m, n] = size(u0);
  u0     = [0, reshape(u0, 1, m*n)];
  u1     = zeros(1, m*n);

  for k = 1:affineCount
    indexes = ifsIndexes(k, :) + 1;
    u1 = u1 + u0(indexes);
  endfor

  u1 = reshape(u1, m, n);

endfunction





%
%
%
imgXsize = 256;
imgYsize = 256;

TaIndexes = buildIfsIndexes(Ta, imgYsize, imgXsize);


colormap(gray(2));
u0 = ones(imgYsize, imgXsize);
image(u0+1);

u1 = ifsStep(TaIndexes, u0);
image(u1+1);

u2 = ifsStep(TaIndexes, u1);
image(u2+1);

u3 = ifsStep(TaIndexes, u2);
image(u3+1);

u4 = ifsStep(TaIndexes, u3);
image(u4+1);

u5 = ifsStep(TaIndexes, u4);
image(u5+1);

u6 = ifsStep(TaIndexes, u5);
image(u6+1);

u7 = ifsStep(TaIndexes, u6);
image(u7+1);

u8 = ifsStep(TaIndexes, u7);
image(u8+1);

