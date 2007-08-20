#
#
#

source("ifs.m");





%
%
%
function result = m2i(x)
  
  result = flipud(x);
  
endfunction





%
%
%
function displayBilevelImage(x)

  colormap(gray(2));
  image(2-m2i(x));

endfunction





%
%
%
function ifsProcessWithStepCount(Tdata, imgYsize, imgXsize, stepCount)

  TaIndexes = buildIfsIndexes(Tdata, imgYsize, imgXsize);


  u0 = ones(imgYsize, imgXsize);
  displayBilevelImage(u0);

  for n = 1:stepCount
    u0 = ifsStep(TaIndexes, u0);
    displayBilevelImage(u0);
  endfor

endfunction





%
%
%
function ifsProcess(Tdata, imgYsize, imgXsize)

  stepCount = floor(max(log2(imgYsize), log2(imgXsize)));

  ifsProcessWithStepCount(Tdata, imgYsize, imgXsize, stepCount);

endfunction





%
% Sierpinsky gasket
%
T0 = [ 1/2 0 0 1/2 0   0];
T1 = [ 1/2 0 0 1/2 1/2 0];
T2 = [ 1/2 0 0 1/2 1/4 1/2];
T_Sierpinsky = [ T0; T1; T2 ];

%
%
%
T0 = [ 0.4000 -0.3733 0.0600 0.6000 0.3533 0.0000];
T1 = [-0.8000 -0.1867 0.1371 0.8000 1.1000 0.1000];
T_Leaf = [T0; T1];

%
%
%
T0 = [ 0.7000  0.0000  0.0000 0.7000 0.1496 0.2962 ];
T1 = [ 0.1000 -0.4330  0.1732 0.2500 0.4478 0.0014 ];
T2 = [ 0.1000  0.4330 -0.1732 0.2500 0.4445 0.1559 ];
T3 = [-0.0001  0.0000  0.0000 0.3000 0.4987 0.0070 ];
T_Fern = [ T0; T1; T2; T3 ];

%
%
%
T0 = [ 0.5000  0.0000  0.0000 -0.5000 0.5000 0.5000 ];
T1 = [ 0.0000 -0.5000 -0.5000  0.0000 0.5000 0.5000 ];
T2 = [-0.5000  0.0000  0.0000 -0.5000 0.5000 1.0000 ];
T_GasketA = [ T0; T1; T2 ];





%
%
%
size=256;
imgXsize = size;
imgYsize = size;

%ifsProcess(T_Sierpinsky, imgYsize, imgXsize);
%ifsProcess(T_Leaf, imgYsize, imgXsize);
ifsProcess(T_Fern, imgYsize, imgXsize);
%ifsProcess(T_GasketA, imgYsize, imgXsize);

