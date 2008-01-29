# 
# 
#
1;





%
%
%
function result = delta(x)

  length   = length(x);
  xForward = x(2:length);
  result   = xForward - x(1:length-1);

endfunction





%
%
%
function result = deltaRel(x)

  length   = length(x);
  xForward = x(2:length);
  xTrunc   = x(1:length-1);
  result   = (xForward - xTrunc) ./ xTrunc;

endfunction

