#load "graphics.cma";;
open Graphics;;
open_graph "";;

let rec koch (x1,y1) (x5,y5) = function
  | 0 -> (moveto x1 y1; lineto x5 y5)
  | n -> 
      let x2 = x1 + (x5 - x1) / 3 and y2 = y1 + (y5 - y1) / 3 in
      let x4 = x1 + 2 * (x5 - x1) / 3 and y4 = y1 + 2 * (y5 - y1) / 3 in
      let x3 = (x2 + x4)/2 - int_of_float(float_of_int(y4 - y2)*.(sqrt(3.)/.2.))
      and y3 = (y2 + y4)/2 + int_of_float(float_of_int(x4 - x2)*.(sqrt(3.)/.2.)) in
      koch (x1,y1) (x2,y2) (n-1);
      koch (x2,y2) (x3,y3) (n-1);
      koch (x3,y3) (x4,y4) (n-1);
      koch (x4,y4) (x5,y5) (n-1);;

clear_graph();;
koch (100,100) (500,500) 5;;
