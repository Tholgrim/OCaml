#load "graphics.cma";;
open Graphics;; open Random;;
open_graph "";;

let rec mountain x y z t = function
	| 0 -> moveto x y; lineto z t
	| n -> 
		let h = (y+t)/2 + int(abs(z-x)/5 + 20) and u = x+(z-x)/2 in
				mountain x y u h (n-1);
				mountain u h z t (n-1);;

mountain 20 20 500 20 8;;