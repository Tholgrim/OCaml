#load "graphics.cma";;
open Graphics;;
open_graph "";;

let rec koch x1 y1 x5 y5 = function
	|0 -> moveto x1 y1; lineto x5 y5
	|n -> 
		let x2=(x1+(x5-x1)/3) and y2=(y1+(y5-y1)/3) in
			let x4=(x1+2*(x5-x1)/3) and y4=(y1+2*(y5-y1)/3) in
				let x3 = int_of_float ((float_of_int x2+.float_of_int x4)/.2. -. (float_of_int y4-.float_of_int y2)*.(sqrt(3.)/.2.))
				and y3 = int_of_float ((float_of_int y2+.float_of_int y4)/.2. +. (float_of_int x4-.float_of_int x2)*.(sqrt(3.)/.2.)) in
					koch x1 y1 x2 y2 (n-1);
					koch x2 y2 x3 y3 (n-1);
					koch x3 y3 x4 y4 (n-1);
					koch x4 y4 x5 y5 (n-1);;

koch 300 530 900 530 4;;
koch 900 530 600 10 4;;
koch 600 10 300 530 4;;