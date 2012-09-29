#load "graphics.cma";;
open Graphics;;
open_graph "";;

let pi=3.1415926535897932384626433832795;;

let ctp (x,y) = let x = float_of_int x and y = float_of_int y in
	( sqrt( x *. x +. y *. y ) , (
		if x > 0. then
			if y>=0. then
				atan (y/.x)
			else
				atan (y/.x) +. 2.*.pi
		else
			if x < 0. then
				atan (y/.x) +. pi
			else
				if y > 0. then pi/.2. else 3.*.pi/.2. 
	));;

	
let rec koch x1 y1 x5 y5 = function
	|0 -> moveto x1 y1; lineto x5 y5
	|n -> 
		let a = float_of_int (x5-x1) and b = float_of_int (y5-y1) in
		let d = sqrt(a*.a+.b*.b) in
		let x2=(x1+(x5-x1)/3) and y2=(y1+(y5-y1)/3) in
			 let (r,teta) = ctp ((x5-x1),(y5-y1)) in
			 	let x3 = x2 + int_of_float((d/.3.)*.cos(pi/.3.+.teta)) and y3 = y2 + int_of_float((d/.3.)*.sin(pi/.3.+.teta)) in
				let x4=(x1+2*(x5-x1)/3) and y4=(y1+2*(y5-y1)/3) in
					koch x1 y1 x2 y2 (n-1);
					koch x2 y2 x3 y3 (n-1);
					koch x3 y3 x4 y4 (n-1);
					koch x4 y4 x5 y5 (n-1);;

koch 300 530 900 530 4;;
koch 900 530 600 10 4;;
koch 600 10 300 530 4;;