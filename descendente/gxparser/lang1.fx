//basic types bool, int (64 bits), Coord(int x, int y)
//literals are of type int, 2, 3, or 0x2dfadfd
//literals of Coord are [3,4] [0x46,4]
//literals of bool are True, False
//operators of int are + - * / ** > >= < <=
//operators of int are %
//operators of bool are | & ! ^
//precedence is like in C, with ** having the
//same precedence as sizeof (not present in fx)

type record vector(int x, int y, int z)
type record difficult (vector v, Coord r)

//builtins
//circle(p, 2, 0x1100001f);
//	at point p, int radius r, color: transparency and rgb
//rect(p, α, col);
//	at point p, int angle (degrees),
//	color: transparency (0-100) and rgb

//macro definition
func line(vector v, int t){
	Coord p;	//only local variables, no globals

				//last number in the loop is the step
	iter (i := 0, v.z, 2){	//declares de variable only in the loop
		p.x = v.x*t;
  i = 0;
		circle(p, 2, 1);
		p.y = v.y*t;
	}
}

//macro entry
func main(){
	vector v;
	Coord pp;

	v.x = 3;
	v.y = 8;
	v.z = 2;
	pp = [4,5];
	if(v.x <= 3 | True) {		// (v.x>3)|True
		circle(pp, 2, 0x1100001f);
	} else {
		line(v);
		line(v);
	}
	line(v);
	line(v);
	iter (i := 0, 3, 1){		//loops 0 1 2 3
		rect(pp, 5, 0xff);
	}
}
