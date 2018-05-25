//basic types bool, int (64 bits), Coord(int x, int y)
//literals are of type int, 2, 3, or 0x2dfadfd
//literals of bool are True, False
//operators of int are + - * / ** > >= < <= %
//operators of bool are | & 
//precedence is like in C, with ** having the
//precedence
//1. ** !
//2. * / % &
//3. + -  ^ |
//4. <= >= < >

//builtins
//circle(x, y, 2, 0x1100001f);
//	at point p, int radius r, color: transparency and rgb
//rect(x, y, alpha, col);
//	at point x, y, int angle (degrees),
//	color: transparency (0-100) and rgb

//macro definition
func line(int vx, int vy, int vz){
	int x;
	int y;

	//last number in the loop is the step
	iter (i := 0, vz, 2){	//declares de variable only in the loop
		x = vx*i;
		y = vy*i;
		circle(x, y, 2, 1);
	}
 i=-3;
}

//macro entry
func main(){
	int vx;
	int vy;
	int vz;
	int px;
	int py;

	vx = 3;
	vy = 8;
	vz = 2;
	px = 4;
	py = 45;
	if(vx > 3 | True) {		// (vx>3)|True
		circle(px, py, 2, 0x1100001f);
	} else {
		line(vx, vy, vz);
		line(vx, vy, vz);
	}
	line(vx, vy, vz);
	line(vx, vy, vz);
	iter (i := 0, 3, 1){		//loops 0 1 2 3
		rect(px, py, , 5, 0xff);
	}
}
