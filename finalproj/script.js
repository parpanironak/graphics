
var M = function () {

    this.mat = [];
    this.mat = [];
    this.ptr = 0;
    this.mat[0] = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1];

    this.push = function (arg) {
        this.mat[this.ptr + 1] = arg;
        this.ptr++;
    };
    this.eye = function () {
        return [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1];
    };

    this.pop = function () {
        this.ptr--;
    };

    this.peek = function () {
        return this.mat[this.ptr];
    };

    this.rotate = function (a, ax) {
        a = a * Math.PI / 180;
        c = Math.cos(a);
        s = Math.sin(a);

        if (ax == 1)
            this.multiplymat([1, 0, 0, 0,
                              0, c, s, 0,
                              0, -s, c, 0,
                              0, 0, 0, 1]);

        if (ax == 2)
            this.multiplymat([c, 0, -s, 0,
                              0, 1, 0, 0,
                              s, 0, c, 0,
                              0, 0, 0, 1]);
        if (ax == 3)
            this.multiplymat([c, s, 0, 0,
                              -s, c, 0, 0,
                              0, 0, 1, 0,
                              0, 0, 0, 1]);
    };

    this.scale = function (arr) {

        var a = arr[0],
            b = arr[1],
            c = arr[2];

        this.push([a, 0, 0, 0,
                0, b, 0, 0,
                0, 0, c, 0,
                0, 0, 0, 1]);
        
    };

    this.translate = function (arr) {
        var a = arr[0],
            b = arr[1],
            c = arr[2];
        this.push([1, 0, 0, 0,
                0, 1, 0, 0,
                0, 0, 1, 0,
                a, b, c, 1]);

        this.push(this.eye());
    };

    this.scale1 = function (a, b, c) {

        this.push([a, 0, 0, 0,
                0, b, 0, 0,
                0, 0, c, 0,
                0, 0, 0, 1]);
    };

    this.translate1 = function (a, b, c) {

        this.push([1, 0, 0, 0,
                0, 1, 0, 0,
                0, 0, 1, 0,
                a, b, c, 1]);

        this.push(this.eye());
    };

    this.multiply = function (q, p) {

        var m = [];

        m[0] = q[0] * p[0] + q[4] * p[1] + q[8] * p[2] + q[12] * p[3];
        m[1] = q[1] * p[0] + q[5] * p[1] + q[9] * p[2] + q[13] * p[3];
        m[2] = q[2] * p[0] + q[6] * p[1] + q[10] * p[2] + q[14] * p[3];
        m[3] = q[3] * p[0] + q[7] * p[1] + q[11] * p[2] + q[15] * p[3];
        m[4] = q[0] * p[4] + q[4] * p[5] + q[8] * p[6] + q[12] * p[7];
        m[5] = q[1] * p[4] + q[5] * p[5] + q[9] * p[6] + q[13] * p[7];
        m[6] = q[2] * p[4] + q[6] * p[5] + q[10] * p[6] + q[14] * p[7];
        m[7] = q[3] * p[4] + q[7] * p[5] + q[11] * p[6] + q[15] * p[7];
        m[8] = q[0] * p[8] + q[4] * p[9] + q[8] * p[10] + q[12] * p[11];
        m[9] = q[1] * p[8] + q[5] * p[9] + q[9] * p[10] + q[13] * p[11];
        m[10] = q[2] * p[8] + q[6] * p[9] + q[10] * p[10] + q[14] * p[11];
        m[11] = q[3] * p[8] + q[7] * p[9] + q[11] * p[10] + q[15] * p[11];
        m[12] = q[0] * p[12] + q[4] * p[13] + q[8] * p[14] + q[12] * p[15];
        m[13] = q[1] * p[12] + q[5] * p[13] + q[9] * p[14] + q[13] * p[15];
        m[14] = q[2] * p[12] + q[6] * p[13] + q[10] * p[14] + q[14] * p[15];
        m[15] = q[3] * p[12] + q[7] * p[13] + q[11] * p[14] + q[15] * p[15];

        return m;
    };

    this.multiplymat = function (x) {
        this.mat[this.ptr] = this.multiply(this.peek(), x);
    };

    this.getMatrix = function () {

        var a = [];
        a = this.peek();

        for (var v = this.ptr - 1 ; v >= 1 ; v -= 1) {
            a = this.multiply(this.mat[v], a);
        }

        return a;
    };
};




var PI = Math.PI;
function cos(t) { return Math.cos(t); }
function dot(a, b) { return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]; }
function floor(t) { return Math.floor(t); }
function abs(t) { return Math.abs(t); }
function lerp(t, a, b) { return a + t * (b - a); }
function min(a, b) { return Math.min(a, b); }
function max(a, b) { return Math.max(a, b); }
function sCurve(t) { return t * t * (3 - t - t); }
function sin(t) { return Math.sin(t); }
function sqrt(t) { return Math.sqrt(t); }
function addv(a,b) { return [a[0]+b[0],a[1]+b[1],a[2]+b[2]]; }
function addvs(a,b) { return [a[0]+b,a[1]+b,a[2]+b]; }
function subv(a,b) { return [a[0]-b[0],a[1]-b[1],a[2]-b[2]]; }
function mulv(a,b) { return [a[0]*b,a[1]*b,a[2]*b]; }
function divv(a,b) { return [a[0]/b,a[1]/b,a[2]/b]; }
function magv(a) { return Math.sqrt(a[0]*a[0]+a[1]*a[1]+a[2]*a[2]); }
function normalize(a) {
	m=magv(a);
	return divv(a, m);
}
function limit(a,b) {
	m=magv(a);
	if(m>b)
		return mulv(a,b/m);
	else
		return a;
}
function absv(a)
{
	return [abs(a[0]),abs(a[1]),abs(a[2])];
}

function filterVertex(p,h)
{
	var r= sqrt(p[0]*p[0]+p[1]*p[1]);
	r=r/0.7*Math.PI;
	if (p[2]>1.)
	{	
		
		p[2]=0.85-0.2*cos(r);
		
	}
		return p;
}