
var M = function () {

    this.mat = [];
    this.mat = [];
    this.ptr= 0;
    this.mat[0] = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1];

    this.push = function (arg){
        this.mat[this.ptr + 1] = arg;
        this.ptr++;
    }

    this.pop = function () {
        this.ptr--;
    }

    this.peek = function () {
        return this.mat[this.ptr];
    }

    this.rotate = function (a, ax) {
        a = a * Math.PI / 180;
        c = Math.cos(a);
        s = Math.sin(a);

        if (ax == 1)
            this.push([1, 0, 0, 0,
                    0, c, s, 0,
                    0, -s, c, 0,
                    0, 0, 0, 1]);
        if (ax == 2)
            this.push([c, 0, -s, 0,
                    0, 1, 0, 0,
                    s, 0, c, 0,
                    0, 0, 0, 1]);
        if (ax == 3)
            this.push([c, s, 0, 0,
                    -s, c, 0, 0,
                    0, 0, 1, 0,
                    0, 0, 0, 1]);
    }

    this.scale=function(a, b, c) {

        this.push([a, 0, 0, 0,
                0, b, 0, 0,
                0, 0, c, 0,
                0, 0, 0, 1]);
    }

    this.translate=function(a, b, c) {

        this.push([1, 0, 0, 0,
                0, 1, 0, 0,
                0, 0, 1, 0,
                a, b, c, 1]);
    }

    this.multiply=function(q, p) {

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
    }

    this.multiplymat = function (x) {
        this.mat[this.ptr] = this.multiply(this.peek(), x);
    }

    this.getMatrix = function () {
        
        var a = [];
        a=this.peek();

        for (var v = this.ptr-1 ; v >=1 ; v -= 1) {
            a = this.multiply(this.mat[v],a);
        }

        return a;
    }
}