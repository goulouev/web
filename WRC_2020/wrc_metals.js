function metal() {
    this.sigma = 6.1;
    this.Zo = 376.7;
    this.iUnit = 1;
}
metal.prototype.SkinDepth = function (k) {
    if (this.sigma < 1E-6) {
        return 0;
    } else {
        var xx = 1 / Math.sqrt(k * this.sigma)
        if (iUnit === 0) {
            return 0.00072864701 * xx;
        } else {
            return 0.0001445774 * xx;
        }
    }
};

metal.prototype.Rs = function (k) {
    if (this.sigma < 1E-6) {
        return 0;
    } else {
        var xx = 1 / Math.sqrt(k * this.sigma)
        if (this.iUnit === 0) {
            return 0.13724066 * Math.sqrt(k / this.sigma);
        } else {
            return 0.02723115 * Math.sqrt(k / this.sigma);
        }
    }
};

metal.prototype.kReso = function (mode, nx, ny, nz, a, b, d) {
    function eps(n) {
        if (n === 0) { return 2; } else { return 1; }
    }
    var delta;
    var vx = Math.PI * nx / a;
    var vy = Math.PI * ny / b;
    var vz = Math.PI * nz / d;
    var k = Math.sqrt(vx * vx + vy * vy + vz * vz);
    if (this.sigma < 1E-6) {
        delta = 0;
    } else {
        if (mode === 0) {
            var su0 = (vx * vx + vy * vy) / eps(nz) / d + vx * vx / eps(ny) / b + vy * vy / eps(nx / a);
            var su1 = 1 / eps(nx) / a + 1 / eps(ny) / b;
            var g2 = k * k - vz * vz;
            delta = 4 * this.Rs(k) / k / this.Zo * (vz * vz / g2 * su0 + g2 * su1);
        } else {
            var su0 = 1 / eps(nz) / d + 1 / a;
            var su1 = 1 / eps(nz) / d + 1 / b;
            var g2 = k * k - vz * vz;
            delta = 4 * k * this.Rs(k) / this.Zo * (vx * vx * su0 + vy * vy * su1) / g2;
        }
    }
    var x = [k * k, delta];
    var z = Math.sqrt(x[0] * x[0] + x[1] * x[1]);
    if (x[1] < 0) {
        return [Math.sqrt((z + x[0]) / 2), -Math.sqrt((z - x[0]) / 2)];
    } else {
        return [Math.sqrt((z + x[0]) / 2), Math.sqrt((z - x[0]) / 2)];
    }
};
