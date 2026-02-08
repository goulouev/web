        //WAVES
        function kf(iUnit, f) {
            if (iUnit === 0) {
                return 0.0209579 * f
            } else {
                return 0.5323312 * f
            }
        }

        function beta(k, x) {
            var g2 = Array(x * x - k[0] * k[0] + k[1] * k[1], -2 * k[0] * k[1]);
            var g = csqrt(g2);
            return Array(g[1], -g[0]);
        }

        function yw(mode, k, beta) {
            if (mode === 0) {
                return xdy(beta, k);
            } else {
                return xdy(k, beta);
            }
        }

        //

        function phase(bt,d) {
            // -j*bt*d
            return [bt[1]*d,-bt[0]*d];
        }

        function Snode(bt,d){
            // phase=exp(-j*beta*d)
            var ph = cexp(phase(bt,d));
            return [0, 0, ph[0], ph[1], ph[0], ph[1], 0, 0];
        }

        function SY2(Y0, Yo, Y1) {
            var Y00 = xdy(Array(Yo[0], Yo[1]), Y0);
            var Y01 = xdy(xdy(Array(Yo[2], Yo[3]), csqrt(Y0)), csqrt(Y1));
            var Y11 = xdy(Array(Yo[4], Yo[5]), Y1);
            var Dx = xmy(xoy(Y00, Y11), xoy(Y01, Y01));
            var DET = Array(1 + Y11[0] + Y00[0] + Dx[0], Y11[1] + Y00[1] + Dx[1]);
            var S11 = xdy(Array(1 - Y00[0] + Y11[0] - Dx[0], - Y00[1] + Y11[1] - Dx[1]), DET);
            var S22 = xdy(Array(1 + Y00[0] - Y11[0] - Dx[0], Y00[1] - Y11[1] - Dx[1]), DET);
            var S12 = aox(-2, xdy(Y01, DET));
            return Array(S11[0], S11[1], S12[0], S12[1], S12[0], S12[1], S22[0], S22[1]);
        }

        function Shunt(Y0, jB, Y1) {
            var Y = Array(Y0[0] + Y1[0] + jB[0], Y0[1] + Y1[1] + jB[1]);
            var R = xdy(Array(Y0[0] - Y1[0], Y0[1] - Y1[1]), Y);
            var dR = xdy(jB, Y);
            var nrm = xdy(csqrt(Y1), csqrt(Y0));
            var T = aox(2, xoy(xdy(Y0, Y), nrm));
            var S11 = Array(R[0] - dR[0], R[1] - dR[01]);
            var S22 = Array(-R[0] - dR[0], -R[1] - dR[01]);
            return Array(S11[0], S11[1], T[0], T[1], T[0], T[1], S22[0], S22[1]);
        }

        function Cascade(S0, S1) {
            var U = Array(1, 0);
            var S011 = Array(S0[0], S0[1]);
            var S012 = Array(S0[2], S0[3]);
            var S022 = Array(S0[6], S0[7]);
            var S111 = Array(S1[0], S1[1]);
            var S112 = Array(S1[2], S1[3]);
            var S122 = Array(S1[6], S1[7]);
            var RS = xmy(U, xoy(S022, S111));
            var So11 = xpy(S011, xdy(xoy(S111, xin2(S012)), RS));
            var So22 = xpy(S122, xdy(xoy(S022, xin2(S112)), RS));
            var So12 = xdy(xoy(S012, S112), RS);
            return Array(So11[0], So11[1], So12[0], So12[1], So12[0], So12[1], So22[0], So22[1]);
        }

 

