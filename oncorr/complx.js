//'=================== COMPLEX FUNCTION MATH

function cmplx(x,y)
{
return Array(x,y);
}

function csqrt(x) {
    var z = Math.sqrt(x[0] * x[0] + x[1] * x[1]);
    if (x[1] < 0) {
        return Array(Math.sqrt((z + x[0]) / 2), -Math.sqrt((z - x[0]) / 2));
    } else {
        return Array(Math.sqrt((z + x[0]) / 2), Math.sqrt((z - x[0]) / 2));
    }
}

//Public Function csqrt(ByVal x As Complex) As Complex
//Dim z As Double
//z = Math.Sqrt(x.Re * x.Re + x.Im * x.Im)
//If x.Im < 0 Then
//csqrt = New Complex(System.Math.Sqrt((z + x.Re) / 2), -System.Math.Sqrt((z - x.Re) / 2))
//Else
//csqrt = New Complex(System.Math.Sqrt((z + x.Re) / 2), System.Math.Sqrt((z - x.Re) / 2))
//End If
//End Function
function xin2(x) {
    return Array(x[0]*x[0]-x[1]*x[1],2*x[0]*x[1]);
}

function xpy(x, y)
{
return Array(x[0] + y[0], x[1] + y[1]);
}

function xmy(x, y)
{
return Array(x[0] - y[0], x[1] - y[1]);
}

function xoy(x, y)
{
return Array(x[0] * y[0] - x[1] * y[1], x[0] * y[1] + x[1] * y[0]);
}

function xdy(x, y)
{
var M= y[0] * y[0] + y[1] * y[1];
return Array((x[0] * y[0] + x[1] * y[1]) / M, (-x[0] * y[1] + x[1] * y[0]) / M);
}

function cexp(x)
{
var a = Math.exp(x[0]);
return Array(a * Math.cos(x[1]), a * Math.sin(x[1]));
}

function aox(a,x)
{
return Array(a*x[0],a*x[1]);
}

function cabs(x)
{
return Math.sqrt(x[0] * x[0] + x[1] * x[1]);
}


function arg(x)
{
var pi= 3.141592653589793238;
var pi2=1.570796326794896619;
var up = x[1] + x[0];
var um = x[1] - x[0];
if (up > 0 && um > 0)
   {
   return pi2 - Math.atan(x[0] / x[1]);
   }
   if (up >= 0 && um <= 0)
   {
   return Math.atan(x[1] / x[0]);
   }
   if (up < 0 && um < 0)
   {  
   return -pi2 - Math.atan(x[0] / x[1]);
   }
   if (up <= 0 && um >= 0)
   {
      if (x[1] >= 0)
      {
      return pi + Math.atan(x[1] / x[0]);
      }
      else
      {
      return -pi + Math.atan(x[1] / x[0]);
      }
   }
}

function dB(x)
{
var R;
R = cabs(x);
if (R > 1E-20)
{
return 20.*Math.log(R)/Math.LN10;

}
else
{
return -400;
}
}
