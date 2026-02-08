
'Q=1.0e9
'T0=-80
'T1=-80

'rr=SpikeMax(T0,T1,Q)

'msgbox rr


Public Function Finter(x,Na,xa,ya)
if x<xa(0) then Finter=ya(0): exit function
if x>xa(Na) then Finter=ya(Na): exit function
for i=0 to Na-1
x0=xa(i) : x1=xa(i+1)
if x >= xa(i) and x =< xa(i+1) then
y0=ya(i): y1=ya(i+1)
exit for
end if
next
Finter=y0+(y1-y0)/(x1-x0)*(x-x0)
end function

Public Function Connect(TdB0,TdB1,Q,w)
T0=10.^(TdB0/20.):T1=10.^(TdB1/20.)
R0=Sqr(1.-T0*T0):R1=Sqr(1.-T1*T1)
RR=R0*R1*Exp(-w/Q)
Tc=T0*T1/sqr(1.+RR*(RR-2.*Cos(2.*w)))
Connect=20.*Log(Tc)/Log(10.)
End Function

Public Function SpikeMin(TdB0,TdB1,Q)
T0=10.^(TdB0/20.):T1=10.^(TdB1/20.)
R0=Sqr(1.-T0*T0):R1=Sqr(1.-T1*T1)
RR=R0*R1*Exp(-6.28/Q)
xM=1.+RR
xMdB=20.*Log(xM)/Log(10.)
SpikeMin=TdB0+TdB1-xMdB
End Function

Public Function SpikeMax(TdB0,TdB1,Q)
T0=10.^(TdB0/20.):T1=10.^(TdB1/20.)
R0=Sqr(1.-T0*T0):R1=Sqr(1.-T1*T1)
RR=R0*R1*Exp(-6.28/Q)
xM=1.-RR
if xM=empty or xM <1.e-12 then 
SpikeMax=0. 
else 
xMdB=20.*Log(xM)/Log(10.)
SpikeMax=TdB0+TdB1-xMdB 
end if

End Function

        Public function kf(unit,f)
        if unit=0 then
        kf=0.0209579*f
        else
        kf=0.5323312*f
        end if
        end function


