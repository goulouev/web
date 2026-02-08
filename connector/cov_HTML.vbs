Call Main

Sub Main()
filo=inputbox("Enter project file name","Open File","project.txt")
if trim(filo)="" then exit sub
call OpenFile(Filo, TextInside)
V=split(TextInside,vbCrLf)
Nv=ubound(V)
RV="<HTML>"
RV=RV&"<SCRIPT LANGUAGE=VBSCRIPT>"&vbCrLf
RV=RV&"Sub Window_OnLoad()"&vbCrLf
RV=RV&"Dim V,VV"&vbCrLf
RV=RV&"VV=TL()"&vbCrLf
RV=RV&"V=split(VV,"&chr(34)&"]["&chr(34)&")"&vbCrLf
RV=RV&"Top.InputDat.Value=V(1)"&vbCrLf
RV=RV&"Top.StrucDat.value=V(2)"&vbCrLf
RV=RV&"Top.ProtoDat.Value=V(3)"&vbCrLf
RV=RV&"Top.OptDat.Value=V(4)"&vbCrLf
RV=RV&"Top.TemplateDat.value=V(5)"&vbCrLf
RV=RV&"Window.Navigate "&chr(34)&"editor.htm"&chr(34)&""&vbCrLf
RV=RV&"End Sub"&vbCrLf
RV=RV&"Function TL()"&vbCrLf
for i=0 to Nv
RV=RV&"TL=TL"&chr(38)&chr(34)&V(i)&chr(34)&chr(38)&"vbCrLf"&vbCrLf
next
RV=RV&"End Function"&vbCrLf
RV=RV&"</SCRIPT>"&vbCrLf
RV=RV&"</HTML>"
aFilo=split(filo,".")
filo=inputbox("Enter project file name","Open File",Trim(aFilo(0))&".htm")
if trim(filo)="" then exit sub
call SaveFile(Filo, RV)
End Sub


Function WordLineCorrect(Median,TextLine)
dim VV,Nvv,iv,icount,N,InMark,i,r,dLine,A
N=len(TextLine)
InMark=True
for i=1 to N
r=mid(TextLine,i,1)
if r=chr(9) then r=" "
dLine=r
if InMark and r=" " then dLine="" end if
if r=" " then InMark=True else InMark=False end if
A=A&dLine
next
if Median =" " then 
WordLineCorrect=trim(A)
else
WordLineCorrect=replace(trim(A)," ",Median)
end if
end function

sub OpenFile(FiloName, TextInside)
Dim fs, f
Set fs=CreateObject("Scripting.FileSystemObject")
Set f=fs.OpenTextFile(FiloName, 1)
TextInside=ltrim(rtrim(f.ReadAll))
f.Close
Set f=Nothing
Set fs=Nothing
end sub

Sub SaveFile(FiloName, TextInside)
Dim fs, f
set fs=Wscript.CreateObject ("Scripting.FileSystemObject")
Set f = fs.CreateTextFile(FiloName, True)
f.Write(TextInside)
Set f=Nothing
Set fs=Nothing
End sub

