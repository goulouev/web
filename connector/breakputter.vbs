Call Main

Sub Main
FL=inputbox("Enter file name where you want to HTML breaks","Open File","")
Call OpenFile(FL,TextIn)
TextLines=split(TextIn,vbCrLf)
Np=ubound(TextLines)
for i=0 to Np-1
Nx=len(TextLines(i))
if Nx>5 and Nx<50 then TextLines(i)="<h2>"&TextLines(i)&"</h2>"
TextLines(i)=TextLines(i)&"<BR>"
next
TextOut=join(TextLines,vbCrLf)
FL=inputbox("Enter corrected file name","Save As",FL)
if trim(FL)="" then exit sub
Call SaveFile(FL,TextOut)
End Sub

'########################### OTHER
private sub WordExtract(TextLine,Nv,V)
dim VV,Nvv,iv,icount
VV=split(TextLine," ")
Nvv=ubound(VV)
iv=-1
for icount=0 to Nvv
if ltrim(rtrim(VV(icount)))<>"" then
iv=iv+1
V(iv)=VV(icount)
end if
next
Nv=iv
end sub

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


'########################### COMPLEX FUNCTION MATH
