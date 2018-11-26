'compile with "c:\freebasic\fbc.exe" -x "c:\src\sfx.exe" -s gui "c:\src\sfx.bas"

'#define _DEBUG_FB     'if using this option show information
#include once ".\ntdll_compress.inc"

#ifdef _DEBUG_FB
	AllocConsole()
#ENDIF

Private Function ImportFile(byref ResName As String , byref NewFile As String) As Long
   dim hRes              AS HRSRC
   dim hLoad             AS HGLOBAL
   dim ulen              AS uLong
   dim pBuff             AS uByte ptr
   dim lFile             AS Long

   hRes = FindResource(NULL ,StrPtr(ResName), RT_RCDATA)
   If hRes = NULL Then Return - 1
   ulen = SizeofResource(NULL , hRes)
   If ulen = 0 Then Return - 2
   hLoad = LoadResource(NULL , hRes)
   If hLoad = NULL Then Return - 3
   pBuff = LockResource(hLoad)
   If pBuff = NULL Then Return - 4
   lFile = Freefile()
   If Open(NewFile For Binary Access Write As #lFile) Then Return - 5
   Put #lFile, , *pBuff, ulen
   Close #lFile
	Sleep 20
   Return 0
End Function

DEBUG_FB(ZgetTempPath() & "my_temp_zip.par")
if ImportFile( "MYRCDATA" , ZgetTempPath() & "my_temp_zip.par") then
	DEBUG_FB("ImportFile failed!")
	end
end if
DEBUG_FB("ImportFile done")

DEBUG_FB("command = " & command)


dim as long iret
if command <> "" THEN
	DEBUG_FB("destination = " & command)
	iret = my_UnZip( ZgetTempPath() & "my_temp_zip.par" , command)

else
	DEBUG_FB("destination = " & exepath())
	iret = my_UnZip( ZgetTempPath() & "my_temp_zip.par" , exepath())
END IF

if iret then
	DEBUG_FB("my_UnZip done")
else
	DEBUG_FB("my_UnZip failed!")
end if
kill ZgetTempPath() & "my_temp_zip.par"
DEBUG_FB("cleaning done")

#ifdef _DEBUG_FB
messagebox(0, "Click to continue", get_FileName(command(0)) & "  :  Waiting...", MB_ICONINFORMATION)
#endif
