
'compile with "c:\freebasic\fbc.exe" -x "c:\src\sfx_create.exe" -s gui "c:\src\sfx_create.bas" "c:\src\sfx_create.rc"

'#define _DEBUG_FB   'if using this option show information

#include once ".\ntdll_compress.inc"


/'  ' to remind the content of sfx_create.rc
	//_BEGIN_RC_
	LANGUAGE LANG_ENGLISH, SUBLANG_ENGLISH_US

	SFX_EXE      RCDATA    "sfx.exe"
	//_END_RC_
'/

#ifdef _DEBUG_FB
	AllocConsole()
#ENDIF

'get ressource from existing executable file and save it as file
Private Function ImportFile(byref ResName As String, byref NewFile As String) As Long
   dim hRes              AS HRSRC
   dim hLoad             AS HGLOBAL
   dim ulen              AS uLong
   dim pBuff             AS uByte ptr
   dim lFile             AS Long

   hRes = FindResource(NULL, StrPtr(ResName), RT_RCDATA)
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

'put ressource into existing executable file
Private Function Put_Resource(byref datFile As String, byref exeFile As String, byref szResName as string) As Long
   Dim hUpdateRes       As HANDLE
   Dim pBuf             As uByte ptr
	Dim ilen					as Long
	Dim myF					as Long

   hUpdateRes = BeginUpdateResource(ExeFile, 0)
   If hUpdateRes = 0 Then return -1
	myF = Freefile()
   If Open(DatFile For Binary as #myF) Then return -2
   ilen = LOF(myF)
   If ilen = 0 Then
      Close #myF
      return -3
   End If
   pBuf = Allocate(ilen)
   Get #myF, , *pBuf, ilen
   Close #myF

   If UpdateResource(hUpdateRes, RT_RCDATA, StrPtr(szResName), _
			MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US), pBuf, ilen) = 0 Then
		deallocate(pBuf)
		return -4
	end if
	deallocate(pBuf)
   If EndUpdateResource(hUpdateRes, 0) = 0 Then return -5
   return 0
End Function

DEBUG_FB("command " & command)
if command = "" or command(1) = "" or command(2) = "" THEN
	messagebox(0, chr(10,10,10) & "Not enought parameters to create sfx executable, aborting!" & _
		chr(10,10,10) & "usage : " & chr(10) & "    sfx_create list_of_files_tocompress destination_file" & _
		chr(10,10) & "    list_of_files_tocompress   separated files by '|'   (e.g.  f1.exe|f2.txt)  ", "Missing Paramaters", MB_ICONERROR)
	end
end if


if my_Zip(command(1), ZgetTempPath() & "my_temp_zip.par" ) = 0 then
	messagebox(0, chr(10,10) & "Problem at compression, aborting!", "Error", MB_ICONERROR)
	end
end if
DEBUG_FB("myZip done")

dim imp1 as long = ImportFile( "SFX_EXE" , command(2))
DEBUG_FB("imp1 = " & imp1)
if imp1 then
	messagebox(0, chr(10,10) & "Problem : " & imp1 & " creating sfx, aborting!", "Error", MB_ICONERROR)
	end
end if
DEBUG_FB("ImportFile done")

if Put_Resource(ZgetTempPath() & "my_temp_zip.par", command(2), "MYRCDATA") then
	messagebox(0, chr(10,10) & "Problem finishing sfx, aborting!", "Error", MB_ICONERROR)
	kill ZgetTempPath() & "my_temp_zip.par"
	kill command(2)
	end
end if
DEBUG_FB("Put_Resource done")

kill ZgetTempPath() & "my_temp_zip.par"
DEBUG_FB("cleaning all")

messagebox(0, chr(10,10) & command(2) & chr(10,10) & "is created!", "Done", MB_ICONINFORMATION)

