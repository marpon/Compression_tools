'compile with "c:\freebasic\fbc.exe" -x "c:\src\test_ntdll_Lzn.exe" -s gui "c:\src\test_ntdll_Lzn.bas"

#define _DEBUG_FB   'if using this option show information

#include once ".\ntdll_compress.inc"

#ifdef _DEBUG_FB
	dim as string stitle = space(520)
	if GetConsoleTitle(strptr(stitle), 520) = 0 then
		AllocConsole()
		? "mode gui , but now console is added"
	else
		? "mode console"
	end if
#ENDIF
' usage of my_zip
'function my_zip( list_of_separated_files as string, destination_file as string) as long (nb of compressed elements)

'compress 1 file only
my_zip("test_ntdll_lzn.bas", "single.par")

'compress 2 files (even pre compressed ) notice the separator |  to separate the file path/names
my_zip("test_ntdll_lzn.bas|single.par", "double.par")

'compress 3 files (even pre compressed ) notice the separator |  to separate the file path/names
my_zip("test_ntdll_lzn.bas|single.par|double.par", "triple.par")

#ifdef _DEBUG_FB
messagebox(0, "Click to continue", get_FileName(command(0)) & "  :  Waiting...", MB_ICONINFORMATION)
#endif