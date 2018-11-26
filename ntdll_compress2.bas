#include once "windows.bi"

' These are already defined in winnt.bi:
'
'#define COMPRESSION_FORMAT_LZNT1 2 ' the only format currently supported
'#define COMPRESSION_ENGINE_STANDARD 0 ' the first of 2 engines currently supported
'#define COMPRESSION_ENGINE_MAXIMUM &H100 ' and the second supported



#define _SUCCESS_NTDLL 						&h00000000
#define _INVALID_PARAMETER_NTDLL 		&hC000000D
#define _NOT_SUPPORTED_NTDLL 				&hC00000BB
#define _UNSUPPORTED_COMPRESSION_NTDLL &hC000025F
#define _BUFFER_ALL_ZEROS_NTDLL 			&h00000117
#define _NOT_SUPPORTED_NTDLL 				&hC00000BB
#define _BUFFER_TOO_SMALL_NTDLL 			&hC0000023
#define _BAD_COMPRESSION_BUFFER_NTDLL 	&hC0000242

#define _SOURCE_FILE_ERROR 				&h00000001
#define _DESTINATION_FILE_ERROR 			&h00000002

#define _MAX_BUFFER_SIZE_NTDLL			&h0FFFFFFF   	' can be modified up to ULONG Max

type my_info_ntdll                               		' gCollect
      dim         as long uncomp_size
      dim         as long comp_size
      dim         as long elapsed
      dim         as long comp_dec
END TYPE

dim shared as my_info_ntdll gCollect             		' to collect the different info


Extern "Windows" Lib "ntdll"
   declare function RtlGetCompressionWorkSpaceSize alias "RtlGetCompressionWorkSpaceSize"( _
         byval dwCompressionFormatAndEngine as Ulong , _
         byval lpdwCompressBufferWorkSpaceSize as Ulong ptr , _
         byval lpdwCompressFragmentWorkSpaceSize as Ulong ptr) _
         as ULong

   declare function RtlCompressBuffer alias "RtlCompressBuffer"( _
         byval dwCompressionFormatAndEngine as Ulong , _
         byval lpUnCompressedBuffer as any Ptr , _
         byval dwUnCompressedBufferSize As Ulong , _
         byval lpCompressedBuffer as any Ptr , _
         byval dwCompressedBufferSize As Ulong , _
         byval dwUnCompressedChunkSize As Ulong , _
         byval lpdwFinalCompressedSize as Ulong ptr , _
         byval lpCompressBufferWorkspace as any Ptr) _
         as ULong

   Declare Function RtlDecompressBuffer alias "RtlDecompressBuffer"( _
         byval dwCompressionFormat as Ulong , _
         byval lpUnCompressedBuffer as any Ptr , _
         byval dwUnCompressedBufferSize As Ulong , _
         byval lpCompressedBuffer as any Ptr , _
         byval dwCompressedBufferSize As Ulong , _
         byval lpdwFinalDecompressedSize as Ulong ptr) _
         as ULong
end extern

'*******************************************************************************************

private sub clean_global()
   gCollect.uncomp_size = 0
   gCollect.comp_size = 0
   gCollect.elapsed = 0
end sub

private function NTCompressFile(Byval uncompressedFileName as zstring ptr , _
         Byval compressedFileName as zstring ptr , _
         Byval engine as ULong = COMPRESSION_ENGINE_STANDARD) _
         as ULong

   dim hFileU            as HANDLE
   dim lofU              as ULong
   dim hMMFU             as HANDLE
   dim lpMemU            as any Ptr
   dim hFileC            as HANDLE
   dim lofC              as ULong
   dim hMMFC             as HANDLE
   dim lpMemC            as any Ptr
   dim workSpaceSize     as ULong
   dim lpWorkSpace       as any Ptr
   dim finalSize         as ULong
   dim rVal              as ULong
   dim junk              as ULong
   dim start             as double
   dim finish            as double

   clean_global()
   gCollect.comp_dec = 1
   start = timer

   ' Open the uncompressed file.
   hFileU = CreateFile(uncompressedFileName , _
         GENERIC_READ or GENERIC_WRITE , _
         FILE_SHARE_READ or FILE_SHARE_WRITE , _
         null , _
         OPEN_EXISTING , _
         FILE_ATTRIBUTE_NORMAL , _
         null)

   if hFileU = INVALID_HANDLE_VALUE then
      function = _SOURCE_FILE_ERROR
      exit function
   end if

   ' Get the size of uncompressed file (low-order Ulong only).
   lofU = GetFileSize(hFileU , null)

   '? "size = ";lofU

   ' Open the compressed file.
   hFileC = CreateFile(compressedFileName , _
         GENERIC_READ or GENERIC_WRITE , _
         FILE_SHARE_READ or FILE_SHARE_WRITE , _
         null , _
         OPEN_ALWAYS , _
         FILE_ATTRIBUTE_NORMAL , _
         null)

   if hFileC = INVALID_HANDLE_VALUE then
      return _DESTINATION_FILE_ERROR
   end if

   if lofU = 0 THEN
      CloseHandle(hFileU)
      CloseHandle(hFileC)
      function = 0
      exit function
   END IF

   ' Create an unnamed file mapping object for the uncompressed file.
   hMMFU = CreateFileMapping(hFileU , null , PAGE_READWRITE , 0 , 0 , null)

   ' Map a view of the uncompressed file into our address space.
   lpMemU = MapViewOfFile(hMMFU , FILE_MAP_WRITE , 0 , 0 , 0)


   ' reserve a maximum size for the compressed file. (it can be bigger than uncompressed)
	lofC = (1.25 * lofU) + 2048
	if lofC > _MAX_BUFFER_SIZE_NTDLL  THEN lofC = _MAX_BUFFER_SIZE_NTDLL

   ' Create an unnamed file mapping object for the compressed
   hMMFC = CreateFileMapping(hFileC , null , PAGE_READWRITE , 0 , lofC , null)

   ' Map a view of the compressed file into our address space.
   lpMemC = MapViewOfFile(hMMFC , FILE_MAP_WRITE , 0 , 0 , 0)

   ' Create a compression workspace (last parameter not needed).
   RtlGetCompressionWorkSpaceSize(COMPRESSION_FORMAT_LZNT1 or engine , _
         @workSpaceSize , _
         @junk)
   lpWorkSpace = HeapAlloc(GetProcessHeap() , 0 , workSpaceSize)

   ' Compress the file.
   rVal = RtlCompressBuffer(COMPRESSION_FORMAT_LZNT1 or engine , _
         lpMemU , _
         lofU , _
         lpMemC , _
         lofC , _
         4096 , _
         @finalSize , _
         lpWorkSpace)

   ' Unmap mapped views and close the file mapping object and
   ' uncompressed file handles.
   UnmapViewOfFile(lpMemU)
   UnmapViewOfFile(lpMemC)
   CloseHandle(hMMFU)
   CloseHandle(hMMFC)
   CloseHandle(hFileU)

   ' Free the allocated compression workspace.
   HeapFree(GetProcessHeap() , 0 , lpWorkSpace)

   ' Set the final length of the compressed file and close the handle.
   SetFilePointer(hFileC , finalSize , 0 , 0)
   SetEndOfFile(hFileC)
   CloseHandle(hFileC)

   ' Return whatever RtlCompressBuffer returned.
   function = rVal
   finish = timer

   gCollect.uncomp_size = lofU
   gCollect.comp_size = finalSize
   gCollect.elapsed = int((finish - start) *1000)

end function

'*******************************************************************************************

private function NTDecompressFile(Byval compressedFileName as zstring ptr , _
			Byval uncompressedFileName as zstring ptr, _
			Byval bufferSize as Ulong = 0) _
         as ULong

   dim hFileC            as HANDLE
   dim lofC              as ULong
   dim hMMFC             as HANDLE
   dim lpMemC            as any Ptr
   dim hFileU            as HANDLE
   dim lofU              as ULong
   dim hMMFU             as HANDLE
   dim lpMemU            as any Ptr
   dim finalSize         as ULong
   dim rVal              as ULong
   dim start             as double
   dim finish            as double

	'dim ncycles           as ULong

   clean_global()
   gCollect.comp_dec = 0
   start = timer

   ' Open the compressed file.
   hFileC = CreateFile(compressedFileName , _
         GENERIC_READ or GENERIC_WRITE , _
         FILE_SHARE_READ or FILE_SHARE_WRITE , _
         null , _
         OPEN_EXISTING , _
         FILE_ATTRIBUTE_NORMAL , _
         null)

   if hFileC = INVALID_HANDLE_VALUE then
      function = _SOURCE_FILE_ERROR
      exit function
   end if

   ' Get the size of compressed file (low-order Ulong only).
   lofC = GetFileSize(hFileC , null)
   '? "size = ";lofC

   ' Open the uncompressed file.
   hFileU = CreateFile(uncompressedFileName , _
         GENERIC_READ or GENERIC_WRITE , _
         FILE_SHARE_READ or FILE_SHARE_WRITE , _
         null , _
         OPEN_ALWAYS , _
         FILE_ATTRIBUTE_NORMAL , _
         null)

   if hFileU = INVALID_HANDLE_VALUE then
      return _DESTINATION_FILE_ERROR
   end if

   if lofC = 0 THEN
      CloseHandle(hFileU)
      CloseHandle(hFileC)
      function = 0
      exit function
   END IF

   ' Create an unnamed file mapping object for the compressed file.
   hMMFC = CreateFileMapping(hFileC , null , PAGE_READWRITE , 0 , 0 , null)

   ' Map a view of the compressed file into our address space.
   lpMemC = MapViewOfFile(hMMFC , FILE_MAP_WRITE , 0 , 0 , 0)

   'test first with "minimal" buffer or bufferSize
   lofU = (lofC * 4) + 2048
	if bufferSize > _MAX_BUFFER_SIZE_NTDLL THEN
		lofU = _MAX_BUFFER_SIZE_NTDLL
	elseif bufferSize > lofU THEN
		lofU = bufferSize
	elseif lofU > _MAX_BUFFER_SIZE_NTDLL then
		lofU = _MAX_BUFFER_SIZE_NTDLL
	end if

	' to insure loop while get _BAD_COMPRESSION_BUFFER_NTDLL (in fact buffer too small)
   do

      'ncycles += 1

      ' Create an unnamed file mapping object for the uncompressed
      ' file, specifying the calculated maximum size.
      hMMFU = CreateFileMapping(hFileU , null , PAGE_READWRITE , 0 , lofU , null)

      ' Map a view of the uncompressed file into our address space.
      lpMemU = MapViewOfFile(hMMFU , FILE_MAP_WRITE , 0 , 0 , 0)

      ' Decompress the file.
      rVal = RtlDecompressBuffer(COMPRESSION_FORMAT_LZNT1 , _
            lpMemU , _
            lofU , _
            lpMemC , _
            lofC , _
            @finalSize)

      ' Unmap mapped views and close the file mapping object and
      ' compressed file handles.
      UnmapViewOfFile(lpMemU)
      CloseHandle(hMMFU)
		lofU *= 2   ' increase buffer *2 each loop
		if lofU >= _MAX_BUFFER_SIZE_NTDLL THEN exit do
   loop while rVal = _BAD_COMPRESSION_BUFFER_NTDLL
   '? "ncycles = "  ;ncycles

   UnmapViewOfFile(lpMemC)
   CloseHandle(hMMFC)
   CloseHandle(hFileC)

   ' Set the final length of the uncompressed file and close the handle.
   SetFilePointer(hFileU , finalSize , 0 , 0)
   SetEndOfFile(hFileU)
   CloseHandle(hFileU)

   ' Return whatever RtlDecompressBuffer returned.
   function = rVal
   finish = timer

   gCollect.uncomp_size = finalSize
   gCollect.comp_size = lofC
   gCollect.elapsed = int((finish - start) *1000)

end function

'*******************************************************************************************

private sub ShowStatus(ByVal rVal as ULong)
   Dim as string info = "COMPRESSION"
   if gCollect.comp_dec = 0 THEN info = "DECOMPRESSION"
   select case rVal
      case _SUCCESS_NTDLL
         print "SUCCESS_" & info & "_NTDLL"
      case _UNSUPPORTED_COMPRESSION_NTDLL
         print "UNSUPPORTED_" & info & "_NTDLL"
      case _INVALID_PARAMETER_NTDLL
         print "INVALID_PARAMETER_NTDLL"
      case _BUFFER_ALL_ZEROS_NTDLL
         print "BUFFER_ALL_ZEROS_NTDLL"
      case _NOT_SUPPORTED_NTDLL
         print "NOT_SUPPORTED_NTDLL"
      case _BUFFER_TOO_SMALL_NTDLL
         print "BUFFER_TOO_SMALL_NTDLL"
      case _BAD_COMPRESSION_BUFFER_NTDLL
         print "BAD_" & info & "_BUFFER_NTDLL"
      case _SOURCE_FILE_ERROR
         print "SOURCE_FILE_ERROR"
      case _DESTINATION_FILE_ERROR
         print "DESTINATION_FILE_ERROR"
      case else
         print "Unexpected Error " ; hex(rVal) ; "h"
   end select
   if rVal = 0 THEN
      print "Uncompressed size = " ; gCollect.uncomp_size ; " bytes"
      print "Compressed size = " ; gCollect.comp_size ; " bytes"
      if gCollect.uncomp_size THEN
			if gCollect.comp_size < gCollect.uncomp_size THEN
				 print "Deflated compressed : " ; 100 - int(gCollect.comp_size / gCollect.uncomp_size * 100) ; " %"
			else
				 print "Inflated compressed : " ; int(gCollect.comp_size / gCollect.uncomp_size * 100) ; " %"
         END IF
      else
         print "Warning : empty files"
      END IF

      print "Elapsed time = " ; gCollect.elapsed ; " ms"
   END IF
end sub


private sub MsgStatus(ByVal rVal as ULong)
   Dim as string info = "COMPRESSION"
	Dim as string collect_info

   if gCollect.comp_dec = 0 THEN info = "DECOMPRESSION"
   select case rVal
      case _SUCCESS_NTDLL
         collect_info += "SUCCESS_" & info & "_NTDLL"
      case _UNSUPPORTED_COMPRESSION_NTDLL
         collect_info +="UNSUPPORTED_" & info & "_NTDLL"
      case _INVALID_PARAMETER_NTDLL
			collect_info +="INVALID_PARAMETER_NTDLL"
      case _BUFFER_ALL_ZEROS_NTDLL
         collect_info +="BUFFER_ALL_ZEROS_NTDLL"
      case _NOT_SUPPORTED_NTDLL
         collect_info +="NOT_SUPPORTED_NTDLL"
      case _BUFFER_TOO_SMALL_NTDLL
         collect_info +="BUFFER_TOO_SMALL_NTDLL"
      case _BAD_COMPRESSION_BUFFER_NTDLL
			collect_info +="BAD_" & info & "_BUFFER_NTDLL"
      case _SOURCE_FILE_ERROR
         collect_info +="SOURCE_FILE_ERROR"
      case _DESTINATION_FILE_ERROR
         collect_info +="DESTINATION_FILE_ERROR"
      case else
        collect_info +="Unexpected Error " & hex(rVal) & "h"
   end select
   if rVal = 0 THEN
      collect_info += (chr(10,10) & "Uncompressed size = " & gCollect.uncomp_size & " bytes" & chr(10))
      collect_info += ("Compressed size = " & gCollect.comp_size & " bytes" & chr(10))
      if gCollect.uncomp_size THEN
			if gCollect.comp_size < gCollect.uncomp_size THEN
				 collect_info += ("Deflated compressed : " & str(100 - int(gCollect.comp_size / gCollect.uncomp_size * 100)) _
									& " %" & chr(10))
			else
				 collect_info += ("Inflated compressed : " & int(gCollect.comp_size / gCollect.uncomp_size * 100) _
				  & " %"	& chr(10))
         END IF
      else
        collect_info += ("Warning : empty files" & chr(10))
      END IF

      collect_info += ("Elapsed time = " & gCollect.elapsed & " ms" & chr(10))
		messagebox(0, collect_info, "Status : " & info, MB_ICONINFORMATION)
	else
		messagebox(0, collect_info, "Status : " & info, MB_ICONERROR)
   END IF

end sub

'*******************************************************************************************

dim         as ULong decompress
dim         as ULong engine
dim         as ULong rVal

  dim 			as string s_nocomp = "dlltool64_undec.exe"
dim			as string s_comp = "dlltool64.ntd"

 /' dim 			as string s_nocomp = "ntdll_compress-cop.log"
dim			as string s_comp = "ntdll_compress.ntd"
 '/

 /'   dim as string s_nocomp = "info2b.txt"
dim as string s_comp = "info2b.ntd"   '/

decompress = 1
engine = COMPRESSION_ENGINE_STANDARD
'engine = COMPRESSION_ENGINE_MAXIMUM



if decompress then

   'print chr(10) ; "Decompressing " & s_comp & " to " & s_nocomp & chr(10)
   rVal = NTDecompressFile(s_comp , s_nocomp)
   'ShowStatus rVal
	MsgStatus rVal
else
   'print chr(10) ; "Compressing " ; s_nocomp ; " to " ; s_comp ; chr(10)
   rVal = NTCompressFile(s_nocomp , s_comp , engine)
   'ShowStatus rVal
	MsgStatus rVal
end if

'print chr(10) ; "Press any key to exit..."
'sleep


