#include once "windows.bi"
#include once "crt/string.bi"                    ' needed for strspn, strcspn or strtok


#define _DEBUG_FB                                'show debug info if uncommented else nothing shown

#ifndef _DEBUG_FB_
   #define _DEBUG_FB_
   #MACRO DEBUG_FB(st)
      #ifdef _DEBUG_FB
         print(st)
      #ENDIF
   #ENDMACRO
#ENDIF

' These are already defined in winnt.bi:
'
'#define COMPRESSION_FORMAT_LZNT1 2 ' the only format currently supported
'#define COMPRESSION_ENGINE_STANDARD 0 ' the first of 2 engines currently supported
'#define COMPRESSION_ENGINE_MAXIMUM &H100 ' and the second supported


' These for ntdll compression usage
#define _SUCCESS_NTDLL &h00000000
#define _INVALID_PARAMETER_NTDLL &hC000000D
#define _NOT_SUPPORTED_NTDLL &hC00000BB
#define _UNSUPPORTED_COMPRESSION_NTDLL &hC000025F
#define _BUFFER_ALL_ZEROS_NTDLL &h00000117
#define _NOT_SUPPORTED_NTDLL &hC00000BB
#define _BUFFER_TOO_SMALL_NTDLL &hC0000023
#define _BAD_COMPRESSION_BUFFER_NTDLL &hC0000242

' These for more error info
#define _SOURCE_FILE_ERROR &h00000001
#define _DESTINATION_FILE_ERROR &h00000002

#define _MAX_BUFFER_SIZE_NTDLL &h0FFFFFFF        ' can be modified up to ULONG Max

#define _FILE_SEPARATOR_ "|"


type my_info_ntdll                               ' gCollect
   as long uncomp_size
   as long comp_size
   as long elapsed
   as long comp_dec
   as long retval
END TYPE

dim shared as my_info_ntdll gCollect             ' to collect the different info


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
   gCollect.retval = 0
end sub

private function NTCompressFile(Byval uncompressedFileName as zstring ptr , _
         Byval compressedFileName as zstring ptr , _
         Byval engine as ULong = COMPRESSION_ENGINE_STANDARD) _
         as ULong

   dim hFileU as HANDLE
   dim lofU as ULong
   dim hMMFU as HANDLE
   dim lpMemU as any Ptr
   dim hFileC as HANDLE
   dim lofC as ULong
   dim hMMFC as HANDLE
   dim lpMemC as any Ptr
   dim workSpaceSize as ULong
   dim lpWorkSpace as any Ptr
   dim finalSize as ULong
   dim rVal as ULong
   dim junk as ULong
   dim start as double
   dim finish as double

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
   if lofC > _MAX_BUFFER_SIZE_NTDLL THEN lofC = _MAX_BUFFER_SIZE_NTDLL

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
   gCollect.retval = rVal
end function

'*******************************************************************************************

private function NTDecompressFile(Byval compressedFileName as zstring ptr , _
         Byval uncompressedFileName as zstring ptr , _
         Byval bufferSize as Ulong = 0) _
         as ULong

   dim hFileC as HANDLE
   dim lofC as ULong
   dim hMMFC as HANDLE
   dim lpMemC as any Ptr
   dim hFileU as HANDLE
   dim lofU as ULong
   dim hMMFU as HANDLE
   dim lpMemU as any Ptr
   dim finalSize as ULong
   dim rVal as ULong
   dim start as double
   dim finish as double

   'dim ncycles as ULong

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
      lofU *= 2                                  ' increase buffer *2 each loop
      if lofU >= _MAX_BUFFER_SIZE_NTDLL THEN exit do
   loop while rVal = _BAD_COMPRESSION_BUFFER_NTDLL
   '? "ncycles = " ;ncycles

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
   gCollect.retval = rVal
end function

'*******************************************************************************************

private sub ShowStatus()
   dim rVal as ULong = gCollect.retval
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


private sub MsgStatus()
   dim rVal as ULong = gCollect.retval
   Dim as string info = "COMPRESSION"
   Dim as string collect_info

   if gCollect.comp_dec = 0 THEN info = "DECOMPRESSION"
   select case rVal
      case _SUCCESS_NTDLL
         collect_info += "SUCCESS_" & info & "_NTDLL"
      case _UNSUPPORTED_COMPRESSION_NTDLL
         collect_info += "UNSUPPORTED_" & info & "_NTDLL"
      case _INVALID_PARAMETER_NTDLL
         collect_info += "INVALID_PARAMETER_NTDLL"
      case _BUFFER_ALL_ZEROS_NTDLL
         collect_info += "BUFFER_ALL_ZEROS_NTDLL"
      case _NOT_SUPPORTED_NTDLL
         collect_info += "NOT_SUPPORTED_NTDLL"
      case _BUFFER_TOO_SMALL_NTDLL
         collect_info += "BUFFER_TOO_SMALL_NTDLL"
      case _BAD_COMPRESSION_BUFFER_NTDLL
         collect_info += "BAD_" & info & "_BUFFER_NTDLL"
      case _SOURCE_FILE_ERROR
         collect_info += "SOURCE_FILE_ERROR"
      case _DESTINATION_FILE_ERROR
         collect_info += "DESTINATION_FILE_ERROR"
      case else
         collect_info += "Unexpected Error " & hex(rVal) & "h"
   end select
   if rVal = 0 THEN
      collect_info += (chr(10 , 10) & "Uncompressed size = " & gCollect.uncomp_size & " bytes" & chr(10))
      collect_info += ( "Compressed size = " & gCollect.comp_size & " bytes" & chr(10))
      if gCollect.uncomp_size THEN
         if gCollect.comp_size < gCollect.uncomp_size THEN
            collect_info += ( "Deflated compressed : " & str(100 - int(gCollect.comp_size / gCollect.uncomp_size * 100)) _
                  & " %" & chr(10))
         else
            collect_info += ( "Inflated compressed : " & int(gCollect.comp_size / gCollect.uncomp_size * 100) _
                  & " %" & chr(10))
         END IF
      else
         collect_info += ( "Warning : empty files" & chr(10))
      END IF

      collect_info += ( "Elapsed time = " & gCollect.elapsed & " ms" & chr(10))
      messagebox(0 , collect_info , "Status : " & info , MB_ICONINFORMATION)
   else
      messagebox(0 , collect_info , "Status : " & info , MB_ICONERROR)
   END IF

end sub

'*******************************************************************************************
private Function strtok_r(byval s as zstring ptr , byval delim as zstring ptr , _
         byval save_ptr as zstring ptr ptr) as zstring ptr
   dim as zstring ptr pend

   if s = NULL then s = *save_ptr
   if *s = 0 then
      *save_ptr = s
      return NULL
   end if
   /' Scan leading delimiters.  '/
   s += strspn(s, delim)
   if *s = 0 then
      *save_ptr = s
      return NULL
   end if
  /' Find the end of the token.  '/
	pend = s + strcspn(s, delim)
   if *pend = 0 then
      *save_ptr = pend
      return s
   end if
  /' Terminate the token and make *SAVE_PTR point past it.  '/
   *pend = 0
   *save_ptr = pend + 1
   return s
end function

private Function Parse_names(ByRef sText As String , _
         ByRef sDelimiter As String , _
         ByVal nPosition As Integer _
         ) As String
   Dim sTemp As String = sText 'needed a copy because strtok_r will modify the content putting 0 for each substring
   Dim pch As ZString Ptr
   Dim i As Integer
   Dim saveptr As ZString Ptr

   If Len(sText) = 0 Then return ""
   pch = strtok_r(sTemp, sDelimiter, @saveptr)
   For i = 1 To nPosition
      If (pch <> NULL) And (i = nPosition) Then Return *pch
      pch = strtok_r(NULL, sDelimiter, @saveptr)
   Next
   return ""
End Function



 /' private Function Parse_names(ByRef sText As String , _
         ByRef sDelimiter As String , _
         ByVal nPosition As Integer _
         ) As String
   Dim sTemp As String = sText
   Dim pch As ZString Ptr
   Dim i As Integer
   If Len(sText) = 0 Then return ""
   pch = strtok(StrPtr(sTemp) , sDelimiter)
   For i = 1 To nPosition
      If (pch <> 0) And (i = nPosition) Then Return * pch
      pch = strtok(0 , sDelimiter)
   Next
   return ""
End Function
 '/


private Sub Kill_files(ByRef filespec As String , Byref path as string)
   Dim As String filename = Dir(path & filespec) ' Start a file search with the specified filespec/attrib *AND* get the first filename.
   Do While Len(filename) > 0                    ' If len(filename) is 0, exit the loop: no more filenames are left to be read.
      DEBUG_FB( "killing ... " & path & filename)
      kill path & filename
      filename = Dir()
   Loop
End Sub

Private Function ZgetTempPath() as String
   Dim as Long bufflen
   Dim as Long res
   Dim as String tpath
   bufflen = GetTempPath(0 , StrPtr(tpath))
   tpath = Space(bufflen)
   res = GetTempPath(bufflen , StrPtr(tpath))
   tpath = RTrim(tpath)
   Return tpath
End Function

private function temp_dir_manage(byval flag as long = 0) as string
   Dim as String tpath = ZgetTempPath() & "temp_ntdll"
   DEBUG_FB(tpath)
   if flag = 0 THEN
      if Dir(tpath , &h10) <> "" THEN
         kill_files( "*.*" , tpath & "\")
      else
         mkdir tpath
      END IF
   elseif flag = 1 THEN
      if Dir(tpath , &h10) <> "" THEN
         kill_files( "*.*" , tpath & "\")
         chdir exepath()
         rmdir tpath & "\"
      END IF
      tpath = ""
   END IF
   return tpath
end function


Private Function get_FileName(ByRef Src As String) As String
   Dim x As Long
   x = InStrrev(Src , Any ":/\")
   If x Then
      Function = Mid(Src , x + 1)
   Else
      Function = Src
   End If
End Function


Private Function File_Ubyte(byref Nom_File as String , Byref fileData as UByte Ptr) as long
   Dim as Long Filesize
   Dim as Long result
   Dim as Long myHandle
   Dim as String cont

   myHandle = Freefile()
   Function = 0
   result = Open(Nom_File For Binary as #myHandle)
   If result <> 0 Then Exit Function
   Filesize = LOF(myHandle)
   If Filesize = 0 Then
      Close #myHandle
      Exit Function
   End If
   fileData = Allocate(Filesize)
   Get #myHandle , 0 , *fileData , Filesize
   Close #myHandle
   function = Filesize
End Function


Private sub Ubyte_File(Byref fileData as UByte Ptr , Byval Filesize as Long , byref Nom_File as String)
   Dim as Long result
   Dim as Integer myHandle
   Dim as String cont

   myHandle = Freefile()
   result = Open(Nom_File For Binary Access write as #myHandle)
   If result <> 0 Then Exit sub
   If Filesize = 0 Then
      Close #myHandle
      exit sub
   end if
   put #myHandle , , *fileData , Filesize
   Close #myHandle
End sub

private function getStrings(ub() as ubyte , byval nb as long) as string
   dim as string st1
   dim x as long

   for x = 0 to nb
      if ub(x) = 0 THEN exit for
      st1 &= chr(ub(x))
   NEXT
   return st1
END FUNCTION

private function my_UnZip(byref source as string , byref dest as string) as long
   dim as ULong rVal
   dim as Ulong index
   dim as ushort ipos
   dim as ULong icount
   dim as long ff1
   dim fileData as UByte Ptr
   dim as ubyte ilen
   dim as string stemp
   dim as ubyte ub2()

   dim as string st1 = temp_dir_manage(0) & "\"
   function = - 1
   if source = "" THEN exit function
   if dest <> "" and right(dest , 1) <> "\" and right(dest , 1) <> "/" THEN dest &= "\"
   ff1 = freefile
   If Open(source For Binary Access read As #ff1) = 0 Then
      DEBUG_FB( "Successfully opened file to decompress")
      redim ub2(6)
      icount = 1
      get #ff1 , icount , ub2()
      stemp = getStrings(ub2() , 6)
      if stemp <> "MYPAR" THEN
         Close #ff1
         st1 = temp_dir_manage(1)
         exit function
      END IF
      DEBUG_FB(stemp)
      icount += 6
      get #ff1 , icount , ipos
      DEBUG_FB(ipos)
      dim as ulong comp_size(ipos)
      dim as ulong uncomp_size(ipos)
      dim as string files(ipos)
      icount += 2
      for index = 0 to ipos - 1
         get #ff1 , icount , comp_size(index)
         DEBUG_FB(comp_size(index))
         icount += 4
         get #ff1 , icount , uncomp_size(index)
         DEBUG_FB(uncomp_size(index))
         icount += 4
         get #ff1 , icount , ilen
         DEBUG_FB(ilen)
         icount += 1
         redim ub2(ilen)
         get #ff1 , icount , ub2()
         DEBUG_FB( "icount = " & icount)
         files(index) = getStrings(ub2() , ilen)
         DEBUG_FB( "<" & files(index) & ">")
         icount += ilen
      NEXT
      for index = 0 to ipos - 1
         if comp_size(index) THEN
            filedata = allocate(comp_size(index))
            get #ff1 , icount , *filedata , comp_size(index)
            icount += comp_size(index)
            Ubyte_File(fileData , comp_size(index) , st1 & files(index) & ".LZN")
            deallocate(filedata)
            filedata = 0
            rVal = NTDecompressFile(st1 & files(index) & ".LZN" , dest & files(index) , uncomp_size(index) + 20)
            if rVal THEN
               ipos = 0
               exit for
            END IF
         else
            Ubyte_File(0 , 0 , dest & files(index))
         END IF
      next
      Close #ff1
   Else
      ipos = 0
   End If

   DEBUG_FB( "nb files uncompressed = " & ipos)
   st1 = temp_dir_manage(1)
   return ipos
end function

private function my_Zip(byref liste as string , byref dest as string , _
         Byval engine as ULong = COMPRESSION_ENGINE_STANDARD) as long
   if liste = "" or dest = "" THEN return 0
   kill dest
   dim as string fname
   dim as string file
   dim as long ipos
   dim as string st1
   dim as ULong rVal
   redim as ulong comp_size(8)
   redim as ulong uncomp_size(8)
   redim as string files(8)
   dim as long index = 8
   dim as long ff1
   dim fileData as UByte Ptr
   dim as long ilen
   dim as ubyte ub1 = 0
   do
      fname = Parse_names(liste , _FILE_SEPARATOR_ , ipos + 1)
      if ipos = 0 and fname <> "" THEN st1 = temp_dir_manage() & "\"
      if fname <> "" THEN
         DEBUG_FB( "fname = " & fname)
         file = get_FileName(fname)
         DEBUG_FB(st1 & file & ".LZN")
         rVal = NTCompressFile(fname , st1 & file & ".LZN" , engine)
         'MsgStatus()
         if rVal THEN
            st1 = temp_dir_manage(1)
            return 0
         END IF
         if ipos = index THEN
            index *= 2
            redim preserve comp_size(index)
            redim preserve uncomp_size(index)
            redim preserve files(index)
         END IF
         comp_size(ipos) = gCollect.comp_size
         uncomp_size(ipos) = gCollect.uncomp_size
         files(ipos) = file
         ipos += 1
      end if
   loop while fname <> ""
   ff1 = freefile
   If Open(dest For Binary Access Write As #ff1) = 0 Then
      DEBUG_FB( "Successfully opened file")
      put #ff1 , , "MYPAR"
      put #ff1 , , ub1
      put #ff1 , , cast(ushort , ipos)

      for index = 0 to ipos - 1
         put #ff1 , , comp_size(index)
         put #ff1 , , uncomp_size(index)
         ilen = len(files(index)) + 1
         put #ff1 , , cast(ubyte , ilen)
         put #ff1 , , files(index)
         put #ff1 , , ub1
      NEXT
      for index = 0 to ipos - 1
         ilen = File_Ubyte(st1 & files(index) & ".LZN" , fileData)
         if fileData THEN
            put #ff1 , , *fileData , ilen
            deallocate(filedata)
            filedata = 0
         END IF
      next
      Close #ff1
   Else
      ipos = 0
   End If
   st1 = temp_dir_manage(1)
   DEBUG_FB( "nb files compressed = " & ipos)
   return ipos
end function



'*******************************************************************************************



 /' my_Zip("info3.txt@dlltool64.exe", "dlltool64.par", "@")
'messagebox 0,"attente" , "wait" , 0
my_UnZip("dlltool64.par", exepath() & "\get_info") '/

my_Zip( "info.txt" , "my_temp_zip.par")


'my_Zip( "dlltool64.exe|dlltool32.exe|def-editor.exe|gendef.exe|as32.exe|as64.exe" , "my_full_zip.par")
'messagebox 0,"attente" , "wait" , 0
'my_UnZip( "my_full_zip.par" , exepath() & "\get_info\")


'dim as ULong decompress
'dim as ULong engine
'dim as ULong rVal

'dim as string s_nocomp = "dlltool64_undec.exe"
'dim as string s_comp = "dlltool64.ntd"




/' dim 			as string s_nocomp = "ntdll_compress-cop.log"
dim			as string s_comp = "ntdll_compress.ntd"
'/

/'   dim as string s_nocomp = "info2b.txt"
dim as string s_comp = "info2b.ntd"   '/

 /' decompress = 1
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
end if '/

print chr(10) ; "Press any key to exit..."


sleep






