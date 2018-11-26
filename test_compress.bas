#include once "windows.bi"

''
'' NTDLL.BI
''
'' This file defines a *tiny* subset of the NTDLL functions
'' and related data types and constants, specifically the
'' *minimum* components required to use the native buffer
'' compression functions.
''
#inclib "ntdll"

'' These already defined in winnt.bi:
''
''#define COMPRESSION_FORMAT_LZNT1    2 ' the only format currently supported
''#define COMPRESSION_ENGINE_STANDARD 0 ' the only 2 engines currently supported
''#define COMPRESSION_ENGINE_MAXIMUM  &H100

type NTSTATUS as Ulong

#if __FB_VERSION__ < "1.02"
	#define STATUS_INVALID_PARAMETER			&hC000000D
#endif

#define STATUS_SUCCESS                  &h00000000
#define STATUS_UNSUPPORTED_COMPRESSION  &hC000025F
#define STATUS_BUFFER_ALL_ZEROS         &h00000117
#define STATUS_NOT_SUPPORTED            &hC00000BB
#define STATUS_BUFFER_TOO_SMALL         &hC0000023
#define STATUS_BAD_COMPRESSION_BUFFER   &hC0000242

declare function RtlGetCompressionWorkSpaceSize alias "RtlGetCompressionWorkSpaceSize"( _
                    byval dwCompressionFormatAndEngine as DWORD,_
                    byval lpdwCompressBufferWorkSpaceSize as DWORD ptr, _
                    byval lpdwCompressFragmentWorkSpaceSize as DWORD ptr ) _
                    as NTSTATUS

declare function RtlCompressBuffer alias "RtlCompressBuffer"( _
                    byval dwCompressionFormatAndEngine as DWORD, _
                    byval lpUnCompressedBuffer as LPCVOID, _
                    byval dwUnCompressedBufferSize As DWORD, _
                    byval lpCompressedBuffer as LPCVOID, _
                    byval dwCompressedBufferSize As DWORD, _
                    byval dwUnCompressedChunkSize As DWORD, _
                    byval lpdwFinalCompressedSize as DWORD ptr, _
                    byval lpCompressBufferWorkspace as LPCVOID ) _
                    as NTSTATUS

Declare Function RtlDecompressBuffer alias "RtlDecompressBuffer"( _
                    byval dwCompressionFormat as DWORD, _
                    byval lpUnCompressedBuffer as LPCVOID, _
                    byval dwUnCompressedBufferSize As DWORD, _
                    byval lpCompressedBuffer as LPCVOID, _
                    byval dwCompressedBufferSize As DWORD, _
                    byval lpdwFinalDecompressedSize as DWORD ptr ) _
                    as NTSTATUS




#define NL chr$(10)

'*******************************************************************************************

function NTCompressFile( uncompressedFileName as zstring ptr, _
                         compressedFileName as zstring ptr, _
                         engine as uint ) _
                         as uint

    dim hFileU as HANDLE, lofU as uint, hMMFU as HANDLE, lpMemU as PVOID
    dim hFileC as HANDLE, lofC as uint, hMMFC as HANDLE, lpMemC as PVOID
    dim workSpaceSize as uint, lpWorkSpace as PVOID
    dim finalSize as uint, rVal as uint, junk as uint
    dim start as double, finish as double

    start = timer

    '' Open the uncompressed file.
    ''
    hFileU = CreateFile( uncompressedFileName, _
                         GENERIC_READ or GENERIC_WRITE, _
                         FILE_SHARE_READ or FILE_SHARE_WRITE, _
                         null, _
                         OPEN_EXISTING, _
                         FILE_ATTRIBUTE_NORMAL, _
                         null )

    if hFileU = INVALID_HANDLE_VALUE then
        print "Error opening source file";NL
        function = -1
        exit function
    endif

    '' Create an unnamed file mapping object for the uncompressed file.
    ''
    hMMFU = CreateFileMapping( hFileU, null, PAGE_READWRITE, 0, 0, null )

    '' Map a view of the uncompressed file into our address space.
    ''
    lpMemU = MapViewOfFile( hMMFU, FILE_MAP_WRITE, 0, 0, 0 )

    '' Open the compressed file.
    ''
    hFileC = CreateFile( compressedFileName, _
                         GENERIC_READ or GENERIC_WRITE, _
                         FILE_SHARE_READ or FILE_SHARE_WRITE, _
                         null, _
                         OPEN_ALWAYS, _
                         FILE_ATTRIBUTE_NORMAL, _
                         null )

    if hFileC = INVALID_HANDLE_VALUE then
        print "Error opening destination file";NL
        return -1
    endif

    '' Get the size of uncompressed file (low-order dword only).
    ''
    lofU = GetFileSize( hFileU, null )

    '' Calc a probable maximum size for the compressed file.
    ''
    lofC = 1.13 * lofU + 4

    '' Create an unnamed file mapping object for the compressed
    '' file, specifying the calculated maximum size.
    ''
    hMMFC = CreateFileMapping( hFileC, null, PAGE_READWRITE, 0, lofC, null )

    ' Map a view of the compressed file into our address space.
    '
    lpMemC = MapViewOfFile( hMMFC, FILE_MAP_WRITE, 0, 0, 0 )

    '' Create a compression workspace (last parameter not needed).
    ''
    RtlGetCompressionWorkSpaceSize( COMPRESSION_FORMAT_LZNT1 or engine, _
                                    @workSpaceSize, _
                                    @junk )
    lpWorkSpace = HeapAlloc( GetProcessHeap( ), 0, workSpaceSize )

    '' Compress the file.
    ''
    rVal = RtlCompressBuffer( COMPRESSION_FORMAT_LZNT1 or engine, _
                              lpMemU, _
                              lofU, _
                              lpMemC, _
                              lofC, _
                              0, _
                              @finalSize, _
                              lpWorkSpace )

    '' Unmap mapped views and close the file mapping object and
    '' uncompressed file handles.
    ''
    UnmapViewOfFile( lpMemU )
    UnmapViewOfFile( lpMemC )
    CloseHandle( hMMFU )
    CloseHandle( hMMFC )
    CloseHandle( hFileU )

    '' Free the allocated compression workspace.
    ''
    HeapFree( GetProcessHeap( ), 0, lpWorkSpace )

    '' Set the final length of the compressed file and close the handle.
    ''
    SetFilePointer( hFileC, finalSize, 0, 0 )
    SetEndOfFile( hFileC )
    CloseHandle( hFileC )

    '' Return whatever RtlCompressBuffer returned.
    ''
    function = rVal

    '' Display statistics.
    ''
    finish = timer
    print "uncompressed size = ";lofU;" bytes"
    print "compressed size = ";finalSize;" bytes"
    print "ratio ="; 100 - int(finalSize / lofU * 100); "%"
    print "elapsed time =";int((finish - start) * 1000);"ms";NL

end function

'*******************************************************************************************

function NTDecompressFile( compressedFileName as zstring ptr, _
                           uncompressedFileName as zstring ptr, _
                           bufferSize as uint) _
                           as uint

    dim hFileC as HANDLE, lofC as uint, hMMFC as HANDLE, lpMemC as PVOID
    dim hFileU as HANDLE, lofU as uint, hMMFU as HANDLE, lpMemU as PVOID
    dim finalSize as uint, rVal as uint
    dim start as double, finish as double

    start = timer

    '' Open the compressed file.
    ''
    hFileC = CreateFile( compressedFileName, _
                         GENERIC_READ or GENERIC_WRITE, _
                         FILE_SHARE_READ or FILE_SHARE_WRITE, _
                         null, _
                         OPEN_EXISTING, _
                         FILE_ATTRIBUTE_NORMAL, _
                         null )

    if hFileC = INVALID_HANDLE_VALUE then
        print "Error opening source file";NL
        function = -1
        exit function
    endif

    '' Create an unnamed file mapping object for the compressed file.
    ''
    hMMFC = CreateFileMapping( hFileC, null, PAGE_READWRITE, 0, 0, null )

    '' Map a view of the compressed file into our address space.
    ''
    lpMemC = MapViewOfFile( hMMFC, FILE_MAP_WRITE, 0, 0, 0 )

    '' Open the uncompressed file.
    ''
    hFileU = CreateFile( uncompressedFileName, _
                         GENERIC_READ or GENERIC_WRITE, _
                         FILE_SHARE_READ or FILE_SHARE_WRITE, _
                         null, _
                         OPEN_ALWAYS, _
                         FILE_ATTRIBUTE_NORMAL, _
                         null )

    if hFileU = INVALID_HANDLE_VALUE then
        print "Error opening destination file";NL
        return -1
    endif

    '' Get the size of compressed file (low-order dword only).
    ''
    lofC = GetFileSize( hFileC, null )


    '' Unless overridden by a non-zero bufferSize parameter, calculate
    '' the maximum size of the uncompressed file based on a 95% ratio.
    ''
    if bufferSize then
      lofU = bufferSize
    else
      lofU = lofC * 20
    endif

    '' Create an unnamed file mapping object for the uncompressed
    '' file, specifying the calculated maximum size.
    ''
    hMMFU = CreateFileMapping( hFileU, null, PAGE_READWRITE, 0, lofU, null )

    '' Map a view of the uncompressed file into our address space.
    ''
    lpMemU = MapViewOfFile( hMMFU, FILE_MAP_WRITE, 0, 0, 0 )

    '' Decompress the file.
    ''
    rVal = RtlDecompressBuffer( COMPRESSION_FORMAT_LZNT1, _
                                lpMemU, _
                                lofU, _
                                lpMemC, _
                                lofC, _
                                @finalSize )

    '' Unmap mapped views and close the file mapping object and
    '' compressed file handles.
    ''
    UnmapViewOfFile( lpMemC )
    UnmapViewOfFile( lpMemU )
    CloseHandle( hMMFC )
    CloseHandle( hMMFU )
    CloseHandle( hFileC )

    '' Set the final length of the uncompressed file and close the handle.
    ''
    SetFilePointer( hFileU, finalSize, 0, 0 )
    SetEndOfFile( hFileU )
    CloseHandle( hFileU )

    '' Return whatever RtlDecompressBuffer returned.
    ''
    function = rVal

    '' Display statistics.
    ''
    finish = timer
    print "compressed size = ";lofC;" bytes"
    print "uncompressed size = ";finalSize;" bytes"
    print "elapsed time =";int((finish - start) * 1000);"ms";NL

end function

'*******************************************************************************************

sub ShowStatus( rVal as uint )
    select case rVal
        case STATUS_SUCCESS
            print "STATUS_SUCCESS"
        case STATUS_UNSUPPORTED_COMPRESSION
            print "STATUS_UNSUPPORTED_COMPRESSION"
        case STATUS_INVALID_PARAMETER
            print "STATUS_INVALID_PARAMETER"
        case STATUS_BUFFER_ALL_ZEROS
            print "STATUS_BUFFER_ALL_ZEROS"
        case STATUS_NOT_SUPPORTED
            print "STATUS_NOT_SUPPORTED"
        case STATUS_BUFFER_TOO_SMALL
            print "STATUS_BUFFER_TOO_SMALL"
        case STATUS_BAD_COMPRESSION_BUFFER
            print "STATUS_BAD_COMPRESSION_BUFFER"
        case else
            print "Unexpected Error ";hex$(rVal);"h"
    end select
end sub

'*******************************************************************************************

dim as uint i,decompress,bufferSize,engine,rVal

i = 1
engine = COMPRESSION_ENGINE_STANDARD

if ucase(left(command$(i),2)) = "/D" then
    decompress = true
    if instr(command$(i),":") then
      bufferSize = val( mid( command$(i), instr(command$(i),":") + 1 ) )
    endif
    i += 1
endif

if ucase(command$(i)) = "/M" then
    engine = COMPRESSION_ENGINE_MAXIMUM
    i += 1
endif

if len(command$(i)) = 0 or len(command$(i+1)) = 0 then
    print NL;"NTCOMP [/D[:buffersize]] [/M] source destination";NL
    print "  /D             Perform a decompress, instead of the default compress."
    print "  [:buffersize]  Specifies a decompress buffer size that overrides the default."
    print "  /M             Use the maximum compression engine.";NL
    print "  The default decompress buffer is sized for a 95% compression ratio.";NL
    print "Press any key to exit..."
    sleep
    end
endif

if decompress then
  print "bufferSize:";bufferSize
  print NL;"Decompressing ";command$(i); " to "; command$(i+1);NL
  rVal = NTDecompressFile( command$(i), command$(i+1), bufferSize )
  ShowStatus rVal
else
  print NL;"Compressing ";command$(i); " to "; command$(i+1);NL
  rVal = NTCompressFile( command$(i), command$(i+1), engine )
  ShowStatus rVal
endif

print NL;"Press any key to exit..."
sleep						  