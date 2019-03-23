'ntdl_test0.bas to test Rtl ntdll code

#Include "file.bi"
#include "string.bi"

'change here for tests
'==================================================
#Define _MY__FORMAT_ 		COMPRESSION_FORMAT_LZNT1  ' 2 ; 3 or 4
#Define _MY__MAX_			1								' 1 or 0


'==================================================



#Ifndef COMPRESSION_ENGINE_STANDARD
    #Define COMPRESSION_ENGINE_STANDARD &h0000
#endif
#Ifndef COMPRESSION_FORMAT_DEFAULT
    #Define COMPRESSION_FORMAT_DEFAULT &h0001
#Endif
#Ifndef COMPRESSION_FORMAT_LZNT1
    #Define COMPRESSION_FORMAT_LZNT1 &h0002
#Endif
#Ifndef COMPRESSION_FORMAT_XPRESS
    #Define COMPRESSION_FORMAT_XPRESS &h0003
#Endif
#Ifndef COMPRESSION_FORMAT_XPRESS_HUFF
    #Define COMPRESSION_FORMAT_XPRESS_HUFF &h0004
#Endif
#Ifndef COMPRESSION_ENGINE_MAXIMUM
    #Define COMPRESSION_ENGINE_MAXIMUM &h0100
#Endif




'static declare at compile time
Extern "Windows" Lib "ntdll"
    Declare Function RtlGetCompressionWorkSpaceSize Alias "RtlGetCompressionWorkSpaceSize"( _
            Byval CompressionFormatAndEngine As Ushort , _
            Byval CompressBufferWorkSpaceSize As Ulong Ptr , _
            Byval CompressFragmentWorkSpaceSize As Ulong Ptr) As Ulong

    Declare Function RtlCompressBuffer Alias "RtlCompressBuffer"( _
            Byval CompressionFormatAndEngine As Ushort , _
            Byval UncompressedBuffer As Ubyte Ptr , _
            Byval UncompressedBufferSize As Ulong , _
            Byval CompressedBuffer As Ubyte Ptr , _
            Byval CompressedBufferSize As Ulong , _
            Byval chunksize As Ulong , _
            Byval FinalCompressedSize As Ulong Ptr , _
            Byval WorkSpace As Ubyte Ptr) As Ulong

	' not possible with  "RtlDecompressBufferEx" why?

End Extern



'https://docs.microsoft.com/en-us/windows-hardware/drivers/ddi/content/ntifs/nf-ntifs-rtlcompressbuffer
Function _WinAPI_LZNTCompress(byval pBinary As Ubyte Ptr , byval iBinarySize As Ulong , Byref iCompressedSize As Ulong , _
            byval bMaxCompression As Ubyte = 1 , byval iCompressionEngine As Ushort = COMPRESSION_FORMAT_LZNT1) _
            As Ubyte Ptr

    Dim  As Ulong iReturn
    Dim  As Ulong iCompressBufferWorkSpaceSize
    Dim  As Ulong iCompressFragmentWorkSpaceSize
    Dim  As Ulong iFinalCompressedSize

    Dim As Ushort iCompressionFormatAndEngine = Iif(bMaxCompression , iCompressionEngine Or COMPRESSION_ENGINE_MAXIMUM , iCompressionEngine)
    iReturn = RtlGetCompressionWorkSpaceSize(iCompressionFormatAndEngine , @iCompressBufferWorkSpaceSize , @iCompressFragmentWorkSpaceSize)
    Dim As Ubyte Ptr pWorkSpace = Allocate(iCompressBufferWorkSpaceSize)
    dim as Ubyte Ptr pBuffer = Allocate(2 * iBinarySize)
    ? "compress WorkSpace size = " & iCompressBufferWorkSpaceSize

    iReturn = RtlCompressBuffer(iCompressionFormatAndEngine , _
            pBinary , _
            iBinarySize , _
            pBuffer , _
            2 * iBinarySize , _
            4096 , _
            @iFinalCompressedSize , _
            pWorkSpace)

    iCompressedSize = iFinalCompressedSize

    pBuffer = reallocate(pBuffer , iFinalCompressedSize)
    Deallocate(pWorkSpace)
    Return pBuffer
End Function


'https://docs.microsoft.com/en-us/windows-hardware/drivers/ddi/content/ntifs/nf-ntifs-rtldecompressbuffer
Function _WinAPI_LZNTDecompress(byval aBinary As Ubyte Ptr , byval iFileSize As Ulong , byval iCompressedSize As Ulong , _
            byval bMaxCompression As Ubyte = 1 , byval iDecompressionEngine As Ushort = COMPRESSION_FORMAT_LZNT1) _
            As Ubyte Ptr

    Dim As Any Ptr hLib = Dylibload( "Ntdll.dll") 'needed dynamic load at run time for RtlDecompressBufferEx why?

    Dim As Ushort iFormatAndEngine = Iif(bMaxCompression , iDecompressionEngine Or COMPRESSION_ENGINE_MAXIMUM , iDecompressionEngine)
    dim As Ubyte Ptr pbuffer = allocate(iFileSize)
    Dim As Ulong iUSize
    Dim As Ulong iBufferWorkSpaceSize
    Dim As Ulong iFragmentWorkSpaceSize
    Dim As Ulong iReturn0 = RtlGetCompressionWorkSpaceSize(iFormatAndEngine , @iBufferWorkSpaceSize , @iFragmentWorkSpaceSize)
    Dim As Ubyte Ptr pWorkSpace = Allocate(iBufferWorkSpaceSize)
    ? "Decompress WorkSpace size = " & iBufferWorkSpaceSize

    dim fRtlDecompressBufferEx as Function( _
            Byval CompressionFormat As Ushort , _
            Byval UncompressedBuffer As Ubyte Ptr , _
            Byval UncompressedBufferSize As Ulong , _
            Byval CompressedBuffer As Ubyte Ptr , _
            Byval CompressedBufferSize As Ulong , _
            Byval FinalUncompressedSize As Ulong Ptr , _
            Byval WorkSpace As Ubyte Ptr) As Ulong

    fRtlDecompressBufferEx = Dylibsymbol(hLib , "RtlDecompressBufferEx")

    Dim As Ulong iReturn = fRtlDecompressBufferEx(iDecompressionEngine , _
            pbuffer , _
            iFileSize , _
            aBinary , _
            iCompressedSize , _
            @iUSize , _
            pWorkSpace)

    ? "decompress size :" & iUSize
    If iReturn Then
        ? "An Error has occured:"
        Select Case iReturn
            Case &hC0000242
                ? Hex(iReturn) & ": STATUS_BAD_COMPRESSION_BUFFER"
            Case &hC00000E8
                ? Hex(iReturn) & ": STATUS_INVALID_USER_BUFFER"
            Case &hC000025F
                ? Hex(iReturn) & ": STATUS_UNSUPPORTED_COMPRESSION"
            Case &hC000000D
                ? Hex(iReturn) & ": STATUS_INVALID_PARAMETER"
            Case &h00000117
                ? Hex(iReturn) & ": STATUS_BUFFER_ALL_ZEROS"
            Case &hC00000BB
                ? Hex(iReturn) & ": STATUS_NOT_SUPPORTED"
            Case &hC0000023
                ? Hex(iReturn) & ": STATUS_BUFFER_TOO_SMALL"
        End Select
    End If

    pbuffer = reallocate(pbuffer , iUSize)

    Dylibfree(hlib)
    Deallocate(pWorkSpace)
    Return pbuffer
End Function

'compress test file
Dim         As Integer iFile
Dim         As Ulong iFileLenSource
Dim         As Ulong iFileLen
Dim         As Ulong iCompressedSize


Dim As Ushort iCompressor = _MY__FORMAT_

dim as string sin0 = !"Good morning Dr. Chandra. This is Hal. I am ready for my first lesson." & chr(0)& _
        "Good morning Dr. Chandra1. This is Hal. I am ready for my first lesson1." & _
        "Good morning Dr. Chandra2. This is Hal. I am ready for my first lesson2." & _
        "Good morning Dr. Chandra3. This is Hal. I am ready for my first lesson3." & _
        "Good morning Dr. Chandra4. This is Hal. I am ready for my first lesson4." & _
        "Good morning Dr. Chandra5. This is Hal. I am ready for my first lesson5." & _
        "Good morning Dr. Chandra6. This is Hal. I am ready for my first lesson6."
iFileLenSource = len(sin0)

Dim As Ubyte Ptr pMem = strptr(sin0) , pMemCompressed


pMemCompressed = _WinAPI_LZNTCompress(pMem , iFileLenSource , iCompressedSize , _MY__MAX_ , iCompressor)


? "File size: " & iFileLenSource & " bytes      compressed size: " & iCompressedSize & " bytes       ratio: " & Format(iCompressedSize / iFileLenSource , "#.##%")

'decompress test file from memory
Dim as Ubyte Ptr pMemDecompressed = _WinAPI_LZNTDecompress(pMemCompressed , iFileLenSource , iCompressedSize , _MY__MAX_, iCompressor)
if pMemDecompressed THEN
    For i As Ulong = 0 To iFileLenSource - 1
        If pMemDecompressed[i] <> pMem[i] Then
            ? "Compare has failed!"
            Exit For
        End If
    Next
else
    ? "Decompressed  has failed!"
END IF


Deallocate(pMem)
Deallocate(pMemCompressed)
Deallocate(pMemDecompressed)

? "Done"
Sleep



