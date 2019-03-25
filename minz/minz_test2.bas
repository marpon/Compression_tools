'minz_test2.bas to test minz static libs, equivalent compression/time as zlib

/' tested under win32 win64 and linux64 (mint) '/

'  change parameters under
'=====================================================================================================
#define _FILE_TO_COMPRESS_		"plan.bmp"	     /' change here for your own files '/

'#define _FILE_TO_COMPRESS_     "blabla.txt"     /' interesting random txt file with very few repetitive words '/


#define _COMPRESSION_LEVEL_		6				 /' from 1 to 10,  1 low compression, but fast   ...   10 high compression, but very slow  '/
'=====================================================================================================



#ifdef __FB_WIN32__
    #Ifdef __FB_64BIT__
        #INCLIB "minz_64"                        ' the lib name is : libminz_64.a
    #Else
        #INCLIB "minz_32"                        ' the lib name is : libminz_32.a
    #Endif
#endif

#ifdef __FB_LINUX__
    #Ifdef __FB_64BIT__
        #INCLIB "lminz_64"                       ' the lib name is : liblminz_64.a
    #Else
        #INCLIB "lminz_32"                       ' the lib name is : liblminz_32.a,  not tested : c compiled on mint64 / gcc64 with -m32
    #Endif
#endif



extern "C"
    declare Function z_compress alias "z_compress"(byref pDest as ubyte ptr , byval pInitial as ubyte ptr , byval Initial_len as ulong , byval level as ulong) as long
    declare Function z_uncompress alias "z_uncompress"(byref pDest as ubyte ptr , byval pComp as ubyte ptr , byval comp_len as ulong) as long

    declare Function z_compressBound alias "z_compressBound"(byval Initial_len as ulong) as ulong ' conservative information
    declare Function z_UncompLen alias "z_UncompLen"(byval pComp as ubyte ptr) as ulong ' precise information, gets from compressed header file
end extern



'================ helper ===========
type mystring
        data1                      as zstring ptr
        len1                       as integer
        size1                      as integer
END TYPE

function tostring(byval ub as ubyte ptr , byval ilen as long) as string
    dim         as mystring s_my
    s_my.data1 = cast(zstring ptr , ub)
    s_my.len1 = ilen
    s_my.size1 = ilen + 1
    dim as string ptr pret = cast(string ptr , @s_my)
    return * pret
END FUNCTION

Private Function FileToString(byref Nom_File as String) as String
    Dim         as uinteger Filesize
    Dim         as long result
    Dim         as long myHandle

    myHandle = Freefile()
    Function = ""
    result = Open(Nom_File For Binary Access Read as #myHandle)
    If result <> 0 Then Exit Function
    Filesize = LOF(myHandle)
    If Filesize = 0 Then
        Close #myHandle
        Exit Function
    End If
    dim as string s1 = string(Filesize , 0)
    Get #myHandle , 0 , s1
    Close #myHandle
    function = s1
End Function


'============== end helper =========



 /'  test code  '/

dim as string sin0 = FileToString(_FILE_TO_COMPRESS_) ' _FILE_TO_COMPRESS_  file to test compression, defined at the top

'sin0=sin0 &"end"
dim as long i0 = len(sin0)
print : print "original size  " & i0



dim as string s = sin0
'print "original string"
'print s
print

dim         as double d0
dim         as long i2
dim         as long i3
dim         as ubyte ptr enc1
dim         as ubyte ptr dec1

d0 = timer()
'estimates compressed size needed to allocate buffer ( conservative value)
i2 = z_compressBound(len(sin0))
i3 = i2
i2 = z_compress(enc1 , cast(ubyte ptr , strptr(sin0)) , i0 , _COMPRESSION_LEVEL_) ' _COMPRESSION_LEVEL_ from 1 to 10, defined at the top

d0 = timer() - d0
print "Compression time : " & int(d0 * 1000000) / 1000 & " ms"
print : print "sizes :   original" , i0 , "compressed" , i2 : print
print "         ratio  " & i2 / i0 : print
print "Buffer allocated size for compress : " & i3 : print
'print tostring(enc1, i2)
print
i3 = 0

'gives right size uncompressed needed to allocate buffer
i3 = z_UncompLen(enc1)
print : print : print "z_UncompLen  : needed for decompress buffer    " & i3


d0 = timer()
i3 = z_uncompress(dec1 , enc1 , i2)
d0 = timer() - d0
print : print "Decompression time : " & int(d0 * 1000000) / 1000 & " ms"

print : print "sizes :   compressed " , i2 , "uncompressed" , i3
dec1[i3] = 0

print : print
dim as string mess = tostring(dec1 , i3)
'print mess
print


if mess = sin0 THEN
    print "  conclusion : decompressed = initial    it's  OK!"
else
    print "  conclusion : decompressed <> initial   it has FAILED!"
END IF

deallocate enc1
deallocate dec1


print : print : print "Press any key to  exit"
sleep






