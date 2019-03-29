
/'
    test code for
    aPLib v1.1.1 - compression library    from Joergen Ibsen
                                          http://www.ibsensoftware.com

    FreeBasic source code                 from Marc Pons   29 March 2019
'/



' change under to select the file you want to compress and uncompress after to verify.
'=====================================================================================================

#define _FILE_TO_COMPRESS_      "plan.bmp"       /' change here for your own file '/

'=====================================================================================================

#ifdef __FB_WIN32__
    #Ifdef __FB_64BIT__
        #INCLIB "aplib64w"                       ' the lib name is renamed to: libaplib64w.a from aplib.lib in lib/coff/
    #Else
        #INCLIB "aplib32w"                       ' the lib name is renamed to: libaplib32w.a from aplib.lib in lib/coff64/
    #Endif
#endif

#ifdef __FB_LINUX__
    #Ifdef __FB_64BIT__
        #INCLIB "aplib64l"                       ' the lib name is renamed to: libaplib64l.a from aplib.lib in lib/elf/
    #Else
        #INCLIB "aplib32l"                       ' the lib name is renamed to: libaplib32l.a from aplib.lib in lib/elf64/
    #Endif
#endif


const APLIB_ERROR = culng(- 1)

extern "C"

    declare function get_workmem_size alias "aP_workmem_size"(byval unpacked_len as ulong) as ulong

    declare function get_packed_size alias "aP_max_packed_size"(byval unpacked_len as ulong) as ulong

    declare function get_crc32 alias "aP_crc32"(byval source as any ptr , byval length as ulong) as ulong

    declare function pack alias "aPsafe_pack"(byval source_buffer as any ptr , byval packed_buffer as any ptr , _
                                                byval source_length as ulong , byval workmem as any ptr , _
                                                byval callback as any ptr = 0 , byval cbparam as any ptr = 0) as ulong

    declare function check_packed alias "aPsafe_check"(byval packed_buffer as any ptr) as ulong

    declare function unpack alias "aPsafe_depack"(byval packed_buffer as any ptr , byval packed_len as ulong , _
                                                byval unpacked_buffer as any ptr , byval unpacked_len as ulong) as ulong

end extern




'================ helper functions ===========
type mystring                                                       ' dummy string type to mimic the normal fbstring type
        data1   as zstring ptr                                      '  and make a simple cast for it
        len1    as integer
        size1   as integer
end type

function tostring(byval ub as ubyte ptr , byval ilen as long) as string
    dim         as mystring s_my
    s_my.data1 = cast(zstring ptr , ub)
    s_my.len1 = ilen
    s_my.size1 = ilen + 1
    dim as string ptr pret = cast(string ptr , @s_my)
    return * pret
end function

private function filetostring(byref nom_file as string) as string
    dim         as uinteger filesize
    dim         as long result
    dim         as long myhandle

    myhandle = freefile()
    function = ""
    result = open(nom_file for binary access read as #myhandle)
    if result <> 0 then exit function
    filesize = lof(myhandle)
    if filesize = 0 then
        close #myhandle
        exit function
    end if
    dim as string s1 = string(filesize , 0)
    get #myhandle , 0 , s1
    close #myhandle
    function = s1
end function





/'  main test code  '/

'open file and put the content into string
dim as string sin0 = filetostring(_FILE_TO_COMPRESS_)               ' _FILE_TO_COMPRESS_ file to test compression, defined at the top

dim as ulong i0 = len(sin0)
print : print "original size  " & i0

dim as ulong crc = get_crc32(strptr(sin0) , i0)                     ' just to verify crc32 function, not needeed on the following test
print "crc32 = " ; crc

dim         as double d0                                
dim         as ulong i2
dim         as ulong i3
dim         as ulong i4
dim         as ulong iret1                                
dim         as ubyte ptr enc1
dim         as ubyte ptr dec1
dim         as ubyte ptr workmem

if i0c <> 0 then

    d0 = timer()

    'compressed size needed to allocate compressed buffer ( bigger than input buffer size)
    i2 = get_packed_size(i0)
    i3 = i2

    'extra work memory size needed to process
    i4 = get_workmem_size(i0)

    if i2 = 0 or i2 = aplib_error or i4 = 0 or i4 = aplib_error then
        print " leaving now because error on size input buffers "
    else
        enc1 = allocate(i2)                                         'malloc comprresed buffer
        workmem = allocate(i4)                                      'malloc work memory buffer
        if enc1 = 0 or workmem = 0 then
            print " Leaving now because error on allocating input compressed buffers "
            if enc1 <> 0 then deallocate enc1
            if workmem <> 0 then deallocate workmem
        else
            'packing now
            i2 = pack(cast(ubyte ptr , strptr(sin0)) , enc1 , i0 , workmem)
            if i2 <> aplib_error and i2 <> 0 then
                enc1 = reallocate(enc1 , i2)                        'realloc reduce allocated memory for the only needed size
                d0 = timer() - d0

                if workmem <> 0 then deallocate workmem             'free the work memory, no more needed

                print "compression time : " & int(d0 * 1000000) / 1000 & " ms"
                print : print "sizes :   original" , i0 , "compressed" , i2 : print
                print "         ratio  " & i2 / i0 : print
                print "buffer allocated size for compress : " & i3 : print
                print "buffer allocated size for workmem  : " & i4 : print
                print
                i3 = 0

                d0 = timer()
                'check needed size to allocate uncompressed buffer
                iret1 = check_packed(enc1)
                'iret1 = i2
                if iret1 = 0 or iret1 = aplib_error then
                    print " Leaving now because error on compressed buffer"
                    if enc1 <> 0 then deallocate enc1
                else

                    dec1 = allocate(iret1)                          'malloc uncompressed buffer
                    if dec1 = 0 then
                        print " Leaving now because error on allocating decompressed buffer "
                        if enc1 <> 0 then deallocate enc1           'free allocated compressed buffer
                    else
                        ' uncompress now
                        i3 = unpack(enc1 , i2 , dec1 , iret1)
                        d0 = timer() - d0
                        print : print " Allocated size for decompress buffer    " & iret1 : print
                        if i3 <> aplib_error and i3 <> 0 then
                            print : print "decompression time : " & int(d0 * 1000000) / 1000 & " ms"
                            print : print "sizes :   compressed " , i2 , "uncompressed" , i3
                            print : print
                            dim as string mess = tostring(dec1 , i3)
                            print
                            if mess = sin0 then
                                print "  Conclusion : decompressed = initial    it's  OK!"
                            else
                                print "  Conclusion : decompressed <> initial   it has FAILED!"
                            end if
                        else
                            print "Failed during decompression "
                        end if
                        if enc1 <> 0 then deallocate enc1           'free compressed buffer
                        if dec1 <> 0 then deallocate dec1           'free uncompressed buffer
                    end if
                end if
            else
                print "failed during compression : error code = " & i2

                if enc1 <> 0 then deallocate enc1                   'free compressed buffer
                if workmem <> 0 then deallocate workmem             'free uncompressed buffer
            end if
        end if
    end if
else
    print "Empty source file of wrong file : error "
end if

print : print : print "Press any key to  exit"
sleep












