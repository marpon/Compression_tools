'minz_test0.bas to test minz static libs

#Ifdef __FB_64BIT__
    #INCLIB "minz_64"                             ' the lib name is : libminz_64.a
#Else
    #INCLIB "minz_32"                             ' the lib name is : libminz_32.a
#Endif



extern "C"
declare Function z_uncompress alias "z_uncompress"(byref pDest as ubyte ptr, byval pComp as ubyte ptr , byval comp_len as ulong) as long
declare Function z_compress alias "z_compress"(byref pDest as ubyte ptr, byval pInitial as ubyte ptr , byval Initial_len as ulong, byval level as ulong)as long
declare Function z_compressBound alias "z_compressBound"(byval Initial_len as ulong)as ulong
declare Function z_UncompLen alias "z_UncompLen"( byval pComp as ubyte ptr)as ulong
end extern





type mystring
	data1 as zstring ptr
	len1 as integer
	size1 as integer
END TYPE

function tostring(byval ub as ubyte ptr, byval ilen as long)as string
	dim as mystring s_my
	s_my.data1 = cast(zstring ptr, ub)
	s_my.len1  = ilen
	s_my.size1  = ilen + 1
	dim as string ptr pret = cast(string ptr, @s_my)
	return *pret
END FUNCTION


'============== use =========

print sizeof(string) , sizeof(mystring)

 /'  test code  '/

dim as string sin0 = !"Good morning Dr. Chandra. This is Hal. I am ready for my first lesson." & chr(0) & _
        "Good morning Dr. Chandra1. This is Hal. I am ready for my first lesson1." & _
        "Good morning Dr. Chandra2. This is Hal. I am ready for my first lesson2." & _
        "Good morning Dr. Chandra3. This is Hal. I am ready for my first lesson3." & _
        "Good morning Dr. Chandra4. This is Hal. I am ready for my first lesson4." & _
        "Good morning Dr. Chandra5. This is Hal. I am ready for my first lesson5." & _
        "Good morning Dr. Chandra6. This is Hal. I am ready for my first lesson6."

'sin0=sin0 &"end"
dim as long i0 = len(sin0)
print : print "original size  " & i0



dim as string s = sin0
print "original string"
print s
print


dim as long i2,i3
dim as ubyte ptr enc1,dec1
'estimates compressed size needed   to allocate  buffer ( conservative value)
i2 = z_compressBound(len(sin0))
print : print "z_compressBound  : estimated size for compress buffer  " & i2

i2 = z_compress(enc1, cast(ubyte ptr , strptr(sin0)) , i0 , 10 )
print : print  "sizes :   original",  i0 , "compressed", i2
print "ratio  " & i2/i0 : print
print tostring(enc1, i2)
print
i3 =0

'gives right size  uncompressed needed   to allocate  buffer
i3 = z_UncompLen( enc1)
print : print :print "z_UncompLen  : needed for decompress buffer    " & i3



i3 = z_uncompress( dec1, enc1  , i2)
print : print  "sizes :   compressed ",  i2 , "uncompressed", i3
dec1[i3] = 0

print :print
dim as string mess =  tostring(dec1, i3)
print mess
print


if mess = sin0 THEN
	print "  conclusion : decompressed = initial    ok!"
else
	print "  conclusion : decompressed <> initial    failed!"
END IF

deallocate enc1
deallocate dec1


print : print : print  "Press any key to  exit"
sleep





