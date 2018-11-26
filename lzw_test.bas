#Ifndef FALSE
    #Define FALSE 0
#EndIf
#Ifndef TRUE
    #Define TRUE 1
#EndIf

#Inclib "fbgfx"

' Declaration of LZW en- and decoder of the fbgfx lib
Declare Function fb_hEncode  Lib "fbgfx" Alias "fb_hEncode" ( _
    Byval lpIn As Any Ptr, _
    Byval size As integer, _
   Byval lpOut As Any Ptr, _
   Byval newsize As integer ptr _
   ) As  integer

Declare Function fb_hDecode Lib "fbgfx" Alias "fb_hDecode" ( _
    Byval lpIn As Any Ptr, _
    Byval size As long, _
    Byval lpOut As Any Ptr, _
    ByRef newsize As long _
    ) As  long

Private Function String_File( byref File as String, _
                   byref Mes as String, _
						 byval ipos as long = 0 _
                  ) as long
     Dim as long  myHandle , result

     myHandle = Freefile()
     result = Open (File For Binary Access Write as #myHandle)
     If result <> 0 Then
        Close #myHandle
        Return -1                                 ' error
     End If
     Put #myHandle, ipos , Mes
     Close #myHandle
     Return 0
End Function


Private Function ubyte_File( byref File as String, _
                    uMes() as ubyte, _
						 byval ilen as long, _
						 byval ipos as long = 0 _
                  ) as long
     Dim as long  myHandle , result

     myHandle = Freefile()
     result = Open (File For Binary Access Write as #myHandle)
     If result <> 0 Then
        Close #myHandle
        Return -1                                 ' error
     End If
     Put #myHandle, ipos , uMes(0), ilen
     Close #myHandle
     Return 0
End Function


Private Function File_uByte(Nom_File as String, byref ub as UByte ptr) as long
    Dim  as long Filesize, result
    Dim  as long myHandle

	 function = 0
    myHandle = Freefile()
    result = Open (Nom_File For Binary Access Read as #myHandle )
    If result <> 0 Then Exit Function

    Filesize = LOF(myHandle)
    If Filesize = 0 Then
       Close #myHandle
       Exit Function
    End If
    ub = Allocate(Filesize)
    Get #myHandle, 0, *ub, Filesize
    Close #myHandle
	 function = Filesize
End Function

dim ub as UByte ptr

'dim as string stin = "coucou c'est un test pour verifier comment cela va marcher ici"
'dim as string stout = space(len(stin)* 1.5)


dim as long nbin = File_uByte("dlltool64.exe", ub)
? "len before = " & nbin
? "ptr ub "  & ub
sleep

dim as long nbout

dim ubout as ubyte ptr = allocate (nbin * 1.5)
? "ptr ubout "  & ubout
   if fb_hEncode(ub, nbin, ubout, @nbout) = 0 then
		? "len before = " & nbin & "   len after = " & nbout
		deallocate(ub)
		deallocate(ubout)
	else
		? "error"

	end if

	sleep




 /' private Function LZW_Encode (SrcMem As UByte Ptr, SrcLen As UInteger, ByRef DesMem As UByte Ptr, ByRef DesLen As UInteger, ByRef DesCRC As Integer) As Integer
    Dim As UInteger ret, crc

    DesLen = SrcLen * 1.25
    DesMem = Allocate(DesLen)

    For n As Integer = 0 To SrcLen - 1
        DesCRC += DesMem[n]
    Next

    ret = fb_hEncode(SrcMem, SrcLen, DesMem, @DesLen)
    If ret Then DeAllocate(DesMem): Return FALSE

    Return TRUE
End Function

private Function LZW_Decode (SrcMem As UByte Ptr, SrcLen As UInteger, OrigCRC As UInteger, ByRef DesMem As UByte Ptr, ByRef DesLen As UInteger) As Integer
    Dim As UInteger ret, NewCRC

    For n As Integer = 0 To SrcLen - 1
        NewCRC += SrcMem[n]
    Next

    If NewCRC <> OrigCRC Then
        Return 0
    EndIf

    DesMem = Allocate(DesLen + 1)

    ret = fb_hDecode (SrcMem, SrcLen, DesMem, @DesLen)
    If ret Then DeAllocate(DesMem): Return FALSE

    Return TRUE
End Function '/ 