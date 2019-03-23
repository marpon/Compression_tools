'zip_test1.bas to test zip static libs


#Ifdef __FB_64BIT__
    #INCLIB "zip_64"                             ' the lib name is : libzip_64.a
#Else
    #INCLIB "zip_32"                             ' the lib name is : libzip_32.a
#Endif



Extern "C"
    Declare Function compressBound(Byval uLen As Ulong) As Ulong
    Declare Function uncompress(Byval dest As Ubyte Ptr , Byval destLen As Ulong Ptr , Byval src As Const Ubyte Ptr , Byval uLen As Ulong) As Long
    Declare Function compress(Byval dest As Ubyte Ptr , Byval destLen As Ulong Ptr , Byval src As Const Ubyte Ptr , Byval ulen As Ulong) As Long
End Extern

namespace packer
    Dim shared As String g_string
    Dim Shared As Ulong g_len
    dim shared as ubyte ptr g_psrc , g_pdest

    Function getpassedinfo(byref text As String) As String
        Dim As String var1 , var2
        Dim As long pst

        pst = Instr(text , "|")
        var1 = "" : var2 = ""
        If pst <> 0 Then
            var1 = Mid(text , 1 , pst - 1)
            var2 = Mid(text , pst + 1)
        Else
            var1 = text
        End If

        text = var2
        g_len = ValUInt(var1)
        Return text
    End Function


    '================= UNPACK ===============
    function unpack() as string
        Var text = getpassedinfo(g_string)


        Dim As ulong stringlength , destinationlength
        stringlength = Len(text)
        destinationlength = g_len

        g_psrc = Allocate(stringlength)
        g_pdest = Allocate(destinationlength)

        g_psrc = @text[0]

        Var mistake = uncompress(g_pdest , @destinationlength , g_psrc , stringlength)
        If mistake <> 0 Then Print "There was an error" : return ""
        Dim As String uncompressed

        'Build the uncompressed string
        uncompressed = String(destinationlength , 0)
        For i As long = 0 To destinationlength - 1
            uncompressed[i] = (g_pdest[i])
        Next
        return uncompressed
    end function

    '=================== PACK ============

    function pack() as string
        Var text = g_string
        Dim As ulong stringlength , destinationlength
        stringlength = Len(text)
        destinationlength = compressBound(stringlength)
		Print "estimated compression buffer size = ",destinationlength
        g_psrc = Allocate(stringlength)
        g_pdest = Allocate(destinationlength)
        g_psrc = @text[0]

        Var mistake = compress(g_pdest , @destinationlength , g_psrc , stringlength)

        If mistake <> 0 Then Print "There was an error"

        Dim As String compressed = String(destinationlength , 0)

        For n As long = 0 To destinationlength - 1
            compressed[n] = g_pdest[n]
        Next n
        compressed = stringlength & "|" + compressed
        return compressed
    end function

    sub finish destructor
        print "ending"
        deallocate g_psrc
        deallocate g_pdest
        g_psrc = 0
        g_pdest = 0
        'end
    end sub
end namespace


Private Function FileToString(byref Nom_File as String ) as String
	Dim as uinteger Filesize
	Dim as long result
	Dim as long myHandle

	myHandle = Freefile()
	Function = ""
	result = Open(Nom_File For Binary Access Read as #myHandle)
	If result <> 0 Then Exit Function
	Filesize = LOF(myHandle)
	If Filesize = 0 Then
		Close #myHandle
		Exit Function
	End If
	dim as string s1 = string(Filesize,0)
	Get #myHandle , 0 , s1
	Close #myHandle
	function = s1
End Function


'============== use =========


 /'  test code  '/

dim as string sin0 = FileToString("blabla.txt")

dim as long i0 = len(sin0)
print : print "original size  " & i0



dim as string s = sin0
'print "original string"
'print s
print

packer.g_string = s
dim as string compressed = packer.pack

print "packed string "
'print compressed
print
print


packer.g_string = compressed
dim as string uncompressed = packer.unpack

print "Retrieve"
'print uncompressed
print
print "compression ratio  " ; len(compressed) / len(uncompressed)
print : print "original size  " & i0, "compressed  " ; len(compressed), "uncompressed  " ; len(uncompressed)
print
if uncompressed = s THEN
	print "  conclusion : decompressed = initial    ok!"
else
	print "  conclusion : decompressed <> initial    failed!"
END IF

print : print : print  "Press any key to  exit"
sleep






