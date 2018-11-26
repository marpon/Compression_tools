/'		simple_lzfx_v3
* version 0.1 translated to freebasic by marpon   november 2018
 * Copyright (c) 2009 Andrew Collette <andrew.collette at gmail.com>
 * http://lzfx.googlecode.com
 *
 * Implements an LZF-compatible compressor/decompressor based on the liblzf
 * codebase written by Marc Lehmann.  This code is released under the BSD license.
 *
'/


#ifndef LZFX_H
    #define LZFX_H

     #ifndef NULL
        # define NULL 						0
    #endif

	/' Hashtable size (2**LZFX_HLOG entries) '/
    #ifndef LZFX_HLOG
        # define LZFX_HLOG 			16
    #endif

	/' Predefined errors. '/
	#define LZFX_ESIZE     	 			-1      /' Output buffer too small '/
	#define LZFX_ECORRUPT   		-2      /' Invalid data for decompression '/
	#define LZFX_EARGS      			-3      /' Arguments invalid (NULL) '/


    #define LZFX_HSIZE 					(1 shl (LZFX_HLOG))

	/' Define the hash function '/
    #define LZFX_FRST(p) 				(((p[0]) shl 8) Or p[1])
    #define LZFX_NEXT(v,p) 			(((v) shl 8) Or p[2])
    #define LZFX_IDX(h) 					((( h shr (3*8 - LZFX_HLOG)) - h ) And (LZFX_HSIZE - 1))

	/' These cannot be changed, as they are related to the compressed format. '/
    #define LZFX_MAX_LIT 				(1 shl 5)
    #define LZFX_MAX_OFF 			(1 shl 13)
    #define LZFX_MAX_REF 			((1 shl 8) + (1 shl 3))

	/' This macro to reproduce   !a    in c'/
	#define MY_NOT(value ) 			IIf ( value = 0, 1, 0 )

    Declare function lzfx_getsize(byVal ibuf as ubyte ptr , byval ilen as ulong , byref olen as ulong  , byval optional as long = 0  ) as long



	/'  Buffer-to buffer compression.
		Supply pre-allocated input and output buffers via ibuf and obuf, and
		their size in bytes via ilen and olen.  Buffers may not overlap.

		On success, the function returns a non-negative value and the argument
		olen contains the compressed size in bytes.  On failure, a negative
		value is returned and olen is not modified.
	'/
    Declare function lzfx_compress(byVal ibuf as ubyte ptr , byval ilen as ulong , byVal obuf as ubyte ptr , byref olen as ulong) as long



	/'  Buffer-to-buffer decompression.
		Supply pre-allocated input and output buffers via ibuf and obuf, and
		their size in bytes via ilen and olen.  Buffers may not overlap.

		On success, the function returns a non-negative value and the argument
		olen contains the uncompressed size in bytes.  On failure, a negative
		value is returned.

		If the failure code is LZFX_ESIZE, olen contains the minimum buffer size
		required to hold the decompressed data.  Otherwise, olen is not modified.

		Supplying a zero olen is a valid and supported strategy to determine the
		required buffer size.  This does not require decompression of the entire
		stream and is consequently very fast.  Argument obuf may be NULL in
		this case only.
	'/
    Declare function lzfx_decompress(byVal ibuf as ubyte ptr , byval ilen as ulong , byVal obuf as ubyte ptr , byref olen as ulong) as long


	/' Guess len. No parameters may be NULL this is not checked. '/
    private function lzfx_getsize(byVal ibuf as ubyte ptr , byval ilen as ulong , byref olen as ulong, byval optional as long  ) as long
        dim as ubyte ptr ip = ibuf
        dim as ubyte ptr in_end = ip + ilen
        dim as ulong tot_len = 0

        while(ip < in_end)
            dim as ulong ctrl = *ip
            ip += 1
            if (ctrl < (1 shl 5)) then
                ctrl += 1
                if (ip + ctrl > in_end) then
					if optional = 0 THEN
						olen =  ilen * 1.1  + 80 / ilen    /'  conservative buffer len '/
						return 0
                    end if
					return LZFX_ECORRUPT
				end if
                tot_len += ctrl
                ip += ctrl
            else
                dim as ulong len1 = (ctrl shr 5)
				if(len1=7) then    /' i.e. format #2 '/
					len1 += *ip
					ip += 1
				end if
				len1 += 2    /' len is now #octets '/
				if (ip >= in_end) then
					if optional = 0 THEN
						olen =  ilen * 1.1  + 80 / ilen    /'  conservative buffer len '/
						return 0
                    end if
					return LZFX_ECORRUPT
				end if
				ip+=1 /' skip the ref byte '/
				tot_len += len1
			end if
		wend
		olen = tot_len
		return 0
	end function



	/' Compressed format

		There are two kinds of structures in LZF/LZFX: literal runs and back
		references. The length of a literal run is encoded as L - 1, as it must
		contain at least one byte.  Literals are encoded as follows:

		000LLLLL <L+1 bytes>

		Back references are encoded as follows.  The smallest possible encoded
		length value is 1, as otherwise the control byte would be recognized as
		a literal run.  Since at least three bytes must match for a back reference
		to be inserted, the length is encoded as L - 2 instead of L - 1.  The
		off1set (distance to the desired data in the output buffer) is encoded as
		o - 1, as all off1sets are at least 1.  The binary format is:

		LLLooooo oooooooo           for backrefs of real length < 9   (1 <= L < 7)
		111ooooo LLLLLLLL oooooooo  for backrefs of real length >= 9  (L > 7)
	'/
	Private function lzfx_compress(byVal ibuf as ubyte ptr , byval ilen as ulong , byVal obuf as ubyte ptr , byref olen as ulong) as long
		/' Hash table an array of ubyte*'s which point
		   to various locations in the input buffer '/
		dim as ubyte ptr htab_arr(LZFX_HSIZE)
		dim as ubyte ptr ptr htab = @htab_arr(lbound(htab_arr))
		dim as ubyte ptr ptr hslot      /' Pointer to entry in hash table '/
		dim as ulong hval      /' Hash value generated by macros above '/
		dim as ubyte ptr ref          /' Pointer to candidate match location in input '/
		dim as ubyte ptr ip = ibuf
		dim as ubyte ptr in_end = ip + ilen
		dim as ubyte ptr op = obuf
		dim as ubyte ptr out_end = op + olen         '(olen = NULL ? NULL : op + *olen)
		dim as long  lit    /' # of bytes in current literal run '/
		dim as ulongint off1

		if (olen = 0) then return LZFX_EARGS
		if (ibuf = NULL) then
			if (ilen <> 0) then return LZFX_EARGS
			olen = 0
			return 0
		end if
		if (obuf = NULL) then
			if (olen <> 0) then return LZFX_EARGS
			return lzfx_getsize(ibuf , ilen , olen)
		end if
		/'  Start a literal run.  Whenever we do this the output pointer is
			advanced because the current byte will hold the encoded length. '/
		lit = 0 : op += 1
		hval = LZFX_FRST(ip)
		while(ip + 2 < in_end)   /' The NEXT macro reads 2 bytes ahead '/
			hval = LZFX_NEXT(hval , ip)
			hslot = htab + LZFX_IDX(hval)
			ref = *hslot : *hslot = ip
			if ref < ip then off1 = ip - ref - 1
			if ((ref < ip)  AndAlso (off1 < LZFX_MAX_OFF) AndAlso ( ip + 4 < in_end)  _
					AndAlso (ref > ibuf ) AndAlso (ref[0] = ip[0]) AndAlso (ref[1] = ip[1])  _
					AndAlso (ref[2] = ip[2])) then
				dim as ulong len1 = 3   /' We already know 3 bytes match '/
				dim as ulong maxlen = LZFX_MAX_REF
				if in_end - ip - 2 < LZFX_MAX_REF then maxlen = in_end - ip - 2
						/' lit = 0:  op + 3 must be < out_end (because we undo the run)
						   lit <> 0:  op + 3 + 1 must be < out_end '/
				if op - MY_NOT(lit) + 4  >=  out_end then return LZFX_ESIZE
				op [- lit - 1] = lit - 1 /' Terminate literal run '/
				op -= MY_NOT(lit)              /' Undo run if length is zero '/
						/'  Start checking at the fourth byte '/
				while((len1 < maxlen) and (ref[len1] = ip[len1]))
					len1 += 1
				wend
				len1 -= 2  /' We encode the length as #octets - 2 '/
						/' Format 1: [LLLooooo oooooooo] '/
				if (len1 < 7) then
					*op = (off1 shr 8) + (len1 shl 5)
					op += 1
					*op = off1
					op += 1
						/' Format 2: [111ooooo LLLLLLLL oooooooo] '/
				else
					*op = (off1 shr 8) + (7 shl 5)
					op += 1
					*op = len1 - 7
					op += 1
					*op = off1
					op += 1
				end if
				lit = 0 : op += 1
				ip += (len1 + 1)  /' ip = initial ip + #octets -1 '/
				if (ip + 3 >= in_end) then
					ip+=1   /' Code following expects exit at bottom of loop '/
					exit while                           'break
				end if
				hval = LZFX_FRST(ip)
				hval = LZFX_NEXT(hval , ip)
				htab[LZFX_IDX(hval)] = ip
				ip+=1   /' ip = initial ip + #octets '/
			else
					  /' Keep copying literal bytes '/
				if (op >= out_end) then return LZFX_ESIZE
				lit += 1 : *op = *ip : op += 1 : ip += 1
				if (lit = LZFX_MAX_LIT) then
					op [- lit - 1] = lit - 1 /' stop run '/
					lit = 0 : op+=1 /' start run '/
				end if
			end if /' if() found match in htab '/
		wend  /' while(ip < ilen -2) '/
		/'  At most 3 bytes remain in input.  We therefore need 4 bytes available
			in the output buffer to store them (3 data + ctrl byte).'/
        if (op + 3 > out_end) then return LZFX_ESIZE
        while(ip < in_end)
            lit += 1 : * op = *ip : op += 1 : ip += 1
            if (lit = LZFX_MAX_LIT) then
                op[- lit - 1] = lit - 1
                lit = 0 : op += 1
            end if
        wend
        op[- lit - 1] = lit - 1
        op -= MY_NOT( lit)
        olen = op - obuf
        return 0
    End function

	#macro my_guess()   'used by lzfx_decompress (better than gosub)
		rc = lzfx_getsize(ip, ilen - (ip-ibuf), remain_len, -1)
		if rc>=0 then olen = remain_len + (op - obuf)
		return rc
	#endmacro

	/' Decompressor '/
	Private function lzfx_decompress(byVal ibuf as ubyte ptr , byval ilen as ulong , byVal obuf as ubyte ptr , byref olen as ulong) as long
		dim as ubyte ptr ip = ibuf
		dim as ubyte ptr in_end = ip + ilen
		dim as ubyte ptr op = obuf
		dim as ubyte ptr out_end = op + olen
		dim as ulong remain_len = 0
		dim as long rc

		if(olen = 0) then return LZFX_EARGS
		if(ibuf = NULL) then
			if(ilen <> 0) then return LZFX_EARGS
			olen = 0
			return 0
		end if
		if(obuf = NULL)then
			if(olen <> 0) then return LZFX_EARGS
			return lzfx_getsize(ibuf, ilen, olen)
		end if
		do
			dim as ulong ctrl = *ip
			ip+=1
			/' Format 000LLLLL: a literal byte string follows, of length L+1 '/
			if(ctrl < (1 shl 5)) then
				ctrl+=1
				if(op + ctrl > out_end) then
					ip -=1      /' Rewind to control byte '/
					my_guess()
				end if
				if(ip + ctrl > in_end) then return LZFX_ECORRUPT
				do
					*op= *ip : op+=1 : ip+=1
					ctrl -= 1
				loop while(ctrl <> 0)
				/'  Format #1 [LLLooooo oooooooo]: backref of length L+1+2
								  ^^^^^ ^^^^^^^^
									A      B
						   #2 [111ooooo LLLLLLLL oooooooo] backref of length L+7+2
								  ^^^^^          ^^^^^^^^
									A               B
					In both cases the location of the backref is computed from the
					remaining part of the data as follows:
						location = op - A*256 - B - 1
				'/
			else
				dim as ulong len1 = (ctrl shr 5)
				dim as ubyte ptr ref = op - ((ctrl And &h1f) shl 8) -1
				if(len1=7) then
					len1 += *ip
					ip+=1    /' i.e. format #2 '/
				end if
				len1 += 2    /' len is now #octets '/
				if(op + len1 > out_end)then
					ip -= iif(len1 >= 9, 2 , 1)   /' Rewind to control byte '/
					my_guess()
				end if
				if(ip >= in_end) then return LZFX_ECORRUPT
				ref -=  *ip  : ip += 1
				if(ref < obuf) then return LZFX_ECORRUPT
				do
					*op = *ref : op+= 1 : ref+=1
					len1 -=1
				loop while (len1 <> 0 )
			end if
		loop while (ip < in_end)
		olen = op - obuf
		return 0
		'my_guess()
	end function

#endif


 /'  test code  '/

dim as string sin0 = "Good morning Dr. Chandra. This is Hal. I am ready for my first lesson." & _
        "Good morning Dr. Chandra1. This is Hal. I am ready for my first lesson1." & _
        "Good morning Dr. Chandra2. This is Hal. I am ready for my first lesson2." & _
        "Good morning Dr. Chandra3. This is Hal. I am ready for my first lesson3." & _
        "Good morning Dr. Chandra4. This is Hal. I am ready for my first lesson4." & _
        "Good morning Dr. Chandra5. This is Hal. I am ready for my first lesson5." & _
        "Good morning Dr. Chandra6. This is Hal. I am ready for my first lesson6."

sin0=sin0 &"end"
dim as long i0 = len(sin0)
print : print "original size  " & i0
dim as long i2

'estimates compressed size needed   to allocate  buffer ( conservative value)
lzfx_getsize(strptr(sin0) , i0 , i2)
print : print "lzfx_getsize  : estimated for compress buffer  " & i2

 i2 =  i0*2
dim as ubyte ptr enc1 = allocate(i2)
lzfx_compress(cast(ubyte ptr , strptr(sin0)) , i0 , enc1 , i2)
print : print  "sizes :   original",  i0 , "compressed", i2

enc1[i2] = 0

print :print : print : print : print  *cast(zstring ptr,enc1)
print :print : print : print : print
dim as long i3 =0

'gives right size  uncompressed needed   to allocate  buffer
lzfx_getsize(enc1 , i2 , i3)
print : print :print "lzfx_getsize  : needed for decompress buffer    " & i3


dim as ubyte ptr dec1 = allocate((i3 +1) )    ' add 1 for be able to put 0  at the end
lzfx_decompress( enc1  , i2,dec1, i3)
print : print  "sizes :   compressed ",  i2 , "uncompressed", i3
dec1[i3] = 0

print :print : print : print : print  *cast(zstring ptr,dec1)
deallocate enc1
deallocate dec1
print : print : print  "Press any key to  exit"
sleep

