'' original code crush.c    Pure C version by powturbo
' adaptation of the code written and placed in the public domain by Ilya Muravyov
''
''
'' converted and adaped for freebasic by Marpon   04/04/2019
''           the compress buffer incorporates the uncompressed size at first 4 bytes
''			 (very little header but helpful)



' change parameters under to use the file you want
'=====================================================================================================
'#define _FILE_TO_COMPRESS_		"plan.bmp"	     		/' change here for your own files '/
#define _FILE_TO_COMPRESS_		"blabla.txt"
'#define _FILE_TO_COMPRESS_ 	"libtest.a" 			/' interesting random txt file with very few repetitive words '/
'#define _FILE_TO_COMPRESS_ 	"WinFBE_Suite.rar"		/' this one will increase when compressed '/

#define _COMPRESSION_LEVEL_		2				 		/' from 0 to 2,  0 low compression, but fast ... 2 better compression, but slower  '/
'=====================================================================================================


'the 'bi part' 4 functions to use :
'  lzmat_compress
'  lzmat_uncompress
'  lzmat_compressBound    to estimate the needed buffer size very conservative size (always bigger than the source size)
'  lzmat_check            to get the exact uncompressed size from compressed buffer , use this one  for uncompress function

#ifndef _LZMAT_COMPRESS_BI_
	#define _LZMAT_COMPRESS_BI_

	'#include once "crt/stdio.bi"
	#include once "crt/string.bi"


	' for tweaking the performances
	#define W_BITS_DEF 			19												'' Window size (17..23)  	21
	#define W_SIZE_DEF 			(1 shl W_BITS_DEF)
	#define W_MASK_DEF 			(W_SIZE_DEF - 1)
	#define SLOT_BITS_DEF 		4												''							4
	#define NUM_SLOTS_DEF 		(1 shl SLOT_BITS_DEF)

	#define A_BITS_DEF 			1 												'' 1 xx						2
	#define B_BITS_DEF 			2 												'' 01 xx					2
	#define C_BITS_DEF 			3												'' 001 xx					2
	#define D_BITS_DEF 			5 												'' 0001 xxx					3
	#define E_BITS_DEF 			7 												'' 00001 xxxxx				5
	#define F_BITS_DEF 			9												'' 00000 xxxxxxxxx			9
	#define A_DEF 				(1 shl A_BITS_DEF)
	#define B_DEF 				((1 shl B_BITS_DEF) + A_DEF)
	#define C_DEF 				((1 shl C_BITS_DEF) + B_DEF)
	#define D_DEF 				((1 shl D_BITS_DEF) + C_DEF)
	#define E_DEF 				((1 shl E_BITS_DEF) + D_DEF)
	#define F_DEF 				((1 shl F_BITS_DEF) + E_DEF)
	#define MIN_MATCH_DEF 		3												''							3
	#define MAX_MATCH_DEF 		((F_DEF - 1) + MIN_MATCH_DEF)

	#define TOO_FAR_DEF 		(1 shl 16)

	#define HASH1_LEN_DEF 		MIN_MATCH_DEF
	#define HASH2_LEN_DEF 		(MIN_MATCH_DEF + 1)
	#define HASH1_BITS_DEF 		19												''							21
	#define HASH2_BITS_DEF 		22												''							24
	#define HASH1_SIZE_DEF 		(1 shl HASH1_BITS_DEF)
	#define HASH2_SIZE_DEF 		(1 shl HASH2_BITS_DEF)
	#define HASH1_MASK_DEF 		(HASH1_SIZE_DEF - 1)
	#define HASH2_MASK_DEF 		(HASH2_SIZE_DEF - 1)
	#define HASH1_SHIFT_DEF 	((HASH1_BITS_DEF + (HASH1_LEN_DEF - 1)) / HASH1_LEN_DEF)
	#define HASH2_SHIFT_DEF 	((HASH2_BITS_DEF + (HASH2_LEN_DEF - 1)) / HASH2_LEN_DEF)

	#macro FILL_OUT_MACRO(xbuf , d1 , d2)
		xbuf[d1] = xbuf[d2]
		d1 += 1
		d2 += 1
	#endmacro

	type info_t
		g_inbuf as ubyte ptr
		g_outbuf as ubyte ptr
		g_inbuf_pos as long
		g_outbuf_pos as long
		g_bit_buf as long
		g_bit_count as long
	end type



	private sub init_bits(byval inbuf as ubyte ptr, byval outbuf as ubyte ptr, byval tinfo as info_t ptr)
		tinfo->g_bit_count = 0
		tinfo->g_bit_buf = 0
		tinfo->g_inbuf_pos = 0
		tinfo->g_outbuf_pos = 0
		tinfo->g_inbuf = inbuf
		tinfo->g_outbuf = outbuf
	end sub

	private sub put_bits(byval n as long, byval x as long, byval tinfo as info_t ptr)
		tinfo->g_bit_buf or= x shl tinfo->g_bit_count
		tinfo->g_bit_count += n
		while tinfo->g_bit_count >= 8
			tinfo->g_outbuf[tinfo->g_outbuf_pos] = tinfo->g_bit_buf
			tinfo->g_outbuf_pos += 1
			tinfo->g_bit_buf shr= 8
			tinfo->g_bit_count -= 8
		wend
	end sub

	private sub flush_bits(byval tinfo as info_t ptr)
		put_bits(7, 0, tinfo)
		tinfo->g_bit_count = 0
		tinfo->g_bit_buf = 0
	end sub

	private function get_bits(byval n as long, byval tinfo as info_t ptr) as long
		while tinfo->g_bit_count < n
			tinfo->g_bit_buf or= tinfo->g_inbuf[tinfo->g_inbuf_pos] shl tinfo->g_bit_count
			tinfo->g_inbuf_pos += 1
			tinfo->g_bit_count += 8
		wend
		dim x as long = tinfo->g_bit_buf and ((1 shl n) - 1)
		tinfo->g_bit_buf shr= n
		tinfo->g_bit_count -= n
		return x
	end function

	#define update_hash1(h, c) clng((((h) shl HASH1_SHIFT_DEF) + (c)) and HASH1_MASK_DEF)
	#define update_hash2(h, c) clng((((h) shl HASH2_SHIFT_DEF) + (c)) and HASH2_MASK_DEF)

	private function get_min(byval a as long, byval b as long) as long
		return iif(a < b, a, b)
	end function

	private function get_max(byval a as long, byval b as long) as long
		return iif(a > b, a, b)
	end function

	private function get_penalty(byval a as long, byval b as long) as long
		dim p as long = 0
		while a > b
			a shr= 3
			p += 1
		wend
		return p
	end function

	private function lzmat_compress(byval outbuf0 as ubyte ptr, byval out_size as long , _
									byval buf as ubyte ptr, byval buf_size as long, byval level as long) as long
		if buf = NULL or buf_size < 1 then
			'fprintf(stderr, !"Source buffer corrupted: size=%d\n", buf_size)
			return -1
		end if
		if outbuf0 = NULL or out_size < 5 then
			'fprintf(stderr, !"Compression buffer corrupted: size=%d\n", out_size)
			return -2
		end if
		dim outbuf as ubyte ptr = outbuf0 + 4
		dim tinfo as info_t
		init_bits(NULL, outbuf, @tinfo)

		if level < 0 or level > 2 THEN level = 0
		redim head(0 to (HASH1_SIZE_DEF + HASH2_SIZE_DEF ) - 1) as long
		redim prev(0 to W_SIZE_DEF - 1) as long
		dim max_chain(0 to ...) as  long = {4, 256, 1 shl 12}
		dim h1 as long = 0
		dim h2 as long = 0
		dim i as long = 0
		while i < (HASH1_SIZE_DEF + HASH2_SIZE_DEF)
			head(i) = -1
			i += 1
		wend
		i = 0
		while i < HASH1_LEN_DEF
			h1 = update_hash1(h1, buf[i])
			i += 1
		wend
		i = 0
		while i < HASH2_LEN_DEF
			h2 = update_hash2(h2, buf[i])
			i += 1
		wend

		dim p as long = 0
		while p < buf_size
			dim len0 as long = MIN_MATCH_DEF - 1
			dim offset0 as long = W_SIZE_DEF
			dim max_match as  long = get_min(MAX_MATCH_DEF, buf_size - p)
			dim limit as  long = get_max(p - W_SIZE_DEF, 0)
			if head(h1) >= limit then
				dim s as long = head(h1)
				if buf[s] = buf[p] then
					dim l as long = 1
					while l < max_match
						if buf[(s + l)] <> buf[(p + l)] then
							exit while
						end if
						l += 1
					wend
					if l > len0 then
						len0 = l
						offset0 = p - s
					end if
				end if
			end if
			if len0 < MAX_MATCH_DEF then
				dim chain_len as long = max_chain(level)
				dim s as long = head((h2 + HASH1_SIZE_DEF))
				while (chain_len <> 0) andalso (s >= limit)
					if (buf[(s + len0)] = buf[(p + len0)]) andalso (buf[s] = buf[p]) then
						dim l as long = 1
						while l < max_match
							if buf[(s + l)] <> buf[(p + l)] then
								exit while
							end if
							l += 1
						wend
						dim as long itemp0 = get_penalty((p - s) shr 4, offset0)
						if l > (len0 + itemp0) then
							len0 = l
							offset0 = p - s
						end if
						if l = max_match then
							exit while
						end if
					end if
					s = prev((s and W_MASK_DEF))
					chain_len -= 1
				wend
			end if
			if (len0 = MIN_MATCH_DEF) andalso (offset0 > TOO_FAR_DEF) then
				len0 = 0
			end if
			if ((level >= 2) andalso (len0 >= MIN_MATCH_DEF)) andalso (len0 < max_match) then
				dim next_p as  long = p + 1
				dim max_lazy as  long = get_min(len0 + 4, max_match)
				dim chain_len as long = max_chain(level)
				dim s as long = head((update_hash2(h2, buf[(next_p + (HASH2_LEN_DEF - 1))]) + HASH1_SIZE_DEF))
				while (chain_len <> 0) andalso (s >= limit)
					if (buf[(s + len0)] = buf[(next_p + len0)]) andalso (buf[s] = buf[next_p]) then
						dim l as long = 1
						while l < max_lazy
							if buf[(s + l)] <> buf[(next_p + l)] then
								exit while
							end if
							l += 1
						wend
						if l > (len0 + get_penalty(next_p - s, offset0)) then
							len0 = 0
							exit while
						end if
						if l = max_lazy then
							exit while
						end if
					end if
					s = prev((s and W_MASK_DEF))
					chain_len -= 1
				wend
			end if
			if len0 >= MIN_MATCH_DEF then
				put_bits(1, 1, @tinfo)
				dim l as  long = len0 - MIN_MATCH_DEF
				if l < A_DEF then
					put_bits(1, 1, @tinfo)
					put_bits(A_BITS_DEF, l, @tinfo)
				elseif l < B_DEF then
					put_bits(2, 1 shl 1, @tinfo)
					put_bits(B_BITS_DEF, l - A_DEF, @tinfo)
				elseif l < C_DEF then
					put_bits(3, 1 shl 2, @tinfo)
					put_bits(C_BITS_DEF, l - B_DEF, @tinfo)
				elseif l < D_DEF then
					put_bits(4, 1 shl 3, @tinfo)
					put_bits(D_BITS_DEF, l - C_DEF, @tinfo)
				elseif l < E_DEF then
					put_bits(5, 1 shl 4, @tinfo)
					put_bits(E_BITS_DEF, l - D_DEF, @tinfo)
				else
					put_bits(5, 0, @tinfo)
					put_bits(F_BITS_DEF, l - E_DEF, @tinfo)
				end if
				offset0 -= 1
				dim log0 as long = W_BITS_DEF - NUM_SLOTS_DEF
				while offset0 >= (2 shl log0)
					log0 += 1
				wend
				put_bits(SLOT_BITS_DEF, log0 - (W_BITS_DEF - NUM_SLOTS_DEF), @tinfo)
				if log0 > (W_BITS_DEF - NUM_SLOTS_DEF) then
					put_bits(log0, offset0 - (1 shl log0), @tinfo)
				else
					put_bits(W_BITS_DEF - (NUM_SLOTS_DEF - 1), offset0, @tinfo)
				end if
			else
				len0 = 1
				put_bits(9, buf[p] shl 1, @tinfo)
			end if
			while len0 <> 0
				len0 -= 1
				head(h1) = p
				prev((p and W_MASK_DEF)) = head((h2 + HASH1_SIZE_DEF))
				head((h2 + HASH1_SIZE_DEF)) = p
				p += 1
				h1 = update_hash1(h1, buf[(p + (HASH1_LEN_DEF - 1))])
				h2 = update_hash2(h2, buf[(p + (HASH2_LEN_DEF - 1))])
			wend
		wend
		flush_bits(@tinfo)
		memcpy(outbuf0, @buf_size, 4)
		return tinfo.g_outbuf_pos + 4
	end function

	private function lzmat_check(byval inbuf as ubyte ptr)as long
		if inbuf = NULL then
			'fprintf(stderr, !"Compressed buffer corrupted!\n")
			return -1
		end if
		return *cptr(ulong ptr, inbuf)
	end function


	private function lzmat_uncompress(byval outbuf as ubyte ptr, byval outsize as long, byval inbuf as ubyte ptr) as long
		if inbuf = NULL then
			'fprintf(stderr, !"Compressed buffer corrupted!\n")
			return -1
		end if
		if outsize < 1 or outbuf = NULL then
			'fprintf(stderr, !"Decompression buffer corrupted: size=%d\n", outsize)
			return -2
		end if
		dim tinfo as info_t
		init_bits(inbuf + 4, NULL, @tinfo)

		dim p as long = 0
		while p < outsize
			if get_bits(1, @tinfo) then
				dim len0 as long
				if get_bits(1, @tinfo) then
					len0 = get_bits(A_BITS_DEF, @tinfo)
				elseif get_bits(1, @tinfo) then
					len0 = get_bits(B_BITS_DEF, @tinfo) + A_DEF
				elseif get_bits(1, @tinfo) then
					len0 = get_bits(C_BITS_DEF, @tinfo) + B_DEF
				elseif get_bits(1, @tinfo) then
					len0 = get_bits(D_BITS_DEF, @tinfo) + C_DEF
				elseif get_bits(1, @tinfo) then
					len0 = get_bits(E_BITS_DEF, @tinfo) + D_DEF
				else
					len0 = get_bits(F_BITS_DEF, @tinfo) + E_DEF
				end if
				dim log0 as  long = get_bits(SLOT_BITS_DEF, @tinfo) + (W_BITS_DEF - NUM_SLOTS_DEF)
				dim s as long = (not iif(log0 > (W_BITS_DEF - NUM_SLOTS_DEF), _
									get_bits(log0, @tinfo) + (1 shl log0), _
									get_bits(W_BITS_DEF - (NUM_SLOTS_DEF - 1), @tinfo))) + p
				if s < 0 then
					'fprintf(stderr, !"Compressed buffer corrupted: s=%d\n", s)
					return -3
				end if
				FILL_OUT_MACRO(outbuf, p, s)
				FILL_OUT_MACRO(outbuf, p, s)
				FILL_OUT_MACRO(outbuf, p, s)
				while len0 <> 0
					FILL_OUT_MACRO(outbuf, p, s)
					len0 -= 1
				wend
			else
				outbuf[p] = get_bits(8, @tinfo)
				p += 1
			end if
		wend
		return p
	end function


	private function lzmat_compressBound(byval insize as long)as long
		if insize = 0 then
			return 0
		elseif insize < 50 then
			return insize * 2 + 16
		elseif insize < 100	then
			return insize * 1.4 + 8
		elseif insize < 1000 then
			return insize * 1.3
		elseif insize < 10000 then
			return insize * 1.25
		end if
		return insize * 1.2 + 32
	end function


#ENDIF   '' _LZMAT_COMPRESS_BI_





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
dim as string sin0 = filetostring( _FILE_TO_COMPRESS_ )               ' _FILE_TO_COMPRESS_  file to test compression, defined at the top


 /' dim as string sin0 = "Good morning Dr. Chandra. This is Hal. I am ready for my first lesson." & chr(0)  &_
        "Good morning Dr. Chandra1. This is Hal. I am ready for my first lesson1." & _
        "Good morning Dr. Chandra2. This is Hal. I am ready for my first lesson2." & _
        "Good morning Dr. Chandra3. This is Hal. I am ready for my first lesson3." & _
        "Good morning Dr. Chandra4. This is Hal. I am ready for my first lesson4." & _
        "Good morning Dr. Chandra5. This is Hal. I am ready for my first lesson5." & _
        "Good morning Dr. Chandra6. This is Hal. I am ready for my first lesson6."  '/


dim as long i0 = len(sin0)
print : print "original size  " & i0 : print



dim         as double d0
dim         as long i2
dim         as long i3
dim         as long iret1

dim         as ubyte ptr enc1
dim         as ubyte ptr dec1




if i0 <> 0 then

    d0 = timer()

    'compressed size needed to allocate compressed buffer ( bigger than input buffer size)
    i2 = lzmat_compressBound(i0)

    i3 = i2


    if i2 < 1 then
        print " leaving now because error on size input buffers "
    else
        enc1 = allocate(i2)                                         'malloc comprresed buffer

        if enc1 = 0 then
            print " Leaving now because error on allocating input compressed buffers "
        else
            'packing now
            i2 = lzmat_compress(enc1, i2 , strptr(sin0) , i0 , _COMPRESSION_LEVEL_)
            if i2 > 1 then
                enc1 = reallocate(enc1 , i2)                        'realloc reduce allocated memory for the only needed size
                d0 = timer() - d0

                print "compression time : " & int(d0 * 1000000) / 1000 & " ms"
                print : print "sizes :   original" , i0 , "compressed" , i2 : print
                print "         ratio  " & i2 / i0 : print
                print "buffer allocated size for compress : " & i3 : print
                print
                i3 = 0

                d0 = timer()
                'check exact size to allocate uncompressed buffer
                iret1 = lzmat_check(enc1)
				'iret1 = i0
				print " verified compressed buffer , size uncompressed = "; iret1 : print
                if iret1 < 1 then
                    print " Leaving now because error on compressed buffer"
                    if enc1 <> 0 then deallocate enc1
                else
                    dec1 = allocate(iret1)                          'malloc uncompressed buffer
                    if dec1 = 0 then
                        print " Leaving now because error on allocating decompressed buffer "
                        if enc1 <> 0 then deallocate enc1           'free allocated compressed buffer
                    else
                        ' unpack now...
                        i3 = lzmat_uncompress( dec1, iret1, enc1)
                        d0 = timer() - d0
                        print : print " Allocated size for decompress buffer    " & iret1 : print
                        if i3 > 0 then
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

            end if
        end if
    end if
else
    print "Empty source file of wrong file : error "
end if

print : print : print "Press any key to  exit"
sleep






