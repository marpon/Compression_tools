 /'
	simple_lzw.bas
	Translated to freebasic  by Marc Pons      email : marpon@aliceadsl.fr
	original code in c,   from rosettacode  LZW encoder/decoder.
	see  :  https://rosettacode.org/wiki/LZW_compression#C
 '/

#include once "crt/string.bi"   /'  for memset, memcpy  and memcmp'/

 /' -------- memory stuff ---------- '/
private function mem_alloc(ByVal item_size as long , ByVal n_item as long) as any ptr
    dim as long ptr x = CAllocate(1 , sizeof(long) * 2 + n_item * item_size)

    x[0] = item_size
    x[1] = n_item
    return cast(any ptr , x + 2)
end function

private function mem_extend(ByVal m as any ptr , ByVal new_n as long) as any ptr
    dim as long ptr x = cast(long ptr , m) - 2

    x = reallocate(x , sizeof(long) * 2 + x[0] * new_n)
    x[1] = new_n
    return cast(any ptr , x + 2)
end function

private sub _clear(ByVal m as any ptr)
    dim as long ptr x = cast(long ptr , m) - 2

    memset(m , 0 , x[0] * x[1])
end sub

#define _new(type, n) 			mem_alloc(sizeof(type), n)
#define _del(m) 					deallocate(cast(long ptr, m) - 2) : m = 0
#define _len(m) 					*(cast(long ptr, m) - 1)
#define _setsize(m, n) 			m = mem_extend(m, n)
#define _extend(m) 				m = mem_extend(m, _len(m) * 2)


 /'  ----------- LZW stuff --------------  '/


#define M_CLR 256			 /'  clear table marker  '/
#define M_EOD 257			 /'  end-of-data marker  '/
#define M_NEW 258 		 /'  new code index  '/

 /'  encode and decode dictionary structures.
   for encoding, entry at code index is a list of indices that follow current one,
   i.e. if code 97 is 'a', code 387 is 'ab', and code 1022 is 'abc',
   then dict[97].next['b'] = 387, dict[387].next['c'] = 1022, etc.  '/

type enc_t
        dim     as ushort next0(0 to 255)
end type


type dec_t
        dim     as ushort prev
        dim     as ushort back
        dim     as ubyte c
end type


'' to simulate inline function, used in  lzw_encode function only
#macro write_bits(x)
    tmp = (tmp Shl bits) or (cast(ushort , x))
    o_bits += bits
    if _len(out0) <= out_len then _extend(out0)
    while o_bits >= 8
        o_bits  -= 8
        out0[out_len] = (tmp Shr o_bits)
        out_len += 1
        tmp = (tmp) and ((1 Shl o_bits) - 1)
    wend
#endmacro

private function lzw_encode(byval in0 as ubyte ptr , byval max_bits as long) as ubyte ptr

    dim as long len0 = _len(in0)
    dim as long bits = 9
    dim as long next_shift = 512
    dim as ushort code
    dim as ushort c
    dim as ushort nc
    dim as ushort next_code = M_NEW
    dim as enc_t ptr d = _new(enc_t , 512)

    if max_bits > 15 then max_bits = 15
    if max_bits < 9 then max_bits = 12

    dim as ubyte ptr out0 = _new(ushort , 4)
    dim as long out_len
    dim as long o_bits
    dim as ulong tmp
    dim as ulong il

    len0 -= 1
    code = in0[il] : il += 1
    while len0 > 0
        c = in0[il] : il += 1
        nc = d[code].next0(c)
        if nc then
            code = nc
        else
            write_bits(code)
            d[code].next0(c) = next_code : nc = next_code : next_code += 1
            code = c
        end if

        /'  next new code would be too long for current table  '/
        if next_code = next_shift then

            /'  either reset table back to 9 bits  '/
            bits += 1
            if bits > max_bits then
                /'  table clear marker must occur before bit reset  '/
                write_bits(M_CLR)
                bits = 9
                next_shift = 512
                next_code = M_NEW
                _clear(d)
            else					 /'  or extend table  '/
                next_shift *= 2
                _setsize(d , next_shift)
            end if
        end if
        len0 -= 1
    wend

    write_bits(code)
    write_bits(M_EOD)
    if tmp then
		write_bits(tmp)
    end if
    _del(d)
    _setsize(out0 , out_len)
    return out0
end function


'' to simulate inline function, used in  lzw_decode function only
#macro write_out(c)
    while out_len >= _len(out0)
        _extend(out0)
    wend
    out0[out_len] = c
    out_len += 1
#endmacro

'' to simulate inline function, used in  lzw_decode function only
#macro get_code
    while n_bits < bits
        if len0 > 0 then
            len0 -= 1
            tmp = (tmp Shl 8)  or (*(in0))
            in0 += 1
            n_bits += 8
        else
            tmp = tmp Shl (bits - n_bits)
            n_bits = bits
        end if
    wend
    n_bits -= bits
    code = tmp Shr n_bits
    tmp = (tmp) and ((1 Shl n_bits) - 1)
#endmacro

'' to simulate inline function, used in  lzw_decode function only
#macro clear_table
    _clear(d)
	for j = 0 to 255
        d[j].c = j
   next
    next_code = M_NEW
    next_shift = 512
    bits = 9
#endmacro


private function lzw_decode(byval in0 as ubyte ptr) as ubyte ptr
    dim as ubyte ptr out0 = _new(ubyte , 4)
	dim as dec_t ptr d = _new(dec_t , 512)
    dim as long next_shift = 512
    dim as long bits = 9
	dim as ushort next_code = M_NEW

    dim as long n_bits
    dim as long len0
    dim as long out_len
    dim as long j
    dim as ushort code
    dim as ushort c
    dim as ushort t
    dim as ulong tmp

    clear_table 		 /'  in case encoded bits didn't start with M_CLR  '/
	len0 = _len(in0)
    do while  len0 <>0
        get_code
        if code = M_EOD then exit do
        if code = M_CLR then
            clear_table
            continue do
        end if
        if code >= next_code then
			print "Bad sequence"
            _del(out0)
            _del(d)
            return out0
        end if
        c = code
        d[next_code].prev = c
        while c > 255
            t = d[c].prev
            d[t].back = c
            c = t
        wend
        d[next_code - 1].c = c
        while d[c].back <> 0
            write_out(d[c].c)
            t = d[c].back
            d[c].back = 0
            c = t
        wend
        write_out(d[c].c)
        next_code += 1
        if next_code >= next_shift then
            bits += 1
            if bits > 16 then
					/'  if input was correct , we     'd have hit M_CLR before this  '/
                print "Too many bits"
                _del(out0)
                _del(d)
                return out0
            end if
            next_shift *= 2
            _setsize(d , next_shift)
        end if
    loop

      /'  might be ok , so just whine , don 't be drastic   '/
    if  code <> M_EOD then print "Bits did not end in EOD"
    _setsize(out0 , out_len)
    _del(d)
    return out0
end function

/' -------- main part  -  to test only ---------- '/

print !"start\n"

dim as string sin0 = "abcdefghijklmnopqrstuvwxyz0123456789zywvutsrqponmlkjihgfedcba9876543210"
 /' dim as string sin0 = "Good morning Dr. Chandra. This is Hal. I am ready for my first lesson." & _
        "Good morning Dr. Chandra1. This is Hal. I am ready for my first lesson1." & _
        "Good morning Dr. Chandra2. This is Hal. I am ready for my first lesson2." & _
        "Good morning Dr. Chandra3. This is Hal. I am ready for my first lesson3." & _
        "Good morning Dr. Chandra4. This is Hal. I am ready for my first lesson4." & _
        "Good morning Dr. Chandra5. This is Hal. I am ready for my first lesson5." & _
        "Good morning Dr. Chandra6. This is Hal. I am ready for my first lesson6." '/



dim as ubyte ptr in0 = _new(ubyte , len(sin0))

memcpy ( in0, strptr(sin0), len(sin0))
print "input size:    " & len(sin0)

dim as ubyte ptr enc0 = lzw_encode(in0 , 9)       ' encoding
print "encoded size:  " & _len(enc0)

dim as ubyte ptr dec0 = lzw_decode(enc0)         ' decoding
print "decoded size:  " & _len(dec0)

if (len(sin0) <> _len(dec0)) or (memcmp (in0, dec0, len(sin0))) <> 0 then
	print : print "Failed comparaison :   decoded versus input"
else
	print : print "Succeeded comparaison :   decoded versus input"
end if


 /'  free allocated memory  '/
_del(in0)
_del(enc0)
_del(dec0)

print : print "Press key to finish"
sleep






