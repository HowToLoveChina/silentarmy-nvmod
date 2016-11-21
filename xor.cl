uint get_row_nr_4(uint xi0, uint round){
uint row;
#if NR_ROWS_LOG == 16
    if (!(round % 2))
        row = (xi0 & 0xffff);
    else
        // if we have in hex: "ab cd ef..." (little endian xi0) then this
        // formula computes the row as 0xdebc. it skips the 'a' nibble as it
        // is part of the PREFIX. The Xi will be stored starting with "ef...";
        // 'e' will be considered padding and 'f' is part of the current PREFIX
        row = ((xi0 & 0xf00) << 4) | ((xi0 & 0xf00000) >> 12) |
            ((xi0 & 0xf) << 4) | ((xi0 & 0xf000) >> 12);
#elif NR_ROWS_LOG == 18
    if (!(round % 2))
        row = (xi0 & 0xffff) | ((xi0 & 0xc00000) >> 6);
    else
        row = ((xi0 & 0xc0000) >> 2) |
            ((xi0 & 0xf00) << 4) | ((xi0 & 0xf00000) >> 12) |
            ((xi0 & 0xf) << 4) | ((xi0 & 0xf000) >> 12);
#elif NR_ROWS_LOG == 19
    if (!(round % 2))
        row = (xi0 & 0xffff) | ((xi0 & 0xe00000) >> 5);
    else
        row = ((xi0 & 0xe0000) >> 1) |
            ((xi0 & 0xf00) << 4) | ((xi0 & 0xf00000) >> 12) |
            ((xi0 & 0xf) << 4) | ((xi0 & 0xf000) >> 12);
#elif NR_ROWS_LOG == 20
    if (!(round % 2))
        row = (xi0 & 0xffff) | ((xi0 & 0xf00000) >> 4);
    else
        row = ((xi0 & 0xf0000) >> 0) |
            ((xi0 & 0xf00) << 4) | ((xi0 & 0xf00000) >> 12) |
            ((xi0 & 0xf) << 4) | ((xi0 & 0xf000) >> 12);
#else
#error "unsupported NR_ROWS_LOG"
#endif
        return row;
}

uint get_row_nr_8(ulong xi0, uint round){
uint row;
#if NR_ROWS_LOG == 16
    if (!(round % 2))
	row = (xi0 & 0xffff);
    else
	// if we have in hex: "ab cd ef..." (little endian xi0) then this
	// formula computes the row as 0xdebc. it skips the 'a' nibble as it
	// is part of the PREFIX. The Xi will be stored starting with "ef...";
	// 'e' will be considered padding and 'f' is part of the current PREFIX
	row = ((xi0 & 0xf00) << 4) | ((xi0 & 0xf00000) >> 12) |
	    ((xi0 & 0xf) << 4) | ((xi0 & 0xf000) >> 12);
#elif NR_ROWS_LOG == 18
    if (!(round % 2))
	row = (xi0 & 0xffff) | ((xi0 & 0xc00000) >> 6);
    else
	row = ((xi0 & 0xc0000) >> 2) |
	    ((xi0 & 0xf00) << 4) | ((xi0 & 0xf00000) >> 12) |
	    ((xi0 & 0xf) << 4) | ((xi0 & 0xf000) >> 12);
#elif NR_ROWS_LOG == 19
    if (!(round % 2))
	row = (xi0 & 0xffff) | ((xi0 & 0xe00000) >> 5);
    else
	row = ((xi0 & 0xe0000) >> 1) |
	    ((xi0 & 0xf00) << 4) | ((xi0 & 0xf00000) >> 12) |
	    ((xi0 & 0xf) << 4) | ((xi0 & 0xf000) >> 12);
#elif NR_ROWS_LOG == 20
    if (!(round % 2))
	row = (xi0 & 0xffff) | ((xi0 & 0xf00000) >> 4);
    else
	row = ((xi0 & 0xf0000) >> 0) |
	    ((xi0 & 0xf00) << 4) | ((xi0 & 0xf00000) >> 12) |
	    ((xi0 & 0xf) << 4) | ((xi0 & 0xf000) >> 12);
#else
#error "unsupported NR_ROWS_LOG"
#endif
	return row;
}

ulong load8_last(__global ulong *p){
        ulong r;
        asm volatile ( "ld.global.lu.b64  %0, [%1];\n\t" : "=l"(r) : "l"(p));
        return r;
       // return *p;
}

void trigger_err(){
	load8_last((__global ulong *)-1);
}

#define nv64to16(a,b,c,d,X) asm volatile( "mov.b64 {%0,%1,%2,%3}, %4; \n\t" : "=r"(a), "=r"(b), "=r"(c), "=r"(d) : "r"(X))


// Round 1

uint xor_and_store1(uint round, __global char *ht_dst, uint x_row,
        uint slot_a, uint slot_b, __global ulong *a, __global ulong *b,
        __global uint *rowCounters){
	
	ulong xi0, xi1, xi2,xi3;
	uint _row;
	uint row;
	__global char       *p;
        uint                cnt;
//LOAD
	ulong2 loada,loadb;
	xi0 = *(a++) ^ *(b++);
	loada = *(__global ulong2 *)a;
	loadb = *(__global ulong2 *)b;
	xi1 = loada.x ^ loadb.x;
	xi2 = loada.y ^ loadb.y;
/*
	xi0 = *(a++) ^ *(b++);
	xi1 = *(a++) ^ *(b++);
	xi2 = *a ^ *b;
	xi3 = 0;
*/
//
	uint i = ENCODE_INPUTS(x_row, slot_a, slot_b);
	

	//256bit shift


	asm("{ .reg .b16 a0,a1,a2,a3,b0,b1,b2,b3,c0,c1,c2,c3; \n\t"
	"mov.b64 {a0,a1,a2,a3}, %4;\n\t"
	"mov.b64 {b0,b1,b2,b3}, %5;\n\t"
	"mov.b64 {c0,c1,c2,c3}, %6;\n\t"

	"mov.b64 %0, {a1,a2,a3,b0};\n\t"
	"mov.b64 %1, {b1,b2,b3,c0};\n\t"
	"mov.b64 %2, {c1,c2,c3,0};\n\t"
	"mov.b32 %3, {a0,a1};\n\t"
	"}\n" : "=l"(xi0), "=l"(xi1), "=l" (xi2), "=r"(_row): "l"(xi0), "l"(xi1), "l"(xi2));


//      row = get_row_nr_4((uint)xi0,round);	
	row = get_row_nr_4(_row,round);

//        xi0 = (xi0 >> 16) | (xi1 << (64 - 16));
//        xi1 = (xi1 >> 16) | (xi2 << (64 - 16));
//        xi2 = (xi2 >> 16);
	
//
	
    p = ht_dst + row * NR_SLOTS * SLOT_LEN;
    uint rowIdx = row/ROWS_PER_UINT;
    uint rowOffset = BITS_PER_ROW*(row%ROWS_PER_UINT);
    uint xcnt = atomic_add(rowCounters + rowIdx, 1 << rowOffset);
    xcnt = (xcnt >> rowOffset) & ROW_MASK;
    cnt = xcnt;
    if (cnt >= NR_SLOTS)
      {
	// avoid overflows
	atomic_sub(rowCounters + rowIdx, 1 << rowOffset);
	return 1;
      }
    __global char       *pp = p + cnt * SLOT_LEN;
    p = pp + xi_offset_for_round(round);
//

//STORE
//        *(__global uint *)(p - 4) = i;
//        *(__global ulong *)(p + 0) = xi0;
//	*(__global ulong *)(p + 8) = xi1;
//	*(__global ulong *)(p + 16) = xi2;


	ulong2 store0;
	ulong2 store1;
	nv32to64(store0.x,0,i);
	store0.y=xi0;
	*(__global ulong2 *)(pp)=store0;
	store1.x=xi1;
	store1.y=xi2;
	*(__global ulong2 *)(pp+16)=store1;

return 0;
}

