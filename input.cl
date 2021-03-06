#include "param.h"

#pragma OPENCL EXTENSION cl_khr_global_int32_base_atomics : enable

/*
** Assuming NR_ROWS_LOG == 16, the hash table slots have this layout (length in
** bytes in parens):
**
** round 0, table 0: cnt(4) i(4)                     pad(0)   Xi(23.0) pad(1)
** round 1, table 1: cnt(4) i(4)                     pad(0.5) Xi(20.5) pad(3)
** round 2, table 0: cnt(4) i(4) i(4)                pad(0)   Xi(18.0) pad(2)
** round 3, table 1: cnt(4) i(4) i(4)                pad(0.5) Xi(15.5) pad(4)
** round 4, table 0: cnt(4) i(4) i(4) i(4)           pad(0)   Xi(13.0) pad(3)
** round 5, table 1: cnt(4) i(4) i(4) i(4)           pad(0.5) Xi(10.5) pad(5)
** round 6, table 0: cnt(4) i(4) i(4) i(4) i(4)      pad(0)   Xi( 8.0) pad(4)
** round 7, table 1: cnt(4) i(4) i(4) i(4) i(4)      pad(0.5) Xi( 5.5) pad(6)
** round 8, table 0: cnt(4) i(4) i(4) i(4) i(4) i(4) pad(0)   Xi( 3.0) pad(5)
**
** If the first byte of Xi is 0xAB then:
** - on even rounds, 'A' is part of the colliding PREFIX, 'B' is part of Xi
** - on odd rounds, 'A' and 'B' are both part of the colliding PREFIX, but
**   'A' is considered redundant padding as it was used to compute the row #
**
** - cnt is an atomic counter keeping track of the number of used slots.
**   it is used in the first slot only; subsequent slots replace it with
**   4 padding bytes
** - i encodes either the 21-bit input value (round 0) or a reference to two
**   inputs from the previous round
**
** Formula for Xi length and pad length above:
** > for i in range(9):
** >   xi=(200-20*i-NR_ROWS_LOG)/8.; ci=8+4*((i)/2); print xi,32-ci-xi
**
** Note that the fractional .5-byte/4-bit padding following Xi for odd rounds
** is the 4 most significant bits of the last byte of Xi.
*/

__constant ulong blake_iv[] =
{
    0x6a09e667f3bcc908, 0xbb67ae8584caa73b,
    0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1,
    0x510e527fade682d1, 0x9b05688c2b3e6c1f,
    0x1f83d9abfb41bd6b, 0x5be0cd19137e2179,
};

/*
** Reset counters in hash table.
*/
__kernel
void kernel_init_ht(__global char *ht, __global uint *rowCounters)
{
    rowCounters[get_global_id(0)] = 0;
}

/*
** If xi0,xi1,xi2,xi3 are stored consecutively in little endian then they
** represent (hex notation, group of 5 hex digits are a group of PREFIX bits):
**   aa aa ab bb bb cc cc cd dd...  [round 0]
**         --------------------
**      ...ab bb bb cc cc cd dd...  [odd round]
**               --------------
**               ...cc cc cd dd...  [next even round]
**                        -----
** Bytes underlined are going to be stored in the slot. Preceding bytes
** (and possibly part of the underlined bytes, depending on NR_ROWS_LOG) are
** used to compute the row number.
**
** Round 0: xi0,xi1,xi2,xi3 is a 25-byte Xi (xi3: only the low byte matter)
** Round 1: xi0,xi1,xi2 is a 23-byte Xi (incl. the colliding PREFIX nibble)
** TODO: update lines below with padding nibbles
** Round 2: xi0,xi1,xi2 is a 20-byte Xi (xi2: only the low 4 bytes matter)
** Round 3: xi0,xi1,xi2 is a 17.5-byte Xi (xi2: only the low 1.5 bytes matter)
** Round 4: xi0,xi1 is a 15-byte Xi (xi1: only the low 7 bytes matter)
** Round 5: xi0,xi1 is a 12.5-byte Xi (xi1: only the low 4.5 bytes matter)
** Round 6: xi0,xi1 is a 10-byte Xi (xi1: only the low 2 bytes matter)
** Round 7: xi0 is a 7.5-byte Xi (xi0: only the low 7.5 bytes matter)
** Round 8: xi0 is a 5-byte Xi (xi0: only the low 5 bytes matter)
**
** Return 0 if successfully stored, or 1 if the row overflowed.
*/

#define nv64to32(low,high,X) asm volatile( "mov.b64 {%0,%1}, %2; \n\t" : "=r"(low), "=r"(high) : "d"(X))
#define nv32to64(X,low,high) asm volatile( "mov.b64 %0,{%1, %2}; \n\t": "=l"(X) : "r"(low), "r"(high))






uint ht_store(uint round, __global char *ht, uint i,
	ulong xi0, ulong xi1, ulong xi2, ulong xi3, __global uint *rowCounters)
{
    uint    row;
    __global char       *p;
    uint                cnt;
   uint                tid = get_global_id(0);
uint                tlid = get_local_id(0);
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

/*
    xi0 = (xi0 >> 16) | (xi1 << (64 - 16));
    xi1 = (xi1 >> 16) | (xi2 << (64 - 16));
    xi2 = (xi2 >> 16) | (xi3 << (64 - 16));
*/
//256bit shfr
    if (round == 0 || round == 1 || round == 2)
      {
    xi0 = (xi0 >> 16) | (xi1 << (64 - 16));
    xi1 = (xi1 >> 16) | (xi2 << (64 - 16));
    xi2 = (xi2 >> 16) | (xi3 << (64 - 16));
	}else if (round == 3 || round == 4 || round == 5){
    xi0 = (xi0 >> 16) | (xi1 << (64 - 16));
    xi1 = (xi1 >> 16) | (xi2 << (64 - 16));
	}else if (round == 6 || round == 7 || round == 8){
    xi0 = (xi0 >> 16) | (xi1 << (64 - 16));
/*	uint xi0_0=(uint)(xi0);
	uint xi0_1=(uint)(xi0 >> 32);
	uint xi1_0=(uint)(xi1);
        uint xi1_1=(uint)(xi1 >> 32);
*/
/*
	uint xi0l,xi0h,xi1l,xi1h;
	uint _xi0l,_xi0h,_xi1l,_xi1h;
	nv64to32(xi0l,xi0h,xi0);
	nv64to32(xi1l,xi1h,xi1);
	asm("{\n\t"
			"shf.r.clamp.b32 %0,%4,%5,%8; \n\t"
                        "shf.r.clamp.b32 %1,%5,%6,%8; \n\t"
                        "shf.r.clamp.b32 %2,%6,%7,%8; \n\t"
                        "shr.b32 %3,%7,%8; \n\t"
			"}\n\t"
			: "=r"(_xi0l), "=r"(_xi0h),"=r"(_xi1l), "=r"(_xi1h):  "r"(xi0l), "r"(xi0h),"r"(xi1l), "r"(xi1h) , "r"(16));
	nv32to64(xi0,_xi0l,_xi0h);
*/

/*
        uint xi0l,xi0h,xi1l,xi1h;
        uint _xi0l,_xi0h,_xi1l,_xi1h;
        nv64to32(xi0l,xi0h,xi0);
        nv64to32(xi1l,xi1h,xi1);
        asm("{\n\t"
                        "shf.r.clamp.b32 %0,%4,%5,%8; \n\t"
                        "shf.r.clamp.b32 %1,%5,%6,%8; \n\t"
                        "}\n\t"
                        : "=r"(_xi0l), "=r"(_xi0h),"=r"(_xi1l), "=r"(_xi1h):  "r"(xi0l), "r"(xi0h),"r"(xi1l), "r"(xi1h) , "r"(16));
        nv32to64(xi0,_xi0l,_xi0h);
*/

        }

    p = ht + row * NR_SLOTS * SLOT_LEN;
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
    // store "i" (always 4 bytes before Xi)
    if (round == 0 || round == 1)
      {
	// store 24 bytes
	ulong2 store0;
	ulong2 store1;
	//store0.x=(ulong)i  | (ulong)i << 32;
	nv32to64(store0.x,i,i);
	store0.y=xi0;
	*(__global ulong2 *)(pp)=store0;
	store1.x=xi1;
	store1.y=xi2;
	*(__global ulong2 *)(pp+16)=store1;
	
/*
	ulong2 store;
	store.x=xi1;
	store.y=xi2;
	*(__global ulong2 *)(p + 8)=store;	
	*(__global ulong *)(p + 0) = xi0;
	*(__global uint *)(p - 4) = i;
*/

      }
    else if (round == 2)
      {
	// store 20 bytes
/*
	*(__global ulong *)(p - 4) = ((ulong)i) | (xi0 << 32);
	*(__global ulong *)(p + 4) = (xi0 >> 32) | (xi1 << 32);
	*(__global ulong *)(p + 12) = (xi1 >> 32) | (xi2 << 32);
*/
	*(__global uint *)(p - 4) = i;
	*(__global uint *)(p + 0) = xi0;

/*	
	uint xi0l, xi0h, xi1l, xi1h, xi2l, xi2h;
	nv64to32(xi0l, xi0h, xi0);
	nv64to32(xi1l, xi1h, xi1);
	nv64to32(xi2l, xi2h, xi2);
	*(__global uint *)(p + 0) = xi0l;
	ulong s1,s2;
	nv32to64(s1,xi0h,xi1l);
	*(__global ulong *)(p + 4) = s1;
        nv32to64(s2,xi1h,xi2l);
        *(__global ulong *)(p + 12) = s2;
*/
/*
	*(__global uint *)(p + 4) = xi0h;
	*(__global uint *)(p + 8) = xi1l;
	*(__global uint *)(p + 12) = xi1h;
	*(__global uint *)(p + 16) = xi2l;
*/


	*(__global ulong *)(p + 4) = (xi0 >> 32) | (xi1 << 32);
	*(__global ulong *)(p + 12) = (xi1 >> 32) | (xi2 << 32);
      }
    else if (round == 3)
      {
	// store 16 bytes
	//8 byte align	

	*(__global ulong *)(p - 4) = ((ulong)i) | (xi0 << 32);
	*(__global ulong *)(p + 4) = (xi0 >> 32) | (xi1 << 32);
	*(__global uint *)(p + 12) = (xi1 >> 32);
	*(__global uint *)(p - 4) = i;
	uint xi0l, xi0h, xi1l, xi1h;
        nv64to32(xi0l, xi0h, xi0);
        nv64to32(xi1l, xi1h, xi1);
        *(__global uint *)(p + 0) = xi0l;
        ulong s1,s2;
        nv32to64(s1,xi0h,xi1l);
        *(__global ulong *)(p + 4) = s1;
	*(__global uint *)(p + 12) = xi1h;

//	*(__global uint *)(p + 0) = xi0;
//	*(__global ulong *)(p + 4) = (xi0 >> 32) | (xi1 << 32);
//	*(__global uint *)(p + 12) = (xi1 >> 32);
      }
    else if (round == 4)
      {
	// store 16 bytes

	ulong2 store;
        store.x=xi0;
        store.y=xi1;
        *(__global ulong2 *)(p + 0) = store;
	*(__global uint *)(p - 4) = i;

/*
	*(__global uint *)(p - 4) = i;
	*(__global ulong *)(p + 0) = xi0;
	*(__global ulong *)(p + 8) = xi1;
*/
      }
    else if (round == 5)
      {
	// store 12 bytes
	*(__global uint *)(p - 4) = i;
	*(__global ulong *)(p + 0) = xi0;

	*(__global uint *)(p + 8) = xi1;
      }
    else if (round == 6 || round == 7)
      {
	// store 8 bytes
/*
	uint xi0l,xi0h;
	nv64to32(xi0l,xi0h,xi0);
	ulong s1;
	nv32to64(s1,i,xi0l);
	*(__global ulong *)(p - 4) = s1;
	*(__global uint *)(p + 4) = xi0h;
*/
	*(__global ulong *)(p - 4) = ((ulong)i) | (xi0 << 32);
	*(__global uint *)(p + 4) = (xi0 >> 32);

/*
	*(__global uint *)(p - 4) = i;
	*(__global uint *)(p + 0) = xi0;
	*(__global uint *)(p + 4) = (xi0 >> 32);
*/	
      }
    else if (round == 8)
      {
	//4 byte align
	*(__global uint *)(p - 4) = i;
	// store 4 bytes
	*(__global uint *)(p + 0) = xi0;

      }

// *(__global uint *)(p - 4) = i;
    return 0;
}




#define mix(va, vb, vc, vd, x, y) \
    va = (va + vb + x); \
vd = rotate((vd ^ va), (ulong)64 - 32); \
vc = (vc + vd); \
vb = rotate((vb ^ vc), (ulong)64 - 24); \
va = (va + vb + y); \
vd = rotate((vd ^ va), (ulong)64 - 16); \
vc = (vc + vd); \
vb = rotate((vb ^ vc), (ulong)64 - 63);

/*
** Execute round 0 (blake).
**
** Note: making the work group size less than or equal to the wavefront size
** allows the OpenCL compiler to remove the barrier() calls, see "2.2 Local
** Memory (LDS) Optimization 2-10" in:
** http://developer.amd.com/tools-and-sdks/opencl-zone/amd-accelerated-parallel-processing-app-sdk/opencl-optimization-guide/
*/
__kernel __attribute__((reqd_work_group_size(THRD, 1, 1)))
void kernel_round0(__global ulong *blake_state, __global char *ht,
	__global uint *rowCounters, __global uint *debug)
{
    uint                tid = get_global_id(0);
    ulong               v[16];
    uint                inputs_per_thread = NR_INPUTS / get_global_size(0);
    uint                input = tid * inputs_per_thread;
    uint                input_end = (tid + 1) * inputs_per_thread;
    uint                dropped = 0;
    while (input < input_end)
      {
	// shift "i" to occupy the high 32 bits of the second ulong word in the
	// message block
	ulong word1 = (ulong)input << 32;
	// init vector v
	v[0] = blake_state[0];
	v[1] = blake_state[1];
	v[2] = blake_state[2];
	v[3] = blake_state[3];
	v[4] = blake_state[4];
	v[5] = blake_state[5];
	v[6] = blake_state[6];
	v[7] = blake_state[7];
	v[8] =  blake_iv[0];
	v[9] =  blake_iv[1];
	v[10] = blake_iv[2];
	v[11] = blake_iv[3];
	v[12] = blake_iv[4];
	v[13] = blake_iv[5];
	v[14] = blake_iv[6];
	v[15] = blake_iv[7];
	// mix in length of data
	v[12] ^= ZCASH_BLOCK_HEADER_LEN + 4 /* length of "i" */;
	// last block
	v[14] ^= (ulong)-1;

	// round 1
	mix(v[0], v[4], v[8],  v[12], 0, word1);
	mix(v[1], v[5], v[9],  v[13], 0, 0);
	mix(v[2], v[6], v[10], v[14], 0, 0);
	mix(v[3], v[7], v[11], v[15], 0, 0);
	mix(v[0], v[5], v[10], v[15], 0, 0);
	mix(v[1], v[6], v[11], v[12], 0, 0);
	mix(v[2], v[7], v[8],  v[13], 0, 0);
	mix(v[3], v[4], v[9],  v[14], 0, 0);
	// round 2
	mix(v[0], v[4], v[8],  v[12], 0, 0);
	mix(v[1], v[5], v[9],  v[13], 0, 0);
	mix(v[2], v[6], v[10], v[14], 0, 0);
	mix(v[3], v[7], v[11], v[15], 0, 0);
	mix(v[0], v[5], v[10], v[15], word1, 0);
	mix(v[1], v[6], v[11], v[12], 0, 0);
	mix(v[2], v[7], v[8],  v[13], 0, 0);
	mix(v[3], v[4], v[9],  v[14], 0, 0);
	// round 3
	mix(v[0], v[4], v[8],  v[12], 0, 0);
	mix(v[1], v[5], v[9],  v[13], 0, 0);
	mix(v[2], v[6], v[10], v[14], 0, 0);
	mix(v[3], v[7], v[11], v[15], 0, 0);
	mix(v[0], v[5], v[10], v[15], 0, 0);
	mix(v[1], v[6], v[11], v[12], 0, 0);
	mix(v[2], v[7], v[8],  v[13], 0, word1);
	mix(v[3], v[4], v[9],  v[14], 0, 0);
	// round 4
	mix(v[0], v[4], v[8],  v[12], 0, 0);
	mix(v[1], v[5], v[9],  v[13], 0, word1);
	mix(v[2], v[6], v[10], v[14], 0, 0);
	mix(v[3], v[7], v[11], v[15], 0, 0);
	mix(v[0], v[5], v[10], v[15], 0, 0);
	mix(v[1], v[6], v[11], v[12], 0, 0);
	mix(v[2], v[7], v[8],  v[13], 0, 0);
	mix(v[3], v[4], v[9],  v[14], 0, 0);
	// round 5
	mix(v[0], v[4], v[8],  v[12], 0, 0);
	mix(v[1], v[5], v[9],  v[13], 0, 0);
	mix(v[2], v[6], v[10], v[14], 0, 0);
	mix(v[3], v[7], v[11], v[15], 0, 0);
	mix(v[0], v[5], v[10], v[15], 0, word1);
	mix(v[1], v[6], v[11], v[12], 0, 0);
	mix(v[2], v[7], v[8],  v[13], 0, 0);
	mix(v[3], v[4], v[9],  v[14], 0, 0);
	// round 6
	mix(v[0], v[4], v[8],  v[12], 0, 0);
	mix(v[1], v[5], v[9],  v[13], 0, 0);
	mix(v[2], v[6], v[10], v[14], 0, 0);
	mix(v[3], v[7], v[11], v[15], 0, 0);
	mix(v[0], v[5], v[10], v[15], 0, 0);
	mix(v[1], v[6], v[11], v[12], 0, 0);
	mix(v[2], v[7], v[8],  v[13], 0, 0);
	mix(v[3], v[4], v[9],  v[14], word1, 0);
	// round 7
	mix(v[0], v[4], v[8],  v[12], 0, 0);
	mix(v[1], v[5], v[9],  v[13], word1, 0);
	mix(v[2], v[6], v[10], v[14], 0, 0);
	mix(v[3], v[7], v[11], v[15], 0, 0);
	mix(v[0], v[5], v[10], v[15], 0, 0);
	mix(v[1], v[6], v[11], v[12], 0, 0);
	mix(v[2], v[7], v[8],  v[13], 0, 0);
	mix(v[3], v[4], v[9],  v[14], 0, 0);
	// round 8
	mix(v[0], v[4], v[8],  v[12], 0, 0);
	mix(v[1], v[5], v[9],  v[13], 0, 0);
	mix(v[2], v[6], v[10], v[14], 0, word1);
	mix(v[3], v[7], v[11], v[15], 0, 0);
	mix(v[0], v[5], v[10], v[15], 0, 0);
	mix(v[1], v[6], v[11], v[12], 0, 0);
	mix(v[2], v[7], v[8],  v[13], 0, 0);
	mix(v[3], v[4], v[9],  v[14], 0, 0);
	// round 9
	mix(v[0], v[4], v[8],  v[12], 0, 0);
	mix(v[1], v[5], v[9],  v[13], 0, 0);
	mix(v[2], v[6], v[10], v[14], 0, 0);
	mix(v[3], v[7], v[11], v[15], 0, 0);
	mix(v[0], v[5], v[10], v[15], 0, 0);
	mix(v[1], v[6], v[11], v[12], 0, 0);
	mix(v[2], v[7], v[8],  v[13], word1, 0);
	mix(v[3], v[4], v[9],  v[14], 0, 0);
	// round 10
	mix(v[0], v[4], v[8],  v[12], 0, 0);
	mix(v[1], v[5], v[9],  v[13], 0, 0);
	mix(v[2], v[6], v[10], v[14], 0, 0);
	mix(v[3], v[7], v[11], v[15], word1, 0);
	mix(v[0], v[5], v[10], v[15], 0, 0);
	mix(v[1], v[6], v[11], v[12], 0, 0);
	mix(v[2], v[7], v[8],  v[13], 0, 0);
	mix(v[3], v[4], v[9],  v[14], 0, 0);
	// round 11
	mix(v[0], v[4], v[8],  v[12], 0, word1);
	mix(v[1], v[5], v[9],  v[13], 0, 0);
	mix(v[2], v[6], v[10], v[14], 0, 0);
	mix(v[3], v[7], v[11], v[15], 0, 0);
	mix(v[0], v[5], v[10], v[15], 0, 0);
	mix(v[1], v[6], v[11], v[12], 0, 0);
	mix(v[2], v[7], v[8],  v[13], 0, 0);
	mix(v[3], v[4], v[9],  v[14], 0, 0);
	// round 12
	mix(v[0], v[4], v[8],  v[12], 0, 0);
	mix(v[1], v[5], v[9],  v[13], 0, 0);
	mix(v[2], v[6], v[10], v[14], 0, 0);
	mix(v[3], v[7], v[11], v[15], 0, 0);
	mix(v[0], v[5], v[10], v[15], word1, 0);
	mix(v[1], v[6], v[11], v[12], 0, 0);
	mix(v[2], v[7], v[8],  v[13], 0, 0);
	mix(v[3], v[4], v[9],  v[14], 0, 0);

	// compress v into the blake state; this produces the 50-byte hash
	// (two Xi values)
	ulong h[7];
	h[0] = blake_state[0] ^ v[0] ^ v[8];
	h[1] = blake_state[1] ^ v[1] ^ v[9];
	h[2] = blake_state[2] ^ v[2] ^ v[10];
	h[3] = blake_state[3] ^ v[3] ^ v[11];
	h[4] = blake_state[4] ^ v[4] ^ v[12];
	h[5] = blake_state[5] ^ v[5] ^ v[13];
	h[6] = (blake_state[6] ^ v[6] ^ v[14]) & 0xffff;

	// store the two Xi values in the hash table
#if ZCASH_HASH_LEN == 50
	dropped += ht_store(0, ht, input * 2,
		h[0],
		h[1],
		h[2],
		h[3], rowCounters);
	dropped += ht_store(0, ht, input * 2 + 1,
		(h[3] >> 8) | (h[4] << (64 - 8)),
		(h[4] >> 8) | (h[5] << (64 - 8)),
		(h[5] >> 8) | (h[6] << (64 - 8)),
		(h[6] >> 8), rowCounters);
#else
#error "unsupported ZCASH_HASH_LEN"
#endif

	input++;
      }
#ifdef ENABLE_DEBUG
    debug[tid * 2] = 0;
    debug[tid * 2 + 1] = dropped;
#endif
}

#if NR_ROWS_LOG <= 16 && NR_SLOTS <= (1 << 8)

#define ENCODE_INPUTS(row, slot0, slot1) \
    ((row << 16) | ((slot1 & 0xff) << 8) | (slot0 & 0xff))
#define DECODE_ROW(REF)   (REF >> 16)
#define DECODE_SLOT1(REF) ((REF >> 8) & 0xff)
#define DECODE_SLOT0(REF) (REF & 0xff)

#elif NR_ROWS_LOG == 18 && NR_SLOTS <= (1 << 7)

#define ENCODE_INPUTS(row, slot0, slot1) \
    ((row << 14) | ((slot1 & 0x7f) << 7) | (slot0 & 0x7f))
#define DECODE_ROW(REF)   (REF >> 14)
#define DECODE_SLOT1(REF) ((REF >> 7) & 0x7f)
#define DECODE_SLOT0(REF) (REF & 0x7f)

#elif NR_ROWS_LOG == 19 && NR_SLOTS <= (1 << 6)

#define ENCODE_INPUTS(row, slot0, slot1) \
    ((row << 13) | ((slot1 & 0x3f) << 6) | (slot0 & 0x3f)) /* 1 spare bit */
#define DECODE_ROW(REF)   (REF >> 13)
#define DECODE_SLOT1(REF) ((REF >> 6) & 0x3f)
#define DECODE_SLOT0(REF) (REF & 0x3f)

#elif NR_ROWS_LOG == 20 && NR_SLOTS <= (1 << 6)

#define ENCODE_INPUTS(row, slot0, slot1) \
    ((row << 12) | ((slot1 & 0x3f) << 6) | (slot0 & 0x3f))
#define DECODE_ROW(REF)   (REF >> 12)
#define DECODE_SLOT1(REF) ((REF >> 6) & 0x3f)
#define DECODE_SLOT0(REF) (REF & 0x3f)

#else
#error "unsupported NR_ROWS_LOG"
#endif


uint read_uint_once(__global uint *p){
	uint r;
	asm volatile( "ld.global.cg.b32  %0, [%1];\n\t" : "=r"(r) : "l"(p));
	return r;
	//return *p;
}

ulong read_ulong_once(__global ulong *p){
        ulong r;
        asm volatile ( "ld.global.lu.b64  %0, [%1];\n\t" : "=l"(r) : "l"(p));
        return r;
       // return *p;
}


/*
** Access a half-aligned long, that is a long aligned on a 4-byte boundary.
*/
ulong half_aligned_long(__global ulong *p, uint offset)
{
    return
	(((ulong)*(__global uint *)((__global char *)p + offset + 0)) << 0) |
	(((ulong)*(__global uint *)((__global char *)p + offset + 4)) << 32);
}

ulong xor_unaligned_long(__global ulong *a,__global ulong *b, uint offset)
{
	uint l1 = *((__global uint *)((__global char *)a + offset + 0)) ^ *(__global uint *)((__global char *)b + offset + 0);
	uint l2 = *(__global uint *)((__global char *)a + offset + 4) ^ *(__global uint *)((__global char *)b + offset + 4);
	return ((ulong)l1 << 0 | (ulong)l2 << 32);
}

/*
** Access a well-aligned int.
*/
uint well_aligned_int(__global ulong *_p, uint offset)
{
    __global char *p = (__global char *)_p;
//    return *(__global uint *)(p + offset);
	return read_uint_once((__global uint *)(p + offset));
}

/*
** Access a well-aligned long.
*/
ulong well_aligned_long(__global ulong *_p, uint offset)
{
    __global char *p = (__global char *)_p;
    return read_ulong_once((__global ulong *)(p + offset));
}

/*
** XOR a pair of Xi values computed at "round - 1" and store the result in the
** hash table being built for "round". Note that when building the table for
** even rounds we need to skip 1 padding byte present in the "round - 1" table
** (the "0xAB" byte mentioned in the description at the top of this file.) But
** also note we can't load data directly past this byte because this would
** cause an unaligned memory access which is undefined per the OpenCL spec.
**
** Return 0 if successfully stored, or 1 if the row overflowed.
*/

#include "xor.cl"

uint xor_and_store(uint round, __global char *ht_dst, uint row,
	uint slot_a, uint slot_b, __global ulong *a, __global ulong *b,
	__global uint *rowCounters)
{

	if(round == 1)
		return xor_and_store1(round,ht_dst,row,slot_a,slot_b,a,b,rowCounters);
	else if(round == 2)
                return xor_and_store2(round,ht_dst,row,slot_a,slot_b,a,b,rowCounters);
	else if(round == 3)
		return xor_and_store3(round,ht_dst,row,slot_a,slot_b,a,b,rowCounters);
	else if(round == 4)
                return xor_and_store4(round,ht_dst,row,slot_a,slot_b,a,b,rowCounters);
	else if(round == 5)
                return xor_and_store5(round,ht_dst,row,slot_a,slot_b,a,b,rowCounters);
	else if(round == 6)
                return xor_and_store6(round,ht_dst,row,slot_a,slot_b,a,b,rowCounters);
        else if(round == 7)
                return xor_and_store7(round,ht_dst,row,slot_a,slot_b,a,b,rowCounters);
        else if(round == 8)
                return xor_and_store8(round,ht_dst,row,slot_a,slot_b,a,b,rowCounters);
}


/*
** Execute one Equihash round. Read from ht_src, XOR colliding pairs of Xi,
** store them in ht_dst.
*/
void equihash_round(uint round,
  __global char *ht_src,
  __global char *ht_dst,
  __global uint *debug,
  __local uint *collisionsData,
  __local uint *collisionsNum,
  __global uint *rowCountersSrc,
  __global uint *rowCountersDst)
{
    uint globalTid = get_global_id(0) / THREADS_PER_ROW;
    uint localRowIdx = get_local_id(0) / THREADS_PER_ROW;
    uint localTid = get_local_id(0) % THREADS_PER_ROW;
    __local uint slotCountersData[COLLISION_TYPES_NUM*ROWS_PER_WORKGROUP];
    __local ushort slotsData[COLLISION_TYPES_NUM*COLLISION_BUFFER_SIZE*ROWS_PER_WORKGROUP];
    
    __local uint *slotCounters = &slotCountersData[COLLISION_TYPES_NUM*localRowIdx];
    __local ushort *slots = &slotsData[COLLISION_TYPES_NUM*COLLISION_BUFFER_SIZE*localRowIdx];
    
    __global char *p;
    uint    cnt;
    uchar   mask;
    uint    shift;
    uint    i, j;
    // NR_SLOTS is already oversized (by a factor of OVERHEAD), but we want to
    // make it even larger
    uint    n;
    uint    dropped_coll = 0;
    uint    dropped_stor = 0;
    __global ulong  *a, *b;
    uint    xi_offset;
    // read first words of Xi from the previous (round - 1) hash table
    xi_offset = xi_offset_for_round(round - 1);
    // the mask is also computed to read data from the previous round
#if NR_ROWS_LOG <= 16
    mask = ((!(round % 2)) ? 0x0f : 0xf0);
    shift = ((!(round % 2)) ? 0 : 4);
#elif NR_ROWS_LOG == 18
    mask = ((!(round % 2)) ? 0x03 : 0x30);
    shift = ((!(round % 2)) ? 0 : 4);    
#elif NR_ROWS_LOG == 19
    mask = ((!(round % 2)) ? 0x01 : 0x10);
    shift = ((!(round % 2)) ? 0 : 4);    
#elif NR_ROWS_LOG == 20
    mask = 0; /* we can vastly simplify the code below */
    shift = 0;
#else
#error "unsupported NR_ROWS_LOG"
#endif    
    
  for (uint chunk = 0; chunk < THREADS_PER_ROW; chunk++) {
    uint tid = globalTid + NR_ROWS/THREADS_PER_ROW*chunk;
    uint gid = tid & ~(ROWS_PER_WORKGROUP - 1);
    
    uint rowIdx = tid/ROWS_PER_UINT;
    uint rowOffset = BITS_PER_ROW*(tid%ROWS_PER_UINT);
    cnt = (rowCountersSrc[rowIdx] >> rowOffset) & ROW_MASK;
    cnt = min(cnt, (uint)NR_SLOTS); // handle possible overflow in prev. round

    *collisionsNum = 0;
    p = (ht_src + tid * NR_SLOTS * SLOT_LEN);
    p += xi_offset;
    p += SLOT_LEN*localTid;

    for (i = get_local_id(0); i < COLLISION_TYPES_NUM*ROWS_PER_WORKGROUP; i += get_local_size(0))
      slotCountersData[i] = 0;
    barrier(CLK_LOCAL_MEM_FENCE);
    
    for (i = localTid; i < cnt; i += THREADS_PER_ROW, p += SLOT_LEN*THREADS_PER_ROW) {
//      uchar x = ((*(__global uchar *)p) & mask) >> shift;
      uchar x = (uchar)(load4((__global ulong *)p, 0 ) & mask)  >> shift;
      uint slotIdx = atomic_inc(&slotCounters[x]);
      slotIdx = min(slotIdx, COLLISION_BUFFER_SIZE-1);
      slots[COLLISION_BUFFER_SIZE*x+slotIdx] = i;
    }
    
    barrier(CLK_LOCAL_MEM_FENCE);
    const uint ct_groupsize = max(1u, THREADS_PER_ROW / COLLISION_TYPES_NUM);
    for (uint collTypeIdx = localTid / ct_groupsize; collTypeIdx < COLLISION_TYPES_NUM; collTypeIdx += THREADS_PER_ROW / ct_groupsize) {
      const uint N = min((uint)slotCounters[collTypeIdx], COLLISION_BUFFER_SIZE);
      for (uint i = 0; i < N; i++) {
        uint collision = (localRowIdx << 24) | (slots[COLLISION_BUFFER_SIZE*collTypeIdx+i] << 12);
        for (uint j = i + 1 + localTid % ct_groupsize; j < N; j += ct_groupsize) {
          uint index = atomic_inc(collisionsNum);
          index = min(index, (uint)(LDS_COLL_SIZE-1));
          collisionsData[index] = collision | slots[COLLISION_BUFFER_SIZE*collTypeIdx+j];
        }
      }
    }

    barrier(CLK_LOCAL_MEM_FENCE);
    uint totalCollisions = *collisionsNum;
    totalCollisions = min(totalCollisions, (uint)LDS_COLL_SIZE);
    for (uint index = get_local_id(0); index < totalCollisions; index += get_local_size(0))
      {
  uint collision = collisionsData[index];
  uint collisionThreadId = gid + (collision >> 24);
  uint i = (collision >> 12) & 0xFFF;
  uint j = collision & 0xFFF;
  __global uchar *ptr = ht_src + collisionThreadId * NR_SLOTS * SLOT_LEN +
      xi_offset;
  a = (__global ulong *)(ptr + i * SLOT_LEN);
  b = (__global ulong *)(ptr + j * SLOT_LEN);
  dropped_stor += xor_and_store(round, ht_dst, collisionThreadId, i, j,
    a, b, rowCountersDst);
      }
  }
      
#ifdef ENABLE_DEBUG
    debug[tid * 2] = dropped_coll;
    debug[tid * 2 + 1] = dropped_stor;
#endif
}

/*
** This defines kernel_round1, kernel_round2, ..., kernel_round7.
*/
#define KERNEL_ROUND(N) \
__kernel __attribute__((reqd_work_group_size(THRD, 1, 1))) \
void kernel_round ## N(__global char *ht_src, __global char *ht_dst, \
	__global uint *rowCountersSrc, __global uint *rowCountersDst, \
       	__global uint *debug) \
{ \
    __local uint    collisionsData[LDS_COLL_SIZE]; \
    __local uint    collisionsNum; \
    equihash_round(N, ht_src, ht_dst, debug, collisionsData, \
	    &collisionsNum, rowCountersSrc, rowCountersDst); \
}
KERNEL_ROUND(1)
KERNEL_ROUND(2)
KERNEL_ROUND(3)
KERNEL_ROUND(4)
KERNEL_ROUND(5)
KERNEL_ROUND(6)
KERNEL_ROUND(7)

// kernel_round8 takes an extra argument, "sols"
__kernel __attribute__((reqd_work_group_size(THRD, 1, 1)))
void kernel_round8(__global char *ht_src, __global char *ht_dst,
	__global uint *rowCountersSrc, __global uint *rowCountersDst,
	__global uint *debug, __global sols_t *sols)
{
    uint		tid = get_global_id(0);
    __local uint	collisionsData[LDS_COLL_SIZE];
    __local uint	collisionsNum;
    equihash_round(8, ht_src, ht_dst, debug, collisionsData,
	    &collisionsNum, rowCountersSrc, rowCountersDst);
    if (!tid)
	sols->nr = sols->likely_invalids = 0;
}

uint expand_ref(__global char *ht, uint xi_offset, uint row, uint slot)
{
//    return *(__global uint *)(ht + row * NR_SLOTS * SLOT_LEN +
//	    slot * SLOT_LEN + xi_offset - 4);
	return load4((__global ulong *)(ht + row * NR_SLOTS * SLOT_LEN +
          slot * SLOT_LEN + xi_offset - 4) , 0);
}

/*
** Expand references to inputs. Return 1 if so far the solution appears valid,
** or 0 otherwise (an invalid solution would be a solution with duplicate
** inputs, which can be detected at the last step: round == 0).
*/
uint expand_refs(uint *ins, uint nr_inputs, __global char **htabs,
	uint round)
{
    __global char	*ht = htabs[round % 2];
    uint		i = nr_inputs - 1;
    uint		j = nr_inputs * 2 - 1;
    uint		xi_offset = xi_offset_for_round(round);
    int			dup_to_watch = -1;
    do
      {
	ins[j] = expand_ref(ht, xi_offset,
		DECODE_ROW(ins[i]), DECODE_SLOT1(ins[i]));
	ins[j - 1] = expand_ref(ht, xi_offset,
		DECODE_ROW(ins[i]), DECODE_SLOT0(ins[i]));
	if (!round)
	  {
	    if (dup_to_watch == -1)
		dup_to_watch = ins[j];
	    else if (ins[j] == dup_to_watch || ins[j - 1] == dup_to_watch)
		return 0;
	  }
	if (!i)
	    break ;
	i--;
	j -= 2;
      }
    while (1);
    return 1;
}

/*
** Verify if a potential solution is in fact valid.
*/
void potential_sol(__global char **htabs, __global sols_t *sols,
	uint ref0, uint ref1)
{
    uint	nr_values;
    uint	values_tmp[(1 << PARAM_K)];
    uint	sol_i;
    uint	i;
    nr_values = 0;
    values_tmp[nr_values++] = ref0;
    values_tmp[nr_values++] = ref1;
    uint round = PARAM_K - 1;
    do
      {
	round--;
	if (!expand_refs(values_tmp, nr_values, htabs, round))
	    return ;
	nr_values *= 2;
      }
    while (round > 0);
    // solution appears valid, copy it to sols
    sol_i = atomic_inc(&sols->nr);
    if (sol_i >= MAX_SOLS)
	return ;
    for (i = 0; i < (1 << PARAM_K); i++)
	sols->values[sol_i][i] = values_tmp[i];
    sols->valid[sol_i] = 1;
}

/*
** Scan the hash tables to find Equihash solutions.
*/
__kernel __attribute__((reqd_work_group_size(THRD, 1, 1)))
void kernel_sols(__global char *ht0, __global char *ht1, __global sols_t *sols,
	__global uint *rowCountersSrc, __global uint *rowCountersDst)
{
    __local uint counters[THRD/THREADS_PER_ROW];
    __local uint refs[NR_SLOTS*(THRD/THREADS_PER_ROW)];
    __local uint data[NR_SLOTS*(THRD/THREADS_PER_ROW)];
    __local uint collisionsNum;
    __local ulong collisions[THRD*4];

    uint globalTid = get_global_id(0) / THREADS_PER_ROW;
    uint localTid = get_local_id(0) / THREADS_PER_ROW;
    uint localGroupId = get_local_id(0) % THREADS_PER_ROW;
    __local uint *refsPtr = &refs[NR_SLOTS*localTid];
    __local uint *dataPtr = &data[NR_SLOTS*localTid];    
    
    __global char	*htabs[2] = { ht0, ht1 };
    __global char	*hcounters[2] = { rowCountersSrc, rowCountersDst };
    uint		ht_i = (PARAM_K - 1) % 2; // table filled at last round
    uint		cnt;
    uint		xi_offset = xi_offset_for_round(PARAM_K - 1);
    uint		i, j;
    __global char	*p;
    uint		ref_i, ref_j;
    // it's ok for the collisions array to be so small, as if it fills up
    // the potential solutions are likely invalid (many duplicate inputs)
//     ulong		collisions;
#if NR_ROWS_LOG >= 16 && NR_ROWS_LOG <= 20
    // in the final hash table, we are looking for a match on both the bits
    // part of the previous PREFIX colliding bits, and the last PREFIX bits.
    uint		mask = 0xffffff;
#else
#error "unsupported NR_ROWS_LOG"
#endif
    
  collisionsNum = 0;    
  
  for (uint chunk = 0; chunk < THREADS_PER_ROW; chunk++) {
    uint tid = globalTid + NR_ROWS/THREADS_PER_ROW*chunk;
    p = htabs[ht_i] + tid * NR_SLOTS * SLOT_LEN;
    uint rowIdx = tid/ROWS_PER_UINT;
    uint rowOffset = BITS_PER_ROW*(tid%ROWS_PER_UINT);
    cnt = (rowCountersSrc[rowIdx] >> rowOffset) & ROW_MASK;
    cnt = min(cnt, (uint)NR_SLOTS); // handle possible overflow in last round
    p += xi_offset;
    p += SLOT_LEN*localGroupId;

    for (i = get_local_id(0); i < THRD/THREADS_PER_ROW; i += get_local_size(0))
      counters[i] = 0;
    for (i = localGroupId; i < cnt; i += THREADS_PER_ROW, p += SLOT_LEN*THREADS_PER_ROW) {
      //refsPtr[i] = *(__global uint *)(p - 4);
      //dataPtr[i] = (*(__global uint *)p) & mask;
	refsPtr[i] = load4((__global ulong *)(p),-4);
	dataPtr[i] = load4((__global ulong *)(p), 0) & mask;
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    
    for (i = 0; i < cnt; i++)
      {
	uint a_data = dataPtr[i];
	ref_i = refsPtr[i];
	for (j = i + 1 + localGroupId; j < cnt; j += THREADS_PER_ROW)
	  {
	    if (a_data == dataPtr[j])
	      {
          if (atomic_inc(&counters[localTid]) == 0)
		        collisions[atomic_inc(&collisionsNum)] = ((ulong)ref_i << 32) | refsPtr[j];
          goto part2;          
	      }
	  }
      }

part2:
    continue;
  }
  
    barrier(CLK_LOCAL_MEM_FENCE);
    uint totalCollisions = collisionsNum;
    if (get_local_id(0) < totalCollisions) {
      ulong coll = collisions[get_local_id(0)];
      potential_sol(htabs, sols, coll >> 32, coll & 0xffffffff);
    }
}
