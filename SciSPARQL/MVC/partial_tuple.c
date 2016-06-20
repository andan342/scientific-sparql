/*****************************************************************************
 * AMOS2
 *
 * Author: (c) 2015 Andrej Andrejev, UDBL
 * $RCSfile: partial_tuple.c,v $
 * $Revision: 1.4 $ $Date: 2015/05/25 13:33:09 $
 * $State: Exp $ $Locker:  $
 *
 * Description: Descriptor of a partially-completed tuple
 ****************************************************************************
 * $Log: partial_tuple.c,v $
 * Revision 1.4  2015/05/25 13:33:09  andan342
 * Fixed bugs with PartialTuple
 *
 * Revision 1.3  2015/05/22 21:47:59  andan342
 * Added nma_create(), nma_map(), nma_condense() 2nd-order functions, a() array comprehension
 *
 * Revision 1.2  2015/05/22 10:34:06  andan342
 * Fixed reader bugs
 *
 * Revision 1.1  2015/05/22 10:15:02  andan342
 * Partial Tuple implemenation - used as closure implemenation for array mappers
 *
 ***************************************************************************/

#include "partial_tuple.h"

EXPORT int PARTIALTUPLETYPE;

oidtype alloc_partial_tuple(int valueCnt, int fillCnt)
{ // Allocate new PARTIALTUPLE
	oidtype res = new_object(sizeof(struct ptcell) + sizeof(struct filldata) * (fillCnt - 1), PARTIALTUPLETYPE);
	a_tuple tpl = a_init_tuple();
	struct ptcell *dres;

	a_newtuple(tpl, valueCnt + fillCnt, FALSE);
	dres = dr(res, ptcell);
	dres->tuple = tpl;	
	dres->size = valueCnt + fillCnt;
	dres->fillCnt = fillCnt;
	dres->remCnt = fillCnt;
	
	return res;
}


void make_partial_tuple_bbf(a_callcontext cxt, a_tuple tpl)
{ // Amos: create partial tuple with values from tpl[0] vector and fill slots at tpl[1] positions	
	oidtype values = a_getelem(tpl, 0, FALSE),
		      fills = a_getelem(tpl, 1, FALSE),
					res = alloc_partial_tuple(a_arraysize(values), a_arraysize(fills));
	struct ptcell *dres = dr(res, ptcell);
	int i = 0, j = 0, k = 0;
	
	while (k < dres->size) {
		if (j < dres->fillCnt && getinteger(a_elt(fills, j)) == k) {
			dres->fill[j].pos = k;
			dres->fill[j].pendingOps = 0;
			a_setelem(dres->tuple, k, nil, FALSE);
			j++;
		} else {
			a_setelem(dres->tuple, k, a_elt(values, i), FALSE);
			i++;
		}
		k++;
	}
	a_setelem(tpl, 2, res, FALSE);
	a_emit(cxt, tpl, FALSE);
}

void partial_tuple_fill(oidtype pt, int fillPos, oidtype value) 
{
	struct ptcell *dpt = dr(pt, ptcell);
	a_setelem(dpt->tuple, dpt->fill[fillPos].pos, value, FALSE);	
}

void dealloc_partial_tuple(oidtype x) 
{
	free_tuple(dr(x, ptcell)->tuple);
  dealloc_object(x); 
}

void print_partial_tuple(oidtype x, oidtype stream, int princflg) 
{ // Standard printer
	struct ptcell *dx = dr(x, ptcell);
	int k;
//	char buff[20];
  oidtype elt;

	a_puts("#[PT",stream);
/*	a_puts(Int32ToString(dx->remCnt, buff), stream); 
	a_puts(" ", stream);
	a_puts(Int32ToString(dx->size, buff), stream); */
	for (k = 0; k < dx->size; k++) {
		a_puts(" ", stream);
		elt = a_getelem(dx->tuple, k, FALSE);
		if (elt == nil) a_puts("*", stream);
		else a_prin1(elt, stream, FALSE);
	}
	a_puts("]", stream);
}

int equal_partial_tuplefn(oidtype x, oidtype y) 
{ // Standard comparison function
  struct ptcell *dx = dr(x, ptcell),
		            *dy = dr(y, ptcell);
	int k;

	if (dx->size != dy->size) return FALSE; 
	for (k = 0; k < dx->size; k++) 
		if (!equal(a_getelem(dx->tuple, k, FALSE), a_getelem(dy->tuple, k, FALSE))) return FALSE;

	return TRUE;	
}

unsigned int partial_tuple_hash(oidtype x)    
{ // Standard hash function
  struct ptcell *dx = dr(x, ptcell);
	int k;
	oidtype elt;

	if (dx->remCnt < dx->size) 
		for (k = 0; k < dx->size; k++) 
	{
		elt = a_getelem(dx->tuple, k, FALSE);
		if (elt != nil) return compute_hash_key(elt);
	}
	return 1;
}

oidtype read_partial_tuple(bindtype env, oidtype tag, oidtype x, oidtype stream) 
{ // Standard reader
	int size = 0, fills = 0, j = 0, k = 0;
	oidtype res, x0;
	struct ptcell *dres;
	
	//Pass 1: compute sizes
	x0 = x;
	while (x0 != nil) {
		if (hd(x) == nil) fills++;
		size++;
		x0 = tl(x0);
	}

	// Allocate PT
	res = alloc_partial_tuple(size - fills, fills);
	dres = dr(res, ptcell);

	//Pass 2: fill PT with values and blanks
	x0 = x;
	while (x0 != nil) {
		if (symbolp(hd(x0)) && strcmp(getpname(hd(x0)), "*") == 0) {			
			dres->fill[j].pos = k;
			dres->fill[j].pendingOps = 0;
			a_setelem(dres->tuple, k, nil, FALSE);
			j++;
		} else a_setelem(dres->tuple, k, hd(x0), FALSE);
		k++;
		x0 = tl(x0);
	}

	return res;
}


void register_partial_tuple(void)
{
	PARTIALTUPLETYPE = a_definetype("PartialTuple", dealloc_partial_tuple, print_partial_tuple);
	typefns[PARTIALTUPLETYPE].equalfn = equal_partial_tuplefn;
	typefns[PARTIALTUPLETYPE].hashfn = partial_tuple_hash;
	type_reader_function("PT", read_partial_tuple);

	a_extfunction("make_partial_tuple--+", make_partial_tuple_bbf);	
}


