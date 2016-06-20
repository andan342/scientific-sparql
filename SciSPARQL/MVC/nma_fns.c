/*****************************************************************************
 * AMOS2
 *
 * Author: (c) 2012 Andrej Andrejev, UDBL
 * $RCSfile: nma_fns.c,v $
 * $Revision: 1.19 $ $Date: 2015/05/26 11:03:55 $
 * $State: Exp $ $Locker:  $
 *
 * Description: Basic functions operating on NMAs
 ****************************************************************************
 * $Log: nma_fns.c,v $
 * Revision 1.19  2015/05/26 11:03:55  andan342
 * ARRAY() constructor is now sensitive to _sq_python_ranges_ switch
 *
 * Revision 1.18  2015/05/25 15:00:52  andan342
 * Opening mapper connection only on demand
 *
 * Revision 1.17  2015/05/22 21:47:59  andan342
 * Added nma_create(), nma_map(), nma_condense() 2nd-order functions, a() array comprehension
 *
 * Revision 1.16  2015/04/16 15:03:44  andan342
 * Changed __int64 to LONGINT for cross-platform compilability
 *
 * Revision 1.15  2015/03/31 19:46:28  andan342
 * Eliminated the remaining memory leaks
 *
 * Revision 1.14  2015/03/19 20:01:46  andan342
 * - all NMA logical and storage indices are now 64-bit, cast to 32-bit when it comes to memory operations
 * - intra-chunk addresses and chunk ids remain 32-bit
 *
 * Revision 1.13  2015/03/16 12:02:36  andan342
 * - treating resolved proxy as ready based on incomplete ops counter
 * - fixed proxy reader bug
 * - array aggregate functions are now type-safe
 *
 * Revision 1.12  2015/01/28 17:36:07  andan342
 * - SUM(), AVG(), MIN(), MAX() now do not return if incompatible values are found in bag
 *
 * Revision 1.11  2014/04/24 18:47:56  torer
 * using int32 interfaces to remove compiler warnings
 *
 * Revision 1.10  2014/04/23 15:50:17  andan342
 * Fixed bug in nmau-cr-transitive-closure
 *
 * Revision 1.9  2014/04/09 11:17:32  andan342
 * Added transitive closure on row-compressed boolean matrix: nmau-rc-transitive-closure
 *
 * Revision 1.8  2014/03/24 15:29:14  andan342
 * Added Boolean matrix operations: reflexive closure, transitive closure, disjunctive product
 *
 * Revision 1.7  2014/01/24 11:34:19  andan342
 * Using BINARY-related functions from binary.h
 *
 * Revision 1.6  2013/11/23 22:56:02  andan342
 * - moved fragment mapper facility into nma.c
 * - added ALisp functions nma-copy, nma-vsum, nmau-scale, nma-scale, nmau-roundto, nma-roundto, nma-round
 *
 * Revision 1.5  2013/02/21 23:33:02  andan342
 * Renamed NMA-PROXY-RESOLVE to APR, using it to define all non-aggregate SciSparql foreign functions as proxy-tolerant
 *
 * Revision 1.4  2013/02/08 00:46:44  andan342
 * Added C implementation of NMA chunk cache and NMA-PROXY-RESOLVE
 *
 * Revision 1.3  2013/02/05 14:12:31  andan342
 * Renamed nma-init to nma-fill (more general behavior), nma-allocate now takes extra argument,
 * implemented RDF:SUM and RDF:AVG aggregate functions in C
 *
 * Revision 1.2  2013/02/04 12:22:14  andan342
 * Implemented vector sum for NMAs
 *
 * Revision 1.1  2012/03/27 10:26:56  andan342
 * Added C implementations of array aggregates
 *
 *
  ****************************************************************************/

#include "nma.h"
#include "partial_tuple.h"

//////////////////// COUNT 

void nma_count_bf(a_callcontext cxt, a_tuple tpl)
{ // Amos: count of the array elements, 1 if atomic number 
	oidtype x = a_getobjectelem(tpl, 0, FALSE);
	int dt = a_datatype(x);
  LONGINT res;

	if (dt == NMATYPE) res = nmacell_elemcnt(dr(x, nmacell));
	else if (dt == INTEGERTYPE || dt == REALTYPE || dt == COMPLEXTYPE) res = 1;
	else return;

	a_setobjectelem(tpl, 1, mkinteger(res), FALSE);
	a_emit(cxt, tpl, FALSE);
}

/////////////////// SUM & AVG 

int nmacell_isum(struct nmacell* dx)
{	
	struct numarraycell *ds;
	int res;

	ds = dr(dx->s, numarraycell);
	res = 0;
	do {
		res += ds->cont[nmacell_iter2si(dx)];
	} while (nmacell_iter_next(dx) > 0);
	return res;
}

double nmacell_dsum(struct nmacell* dx)
{	
	struct numarraycell *ds;
	double res;
	double *dcont;
	
	ds = dr(dx->s, numarraycell);
	dcont = (double*)ds->cont;
	res = 0;
	do {
		res += dcont[nmacell_iter2si(dx)];
	} while (nmacell_iter_next(dx) > 0);
	return res;
}

oidtype nma_sumfn(bindtype env, oidtype x)
{ // ALisp (unsafe): sum of the array elements
	struct nmacell *dx = dr(x, nmacell);

	nmacell_iter_reset(dx);
	switch (dr(dx->s, numarraycell)->kind) {
		case 0: return mkinteger(nmacell_isum(dx)); 
		case 1: return mkreal(nmacell_dsum(dx));
	}
	return nil;
}

oidtype nma_avgfn(bindtype env, oidtype x)
{ // ALisp (unsafe): average of the array elements
	struct nmacell *dx = dr(x, nmacell);
	LONGINT cnt = nmacell_elemcnt(dx); // guaranteed >= 1

	nmacell_iter_reset(dx);
	switch (dr(dx->s, numarraycell)->kind) {
		case 0: return mkreal(nmacell_isum(dx) * 1.0 / cnt); 
		case 1: return mkreal(nmacell_dsum(dx) / cnt);
	}
	return nil;
}

////////////////// MIN 

int nmacell_imin(struct nmacell* dx)
{	
	struct numarraycell *ds;
	int res;

	ds = dr(dx->s, numarraycell);
	res = ds->cont[nmacell_iter2si(dx)];
	while (nmacell_iter_next(dx) > 0)  
		if (ds->cont[nmacell_iter2si(dx)] < res)
			res = ds->cont[nmacell_iter2si(dx)];
	return res;
}

double nmacell_dmin(struct nmacell* dx)
{	
	struct numarraycell *ds;
	double res;
	double *dcont;

	ds = dr(dx->s, numarraycell);
	dcont = (double*)ds->cont;
	res = dcont[nmacell_iter2si(dx)];
	while (nmacell_iter_next(dx) > 0) 
		if (dcont[nmacell_iter2si(dx)] < res)
			res = dcont[nmacell_iter2si(dx)];
	return res;
}

oidtype nma_minfn(bindtype env, oidtype x)
{ // ALisp (unsafe): min of the array elements
	struct nmacell *dx = dr(x, nmacell);

	nmacell_iter_reset(dx);
	switch (dr(dx->s, numarraycell)->kind) {
		case 0: return mkinteger(nmacell_imin(dx)); 
		case 1: return mkreal(nmacell_dmin(dx));
	}
	return nil;
}

/////////////// MAX

int nmacell_imax(struct nmacell* dx)
{	
	struct numarraycell *ds;
	int res;

	ds = dr(dx->s, numarraycell);
	res = ds->cont[nmacell_iter2si(dx)];
	while (nmacell_iter_next(dx) > 0)  
		if (ds->cont[nmacell_iter2si(dx)] > res)
			res = ds->cont[nmacell_iter2si(dx)];
	return res;
}

double nmacell_dmax(struct nmacell* dx)
{	
	struct numarraycell *ds;
	double res;
	double *dcont;

	ds = dr(dx->s, numarraycell);
	dcont = (double*)ds->cont;
	res = dcont[nmacell_iter2si(dx)];
	while (nmacell_iter_next(dx) > 0) 
		if (dcont[nmacell_iter2si(dx)] > res)
			res = dcont[nmacell_iter2si(dx)];
	return res;
}

oidtype nma_maxfn(bindtype env, oidtype x)
{ // ALisp (unsafe): max of the array elements
	struct nmacell *dx = dr(x, nmacell);

	nmacell_iter_reset(dx);
	switch (dr(dx->s, numarraycell)->kind) {
		case 0: return mkinteger(nmacell_imax(dx)); 
		case 1: return mkreal(nmacell_dmax(dx));
	}
	return nil;
}

//////////////// ARRAY SUM, AVG, MIN & MAX 

void nma_aggregate_bbf(a_callcontext cxt, a_tuple tpl) // TODO: extensible aggregation
{ // Amos: apply to array tpl[0] aggregate function identified by tpl[1]:
	// 1 = sum, 2 = avg, 3 = min, 4 = max
	oidtype x = nil, res = nil;
	int dt;

	a_let(x, aprfn(cxt->env, a_getobjectelem(tpl, 0, FALSE)));
	dt = a_datatype(x);

	if (dt == INTEGERTYPE || dt == REALTYPE || dt == COMPLEXTYPE) res =  x;
	else if (dt == NMATYPE) 
		switch(a_getintelem(tpl, 1, FALSE)) {
			case 1: res = nma_sumfn(cxt->env, x); break;
			case 2: res = nma_avgfn(cxt->env, x); break;
			case 3: res = nma_minfn(cxt->env, x); break;
			case 4: res = nma_maxfn(cxt->env, x); break;
			default: return;
	}	

	a_free(x);
	if (res != nil) {
		a_setobjectelem(tpl, 2, res, FALSE);
		a_emit(cxt, tpl, FALSE);
	}
}

////////////////////// VECTOR SUM

void nmacellu_IIvsum(struct nmacell* dx, struct nmacell* dy)
{	
	int *dxcont = dr(dx->s, numarraycell)->cont,
		  *dycont = dr(dy->s, numarraycell)->cont;

	do dxcont[nmacell_yiter2xsi(dx, dy)] += dycont[nmacell_iter2si(dy)];
	while (nmacell_iter_next(dy) > 0);
}

void nmacellu_DIvsum(struct nmacell* dx, struct nmacell* dy)
{	
	double *dxcont = (double*)dr(dx->s, numarraycell)->cont;
  int *dycont = dr(dy->s, numarraycell)->cont;

	do dxcont[nmacell_yiter2xsi(dx, dy)] += dycont[nmacell_iter2si(dy)];
	while (nmacell_iter_next(dy) > 0);
}

void nmacellu_DDvsum(struct nmacell* dx, struct nmacell* dy)
{	
	double *dxcont = (double*)dr(dx->s, numarraycell)->cont,
		     *dycont = (double*)dr(dy->s, numarraycell)->cont;

	do dxcont[nmacell_yiter2xsi(dx, dy)] += dycont[nmacell_iter2si(dy)];
	while (nmacell_iter_next(dy) > 0);
}

oidtype nmau_vsumfn(bindtype env, oidtype x, oidtype y)
{ // ALisp (unsafe): update X += Y elementwise, return T on success
	// assumes both X and Y are resident NMAs, 
	// checks if they are of same shape and element type of X includes that of Y
	struct nmacell *dx = dr(x, nmacell),
		             *dy = dr(y, nmacell);

	if (dx->kind < dy->kind || nmacell_compareDims(dx, dy) != 0) return nil; 	
  nmacell_iter_reset(dy);
	switch (dx->kind) {
		case 0: 
			nmacellu_IIvsum(dx, dy);			
			break;
		case 1: 
			if(dy->kind == 0) nmacellu_DIvsum(dx, dy);
			else nmacellu_DDvsum(dx, dy);
			break;		
		default: return nil; // X kind not supported
	}
	return t;
}

oidtype nma_vsumfn(bindtype env, oidtype x, oidtype y)
{ // ALisp (unsafe): return X + Y elementwise
	// assumes both X and Y are resident NMAs, 
	// checks if they are of same shape and element type of X includes that of Y
	oidtype res = nma_copy(x);

	if (nmau_vsumfn(env, res, y)==t) return res;
	else return nil;
}

//////////////////// SCALAR MULTIPLICATION

void nmacellu_scaleII(struct nmacell* dx, int s)
{
	int *dxcont = dr(dx->s, numarraycell)->cont;

	nmacell_iter_reset(dx);
	do dxcont[nmacell_iter2si(dx)] *= s;
	while (nmacell_iter_next(dx));
}

void nmacellu_scaleID(struct nmacell* dx, double s)
{
	int *dxcont = dr(dx->s, numarraycell)->cont;

	nmacell_iter_reset(dx);
	do dxcont[nmacell_iter2si(dx)] = a_roundi(s * dxcont[nmacell_iter2si(dx)]);
	while (nmacell_iter_next(dx));
}

void nmacellu_scaleDD(struct nmacell* dx, double s)
{
	double *dxcont = (double*)dr(dx->s, numarraycell)->cont;

	nmacell_iter_reset(dx);
	do dxcont[nmacell_iter2si(dx)] *= s;
	while (nmacell_iter_next(dx));
}

oidtype nmau_scalefn(bindtype env, oidtype x, oidtype s) 
{ // ALisp: update X *= S, return X on success
	struct nmacell *dx = dr(x, nmacell);

	OfType(x, NMATYPE, env);

	switch (a_datatype(s)) {
		case INTEGERTYPE:
			switch (dx->kind) {
			case 0: 
				nmacellu_scaleII(dx, getinteger32(s));
				return x;
			case 1:
				nmacellu_scaleDD(dx, 1.0 * getinteger32(s));
				return x;
			default:
				return nil; //element type not supported
			}
		case REALTYPE:
			switch (dx->kind) {
			case 0:
				nmacellu_scaleID(dx, getreal(s));
				return x;
			case 1:
				nmacellu_scaleDD(dx, getreal(s));
				return x;
			default:
				return nil; //element type not supported;
			}
			default:
				return nil; //factor is not a number
	}
}

oidtype nma_scalefn(bindtype env, oidtype x, oidtype s) 
{ // ALisp: return X * S
	return nmau_scalefn(env, nma_copy(x), s);
}


////////////////// ROUNDING

oidtype nmau_roundtofn(bindtype env, oidtype x, oidtype digits)
{ // ALisp: update X, round all elements to DIGITS precision
  struct nmacell* dx = dr(x, nmacell);
	int ddigits;
	LONGINT i;
	double scale = 1.0;
	double *dcont;

	OfType(x, NMATYPE, env);
	OfType(digits, INTEGERTYPE, env);

	ddigits = getinteger32(digits);
	if (digits < 0) return nil; // negative digits to round
	if (dx->kind == 0) return x; // integer array is always valid result of any round operation

	for (i=0; i<ddigits; i++) scale *= 10.0;
	nmacell_iter_reset(dx);			

	switch (dx->kind) {
		case 1: 			
			dcont = (double*)dr(dx->s, numarraycell)->cont;
			do {
				i = nmacell_iter2si(dx);
				dcont[i] = a_round(dcont[i] * scale) / scale;
			} while (nmacell_iter_next(dx));
			return x;
		default:
			return nil; //ellement type not supported
	}
}

oidtype nma_roundtofn(bindtype env, oidtype x, oidtype digits)
{ // ALisp: round all elements to DIGITS precision
	return nmau_roundtofn(env, nma_copy(x), digits);
}

oidtype nma_roundfn(bindtype env, oidtype x)
{ // ALisp: return integer array consisting of rounded elements of X	
	oidtype res;
	struct nmacell *dx, *dres;
	double *dcont;
	int *icont;

	OfType(x, NMATYPE, env);
	if (dr(x, nmacell)->kind == 0) return x; // an integer array is valid result

	res = nma_allocate(x, 0);
	dres = dr(res, nmacell);
	icont = (int*)dr(dres->s, numarraycell)->cont;
	dx = dr(x, nmacell);

	nmacell_iter_reset(dres);
	switch (dx->kind) {
		case 1: 			
			dcont = (double*)dr(dx->s, numarraycell)->cont;
			do icont[nmacell_iter2si(dres)] = a_roundi(dcont[nmacell_yiter2xsi(dx, dres)]);
			while (nmacell_iter_next(dres));
			return res;
		default:
			return nil; //ellement type not supported
	}
}


//////////////// BOOLEAN OPERATIONS

struct nmacell* nma_validateBooleanMatrix(bindtype env, oidtype x, int check2d)
{
	struct nmacell *dx;

	if (a_datatype(x) != NMATYPE) lerror(4, x, env);
	dx = dr(x, nmacell);

	if (dx->kind != 0) lerror(NMA_KIND_ERROR, mkinteger(dx->kind), env);
	if (check2d && dx->ndims != 2) lerror(NMA_NODIM_ERROR, mkinteger(dx->ndims), env);

	return dx;
}

int bmGet(struct nmacell *dx, LONGINT i, LONGINT j, int padding)
{   
	if (i < dx->dims[0].psize && j < dx->dims[1].psize)
		return naget_i(dx->s, (int)(i * dx->dims[0].am + j * dx->dims[1].am)); //TODO: use own 64-bit access
	else if (padding == 1) //diagonal padding
		return (i == j);
	else //all zeroes padding
		return 0;
}

void bmSet(struct nmacell *dx, int i, int j, int val)
{	
	naset_i(dx->s, (int)(i * dx->dims[0].am + j * dx->dims[1].am), val);
}

oidtype nmau_reflexiveClosurefn(bindtype env, oidtype x)
{	//ALisp: Update X with its reflexive closure
	struct nmacell *dx = nma_validateBooleanMatrix(env, x, 1);
	int k;

	for (k=0; (k < dx->dims[0].psize && k < dx->dims[1].psize); k++)
		if (!bmGet(dx, k, k, 0)) bmSet(dx, k, k, 1);
	return x;
}

oidtype nmau_CRtransitiveClosurefn(bindtype env, oidtype x, oidtype columnMap)
{   //ALisp: Update X with its transitive closure (Warschall algorithm)
	//given X is a row-compressed array where columnMap provides column-to-row mapping
	struct nmacell *dx = nma_validateBooleanMatrix(env, x, 1),
		           *dcm = nma_validateBooleanMatrix(env, columnMap, 0);
	int i, j, k;
	struct numarraycell *dcms = dr(dcm->s, numarraycell);
	int *cm = (int*)(dcms->cont);
	
	for (k=0; k < dx->dims[1].psize; k++) //use number of columns for indexing
		for (i=0; i < dx->dims[0].psize; i++)
			for (j=0; j < dx->dims[1].psize; j++)
				if (!bmGet(dx, i, j, 0) && bmGet(dx, i, k, 0) && bmGet(dx, cm[k], j, 0))
					bmSet(dx, i, j, 1); 
	return x;
}

oidtype nmau_transitiveClosurefn(bindtype env, oidtype x)
{   //ALisp: Update X with its transitive closure (Warschall algorithm)
	struct nmacell *dx = nma_validateBooleanMatrix(env, x, 1);
	int i, j, k;
	
	for (k=0; (k < dx->dims[0].psize && k < dx->dims[1].psize); k++)
		for (i=0; i < dx->dims[0].psize; i++)
			for (j=0; j < dx->dims[1].psize; j++)
				if (!bmGet(dx, i, j, 0) && bmGet(dx, i, k, 0) && bmGet(dx, k, j, 0))
					bmSet(dx, i, j, 1); 
	return x;
}

void nmacell_booleanProduct(struct nmacell *dx, struct nmacell *dy, struct nmacell *dres, int mode)
{	//set RES to (disjunctive) boolean product of X and Y
	int i, j, k, v;
	for (i=0; i < dx->dims[0].psize; i++)
		for (j=0; j < dy->dims[1].psize; j++) {
			v = 0;
			for (k=0; k < dx->dims[1].psize || k < dy->dims[0].psize; k++) 
//				printf("k=%d, x[%d,%d]=%d, y[%d,%d]=%d\n", k, i, k, bmGet(dx, i, k, mode), k, j, bmGet(dy, k, j, mode)); //DEBUG
				if (bmGet(dx, i, k, mode) && bmGet(dy, k, j, mode)) {
					v = 1;
					break;
				}
			bmSet(dres, i, j, v);			
		}
}


oidtype nma_booleanProductfn(bindtype env, oidtype x, oidtype y, oidtype padding)
{	//ALisp: return (disjunctive) boolean product of X and Y
	//padding modes: 0 = no padding (strict), 1 = diagonal, 2 = all zeroes
	struct nmacell *dx = nma_validateBooleanMatrix(env, x, 1),
		           *dy = nma_validateBooleanMatrix(env, y, 1);
	oidtype res;
	int mode;

	if (padding == nil) mode = 0;
	else IntoInteger32(padding, mode, env);

	if (dx->dims[1].psize != dy->dims[0].psize && mode == 0) lerror(NMA_DIM_ERROR, y, env);

	res = make_nma0(2);
	nma_setdim(res, 0, dr(x, nmacell)->dims[0].psize);
	nma_setdim(res, 1, dr(y, nmacell)->dims[1].psize);
	nma_init(res, 0);	

	nmacell_booleanProduct(dr(x, nmacell), dr(y, nmacell), dr(res, nmacell), mode);
	return res;
}


//////////////// AGGREGATE SUM & AVG 
	
struct NMASumCntMapperData {
	int newKind, cnt;
	double d;
	oidtype o;
};

oidtype nma_sum_cnt_mapper(a_callcontext cxt, int width, oidtype res[], void *xa) 
{ 
	struct NMASumCntMapperData *data = (struct NMASumCntMapperData*)xa;
	int dt = a_datatype(res[0]);

	if (dt == NMATYPE) {
		if (!data->cnt) { //if first value in bag
			data->o = nma_allocate(res[0], data->newKind); //create resulting array
			nmacell_fill_rec(cxt->env, dr(data->o, nmacell), mkinteger(0), 0); //initialize with 0
		}
		if (data->o != nil) {
			nmau_vsumfn(cxt->env, data->o, res[0]); // sum up the elements
			data->cnt++;
		} else a_map_done(cxt, t); //stop: array after scalar type in a bag
	} else if (dt == INTEGERTYPE && data->o == nil) {
		data->d += getinteger32(res[0]);
		data->cnt++;		
	} else if (dt == REALTYPE && data->o == nil) {					
		data->d += getreal(res[0]);
		data->cnt++;
	} else a_map_done(cxt, t); //stop: unsupported type in a bag
	return nil;
}

oidtype nma_sum_cnt(a_callcontext cxt, int newKind)
{ // return (sum . count) of a bag of numbers or resident NMAs (elementwise)
	oidtype res, mapbagres;
	struct NMASumCntMapperData data;

	data.newKind = newKind;
	data.cnt = 0;
	data.d = 0;
	data.o = nil;
	mapbagres = a_mapbag(cxt, a_arg(cxt,1), nma_sum_cnt_mapper, (void*)&data);

	if (mapbagres == t || !data.cnt) return nil; //conflicting values or empty bag
	else if (data.o == nil) return cons(mkreal(data.d), mkinteger(data.cnt)); // numeric sum
	else { // array sum
		res = cons(data.o, mkinteger(data.cnt));
//		a_free(data.o);
		return res;
	}
}

oidtype rdf_sumBF(a_callcontext cxt)
{ // Amos: aggregate sum of a bag of numbers or resident NMAs (elementwise)
	// result type is dictated by first value in bag, REAL for single numbers
	oidtype sum_cnt = nil;	
	a_let(sum_cnt, nma_sum_cnt(cxt, -1));

	if (sum_cnt != nil) {
		a_bind(cxt, 2, hd(sum_cnt));  //TODO: maybe should free sum_cnt
		a_result(cxt);
		a_free(sum_cnt);
	}
	return nil;
}

oidtype rdf_avgBF(a_callcontext cxt)
{ // Amos: aggregate average of a bag of numbers or resident NMAs (elementwise)
	// result type is dictated by first value in bag: either REAL or REAL NMA     
	int cnt;
	oidtype sum, sum_cnt = nil;	
	a_let(sum_cnt, nma_sum_cnt(cxt, 1));

	if (sum_cnt != nil) {
		sum = hd(sum_cnt);
		cnt = getinteger32(tl(sum_cnt)); // >0 
		if (a_datatype(sum) == NMATYPE) {
			nmacellu_scaleDD(dr(sum, nmacell), 1.0 / cnt);
			a_bind(cxt, 2, sum);
		} else a_bind(cxt, 2, mkreal(getreal(sum) / cnt));
		a_result(cxt);
		a_free(sum_cnt);
	}
	return nil;
}

////////////////////////// ARRAY MAPPERS

EXPORT int CLOSURE_MISMATCH_ERROR, INVALID_SHAPE_ERROR, NO_MAPPER_ARGS_ERROR, MAPPER_ARG_SHAPES_MISMATCH;
a_connection mapperConnection;

void nma_create_bbbbf(a_callcontext cxt, a_tuple tpl)
{
	oidtype reskind = a_getelem(tpl, 0, FALSE),
					shape = a_getelem(tpl, 1, FALSE),
					fn = a_getelem(tpl, 2, FALSE),
					pt = a_getelem(tpl, 3, FALSE),
					python_ranges = eval_forms(cxt->env, "_sq_python_ranges_"),
					res, arg;
	struct nmacell *dshape;
	int ndims, k;
		  
	dcl_scan(s);
	dcl_tuple(row);
	if (mapperConnection->status == 0) a_connect(mapperConnection, "", FALSE);
	

	//Check the types
	if (a_datatype(reskind) != INTEGERTYPE) lerror(4, reskind, cxt->env);
	if (a_datatype(shape) != NMATYPE) lerror(4, shape, cxt->env);
	if (a_datatype(fn) != SURROGATETYPE) lerror(4, fn, cxt->env); //type of Amos Function objects
	if (a_datatype(pt) != PARTIALTUPLETYPE) lerror(4, pt, cxt->env);

	//Make sure the clousure takes 1 argument
	if (1 != (dr(pt, ptcell)->fillCnt)) 
		lerror(CLOSURE_MISMATCH_ERROR, fn, cxt->env);
	
	//Make sure shape is 1-d integer NMA
	dshape = dr(shape, nmacell);
	if (dshape->ndims != 1 || dshape->kind != 0)
		lerror(INVALID_SHAPE_ERROR, shape, cxt->env);

	//Create result NMA
	ndims =(int)dshape->dims[0].psize;
	res = make_nma0(ndims);
	nma_iter_reset(shape);
	for (k = 0; k < ndims; k++) {
		nma_setdim(res, k, *(int*)nma_iter2pointer(shape));
		nma_iter_next(shape);
	}
	nma_init(res, (int)getinteger(reskind));

	//Create argument NMA - always used in the closure
	arg = nma_allocate(shape, 0);
	partial_tuple_fill(pt, 0, arg);

	//Fill result with data
	nma_iter_reset(res);
	do {
		//Fill the argument with results iterator indexes
		nma_iter_reset(arg);
		for (k =0; k< ndims; k++) {
			*(int*)nma_iter2pointer(arg) = (int)nma_iter_getidx(res, k) + ((python_ranges == nil)? 1 : 0);
			nma_iter_next(arg);
		}
		//Call the mapper & store the result
		a_callfunction(mapperConnection, s, fn, dr(pt, ptcell)->tuple, FALSE);
		a_getrow(s, row, FALSE);
		nma_iter_setcur(cxt->env, res, a_getelem(row, 0, FALSE));		
	} while (nma_iter_next(res));

	//Tidy up and return
	free_tuple(row);
	free_scan(s);

	a_setelem(tpl, 4, res, FALSE);
	a_emit(cxt, tpl, FALSE);
}


void nma_map_bbbbf(a_callcontext cxt, a_tuple tpl)
{
	int arity, j;
	oidtype reskind = a_getelem(tpl, 0, FALSE),
					xs = a_getelem(tpl, 1, FALSE),
		      fn = a_getelem(tpl, 2, FALSE),
					pt = a_getelem(tpl, 3, FALSE),
					x, res, *xsr;	
	dcl_scan(s);
	dcl_tuple(row);
	if (mapperConnection->status == 0) a_connect(mapperConnection, "", FALSE);
	
	//Check the types
	if (a_datatype(reskind) != INTEGERTYPE) lerror(4, reskind, cxt->env);
	if (a_datatype(xs) != ARRAYTYPE) lerror(4, xs, cxt->env);
	if (a_datatype(fn) != SURROGATETYPE) lerror(4, fn, cxt->env); //type of Amos Function objects
	if (a_datatype(pt) != PARTIALTUPLETYPE) lerror(4, pt, cxt->env);
	
	//Make sure the argument vector is non-empty and its size matches the colusre
	arity = a_arraysize(xs);
	if (!arity)
		lerror(NO_MAPPER_ARGS_ERROR, fn, cxt->env);
	if (arity != (dr(pt, ptcell)->fillCnt)) 
		lerror(CLOSURE_MISMATCH_ERROR, fn, cxt->env);

	//Check the argument arrays are aligned, and resolve them
	xsr = malloc(arity * sizeof(oidtype));
	for (j = 0; j < arity; j++) {
		x = a_elt(xs, j);
		if (a_datatype(x) != NMATYPE) lerror(4, x, cxt->env);
		else if (j > 0 && nma_compareDims(x, xsr[0])) lerror(MAPPER_ARG_SHAPES_MISMATCH, fn, cxt->env);

		xsr[j] = aprfn(cxt->env, x);
		nma_iter_reset(xsr[j]);
	}

	//Allocate the result NMA
	res = nma_allocate(xsr[0], (int)getinteger(reskind)),

	//Fill result with data, calling the mapper
	nma_iter_reset(res);
	do {
		//Fill the closure with argument array elements
		for (j = 0; j < arity; j++) {
			partial_tuple_fill(pt, j, nma_iter_cur(cxt->env, xsr[j]));
			nma_iter_next(xsr[j]);
		}
		//Call the mapper & store the result
		a_callfunction(mapperConnection, s, fn, dr(pt, ptcell)->tuple, FALSE);
		a_getrow(s, row, FALSE);
		nma_iter_setcur(cxt->env, res, a_getelem(row, 0, FALSE));		
	} while (nma_iter_next(res));

	//Tidy up and return
	free(xsr);
	free_tuple(row);
	free_scan(s);

	a_setelem(tpl, 4, res, FALSE);
	a_emit(cxt, tpl, FALSE);
}

EXPORT int rdf_ebv(oidtype x)
{
	if (a_datatype(x) == INTEGERTYPE) return (int)getinteger(x);
	if (a_datatype(x) == REALTYPE) return (getreal(x) == 0.0)? 0 : 1;
	if (symbolp(x)) return strcmp(getpname(x), "FALSE");
	if (a_datatype(x) == STRINGTYPE) return strcmp(getstring(x), "");
	return 1;
}

void nma_condense_bbbbf(a_callcontext cxt, a_tuple tpl)
{ //agg: 0=count, 1=sum, 2=avg, 3=min, 4=max, 5=product
	//TODO: support COMPLEX type in some cases!
	oidtype agg = a_getelem(tpl, 0, FALSE),
					x = aprfn(cxt->env, a_getelem(tpl, 1, FALSE)),
		      fn = a_getelem(tpl, 2, FALSE),
					pt = a_getelem(tpl, 3, FALSE),
					res;
	int dagg, xkind, accepted, cnt = 0;
	double e, a;	
	dcl_scan(s);
	dcl_tuple(row);
	if (mapperConnection->status == 0) a_connect(mapperConnection, "", FALSE);

	//Check the types
	if (a_datatype(agg) != INTEGERTYPE) lerror(4, agg, cxt->env);
	if (a_datatype(x) != NMATYPE) lerror(4, x, cxt->env);
	if (a_datatype(fn) != SURROGATETYPE) lerror(4, fn, cxt->env); //type of Amos Function objects
	if (a_datatype(pt) != PARTIALTUPLETYPE) lerror(4, pt, cxt->env);	

	//Make sure the clousure takes 1 argument
	if (1 != (dr(pt, ptcell)->fillCnt)) 
		lerror(CLOSURE_MISMATCH_ERROR, fn, cxt->env);

	//Compute the result
	dagg = (int)getinteger(agg);
	xkind = dr(x, nmacell)->kind;
	a = (dagg == 5)? 1 : 0;
	nma_iter_reset(x);
	do {
		//Fill the closure with argument array element
		partial_tuple_fill(pt, 0, nma_iter_cur(cxt->env, x));

		//Evaluate the filter
		a_callfunction(mapperConnection, s, fn, dr(pt, ptcell)->tuple, FALSE);
		accepted = !a_eos(s); //filter closure is allowed not to return
		if (accepted) {
			a_getrow(s, row, FALSE);
			accepted = rdf_ebv(a_getelem(row, 0, FALSE));
		}

		//Update result and count
		if (accepted) {			
			if (dagg > 0) {
				e = (xkind == 0)? *(int*)nma_iter2pointer(x) : *(double*)nma_iter2pointer(x);
				switch (dagg) {
					case 1: 
					case 2: a += e; 
						break;
					case 3: if (!cnt || e < a) a = e; 
						break;
					case 4: if (!cnt || e > a) a = e;
						break;
					case 5: a *= e;
				}
			}
			cnt++;
		} //of filter = true
	} while (nma_iter_next(x));

	//Prepare the final result
	switch (dagg) {
		case 0: res = mkinteger(cnt); //count - always integer
			break;
		case 2: res = mkreal(a / cnt); //avg - always real
			break;
		default:
			res = (xkind == 0)? mkinteger(a_roundi(a)) : mkreal(a);
	}

	//Tidy up and return
	free_tuple(row);
	free_scan(s);

	a_setelem(tpl, 4, res, FALSE);
	a_emit(cxt, tpl, FALSE);
}


//////////////////////////

void register_nma_fns(void)
{
	extfunction1("nma-sum", nma_sumfn);
	extfunction1("nma-avg", nma_sumfn);
	extfunction1("nma-min", nma_minfn);
	extfunction1("nma-max", nma_maxfn);
	a_extfunction("nma-count-+", nma_count_bf);
	a_extfunction("nma-aggregate--+", nma_aggregate_bbf);

	extfunction2("nmau-vsum", nmau_vsumfn);
	extfunction2("nma-vsum", nma_vsumfn);
	extfunction2("nmau-scale", nmau_scalefn);
	extfunction2("nma-scale", nma_scalefn);
	extfunction2("nmau-roundto", nmau_roundtofn);
	extfunction2("nma-roundto", nma_roundtofn);
	extfunction1("nma-round", nma_roundfn);

	extfunction1("nmau-reflexive-closure", nmau_reflexiveClosurefn);
	extfunction1("nmau-transitive-closure", nmau_transitiveClosurefn);
	extfunction2("nmau-cr-transitive-closure", nmau_CRtransitiveClosurefn);
	extfunction3("nma-boolean-product", nma_booleanProductfn);

	a_extimpl("rdf-sum-+", rdf_sumBF);
	a_extimpl("rdf-avg-+", rdf_avgBF);


	NO_MAPPER_ARGS_ERROR = a_register_error("No arguments to map over");
	INVALID_SHAPE_ERROR = a_register_error("Shape should be 1-dimensional integer array"); 
	CLOSURE_MISMATCH_ERROR = a_register_error("Number of arguments does not match the closure");
	MAPPER_ARG_SHAPES_MISMATCH = a_register_error("Mapping argument shapes mismatch");

	a_extfunction("nma_create----+", nma_create_bbbbf);
	a_extfunction("nma_map----+", nma_map_bbbbf);	
	a_extfunction("nma_condense----+", nma_condense_bbbbf);

	mapperConnection = a_init_connection();
}
