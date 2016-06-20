 /*****************************************************************************
 * AMOS2
 *
 * Author: (c) 2011 Andrej Andrejev, UDBL
 * $RCSfile: nma.h,v $
 * $Revision: 1.30 $ $Date: 2016/06/02 16:46:16 $
 * $State: Exp $ $Locker:  $
 *
 * Description: Multidimensonal arrays based on NumArray
 ****************************************************************************
 * $Log: nma.h,v $
 * Revision 1.30  2016/06/02 16:46:16  andan342
 * Logging projection operations on derived NMAs and NMA Proxies, more external interface methods
 *
 * Revision 1.29  2015/11/17 13:31:44  andan342
 * Headers made C++ compatible
 *
 * Revision 1.28  2015/05/30 13:45:24  andan342
 * Supporting tiled array retrieval
 *
 * Revision 1.27  2015/05/22 21:48:00  andan342
 * Added nma_create(), nma_map(), nma_condense() 2nd-order functions, a() array comprehension
 *
 * Revision 1.26  2015/04/16 15:03:43  andan342
 * Changed __int64 to LONGINT for cross-platform compilability
 *
 * Revision 1.25  2015/03/19 20:01:45  andan342
 * - all NMA logical and storage indices are now 64-bit, cast to 32-bit when it comes to memory operations
 * - intra-chunk addresses and chunk ids remain 32-bit
 *
 * Revision 1.24  2015/03/19 12:01:17  andan342
 * - false-positives limit implemented
 * - proxy counter added
 *
 * Revision 1.23  2015/03/17 08:17:07  andan342
 * Storing "pendingOps" property on resident arrays, simplified "readiness" criterion
 *
 * Revision 1.22  2015/02/23 12:34:12  andan342
 * Completely re-written APR and Aggregate APR implementation to merge-based one,
 * fixed bug with reading singleton proxies
 *
 * Revision 1.21  2015/02/17 16:28:44  andan342
 * - marshalling 'access modifier' field in array proxies
 * - added NMA_fileDumpChunks(), NMA_makeRandom_chunks()
 * - exporting more functions
 * - fixed offset bug when projecting
 * - detecting "invalid chunkid" internal error
 *
 * Revision 1.20  2014/03/24 15:29:13  andan342
 * Added Boolean matrix operations: reflexive closure, transitive closure, disjunctive product
 *
 * Revision 1.19  2014/02/27 02:10:32  andan342
 * changed NMA proxy reader/printer, now storage order, offset, strides and origins are transmitted
 *
 * Revision 1.18  2014/02/26 14:12:24  andan342
 * Exporting debug and fragment iterator functionality
 *
 * Revision 1.17  2014/01/26 19:03:09  andan342
 * Exporting nma_s(), nma_allocate() in ssdm.h
 *
 * Revision 1.16  2013/11/23 22:56:01  andan342
 * - moved fragment mapper facility into nma.c
 * - added ALisp functions nma-copy, nma-vsum, nmau-scale, nma-scale, nmau-roundto, nma-roundto, nma-round
 *
 * Revision 1.15  2013/07/28 06:23:10  andan342
 * Moved array chunk file generation from dumper.lsp to nma-filedump function in C
 *
 * Revision 1.14  2013/07/12 12:41:38  andan342
 * Added hexadecimal string option to store binary data on SQL backend
 *
 * Revision 1.13  2013/02/21 23:33:02  andan342
 * Renamed NMA-PROXY-RESOLVE to APR, using it to define all non-aggregate SciSparql foreign functions as proxy-tolerant
 *
 * Revision 1.12  2013/02/12 23:50:50  andan342
 * Added nma-proxy-has-cache and nma-cache-put functions
 *
 * Revision 1.11  2013/02/08 00:46:44  andan342
 * Added C implementation of NMA chunk cache and NMA-PROXY-RESOLVE
 *
 * Revision 1.10  2013/02/05 21:58:06  andan342
 * Projecting NMA descriptors and proxies with C functions nma-project---+ and nma-project--++
 *
 * Revision 1.9  2013/02/05 14:12:31  andan342
 * Renamed nma-init to nma-fill (more general behavior), nma-allocate now takes extra argument,
 * implemented RDF:SUM and RDF:AVG aggregate functions in C
 *
 * Revision 1.8  2013/02/04 12:22:14  andan342
 * Implemented vector sum for NMAs
 *
 * Revision 1.7  2013/02/01 12:00:09  andan342
 * Using same NMA descriptor objects as proxies, major code makeup
 *
 * Revision 1.6  2012/12/13 20:51:26  andan342
 * Implemented C callin interface for in-memory NMA objects
 *
 * Revision 1.5  2012/11/26 01:07:03  andan342
 * Added more safety checks, generalized nma-elemsize to nma-kind2elemsize
 *
 * Revision 1.4  2012/11/20 15:11:38  andan342
 * Avoid complete printing of arrays bigger than NMA_PRINT_LIMIT = 128 elements
 *
 * Revision 1.3  2012/10/29 22:29:27  andan342
 * Storing NMAs in ArrayChunks table using new BLOB<->BINARY functionality of JDBC interface
 *
 * Revision 1.2  2012/03/27 10:26:56  andan342
 * Added C implementations of array aggregates
 *
 * 
 *****************************************************************************/

#include "amos.h"
#include "storage.h"
#include "numarray.h"
#include "fftcomplex.h"
#include "binary.h"

#include "nma_chunks.h"

EXPORT int NMATYPE, NMA_KIND_ERROR, NMA_NODIM_ERROR, NMA_DIM_ERROR, NMA_STEP_ERROR, NMA_READER_ERROR, NMA_PROXY_ERROR, NMA_PROXYTAG_ERROR, 
           NMA_TILES_ERROR, NMA_TILES_DISALLOWED_ERROR;

/////////////////////////// BASIC FUNCTIONALITY

//a per-dimension component of 'nmacell' below
struct dimdata {
  int no; //physical nesting order
  LONGINT size, //size of array in this dimension
          am, //access multiplier (derived)
			    lo, //lower bound of projection
			    step, //step of projection
					psize, //size of projection (derived)
					iter, //iterator position
          origin; //subscript origin (defaults to 0)
};

//an Amos storage type for NMA (descriptors and proxies)
struct nmacell 
{
  objtags tags;
  oidtype s; //numarray as storage, any lisp data if proxy
  int ndims, //number of dimensions      
			ondims, //original ndims - will be copied to all the derived NMAs
			kind, //0=int, 1=double, 2=complex
			isOriginal, //1 if referes to all elements of (external) storage object,
			proxyTag, //0 if not a proxy, proxy type tag otherwise
	    pendingOps; //used during APR  
	LONGINT offset; //overall storage offset
  struct NMAProjection* pProj; //list of logged projections
	struct NMATile* tiles; //pointer to an array of NMATile instances
	struct NMACacheRec* pCache; //pointer to a particular chunk cache - copied on derive
  struct dimdata dims[1]; //dimdata for each dimension
};

//recorded projection information - needed only when resolving proxies 
//based on original logical subscripts
struct NMAProjection
{
  struct NMAProjection* pNext;
  int odim; //dimension number, according to original NMA's dimension count
  LONGINT oidx; //index + origin
};

////////////////////////// PROXIES

//a per-dimension information for a tiled NMA proxy
struct NMATile 
{
	LONGINT odim, oam;
	int size, n, intra_am, inter_am;	
};

//a chunk cache, referred from an original NMA proxy and its derivations
struct NMACacheRec 
{
	struct NMACacheRec* pNext;
	clock_t rut; //recently-used timestamp
	int cnt; //size of chunks array
	long bytes; //total size of all cached BINARY objects	
	oidtype s; //lisp value identifying the array
	oidtype chunks[1];
};

#define NMA_PROXYTAG_TABLE_SIZE 4

//a registry information for a particular proxy tag
struct NMAProxyTagTableRec {
	oidtype resolveFn; //TODO: remove last two
	int defaultChunkSize;
	//AAPR functionality
	oidtype bufferToQueryFn, queryFn;
	int bufferLimit, fpLimit;
	a_connection amosConnection;
};

/// end of PROXIES

static int NMA_PRINT_LIMIT = 128; //elements

#ifdef __cplusplus
extern"C" {
#endif

/* NMA constructors (safe) */
EXPORT oidtype make_nma0(int ndims);
EXPORT void nma_setdim(oidtype x, int i, LONGINT size);
EXPORT void nma_reverseStorageOrder(oidtype x);
EXPORT void nma_setkind(oidtype x, int kind);
EXPORT void nma_init(oidtype x, int kind);
EXPORT oidtype nma_allocate(oidtype x, int newKind); 
EXPORT oidtype nma_copy(oidtype x);
//void dealloc_nma(oidtype x);

/* NMA accessors (unsafe) */
LONGINT nmacell_elemcnt(struct nmacell *dx);
LONGINT nmacell_original_elemcnt(struct nmacell *dx);
void nmacell_fill_rec(bindtype env, struct nmacell *dx, oidtype list, int k);
int nmacell_compareDims(struct nmacell *dx, struct nmacell *dy);

/* NMA accessors (safe) */
EXPORT int nma_kind2elemsize(int kind);
EXPORT void nma_inspect(oidtype x);
EXPORT int nma_compareDims(oidtype x, oidtype y);

/* NMA iterator interface (unsafe) */
void nmacell_iter_setidx(struct nmacell* dx, int k, LONGINT idx);
void nmacell_iter_reset(struct nmacell* dx);
int nmacell_iter_next(struct nmacell* dx);
LONGINT nmacell_iter2si(struct nmacell* dx);
LONGINT nmacell_yiter2xsi(struct nmacell* dx, struct nmacell* dy);

/* NMA iterator interface (safe) */
EXPORT void nma_iter_reset(oidtype x);
EXPORT int nma_iter_next(oidtype x);
EXPORT LONGINT nma_iter_getidx(oidtype x, int k);
EXPORT void nma_iter_setidx(oidtype x, int k, int idx);
EXPORT void* nma_iter2pointer(oidtype x);
EXPORT oidtype nma_iter_cur(bindtype env, oidtype x);
EXPORT oidtype nma_iter_setcur(bindtype env, oidtype x, oidtype val);

/* NMA fragment mapper interface */
typedef int (*NMAFragmentMapper)(oidtype x, LONGINT si, LONGINT fsize, void *xa);

int nmacell_get_ibd_fsize(struct nmacell *dx, int min_ibd, LONGINT *fsize);
EXPORT void nma_mapfragments(oidtype x, int min_ibd, NMAFragmentMapper mapperFn, void *xa);

/* NMA APR interface */
oidtype aprfn(bindtype env, oidtype x);
void register_nma(void);

#ifdef __cplusplus
}
#endif