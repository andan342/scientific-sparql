/*****************************************************************************
 * AMOS2 
 *
 * Author: (c) 2012 Andrej Andrejev, UDBL
 * $RCSfile: nma_chunks.c,v $
 * $Revision: 1.33 $ $Date: 2016/01/28 23:21:39 $
 * $State: Exp $ $Locker:  $
 *
 * Description: Support for converting the NMAs to and from BINARY chunks
 * ===========================================================================
 * $Log: nma_chunks.c,v $
 * Revision 1.33  2016/01/28 23:21:39  andan342
 * Added NMA_emit_chunkIds() function, sharing most of the code with AAPR()
 *
 * Revision 1.32  2015/06/01 12:37:12  andan342
 * Tiling-related bugs fixed
 *
 * Revision 1.31  2015/05/31 00:35:32  andan342
 * *** empty log message ***
 *
 * Revision 1.30  2015/05/30 13:45:24  andan342
 * Supporting tiled array retrieval
 *
 * Revision 1.29  2015/05/26 13:06:34  andan342
 * Using hash table for a buffer in APR() - changed buffer2query signature
 *
 * Revision 1.28  2015/04/25 13:37:26  torer
 * NMA bug fix
 *
 * Revision 1.27  2015/04/24 11:04:45  andan342
 * Added AAPR2() - a simlified dynamic-signature Aggregated Array Proxy Resolver
 *
 * Revision 1.26  2015/04/16 16:35:43  andan342
 * '(int)value = ...' is not supported by Linux C compiler - refactored
 *
 * Revision 1.25  2015/04/16 15:03:44  andan342
 * Changed __int64 to LONGINT for cross-platform compilability
 *
 * Revision 1.24  2015/04/12 13:29:54  andan342
 * Fixed bug for resolving a singleton proxy with ALisp APR()
 *
 * Revision 1.23  2015/04/01 17:15:32  andan342
 * Simplified interaction with buffer2sql translator
 *
 * Revision 1.22  2015/03/31 19:46:28  andan342
 * Eliminated the remaining memory leaks
 *
 * Revision 1.21  2015/03/25 22:03:15  andan342
 * Stable version used for perfomance tests:
 * - using Amos scans
 * - eliminated remaining memory leaks
 *
 * Revision 1.20  2015/03/19 20:01:45  andan342
 * - all NMA logical and storage indices are now 64-bit, cast to 32-bit when it comes to memory operations
 * - intra-chunk addresses and chunk ids remain 32-bit
 *
 * Revision 1.19  2015/03/19 18:16:43  andan342
 * - using __int64 when generating chunked arrays
 * - added nma-set-s lispfn to modify array id in a proxy
 *
 * Revision 1.18  2015/03/19 12:01:17  andan342
 * - false-positives limit implemented
 * - proxy counter added
 *
 * Revision 1.17  2015/03/19 07:59:22  andan342
 * Statistics-related bug fixed
 *
 * Revision 1.16  2015/03/17 08:17:07  andan342
 * Storing "pendingOps" property on resident arrays, simplified "readiness" criterion
 *
 * Revision 1.15  2015/03/16 12:02:36  andan342
 * - treating resolved proxy as ready based on incomplete ops counter
 * - fixed proxy reader bug
 * - array aggregate functions are now type-safe
 *
 * Revision 1.14  2015/02/23 12:34:13  andan342
 * Completely re-written APR and Aggregate APR implementation to merge-based one,
 * fixed bug with reading singleton proxies
 *
 * Revision 1.13  2015/02/20 17:05:36  andan342
 * Added chunk loader counter, fixed dumper-related performance bugs
 *
 * Revision 1.12  2015/02/17 16:28:45  andan342
 * - marshalling 'access modifier' field in array proxies
 * - added NMA_fileDumpChunks(), NMA_makeRandom_chunks()
 * - exporting more functions
 * - fixed offset bug when projecting
 * - detecting "invalid chunkid" internal error
 *
 * Revision 1.11  2014/04/24 18:47:56  torer
 * using int32 interfaces to remove compiler warnings
 *
 * Revision 1.10  2014/01/24 11:34:19  andan342
 * Using BINARY-related functions from binary.h
 *
 * Revision 1.9  2013/11/23 22:56:02  andan342
 * - moved fragment mapper facility into nma.c
 * - added ALisp functions nma-copy, nma-vsum, nmau-scale, nma-scale, nmau-roundto, nma-roundto, nma-round
 *
 * Revision 1.8  2013/07/28 06:23:11  andan342
 * Moved array chunk file generation from dumper.lsp to nma-filedump function in C
 *
 * Revision 1.7  2013/07/19 14:02:25  andan342
 * Re-commit due to repository server failure (BUG FIX with hex chunk caching)
 *
 * Revision 1.6  2013/07/14 11:06:44  andan342
 * Using Integer parameters to nma2chunks function
 *
 * Revision 1.5  2013/07/13 22:35:31  andan342
 * Complete hexadecimal storage of chunks
 *
 * Revision 1.4  2013/07/12 12:41:38  andan342
 * Added hexadecimal string option to store binary data on SQL backend
 *
 * Revision 1.3  2013/02/21 23:33:02  andan342
 * Renamed NMA-PROXY-RESOLVE to APR, using it to define all non-aggregate SciSparql foreign functions as proxy-tolerant
 *
 * Revision 1.2  2013/02/12 23:50:50  andan342
 * Added nma-proxy-has-cache and nma-cache-put functions
 *
 * Revision 1.1  2013/02/08 00:46:44  andan342
 * Added C implementation of NMA chunk cache and NMA-PROXY-RESOLVE
 *
 *
 ****************************************************************************/

#include "nma.h"

struct NMACacheRec* NMACacheRoot = NULL;
long NMACacheTotalSize = 0,
     NMACacheLimit = 0; // nma-cache-setlimit should be used

struct NMAProxyTagTableRec NMAProxyTagTable[NMA_PROXYTAG_TABLE_SIZE];

int NMALastProxyTag = 0,
    NMAProxyLimit = -1; // no limit, use nma-proxy-setlimit to change

int NMA_NOCHUNK_ERROR, NMA_NOCHUNKSIZE_ERROR, NMA_BIGPROXY_ERROR;

int NMAProxyChunkidSampleSize = 16;

/////////////////////////////////// EMITTING CHUNKS

void nma2chunks_bbff(a_callcontext cxt, a_tuple tpl)
{ // Amos: emit the contents of tpl[0] NMA 
  // in BINARY or, if tpl[2], hex CHARSTRING chunks of tpl[1] size
	oidtype x = a_getobjectelem(tpl, 0, FALSE),
		    s;
	struct nmacell *dx = dr(x, nmacell);
	int chunkSize = (int)a_getintelem(tpl, 1, FALSE),
		  isHex = (int)a_getintelem(tpl, 2, FALSE),
	    elemSize, ibd,  i, blobPos, writeSize, blobSize, blobCount;
	LONGINT fragmentSize, arraySize, remFragmentSize, fragmentPos;
	char *pFragment;
	a_blob theBLOB = NULL; //TODO: might be faster to use BINARY object directly

	if (a_datatype(x) != NMATYPE) lerror(4, x, cxt->env); // OfType without 'return'
	else if (dx->proxyTag) lerror(NMA_PROXY_ERROR, x, cxt->env);

	s = dx->s;
	elemSize = numarray_elemsize(dx->kind);

	ibd = nmacell_get_ibd_fsize(dx, -1, &fragmentSize);	// Get innermost unbroken dimension
	fragmentSize *= elemSize; // Refer to fragment size in bytes

	// Array size is needed to precisely allocate the last emitted BLOB
	arraySize = fragmentSize;
	for (i = 0; i <= ibd; i++) arraySize *= dx->dims[i].psize; // Factor up sizes in all broken dimensions

	blobSize = 0;
	blobCount = 0;

	// Initialize fragment iterator 
	nmacell_iter_reset(dx);
	fragmentPos = nmacell_iter2si(dx) * elemSize;
	remFragmentSize = fragmentSize;

	while (remFragmentSize) // Cycle through fragments, refreshing BLOBs on demand
	{				
		writeSize = (blobSize > remFragmentSize)? (int)remFragmentSize : blobSize; // 0 on 1st pass, since blobSize=0

		if (writeSize) // never on 1st pass
		{
			pFragment = (char*)(dr(s, numarraycell)->cont) + fragmentPos; //s might have been relocated after emit
			a_putBLOBbytes(theBLOB, blobPos, writeSize, pFragment, FALSE);
			blobSize -= writeSize;
			remFragmentSize -= writeSize;
		}

		if (!remFragmentSize) // never on 1st pass
		{ // Proceed to the next fragment			
			for (i=ibd; i>=0; i--) // Iterator increment loop, starting with d as innermost iterated dimension
			{
				dx = dr(x,nmacell); //x might have been relocated after emit
				dx->dims[i].iter++;
				if (dx->dims[i].iter == dx->dims[i].psize) 
					dx->dims[i].iter = 0;
				else {
					fragmentPos = nmacell_iter2si(dx) * elemSize;
					remFragmentSize = fragmentSize;
					break; 
				}
			} 
		} else fragmentPos += writeSize; // Normally proceed through the fragment

		if (!blobSize) // Refresh the BLOB
		{			
			if (theBLOB) // always on the last pass
			{ // If there is a "full" blob - emit it
				a_setintelem(tpl, 3, blobCount, FALSE);
				if (isHex) a_setobjectelem(tpl, 4, blob2hex(theBLOB), FALSE); 
				else a_putBLOBelem(tpl, 4, theBLOB, FALSE);
				a_emit(cxt, tpl, FALSE);
				a_freeBLOB(theBLOB, FALSE); //We are emitting BINARY, BLOB was just an interface
				blobCount++;
			}
			if (arraySize) //Assert remFragmentSize > 0, since arraySize decrements in parallel
			{ // always on 1st pass: Make a new BLOB
				blobSize = (chunkSize > arraySize)? (int)arraySize : chunkSize;
				arraySize -= blobSize;
				theBLOB = a_initBLOB();
				a_newBLOB(theBLOB, blobSize, FALSE);
				blobPos = 0;
			}
		} else blobPos += writeSize;
	}
	
	return;
}					


//////////////////////////////// GENERATING ARRAYS IN CHUNKS

void nma_makerandom_in_chunks_bbbbbff(a_callcontext cxt, a_tuple tpl)
{	//Amos (unsafe): generate an array of type tpl[0], linear size tpl[1] elements, in chunks of size tpl[2] bytes
	//fill it with values from vector tpl[3] distributed with probabilities in vector tpl[4]
	oidtype probs = a_getelem(tpl, 4, FALSE),
		    values = a_getelem(tpl, 3, FALSE);
	int eltType = (int)a_getintelem(tpl, 0, FALSE),
		eltSize = nma_kind2elemsize(eltType), //in bytes			
		chunkSize = (int)a_getintelem(tpl, 2, FALSE) / eltSize, //in elements
		kMax = a_arraysize(probs) - 1,
		k, blobSize, pos, ivalue;
	LONGINT remSize = a_getintelem(tpl, 1, FALSE), //in elements		    			
			blobCount = 0;
	a_blob b = NULL; 	
	double rv, prob, dvalue; 

	while (remSize) 
	{
		blobSize = (remSize > chunkSize)? chunkSize : (int)remSize; //in elements
		b = a_initBLOB();
		a_newBLOB(b, blobSize * eltSize, FALSE);
		for (pos = 0; pos < blobSize; pos++) 
		{ //TODO: make this a separate function
			rv = 1.0 * rand() / RAND_MAX;
			for (k = 0; k <= kMax; k++) 
			{
				prob = getreal(a_elt(probs, k));
				if (rv <= prob || k == kMax)
				{
					if (eltType == 0) {
						ivalue = getinteger32(a_elt(values, k));
						a_putBLOBbytes(b, pos * eltSize, eltSize, (char*)&ivalue, FALSE);
					} else {
						dvalue = getreal(a_elt(values, k));
						a_putBLOBbytes(b, pos * eltSize, eltSize, (char*)&dvalue, FALSE);
					}
					break;
				} 
				else rv -= prob;
			} 
		}
		a_setintelem(tpl, 5, blobCount, FALSE);
		a_putBLOBelem(tpl, 6, b, FALSE);
		a_emit(cxt, tpl, FALSE);
		a_freeBLOB(b, FALSE);
		blobCount++;
		remSize -= blobSize;
	}
}


//////////////////////////////// LOADING CHUNKS

int chunksize(oidtype chunk)
{ // Return effective size of the chunk;
	switch a_datatype(chunk) {
		case STRINGTYPE:
			return (dstringlen(dr(chunk, stringcell)) - 1) / 2;
		case BINARYTYPE:
			return binary_size(dr(chunk, binarycell));
		default:
			return -1;
	}
}	

int nmacell_loadchunk(struct nmacell *dx, oidtype chunk, LONGINT xpos, int start, LONGINT size)
{ // ALisp (unsafe): load SIZE bytes at START of CHUNK object into resident array X at position XPOS
	// CHUNK should be either Binary or Charstring type, and contain hexadecimal represenation in the latter case
	struct numarraycell *ds = dr(dx->s, numarraycell);
	char* xc = (char*)ds->cont;
	struct binarycell *dbinary;
	struct stringcell *dstring;
	char* bc;
	int remBinarySize, res;
	LONGINT remXSize, i;

  if (a_datatype(chunk) == STRINGTYPE) { 
		dstring = dr(chunk, stringcell);
		bc = dstring->cont.string;
	} else {
		dbinary = dr(chunk, binarycell);
		bc = (char*)dbinary->cont;
	}

	remBinarySize = chunksize(chunk) - start;		
	res = (size > remBinarySize)? remBinarySize : (int)size; //as min(dsize, remBinarySize, remXSize)
	remXSize = numarray_size(ds) - xpos;
	if (remXSize < res) res = (int)remXSize;

	if (res>0) { //do the thing
		if (a_datatype(chunk) == STRINGTYPE) 
			for (i=0; i<res; i++) // convert from hex to binary
				xc[xpos + i] = fromHex(bc + 2 * (start + i));
		else memcpy(xc+xpos, bc+start, res); // copy binary data
	}	
	return res; //return the amount of bytes loaded
}

oidtype chunk_to_singleton(oidtype chunk, int start, int kind)
{
	int i, elemsize = nma_kind2elemsize(kind);
	char* tgt = (char*)malloc(elemsize);
	char* src;

	if (a_datatype(chunk) == STRINGTYPE) { 
		src = dr(chunk, stringcell)->cont.string;
		for (i = 0; i < elemsize; i++) // convert from hex to binary
			tgt[i] = fromHex(src + 2 * (start + i));		
	} else {
		src = (char*)(dr(chunk, binarycell)->cont);
		memcpy(tgt, src + start, elemsize);
	}
		
	switch (kind) {
		case 0: return mkinteger(*(int*)tgt);
		case 1: return mkreal(*(double*)tgt);
		//TODO: add other kinds
		default: return nil;
	}
}

/* NOT USED:
oidtype nma_loadchunkfn(bindtype env, oidtype x, oidtype binary, oidtype xpos, oidtype start, oidtype size)
{ // ALisp (unsafe): load SIZE bytes at START of CHUNK object into resident array X at position XPOS
	return mkinteger(nmacell_loadchunk(dr(x, nmacell), binary, getinteger(xpos), getinteger(start), getinteger(size)));
}
*/

/////////////////////////////// MANAGINNG CHUNK CACHE 

oidtype nma_cache_setlimitfn(bindtype env, oidtype cachelimit)
{ // ALisp: set NMA cache limit
	OfType(cachelimit, INTEGERTYPE, env);
	NMACacheLimit = getinteger32(cachelimit);
	return cachelimit;
}

oidtype nma_proxy_startcachefn(bindtype env, oidtype x)
{ // ALisp (unsafe): start cache for NMA proxy X, return cache size
	struct NMACacheRec* pCache;
	struct nmacell* dx = dr(x, nmacell);
	int cnt, i, 
		  cs = NMAProxyTagTable[dx->proxyTag-1].defaultChunkSize; //TODOL: when each arrey will have its own chunk size - use that

	// try to find cache with same S in list
	pCache = NMACacheRoot;
	while (pCache && a_compare(pCache->s, dx->s))
		pCache = pCache->pNext;

	if (!pCache) { // create new cache record
		cnt = (int)ceil(1.0 * nmacell_original_elemcnt(dx) * numarray_elemsize(dx->kind) / cs); 
		pCache = (struct NMACacheRec*)malloc(sizeof(struct NMACacheRec) + sizeof(oidtype) * (cnt - 1)); 
		// add to the global list
		pCache->pNext = NMACacheRoot; 
		NMACacheRoot = pCache;
		// initialize as ampty
		pCache->cnt = cnt;
		pCache->rut = clock();
		a_let(pCache->s, dx->s); // use array id from the proxy
		pCache->bytes = 0;
		for (i=0; i < cnt; i++) pCache->chunks[i] = nil;
		a_let(pCache->s, dx->s); // use array id from the proxy		
	} 
	dx->pCache = pCache; // link from X (and all its descendants)
	return mkinteger(pCache->cnt);
}

oidtype nma_proxy_has_cachefn(bindtype env, oidtype x)
{ // ALisp: return T if cache is defined on array proxy X
	if (a_datatype(x) == NMATYPE && dr(x, nmacell)->pCache) return t;
	else return nil;
}

void nma_cache_clear(struct NMACacheRec* pCache) 
{ // clear all BINARY chunks from cache
	int i;
	for (i=0; i < pCache->cnt; i++) 
		if (pCache->chunks[i] != nil) a_free(pCache->chunks[i]);
	pCache->bytes = 0;
}

void nma_cache_clearall()
{
	struct NMACacheRec* pNode = NMACacheRoot;
	while (pNode) {
		if (pNode->bytes) 			
			nma_cache_clear(pNode);		
		pNode = pNode->pNext;
	}
	NMACacheTotalSize = 0;
}

oidtype nma_cache_resetfn(bindtype env) 
{ // ALisp: clear all NMA caches
	nma_cache_clearall();
	return nil;
}

long nma_cache_shrink(long shrinkSize, struct NMACacheRec* pCurrent)
{
	struct NMACacheRec *pFirstPositive = NMACacheRoot,
			               *pNode, *pOldest;

	if (shrinkSize > (NMACacheTotalSize - ((pCurrent)? pCurrent->bytes : 1))) {
		shrinkSize -= NMACacheTotalSize;
		nma_cache_clearall();		
	} else {
		// point pFirstPositive to the first non-empty cache in list
		while (pFirstPositive && !pFirstPositive->bytes) 
			pFirstPositive = pFirstPositive->pNext; 
		
		//TODO: this could have been O(n) if NMACacheRoot list is sorted first.
		while (shrinkSize > 0) {
			pNode = pFirstPositive;
			// select the oldest node among all positive nodes except current one
			pOldest = NULL;
			while (pNode) {
				if (pNode->bytes && (!pOldest || pOldest->rut > pNode->rut) && pNode != pCurrent) pOldest = pNode;
				pNode = pNode->pNext;
			}
			// clear oldest cache
			if (!pOldest) return shrinkSize; //ASSERT: should never happen
			NMACacheTotalSize -= pOldest->bytes;
			shrinkSize -= pOldest->bytes;
			nma_cache_clear(pOldest);		
		}
	}
	return shrinkSize;
}

oidtype nma_cache_get(struct NMACacheRec* pCache, int chunkid)
{ // (unsafe) get BINARY chunk from given cache
	if (chunkid < 0 || chunkid >= pCache->cnt) return t; //signal 'bad chunkid' error
	pCache->rut = clock();
	return pCache->chunks[chunkid];
}
		
int nma_cache_put(struct NMACacheRec* pCache, int chunkid, oidtype chunk)
{ // (unsafe) put BINARY chunk into given cache, shrink if necessary
	long csize = chunksize(chunk),
		   shrinkSize = NMACacheTotalSize + csize - NMACacheLimit;

//	printf("NMA_CACHE_PUT(%d)\n", chunkid); //DEBUG

	if (chunkid < 0 || chunkid >= pCache->cnt) return 0; //wrong chunkid
	
	if (csize > NMACacheLimit) return 0; // cannot fit this chunk anyway
	if (shrinkSize > 0) nma_cache_shrink(shrinkSize, pCache);

	pCache->rut = clock();
	a_let(pCache->chunks[chunkid], chunk); //ASSERT: chunks[chunkid] is nil
	pCache->bytes += csize;
	NMACacheTotalSize += csize;
	return 1; //success
}

oidtype nma_cache_putfn(bindtype env, oidtype x, oidtype chunkid, oidtype chunk)
{ // ALisp (unsafe): put BINARY chunk with given chunkid into cache associated with NMA proxy X
	if (nma_cache_put(dr(x, nmacell)->pCache, getinteger32(chunkid), chunk)) return t;
	else return nil;
}

////////////////////////////  RESOLVING NMA PROXIES - GENERAL

LONGINT elemCnt = 0, proxyCnt = 0;
int retrievedChunkCnt = 0, redundantChunkCnt = 0;

oidtype nma_resetchunkcntfn(bindtype env)
{	//ALisp: return the value of chunkCnt (incretmented when loading new chunks) and reset it to 0;
	oidtype res = a_vector(mkinteger(elemCnt), mkinteger(retrievedChunkCnt), mkinteger(redundantChunkCnt), mkinteger(proxyCnt), NULL);	
	elemCnt = 0;
	retrievedChunkCnt = 0;
	redundantChunkCnt = 0;
	proxyCnt = 0;
	return res;
}

oidtype nma_register_proxytagfn(bindtype env, oidtype resolveFn) 
{ // ALisp: register proxy-handling functions and return new proxytag value
	//  resolveFn - resolve a proxy, NIL if built-in chunk-based resolver is to be used, which needs to be registered with  nma_register_aapr()
	OfType(resolveFn, SYMBOLTYPE, env);

	if (NMALastProxyTag < NMA_PROXYTAG_TABLE_SIZE) {
		NMALastProxyTag++;
		NMAProxyTagTable[NMALastProxyTag-1].resolveFn = resolveFn; // store the symbols, no reference counting
		NMAProxyTagTable[NMALastProxyTag-1].defaultChunkSize = 0; // nma-proxy-set-default-chunksize should be used
		//AAPR functionality
		NMAProxyTagTable[NMALastProxyTag-1].bufferToQueryFn = nil;
		NMAProxyTagTable[NMALastProxyTag-1].queryFn = nil;
		NMAProxyTagTable[NMALastProxyTag-1].bufferLimit = 0;
		NMAProxyTagTable[NMALastProxyTag-1].amosConnection = NULL;
		return mkinteger(NMALastProxyTag); // return generated proxy tag
	} else return nil; // proxy tag table is full
}

oidtype nma_proxy_set_default_chunksizefn(bindtype env, oidtype proxyTag, oidtype defaultChunkSize)
{ // ALisp : set default chunk size for the registered proxy tag
	int dproxyTag;

	OfType(proxyTag, INTEGERTYPE, env);
	OfType(defaultChunkSize, INTEGERTYPE, env);
	
	dproxyTag = getinteger32(proxyTag);
	if (dproxyTag > 0 && dproxyTag <= NMALastProxyTag) 
		NMAProxyTagTable[dproxyTag-1].defaultChunkSize = getinteger32(defaultChunkSize);		
	else lerror(NMA_PROXYTAG_ERROR, proxyTag, env);
	return defaultChunkSize;
}

oidtype nma_proxy_enabledfn(bindtype env)
{ //ALisp: return T if there are any proxy tags registered
	if (NMALastProxyTag) return t;
	else return nil;
}

oidtype nma_proxy_setlimitfn(bindtype env, oidtype limit)
{ // ALisp: set NMA proxy limit - never resolve proxies containing more elements
	OfType(limit, INTEGERTYPE, env);
	NMAProxyLimit = getinteger32(limit);
	return limit;
}


///////////////////////////// NEW AAPR()

oidtype nma_register_aaprfn(bindtype env, oidtype proxyTag, oidtype bufferToQueryFn, oidtype queryFnStr, 
							oidtype bufferLimit, oidtype fpLimit) 
{ //ALisp: register AAPR functionality for the given proxy tag, initializing connection to host Amos process
	int pt, i;
	
	OfType(proxyTag, INTEGERTYPE, env);
	OfType(bufferToQueryFn, SYMBOLTYPE, env);
	OfType(queryFnStr, STRINGTYPE, env);
	OfType(bufferLimit, INTEGERTYPE, env);	
	
	pt = getinteger32(proxyTag) - 1;
	if (pt < 0 || pt >= NMALastProxyTag) return nil; //invalid proxytag
	for (i = 0; i <= NMALastProxyTag; i++)
		if (i != pt && NMAProxyTagTable[i].bufferLimit) return nil; //AAPR functionality already registered on another proxytag!

	NMAProxyTagTable[pt].amosConnection = a_init_connection();
	a_connect(NMAProxyTagTable[pt].amosConnection, "", FALSE);
	//a_connectto(NMAProxyTagTable[pt].amosConnection, "ssdm2", "localhost", FALSE);
	NMAProxyTagTable[pt].bufferToQueryFn = bufferToQueryFn; //symbol
	a_setf(NMAProxyTagTable[pt].queryFn, a_getfunction(NMAProxyTagTable[pt].amosConnection, getstring(queryFnStr), FALSE));
	NMAProxyTagTable[pt].bufferLimit = getinteger32(bufferLimit);
	NMAProxyTagTable[pt].fpLimit = getinteger32(fpLimit);
	return t;
}

int RESOLVE = 0, EMIT_CHUNKIDS = 1; //NMAProxyMapperData.mode values

struct NMAProxyMapperData {
	oidtype bufht, chunk, res;
	int proxyTag, arrayid, bufferSize, chunksize, kind, isOpenScan, chunkid, fpCnt, residx, mode;
	LONGINT respos;
	a_callcontext cxt; //can be used both in Amos functions
	bindtype env; //and lisp functions
	struct NMACacheRec* pCache;
};

void aapr_advance_scan(struct NMAProxyMapperData *data, int chunkid, int useFpLimit)
{ //ASSERT: data->isOpenScan
	dcl_tuple(row);
	int row_chunkid;
	oidtype row_chunk;
	a_scan s = a_init_scan(); 
	
	while (data->chunkid < chunkid) {
		a_execute_custom(NMAProxyTagTable[data->proxyTag].amosConnection, s, "fetch :s;", "(:buffersize 1)", FALSE);
		if (a_eos(s)) break;
		a_getrow(s, row, FALSE);
		retrievedChunkCnt++;
		row_chunkid = getinteger32(a_elt(a_getobjectelem(row, 0, FALSE), 0));
//		printf("AAPR: chunkid retrieved: %d, wanted: %d...", row_chunkid, chunkid); //DEBUG
		
		row_chunk = a_elt(a_getobjectelem(row, 0, FALSE), 1);
//		printf("OK!\n"); //DEBUG
//		if (data->pCache) nma_cache_put(data->pCache, row_chunkid, row_chunk); //Cache this chunk even if it is redundant		
	
		if (row_chunkid == chunkid) {
			//C hunk found						
			//a_setf(data->chunk, row_chunk); 
			data->chunk = row_chunk;
			data->chunkid = chunkid;
		}
		else {
			redundantChunkCnt++;
			data->fpCnt++;
			if (row_chunkid > chunkid || (useFpLimit && data->fpCnt > NMAProxyTagTable[data->proxyTag].fpLimit)) break; //Chunk missed - stop scanning
		}
//		a_nextrow(data->scan, FALSE); // iterate to next result
	} 
	if (data->chunkid != chunkid) data->isOpenScan = 0; //Close scan if wanted chunk not retrieved
	free_tuple(row);
	free_scan(s);
}

int aapr_isReady(oidtype res, oidtype cur_res) 
{
	return a_datatype(res) != NMATYPE || (res != cur_res && dr(res, nmacell)->pendingOps == 0);
}

void aapr_buffer_to_scan(struct NMAProxyMapperData *data, int emitCurrent) 
{ 	
//	dcl_tuple(args);
	int buf_chunkid, buf_chunkpos, buf_bytes, bytesread;
	LONGINT buf_respos;
	oidtype chunkid_list, buftl, cid, items, item, query, res = nil;
	struct nmacell *dres;
	a_scan s = a_init_scan();
//	dcl_tuple(row);

	//1. Obtain sorted list of chunkids in the buffer
	a_let(chunkid_list, call_lisp(mksymbol("htkeys-sort"), data->env, 1, data->bufht));

	//2. Translate buffer to query
	a_let(query, call_lisp(NMAProxyTagTable[data->proxyTag].bufferToQueryFn, data->env, 2, chunkid_list, mkinteger(data->arrayid)));	        
//	printf("AAPR: resetting buffer, SQL arguments: "); a_print(argvector); //DEBUG

	//3. Reset scan
/*	a_newtuple(args, a_arraysize(argvector), FALSE); 	
	for (i = 0; i < a_arraysize(argvector); i++) a_setobjectelem(args, i, a_elt(argvector, i), FALSE);
*/
//	printf("AAPR: Sending SQL...");
//	a_callfunction( NMAProxyTagTable[data->proxyTag].queryFn, args, FALSE);
	a_execute_custom(NMAProxyTagTable[data->proxyTag].amosConnection, s, getstring(query), "(:buffersize 1)", FALSE);
	free_scan(s);
//	printf("OK!\n");

	data->isOpenScan = 1;
	data->fpCnt = 0;
	data->chunkid = -1;
//	printf("AAPR: resetting buffer, SQL arguments: "); a_print(argvector); //DEBUG
//	free_tuple(args);
//	printf("AAPR: about to free the query (refcnt=%d)\n", refcnt(query)); //DEBUG
	a_free(query);	

	//4. Load chunks from new scan and emit bufferized arrays
	buftl = chunkid_list;
	while (buftl != nil) {
		// 4.1. Get buffered chunk from scan
		cid = hd(buftl);
		buf_chunkid = getinteger32(cid);
		aapr_advance_scan(data, buf_chunkid, FALSE); //TODO: should implement re-buffering instead of ignoring FP limit
		if (data->chunkid != buf_chunkid) lerror(NMA_NOCHUNK_ERROR, cid, data->env);

		// 4.2. Process all items for this chunk
		items = get_hashtable(data->bufht, cid);
		while (items != nil) {
			item = hd(items);
//			printf("AAPR: obtained item from buffer... "); //DEBUG
//			a_print(item); //DEBUG
			buf_chunkpos = getinteger32(a_elt(item, 0));
			buf_respos = getinteger(a_elt(item, 1));
			buf_bytes = getinteger32(a_elt(item, 2)); 
//		a_setf(res, a_elt(bufrec, 4));
			res = a_elt(item, 3);
//		if (a_datatype(res) > 44) lerror(NMA_NOCHUNK_ERROR, nil, data->env); //DEBUG
//		printf("AAPR: res obtained, refcnt=%d\n", refcnt(res)); //DEBUG
				
			// Load chunk into number or NMA
			if (res == nil) {
//			a_setf(res, chunk_to_singleton(data->chunk, rec_chunkpos, data->kind));
				res = chunk_to_singleton(data->chunk, buf_chunkpos, data->kind);
				bytesread = nma_kind2elemsize(data->kind);
			} else {			
				bytesread = nmacell_loadchunk(dr(res, nmacell), data->chunk, buf_respos, buf_chunkpos, buf_bytes);
				dres = dr(res, nmacell);
				dres->pendingOps--;
			}		
			if (bytesread != buf_bytes) 
			printf("ERROR: wrong simulation: expected to load %d bytes from chunk %d, loaded %d\n", buf_bytes, buf_chunkid, bytesread);
			// Emit buffered result, if all chunks are loaded
			if (data->cxt && aapr_isReady(res, (emitCurrent)? nil : data->res)) {
				a_bind(data->cxt, data->residx, res);
				a_result(data->cxt);
			}
			else if (!data->cxt && data->res == nil) 
				a_setf(data->res, res); //return single numeric result from APR lispfn

			// Get the next item for this chunkid
			items = tl(items);
		}
		// Get next chunkid from list
		buftl = tl(buftl);
	}
//	printf("AAPR: refcnt(data->res)=%d before buffer cleanup\n", refcnt(data->res)); //DEBUG
	a_free(chunkid_list);
	clear_hashtable(data->bufht);	
//	printf("AAPR: refcnt(data->res)=%d after buffer cleanup\n", refcnt(data->res)); //DEBUG
	data->bufferSize = 0;	
}

void aapr_buffer_or_process(int chunkid, int chunkpos, int bytes, struct NMAProxyMapperData *data)
{
	oidtype cid, item, items;
	int bytesread;

	if (chunkid != data->chunkid && data->pCache) {						
	//1. try to get new chunk from cache
//			printf("AAPR: looking up cache... "); //DEBUG
		data->chunk = nma_cache_get(data->pCache, chunkid);
		if (data->chunk != nil) data->chunkid = chunkid;
	}
		//2. try to get new chunk from scan 
	if (data->chunkid < chunkid && data->isOpenScan) {		
//			printf("AAPR: advancing scan... "); //DEBUG
		aapr_advance_scan(data, chunkid, TRUE);
	}

		//3. Load or bufferize
	if (chunkid == data->chunkid) { 
		// Load (Phase III)
		if (data->res == nil) {
			a_setf(data->res, chunk_to_singleton(data->chunk, chunkpos, data->kind));
//				data->res = chunk_to_singleton(data->chunk, chunkpos, data->kind);
			bytesread = nma_kind2elemsize(data->kind);
		} else  bytesread = nmacell_loadchunk(dr(data->res, nmacell), data->chunk, data->respos, chunkpos, bytes);			
		//ASSERT: bytesread == bytes
	} else {  
		// Bufferize (Phase I)
		if (data->res != nil) 
			dr(data->res, nmacell)->pendingOps++; 		
		//insert {chunkid, chunkpos, respos, bytes, res} vector into buffer
//	  printf("AAPR: constructing buffer item... "); //DEBUG
		item = a_vector(mkinteger(chunkpos), mkinteger(data->respos), mkinteger(bytes), data->res, NULL);
//		a_print(item); //DEBUG
		cid = mkinteger(chunkid);
		items = get_hashtable(data->bufht, cid);
		if (items == 0) items = nil; // Probably a bug in get_hashtable()
		if (items == nil) data->bufferSize++;
		put_hashtable(data->bufht, cid, cons(item, items));

			//reset buffer if reached limit
		if (data->bufferSize >= NMAProxyTagTable[data->proxyTag].bufferLimit)
			aapr_buffer_to_scan(data, FALSE);
//			printf("AAPR: proceeding to next fragment...\n"); //DEBUG
	}
	data->respos += bytes;
}

void emit_chunkid(struct NMAProxyMapperData *data, int chunkid)
{
	a_bind(data->cxt, data->residx, mkinteger(chunkid));
	a_result(data->cxt);
}

int aapr_fragment_mapper(oidtype x, LONGINT si, LONGINT fsize, void *xa)
{ // Chunkids are always increasing while mapping thru same proxy
	struct NMAProxyMapperData *data = (struct NMAProxyMapperData*)xa;
	int elemsize = nma_kind2elemsize(data->kind);
	LONGINT  sib = si * elemsize,		             //in bytes
			     remFragmentSize = fsize * elemsize; //in bytes
	int chunkid = (int)(sib / data->chunksize),
			chunkpos = (int)(sib % data->chunksize), //in bytes
			bytes;	
	
	while (remFragmentSize > 0) {		
		// Simulate reading
		bytes = data->chunksize - chunkpos;
		if (remFragmentSize < bytes) bytes = (int)remFragmentSize;

		if (data->mode == EMIT_CHUNKIDS) emit_chunkid(data, chunkid);
		else // Buffer or process
			aapr_buffer_or_process(chunkid, chunkpos, bytes, data);

		// Proceed to next chunk for this fragment
		remFragmentSize -= bytes;		
		if (remFragmentSize > 0) { 
			chunkid++;
			chunkpos = 0;
		}
	}
	return 0;
}

int aapr_fragment_tile_mapper(oidtype x, LONGINT si, LONGINT fsize, void *xa)
{ // Assume nestring order of original of X is ascending, and last am==1 	
	//  (true as long as we do not store custom nesting orders in a backend)
	struct NMAProxyMapperData *data = (struct NMAProxyMapperData*)xa;
	struct nmacell *dx = dr(x, nmacell);
	int elemsize = nma_kind2elemsize(data->kind),		  
		  tileSi = 0, tilePos = 0,
		  k, tileIdx_k, tileElt_k = 0, tilesize_last, size;
	LONGINT rem_si, oidx_k, remfsize;

	// Determine the tile id and tile position for the fragment's beginning
	tileSi = 0;
	tilePos = 0;
	rem_si = si;
//	printf("TILES: si = %d, fsize=%d =====================\n", (int)si, (int)fsize);
	for (k = 0; k < dx->ondims; k++) { 		
		oidx_k = rem_si / dx->tiles[k].oam; // fragment start's logical index in the original of X
		rem_si -= oidx_k * dx->tiles[k].oam;		
		tileIdx_k = (int)(oidx_k / dx->tiles[k].size); // tile's logical index
		tileSi += tileIdx_k * dx->tiles[k].inter_am; // tile's id (storage index)
		tileElt_k = (int)(oidx_k % dx->tiles[k].size); // logical index inside a tile
		tilePos += tileElt_k * dx->tiles[k].intra_am; // storage index inside a tile
//		printf("TILES: k=%d, oidx_k=%d, rem_si=%d, tileIdx_k = %d, tileSi = %d, tileElt_k = %d, tilePos = %d\n",
//			     k, (int)oidx_k, (int)rem_si, tileIdx_k, tileSi, tileElt_k, tilePos); //DEBUG
	}
	
	tilesize_last = dx->tiles[dx->ondims - 1].size; // tile size in the last dimension 
	remfsize = fsize;
	while (remfsize > 0) {		
		// Determine size to transfer (in elements)		
		size = tilesize_last - tileElt_k;  //using intra-tile element idx in last dimension
		if (size > remfsize) size = (int)remfsize; 

		// Buffer or process
//		printf("TILES: tileSi=%d, tileElt_k=%d, tilePos=%d, remfsize=%d, size=%d\n", tileSi, tileElt_k, tilePos, (int)remfsize, size); ///DEBUG
		if (data->mode == EMIT_CHUNKIDS) emit_chunkid(data, tileSi);
		else // Buffer or process
			aapr_buffer_or_process(tileSi, tilePos * elemsize, size * elemsize, data); 

		//Take next tiles along the last dimension
		if (remfsize == fsize) {
			tilePos -= tileElt_k; //do this only on first iteration, assuming tiles[last].intra_am==1
			tileElt_k = 0;
		}
		remfsize -= size;		
		if (remfsize > 0)
			tileSi++; // Assuming tiles[last].inter_am==1 and fsize <= tiles[last].odim	
	}
	return 0;
}		

int isResolvable(bindtype env, oidtype x)
{ //return 0 if no resolution needed, 1 for completely external resolution with resolveFn, 2 for AAPR
	struct nmacell *dx;

	if (a_datatype(x) == NMATYPE) { // else not an NMA - return unchanged
		dx = dr(x, nmacell);
		if (dx->proxyTag) { // else resident NMA - return unchanged
			if (dx->proxyTag < 0 || dx->proxyTag > NMALastProxyTag) 
				lerror(NMA_PROXYTAG_ERROR, x, env); // invalid proxy tag - can't resolved
			else if (NMAProxyLimit >= 0 && nmacell_elemcnt(dx) > NMAProxyLimit)
				lerror(NMA_BIGPROXY_ERROR, x, env); // proxy exceeds the defined limit
			else if (NMAProxyTagTable[dx->proxyTag-1].resolveFn != nil)				
				return 1; //simple resolve
			else return 2; //aggregated resolve //TODO: check that needed functions are provided!
		} 
	}
	return 0; //not an NMA or resident NMA - do not resolve
}

//int aapr = 0; //DEBUG

int aapr_inner(struct NMAProxyMapperData *data, oidtype x)
{ //Resolve RDF literal, if actually buffering, return 0, otherwise return 1, result is placed in data->res
	struct nmacell *dx;
	int newarrayid, min_ibd;

	switch (isResolvable(data->env, x)) {
		case 0: 
//			if (aapr) printf("AAPR: not a proxy: "); a_print(x); //DEBUG 
			a_setf(data->res, x); 			
//			data->res = x;
			return 1;
		case 1: 
//			if (aapr) printf("AAPR: black box proxy: "); a_print(x); //DEBUG
			a_setf(data->res, call_lisp(NMAProxyTagTable[dr(x, nmacell)->proxyTag-1].resolveFn, data->env, 1, x)); // call lisp function to resolve
//			data->res = call_lisp(NMAProxyTagTable[dr(x, nmacell)->proxyTag-1].resolveFn, data->env, 1, x);
			return 1;
		default: 
//			printf("AAPR: resolving proxy: "); a_print(x); //DEBUG
			//TODO: provide a simplified case for Original proxies							
			newarrayid = getinteger32(dr(x, nmacell)->s); //TODO: handle not only integer arrayids
			if (newarrayid != data->arrayid) { 
//				printf("AAPR: new arrayid=%d, old arrayid=%d\n", newarrayid, data->arrayid); //DEBUG
				//switching to another array
				if (data->bufferSize > 0) aapr_buffer_to_scan(data, TRUE); //never happens when called from APR()						
				data->isOpenScan = 0; //after processing the buffer, scan for another array is useless
				data->chunkid = -1;	//moved from line below '}'
				data->chunk = nil; 
				data->arrayid = newarrayid;
				dx = dr(x, nmacell);			
				data->proxyTag = dx->proxyTag - 1; //Since there can be only one registered proxy tag with AAPR functionality is defined
				data->chunksize = NMAProxyTagTable[data->proxyTag].defaultChunkSize; //TODO: this might be array-dependent in the future
				data->kind = dx->kind;
				data->pCache = dx->pCache; //proxies derived from same parent inherit the same pCache pointer
//				printf("AAPR: switched to arrayid=%d\n", data->arrayid); //DEBUG
			} else dx = dr(x, nmacell);

			if (dx->ndims) { //allocate resident NMA for result
				elemCnt += nmacell_elemcnt(dx);
				a_setf(data->res, nma_allocate(x, -1));  //initialized with pendingOps = 0 //LEAK
//				printf("AAPR: allocated (refcnt=%d) ", refcnt(data->res)); a_print(data->res); //DEBUG
			} else { //resolve singleton proxies to directly to numeric values
				a_setf(data->res, nil);
				elemCnt++;
			}
			data->respos = 0;
			proxyCnt++;
			min_ibd = (dr(x, nmacell)->tiles)? dr(x, nmacell)->ndims - 2 : 0; //TODO: support 'cyllindric' tiles also
			nma_mapfragments(x, min_ibd, (dr(x, nmacell)->tiles)? aapr_fragment_tile_mapper : aapr_fragment_mapper, (void*)data);		
			return (data->res != nil && aapr_isReady(data->res, nil)); 
	} //nil result can either mean "buffer is active - need to reset" or argument x == nil
}


oidtype aapr_mapper(a_callcontext cxt, int width, oidtype res[], void *xa) 
{ 	
	struct NMAProxyMapperData *data = (struct NMAProxyMapperData*)xa;
		
	if (aapr_inner(data, res[0]) && data->mode == RESOLVE) {	
	//	printf("AAPR: immediate emit %d %d(refcnt=%d): ", width, data->residx, refcnt(data->res)); a_print(data->res); //DEBUG
		a_bind(cxt, width+1, data->res);

		a_result(cxt);
	}
	return nil;
}

void aapr_data_init(struct NMAProxyMapperData *data, a_callcontext cxt, bindtype env, int mode)
{
	data->res = nil;
	data->mode = mode;
	if (data->mode == EMIT_CHUNKIDS) data->bufht = nil;
	else a_let(data->bufht, new_hashtable(TRUE));
	data->bufferSize = 0; //only count unique chunkdids
	data->cxt = cxt;
	data->env = env;
	data->arrayid = -1;
	data->isOpenScan = 0;	
}

void aapr_data_finalize(struct NMAProxyMapperData *data)
{
	dcl_scan(s);

	if (data->bufferSize > 0) aapr_buffer_to_scan(data, TRUE); 
	a_free(data->bufht);
	if (data->isOpenScan) {
		a_execute_custom(NMAProxyTagTable[data->proxyTag].amosConnection, s, "close :s;", "(:buffersize 1)", FALSE);
//		printf("AAPR: Closing scan (result refcnt=%d)...",refcnt(data->res)); a_print(data->res); //DEBUG
		free_scan(s);		
//		printf("...Ok!\n"); //DEBUG
	}	
}

oidtype aapr_bf(a_callcontext cxt)
{ 
	struct NMAProxyMapperData data;	
//	printf("AAPR started\n"); //DEBUG
//	aapr = 1; //DEBUG
	aapr_data_init(&data, cxt, cxt->env, RESOLVE);
	data.residx = 2;
	a_mapbag(cxt, a_arg(cxt,1), aapr_mapper, (void*)&data);
	aapr_data_finalize(&data); //might emit, but not return, since data.cxt != NULL
	if (data.res != nil) a_free(data.res);

//	aapr = 0; //DEBUG
//	printf("AAPR completed\n"); //DEBUG
	return nil;
}

oidtype nma_emit_chunkids_bf(a_callcontext cxt)
{ 
	struct NMAProxyMapperData data;	
//	printf("emitChunkids started\n"); //DEBUG
	aapr_data_init(&data, cxt, cxt->env, EMIT_CHUNKIDS);
	data.residx = 2;
	a_mapbag(cxt, a_arg(cxt,1), aapr_mapper, (void*)&data);	
	return nil;
}

/////////////////////////////// AAPR2: simplified dynamic-signature AAPR, resolve only last column

oidtype aapr2_mapper(a_callcontext cxt, int width, oidtype res[], void *xa) 
{ 	
	struct NMAProxyMapperData *data = (struct NMAProxyMapperData*)xa;
	int i;

	for (i = 0; i < (width-1); i++)
		a_bind(cxt, i + 2, res[i]);
	data->residx = width + 1;
		
	if (aapr_inner(data, res[width-1])) {		
		a_bind(cxt, data->residx, data->res);
		a_result(cxt);
	}
	return nil;
}

oidtype aapr2_bf(a_callcontext cxt)
{ 
	struct NMAProxyMapperData data;	

	aapr_data_init(&data, cxt, cxt->env, RESOLVE);
	a_mapbag(cxt, a_arg(cxt, 1), aapr2_mapper, (void*)&data);
	aapr_data_finalize(&data); //might emit, but not return, since data.cxt != NULL
	if (data.res != nil) a_free(data.res);

	return nil;
}

////////////////////////////////// SIMPLE APR()


oidtype aprfn(bindtype env, oidtype x) 
{ // ALisp: if X is NMA proxy, resolve it
	struct NMAProxyMapperData data;	
//	oidtype res;

	aapr_data_init(&data, NULL, env, RESOLVE);
	aapr_inner(&data, x); //might start buffering
//	printf("APR: mapped (refcnt=%d) ", refcnt(data.res)); a_print(data.res); //DEBUG
	aapr_data_finalize(&data); //if was buffering - return actual result here	
//	printf("APR: finalized (refcnt=%d) ", refcnt(data.res)); a_print(data.res); //DEBUG

//	res = data.res;	
//	a_free(data.res);
	decref(doid(data.res));
//	printf("APR: emitting (refcnt=%d) ", refcnt(res)); a_print(res); //DEBUG
	return data.res;
}

void apr_bf(a_callcontext cxt, a_tuple tpl)
{ // Amos: if X is NMA proxy, resolve it, return unchanged otherwise, NIL-tolerant
	oidtype res = aprfn(cxt->env, a_getobjectelem(tpl, 0, FALSE));
	a_setobjectelem(tpl, 1, res, FALSE);
	a_emit(cxt, tpl, FALSE);
}

////////////////////////////////// OTHER CHUNK UTILITIES 

/* NOT USED!
oidtype dumpbinaryfn(bindtype env, oidtype stream, oidtype binary)
{ // ALisp (unsafe): print HEX representation of BINARY into Lisp stream
	struct binarycell *dbinary = dr(binary, binarycell);
	unsigned char* bc = (unsigned char*)dbinary->cont;
	unsigned int i;
	char buf[3];

	for (i=0; i < binary_size(dbinary); i++) {
		sprintf(buf, "%02X", bc[i]);
		a_puts(buf, stream);
	}
	return nil;
}
*/

FILE* dump_fout = NULL;
double dump_fsize = 0;

struct DumpNMAData {
	int arrayid, chunksize;
	double filesize_max;
	bindtype env;
	oidtype getfilenamefn;	
};

oidtype nma_filedump_closefn(bindtype env)
{ //ALisp: close nma filedump
	if (dump_fout) {
		fclose(dump_fout);
		dump_fout = NULL;
		dump_fsize = 0;
		return t;
	}
	return nil;
}

void nma_filedump_prepare(struct DumpNMAData* data, int dumpsize)
{ //common part of nma_hexdump_mapper() and nma_bindump_mapper()
	//ASSERT: dumpsize <= data.filesize_max
	char* filename;	
	// close file if size limit is going to be exceeded
	if (dump_fsize + dumpsize > data->filesize_max)
		nma_filedump_closefn(NULL);
	// open a new file
	if (!dump_fout) {
		filename = getstring(call_lisp(data->getfilenamefn, data->env, 0));
		printf("Writing file %s...\n", filename);
		dump_fout = fopen(filename, "wb");
	}
	// update file size
	dump_fsize += dumpsize;
}

oidtype nma_hexdump_mapper(a_callcontext cxt, int width, oidtype tpl[], void *xa)
{
	struct DumpNMAData* data = xa;
	char c0A = 0x0A;
	oidtype hex = nil;	
	a_setf(hex, binary2hex(tpl[1]));

	nma_filedump_prepare(data, 12 + data->chunksize * 2);
	// write arraid, chunkid, chunk
	fprintf(dump_fout, "%d,%d,%s", data->arrayid, getinteger32(tpl[0]), getstring(hex)); //TODO: avoid creating Amos string
	a_free(hex);

	fwrite(&c0A, 1, 1, dump_fout);
	return nil;
}

oidtype nma_bindump_mapper(a_callcontext cxt, int width, oidtype tpl[], void *xa)
{
	struct DumpNMAData* data = xa;
	struct binarycell *dbinary = dr(tpl[1], binarycell);	
	int act_chunksize = binary_size(dbinary),
		  chunkid = getinteger32(tpl[0]),
			i;
	char c4 = 4, 
		   c0 = 0;

	nma_filedump_prepare(data, 12 + data->chunksize);
	// write arraid, chunkid, chunk
	fwrite(&c4, 1, 1, dump_fout);
	fwrite(&(data->arrayid), 4, 1, dump_fout);
	fwrite(&c4, 1, 1, dump_fout);
	fwrite(&chunkid, 4, 1, dump_fout);
	fwrite(&(data->chunksize), 2, 1, dump_fout);
	fwrite(dbinary->cont, act_chunksize, 1, dump_fout);
	// pad rest of chunk field with 0x00
	for (i = act_chunksize; i < data->chunksize; i++)
		fwrite(&c0, 1, 1, dump_fout);
	return nil;
}

oidtype nma_filedumpfn(bindtype args, bindtype env)
{ // ALisp (unsafe): write NMA (arg 1) identified by ARRAYID (arg 2) in CHUNKSIZE (arg 3) chunks into CSV HEX (if arg 4)
	// or otherwise binary file(s) of size <= FILESIZE_MAX (arg 5, double) with names provided by GETFILENAMEFN() (arg 6)
	dcl_oid(nma2chunksfn);
	dcl_global_cxt(cxt);
	oidtype chunksize = nthargval(args, 3),
		      isHex = nthargval(args, 4),
					n2cargs[3];
	struct DumpNMAData data;

	// prepare to call function nma2chunks() with following args:
	a_setf(nma2chunksfn, a_getfunctionnamed("NMA.INTEGER.INTEGER.NMA2CHUNKS->INTEGER.BINARY", FALSE));
	n2cargs[0] = nthargval(args, 1); //NMA
	n2cargs[1] = chunksize; 
	n2cargs[2] = isHex; 

	// use the following data in a mapper
	data.arrayid = getinteger32(nthargval(args, 2));
	data.chunksize = getinteger32(chunksize);
	data.filesize_max = getreal(nthargval(args, 5));
	data.env = env;
	data.getfilenamefn = nthargval(args, 6); //a function or closure to generate filenames to write to

	a_mapfunctionC(cxt, nma2chunksfn, 3, n2cargs, (getinteger32(isHex)==1)? nma_hexdump_mapper : nma_bindump_mapper, &data);

	free_oid(nma2chunksfn);
	return nil;
}

oidtype nma_filedumpchunks_bbbbbb(a_callcontext cxt)
{	//Amos (unsafe): write each BLOB in chunks bag (arg 1) to files generated by filename_fn (arg 2), with max_size (arg 3)
	//using arrayid (arg 4), under chunk_size (arg 5) and is_hex flag (arg 6)	
	int isHex = getinteger32(a_arg(cxt, 6));
	struct DumpNMAData data;

	data.arrayid = getinteger32(a_arg(cxt, 4));
	data.chunksize = getinteger32(a_arg(cxt, 5));
	data.filesize_max = getreal(a_arg(cxt, 3));
	data.getfilenamefn = mksymbol(getstring(a_arg(cxt, 2)));
	data.env = cxt->env;
	
	a_mapbag(cxt, a_arg(cxt, 1), (isHex == 1)? nma_hexdump_mapper : nma_bindump_mapper, &data); 
	return nil;
}	

void register_nma_chunks(void)
{
	NMA_NOCHUNK_ERROR = a_register_error("Chunk not found while resolving array");
	NMA_NOCHUNKSIZE_ERROR = a_register_error("Cannot determine chunk size while resolving array");
	NMA_BIGPROXY_ERROR = a_register_error("Array to resolve exceeds defined limit");

//	extfunction5("nma-load-chunk", nma_loadbinaryfn); not used
//	extfunction2("dumpbinary", dumpbinaryfn);  not used
	extfunctionn("nma-filedump", nma_filedumpfn);
	extfunction0("nma-filedump-close", nma_filedump_closefn);

	extfunction1("nma-cache-setlimit", nma_cache_setlimitfn);
	extfunction1("nma-proxy-startcache", nma_proxy_startcachefn);
	extfunction1("nma-proxy-has-cache", nma_proxy_has_cachefn);
	extfunction0("nma-cache-reset", nma_cache_resetfn);
	extfunction3("nma-cache-put", nma_cache_putfn);
	
	extfunction0("nma-reset-chunk-cnt", nma_resetchunkcntfn);
	extfunction1("nma-register-proxytag", nma_register_proxytagfn);
	extfunction2("nma-proxy-set-default-chunksize", nma_proxy_set_default_chunksizefn);
	extfunction0("nma-proxy-enabled", nma_proxy_enabledfn);
	extfunction1("nma-proxy-setlimit", nma_proxy_setlimitfn);

	extfunction1("apr", aprfn);

	a_extfunction("NMA2Chunks--++", nma2chunks_bbff);	
	a_extfunction("APR-+", apr_bf);

	a_extimpl("NMA_fileDumpChunks------", nma_filedumpchunks_bbbbbb);
	a_extfunction("NMA_makeRandom_chunks-----++", nma_makerandom_in_chunks_bbbbbff);

	extfunction5("nma-register-aapr", nma_register_aaprfn);
	a_extimpl("AAPR-+", aapr_bf);
	a_extimpl("NMA_emit_chunkIds-+", nma_emit_chunkids_bf);
	a_extimpl("AAPR2-+", aapr2_bf); //simplified dynamic-signature AAPR
}
