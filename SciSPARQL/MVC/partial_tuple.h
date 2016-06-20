/*****************************************************************************
 * AMOS2
 *
 * Author: (c) 2011 Andrej Andrejev, UDBL
 * $RCSfile: partial_tuple.h,v $
 * $Revision: 1.2 $ $Date: 2015/05/22 21:48:00 $
 * $State: Exp $ $Locker:  $
 *
 * Description: Descriptor of a partially-completed tuple
 ****************************************************************************
 * $Log: partial_tuple.h,v $
 * Revision 1.2  2015/05/22 21:48:00  andan342
 * Added nma_create(), nma_map(), nma_condense() 2nd-order functions, a() array comprehension
 *
 * Revision 1.1  2015/05/22 10:15:03  andan342
 * Partial Tuple implemenation - used as closure implemenation for array mappers
 *
  *****************************************************************************/

#include "amos.h"
#include "storage.h"

EXPORT int PARTIALTUPLETYPE;

struct filldata {
  int pos, pendingOps;
};

struct ptcell
{
  objtags tags;
  a_tuple tuple; 
  int size, fillCnt, remCnt;
  struct filldata fill[1]; 
};

void register_partial_tuple(void);

void partial_tuple_fill(oidtype pt, int fillPos, oidtype value);