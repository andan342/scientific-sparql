/****************************************************************************
 * AMOS2
 *
 * Author: (c) 2007 Erik Zeitler, Tore Risch, Andrej Andrejev UDBL
 * $RCSfile: numarray.h,v $
 * $Revision: 1.5 $ $Date: 2015/08/17 15:21:17 $
 * $State: Exp $ $Locker:  $
 *
 * Description: Management of binary repr numerical data
 ***************************************************************************/

/* Double word aligned (aligned_objectcell) array of bytes */

#ifndef _numarray_h_
#define _numarray_h_

struct numarraycell 
{
  objtags tags;
  short int kind;                        // Kind of numarray. Default 0
  int bytes;                             // Total size in bytes 
  int numelems;                          // Num of elements
  int cont[1];                           // Contents padded here
};

#define IARRAY 0
#define DARRAY 1
#define CARRAY 2
#define FARRAY 3

EXTERN int NUMOOB_ERROR;

#define numarray_size(db) db->bytes - sizeof(*db) + sizeof(db->cont)

EXTERN oidtype na_csv_double_readfn(bindtype env, oidtype str, oidtype delim);
EXTERN oidtype na_csv_float_readfn(bindtype env, oidtype str, oidtype delim);
EXTERN oidtype na_new_iarray(int dim); // allocate new integer numarray
EXTERN oidtype na_new_darray(int dim); // allocate new souble numaray
EXTERN oidtype make_numarray(int numelems, int elemsize, int kind);
EXTERN oidtype make_iarrayfn(bindtype env, oidtype numelems);
EXTERN oidtype make_darrayfn(bindtype env, oidtype numelems);
EXTERN oidtype make_farrayfn(bindtype env, oidtype numelems);
EXTERN oidtype make_carrayfn(bindtype env, oidtype numelems);
EXTERN size_t numarray_elemsize(int kind);
EXTERN oidtype na_eltfn(bindtype env, oidtype array, oidtype index);
EXTERN oidtype na_setafn(bindtype env, oidtype array, oidtype index, 
                         oidtype val);
EXTERN oidtype copy_numarrayfn(bindtype env, oidtype array, oidtype newsize);

// ADDED BY Andrej
EXTERN int naget_i(oidtype array, int i);
EXTERN void naset_i(oidtype array, int i, int dval);
EXTERN oidtype nasetafn(bindtype env, oidtype array, oidtype index, 
                        oidtype val);
EXTERN size_t numarray_hash(oidtype key);

#endif
