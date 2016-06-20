/****************************************************************************
 * AMOS2
 *
 * Author: (c) 2006 Tore Risch, UDBL
 * $RCSfile: binary.h,v $
 * $Revision: 1.15 $ $Date: 2015/02/01 19:34:58 $
 * $State: Exp $ $Locker:  $
 *
 * Description: Management of binary data
 * ==========================================================================
 * $Log: binary.h,v $
 * Revision 1.15  2015/02/01 19:34:58  torer
 * EXPORT -> EXTERN
 *
 * Revision 1.14  2014/12/29 14:37:07  torer
 * 1. Built-in BTREE used by default
 * 2. Mexima temporarily disabled
 *
 * Revision 1.13  2014/12/23 12:55:51  torer
 * Using size_t
 *
 * Revision 1.12  2014/12/19 09:13:57  torer
 * Revert .h files
 *
 * Revision 1.10  2014/01/31 14:25:53  andan342
 * Exporting binary_load() and binary_save()
 *
 * Revision 1.9  2014/01/24 11:32:59  andan342
 * Exporting hex2binary(), binary2hex() functions
 *
 * Revision 1.8  2011/05/04 18:44:18  torer
 * Use declaration size_t for byte array sizes
 *
 * Revision 1.7  2007/02/04 10:13:17  torer
 * Aligned binary_cell template with aligned_objectcell to guarantee machine independece
 *
 * Revision 1.6  2007/02/02 19:50:23  torer
 * Compact representation of kind of type BINARY
 *
 * Revision 1.5  2007/02/02 06:45:48  torer
 * Template for struct binary moved here from storage.h
 *
 * Revision 1.4  2007/02/01 19:03:28  zeitler
 * *** empty log message ***
 *
 * Revision 1.3  2007/01/31 11:44:02  zeitler
 * *** empty log message ***
 *
 * Revision 1.2  2007/01/29 22:41:21  zeitler
 * Added access functions
 *
 * Revision 1.1  2006/06/05 12:39:09  torer
 * Include for type BINARY
 *
 ***************************************************************************/

struct binarycell /* Double word aligned (aligned_objectcell) array of bytes */
{
  objtags tags;                     
  short int binkind;                           /* Kind of binary. Default 0. */
  size_t bytes;                                        /* Total size in bytes*/
  int cont[1];                                       /* Contents padded here */
};
#define binary_size(db) db->bytes - sizeof(*db) + sizeof(db->cont)

EXTERN oidtype new_binary(size_t size, int init);
EXTERN oidtype type_of_binaryfn(bindtype env, oidtype b);
EXTERN oidtype get_float(bindtype env, oidtype b, int ind);
EXTERN oidtype put_float(bindtype env, oidtype b, int ind, oidtype v);
EXTERN oidtype binary_sizefn(bindtype env, oidtype b);
EXTERN void *a_get_image_buffer(int len);
EXTERN oidtype a_read_from_image_buffer(void);
EXTERN void a_print_to_buffer(oidtype obj, void **buffer, 
                              size_t *length);
oidtype print_darrayfn(bindtype env, oidtype x, oidtype stream);


EXTERN char fromHex(char* buf); //AA
EXTERN oidtype blob2hex(a_blob b); //AA
EXTERN oidtype binary2hex(oidtype b); //AA
EXTERN oidtype hex2binary(oidtype hs); //AA
EXTERN oidtype binary_load(char* filename); //AA
EXTERN void binary_save(oidtype b, char* filename); //AA
