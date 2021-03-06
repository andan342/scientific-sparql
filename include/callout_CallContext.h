/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
#include "callout.h"
/* Header for class callout_CallContext */

#ifndef _Included_callout_CallContext
#define _Included_callout_CallContext
#ifdef __cplusplus
extern "C" {
#endif

  /*
   * Class:     callout_CallContext
   * Method:    emit
   * Signature: (Lcallin/Tuple;)V
   */
  JNIEXPORT void JNICALL Java_callout_CallContext_emit
  (JNIEnv *, jobject, jobject);

  /*
   * Class:     callout_CallContext
   * Method:    getBG
   * Signature: ()Lcallin/Oid;
   */
  JNIEXPORT jobject JNICALL Java_callout_CallContext_getBG
  (JNIEnv *env, jobject obj);

  /*
   * Class:     callout_CallContext
   * Method:    enterBG
   * Signature: (Lcallin/Oid;)V
   */

  JNIEXPORT void JNICALL Java_callout_CallContext_enterBG
  (JNIEnv *env, jobject obj, jobject co);

  /*
   * Class:     callout_CallContext
   * Method:    leaveBG
   * Signature: (Lcallin/Oid;)V
   */
  JNIEXPORT void JNICALL Java_callout_CallContext_leaveBG
  (JNIEnv *env, jobject obj, jobject co);


#ifdef __cplusplus
}
#endif
#endif

