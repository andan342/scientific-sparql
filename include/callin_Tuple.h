/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class callin_Tuple */

#ifndef _Included_callin_Tuple
#define _Included_callin_Tuple
#ifdef __cplusplus
extern "C" {
#endif
/*
 * Class:     callin_Tuple
 * Method:    getArity
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_callin_Tuple_getArity
  (JNIEnv *, jobject);

/*
 * Class:     callin_Tuple
 * Method:    setArity
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_setArity
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    getStringElem
 * Signature: (I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_callin_Tuple_getStringElem
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    getLongElem
 * Signature: (I)J
 */
JNIEXPORT jlong JNICALL Java_callin_Tuple_getLongElem
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    getDoubleElem
 * Signature: (I)D
 */
JNIEXPORT jdouble JNICALL Java_callin_Tuple_getDoubleElem
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    getBinaryElem
 * Signature: (I)[B
 */
JNIEXPORT jbyteArray JNICALL Java_callin_Tuple_getBinaryElem
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    getBooleanElem
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL Java_callin_Tuple_getBooleanElem
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    getOidElem
 * Signature: (I)Lcallin/Oid;
 */
JNIEXPORT jobject JNICALL Java_callin_Tuple_getOidElem
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    getSeqElem
 * Signature: (I)Lcallin/Tuple;
 */
JNIEXPORT jobject JNICALL Java_callin_Tuple_getSeqElem
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    getElem
 * Signature: (I)Ljava/lang/Object;
 */
JNIEXPORT jobject JNICALL Java_callin_Tuple_getElem
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    setElem
 * Signature: (ILjava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_setElem__ILjava_lang_String_2
  (JNIEnv *, jobject, jint, jstring);

/*
 * Class:     callin_Tuple
 * Method:    setElem
 * Signature: (I[BI)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_setElem__I_3BI
  (JNIEnv *, jobject, jint, jbyteArray, jint);

/*
 * Class:     callin_Tuple
 * Method:    setBinaryElem
 * Signature: (I[BI)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_setBinaryElem
  (JNIEnv *, jobject, jint, jbyteArray, jint);

/*
 * Class:     callin_Tuple
 * Method:    addElem
 * Signature: (ILjava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_addElem__ILjava_lang_String_2
  (JNIEnv *, jobject, jint, jstring);

/*
 * Class:     callin_Tuple
 * Method:    addElem
 * Signature: (I[BI)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_addElem__I_3BI
  (JNIEnv *, jobject, jint, jbyteArray, jint);

/*
 * Class:     callin_Tuple
 * Method:    setElem
 * Signature: (II)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_setElem__II
  (JNIEnv *, jobject, jint, jint);

/*
 * Class:     callin_Tuple
 * Method:    setElem
 * Signature: (IJ)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_setElem__IJ
  (JNIEnv *, jobject, jint, jlong);

/*
 * Class:     callin_Tuple
 * Method:    setElem
 * Signature: (ID)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_setElem__ID
  (JNIEnv *, jobject, jint, jdouble);

/*
 * Class:     callin_Tuple
 * Method:    setElem
 * Signature: (IZ)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_setElem__IZ
  (JNIEnv *, jobject, jint, jboolean);

/*
 * Class:     callin_Tuple
 * Method:    setElem
 * Signature: (ILcallin/Oid;)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_setElem__ILcallin_Oid_2
  (JNIEnv *, jobject, jint, jobject);

/*
 * Class:     callin_Tuple
 * Method:    setElem
 * Signature: (ILcallin/Tuple;)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_setElem__ILcallin_Tuple_2
  (JNIEnv *, jobject, jint, jobject);

/*
 * Class:     callin_Tuple
 * Method:    setElem
 * Signature: (ILjava/lang/Object;)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_setElem__ILjava_lang_Object_2
  (JNIEnv *, jobject, jint, jobject);

/*
 * Class:     callin_Tuple
 * Method:    isString
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL Java_callin_Tuple_isString
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    isInteger
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL Java_callin_Tuple_isInteger
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    isDouble
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL Java_callin_Tuple_isDouble
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    isBinary
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL Java_callin_Tuple_isBinary
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    isObject
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL Java_callin_Tuple_isObject
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    isTuple
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL Java_callin_Tuple_isTuple
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    init
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_init__
  (JNIEnv *, jobject);

/*
 * Class:     callin_Tuple
 * Method:    init
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_init__I
  (JNIEnv *, jobject, jint);

/*
 * Class:     callin_Tuple
 * Method:    destroy
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_callin_Tuple_destroy
  (JNIEnv *, jobject);

#ifdef __cplusplus
}
#endif
#endif
