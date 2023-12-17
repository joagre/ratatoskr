
VEC_LOCAL intptr_t CAT3(vec_,VEC_SIMD_TYPE,get_element_int)(vec_t* a, int i)
{
    vec_int_t* ptr;
    int j;
    if ((i < 0) || (i >= (int)a->size)) return 0;
    j = (i*a->width) / sizeof(vec_native_t);        // vector number
    ptr = ((vec_int_t*) a->base) + j;
#ifdef vec_scalar
    switch(a->width) {
    case sizeof(int8_t): return (intptr_t)*((elem_int8_t*) ptr);
    case sizeof(int16_t): return (intptr_t)*((elem_int16_t*) ptr);
    case sizeof(int32_t): return (intptr_t)*((elem_int32_t*) ptr);
    case sizeof(int64_t): return (intptr_t)*((elem_int64_t*) ptr);
    default: return 0;
    }
#else
    i -= j*(sizeof(vec_native_t)/a->width);
    switch(a->width) {
    case sizeof(int8_t): return (intptr_t)(*((elem_int8_t*) ptr))[i];
    case sizeof(int16_t): return (intptr_t)(*((elem_int16_t*) ptr))[i];
    case sizeof(int32_t): return (intptr_t)(*((elem_int32_t*) ptr))[i];
    case sizeof(int64_t): return (intptr_t)(*((elem_int64_t*) ptr))[i];
    default: return 0;
    }
#endif
}

VEC_LOCAL double CAT3(vec_,VEC_SIMD_TYPE,get_element_float)(vec_t* a, int i)
{
    vec_int_t* ptr;
    int j;
    if ((i < 0) || (i >= (int)a->size)) return 0;
    j = (i*a->width) / sizeof(vec_native_t);  // vector number
    ptr = ((vec_int_t*) a->base) + j;
#ifdef vec_scalar    
    switch(a->width) {
    // case sizeof(uint16_t): return (intptr_t)(*((__v8hi*) ptr))[i];
    case sizeof(float32_t): return (double)*((elem_float32_t*) ptr);
    case sizeof(float64_t): return (double)*((elem_float64_t*) ptr);
    default: return 0.0;
    }
#else    
    i -= j*(sizeof(vec_native_t)/a->width);
    switch(a->width) {
    // case sizeof(uint16_t): return (intptr_t)(*((__v8hi*) ptr))[i];
    case sizeof(float32_t): return (double)(*((elem_float32_t*) ptr))[i];
    case sizeof(float64_t): return (*((elem_float64_t*) ptr))[i];
    default: return 0.0;
    }
#endif
}

VEC_LOCAL void CAT3(vec_,VEC_SIMD_TYPE,set_element_int)(vec_t* vp,int i,intptr_t value)
{
    vec_int_t* ptr;
    int j;

    if (i < 0) return;
    if (i >= (int)vp->size) {
	if (vec_resize(vp, i+1) < 0)
	    return;
    }
    j = (i*vp->width) / sizeof(vec_native_t);  // vector number
    ptr = ((vec_int_t*) vp->base) + j;
#ifdef vec_scalar
    switch(vp->width) {
    case sizeof(uint8_t): *((elem_int8_t*) ptr) = value; break;
    case sizeof(uint16_t): *((elem_int16_t*) ptr) = value; break;
    case sizeof(uint32_t): *((elem_int32_t*) ptr) = value; break;
    case sizeof(uint64_t): *((elem_int64_t*) ptr) = value; break;
    default: break;
    }
#else
    i -= j*(sizeof(vec_native_t)/vp->width);   // element position in vector
    switch(vp->width) {
    case sizeof(uint8_t): (*((elem_int8_t*) ptr))[i] = value; break;
    case sizeof(uint16_t): (*((elem_int16_t*) ptr))[i] = value; break;
    case sizeof(uint32_t): (*((elem_int32_t*) ptr))[i] = value; break;
    case sizeof(uint64_t): (*((elem_int64_t*) ptr))[i] = value; break;
    default:  break;
    }
#endif
}

VEC_LOCAL void CAT3(vec_,VEC_SIMD_TYPE,set_element_float)(vec_t* vp,int i,double value)
{
    vec_flt_t* ptr;

    if (i < 0) return;
    if (i >= (int)vp->size) {
	if (vec_resize(vp, i+1) < 0)
	    return;
    }
#ifdef vec_scalar
    ptr = ((vec_flt_t*) vp->base);
    switch(vp->width) {	
    case sizeof(float):
	((elem_float32_t*) ptr)[i] = value; break;
    case sizeof(double):
	((elem_float64_t*) ptr)[i] = value; break;
    default: break;
    }
#else
    {
	int j = (i*vp->width) / sizeof(vec_native_t);  // vector number
	ptr = ((vec_flt_t*) vp->base) + j;    
	i -= j*(sizeof(vec_native_t)/vp->width);   // element position in vector
	switch(vp->width) {
	case sizeof(float):  (*((elem_float32_t*) ptr))[i] = value; break;
	case sizeof(double): (*((elem_float64_t*) ptr))[i] = value; break;
	default: break;
	}
    }
#endif
}

/***************************************************************************
/  negate element-wise vector c = -a
***************************************************************************/

VEC_UFUNC(VEC_SIMD_TYPE, neg, int8);
VEC_UFUNC(VEC_SIMD_TYPE, neg, int16);
VEC_UFUNC(VEC_SIMD_TYPE, neg, int32);
VEC_UFUNC(VEC_SIMD_TYPE, neg, int64);
VEC_UFUNC(VEC_SIMD_TYPE, neg, float32);
VEC_UFUNC(VEC_SIMD_TYPE, neg, float64);

VEC_UFUNC_TAB(VEC_SIMD_TYPE, neg);

/***************************************************************************
/  add element-wise vector c = a+b
****************************************************************************/

VEC_BFUNC(VEC_SIMD_TYPE, add, int8);
VEC_BFUNC(VEC_SIMD_TYPE, add, int16);
VEC_BFUNC(VEC_SIMD_TYPE, add, int32);
VEC_BFUNC(VEC_SIMD_TYPE, add, int64);
VEC_BFUNC(VEC_SIMD_TYPE, add, float32);
VEC_BFUNC(VEC_SIMD_TYPE, add, float64);

VEC_BFUNC_TAB(VEC_SIMD_TYPE, add);

/***************************************************************************
/ subtract element-wise vector a-b and store in c
****************************************************************************/

VEC_BFUNC(VEC_SIMD_TYPE, sub, int8);
VEC_BFUNC(VEC_SIMD_TYPE, sub, int16);
VEC_BFUNC(VEC_SIMD_TYPE, sub, int32);
VEC_BFUNC(VEC_SIMD_TYPE, sub, int64);
VEC_BFUNC(VEC_SIMD_TYPE, sub, float32);
VEC_BFUNC(VEC_SIMD_TYPE, sub, float64);

VEC_BFUNC_TAB(VEC_SIMD_TYPE, sub);

/***************************************************************************
/ max element-wise vector c = min(a,b)
***************************************************************************/

VEC_BFUNC(VEC_SIMD_TYPE, min, int8);
VEC_BFUNC(VEC_SIMD_TYPE, min, int16);
VEC_BFUNC(VEC_SIMD_TYPE, min, int32);
VEC_BFUNC(VEC_SIMD_TYPE, min, int64);
VEC_BFUNC(VEC_SIMD_TYPE, min, float32);
VEC_BFUNC(VEC_SIMD_TYPE, min, float64);

VEC_BFUNC_TAB(VEC_SIMD_TYPE, min);


/***************************************************************************
/ max element-wise vector c = max(a,b)
***************************************************************************/

VEC_BFUNC(VEC_SIMD_TYPE, max, int8);
VEC_BFUNC(VEC_SIMD_TYPE, max, int16);
VEC_BFUNC(VEC_SIMD_TYPE, max, int32);
VEC_BFUNC(VEC_SIMD_TYPE, max, int64);
VEC_BFUNC(VEC_SIMD_TYPE, max, float32);
VEC_BFUNC(VEC_SIMD_TYPE, max, float64);

VEC_BFUNC_TAB(VEC_SIMD_TYPE, max);

/***************************************************************************
/ cmpeq element-wise c = (a==b)
***************************************************************************/

VEC_CFUNC(VEC_SIMD_TYPE, cmpeq, int8, int8);
VEC_CFUNC(VEC_SIMD_TYPE, cmpeq, int16, int16);
VEC_CFUNC(VEC_SIMD_TYPE, cmpeq, int32, int32);
VEC_CFUNC(VEC_SIMD_TYPE, cmpeq, int64, int64);
VEC_CFUNC(VEC_SIMD_TYPE, cmpeq, float32, int32);
VEC_CFUNC(VEC_SIMD_TYPE, cmpeq, float64, int64);

VEC_CFUNC_TAB(VEC_SIMD_TYPE, cmpeq);


/***************************************************************************
/ cmpgt element-wise c = (a>b)
***************************************************************************/

VEC_CFUNC(VEC_SIMD_TYPE, cmpgt, int8, int8);
VEC_CFUNC(VEC_SIMD_TYPE, cmpgt, int16, int16);
VEC_CFUNC(VEC_SIMD_TYPE, cmpgt, int32, int32);
VEC_CFUNC(VEC_SIMD_TYPE, cmpgt, int64, int64);
VEC_CFUNC(VEC_SIMD_TYPE, cmpgt, float32, int32);
VEC_CFUNC(VEC_SIMD_TYPE, cmpgt, float64, int64);

VEC_CFUNC_TAB(VEC_SIMD_TYPE, cmpgt);

/***************************************************************************
/ cmpgt element-wise c = (a<b)
***************************************************************************/

VEC_CFUNC(VEC_SIMD_TYPE, cmplt, int8, int8);
VEC_CFUNC(VEC_SIMD_TYPE, cmplt, int16, int16);
VEC_CFUNC(VEC_SIMD_TYPE, cmplt, int32, int32);
VEC_CFUNC(VEC_SIMD_TYPE, cmplt, int64, int64);
VEC_CFUNC(VEC_SIMD_TYPE, cmplt, float32, int32);
VEC_CFUNC(VEC_SIMD_TYPE, cmplt, float64, int64);

VEC_CFUNC_TAB(VEC_SIMD_TYPE, cmplt);

/***************************************************************************
/ vec_bnot
***************************************************************************/

VEC_LOCAL void CAT3(vec_,VEC_SIMD_TYPE,bnot_int)(vec_t* a, vec_t* c)
{
    vec_int_t* ap = (vec_int_t*) a->base;
    vec_int_t* cp =  (vec_int_t*) c->base;
    int nw = imin2(a->capacity, c->capacity);
    ULOOP(nw, ap, cp, vecop_bnot_bits);
}

/***************************************************************************
/ vec_band
***************************************************************************/

VEC_LOCAL void CAT3(vec_,VEC_SIMD_TYPE,band_int)(vec_t* a, vec_t* b, vec_t* c)
{
    vec_int_t* ap = (vec_int_t*) a->base;
    vec_int_t* bp =  (vec_int_t*) b->base;
    vec_int_t* cp =  (vec_int_t*) c->base;
    int nw = imin3(a->capacity, b->capacity, c->capacity);
    BLOOP(nw, ap, bp, cp, vecop_band_bits);
}
/***************************************************************************
/ vec_bor
***************************************************************************/

VEC_LOCAL void CAT3(vec_,VEC_SIMD_TYPE,bor_int)(vec_t* a, vec_t* b, vec_t* c)
{
    vec_int_t* ap = (vec_int_t*) a->base;
    vec_int_t* bp =  (vec_int_t*) b->base;
    vec_int_t* cp =  (vec_int_t*) c->base;
    int nw = imin3(a->capacity, b->capacity, c->capacity);
    BLOOP(nw, ap, bp, cp, vecop_bor_bits);
}
/***************************************************************************
/ vec_bxor
***************************************************************************/

VEC_LOCAL void CAT3(vec_,VEC_SIMD_TYPE,bxor_int)(vec_t* a, vec_t* b, vec_t* c)
{
    vec_int_t* ap = (vec_int_t*) a->base;
    vec_int_t* bp =  (vec_int_t*) b->base;
    vec_int_t* cp =  (vec_int_t*) c->base;
    int nw = imin3(a->capacity, b->capacity, c->capacity);
    BLOOP(nw, ap, bp, cp, vecop_bxor_bits);
}

/***************************************************************************
/ make table of vector op tables
****************************************************************************/

VEC_LOCAL vec_alg_t VEC_SIMD_ALG =
{
    .name = VEC_SIMD_NAME,
    .size = sizeof(vec_native_t),
    .align = sizeof(vec_native_t),
    .get_element_int = CAT3(vec_,VEC_SIMD_TYPE,get_element_int),
    .get_element_float = CAT3(vec_,VEC_SIMD_TYPE,get_element_float),
    .set_element_int = CAT3(vec_,VEC_SIMD_TYPE,set_element_int),
    .set_element_float = CAT3(vec_,VEC_SIMD_TYPE,set_element_float),
    .neg = CAT3(vec_,VEC_SIMD_TYPE,neg_ops),
    .add = CAT3(vec_,VEC_SIMD_TYPE,add_ops),
    .sub = CAT3(vec_,VEC_SIMD_TYPE,sub_ops),
    .min = CAT3(vec_,VEC_SIMD_TYPE,min_ops),
    .max = CAT3(vec_,VEC_SIMD_TYPE,max_ops),
    .cmpeq = CAT3(vec_,VEC_SIMD_TYPE,cmpeq_ops),
    .cmplt = CAT3(vec_,VEC_SIMD_TYPE,cmplt_ops),
    .cmpgt = CAT3(vec_,VEC_SIMD_TYPE,cmpgt_ops),
    .bnot  = CAT3(vec_,VEC_SIMD_TYPE,bnot_int),
    .band  = CAT3(vec_,VEC_SIMD_TYPE,band_int),
    .bor   = CAT3(vec_,VEC_SIMD_TYPE,bor_int),
    .bxor   = CAT3(vec_,VEC_SIMD_TYPE,bxor_int)
};
