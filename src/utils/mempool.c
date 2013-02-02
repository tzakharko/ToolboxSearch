/* ---------------------------------------------------
 Copyright 2012 Taras Zakharko
 
 This file is part of the ToolboxSearch R package.
 
 ToolboxSearch is free software: you can redistribute it and/or modify it
 under the terms of the GNU General Public License as published by the Free
 Software Foundation, either version 2 of the License, or (at your option)
 any later version.
 
 ToolboxSearch is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 more details.

---------------------------------------------------  
 
 This is a very simple chained memory pool implementation.
 Since we will dynamically generate lots of objects while trying to
 import the records, relance on standard R memory allocation is inneficient. 
 A reusable memory pool improves performance of small allocations and 
 provides us with all the benefits of automated garbage collection (meaning that
 we don't need to deallocate each small object per hand), which in turn reduces the
 change of a memory leak.

 Also, this file includes implementation of a safe_sprinf function which will always allocate 
 enough memory for a new string

 Finally, includes implementation of fast growing vector allocation in R:

 dyn_alloc will allocate an R vector of specific type 
 dyn_ensure will make sure that the vector has at least n free cells left
 dyn_clip will clip the vector to its actual length
 
--------------------------------------------------- */

#define POOL_SIZE 16384

typedef struct _mempool_chain_t {
char *start;
char *next;
unsigned int  length;
struct _mempool_chain_t *next_pool;
} mempool_chain_t;

static
mempool_chain_t *init_mempool_chain()
{
	mempool_chain_t *mempool = (mempool_chain_t*) S_alloc(1, sizeof(mempool_chain_t));
	
	mempool -> start = S_alloc(POOL_SIZE, sizeof(char));
	mempool -> next = mempool->start;
	mempool -> length = POOL_SIZE;
	mempool -> next_pool = NULL;
	
	return(mempool);
}

static
void reuse_mempool_chain(mempool_chain_t *mempool)
{
	mempool_chain_t *t = mempool;
	
	while(t)
	{
		t->next = t->start;
		memset(t->start, 0x00, t->length);
		t = t->next_pool;
	}
}


static
char* alloc_mempool_chain(mempool_chain_t *mempool, unsigned int n)
{ 
	const unsigned int mask = ~7;
	// ensure that n is aligned to next QWORD
	n = (n + 7) & mask;
	
	mempool_chain_t *t = mempool;
	char *p;
	
	while((t->next + n) > (t-> start + t -> length))
	{
		if(!t->next_pool) t->next_pool = init_mempool_chain();
		
		t = t->next_pool;
	}
	
	
	p = t->next;
	t->next = t->next + n;
	
	return(p);
}

static
char *safe_sprintf(mempool_chain_t *mempool, char * format, ...)
{
 	char *buffer;
 	int len;
   	va_list args;
	
	
  	va_start (args, format);
  	len = vsnprintf (NULL, 0, format, args);
  	va_end(args);
	
	buffer = alloc_mempool_chain(mempool, len + 1);
  	
	va_start (args, format);
	vsnprintf(buffer, len+1, format, args);
	va_end(args);


	return(buffer);
}


typedef struct {
	SEXP v;
	int next;
	SEXPTYPE tp;
} dynamic_SEXP_t;

static inline
void dyn_alloc(dynamic_SEXP_t *o, SEXPTYPE type)
{
	PROTECT(o->v = allocVector(type, 1024));
	o->tp = type;
	o->next=0;
}

static inline
void dyn_ensure(dynamic_SEXP_t *o, int n)
{
	if(o->next + n > LENGTH(o->v))
	{
		int newlen = LENGTH(o->v) + 1024 * (n/1024 + 1);
		SEXP nv;
	
		PROTECT(nv = allocVector(o->tp, newlen));
		UNPROTECT_PTR(o->v);

	
		for(int i = 0; i < o->next; i++)
		if(o->tp == VECSXP)
			SET_VECTOR_ELT(nv, i, VECTOR_ELT(o->v, i)); else
		if(o->tp == INTSXP)
			INTEGER(nv)[i] = INTEGER(o->v)[i]; else
		if(o->tp == STRSXP)
			SET_STRING_ELT(nv, i, STRING_ELT(o->v, i)); else
		error("Unsupported type!");

	
		o->v = nv;		
	}
}

static inline
void dyn_clip(dynamic_SEXP_t *o)
{
	SEXP nv;
	
	PROTECT(nv = allocVector(o->tp, o->next));

	
	for(int i = 0; i < o->next; i++)
	if(o->tp == VECSXP)
		SET_VECTOR_ELT(nv, i, VECTOR_ELT(o->v, i)); else
	if(o->tp == INTSXP)
		INTEGER(nv)[i] = INTEGER(o->v)[i]; else
	if(o->tp == STRSXP)
		SET_STRING_ELT(nv, i, STRING_ELT(o->v, i)); else
		error("Unsupported type!");

	UNPROTECT_PTR(o->v);
	
	o->v = nv;
}