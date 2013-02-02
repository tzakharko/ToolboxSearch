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
 
 Various utilities (formatting, output, performance, etc. )

--------------------------------------------------- */

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <stdarg.h>

#include "mempool.c"

#include "../utf8proc/utf8proc.h"

static 
int _char_len(const char* str, int skip_invisibles_, int use_bytes_)
{
	int len=0;

	while(*str != 0x00)
	{
		int32_t uc;
		int b_;
		const utf8proc_property_t *prop;
			
		// read a single utf8 character
		b_ = utf8proc_iterate(str, -1, &uc);
		if(b_ < 0)
			error("utf8proc raised an error while scanning the string %s (error code %d)\n", str, b_);
						
		prop = utf8proc_get_property(uc);
				
		if(prop->category == 0)
			error("utf8proc: unknown character code %X\n", str, b_);

		// advance the string
		str += b_;

		// if we don't want to use byte alignment, here we do it:
		if (!use_bytes_)
			b_ = 1;
			
		// do we want to skip invisibles?
		if(skip_invisibles_ &
		   ((prop->category == UTF8PROC_CATEGORY_MN) |
		   (prop->category == UTF8PROC_CATEGORY_MC) |
		   (prop->category == UTF8PROC_CATEGORY_ME)))
		{
			// do exactly nothing as we ignore such characters
		} else
		{
			// advance the rendering pos
			len += b_;
		}
	}
	
	return(len);	
}

SEXP char_len(SEXP x, SEXP skip_invisibles, SEXP use_bytes)
{
	SEXP result;
	int skip_invisibles_ = *LOGICAL(skip_invisibles);
	int use_bytes_ = *LOGICAL(use_bytes);
	
	PROTECT(result = allocVector(INTSXP, LENGTH(x)));
	
	for(int i=0; i < LENGTH(x); i++)
		INTEGER(result)[i] = _char_len(CHAR(STRING_ELT(x, i)), skip_invisibles_, use_bytes_);		
	
	UNPROTECT(1);
	
	return(result);
}

static 
void set_class(SEXP o, const char* class)
{
	SEXP class_str;
	
	PROTECT(class_str = allocVector(STRSXP, 1));
	SET_STRING_ELT(class_str, 0, mkChar(class));
	setAttrib(o, R_ClassSymbol, class_str);
	UNPROTECT(1);
}

SEXP getListElement(SEXP list, SEXP str)
{
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);

    for (R_len_t i = 0; i < length(list); i++)
        if(STRING_ELT(names, i) == str) {
		   	
           elmt = VECTOR_ELT(list, i);
           break;
        }
    return elmt;
}

SEXP write_aligned_strings(SEXP strlist, SEXP skip_invisibles, SEXP use_bytes)
{
	int skip_invisibles_ = *LOGICAL(skip_invisibles);
	int use_bytes_ = *LOGICAL(use_bytes);
	mempool_chain_t *mempool = init_mempool_chain();
	const char *outputs[LENGTH(strlist)];
	SEXP result;
	
	// init the outputs
	for(int j=0; j<LENGTH(strlist); j++) outputs[j] = CHAR(STRING_ELT(VECTOR_ELT(strlist, j), 0));
	
	for(int i=1; i<LENGTH(VECTOR_ELT(strlist, 0)); i++)
	{
		int lens[LENGTH(strlist)];
		int next_nonzero[LENGTH(strlist)];
		int longest = 0;
		
		// we want to ignore all strings whose next token is empty
		for(int j=0; j <LENGTH(strlist); j++)
			if(strlen(CHAR(STRING_ELT(VECTOR_ELT(strlist, j), i)))==0)	next_nonzero[j] = 0; else next_nonzero[j] = 1;
		
		// compute the length of each string
		for(int j=0; j <LENGTH(strlist); j++)
			lens[j] = _char_len(outputs[j], skip_invisibles_, use_bytes_);
			
		// which string is the longest len?
		for(int j=0; j <LENGTH(strlist); j++)
		if(next_nonzero[j] == 1)
			if(lens[j] > longest) longest = lens[j];
			
		// now, add spaces to the end of the strings to match the length
		for(int j=0; j <LENGTH(strlist); j++)
		if(next_nonzero[j] == 1)
		{
			int fill = longest - lens[j];
			
			for(int k =0; k<fill; k++) outputs[j] = safe_sprintf(mempool, "%s ", outputs[j]);
		}
		
		// and finally, add the next token
		for(int j=0; j <LENGTH(strlist); j++)
		if(next_nonzero[j] == 1)
			outputs[j] = safe_sprintf(mempool, "%s %s", outputs[j], CHAR(STRING_ELT(VECTOR_ELT(strlist, j), i)));	
	}
	
	PROTECT(result = allocVector(STRSXP, LENGTH(strlist)));
	for(int j=0; j<LENGTH(strlist); j++) 
		SET_STRING_ELT(result, j, mkChar(outputs[j]));

	UNPROTECT(1);
	
	return(result);
}


/*
	This function provides pretty printing for integer sequences
	E.g. 
	  1, 2, 3, 5, 8 will be converted to '(1-3, 5, 8)'
	
	If the resulting string becomes too long (over char_limit), the function will simply 
	return '(... n elements...)'
	
*/
SEXP _format_seq(SEXP seq, SEXP char_limit)
{
	SEXP result;
	int *p =INTEGER(seq);
	char *str = NULL;
	char *str1; 
	int from, to;
	mempool_chain_t *mempool=init_mempool_chain();
	
	if(LENGTH(seq)==0) goto done_format_seq;
	
	from = to = p[0];
	for(int i = 1; i<LENGTH(seq); i++)
	{
		if(p[i] == to+1) to = p[i]; 
		else
		{
			if(from == to) 
				str1 = safe_sprintf(mempool, "%d", to); 
			else
			 	str1 = safe_sprintf(mempool, "%d-%d", from, to);
			
			if(str==NULL) 
				str=str1; 
			else 
				str=safe_sprintf(mempool, "%s, %s", str, str1);
			
			if(strlen(str)>*INTEGER(char_limit)) goto hit_limit_format_seq;
			
			from = p[i];
			to = p[i];
		}
	}	

	if(from == to) 
		str1 = safe_sprintf(mempool, "%d", to); 
	else
	 	str1 = safe_sprintf(mempool, "%d-%d", from, to);
	
	if(str==NULL) 
		str=str1; 
	else 
		str=safe_sprintf(mempool, "%s, %s", str, str1);
	
	if(strlen(str)>*INTEGER(char_limit)) goto hit_limit_format_seq;


	goto done_format_seq;

hit_limit_format_seq:
	str = safe_sprintf(mempool, "(... %d elements ...)", LENGTH(seq)); 
	
done_format_seq:	
	if(str==NULL) str="";
	PROTECT(result = allocVector(STRSXP, 1));
	SET_STRING_ELT(result, 0, mkChar(str));
	UNPROTECT(1);	
	
	return(result);
}


SEXP _fastsplit(SEXP tosplit, SEXP splitter, SEXP splitter_i)
{
	SEXP result;
	int *tosplit_p = INTEGER(tosplit);
	int *splitter_p = INTEGER(splitter);
	int p = 0;
	
	PROTECT(result = allocVector(VECSXP, LENGTH(splitter_i)));
	
	for(int i = 0; i < LENGTH(splitter_i); i++)
	{
		SEXP subs;
		int marker0, marker1, n, splitee;
		int pivot = INTEGER(splitter_i)[i];
		
		// find the first pivot index
		while(splitter_p[p] < pivot || splitter_p[p] == NA_INTEGER) p++;
			
		marker0 = p;
		
		// find where the pivot ends
		while(splitter_p[p] == pivot) 
		{
			p++;
			if(p == LENGTH(splitter)) break;
		}
		
		marker1 = p -1;

#ifdef DEBUG		
		Rprintf("Splitter %d from %d to %d\n", pivot, marker0, marker1);
#endif
		
		// count how many distinct splitees we have here
		n = 0;
		splitee = NA_INTEGER;
		for(int j = marker0; j <= marker1; j++)
		if(tosplit_p[j] != splitee && tosplit_p[j] != NA_INTEGER)
		{
			n++;
			splitee = tosplit_p[j];
		}
		
		// now we are ready to collect the splitees!
		PROTECT(subs = allocVector(INTSXP, n));
		n = 0;
		splitee = NA_INTEGER;
		for(int j = marker0; j <= marker1; j++)
		if(tosplit_p[j] != splitee && tosplit_p[j] != NA_INTEGER)
		{
			splitee = tosplit_p[j];
			INTEGER(subs)[n] = splitee;
			n++;
		}
		
		UNPROTECT(1);
		
		SET_VECTOR_ELT(result, i, subs);
		
		// we are at the end here
		if(p == LENGTH(splitter)) break;
	}
	
	
	UNPROTECT(1);
	return(result);	
}


SEXP _fast_combine_corpus(SEXP a, SEXP b, SEXP tiers, SEXP levels, SEXP a_elements_in_level, SEXP a_elements_in_tier, SEXP b_elements_in_level, SEXP b_elements_in_tier, SEXP attr_fmt_desc, SEXP attr_record_log, SEXP attr_header)
{
	SEXP result, tok_table, tier_table, names;
	SEXP a_tiers = VECTOR_ELT(a, 0), a_tok_table = VECTOR_ELT(a, 1);
	SEXP b_tiers = VECTOR_ELT(b, 0), b_tok_table = VECTOR_ELT(b, 1);
	
	
	PROTECT(result = allocVector(VECSXP, 2));
	
	// Rprintf("tiers is character %d\n", IS_CHARACTER(tiers));
	// Rprintf("levels is character %d\n", IS_CHARACTER(levels));
	
	// Rprintf("a_elements_in_level=");
	// for(int i=0;i<LENGTH(a_elements_in_level);i++) Rprintf("%d ", INTEGER(a_elements_in_level)[i]);
	// Rprintf("\n");

	// Rprintf("a_elements_in_tier=");
	// for(int i=0;i<LENGTH(a_elements_in_tier);i++) Rprintf("%d ", INTEGER(a_elements_in_tier)[i]);
	// Rprintf("\n");

#ifdef DEBUG			
	 Rprintf("doing tier table\n");
#endif
	
	// first element is the tiers list
	PROTECT(tier_table = allocVector(VECSXP, LENGTH(tiers)));
	setAttrib(tier_table, R_NamesSymbol, tiers);
	for(int tr = 0; tr < LENGTH(tiers); tr++)
	{
		SEXP a_d = getListElement(a_tiers, STRING_ELT(tiers, tr));
		SEXP b_d = getListElement(b_tiers, STRING_ELT(tiers, tr));
		int a_len = INTEGER(a_elements_in_tier)[tr];
		int b_len = INTEGER(b_elements_in_tier)[tr];
		SEXP r = allocVector(STRSXP, a_len + b_len);
		
		PROTECT(r);
		
#ifdef DEBUG
		Rprintf("   doing tier %s(a with %d[%d], b with %d[%d])\n", CHAR(STRING_ELT(tiers, tr)), a_len, LENGTH(a_d), b_len, LENGTH(b_d));
#endif
		// Rprintf("   ....starting\n");
		
		
		if(a_d == R_NilValue) // the tier is missing in a
	    {
#ifdef DEBUG
			 Rprintf("........a is NULL\n", b_len);
			 Rprintf("........setting %d NAs\n", b_len);		
#endif
			for(int i=0; i<a_len; i++) 
				SET_STRING_ELT(r, i, NA_STRING);
		 }	
		 else
		 {
			for(int i=0; i<a_len; i++) 
				SET_STRING_ELT(r, i, STRING_ELT(a_d, i));
		 } 
		
		if(b_d == R_NilValue) // the tier is missing in b
	    {
#ifdef DEBUG
			Rprintf("........b is NULL\n", b_len);
			Rprintf("........setting %d NAs\n", b_len);
#endif
			for(int i=0; i<b_len; i++) 
				SET_STRING_ELT(r, a_len+i, NA_STRING);
		 }	
		 else
		 {
			for(int i=0; i<b_len; i++) 
				SET_STRING_ELT(r, a_len+i, STRING_ELT(b_d, i));
		 }
		
		UNPROTECT(1);
		SET_VECTOR_ELT(tier_table, tr, r);
	}	
	
#ifdef DEBUG	
	Rprintf("doing token table\n");
#endif
	
	// second element is the tok.table
	PROTECT(tok_table = allocVector(VECSXP, LENGTH(levels)));
	setAttrib(tok_table, R_NamesSymbol, levels);
	
	for(int lvl = 0; lvl < LENGTH(levels); lvl++)
	{
		SEXP a_d = getListElement(a_tok_table, STRING_ELT(levels, lvl));
		SEXP b_d = getListElement(b_tok_table, STRING_ELT(levels, lvl));
		SEXP r = allocVector(INTSXP, LENGTH(a_d) + LENGTH(b_d));
		int  *_r = INTEGER(r), *_a = INTEGER(a_d), *_b = INTEGER(b_d);
		int offset = INTEGER(a_elements_in_level)[lvl];
		
		PROTECT(r);

#ifdef DEBUG		
		Rprintf("   doing level %s(offset %d)\n", CHAR(STRING_ELT(levels, lvl)), offset);
#endif
		
		for(int i=0; i<LENGTH(a_d); i++) 
		{
			*_r = _a[i];
			_r++;
		}
		
		for(int i=0; i<LENGTH(b_d); i++) 
		{
			if(_b[i] == NA_INTEGER) *_r = NA_INTEGER; else *_r = _b[i] + offset;
			_r++;
		}
		
		UNPROTECT(1);
		
		SET_VECTOR_ELT(tok_table, lvl, r);
	}
	
	SET_VECTOR_ELT(result, 0, tier_table);
	SET_VECTOR_ELT(result, 1, tok_table);
	
	PROTECT(names = allocVector(STRSXP, 2));
	SET_STRING_ELT(names, 0, mkChar("tiers"));
	SET_STRING_ELT(names, 1, mkChar("tok.table"));
	
	setAttrib(result, R_NamesSymbol, names);
	setAttrib(result, install("format.descriptor"), attr_fmt_desc);
	setAttrib(result, install("parse.log"), attr_record_log);
	setAttrib(result, install("source.header"), attr_header);
	set_class(result, "language.corpus");

	UNPROTECT(4);
	return(result);
}

SEXP _fast_unique(SEXP intvec)
{
	SEXP result;
	int current = NA_INTEGER;
	int n = 0;
	int *p = INTEGER(intvec), *r;
	
	for(int i = 0; i<LENGTH(intvec); i++)
	if(p[i] != NA_INTEGER && p[i] != current)
	{
		n++;
		current = p[i];
	}
	
	Rprintf("found %d unique\n", n);
	
	PROTECT(result = allocVector(INTSXP, n));
	r = INTEGER(result);
	
	current = NA_INTEGER;
	for(int i = 0; i<LENGTH(intvec); i++)
	if(p[i] != NA_INTEGER && p[i] != current)
	{
		*r = p[i];
		current = p[i];
		r++;
	}
	
	UNPROTECT(1);
	return(result);
}