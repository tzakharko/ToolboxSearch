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
 
 This file implements a fast non-deterministic finite state machine 
 for topological corpus pattern matches

--------------------------------------------------- */

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <string.h>
#include <assert.h>

static inline
int binary_search(int target, SEXP table) {
	int *a = INTEGER(table); 
	int low  = 0;
	int high = LENGTH(table)-1;
	
    while (low <= high) {
        int middle = low + (high - low)/2;
        if (target < a[middle])
            high = middle - 1;
        else if (target > a[middle])
            low = middle + 1;
        else
            return 1;
    }
    return 0;
}

//#define DEBUG 1

static
int do_match_nfa(int* tokens, int tlen, SEXP nfa,SEXP init, SEXP final, SEXP match_table, int right_boundary)
{
	// this will mark whether the machine is in the state
	int state0[LENGTH(nfa)];
	int state_final[LENGTH(nfa)];
	// this will mark whether the token was  already tested for a particular match
	int match0[LENGTH(match_table)];
	
	// init the states
	for(int i =0; i < LENGTH(nfa); i++) state0[i] = 0;
	for(int i =0; i < LENGTH(init); i++) state0[INTEGER(init)[i]-1] = 1;
	for(int i =0; i < LENGTH(nfa); i++) state_final[i] = 0;
	for(int i =0; i < LENGTH(final); i++) state_final[INTEGER(final)[i]-1] = 1;

#ifdef DEBUG	
	Rprintf("Enter - sequence of %d symbols\n", tlen);
#endif	
	
	for(int t_i = 0; t_i<tlen; t_i++)
	{
		int token = tokens[t_i];
		int state1[LENGTH(nfa)];
	
#ifdef DEBUG	
	    Rprintf("******");
		Rprintf("Current state: ");
		for(int i =0; i < LENGTH(nfa); i++) if(state0[i] == 1) Rprintf(" %d", i+1);
		Rprintf("\n");
		
		Rprintf("Received symbol %d\n", token);
#endif	
		
		// clear stuff
		for(int i =0; i < LENGTH(nfa); i++) state1[i] = 0;
		for(int i =0; i < LENGTH(match_table); i++) match0[i] = -1;
		
		// do the transitions
		for(int i =0; i < LENGTH(nfa); i++) 
		if(state0[i] == 1)
		{
			SEXP tr = VECTOR_ELT(VECTOR_ELT(nfa, i), 0);
			int tr_len = LENGTH(VECTOR_ELT(tr, 0));
			int *tr_match = INTEGER(VECTOR_ELT(tr, 0));
			int *tr_state = INTEGER(VECTOR_ELT(tr, 1));
			
#ifdef DEBUG	
			Rprintf(" Entering state %d:\n", i);
#endif	

			
			
			for(int transition = 0; transition < tr_len; transition ++)
			if(tr_state[transition]>0) // we don't want to transit to a failstate
			{
				if(tr_match[transition] == 0) // any
				{
#ifdef DEBUG	
					Rprintf(" 	transition on #any# to %d: success!\n", tr_state[transition]);
#endif						
					state1[tr_state[transition]-1] = 1;

					if(right_boundary == 0)
					if(state_final[tr_state[transition]-1] == 1)
					{
#ifdef DEBUG							
						Rprintf("NFA match!\n");
#endif						
						return(1);
					}
				}
				else if(tr_match[transition] < 0) // negative transition, we want to transit of the match is NOT successfull
				{
#ifdef DEBUG					
					Rprintf(" 	transition on %d to %d:", tr_match[transition], tr_state[transition]);
#endif						
					int m = -tr_match[transition] - 1;
					if(match0[m] == -1) match0[m] = binary_search(token, VECTOR_ELT(match_table, m));
					if(match0[m] == 0) 
					{
						state1[tr_state[transition]-1] = 1;
#ifdef DEBUG							
						Rprintf(" success!\n");
#endif							
						if(right_boundary == 0)
						if(state_final[tr_state[transition]-1] == 1)
						{
#ifdef DEBUG							
							Rprintf("NFA match!\n");
#endif						
							return(1);
						}
					} else
					{
#ifdef DEBUG							
					    Rprintf(" failure!\n");
#endif						
					}
				} else // positive transition, we want to transit of the match is successfull
				{
#ifdef DEBUG						
					Rprintf(" 	transiting on %d to %d:", tr_match[transition], tr_state[transition]);
#endif						
					int m = tr_match[transition] - 1;
					if(match0[m] == -1) match0[m] = binary_search(token, VECTOR_ELT(match_table, m));
					if(match0[m] == 1) 
					{
						state1[tr_state[transition]-1] = 1;
#ifdef DEBUG	
						Rprintf(" success!\n");
#endif				
						if(right_boundary == 0)
						if(state_final[tr_state[transition]-1] == 1)
						{
#ifdef DEBUG							
							Rprintf("NFA match!\n");
#endif						
							return(1);
						}
					} else
					{
#ifdef DEBUG							
				   		Rprintf(" failure!\n");				
#endif						
				    }
				}
			}	
		}	
		
		// test whether we are successful
//		if(right_boundary == 0)
//		{
//			for(int i =0; i < LENGTH(final); i++) if(state1[INTEGER(final)[i]-1] == 1) return(1);
//		}

		// test wheter we failed (this happens is no state is active)
		for(int i =0; i < LENGTH(nfa); i++) if(state1[i] != 0) goto nofail;
#ifdef DEBUG							
		Rprintf("NFA no match!\n");
#endif						
		return(0);
		 
nofail:		
		// update current state and go on
		for(int i =0; i < LENGTH(nfa); i++) state0[i] = state1[i];
	}

	// test whether we are successful
	for(int i =0; i < LENGTH(final); i++) if(state0[INTEGER(final)[i]-1] == 1) 
	{
#ifdef DEBUG							
		Rprintf("NFA match!\n");
#endif						
		return(1);
	}
	
#ifdef DEBUG							
	Rprintf("NFA no match!\n");
#endif						
	
	return(0);
}


SEXP match_nfa(SEXP tokens, SEXP nfa, SEXP init, SEXP final, SEXP match_table, SEXP left_boundary, SEXP right_boundary)
{
	SEXP result;
	int leftbr = LOGICAL(left_boundary)[0];
	int rightbr = LOGICAL(right_boundary)[0];
	
	
	PROTECT(result = allocVector(LGLSXP, LENGTH(tokens)));
			
		
	for(int i = 0; i < LENGTH(tokens); i++)
	{
		SEXP tok = VECTOR_ELT(tokens, i);		
		
		if(leftbr==1)
			LOGICAL(result)[i] = do_match_nfa(INTEGER(tok), LENGTH(tok), nfa, init, final, match_table, rightbr);
		else
		{	
			LOGICAL(result)[i] = 0;
			for(int o = 0; o < LENGTH(tok); o++)
			{
#ifdef DEBUG					
				Rprintf("================\n");
#endif				
				if(do_match_nfa(INTEGER(tok) + o, LENGTH(tok) - o, nfa, init, final, match_table, rightbr) == 1)
				{
					LOGICAL(result)[i] = 1;
					break;
				}
			}	
		}	
	}
	
	UNPROTECT(1);
	return(result);
}
