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
 
 This is the Toolbox file parser. It implements the toolbox import 
 functionality of ToolboxSearch

 The the core function is 
  
	SEXP read_toolbox(SEXP lines, SEXP format, SEXP tokens_to_ignore, SEXP dissalow_hanging, SEXP strict)

 which is the interface between the C parser and the R code.

 The parser allows flexible operating modes:
   
   - parse the file assuming that original Toolbox spacing is valid (attemps to reconstruct gloss structure from file spacing)*
   - parse the file using use-provided morpheme delimiters (attemps to reconstuct gloss structure from morpheme shapes)

--------------------------------------------------- */

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <stdarg.h>

#include "../utf8proc/utf8proc.h"

// memory management
#include "../utils/mempool.c"


/*
	Concatenate two strings (similar to R's paste)
*/
static inline
char * concat(const char *s0, const char *s1, const char *sep)
{
	char *dest = S_alloc(strlen(s0) + strlen(s1) + strlen(sep) +1, sizeof(char));
    memcpy(dest, s0, strlen(s0)*sizeof(char));
    memcpy(dest+strlen(s0), sep, strlen(sep)*sizeof(char));
    memcpy(dest+strlen(s0)+strlen(sep), s1, strlen(s1)*sizeof(char));
    return dest;
}

/*
	Given a line from a toolbox file, split it into the marker and the data strings
*/
static
int scan_toolbox_line(const char *str, char **marker, char **data)
{
	*marker = NULL;
	*data = NULL;
	
	// skip the spaces
	while (*str != 0x00 & *str == 0x20) str++;
	
	// if we are at the end, the string was empty
	if (*str == 0x00) return(0);
		
	// is this a beginning of a marker?
	if (*str == '\\')
	{
		const char* start = str+1;

		// skip until the first space
		while (*str != 0x00 & *str != 0x20) str++;
		
		*marker = S_alloc(str - start + 1, sizeof(char));
		strncpy(*marker, start, str-start);
	} 
	else *marker = NULL;
		
	// skip the spaces
	while (*str != 0x00 & *str == 0x20) str++;
		
	// if we are at the end, there is no data
	if (*str == 0x00) return(-1);

	{
		const char* start = str;

		// skip until the string end
		while (*str != 0x00) str++;
		str--;
		// and now skip the spaces backwards
		while(*str == 0x20) str--;
		str++;
		
		*data = S_alloc(str - start + 1, sizeof(char));
		strncpy(*data, start, str-start);
	}
	
	return(1);
}

/*
 Representation of a token
*/
typedef struct _toolbox_token_t {
char *str; // copy of the string token
int  render_position; // offset in the string calculated as number of unicode characters (invisible characters skipped)
int  real_position; // byte offset in the string
int  render_width; // number of unicode characters (invisible characters skipped)
int  source_line_number; // line of the original file
int  token_id;  // id in the line
int  string_cont; // true if this token is a continuation of the glossed word (only applicable to morphemes)
struct _toolbox_token_t *next; // next token in the chain
} toolbox_token_t;


#define NEW_TOKEN(mempool) (toolbox_token_t *) alloc_mempool_chain(mempool, sizeof(toolbox_token_t));

 typedef struct {
 	bool skip_invisibles;
 	bool use_bytes;
 } tokenize_settings_t;

static 
toolbox_token_t *collect_tokens(mempool_chain_t *mempool, char *str, int pos_offset, int line_num,  int id0, const tokenize_settings_t *settings, SEXP connector_chars)
{
	toolbox_token_t *token;
	toolbox_token_t dummy_first;
	size_t render_p = 0;
	char *tok;
	int cont = 0;
	
	// alloc first token
	token = &dummy_first;
	
	// skip spaces
	while (*str == 0x20) {str++; render_p++;}
	
	// parsing loop
	while(*str != 0x00)
	{
		const char *tokStart_ = str;
		const int tokPos_ = render_p;
				
		// scan the token
		while ((*str!= 0x20) & (*str != 0x00))
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
			if (!settings->use_bytes)
				b_ = 1;
			
			// do we want to skip invisibles?
			if(settings->skip_invisibles &
			   ((prop->category == UTF8PROC_CATEGORY_MN) |
			   (prop->category == UTF8PROC_CATEGORY_MC) |
			   (prop->category == UTF8PROC_CATEGORY_ME)))
			{
				// do exactly nothing as we ignore such characters
			} else
			{
				// advance the rendering pos
				render_p += b_;
			}
			
		}
			
			
		// extract the substring
		tok = S_alloc( (long) str - (long) tokStart_ +1, sizeof(char));
		strncpy(tok, tokStart_, (size_t) str - (size_t) tokStart_);
		
		// test if the token starts with a connector string
		for(int i=0; i < LENGTH(connector_chars); i++)
			if(tok[0] == CHAR(STRING_ELT(connector_chars, i))[0]) {cont = 1; break;}
	
		// allocate new token struct
		token->next = NEW_TOKEN(mempool);
		token = token->next;
		
		// fill in the token data
		token->str=tok;
		token->render_position = tokPos_ + pos_offset;
		token->real_position = tokPos_;
		token->render_width = render_p - tokPos_;
		token->source_line_number = line_num;
		token->token_id = id0;
		token->string_cont = cont;
			
		id0++;
		
		cont = 0;
		
		// test if the token ends with a connector string
		for(int i=0; i < LENGTH(connector_chars); i++)
			if(tok[strlen(tok)-1] == CHAR(STRING_ELT(connector_chars, i))[0]) {cont = 1; break;}

		// skip spaces
		while (*str == 0x20) {str++; render_p++;}
	}
	
	return(dummy_first.next);
}




/*  align_tokens()
    
    This is a rather complicated function which attempts to align the tokens in the toolbox record
    TODO: maybe document this better at some point


	input to this function is the list with following components (just to save time writing buggy C)
	0: vector of tier markers (first one being the record marker)
	1: integer vector of tier level 
	2: integer vector of tier property: 
	 		0 - top-level tier
	        1 - top-level tier  with grandfather treatment
	 		2 - has to be split with exact align
	 		3 - has to be split but not aligned (align per number of tokens to the "main" tier token)
	3: integer scalar with the number of levels
*/


typedef struct {
	int number_of_tiers;
	int number_of_levels;
	const char **tier_marker;
	SEXP *connector_chars;
	const char *master_marker_str;
	int *tier_level;
	int *tier_prop;
} toolbox_fmt_t;


/*
 Token align state machine

 TODO: I forgot how this actually works :(
*/
typedef struct {
	int total;
	char **marker;
	int *marker_tier;
	char **data;
} toolbox_lines_t;

enum token_align_state {
	waiting, 
	open, 
	done, 
	next, 
	mark,
	mark_and_next
};


static inline
int _match_token(int current, toolbox_token_t **token, enum token_align_state *state)
{
	int any_open_match = 0;
	int any_direct_match = 0;
		
			
	// is this the highest-level token?
	if(current == 0)
	{
		// then we should mark only it and move along imemdiately without putting it into the open state
		state[0] = mark_and_next;
		return(1);  
	}
	
	// check whether the current token is within the range of all open tokens
	for(int i = 0; i < current; i++)
	if(state[i] == open)
	{
		any_open_match = 1;
		
		// is the current token still within the open token's reach?
		if(token[i]->next)
		{
			if ( ((token[i]->next)->render_position) <= (token[current]->render_position + token[current]->render_width) )
				return(0); // at least one open token is too "short", no match here!
		}
	}
	
	// here, either we fit all the open tokens, or there are none (which is ok as well)
	// we look for direct matches
	for(int i = 0; i < current; i++)
	if(state[i] == waiting)
	{
		// is it a direct match?
		if((token[i]->render_position) == (token[current]->render_position))
		{
			state[i] = mark;
			any_direct_match = 1;
		}
	}
	
	// here we have marked all the direkt matches (if any)
	// now for some final treatment
	if(!(any_open_match | any_direct_match))
	{
		return(0);
	}
	
	// mark any open tokens
	if(any_open_match)
	for(int i = 0; i < current; i++)
		if(state[i] == open) state[i] = mark;
	
	// and mark the current
	state[current] = mark;
	
	return(1);
}


typedef struct  _toolbox_aligned_tokens_t{
	int  levelCount;
	int  *token_id;
	struct _toolbox_aligned_tokens_t *next;
} toolbox_aligned_tokens_t;



static
char *align_tokens(mempool_chain_t *mempool, toolbox_fmt_t *fmt, toolbox_token_t **levels, int record_id, dynamic_SEXP_t *align_table)
{
	const int levelCount = fmt->number_of_levels-1;
	toolbox_token_t *current_token[levelCount]; // stores the current token
	enum token_align_state state[levelCount]; // stores the current state
	toolbox_aligned_tokens_t firstMark;
	toolbox_aligned_tokens_t *currentMark = &firstMark;
	
	// initialize the states
	for(int i = 0; i < levelCount; i++)
	{
		current_token[i] = levels[i];
		
		// a token may be null (for empty)
		if(current_token[i])
		{
			state[i] = waiting;
		} else
		{
			state[i] = done;
		}
	}
	
	
	// do the actual state computation
	while(1)
	{
		int current = -1;
		int marking[levelCount];
		int any_marked = 0;
		 
		// mark each token which requires marking
		for(int i = 0; i < levelCount; i++)
		if(state[i] == mark || state[i] == mark_and_next )
		{
			marking[i] = current_token[i]->token_id+1;
			if (state[i] == mark) state[i] = open; else state[i] = next;
			any_marked = 1;
		} else
			marking[i] = NA_INTEGER;
		
		// if anythign was marked, store it:
		if(any_marked)
		{
			int *_p;
			
			dyn_ensure(align_table, levelCount+1);
			_p = INTEGER(align_table->v) + align_table->next;
			_p[0] = record_id;
			for(int i=0; i<levelCount; i++)
				_p[i+1] = marking[i];
				
			align_table->next = align_table->next + levelCount + 1;	
		}
			
		
		// advance all levels which need anvancing
		for(int i = 0; i < levelCount; i++)
		if(state[i] == next)
		{
			current_token[i] = current_token[i]->next;
			// a token may be null (for empty)
			if(current_token[i])
			{
				state[i] = waiting;
			} else
			{
				state[i] = done;
			}
		}
			
		// check if all are done
		{
			int all_done = 1;
			for(int i = 0; i < levelCount; i++) 
			if(state[i] != done)
			{
				all_done = 0;
				break;
			}
			
			// quit the loop if all are done!
			if(all_done) break;
		}
		
		
		// find a left-most, bottom-most token in waiting state
		for(int i = 0; i<levelCount; i++)
		if(state[i] == waiting)
		{
			if(current==-1) 
				current = i; // this is the first suitable candidate
			else
			{
				if(current_token[i]->render_position <= current_token[current]->render_position)
					current = i; // this is a waiting token more to the left or bottom
			}
		}		
		
		// if a suitable candidate was found, we must perform the matching
		if(current != -1)
			if(_match_token(current, current_token, state)) continue;
			
		
		// if we are here, no match could be performed and we want to advance the most-right lower open token
		// find a right-most, bottom-most token in open state
		current = -1;
		for(int i = 0; i<levelCount; i++)
		if(state[i] == open)
		{
			if(current==-1) 
				current = i; // this is the first suitable candidate
			else
			{
				if(current_token[i]->render_position >= current_token[current]->render_position)
					current = i; // this is a open token more to the right or bottom
			}
		}		
		
		if(current == -1)
		{	
			////Rprintf("Impossible token configuration!\n");
			return(safe_sprintf(mempool, "\terror: no proper alignment between tiers %s! Re-check the record!\n", fmt->master_marker_str));
		}
			
		// advance this one
		state[current] = next;
	}
	
	return(NULL);
}

/*
	This function reads in a single record
*/
static
char* _scan_record(mempool_chain_t *mempool, toolbox_fmt_t *fmt,  toolbox_lines_t *line_data, int r_first, int r_last, toolbox_token_t **token, const tokenize_settings_t *tok_settings, int *id0, SEXP tokens_to_ignore)
{
	int current = r_first;
	int max_len = 0;
	int lines_per_tier[fmt->number_of_tiers];
	char *report = "";
	
	// init with zeros
	for(int i =0; i < fmt->number_of_tiers; i++)
		lines_per_tier[i] = 0;
		
	// determine the length of the longest interlinear line (we need this to correctly process multi-line glosses)
	for(int i = current+1; i <= r_last; i++)
	if(line_data->marker_tier[i] > 1)
	{
		int l = 0;
		if(line_data->data[i]) l = strlen(line_data->data[i]);
		if(l > max_len) max_len = l;
	}	
		
	// read in the record header
	token[0] = NEW_TOKEN(mempool);
	token[0]->str = line_data->data[current];
	token[0]->source_line_number = current + 1; // offset by one 
	
	if(!line_data->data[current])
		token[0]->str = "(empty)";
		
	
	while(++current <= r_last)
	if(line_data->marker_tier[current] > 0)
	{
		const int ti = line_data->marker_tier[current];
		
		// different treamtment dependign on the tier type
		if(fmt->tier_prop[ti]==1)
		{
				
			if(line_data->data[current] == NULL)
			 	continue; // if no data, nothing happened

			if(token[ti] == NULL)
			{
				token[ti] = NEW_TOKEN(mempool);
				token[ti]->str = line_data->data[current];
				token[ti]->source_line_number = current + 1; // nooffset by one
			}
			else
			{	
				if(*(line_data->data[current]) != 0x00)
					token[ti]->str = safe_sprintf(mempool, "%s %s", token[ti]->str, line_data->data[current]);	
			}
			
			lines_per_tier[ti]++;
			
			// now do the grandfather
			// while(1)
			// {
			// 	// yank next line
			// 	current++;
			// 	if(current > r_last) break; // eof
			// 
			// 		
			// 	if(line_data->marker[current] != NULL || line_data->data[current] == NULL) break;
			// 	if(line_data->data[current] != NULL) 
			// 	{
			// 		if(*(line_data->data[current]) != 0x00)
			// 			token[ti]->str = safe_sprintf(mempool, "%s %s", token[ti]->str, line_data->data[current]);	
			// 		lines_per_tier[ti]++;
			// 	}
			// }
			// 
			// current--;
		}
 		else
		{
 				
			if(line_data->data[current] == NULL)
				continue;
			

			if(token[ti] == NULL)
			{
				token[ti] = collect_tokens(mempool, line_data->data[current], 0, current, id0[ti], tok_settings, fmt->connector_chars[ti]);
			}
			else
			{
				toolbox_token_t *last;
				
				last = token[ti];
				while(last->next) last = last->next;
				
				last->next = collect_tokens(mempool, line_data->data[current], lines_per_tier[ti]*(max_len+1), current, last->token_id+1, tok_settings, fmt->connector_chars[ti]);	
			}
			
			lines_per_tier[ti]++;
		}
 	}
	
	return(report);
} 

static
const char *_write_tokens(mempool_chain_t *mempool, toolbox_fmt_t *fmt, toolbox_token_t **parsed_tok, dynamic_SEXP_t *token_list)
{
	const char *report = "";
	
	// write the higher-level tokens
	// important! write NAs where there was no input (record-level tokens always have an id!)
	for(int i = 0; i < fmt->number_of_tiers; i++)
	if(fmt->tier_level[i] < 2)
	{
		if (parsed_tok[i] != NULL)
		{
			dyn_ensure(&token_list[i], 1);
			SET_STRING_ELT(token_list[i].v, token_list[i].next, mkChar(parsed_tok[i]->str));
			token_list[i].next++;
		} else
		{
			dyn_ensure(&token_list[i], 1);
			SET_STRING_ELT(token_list[i].v, token_list[i].next, NA_STRING);
			token_list[i].next++;
		}
	}
	
	// validate and write each level
	for(int lvl = 2; lvl <= fmt->number_of_levels; lvl++)
	{
		int master_tier;
		int master_token_count;
		int num_tiers = 0;
		toolbox_token_t **lvl_tok;
		int *lvl_tier;
		int *is_doa;
		int fail = 0;

		int c = 0;
			
		// locate the tiers which belong to this level
		for(int i = 1; i < fmt->number_of_tiers; i++)
			if(fmt->tier_level[i]==lvl) num_tiers++;
			
		lvl_tok = (toolbox_token_t**) S_alloc(num_tiers, sizeof(toolbox_token_t**));
		lvl_tier = (int*) S_alloc(num_tiers, sizeof(int));
		is_doa = (int*) S_alloc(num_tiers, sizeof(int));

		for(int i = 1; i < fmt->number_of_tiers; i++)
			if(fmt->tier_level[i]==lvl) 
			{
				lvl_tok[c] = parsed_tok[i];
				lvl_tier[c] = i;
				c++;
			}
	
		
		if(lvl_tok[0] == NULL)
		{
			for(int i = 1; i < num_tiers; i++)
			if(lvl_tok[i])
			{
				report = safe_sprintf(mempool, "%s\terror: tier %s is not empty, but the master tier %s is empty or missing!\n", report, fmt->tier_marker[lvl_tier[i]], fmt->tier_marker[lvl_tier[0]]);
				
				continue;
			}
			
			continue; // the level is just empty or missing
		}	
		
		// mark each empty tier as DOA
		for(int i = 1; i < num_tiers; i++)
			if(!lvl_tok[i])	
				is_doa[i] = 1;
			
		// match each tear to the master tier, while writing the tokens to the token list
		while(lvl_tok[0])
		{
			for(int i = 1; i < num_tiers; i++)
			{
				const int ti = lvl_tier[i];
				
				if(!lvl_tok[i]) // the token list has run out!
				{
					if(fmt->tier_prop[ti] == 3 & !is_doa[i]) // must match the number of master tokens
					{
						// there was obviously an error
						report = safe_sprintf(mempool, "%s\terror: number of tokens in tier %s does not match the master tier %s!\n", report, fmt->tier_marker[ti], fmt->tier_marker[lvl_tier[0]]);
						is_doa[i] = 1; // fake DOA so that it does not bother us again
						lvl_tok[i] = NULL;
						continue;
					}

					// otherwise, simply write an NA into the token list
					dyn_ensure(&token_list[ti], 1);
					SET_STRING_ELT(token_list[ti].v, token_list[ti].next, NA_STRING);
					token_list[ti].next++;
					continue;
				}
								
				if(fmt->tier_prop[ti] == 2) // must align exactly to the master tier, but may allow missing values
				{
					if(lvl_tok[i]->render_position > lvl_tok[0]->render_position)
					{
						// this is possibly a gap, so we just write a NA into the token list
						dyn_ensure(&token_list[ti], 1);
						SET_STRING_ELT(token_list[ti].v, token_list[ti].next, NA_STRING);
						token_list[ti].next++;
						continue;
					}
					if(lvl_tok[i]->render_position < lvl_tok[0]->render_position)
					{
						// there was obviously an error
						report = safe_sprintf(mempool, "%s\terror: tier %s does not match the alignment of the master tier %s!\n", report, fmt->tier_marker[ti], fmt->tier_marker[lvl_tier[0]]);
						is_doa[i] = 1; // fake DOA so that it does not bother us again
						lvl_tok[i] = NULL;

						continue;
					}
				}
				
				// write and advance the token
				dyn_ensure(&token_list[ti], 1);
				SET_STRING_ELT(token_list[ti].v, token_list[ti].next, mkChar(lvl_tok[i]->str));
				token_list[ti].next++;
				lvl_tok[i] = lvl_tok[i] -> next;
			}
			
			
			
			// write and advance the master token
			dyn_ensure(&token_list[lvl_tier[0]], 1);
			SET_STRING_ELT(token_list[lvl_tier[0]].v, token_list[lvl_tier[0]].next, mkChar(lvl_tok[0]->str));
			token_list[lvl_tier[0]].next++;
			lvl_tok[0] = lvl_tok[0] -> next;
		}
		
		// the master tier is done, so must be all the others
		for(int i = 1; i < num_tiers; i++)
		if(lvl_tok[i])
			report = safe_sprintf(mempool, "%s\terror: tier %s has tokens after the end of the master tier %s!\n", report, fmt->tier_marker[lvl_tier[i]], fmt->tier_marker[lvl_tier[0]]);

	}
	
	
	if(strlen(report)==0)
		return(NULL); 
	else return(report);
}

const tokenize_settings_t tok_possibilities[] = 
{
	{0, 0},
	{1, 0},
	{0, 1},
	{1, 1}
};

void offset_token(toolbox_token_t *t, int offset)
{
	while(t)
	{
		t->render_position = t->render_position + offset;
		t = t->next;
	}
}


/*
 force the alignment of a and b tokens
 a_i is the tier index of the a token, the b token must be a_i+1
*/

void force_aling(toolbox_token_t **master_tokens, int len, int a_i, toolbox_token_t* a,  toolbox_token_t* b)
{
	int offset = a->render_position - b->render_position;
	
		
	if(offset == 0) return; // nothing to do
	
	// if offset is positive, then, we have a situation like
	//
	//   x    x
	//   x    x   x   <- a
	// x   x          <- b
	//
	// and we must move all the tokens b and below to the right
	if(offset > 0)
	{
		int marker = b->render_position;
		
		for(int i = a_i+1; i<len; i++)
		{
			toolbox_token_t *t = master_tokens[i];
			while(t->render_position < marker)
			{
				t  = t->next;
				if(!t) break; 
			}
			
			
			offset_token(t, offset);
		}
	} else
	// if offset is negative, then, we have a situation like
	//
	//   x    x
	//   x    x   x   <- a
	//     x   x          <- b
	//
	// and we must move all the tokens a and above to the right
	{
		int marker = a->render_position;
		offset = -offset;
		for(int i = 0; i<=a_i; i++)
		{
			toolbox_token_t *t = master_tokens[i];
			while(t->render_position < marker)
			{
				t  = t->next;
				if(!t) break; 
			}
			offset_token(t, offset);
		}
	}	

}


/*
	The main interface function
*/
SEXP read_toolbox(SEXP lines, SEXP format, SEXP tokens_to_ignore, SEXP dissalow_hanging, SEXP strict)
{
	// input
	toolbox_lines_t line_data;
	toolbox_fmt_t fmt;
	// output
	dynamic_SEXP_t tokens[LENGTH(VECTOR_ELT(format, 0))];
	dynamic_SEXP_t align_table;
	SEXP log_per_record;	
	SEXP result;
	SEXP scan_log;
	// record data
	int record_count = 0;
	int *record_offset;
	mempool_chain_t *mempool = init_mempool_chain();
	int fail_on_hanging = INTEGER(dissalow_hanging)[0];
	int start_tok_setting;
	
	if (INTEGER(strict)[0] == 0) start_tok_setting=0; else start_tok_setting=3; 
		
	// initalize the format
	fmt.number_of_tiers = LENGTH(VECTOR_ELT(format, 0));
	fmt.number_of_levels = *INTEGER(VECTOR_ELT(format, 3));
	fmt.tier_marker = (const char**) S_alloc(fmt.number_of_tiers, sizeof(char*));
	fmt.connector_chars = (SEXP*) S_alloc(fmt.number_of_tiers, sizeof(SEXP));
	fmt.tier_level = INTEGER(VECTOR_ELT(format, 1));
	fmt.tier_prop = INTEGER(VECTOR_ELT(format, 2));
	fmt.master_marker_str = NULL;
	

	
	for(int i=0; i < fmt.number_of_tiers; i++)
	{
		fmt.tier_marker[i] = CHAR(STRING_ELT(VECTOR_ELT(format, 0), i));
		fmt.connector_chars[i] = VECTOR_ELT(VECTOR_ELT(format, 4), i);
	}
		
	
	for(int lvl = 2; lvl <= fmt.number_of_levels; lvl++)
		for(int i = 1; i < fmt.number_of_tiers; i++)
		if(fmt.tier_level[i] == lvl)
		{
			if (!fmt.master_marker_str) 
				fmt.master_marker_str = fmt.tier_marker[i];
			else
				fmt.master_marker_str = concat(fmt.master_marker_str, fmt.tier_marker[i], ", ");
			
			break;	
		}
	
		
			
	// initialize the result
	for(int i=0; i < fmt.number_of_tiers; i++)
		dyn_alloc(&tokens[i], STRSXP);
		
	dyn_alloc(&align_table, INTSXP);	
	
	// extract the line data
	line_data.marker = (char**) S_alloc(LENGTH(lines), sizeof(char*));
	line_data.data = (char**) S_alloc(LENGTH(lines), sizeof(char*));
	line_data.marker_tier = (int*) S_alloc(LENGTH(lines), sizeof(int));
	line_data.total = LENGTH(lines);
	
	for(int i=0; i < line_data.total; i++)
	{
		scan_toolbox_line(CHAR(STRING_ELT(lines, i)), &line_data.marker[i], &line_data.data[i]);
		line_data.marker_tier[i] = -1;
	}
	
	for(int i=0; i < line_data.total; i++)
	{
		if(line_data.marker[i] == NULL) continue;
		for(int tier=0; tier < fmt.number_of_tiers; tier++)
			if(strcmp(line_data.marker[i], fmt.tier_marker[tier])==0) 
			{
				line_data.marker_tier[i] = tier;
				if(tier==0) record_count++; // count the records
				break;
			}
	}
	
		
	// locate the records
	record_offset = (int*) S_alloc(record_count+1, sizeof(int));	
	{
		int c = 0;
		for(int i=0; i < line_data.total; i++)
			if(line_data.marker_tier[i] == 0)
			{
				record_offset[c] = i;
				c++;
			}
		record_offset[c] = line_data.total;
	}
	
	
	PROTECT(log_per_record = allocVector(STRSXP, record_count));
	PROTECT(scan_log = allocVector(STRSXP, record_count));

	for(int i=0; i < record_count; i++)
	{
		int fail=1;
		
		reuse_mempool_chain(mempool);
		
		for(int tok_poss = start_tok_setting; tok_poss < 4; tok_poss++)	
		if(fail)
	{
		toolbox_token_t *tier_tokens[fmt.number_of_tiers];
		toolbox_token_t *master_token[fmt.number_of_levels - 1];
		int master_align_type[fmt.number_of_levels - 1];
		const char *status = NULL;
		char *scan_log_ = "";
		int token_old_pos[fmt.number_of_tiers];
		int align_old_pos;
		int all_empty = 1;
		
		// init to zero
		for(int i = 0; i<fmt.number_of_tiers; i++)
			tier_tokens[i] = NULL;
		
		// store the current pos for rollback in case of an error
		for(int i = 0; i<fmt.number_of_tiers; i++)
			token_old_pos[i] = tokens[i].next;
		align_old_pos = align_table.next;	
		
		_scan_record(mempool, &fmt, &line_data, record_offset[i], record_offset[i+1]-1, tier_tokens, &tok_possibilities[tok_poss], token_old_pos, tokens_to_ignore);
		
		status = _write_tokens(mempool, &fmt, tier_tokens, tokens);

		if(status != NULL) goto rollback;
				
			
		// now we have to align the interlinear levels
		// 1. gather the master tokens
		for(int lvl = 2; lvl <= fmt.number_of_levels; lvl++)
			for(int i = 1; i < fmt.number_of_tiers; i++)
			if(fmt.tier_level[i] == lvl)
			{
				master_token[lvl-2] = tier_tokens[i];
				master_align_type[lvl-2] = fmt.tier_prop[i];
				if(tier_tokens[i]) all_empty = 0;
				break;
			}
		
		
		// force the alignment of elvels if the
		// alignment is given by the morpheme continuation via morpheme connectors (hyphens)
		for(int i=1; i < fmt.number_of_levels - 1; i++)
		if(master_align_type[i] == 5) 
		{
			toolbox_token_t* a = master_token[i-1];
			toolbox_token_t* b = master_token[i];
			
			// if the level is missing, we go on
			if(!b) continue;
						
			while(a != NULL && b != NULL)
			{
				force_aling(master_token, fmt.number_of_levels - 1, i-1, a, b);

				a = a->next;
				b = b->next;

				while(b)
				{
					if(!b->string_cont) break;
					b = b->next;
				}	
			}
			
			
			if(a == NULL && b != NULL)
			{
				// here, b is requesting a new master token, but there is none. This is obviously an error
				status = safe_sprintf(mempool, "\terror: no proper alignment between tiers %s! Re-check the record!\n", fmt.master_marker_str);
				goto rollback;
			}
			
			if(a != NULL && b == NULL)
			{
				// here, we want to move a (and everything above it) far out of the boundary of last b
				int marker = a->render_position;
				int offset;
				b = master_token[i];
				while(b->next) b=b->next;
				
				offset = (b->render_position + b->render_width + 2) - a->render_position;
				if(offset > 0)
					for(int j = 0; j<=i-1; j++)
					{
						toolbox_token_t *t = master_token[j];
						while(t->render_position < marker)
						{
							t  = t->next;
							if(!t) break; 
						}
			
			
						offset_token(t, offset);					
					}
			}
		}
		
			
		//	if all are empty, we don't need to align anything
		// but write a single empty record
		if(all_empty)
		{
			int *_p;
			
			dyn_ensure(&align_table, fmt.number_of_levels);
			_p = INTEGER(align_table.v) + align_table.next;
			_p[0] = i+1;
			for(int i=0; i<fmt.number_of_levels-1; i++)
				_p[i+1] = NA_INTEGER;
				
			align_table.next = align_table.next + fmt.number_of_levels;	
			SET_STRING_ELT(log_per_record, i, mkChar(" not glossed!\n"));
			fail=0;
			continue;
		}
		

		status = align_tokens(mempool, &fmt, master_token, i+1, &align_table);
		
		
		
		if(status != NULL) 
		{
			// prepare a nicely formated token list so that we can see what went wrong
			char *line;
			char *zero_line;
			
			
			for(int i = 0; i < fmt.number_of_levels-1; i++)
			{
				toolbox_token_t *t = master_token[i];
				
				int toksofar = 0; 
				
				for(int j = 1; j < fmt.number_of_tiers; j++)
					if(fmt.tier_level[j] == i + 2)
					{
						zero_line = safe_sprintf(mempool, "      %s:", fmt.tier_marker[j]);
						break;
					}
				
				line = safe_sprintf(mempool, "%s", zero_line);
				for(int jjj = 0; jjj < strlen(zero_line); jjj++)
					zero_line[jjj] = 0x20;
					
					
				while(t)
				{
					if(toksofar % 5 == 0 && !(toksofar==0))
						line = safe_sprintf(mempool, "%s\n%s", line, zero_line);
						
					line = safe_sprintf(mempool, "%s  %s(%d:%d)", line, t->str, t->source_line_number, t->real_position);
					
					t = t->next;
					toksofar++;
				}	
				
				line = safe_sprintf(mempool, "%s\n%s  ------------------------", line, zero_line);
				
				status = safe_sprintf(mempool, "%s\n%s", status, line);
			}
			
			status = safe_sprintf(mempool, "%s\n", status);
			
			goto rollback;
		} 
		
		
		if(fail_on_hanging == 1)
		{
			// we have a hanging if there is at least one NA in the produced align table, but also non-nas
			int *p = INTEGER(align_table.v); 
			// cc is the width of a token index line
			int cc = (align_table.next - align_old_pos) / fmt.number_of_levels; 
			
			for(int level_i = 1; level_i < fmt.number_of_levels; level_i++)
			{
				int na_count = 0;
				for(int pi = 0; pi < cc; pi++)
				{
					int pi_ = align_old_pos + pi*fmt.number_of_levels + level_i;
					
					if(p[pi_] == NA_INTEGER) na_count++;
				}
				
				if(na_count > 0 && na_count < cc) 
				{
					status = safe_sprintf(mempool, "\terror: no proper alignment between tiers %s! Re-check the record!\n", fmt.master_marker_str);
					goto rollback;					
				}
			}	
		}

		SET_STRING_ELT(log_per_record, i, mkChar(safe_sprintf(mempool, "ok(settings: skip_invisibles=%d, use_bytes=%d)", tok_possibilities[tok_poss].skip_invisibles, tok_possibilities[tok_poss].use_bytes)));
		SET_STRING_ELT(scan_log, i, mkChar(scan_log_));
		

		
		fail = 0;
		continue;
		
rollback:
		{
			int *_p;
			
			if(tok_poss != 3) // we still have some choices left
			{
				// total rollback
				for(int i = 0; i < fmt.number_of_tiers; i++)
					tokens[i].next = token_old_pos[i];
				align_table.next = align_old_pos;
				
				continue;
			}
			
			// Rollback! but write the record (make sure its all NAs below)
			for(int i = 1; i < fmt.number_of_tiers; i++)
			if(fmt.tier_level[i]>=2)
				tokens[i].next = token_old_pos[i];
			align_table.next = align_old_pos;

			dyn_ensure(&align_table, fmt.number_of_levels);
			_p = INTEGER(align_table.v) + align_table.next;
			_p[0] = i+1;
			for(int i=0; i<fmt.number_of_levels-1; i++)
				_p[i+1] = NA_INTEGER;
				
			align_table.next = align_table.next + fmt.number_of_levels;	

			status = safe_sprintf(mempool, "failure(settings: skip_invisibles=%d, use_bytes=%d)\n%s", tok_possibilities[tok_poss].skip_invisibles, tok_possibilities[tok_poss].use_bytes, status);

			SET_STRING_ELT(log_per_record, i, mkChar(status));
			SET_STRING_ELT(scan_log, i, mkChar(scan_log_));		
		}
	}
	}	
		
	for(int i = 0; i < fmt.number_of_tiers; i++)
		dyn_clip(&tokens[i]);
	
	dyn_clip(&align_table);	
	
	
	
	SEXP tiers_list;
		
	PROTECT(result = allocVector(VECSXP, 4));
	PROTECT(tiers_list = allocVector(VECSXP, fmt.number_of_tiers));
		
	for(int i = 0; i < fmt.number_of_tiers; i++)
		SET_VECTOR_ELT(tiers_list, i, tokens[i].v);
		
	SET_VECTOR_ELT(result, 0, tiers_list);
	SET_VECTOR_ELT(result, 1, align_table.v);
	SET_VECTOR_ELT(result, 2, log_per_record);
	SET_VECTOR_ELT(result, 3, scan_log);
	
	UNPROTECT(fmt.number_of_tiers + 5);

	return(result);
}	

