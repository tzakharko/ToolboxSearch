/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is included that follows the "include" declaration
** in the input grammar file. */
#include <stdio.h>
/* Next is all token values, in a form suitable for use by makeheaders.
** This section will be null unless lemon is run with the -m switch.
*/
/* 
** These constants (all generated automatically by the parser generator)
** specify the various kinds of tokens (terminals) that the parser
** understands. 
**
** Each symbol here is a terminal symbol in the grammar.
*/
/* Make sure the INTERFACE macro is defined.
*/
#ifndef INTERFACE
# define INTERFACE 1
#endif
/* The next thing included is series of defines which control
** various aspects of the generated parser.
**    YYCODETYPE         is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 terminals
**                       and nonterminals.  "int" is used otherwise.
**    YYNOCODE           is a number of type YYCODETYPE which corresponds
**                       to no legal terminal or nonterminal number.  This
**                       number is used to fill in empty slots of the hash 
**                       table.
**    YYFALLBACK         If defined, this indicates that one or more tokens
**                       have fall-back values which should be used if the
**                       original value of the token will not parse.
**    YYACTIONTYPE       is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 rules and
**                       states combined.  "int" is used otherwise.
**    ParseTOKENTYPE     is the data type used for minor tokens given 
**                       directly to the parser from the tokenizer.
**    YYMINORTYPE        is the data type used for all minor tokens.
**                       This is typically a union of many types, one of
**                       which is ParseTOKENTYPE.  The entry in the union
**                       for base tokens is called "yy0".
**    YYSTACKDEPTH       is the maximum depth of the parser's stack.  If
**                       zero the stack is dynamically sized using realloc()
**    ParseARG_SDECL     A static variable declaration for the %extra_argument
**    ParseARG_PDECL     A parameter declaration for the %extra_argument
**    ParseARG_STORE     Code to store %extra_argument into yypParser
**    ParseARG_FETCH     Code to extract %extra_argument from yypParser
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
*/
#define YYCODETYPE unsigned char
#define YYNOCODE 38
#define YYACTIONTYPE unsigned char
#define ParseTOKENTYPE SEXP
typedef union {
  int yyinit;
  ParseTOKENTYPE yy0;
  int yy8;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define ParseARG_SDECL  feedback_t *feedback ;
#define ParseARG_PDECL , feedback_t *feedback 
#define ParseARG_FETCH  feedback_t *feedback  = yypParser->feedback 
#define ParseARG_STORE yypParser->feedback  = feedback 
#define YYNSTATE 51
#define YYNRULE 30
#define YY_NO_ACTION      (YYNSTATE+YYNRULE+2)
#define YY_ACCEPT_ACTION  (YYNSTATE+YYNRULE+1)
#define YY_ERROR_ACTION   (YYNSTATE+YYNRULE)

/* The yyzerominor constant is used to initialize instances of
** YYMINORTYPE objects to zero. */
static const YYMINORTYPE yyzerominor = { 0 };

/* Define the yytestcase() macro to be a no-op if is not already defined
** otherwise.
**
** Applications can choose to define yytestcase() in the %include section
** to a macro that can assist in verifying code coverage.  For production
** code the yytestcase() macro should be turned off.  But it is useful
** for testing.
*/
#ifndef yytestcase
# define yytestcase(X)
#endif


/* Next are the tables used to determine what action to take based on the
** current state and lookahead token.  These tables are used to implement
** functions that take a state number and lookahead value and return an
** action integer.  
**
** Suppose the action integer is N.  Then the action is determined as
** follows
**
**   0 <= N < YYNSTATE                  Shift N.  That is, push the lookahead
**                                      token onto the stack and goto state N.
**
**   YYNSTATE <= N < YYNSTATE+YYNRULE   Reduce by rule N-YYNSTATE.
**
**   N == YYNSTATE+YYNRULE              A syntax error has occurred.
**
**   N == YYNSTATE+YYNRULE+1            The parser accepts its input.
**
**   N == YYNSTATE+YYNRULE+2            No such action.  Denotes unused
**                                      slots in the yy_action[] table.
**
** The action table is constructed as a single large table named yy_action[].
** Given state S and lookahead X, the action is computed as
**
**      yy_action[ yy_shift_ofst[S] + X ]
**
** If the index value yy_shift_ofst[S]+X is out of range or if the value
** yy_lookahead[yy_shift_ofst[S]+X] is not equal to X or if yy_shift_ofst[S]
** is equal to YY_SHIFT_USE_DFLT, it means that the action is not in the table
** and that yy_default[S] should be used instead.  
**
** The formula above is for computing the action when the lookahead is
** a terminal symbol.  If the lookahead is a non-terminal (as occurs after
** a reduce action) then the yy_reduce_ofst[] array is used in place of
** the yy_shift_ofst[] array and YY_REDUCE_USE_DFLT is used in place of
** YY_SHIFT_USE_DFLT.
**
** The following are the tables generated in this section:
**
**  yy_action[]        A single table containing all actions.
**  yy_lookahead[]     A table containing the lookahead for each entry in
**                     yy_action.  Used to detect hash collisions.
**  yy_shift_ofst[]    For each state, the offset into yy_action for
**                     shifting terminals.
**  yy_reduce_ofst[]   For each state, the offset into yy_action for
**                     shifting non-terminals after a reduce.
**  yy_default[]       Default action for each state.
*/
#define YY_ACTTAB_COUNT (116)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */    22,   44,   30,   29,   28,   27,   22,   44,   51,   11,
 /*    10 */    41,   13,   37,   36,    2,   21,    5,   13,   37,   36,
 /*    20 */     2,   17,    5,   22,   44,    8,   82,   42,   43,   23,
 /*    30 */    10,   12,    7,   19,   13,   37,   36,    2,   14,    5,
 /*    40 */    42,   43,   38,   26,   42,   43,   46,   18,    3,   31,
 /*    50 */    83,   45,   42,   43,   38,    9,   11,   83,   42,   43,
 /*    60 */    38,   35,   42,   43,   38,   50,   83,   34,   42,   43,
 /*    70 */    38,   33,   42,   43,   38,    9,   11,   40,   22,   44,
 /*    80 */    83,   32,   42,   43,   39,   83,   25,   49,   47,   16,
 /*    90 */    25,   83,   47,   24,   25,   83,   47,   15,   25,   83,
 /*   100 */    47,   20,   25,   83,   47,   48,   22,   44,   83,    6,
 /*   110 */    83,    4,   83,   83,   83,    1,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */     8,    9,    3,    4,    5,    6,    8,    9,    0,    2,
 /*    10 */    18,   19,   20,   21,   22,    9,   24,   19,   20,   21,
 /*    20 */    22,   23,   24,    8,    9,    7,   26,   27,   28,   29,
 /*    30 */    12,   10,   14,   15,   19,   20,   21,   22,    9,   24,
 /*    40 */    27,   28,   29,   16,   27,   28,   29,   31,   35,   36,
 /*    50 */    37,   34,   27,   28,   29,    1,    2,   37,   27,   28,
 /*    60 */    29,   36,   27,   28,   29,   11,   37,   36,   27,   28,
 /*    70 */    29,   36,   27,   28,   29,    1,    2,   36,    8,    9,
 /*    80 */    37,   36,   27,   28,   29,   37,   30,   13,   32,   33,
 /*    90 */    30,   37,   32,   33,   30,   37,   32,   33,   30,   37,
 /*   100 */    32,   33,   30,   37,   32,   33,    8,    9,   37,   22,
 /*   110 */    37,   24,   37,   37,   37,   17,
};
#define YY_SHIFT_USE_DFLT (-9)
#define YY_SHIFT_COUNT (23)
#define YY_SHIFT_MIN   (-8)
#define YY_SHIFT_MAX   (98)
static const signed char yy_shift_ofst[] = {
 /*     0 */    70,   15,   -2,   -8,   15,   15,   15,   98,   18,   18,
 /*    10 */    18,   18,   18,   70,   -1,   74,   54,   87,   27,   29,
 /*    20 */     7,   21,    6,    8,
};
#define YY_REDUCE_USE_DFLT (-1)
#define YY_REDUCE_COUNT (14)
#define YY_REDUCE_MIN   (0)
#define YY_REDUCE_MAX   (72)
static const signed char yy_reduce_ofst[] = {
 /*     0 */     0,   13,   45,   41,   35,   31,   25,   17,   72,   68,
 /*    10 */    64,   60,   56,   55,   16,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */    81,   81,   81,   81,   81,   81,   81,   81,   81,   81,
 /*    10 */    81,   81,   81,   81,   81,   81,   81,   81,   81,   81,
 /*    20 */    56,   52,   81,   81,   55,   69,   68,   67,   66,   65,
 /*    30 */    64,   71,   79,   78,   80,   77,   76,   75,   74,   73,
 /*    40 */    72,   70,   63,   62,   61,   60,   59,   58,   57,   54,
 /*    50 */    53,
};
#define YY_SZ_ACTTAB (int)(sizeof(yy_action)/sizeof(yy_action[0]))

/* The next table maps tokens into fallback tokens.  If a construct
** like the following:
** 
**      %fallback ID X Y Z.
**
** appears in the grammar, then ID becomes a fallback token for X, Y,
** and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
** but it does not parse, the type of the token is changed to ID and
** the parse is retried before an error is thrown.
*/
#ifdef YYFALLBACK
static const YYCODETYPE yyFallback[] = {
};
#endif /* YYFALLBACK */

/* The following structure represents a single element of the
** parser's stack.  Information stored includes:
**
**   +  The state number for the parser at this level of the stack.
**
**   +  The value of the token stored at this level of the stack.
**      (In other words, the "major" token.)
**
**   +  The semantic value stored at this level of the stack.  This is
**      the information used by the action routines in the grammar.
**      It is sometimes called the "minor" token.
*/
struct yyStackEntry {
  YYACTIONTYPE stateno;  /* The state-number */
  YYCODETYPE major;      /* The major token value.  This is the code
                         ** number for the token at this stack level */
  YYMINORTYPE minor;     /* The user-supplied minor token value.  This
                         ** is the value of the token  */
};
typedef struct yyStackEntry yyStackEntry;

/* The state of the parser is completely contained in an instance of
** the following structure */
struct yyParser {
  int yyidx;                    /* Index of top element in stack */
#ifdef YYTRACKMAXSTACKDEPTH
  int yyidxMax;                 /* Maximum value of yyidx */
#endif
  int yyerrcnt;                 /* Shifts left before out of the error */
  ParseARG_SDECL                /* A place to hold %extra_argument */
#if YYSTACKDEPTH<=0
  int yystksz;                  /* Current side of the stack */
  yyStackEntry *yystack;        /* The parser's stack */
#else
  yyStackEntry yystack[YYSTACKDEPTH];  /* The parser's stack */
#endif
};
typedef struct yyParser yyParser;

#ifndef NDEBUG
#include <stdio.h>
static FILE *yyTraceFILE = 0;
static char *yyTracePrompt = 0;
#endif /* NDEBUG */

#ifndef NDEBUG
/* 
** Turn parser tracing on by giving a stream to which to write the trace
** and a prompt to preface each trace message.  Tracing is turned off
** by making either argument NULL 
**
** Inputs:
** <ul>
** <li> A FILE* to which trace output should be written.
**      If NULL, then tracing is turned off.
** <li> A prefix string written at the beginning of every
**      line of trace output.  If NULL, then tracing is
**      turned off.
** </ul>
**
** Outputs:
** None.
*/
void ParseTrace(FILE *TraceFILE, char *zTracePrompt){
  yyTraceFILE = TraceFILE;
  yyTracePrompt = zTracePrompt;
  if( yyTraceFILE==0 ) yyTracePrompt = 0;
  else if( yyTracePrompt==0 ) yyTraceFILE = 0;
}
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing shifts, the names of all terminals and nonterminals
** are required.  The following table supplies these names */
static const char *const yyTokenName[] = { 
  "$",             "OR",            "AND",           "EQ",          
  "NEQ",           "REGEX",         "NREGEX",        "NOT",         
  "AT",            "LITERAL",       "LCURVBR",       "RCURVBR",     
  "LPAREN",        "RPAREN",        "CONTAINS",      "DOLLAR",      
  "STRING",        "LSQBR",         "RSQBR",         "EXCL",        
  "ANY",           "BR",            "NUMBER",        "SEMICOLON",   
  "STAR",          "error",         "decl_query",    "reference_obj",
  "reference_var",  "reference",     "pred_annotation",  "operator",    
  "predicate",     "eval",          "topo_pattern",  "topo_pattern_seq",
  "topo_match",  
};
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing reduce actions, the names of all rules are required.
*/
static const char *const yyRuleName[] = {
 /*   0 */ "decl_query ::= reference",
 /*   1 */ "reference_obj ::= AT LITERAL",
 /*   2 */ "reference_obj ::= AT LITERAL LCURVBR eval RCURVBR",
 /*   3 */ "eval ::= LPAREN eval RPAREN",
 /*   4 */ "eval ::= eval AND eval",
 /*   5 */ "eval ::= eval OR eval",
 /*   6 */ "eval ::= NOT eval",
 /*   7 */ "eval ::= predicate",
 /*   8 */ "eval ::= CONTAINS reference",
 /*   9 */ "eval ::= CONTAINS topo_pattern",
 /*  10 */ "reference_var ::= LITERAL",
 /*  11 */ "reference ::= reference_var",
 /*  12 */ "reference ::= reference_obj",
 /*  13 */ "operator ::= EQ",
 /*  14 */ "operator ::= NEQ",
 /*  15 */ "operator ::= REGEX",
 /*  16 */ "operator ::= NREGEX",
 /*  17 */ "pred_annotation ::= DOLLAR LITERAL operator STRING",
 /*  18 */ "predicate ::= pred_annotation",
 /*  19 */ "topo_pattern ::= LSQBR topo_pattern_seq RSQBR",
 /*  20 */ "topo_pattern_seq ::= topo_match",
 /*  21 */ "topo_pattern_seq ::= topo_pattern_seq topo_match",
 /*  22 */ "topo_match ::= EXCL reference",
 /*  23 */ "topo_match ::= reference",
 /*  24 */ "topo_match ::= ANY",
 /*  25 */ "topo_match ::= BR",
 /*  26 */ "topo_match ::= NUMBER SEMICOLON NUMBER topo_match",
 /*  27 */ "topo_match ::= NUMBER SEMICOLON STAR topo_match",
 /*  28 */ "topo_match ::= NUMBER topo_match",
 /*  29 */ "topo_match ::= STAR topo_match",
};
#endif /* NDEBUG */


#if YYSTACKDEPTH<=0
/*
** Try to increase the size of the parser stack.
*/
static void yyGrowStack(yyParser *p){
  int newSize;
  yyStackEntry *pNew;

  newSize = p->yystksz*2 + 100;
  pNew = realloc(p->yystack, newSize*sizeof(pNew[0]));
  if( pNew ){
    p->yystack = pNew;
    p->yystksz = newSize;
#ifndef NDEBUG
    if( yyTraceFILE ){
      fprintf(yyTraceFILE,"%sStack grows to %d entries!\n",
              yyTracePrompt, p->yystksz);
    }
#endif
  }
}
#endif

/* 
** This function allocates a new parser.
** The only argument is a pointer to a function which works like
** malloc.
**
** Inputs:
** A pointer to the function used to allocate memory.
**
** Outputs:
** A pointer to a parser.  This pointer is used in subsequent calls
** to Parse and ParseFree.
*/
void *ParseAlloc(void *(*mallocProc)(size_t)){
  yyParser *pParser;
  pParser = (yyParser*)(*mallocProc)( (size_t)sizeof(yyParser) );
  if( pParser ){
    pParser->yyidx = -1;
#ifdef YYTRACKMAXSTACKDEPTH
    pParser->yyidxMax = 0;
#endif
#if YYSTACKDEPTH<=0
    pParser->yystack = NULL;
    pParser->yystksz = 0;
    yyGrowStack(pParser);
#endif
  }
  return pParser;
}

/* The following function deletes the value associated with a
** symbol.  The symbol can be either a terminal or nonterminal.
** "yymajor" is the symbol code, and "yypminor" is a pointer to
** the value.
*/
static void yy_destructor(
  yyParser *yypParser,    /* The parser */
  YYCODETYPE yymajor,     /* Type code for object to destroy */
  YYMINORTYPE *yypminor   /* The object to be destroyed */
){
  ParseARG_FETCH;
  switch( yymajor ){
    /* Here is inserted the actions which take place when a
    ** terminal or non-terminal is destroyed.  This can happen
    ** when the symbol is popped from the stack during a
    ** reduce or during error processing or when a parser is 
    ** being destroyed before it is finished parsing.
    **
    ** Note: during a reduce, the only symbols destroyed are those
    ** which appear on the RHS of the rule, but which are not used
    ** inside the C code.
    */
    default:  break;   /* If no destructor action specified: do nothing */
  }
}

/*
** Pop the parser's stack once.
**
** If there is a destructor routine associated with the token which
** is popped from the stack, then call it.
**
** Return the major token number for the symbol popped.
*/
static int yy_pop_parser_stack(yyParser *pParser){
  YYCODETYPE yymajor;
  yyStackEntry *yytos = &pParser->yystack[pParser->yyidx];

  if( pParser->yyidx<0 ) return 0;
#ifndef NDEBUG
  if( yyTraceFILE && pParser->yyidx>=0 ){
    fprintf(yyTraceFILE,"%sPopping %s\n",
      yyTracePrompt,
      yyTokenName[yytos->major]);
  }
#endif
  yymajor = yytos->major;
  yy_destructor(pParser, yymajor, &yytos->minor);
  pParser->yyidx--;
  return yymajor;
}

/* 
** Deallocate and destroy a parser.  Destructors are all called for
** all stack elements before shutting the parser down.
**
** Inputs:
** <ul>
** <li>  A pointer to the parser.  This should be a pointer
**       obtained from ParseAlloc.
** <li>  A pointer to a function used to reclaim memory obtained
**       from malloc.
** </ul>
*/
void ParseFree(
  void *p,                    /* The parser to be deleted */
  void (*freeProc)(void*)     /* Function used to reclaim memory */
){
  yyParser *pParser = (yyParser*)p;
  if( pParser==0 ) return;
  while( pParser->yyidx>=0 ) yy_pop_parser_stack(pParser);
#if YYSTACKDEPTH<=0
  free(pParser->yystack);
#endif
  (*freeProc)((void*)pParser);
}

/*
** Return the peak depth of the stack for a parser.
*/
#ifdef YYTRACKMAXSTACKDEPTH
int ParseStackPeak(void *p){
  yyParser *pParser = (yyParser*)p;
  return pParser->yyidxMax;
}
#endif

/*
** Find the appropriate action for a parser given the terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_shift_action(
  yyParser *pParser,        /* The parser */
  YYCODETYPE iLookAhead     /* The look-ahead token */
){
  int i;
  int stateno = pParser->yystack[pParser->yyidx].stateno;
 
  if( stateno>YY_SHIFT_MAX || (i = yy_shift_ofst[stateno])==YY_SHIFT_USE_DFLT ){
    return yy_default[stateno];
  }
  assert( iLookAhead!=YYNOCODE );
  i += iLookAhead;
  if( i<0 || i>=YY_SZ_ACTTAB || yy_lookahead[i]!=iLookAhead ){
    if( iLookAhead>0 ){
#ifdef YYFALLBACK
      YYCODETYPE iFallback;            /* Fallback token */
      if( iLookAhead<sizeof(yyFallback)/sizeof(yyFallback[0])
             && (iFallback = yyFallback[iLookAhead])!=0 ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE, "%sFALLBACK %s => %s\n",
             yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[iFallback]);
        }
#endif
        return yy_find_shift_action(pParser, iFallback);
      }
#endif
#ifdef YYWILDCARD
      {
        int j = i - iLookAhead + YYWILDCARD;
        if( j>=0 && j<YY_SZ_ACTTAB && yy_lookahead[j]==YYWILDCARD ){
#ifndef NDEBUG
          if( yyTraceFILE ){
            fprintf(yyTraceFILE, "%sWILDCARD %s => %s\n",
               yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[YYWILDCARD]);
          }
#endif /* NDEBUG */
          return yy_action[j];
        }
      }
#endif /* YYWILDCARD */
    }
    return yy_default[stateno];
  }else{
    return yy_action[i];
  }
}

/*
** Find the appropriate action for a parser given the non-terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_reduce_action(
  int stateno,              /* Current state number */
  YYCODETYPE iLookAhead     /* The look-ahead token */
){
  int i;
#ifdef YYERRORSYMBOL
  if( stateno>YY_REDUCE_MAX ){
    return yy_default[stateno];
  }
#else
  assert( stateno<=YY_REDUCE_MAX );
#endif
  i = yy_reduce_ofst[stateno];
  assert( i!=YY_REDUCE_USE_DFLT );
  assert( iLookAhead!=YYNOCODE );
  i += iLookAhead;
#ifdef YYERRORSYMBOL
  if( i<0 || i>=YY_SZ_ACTTAB || yy_lookahead[i]!=iLookAhead ){
    return yy_default[stateno];
  }
#else
  assert( i>=0 && i<YY_SZ_ACTTAB );
  assert( yy_lookahead[i]==iLookAhead );
#endif
  return yy_action[i];
}

/*
** The following routine is called if the stack overflows.
*/
static void yyStackOverflow(yyParser *yypParser, YYMINORTYPE *yypMinor){
   ParseARG_FETCH;
   yypParser->yyidx--;
#ifndef NDEBUG
   if( yyTraceFILE ){
     fprintf(yyTraceFILE,"%sStack Overflow!\n",yyTracePrompt);
   }
#endif
   while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
   /* Here code is inserted which will execute if the parser
   ** stack every overflows */
   ParseARG_STORE; /* Suppress warning about unused %extra_argument var */
}

/*
** Perform a shift action.
*/
static void yy_shift(
  yyParser *yypParser,          /* The parser to be shifted */
  int yyNewState,               /* The new state to shift in */
  int yyMajor,                  /* The major token to shift in */
  YYMINORTYPE *yypMinor         /* Pointer to the minor token to shift in */
){
  yyStackEntry *yytos;
  yypParser->yyidx++;
#ifdef YYTRACKMAXSTACKDEPTH
  if( yypParser->yyidx>yypParser->yyidxMax ){
    yypParser->yyidxMax = yypParser->yyidx;
  }
#endif
#if YYSTACKDEPTH>0 
  if( yypParser->yyidx>=YYSTACKDEPTH ){
    yyStackOverflow(yypParser, yypMinor);
    return;
  }
#else
  if( yypParser->yyidx>=yypParser->yystksz ){
    yyGrowStack(yypParser);
    if( yypParser->yyidx>=yypParser->yystksz ){
      yyStackOverflow(yypParser, yypMinor);
      return;
    }
  }
#endif
  yytos = &yypParser->yystack[yypParser->yyidx];
  yytos->stateno = (YYACTIONTYPE)yyNewState;
  yytos->major = (YYCODETYPE)yyMajor;
  yytos->minor = *yypMinor;
#ifndef NDEBUG
  if( yyTraceFILE && yypParser->yyidx>0 ){
    int i;
    fprintf(yyTraceFILE,"%sShift %d\n",yyTracePrompt,yyNewState);
    fprintf(yyTraceFILE,"%sStack:",yyTracePrompt);
    for(i=1; i<=yypParser->yyidx; i++)
      fprintf(yyTraceFILE," %s",yyTokenName[yypParser->yystack[i].major]);
    fprintf(yyTraceFILE,"\n");
  }
#endif
}

/* The following table contains information about every rule that
** is used during the reduce.
*/
static const struct {
  YYCODETYPE lhs;         /* Symbol on the left-hand side of the rule */
  unsigned char nrhs;     /* Number of right-hand side symbols in the rule */
} yyRuleInfo[] = {
  { 26, 1 },
  { 27, 2 },
  { 27, 5 },
  { 33, 3 },
  { 33, 3 },
  { 33, 3 },
  { 33, 2 },
  { 33, 1 },
  { 33, 2 },
  { 33, 2 },
  { 28, 1 },
  { 29, 1 },
  { 29, 1 },
  { 31, 1 },
  { 31, 1 },
  { 31, 1 },
  { 31, 1 },
  { 30, 4 },
  { 32, 1 },
  { 34, 3 },
  { 35, 1 },
  { 35, 2 },
  { 36, 2 },
  { 36, 1 },
  { 36, 1 },
  { 36, 1 },
  { 36, 4 },
  { 36, 4 },
  { 36, 2 },
  { 36, 2 },
};

static void yy_accept(yyParser*);  /* Forward Declaration */

/*
** Perform a reduce action and the shift that must immediately
** follow the reduce.
*/
static void yy_reduce(
  yyParser *yypParser,         /* The parser */
  int yyruleno                 /* Number of the rule by which to reduce */
){
  int yygoto;                     /* The next state */
  int yyact;                      /* The next action */
  YYMINORTYPE yygotominor;        /* The LHS of the rule reduced */
  yyStackEntry *yymsp;            /* The top of the parser's stack */
  int yysize;                     /* Amount to pop the stack */
  ParseARG_FETCH;
  yymsp = &yypParser->yystack[yypParser->yyidx];
#ifndef NDEBUG
  if( yyTraceFILE && yyruleno>=0 
        && yyruleno<(int)(sizeof(yyRuleName)/sizeof(yyRuleName[0])) ){
    fprintf(yyTraceFILE, "%sReduce [%s].\n", yyTracePrompt,
      yyRuleName[yyruleno]);
  }
#endif /* NDEBUG */

  /* Silence complaints from purify about yygotominor being uninitialized
  ** in some cases when it is copied into the stack after the following
  ** switch.  yygotominor is uninitialized when a rule reduces that does
  ** not set the value of its left-hand side nonterminal.  Leaving the
  ** value of the nonterminal uninitialized is utterly harmless as long
  ** as the value is never used.  So really the only thing this code
  ** accomplishes is to quieten purify.  
  **
  ** 2007-01-16:  The wireshark project (www.wireshark.org) reports that
  ** without this code, their parser segfaults.  I'm not sure what there
  ** parser is doing to make this happen.  This is the second bug report
  ** from wireshark this week.  Clearly they are stressing Lemon in ways
  ** that it has not been previously stressed...  (SQLite ticket #2172)
  */
  /*memset(&yygotominor, 0, sizeof(yygotominor));*/
  yygotominor = yyzerominor;


  switch( yyruleno ){
  /* Beginning here are the reduction cases.  A typical example
  ** follows:
  **   case 0:
  **  #line <lineno> <grammarfile>
  **     { ... }           // User supplied code
  **  #line <lineno> <thisfile>
  **     break;
  */
      case 0: /* decl_query ::= reference */
#line 52 "parser.y"
{
	PROTECT(yygotominor.yy0 =  allocVector(INTSXP, 1));
	INTEGER(yygotominor.yy0)[0] = yymsp[0].minor.yy8;
	set_class(yygotominor.yy0, "node.DECL.QUERY");
	feedback->root = yygotominor.yy0;
}
#line 732 "parser.c"
        break;
      case 1: /* reference_obj ::= AT LITERAL */
#line 59 "parser.y"
{
	yygotominor.yy0 = constructNode_REFERENCE_OBJ(yymsp[0].minor.yy0, R_NilValue);
}
#line 739 "parser.c"
        break;
      case 2: /* reference_obj ::= AT LITERAL LCURVBR eval RCURVBR */
#line 63 "parser.y"
{
	yygotominor.yy0 = constructNode_REFERENCE_OBJ(yymsp[-3].minor.yy0, deMorgan_transform(yymsp[-1].minor.yy0));
}
#line 746 "parser.c"
        break;
      case 3: /* eval ::= LPAREN eval RPAREN */
#line 67 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing (exp)\n"); 
#endif		
	yygotominor.yy0 = yymsp[-1].minor.yy0; 
}
#line 756 "parser.c"
        break;
      case 4: /* eval ::= eval AND eval */
#line 74 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing and\n"); 
#endif		
	yygotominor.yy0 = constructNode_and(yymsp[-2].minor.yy0, yymsp[0].minor.yy0); 
}
#line 766 "parser.c"
        break;
      case 5: /* eval ::= eval OR eval */
#line 81 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing or\n"); 
#endif		
	yygotominor.yy0 = constructNode_or(yymsp[-2].minor.yy0, yymsp[0].minor.yy0); 
}
#line 776 "parser.c"
        break;
      case 6: /* eval ::= NOT eval */
#line 89 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing not\n"); 
#endif		
	yygotominor.yy0 = constructNode_not(yymsp[0].minor.yy0); 
}
#line 786 "parser.c"
        break;
      case 7: /* eval ::= predicate */
#line 97 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing filter = contains\n"); 
#endif		
	yygotominor.yy0 = constructNode_EVAL_PREDICATE(yymsp[0].minor.yy8); 
}
#line 796 "parser.c"
        break;
      case 8: /* eval ::= CONTAINS reference */
#line 104 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing filter = contains\n"); 
#endif		
	yygotominor.yy0 = constructNode_EVAL_CONTAINS_OBJECT(yymsp[0].minor.yy8); 
}
#line 806 "parser.c"
        break;
      case 9: /* eval ::= CONTAINS topo_pattern */
#line 111 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing filter = contains\n"); 
#endif		
	yygotominor.yy0 = constructNode_EVAL_CONTAINS_TOPO(install_node(&(feedback->refTable), yymsp[0].minor.yy0)); 
}
#line 816 "parser.c"
        break;
      case 10: /* reference_var ::= LITERAL */
#line 118 "parser.y"
{
	error("Variables are not supported in this version (variable %s referenced)\n", CHAR(STRING_ELT(yymsp[0].minor.yy0, 0)));
	
	yygotominor.yy0 = constructNode_REFERENCE_VAR(yymsp[0].minor.yy0);
}
#line 825 "parser.c"
        break;
      case 11: /* reference ::= reference_var */
      case 12: /* reference ::= reference_obj */ yytestcase(yyruleno==12);
#line 124 "parser.y"
{
	yygotominor.yy8 = install_node(&(feedback->refTable), yymsp[0].minor.yy0);
}
#line 833 "parser.c"
        break;
      case 13: /* operator ::= EQ */
#line 133 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing ==\n"); 
#endif		
	yygotominor.yy0 = make_str(mkChar("==")); 
}
#line 843 "parser.c"
        break;
      case 14: /* operator ::= NEQ */
#line 140 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing !=\n"); 
#endif		
	yygotominor.yy0 = make_str(mkChar("!=")); 
}
#line 853 "parser.c"
        break;
      case 15: /* operator ::= REGEX */
#line 147 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("=~\n"); 
#endif		
	yygotominor.yy0 = make_str(mkChar("=~")); 
}
#line 863 "parser.c"
        break;
      case 16: /* operator ::= NREGEX */
#line 154 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("!=\n"); 
#endif		
	yygotominor.yy0 = make_str(mkChar("!~")); 
}
#line 873 "parser.c"
        break;
      case 17: /* pred_annotation ::= DOLLAR LITERAL operator STRING */
#line 161 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing annotation match\n"); 
#endif		
	yygotominor.yy0 = constructNode_PREDICATE_ANNOTATION(yymsp[-2].minor.yy0, yymsp[-1].minor.yy0, yymsp[0].minor.yy0); 
}
#line 883 "parser.c"
        break;
      case 18: /* predicate ::= pred_annotation */
#line 168 "parser.y"
{
	yygotominor.yy8 = install_node(&(feedback->predTable), yymsp[0].minor.yy0);
}
#line 890 "parser.c"
        break;
      case 19: /* topo_pattern ::= LSQBR topo_pattern_seq RSQBR */
#line 172 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing pattern = items\n"); 
#endif	
	yygotominor.yy0 = yymsp[-1].minor.yy0; 
}
#line 900 "parser.c"
        break;
      case 20: /* topo_pattern_seq ::= topo_match */
#line 179 "parser.y"
{
	yygotominor.yy0 = constructNode_TOPO_PATTERN(R_NilValue, yymsp[0].minor.yy0);
}
#line 907 "parser.c"
        break;
      case 21: /* topo_pattern_seq ::= topo_pattern_seq topo_match */
#line 183 "parser.y"
{
	yygotominor.yy0 = constructNode_TOPO_PATTERN(yymsp[-1].minor.yy0, yymsp[0].minor.yy0);
}
#line 914 "parser.c"
        break;
      case 22: /* topo_match ::= EXCL reference */
#line 188 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing negative match\n"); 
#endif		
	yygotominor.yy0 = constructNode_TOPO_MATCH_INV(yymsp[0].minor.yy8); 
}
#line 924 "parser.c"
        break;
      case 23: /* topo_match ::= reference */
#line 195 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing negative match\n"); 
#endif		
	yygotominor.yy0 = constructNode_TOPO_MATCH(yymsp[0].minor.yy8); 
}
#line 934 "parser.c"
        break;
      case 24: /* topo_match ::= ANY */
#line 202 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing negative match\n"); 
#endif		
	yygotominor.yy0 = constructNode_TOPO_MATCH_ANY(); 
}
#line 944 "parser.c"
        break;
      case 25: /* topo_match ::= BR */
#line 209 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing negative match\n"); 
#endif		
	yygotominor.yy0 = constructNode_TOPO_MATCH_BR(); 
}
#line 954 "parser.c"
        break;
      case 26: /* topo_match ::= NUMBER SEMICOLON NUMBER topo_match */
#line 217 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing min:max match\n"); 
#endif		
	UNPROTECT_PTR(yymsp[-3].minor.yy0);
	UNPROTECT_PTR(yymsp[-1].minor.yy0);
	yygotominor.yy0 = makeNode_TOPO_MATCH_REPEAT(yymsp[0].minor.yy0, atoi(CHAR(STRING_ELT(yymsp[-3].minor.yy0, 0))), atoi(CHAR(STRING_ELT(yymsp[-1].minor.yy0, 0)))); 
}
#line 966 "parser.c"
        break;
      case 27: /* topo_match ::= NUMBER SEMICOLON STAR topo_match */
#line 226 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing min:* match\n"); 
#endif		
	UNPROTECT_PTR(yymsp[-3].minor.yy0);
	yygotominor.yy0 = makeNode_TOPO_MATCH_REPEAT(yymsp[0].minor.yy0, atoi(CHAR(STRING_ELT(yymsp[-3].minor.yy0, 0))), -1); 
}
#line 977 "parser.c"
        break;
      case 28: /* topo_match ::= NUMBER topo_match */
#line 234 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing n match\n"); 
#endif		
	UNPROTECT_PTR(yymsp[-1].minor.yy0);
	yygotominor.yy0 = makeNode_TOPO_MATCH_REPEAT(yymsp[0].minor.yy0, atoi(CHAR(STRING_ELT(yymsp[-1].minor.yy0, 0))), atoi(CHAR(STRING_ELT(yymsp[-1].minor.yy0, 0)))); 
}
#line 988 "parser.c"
        break;
      case 29: /* topo_match ::= STAR topo_match */
#line 242 "parser.y"
{ 
#ifdef DEBUG
	Rprintf("Doing * match\n"); 
#endif		
	yygotominor.yy0 = makeNode_TOPO_MATCH_REPEAT(yymsp[0].minor.yy0, 0, -1); 
}
#line 998 "parser.c"
        break;
      default:
        break;
  };
  yygoto = yyRuleInfo[yyruleno].lhs;
  yysize = yyRuleInfo[yyruleno].nrhs;
  yypParser->yyidx -= yysize;
  yyact = yy_find_reduce_action(yymsp[-yysize].stateno,(YYCODETYPE)yygoto);
  if( yyact < YYNSTATE ){
#ifdef NDEBUG
    /* If we are not debugging and the reduce action popped at least
    ** one element off the stack, then we can push the new element back
    ** onto the stack here, and skip the stack overflow test in yy_shift().
    ** That gives a significant speed improvement. */
    if( yysize ){
      yypParser->yyidx++;
      yymsp -= yysize-1;
      yymsp->stateno = (YYACTIONTYPE)yyact;
      yymsp->major = (YYCODETYPE)yygoto;
      yymsp->minor = yygotominor;
    }else
#endif
    {
      yy_shift(yypParser,yyact,yygoto,&yygotominor);
    }
  }else{
    assert( yyact == YYNSTATE + YYNRULE + 1 );
    yy_accept(yypParser);
  }
}

/*
** The following code executes when the parse fails
*/
#ifndef YYNOERRORRECOVERY
static void yy_parse_failed(
  yyParser *yypParser           /* The parser */
){
  ParseARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sFail!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser fails */
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}
#endif /* YYNOERRORRECOVERY */

/*
** The following code executes when a syntax error first occurs.
*/
static void yy_syntax_error(
  yyParser *yypParser,           /* The parser */
  int yymajor,                   /* The major type of the error token */
  YYMINORTYPE yyminor            /* The minor type of the error token */
){
  ParseARG_FETCH;
#define TOKEN (yyminor.yy0)
#line 29 "parser.y"
  
	feedback->error = 1; 
#line 1063 "parser.c"
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following is executed when the parser accepts
*/
static void yy_accept(
  yyParser *yypParser           /* The parser */
){
  ParseARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sAccept!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser accepts */
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** "ParseAlloc" which describes the current state of the parser.
** The second argument is the major token number.  The third is
** the minor token.  The fourth optional argument is whatever the
** user wants (and specified in the grammar) and is available for
** use by the action routines.
**
** Inputs:
** <ul>
** <li> A pointer to the parser (an opaque structure.)
** <li> The major token number.
** <li> The minor token number.
** <li> An option argument of a grammar-specified type.
** </ul>
**
** Outputs:
** None.
*/
void Parse(
  void *yyp,                   /* The parser */
  int yymajor,                 /* The major token code number */
  ParseTOKENTYPE yyminor       /* The value for the token */
  ParseARG_PDECL               /* Optional %extra_argument parameter */
){
  YYMINORTYPE yyminorunion;
  int yyact;            /* The parser action. */
  int yyendofinput;     /* True if we are at the end of input */
#ifdef YYERRORSYMBOL
  int yyerrorhit = 0;   /* True if yymajor has invoked an error */
#endif
  yyParser *yypParser;  /* The parser */

  /* (re)initialize the parser, if necessary */
  yypParser = (yyParser*)yyp;
  if( yypParser->yyidx<0 ){
#if YYSTACKDEPTH<=0
    if( yypParser->yystksz <=0 ){
      /*memset(&yyminorunion, 0, sizeof(yyminorunion));*/
      yyminorunion = yyzerominor;
      yyStackOverflow(yypParser, &yyminorunion);
      return;
    }
#endif
    yypParser->yyidx = 0;
    yypParser->yyerrcnt = -1;
    yypParser->yystack[0].stateno = 0;
    yypParser->yystack[0].major = 0;
  }
  yyminorunion.yy0 = yyminor;
  yyendofinput = (yymajor==0);
  ParseARG_STORE;

#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sInput %s\n",yyTracePrompt,yyTokenName[yymajor]);
  }
#endif

  do{
    yyact = yy_find_shift_action(yypParser,(YYCODETYPE)yymajor);
    if( yyact<YYNSTATE ){
      assert( !yyendofinput );  /* Impossible to shift the $ token */
      yy_shift(yypParser,yyact,yymajor,&yyminorunion);
      yypParser->yyerrcnt--;
      yymajor = YYNOCODE;
    }else if( yyact < YYNSTATE + YYNRULE ){
      yy_reduce(yypParser,yyact-YYNSTATE);
    }else{
      assert( yyact == YY_ERROR_ACTION );
#ifdef YYERRORSYMBOL
      int yymx;
#endif
#ifndef NDEBUG
      if( yyTraceFILE ){
        fprintf(yyTraceFILE,"%sSyntax Error!\n",yyTracePrompt);
      }
#endif
#ifdef YYERRORSYMBOL
      /* A syntax error has occurred.
      ** The response to an error depends upon whether or not the
      ** grammar defines an error token "ERROR".  
      **
      ** This is what we do if the grammar does define ERROR:
      **
      **  * Call the %syntax_error function.
      **
      **  * Begin popping the stack until we enter a state where
      **    it is legal to shift the error symbol, then shift
      **    the error symbol.
      **
      **  * Set the error count to three.
      **
      **  * Begin accepting and shifting new tokens.  No new error
      **    processing will occur until three tokens have been
      **    shifted successfully.
      **
      */
      if( yypParser->yyerrcnt<0 ){
        yy_syntax_error(yypParser,yymajor,yyminorunion);
      }
      yymx = yypParser->yystack[yypParser->yyidx].major;
      if( yymx==YYERRORSYMBOL || yyerrorhit ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE,"%sDiscard input token %s\n",
             yyTracePrompt,yyTokenName[yymajor]);
        }
#endif
        yy_destructor(yypParser, (YYCODETYPE)yymajor,&yyminorunion);
        yymajor = YYNOCODE;
      }else{
         while(
          yypParser->yyidx >= 0 &&
          yymx != YYERRORSYMBOL &&
          (yyact = yy_find_reduce_action(
                        yypParser->yystack[yypParser->yyidx].stateno,
                        YYERRORSYMBOL)) >= YYNSTATE
        ){
          yy_pop_parser_stack(yypParser);
        }
        if( yypParser->yyidx < 0 || yymajor==0 ){
          yy_destructor(yypParser,(YYCODETYPE)yymajor,&yyminorunion);
          yy_parse_failed(yypParser);
          yymajor = YYNOCODE;
        }else if( yymx!=YYERRORSYMBOL ){
          YYMINORTYPE u2;
          u2.YYERRSYMDT = 0;
          yy_shift(yypParser,yyact,YYERRORSYMBOL,&u2);
        }
      }
      yypParser->yyerrcnt = 3;
      yyerrorhit = 1;
#elif defined(YYNOERRORRECOVERY)
      /* If the YYNOERRORRECOVERY macro is defined, then do not attempt to
      ** do any kind of error recovery.  Instead, simply invoke the syntax
      ** error routine and continue going as if nothing had happened.
      **
      ** Applications can set this macro (for example inside %include) if
      ** they intend to abandon the parse upon the first syntax error seen.
      */
      yy_syntax_error(yypParser,yymajor,yyminorunion);
      yy_destructor(yypParser,(YYCODETYPE)yymajor,&yyminorunion);
      yymajor = YYNOCODE;
      
#else  /* YYERRORSYMBOL is not defined */
      /* This is what we do if the grammar does not define ERROR:
      **
      **  * Report an error message, and throw away the input token.
      **
      **  * If the input token is $, then fail the parse.
      **
      ** As before, subsequent error messages are suppressed until
      ** three input tokens have been successfully shifted.
      */
      if( yypParser->yyerrcnt<=0 ){
        yy_syntax_error(yypParser,yymajor,yyminorunion);
      }
      yypParser->yyerrcnt = 3;
      yy_destructor(yypParser,(YYCODETYPE)yymajor,&yyminorunion);
      if( yyendofinput ){
        yy_parse_failed(yypParser);
      }
      yymajor = YYNOCODE;
#endif
    }
  }while( yymajor!=YYNOCODE && yypParser->yyidx>=0 );
  return;
}
