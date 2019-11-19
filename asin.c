/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 6 "src/asin.y" /* yacc.c:339  */

#include <stdio.h>
#include <string.h>
#include "header.h"
#include "libtds.h"

#line 73 "asin.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "asin.h".  */
#ifndef YY_YY_ASIN_H_INCLUDED
# define YY_YY_ASIN_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    MAS_ = 258,
    MENOS_ = 259,
    POR_ = 260,
    DIV_ = 261,
    MOD_ = 262,
    MASASIG_ = 263,
    MENOSASIG_ = 264,
    PORASIG_ = 265,
    DIVASIG_ = 266,
    INC_ = 267,
    DEC_ = 268,
    ASIG_ = 269,
    ALLA_ = 270,
    CLLA_ = 271,
    APAR_ = 272,
    CPAR_ = 273,
    ACOR_ = 274,
    CCOR_ = 275,
    IGU_ = 276,
    DIST_ = 277,
    MAY_ = 278,
    MEN_ = 279,
    MAYIGU_ = 280,
    MENIGU_ = 281,
    AND_ = 282,
    OR_ = 283,
    NOT_ = 284,
    STRUCT_ = 285,
    INT_ = 286,
    BOOL_ = 287,
    READ_ = 288,
    PRINT_ = 289,
    IF_ = 290,
    ELSE_ = 291,
    WHILE_ = 292,
    TRUE_ = 293,
    FALSE_ = 294,
    DELI_ = 295,
    PUNTO_ = 296,
    ID_ = 297,
    CTE_ = 298
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 13 "src/asin.y" /* yacc.c:355  */
/********************************************************************/
  int   cent;                 /* Para el terminal "cte" entera              */
  char  *ident;               /* Nombre del identificador                   */
  int tipo;                   /* Tipo del simbolo                           */
  struct CamposStruct lisCampos;  
  struct CteStruct  constanteStru;

#line 165 "asin.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_ASIN_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 182 "asin.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   283

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  44
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  31
/* YYNRULES -- Number of rules.  */
#define YYNRULES  78
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  140

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   298

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    44,    44,    44,    52,    53,    57,    58,    63,    72,
      84,   100,   111,   115,   123,   134,   148,   149,   150,   151,
     152,   153,   157,   158,   162,   171,   180,   189,   198,   199,
     203,   207,   220,   240,   264,   268,   280,   284,   297,   301,
     314,   318,   331,   335,   348,   352,   362,   377,   381,   393,
     409,   420,   439,   446,   451,   456,   464,   465,   466,   467,
     468,   472,   473,   477,   478,   482,   483,   484,   485,   489,
     490,   494,   495,   496,   500,   504,   508,   515,   516
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "MAS_", "MENOS_", "POR_", "DIV_", "MOD_",
  "MASASIG_", "MENOSASIG_", "PORASIG_", "DIVASIG_", "INC_", "DEC_",
  "ASIG_", "ALLA_", "CLLA_", "APAR_", "CPAR_", "ACOR_", "CCOR_", "IGU_",
  "DIST_", "MAY_", "MEN_", "MAYIGU_", "MENIGU_", "AND_", "OR_", "NOT_",
  "STRUCT_", "INT_", "BOOL_", "READ_", "PRINT_", "IF_", "ELSE_", "WHILE_",
  "TRUE_", "FALSE_", "DELI_", "PUNTO_", "ID_", "CTE_", "$accept",
  "programa", "$@1", "secuenciaSentencias", "sentencia", "declaracion",
  "tipoSimple", "listaCampos", "instruccion", "listaInstrucciones",
  "instruccionEntradaSalida", "instruccionSeleccion",
  "instruccionIteracion", "instruccionExpresion", "expresion",
  "expresionLogica", "expresionIgualdad", "expresionRelacional",
  "expresionAditiva", "expresionMultiplicativa", "expresionUnaria",
  "expresionSufija", "constante", "operadorAsignacion", "operadorLogico",
  "operadorIgualdad", "operadorRelacional", "operadorAditivo",
  "operadorMultiplicativo", "operadorUnario", "operadorIncremento", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298
};
# endif

#define YYPACT_NINF -94

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-94)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -94,     5,     2,   -94,   100,   -94,   -94,   -94,   133,   232,
     -94,   -94,     7,   -94,   -94,    11,    13,    35,    42,   -94,
     -94,   -94,    -1,   -94,    45,   -94,   -94,    -5,   -94,   -94,
     -94,   -94,   -94,    28,    -2,    20,    41,    67,    49,   -94,
     -94,   -94,   240,    30,   -94,   -94,   166,    55,    58,    39,
     232,   232,   232,   -94,   -94,   -94,   -94,   -94,   232,    44,
     232,   -94,   -94,   -94,    10,   -94,   -94,   -94,   240,   -94,
     -94,   240,   -94,   -94,   -94,   -94,   240,   -94,   -94,   240,
     -94,   -94,   -94,   240,     8,   -94,   -94,   -94,   -94,   -94,
      50,     0,    73,    75,    76,    77,    78,    25,   -94,   -24,
      53,   -94,    20,    41,    67,    49,   -94,   232,    57,    61,
      60,    62,    65,    68,   199,   199,    25,   232,    69,    91,
      94,   -94,   -94,    79,    80,   -94,   -94,    82,   -94,   232,
     -94,   -94,    81,   -94,   -94,   -94,   199,   -94,   -94,   -94
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     0,     1,     0,    74,    77,    78,     0,     0,
      75,    76,     0,    12,    13,     0,     0,     0,     0,    54,
      55,    29,    50,    53,     0,     4,     6,     0,     7,    18,
      19,    20,    21,     0,    30,    34,    36,    38,    40,    42,
      44,    52,     0,     0,    16,    22,     0,     0,     0,     0,
       0,     0,     0,    57,    58,    59,    60,    56,     0,     0,
       0,    48,     3,     5,     0,    28,    61,    62,     0,    63,
      64,     0,    65,    66,    67,    68,     0,    69,    70,     0,
      71,    72,    73,     0,    50,    45,    46,    17,    23,    47,
       0,     0,     0,     0,     0,     0,     0,    51,    31,     0,
       0,     8,    35,    37,    39,    41,    43,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,     0,     0,     0,
       0,    51,    14,     0,     0,    24,    25,     0,    27,     0,
      33,     9,     0,    49,    11,    15,     0,    32,    10,    26
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -94,   -94,   -94,   -94,    92,   -94,   -45,   -94,    -8,   -94,
     -94,   -94,   -94,   -94,    -7,   -94,    29,    52,    51,    46,
     -36,   -94,    27,   -93,   -94,   -94,   -94,   -94,   -94,   -94,
     -21
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     1,     2,    24,    25,    26,    27,    91,    28,    46,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    60,    68,    71,    76,    79,    83,    42,
      43
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      45,    61,    47,    90,   117,     3,    85,    53,    54,    55,
      56,     6,     7,    57,    19,    20,   110,     4,    58,    23,
       6,     7,    48,   129,    99,    66,    67,   107,    49,   100,
      50,    13,    14,    53,    54,    55,    56,    64,    88,    57,
      59,    69,    70,    93,    94,    95,   111,   106,     5,   108,
     101,    96,    51,    98,    80,    81,    82,     6,     7,    52,
       8,    62,     9,    61,    72,    73,    74,    75,    65,    10,
      77,    78,    86,    89,    11,    12,    13,    14,    15,    16,
      17,    92,    18,    19,    20,    21,    97,    22,    23,    13,
      14,   112,   109,   113,   114,   115,   119,   102,   116,   121,
     120,   122,   123,     5,   124,   125,   127,   128,   126,   131,
     130,   132,     6,     7,   133,     8,    63,     9,   136,   134,
     135,   138,   137,   103,    10,   105,   118,   104,   139,    11,
      12,    13,    14,    15,    16,    17,     5,    18,    19,    20,
      21,     0,    22,    23,     0,     6,     7,     0,     8,    44,
       9,     0,     0,     0,     0,     0,     0,    10,     0,     0,
       0,     0,    11,     0,     0,     0,    15,    16,    17,     5,
      18,    19,    20,    21,     0,    22,    23,     0,     6,     7,
       0,     8,    87,     9,     0,     0,     0,     0,     0,     0,
      10,     0,     0,     0,     0,    11,     0,     0,     0,    15,
      16,    17,     5,    18,    19,    20,    21,     0,    22,    23,
       0,     6,     7,     0,     8,     0,     9,     0,     0,     0,
       0,     0,     0,    10,     0,     0,     0,     0,    11,     0,
       0,     0,    15,    16,    17,     5,    18,    19,    20,    21,
       0,    22,    23,     5,     6,     7,     0,     0,     0,     9,
       0,     0,     6,     7,     0,     0,    10,     9,     0,     0,
       0,    11,     0,     0,    10,     0,     0,     0,     0,    11,
      19,    20,     0,     0,    22,    23,     0,     0,    19,    20,
       0,     0,    84,    23
};

static const yytype_int16 yycheck[] =
{
       8,    22,     9,    48,    97,     0,    42,     8,     9,    10,
      11,    12,    13,    14,    38,    39,    16,    15,    19,    43,
      12,    13,    15,   116,    14,    27,    28,    19,    17,    19,
      17,    31,    32,     8,     9,    10,    11,    42,    46,    14,
      41,    21,    22,    50,    51,    52,    91,    83,     3,    41,
      40,    58,    17,    60,     5,     6,     7,    12,    13,    17,
      15,    16,    17,    84,    23,    24,    25,    26,    40,    24,
       3,     4,    42,    18,    29,    30,    31,    32,    33,    34,
      35,    42,    37,    38,    39,    40,    42,    42,    43,    31,
      32,    18,    42,    18,    18,    18,    43,    68,    20,    42,
     107,    40,    42,     3,    42,    40,   114,   115,    40,    40,
     117,    20,    12,    13,    20,    15,    24,    17,    36,    40,
      40,    40,   129,    71,    24,    79,    99,    76,   136,    29,
      30,    31,    32,    33,    34,    35,     3,    37,    38,    39,
      40,    -1,    42,    43,    -1,    12,    13,    -1,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    -1,    -1,    33,    34,    35,     3,
      37,    38,    39,    40,    -1,    42,    43,    -1,    12,    13,
      -1,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,    33,
      34,    35,     3,    37,    38,    39,    40,    -1,    42,    43,
      -1,    12,    13,    -1,    15,    -1,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      -1,    -1,    33,    34,    35,     3,    37,    38,    39,    40,
      -1,    42,    43,     3,    12,    13,    -1,    -1,    -1,    17,
      -1,    -1,    12,    13,    -1,    -1,    24,    17,    -1,    -1,
      -1,    29,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      38,    39,    -1,    -1,    42,    43,    -1,    -1,    38,    39,
      -1,    -1,    42,    43
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    45,    46,     0,    15,     3,    12,    13,    15,    17,
      24,    29,    30,    31,    32,    33,    34,    35,    37,    38,
      39,    40,    42,    43,    47,    48,    49,    50,    52,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    73,    74,    16,    52,    53,    58,    15,    17,
      17,    17,    17,     8,     9,    10,    11,    14,    19,    41,
      67,    74,    16,    48,    42,    40,    27,    28,    68,    21,
      22,    69,    23,    24,    25,    26,    70,     3,     4,    71,
       5,     6,     7,    72,    42,    64,    42,    16,    52,    18,
      50,    51,    42,    58,    58,    58,    58,    42,    58,    14,
      19,    40,    60,    61,    62,    63,    64,    19,    41,    42,
      16,    50,    18,    18,    18,    18,    20,    67,    66,    43,
      58,    42,    40,    42,    42,    40,    40,    52,    52,    67,
      58,    40,    20,    20,    40,    40,    36,    58,    40,    52
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    44,    46,    45,    47,    47,    48,    48,    49,    49,
      49,    49,    50,    50,    51,    51,    52,    52,    52,    52,
      52,    52,    53,    53,    54,    54,    55,    56,    57,    57,
      58,    58,    58,    58,    59,    59,    60,    60,    61,    61,
      62,    62,    63,    63,    64,    64,    64,    65,    65,    65,
      65,    65,    65,    66,    66,    66,    67,    67,    67,    67,
      67,    68,    68,    69,    69,    70,    70,    70,    70,    71,
      71,    72,    72,    72,    73,    73,    73,    74,    74
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     4,     1,     2,     1,     1,     3,     5,
       6,     6,     1,     1,     3,     4,     2,     3,     1,     1,
       1,     1,     1,     2,     5,     5,     7,     5,     2,     1,
       1,     3,     6,     5,     1,     3,     1,     3,     1,     3,
       1,     3,     1,     3,     1,     2,     2,     3,     2,     4,
       1,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 44 "src/asin.y" /* yacc.c:1646  */
    { dvar = 0; }
#line 1390 "asin.c" /* yacc.c:1646  */
    break;

  case 3:
#line 46 "src/asin.y" /* yacc.c:1646  */
    {
      verTdS();
    }
#line 1398 "asin.c" /* yacc.c:1646  */
    break;

  case 8:
#line 64 "src/asin.y" /* yacc.c:1646  */
    {
        if (! insTdS((yyvsp[-1].ident), (yyvsp[-2].tipo), dvar, -1)) {
          yyerror("Identificador repetido");
        } else {
          dvar += TALLA_TIPO_SIMPLE;
        }
      }
#line 1410 "asin.c" /* yacc.c:1646  */
    break;

  case 9:
#line 73 "src/asin.y" /* yacc.c:1646  */
    {
        if ((yyvsp[-4].tipo) != (yyvsp[-1].constanteStru).tipo) {
          yyerror("Tipos incompatibles");
        }
        else if (! insTdS((yyvsp[-3].ident), (yyvsp[-4].tipo), dvar, -1)) {
          yyerror("Identificador repetido");
        } else {
          dvar += TALLA_TIPO_SIMPLE;
        }
      }
#line 1425 "asin.c" /* yacc.c:1646  */
    break;

  case 10:
#line 85 "src/asin.y" /* yacc.c:1646  */
    {
        int numelem = (yyvsp[-2].cent);
        if (numelem <= 0) {
          yyerror("Talla negativa del array");
          numelem = 0;
        } else  {
          int refe = insTdA((yyvsp[-5].tipo), numelem);
          if (! insTdS((yyvsp[-4].ident), T_ARRAY, dvar, refe)) {
            yyerror("Identificador repetido");
          } else {
            dvar += numelem * TALLA_TIPO_SIMPLE;
          }
        }
      }
#line 1444 "asin.c" /* yacc.c:1646  */
    break;

  case 11:
#line 101 "src/asin.y" /* yacc.c:1646  */
    {
        if (! insTdS((yyvsp[-1].ident), T_RECORD, dvar, (yyvsp[-3].lisCampos).ref)) {
          yyerror("Identificador repetido");
        } else {
          dvar += TALLA_TIPO_SIMPLE;
        }
      }
#line 1456 "asin.c" /* yacc.c:1646  */
    break;

  case 12:
#line 112 "src/asin.y" /* yacc.c:1646  */
    {
        (yyval.tipo) = T_ENTERO;
      }
#line 1464 "asin.c" /* yacc.c:1646  */
    break;

  case 13:
#line 116 "src/asin.y" /* yacc.c:1646  */
    {
        (yyval.tipo) = T_LOGICO;
      }
#line 1472 "asin.c" /* yacc.c:1646  */
    break;

  case 14:
#line 124 "src/asin.y" /* yacc.c:1646  */
    {
        (yyval.lisCampos).talla = 1;
        int refe = insTdR(-1, (yyvsp[-1].ident), (yyvsp[-2].tipo), dvar);
        if ( refe == -1) {
          yyerror("Identificador repetido");
        } else {
          (yyval.lisCampos).ref = refe;
          dvar += TALLA_TIPO_SIMPLE;
        }
      }
#line 1487 "asin.c" /* yacc.c:1646  */
    break;

  case 15:
#line 135 "src/asin.y" /* yacc.c:1646  */
    {
        (yyval.lisCampos).talla = (yyvsp[-3].lisCampos).talla + 1;
        int refe = insTdR((yyvsp[-3].lisCampos).ref, (yyvsp[-1].ident), (yyvsp[-2].tipo), dvar);
        if ( refe == -1) {
          yyerror("Identificador repetido");
        } else {
          (yyval.lisCampos).ref = (yyvsp[-3].lisCampos).ref;
          dvar += TALLA_TIPO_SIMPLE;
        }
      }
#line 1502 "asin.c" /* yacc.c:1646  */
    break;

  case 24:
#line 163 "src/asin.y" /* yacc.c:1646  */
    {
      SIMB sim = obtTdS((yyvsp[-2].ident));
      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if (sim.tipo != T_ENTERO) {
        yyerror("El argumento del read debe ser entero");
      }
    }
#line 1515 "asin.c" /* yacc.c:1646  */
    break;

  case 25:
#line 172 "src/asin.y" /* yacc.c:1646  */
    {
      if ((yyvsp[-2].tipo) != T_ENTERO) {
        yyerror("El argumento del print debe ser entero");
      }
    }
#line 1525 "asin.c" /* yacc.c:1646  */
    break;

  case 26:
#line 181 "src/asin.y" /* yacc.c:1646  */
    {
      if ((yyvsp[-4].tipo) != T_LOGICO) {
        yyerror("La condicion debe ser de tipo logica");
      }
    }
#line 1535 "asin.c" /* yacc.c:1646  */
    break;

  case 27:
#line 190 "src/asin.y" /* yacc.c:1646  */
    {
      if ((yyvsp[-2].tipo) != T_LOGICO) {
        yyerror("La condicion deber ser de tipo logica");
      }
    }
#line 1545 "asin.c" /* yacc.c:1646  */
    break;

  case 30:
#line 204 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = (yyvsp[0].tipo);
    }
#line 1553 "asin.c" /* yacc.c:1646  */
    break;

  case 31:
#line 208 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;
      SIMB sim = obtTdS((yyvsp[-2].ident));

      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if (! ((sim.tipo == (yyvsp[0].tipo) == T_ENTERO) || (sim.tipo == (yyvsp[0].tipo) == T_LOGICO))) {
        yyerror("Tipos incompatibles en la instruccion de asignacion");
      } else {
        (yyval.tipo) = sim.tipo;
      }
    }
#line 1570 "asin.c" /* yacc.c:1646  */
    break;

  case 32:
#line 221 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;
      SIMB sim = obtTdS((yyvsp[-5].ident));

      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if ((yyvsp[-3].tipo) != T_ENTERO) {
        yyerror("Incompatibilidad de tipo en el indice del array");
      } else if (sim.tipo != T_ARRAY) {
        yyerror("El objeto no es de tipo array");
      } else {
        DIM dim = obtTdA(sim.ref);
        if (dim.telem != (yyvsp[0].tipo)) {
          yyerror("Incomptabilidad de tipo entre el array y la asignacion");
        } else {
          (yyval.tipo) = dim.telem;
        }
      }
    }
#line 1594 "asin.c" /* yacc.c:1646  */
    break;

  case 33:
#line 241 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;
      SIMB sim = obtTdS((yyvsp[-4].ident));
      CAMP cam;

      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if (sim.tipo != T_RECORD) {
        yyerror("El objeto no es de tipo registro");
      } else {
        cam = obtTdR(sim.ref, (yyvsp[-2].ident));
        if (cam.tipo == T_ERROR) {
          yyerror("No existe el campo del registro");
        } else if (cam.tipo != (yyvsp[0].tipo)) {
          yyerror("Incomptabilidad de tipos entre el campo del registro y la asignacion");
        } else {
          (yyval.tipo) = cam.tipo;
        }
      }
    }
#line 1619 "asin.c" /* yacc.c:1646  */
    break;

  case 34:
#line 265 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = (yyvsp[0].tipo);
    }
#line 1627 "asin.c" /* yacc.c:1646  */
    break;

  case 35:
#line 269 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;
      if (! (((yyvsp[-2].tipo) == T_LOGICO) && ((yyvsp[0].tipo) == T_LOGICO))) {
        yyerror("Los tipos del operador logico son incompatibles, deben ser logicos");
      } else {
        (yyval.tipo) = T_LOGICO;
      }
    }
#line 1640 "asin.c" /* yacc.c:1646  */
    break;

  case 36:
#line 281 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = (yyvsp[0].tipo);
    }
#line 1648 "asin.c" /* yacc.c:1646  */
    break;

  case 37:
#line 285 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;

      if ((yyvsp[-2].tipo) != (yyvsp[0].tipo)) {
        yyerror("Los tipos del operador de igualdad son incompatibles");
      } else {
        (yyval.tipo) = T_LOGICO;
      }
    }
#line 1662 "asin.c" /* yacc.c:1646  */
    break;

  case 38:
#line 298 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = (yyvsp[0].tipo);
    }
#line 1670 "asin.c" /* yacc.c:1646  */
    break;

  case 39:
#line 302 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;

      if (! (((yyvsp[-2].tipo) == T_ENTERO) && ((yyvsp[0].tipo) == T_ENTERO))) {
        yyerror("Los tipos del operador relacional son incompatibles, deben ser enteros");
      } else {
        (yyval.tipo) = T_LOGICO;
      }
    }
#line 1684 "asin.c" /* yacc.c:1646  */
    break;

  case 40:
#line 315 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = (yyvsp[0].tipo);
    }
#line 1692 "asin.c" /* yacc.c:1646  */
    break;

  case 41:
#line 319 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;

      if (! ((yyvsp[-2].tipo) == (yyvsp[0].tipo) == T_ENTERO)) {
        yyerror("Los tipos del operador aditivo son incompatibles, deben ser enteros");
      } else {
        (yyval.tipo) = T_ENTERO;
      }
    }
#line 1706 "asin.c" /* yacc.c:1646  */
    break;

  case 42:
#line 332 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = (yyvsp[0].tipo);
    }
#line 1714 "asin.c" /* yacc.c:1646  */
    break;

  case 43:
#line 336 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;

      if (! ((yyvsp[-2].tipo) == (yyvsp[0].tipo) == T_ENTERO)) {
        yyerror("Los tipos del operador multiplicativo son incompatibles, deben ser enteros");
      } else {
        (yyval.tipo) = T_ENTERO;
      }
    }
#line 1728 "asin.c" /* yacc.c:1646  */
    break;

  case 44:
#line 349 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = (yyvsp[0].tipo);
    }
#line 1736 "asin.c" /* yacc.c:1646  */
    break;

  case 45:
#line 353 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;

      if ((yyvsp[-1].tipo) != (yyvsp[0].tipo)) {
        yyerror("El tipo del operador unario no es compatible");
      } else {
        (yyval.tipo) = (yyvsp[-1].tipo);
      }
    }
#line 1750 "asin.c" /* yacc.c:1646  */
    break;

  case 46:
#line 363 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;
      SIMB sim = obtTdS((yyvsp[0].ident));
      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if (sim.tipo != T_ENTERO) {
        yyerror("Incomptabilidad de tipos, debe ser entero");
      } else {
        (yyval.tipo) = T_ENTERO;
      }
    }
#line 1766 "asin.c" /* yacc.c:1646  */
    break;

  case 47:
#line 378 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = (yyvsp[-1].tipo);
    }
#line 1774 "asin.c" /* yacc.c:1646  */
    break;

  case 48:
#line 382 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;
      SIMB sim = obtTdS((yyvsp[-1].ident));
      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if (sim.tipo != T_ENTERO) {
        yyerror("Incomptabilidad de tipos, debe ser entero");
      } else {
        (yyval.tipo) = T_ENTERO;
      }
    }
#line 1790 "asin.c" /* yacc.c:1646  */
    break;

  case 49:
#line 394 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;
      SIMB sim = obtTdS((yyvsp[-3].ident));

      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if ((yyvsp[-1].tipo) != T_ENTERO) {
        yyerror("Incompatibilidad de tipo en el indice del array");
      } else if (sim.tipo != T_ARRAY) {
        yyerror("El objeto no es de tipo array");
      } else {
        DIM dim = obtTdA(sim.ref);
        (yyval.tipo) = dim.telem;
      }
    }
#line 1810 "asin.c" /* yacc.c:1646  */
    break;

  case 50:
#line 410 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;
      SIMB sim = obtTdS((yyvsp[0].ident));

      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else {
        (yyval.tipo) = sim.tipo;
      }
    }
#line 1825 "asin.c" /* yacc.c:1646  */
    break;

  case 51:
#line 421 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ERROR;
      SIMB sim = obtTdS((yyvsp[-2].ident));
      CAMP cam;

      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if (sim.tipo != T_RECORD) {
        yyerror("El objeto no es de tipo registro");
      } else {
        cam = obtTdR(sim.ref, (yyvsp[0].ident));
        if (cam.tipo == T_ERROR) {
          yyerror("No existe el campo del registro");
        } else {
          (yyval.tipo) = cam.tipo;
        }
      }
    }
#line 1848 "asin.c" /* yacc.c:1646  */
    break;

  case 52:
#line 440 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = (yyvsp[0].constanteStru).tipo;
    }
#line 1856 "asin.c" /* yacc.c:1646  */
    break;

  case 53:
#line 447 "src/asin.y" /* yacc.c:1646  */
    {
        (yyval.constanteStru).tipo = T_ENTERO;
        (yyval.constanteStru).cent = (yyvsp[0].cent);
      }
#line 1865 "asin.c" /* yacc.c:1646  */
    break;

  case 54:
#line 452 "src/asin.y" /* yacc.c:1646  */
    {
        (yyval.constanteStru).tipo = T_LOGICO;
        (yyval.constanteStru).cent = TRUE;
      }
#line 1874 "asin.c" /* yacc.c:1646  */
    break;

  case 55:
#line 457 "src/asin.y" /* yacc.c:1646  */
    {
        (yyval.constanteStru).tipo = T_LOGICO;
        (yyval.constanteStru).cent = FALSE;
      }
#line 1883 "asin.c" /* yacc.c:1646  */
    break;

  case 74:
#line 501 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ENTERO;
    }
#line 1891 "asin.c" /* yacc.c:1646  */
    break;

  case 75:
#line 505 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_ENTERO;
    }
#line 1899 "asin.c" /* yacc.c:1646  */
    break;

  case 76:
#line 509 "src/asin.y" /* yacc.c:1646  */
    {
      (yyval.tipo) = T_LOGICO;
    }
#line 1907 "asin.c" /* yacc.c:1646  */
    break;


#line 1911 "asin.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 519 "src/asin.y" /* yacc.c:1906  */

/*****************************************************************************/


