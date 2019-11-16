/*****************************************************************************/
/**     2019-2020                         Cardona Lorenzo, Victor           **/
/**  Analizador sintactico                Murcia Serrano, Andrea            **/
/**                                       Serrano Hernandez, Luis           **/
/*****************************************************************************/
%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "libtds.h"
%}

%union {/********************************************************************/
  int   cent;                 /* Para el terminal "cte" entera              */
  char  *ident;               /* Nombre del identificador                   */
  int tipo;                   /* Tipo del simbolo                           */
  struct camposStruct lisCampos;  
  struct cteStruct  constanteStru;
}/***************************************************************************/

%token MAS_ MENOS_ POR_ DIV_ MOD_
MASASIG_ MENOSASIG_ PORASIG_ DIVASIG_ INC_ DEC_ ASIG_
ALLA_ CLLA_ APAR_ CPAR_ ACOR_ CCOR_
IGU_ DIST_ MAY_ MEN_ MAYIGU_ MENIGU_
AND_ OR_ NOT_
STRUCT_ INT_ BOOL_
READ_ PRINT_
IF_ ELSE_ WHILE_
TRUE_ FALSE_
DELI_ PUNTO_

%token<ident> ID_ 
%token<cent> CTE_ 

%type<tipo> tipoSimple
%type<lisCampos> listaCampos
%type<constanteStru> constante

%%
programa
  : { dvar=0; } 
    ALLA_ secuenciaSentencias CLLA_
    {
      verTdS();
    }
  ;

secuenciaSentencias
  : sentencia
  | secuenciaSentencias sentencia
  ;

sentencia
  : declaracion 
  | instruccion
  ;

declaracion
    //declaracion de un tipo simple
  : tipoSimple ID_ DELI_
      {
        if (! insTdS($2, $1, dvar, -1)) {
          yyerror("Identificador repetido");
        } else {
          dvar += TALLA_TIPO_SIMPLE;
        }
      }
    //declaracion y asignacion de un tipo simple
  | tipoSimple ID_ ASIG_ constante DELI_ 
      {
        if ($1 != $4.tipo) {
          yyerror("Tipos incompatibles");
        }
        if (! insTdS($2, $1, dvar, -1)) {
          yyerror("Identificador repetido");
        } else {
          dvar += TALLA_TIPO_SIMPLE;
        }
      }
    //declaracion de un array
  | tipoSimple ID_ ACOR_ CTE_ CCOR_ DELI_
      {
        int numelem = $4;
        if ($4 <= 0) {
          yyerror("Talla negativa del array");
          numelem = 0;
        }
        int refe = insTdA($1, numelem);
        if (! insTdS($2, T_ARRAY, dvar, refe)) {
          yyerror("Identificador repetido");
        } else {
          dvar += numelem * TALLA_TIPO_SIMPLE;
        }
        
      }
    //declaracion de una estructura
  | STRUCT_ ALLA_ listaCampos CLLA_ ID_ DELI_
      {
        if (! insTdS($5, T_RECORD, dvar, $3.ref)) {
          yyerror("Identificador repetido");
        } else {
          dvar += TALLA_TIPO_SIMPLE;
        }
      }
  ;

tipoSimple
  : INT_
      {
        $$ = T_ENTERO;
      }
  | BOOL_
      {
        $$ = T_LOGICO;
      }
  ;

/*campos para las estructuras*/
listaCampos
  : tipoSimple ID_ DELI_
      {
        $$.talla = 1;
        int refe = insTdR(-1, $2, $1, dvar);
        if ( refe == -1) {
          yyerror("Identificador repetido");
        } else {
          $$.ref = refe;
          dvar += TALLA_TIPO_SIMPLE;
        }
      }
  | listaCampos tipoSimple ID_ DELI_
      {
        $$.talla = $1.talla + 1;
        int refe = insTdR($1.ref, $3, $2, dvar);
        if ( refe == -1) {
          yyerror("Identificador repetido");
        } else {
          /*con que se rellena la referencia $.ref o refe? en caso de error T_ERROR?*/
          $$.ref = $1.ref;
          dvar += TALLA_TIPO_SIMPLE;
        }
      }
  ;

instruccion
  : ALLA_ CLLA_ 
  | ALLA_ listaInstrucciones CLLA_
  | instruccionEntradaSalida
  | instruccionSeleccion
  | instruccionIteracion
  | instruccionExpresion
  ;

listaInstrucciones
  : instruccion
  | listaInstrucciones instruccion
  ;

instruccionEntradaSalida
  : READ_ APAR_ ID_ CPAR_ DELI_
  | PRINT_ APAR_ expresion CPAR_ DELI_
  ;

instruccionSeleccion
  : IF_ APAR_ expresion CPAR_ instruccion ELSE_ instruccion
  ;

instruccionIteracion
  : WHILE_ APAR_ expresion CPAR_ instruccion
  ;

instruccionExpresion
  : expresion DELI_
  | DELI_
  ;

expresion
  : expresionLogica
  | ID_ operadorAsignacion expresion
  | ID_ ACOR_ expresion CCOR_ operadorAsignacion expresion
  | ID_ PUNTO_ ID_ operadorAsignacion expresion
  ;

expresionLogica
  : expresionIgualdad
  | expresionLogica operadorLogico expresionIgualdad
  ;

expresionIgualdad
  : expresionRelacional
  | expresionIgualdad operadorIgualdad expresionRelacional
  ;

expresionRelacional
  : expresionAditiva
  | expresionRelacional operadorRelacional expresionAditiva
  ;

expresionAditiva
  : expresionMultiplicativa
  | expresionAditiva operadorAditivo expresionMultiplicativa
  ;

expresionMultiplicativa
  : expresionUnaria
  | expresionMultiplicativa operadorMultiplicativo expresionUnaria
  ;

expresionUnaria
  : expresionSufija
  | operadorUnario expresionUnaria
  | operadorIncremento ID_
  ;

expresionSufija
  : APAR_ expresion CPAR_
  | ID_ operadorIncremento
  | ID_ ACOR_ expresion CCOR_
  | ID_
  | ID_ PUNTO_ ID_
  | constante
  ;

constante
  : CTE_ 
      {
        $$.tipo = T_ENTERO;
        $$.cent = $1;
      }
  | TRUE_ 
      {
        $$.tipo = T_LOGICO;
        $$.cent = TRUE;
      }
  | FALSE_
      {
        $$.tipo = T_LOGICO;
        $$.cent = FALSE;
      }
  ;

operadorAsignacion
  : ASIG_
  | MASASIG_
  | MENOSASIG_
  | PORASIG_
  | DIVASIG_
  ;

operadorLogico
  : AND_
  | OR_
  ;

operadorIgualdad
  : IGU_
  | DIST_
  ;

operadorRelacional
  : MAY_
  | MEN_
  | MAYIGU_
  | MENIGU_
  ;

operadorAditivo
  : MAS_
  | MENOS_
  ;

operadorMultiplicativo
  : POR_
  | DIV_
  | MOD_
  ;

operadorUnario
  : MAS_
  | MEN_
  | NOT_
  ;

operadorIncremento
  : INC_
  | DEC_
  ;

%%
/*****************************************************************************/


