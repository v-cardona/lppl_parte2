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
  struct CamposStruct lisCampos;  
  struct CteStruct  constanteStru;
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

%type<tipo> expresion expresionAditiva expresionIgualdad expresionLogica expresionMultiplicativa
  expresionRelacional expresionSufija expresionUnaria
%type<tipo> operadorUnario
%%
programa
  : { dvar = 0; } 
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
        else if (! insTdS($2, $1, dvar, -1)) {
          yyerror("Identificador repetido");
        } else {
          dvar += TALLA_TIPO_SIMPLE;
        }
      }
    //declaracion de un array
  | tipoSimple ID_ ACOR_ CTE_ CCOR_ DELI_
      {
        int numelem = $4;
        if (numelem <= 0) {
          yyerror("Talla negativa del array");
          numelem = 0;
        } else  {
          int refe = insTdA($1, numelem);
          if (! insTdS($2, T_ARRAY, dvar, refe)) {
            yyerror("Identificador repetido");
          } else {
            dvar += numelem * TALLA_TIPO_SIMPLE;
          }
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
    {
      $$ = $1;
    }
  | ID_ operadorAsignacion expresion
    {
      $$ = T_ERROR;
      SIMB sim = obtTdS($1);

      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if (! ((sim.tipo == $3 == T_ENTERO) || (sim.tipo == $3 == T_LOGICO))) {
        yyerror("Tipos incompatibles en la instruccion de asignacion");
      } else {
        $$ = sim.tipo;
      }
    }
  | ID_ ACOR_ expresion CCOR_ operadorAsignacion expresion
    {
      $$ = T_ERROR;
      SIMB sim = obtTdS($1);

      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if ($3 != T_ENTERO) {
        yyerror("Incompatibilidad de tipo en el indice del array");
      } else if (sim.tipo != T_ARRAY) {
        yyerror("El objeto no es de tipo array");
      } else {
        DIM dim = obtTdA(sim.ref);
        if (dim.telem != $6) {
          yyerror("Incomptabilidad de tipo entre el array y la asignacion");
        } else {
          $$ = dim.telem;
        }
      }
    }
  | ID_ PUNTO_ ID_ operadorAsignacion expresion
    {
      $$ = T_ERROR;
      SIMB sim = obtTdS($1);
      CAMP cam;

      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if (sim.tipo != T_RECORD) {
        yyerror("El objeto no es de tipo registro");
      } else {
        cam = obtTdR(sim.ref, $3);
        if (cam.tipo == T_ERROR) {
          yyerror("No existe el campo del registro");
        } else if (cam.tipo != $5) {
          yyerror("Incomptabilidad de tipos entre el campo del registro y la asignacion");
        } else {
          $$ = cam.tipo;
        }
      }
    }
  ;

expresionLogica
  : expresionIgualdad
    {
      $$ = $1;
    }
  | expresionLogica operadorLogico expresionIgualdad
    {
      $$ = T_ERROR;
      if (! (($1 == T_LOGICO) && ($3 == T_LOGICO))) {
        yyerror("Los tipos del operador logico son incompatibles, deben ser logicos");
      } else {
        $$ = T_LOGICO;
      }
    }
  ;

expresionIgualdad
  : expresionRelacional
    {
      $$ = $1;
    }
  | expresionIgualdad operadorIgualdad expresionRelacional
    {
      $$ = T_ERROR;

      if ($1 != $3) {
        yyerror("Los tipos del operador de igualdad son incompatibles");
      } else {
        $$ = $1;
      }
    }
  ;

expresionRelacional
  : expresionAditiva
    {
      $$ = $1;
    }
  | expresionRelacional operadorRelacional expresionAditiva
    {
      $$ = T_ERROR;

      if (! (($1 == T_ENTERO) && ($3 == T_ENTERO))) {
        yyerror("Los tipos del operador relacional son incompatibles, deben ser enteros");
      } else {
        $$ = T_LOGICO;
      }
    }
  ;

expresionAditiva
  : expresionMultiplicativa
    {
      $$ = $1;
    }
  | expresionAditiva operadorAditivo expresionMultiplicativa
    {
      $$ = T_ERROR;

      if (! ($1 == $3 == T_ENTERO)) {
        yyerror("Los tipos del operador aditivo son incompatibles, deben ser enteros");
      } else {
        $$ = T_ENTERO;
      }
    }
  ;

expresionMultiplicativa
  : expresionUnaria
    {
      $$ = $1;
    }
  | expresionMultiplicativa operadorMultiplicativo expresionUnaria
    {
      $$ = T_ERROR;

      if (! ($1 == $3 == T_ENTERO)) {
        yyerror("Los tipos del operador multiplicativo son incompatibles, deben ser enteros");
      } else {
        $$ = T_ENTERO;
      }
    }
  ;

expresionUnaria
  : expresionSufija
    {
      $$ = $1;
    }
  | operadorUnario expresionUnaria
    {
      $$ = T_ERROR;

      if ($1 != $2) {
        yyerror("El tipo del operador unario no es compatible");
      } else {
        $$ = $1;
      }
    }
  | operadorIncremento ID_
    {
      $$ = T_ERROR;
      SIMB sim = obtTdS($2);
      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if (sim.tipo != T_ENTERO) {
        yyerror("Incomptabilidad de tipos, debe ser entero");
      } else {
        $$ = T_ENTERO;
      }
    }
  ;

expresionSufija
  : APAR_ expresion CPAR_
    {
      $$ = $2;
    }
  | ID_ operadorIncremento
    {
      $$ = T_ERROR;
      SIMB sim = obtTdS($1);
      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if (sim.tipo != T_ENTERO) {
        yyerror("Incomptabilidad de tipos, debe ser entero");
      } else {
        $$ = T_ENTERO;
      }
    }
  | ID_ ACOR_ expresion CCOR_
    {
      $$ = T_ERROR;
      SIMB sim = obtTdS($1);

      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if ($3 != T_ENTERO) {
        yyerror("Incompatibilidad de tipo en el indice del array");
      } else if (sim.tipo != T_ARRAY) {
        yyerror("El objeto no es de tipo array");
      } else {
        DIM dim = obtTdA(sim.ref);
        $$ = dim.telem;
      }
    }
  | ID_
    {
      $$ = T_ERROR;
      SIMB sim = obtTdS($1);

      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else {
        $$ = sim.tipo;
      }
    }
  | ID_ PUNTO_ ID_
    {
      $$ = T_ERROR;
      SIMB sim = obtTdS($1);
      CAMP cam;

      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if (sim.tipo != T_RECORD) {
        yyerror("El objeto no es de tipo registro");
      } else {
        cam = obtTdR(sim.ref, $3);
        if (cam.tipo == T_ERROR) {
          yyerror("No existe el campo del registro");
        } else {
          $$ = cam.tipo;
        }
      }
    }
  | constante
    {
      $$ = $1.tipo;
    }
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
    {
      $$ = T_ENTERO;
    }
  | MEN_
    {
      $$ = T_ENTERO;
    }
  | NOT_
    {
      $$ = T_LOGICO;
    }
  ;

operadorIncremento
  : INC_
  | DEC_
  ;

%%
/*****************************************************************************/


