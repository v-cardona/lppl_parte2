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
  struct CamposStruct lisCampos;  /* Estructura para los campos del registro*/
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
%type<tipo> constante

%type<tipo> expresion expresionAditiva expresionIgualdad expresionLogica expresionMultiplicativa
  expresionRelacional expresionSufija expresionUnaria
%type<tipo> operadorUnario operadorAsignacion
%%
programa
  : { dvar = 0; } 
    ALLA_ secuenciaSentencias CLLA_
    {
      if (verTDS) verTdS();
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
        if ($1 != $4) {
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
        $$.ref = insTdR(-1, $2, $1, 0);
        $$.talla= TALLA_TIPO_SIMPLE;
      }
  | listaCampos tipoSimple ID_ DELI_
      {
        $$.ref = $1.ref;
        int refe = insTdR($1.ref, $3, $2, $1.talla);
        if ( refe == -1) {
          yyerror("Campo repetido");
        } else{
          $$.talla = $1.talla + TALLA_TIPO_SIMPLE;
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
    {
      SIMB sim = obtTdS($3);
      if (sim.tipo == T_ERROR) {
        yyerror("Objeto no declarado");
      } else if (sim.tipo != T_ENTERO) {
        yyerror("El argumento del read debe ser entero");
      }
    }
  | PRINT_ APAR_ expresion CPAR_ DELI_
    {
      if ($3 != T_ENTERO && $3 != T_ERROR) {
        yyerror("El argumento del print debe ser entero");
      }
    }
  ;

instruccionSeleccion
  : IF_ APAR_ expresion CPAR_ instruccion ELSE_ instruccion
    {
      if ($3 != T_LOGICO && $3 != T_ERROR) {
        yyerror("La condicion del if debe ser logica");
      }
    }
  ;

instruccionIteracion
  : WHILE_ APAR_ expresion 
    {
      if ($3 != T_LOGICO && $3 != T_ERROR) {
        yyerror("La condicion del while deber ser logica");
      }
    }
    CPAR_ instruccion

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
        if ($3 != T_ERROR) {
          yyerror("Tipos incompatibles en la asignacion");
        }
      } else if ($3 == T_LOGICO && $2 != T_ASIG) {
        yyerror("Operador no compatible con el tipo logico");
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
        if ($3 != T_ERROR) {
          yyerror("Incompatibilidad de tipo en el indice del array");
        }
      } else if (sim.tipo != T_ARRAY) {
        yyerror("El objeto no es de tipo array");
      } else {
        DIM dim = obtTdA(sim.ref);
        if (dim.telem != $6) {
          if ($6 != T_ERROR) {
            yyerror("Incomptabilidad de tipo entre el array y la asignacion");
          }
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
          if ($5 != T_ERROR) {
            yyerror("Incomptabilidad de tipos entre el campo del registro y la asignacion");
          }
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
        if ($1 != T_ERROR && $3 != T_ERROR) {
          yyerror("Los tipos del operador logico son incompatibles, deben ser logicos");
        }
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
        if ($1 != T_ERROR && $3 != T_ERROR) {
          yyerror("Los tipos del operador de igualdad son incompatibles");
        }
      } else {
        $$ = T_LOGICO;
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
        if ($1 != T_ERROR && $3 != T_ERROR) {
          yyerror("Los tipos del operador relacional son incompatibles, deben ser enteros");
        }
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
        if ($1 != T_ERROR && $3 != T_ERROR) {
          yyerror("Los tipos del operador aditivo son incompatibles, deben ser enteros");
        }
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
        if ($1 != T_ERROR && $3 != T_ERROR) {
          yyerror("Los tipos del operador multiplicativo son incompatibles, deben ser enteros");
        }
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
        if ($1 != T_ERROR && $2 != T_ERROR) {
          yyerror("El tipo del operador unario no es compatible");
        }
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
        if ($3 != T_ERROR) {
          yyerror("Incompatibilidad de tipo en el indice del array");
        }
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
      $$ = $1;
    }
  ;

constante
  : CTE_ 
      {
        $$ = T_ENTERO;
      }
  | TRUE_ 
      {
        $$ = T_LOGICO;
      }
  | FALSE_
      {
        $$ = T_LOGICO;
      }
  ;

operadorAsignacion
  : ASIG_
    {
      $$ = T_ASIG;
    }
  | MASASIG_ 
    {
      $$ = T_ENTERO;
    }
  | MENOSASIG_
    {
      $$ = T_ENTERO;
    }
  | PORASIG_
    {
      $$ = T_ENTERO;
    }
  | DIVASIG_
    {
      $$ = T_ENTERO;
    }
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
  | MENOS_
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


