/*
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 * Copyright (C) 2019 Abinav Puthan Purayil
 *
 * To prettyprint ASTs (ie. to be used in flang1), do the following:
 * - add this file in flang1exe/
 * - Write #define PRETTYPRINT_FLANG1 *followed* by #including this file (shush...
 *   I know #including .c file is bad) wherever you want in flang1/
 * - call prettyprint_ast(any_ast_you_want)
 * - Enjoy!
 *
 * To prettyprint ILIs, do the analgous of above for flang2.
 *
 * WARNING: CMake will fail if it got re-invoked (unless you track this file in
 * flang's CMake), duh.
 */

#ifndef PRETTYPRINT_FLANG
#define PRETTYPRINT_FLANG

/* There are still more macros to be handled, we currently use the build path to
 * grep for the unhandled macro.
 */
#define FLANG_BUILD_PATH "/home/abinav/llvm_dev/build/flang-compiler/flang"

enum termstr {
  termstr_clear,
  termstr_red,
  termstr_green,
  termstr_yellow,
  termstr_blue
};

const char *get_termstr(enum termstr ts) {
  if (!isatty(fileno(stdout))) {
    return "";
  }

  switch(ts) {
  case termstr_clear:
    return "\033[0m";
  case termstr_red:
    return "\033[31m";
  case termstr_green:
    return "\033[32m";
  case termstr_yellow:
    return "\033[33m";
  case termstr_blue:
    return "\033[34m";
  default:
    return "";
  }
}

struct prettyprint_attr {
  bool newline;
  bool expand_obj;
  bool lineno;

  bool is_ili_opn;
};

// Copied from mwd.cpp's dumpdtype()
static char *prettystr_ty(int ty)
{
  char *r = "unknown";

  switch (ty) {
    case TY_NONE:
      r = "none";
      break;
    case TY_WORD:
      r = "word";
      break;
    case TY_DWORD:
      r = "dword";
      break;
    case TY_HOLL:
      r = "hollerith";
      break;
    case TY_BINT:
      r = "int*1";
      break;
    case TY_SINT:
      r = "short int";
      break;
    case TY_INT:
      r = "int";
      break;
    case TY_INT8:
      r = "int*8";
      break;
    case TY_INT128:
      r = "int128";
      break;
#ifdef PRETTYPRINT_FLANG2
    case TY_UBINT:
      r = "uint*1";
      break;
    case TY_USINT:
      r = "unsigned short";
      break;
    case TY_UINT:
      r = "unsigned int";
      break;
    case TY_UINT8:
      r = "unsigned int*8";
      break;
    case TY_UINT128:
      r = "uint128";
      break;
#endif
    case TY_128:
      r = "ty128";
      break;
    case TY_256:
      r = "ty256";
      break;
    case TY_512:
      r = "ty512";
      break;
    case TY_REAL:
      r = "real";
      break;
    case TY_FLOAT128:
      r = "float128";
      break;
    case TY_DBLE:
      r = "double";
      break;
    case TY_QUAD:
      r = "quad";
      break;
    case TY_CMPLX:
      r = "complex";
      break;
    case TY_DCMPLX:
      r = "double complex";
      break;
    case TY_CMPLX128:
      r = "cmplx128";
      break;
    case TY_BLOG:
      r = "byte logical";
      break;
    case TY_SLOG:
      r = "short logical";
      break;
    case TY_LOG:
      r = "logical";
      break;
    case TY_LOG8:
      r = "logical*8";
      break;
    case TY_CHAR:
      r = "character";
      break;
    case TY_NCHAR:
      r = "ncharacter";
      break;
    case TY_PTR:
      r = "pointer";
      break;
    case TY_ARRAY:
      r = "array";
      break;
    case TY_STRUCT:
      r = "struct";
      break;
    case TY_UNION:
      r = "union";
      break;
    case TY_NUMERIC:
      r = "numeric";
      break;
    case TY_ANY:
      r = "any";
      break;
    case TY_PROC:
      r = "proc";
      break;
#ifdef PRETTYPRINT_FLANG2
    case TY_VECT:
      r = "vect";
      break;
    case TY_PFUNC:
      r = "prototype func";
      break;
    case TY_PARAM:
      r = "parameter";
      break;
#endif
    default:
      // Don't use a case label for TY_FLOAT, because it might alias TY_REAL.
      if (ty == TY_FLOAT) {
        r = "float";
        break;
      } else {
        char cmdline[1024];
        printf(get_termstr(termstr_red));
        printf("ty %d not handled!\n", ty);
        sprintf(cmdline,
            "grep '#define TY_.* %d$' "FLANG_BUILD_PATH"/tools/flang1/utils/symtab/symtab.h",
            ty);
        system(cmdline);
        sprintf(cmdline,
            "grep '#define TY_.* %d$' "FLANG_BUILD_PATH"/tools/flang2/utils/symtab/symtab.h",
            ty);
        system(cmdline);
        printf(get_termstr(termstr_clear));
      }
      break;
  }
  return r;
}

static char *prettystr_sym(int sptr) {
  return getprint(sptr);
}

static char *prettystr_dt(int _dt) {
  static char dt_str[1024];
  strcpy(dt_str, "");
  DTYPE dt = (DTYPE)_dt;

  // Are flang array types recursive ?
  if (DTY(dt) == TY_ARRAY) {
    strcat(dt_str, prettystr_ty(DTY((DTYPE)((int)dt + 1))));
    strcat(dt_str, " ");
    strcat(dt_str, prettystr_ty(DTY(dt)));

    ADSC *ad = AD_DPTR(dt);
    int numdim = AD_NUMDIM(ad);
    strcat(dt_str, "[");
    for (int i = 0; i < numdim; ++i) {
      char bounds_str[128];
      /* FIXME: AD_XXX() returns AST in flang1 and SPTR in flang2!!! we need a
       * flang1 and flang2 compatible API here */
#ifdef PRETTYPRINT_FLANG2
      sprintf(bounds_str, "%s:%s", prettystr_sym(AD_LWBD(ad, i)), prettystr_sym(AD_UPBD(ad, i)));
#else
      sprintf(bounds_str, "?:?");
#endif
      strcat(dt_str, bounds_str);
      if (i != numdim - 1) { strcat(dt_str, ", "); }
    }
    strcat(dt_str, "]");

  } else if (DTY(dt) == TY_CHAR)  {
    strcat(dt_str, prettystr_ty(DTY(dt)));
    strcat(dt_str, "*");
    strcat(dt_str, prettystr_sym(((int)dt + 1)));

  } else if (DTY(dt) == TY_PTR) {
    strcat(dt_str, prettystr_ty(DTY((DTYPE)((int)dt + 1))));
    strcat(dt_str, " ");
    strcat(dt_str, prettystr_ty(DTY(dt)));

  } else if (DTY(dt) == TY_STRUCT || DTY(dt) == TY_UNION) {
    strcat(dt_str, prettystr_ty(DTY(dt)));
    strcat(dt_str, ".");
    strcat(dt_str, prettystr_sym(DTY((DTYPE)((int)dt + 1))));

#ifdef PRETTYPRINT_FLANG2
  } else if (DTY(dt) == TY_VECT) {
    char type[64];
    sprintf(type, "<%lu x %s>", ((int)dt + 2), prettystr_ty(DTY(dt)));
    strcat(dt_str, type);
#endif

  } else {
    strcat(dt_str, prettystr_ty(DTY(dt)));
  }
  return dt_str;
}

static char *prettystr_symtype(int sptr) {
  char *dt_str = prettystr_dt(DTYPEG(sptr));

  switch(STYPEG(sptr)) {
  case ST_UNKNOWN:
    strcpy(dt_str, "");
    break;
  case ST_LABEL:
    strcpy(dt_str, "label");
    break;
  case ST_ENTRY:
    strcpy(dt_str, "entry");
    break;
  }

  return dt_str;
}

static char *prettystr_func_signature(int func_sptr) {
  static char func_signature[4096];

  sprintf(func_signature, "%s (", prettystr_sym(func_sptr));
  int func_paramct = PARAMCTG(func_sptr);
  int func_dpsc = DPDSCG(func_sptr);

  bool first_ele = true;
  for (int i = 0; i < func_paramct; i++) {
    SPTR arg_sptr = (SPTR)aux.dpdsc_base[func_dpsc + i];
    if (first_ele) { first_ele = false; }
    else { strcat(func_signature, ", "); }
    strcat(func_signature, prettystr_dt(DTYPEG(arg_sptr)));
    strcat(func_signature, " ");
    strcat(func_signature, prettystr_sym(arg_sptr));
  }
  strcat(func_signature, ")");

  return func_signature;
}

#include "llmputil.h"

static const char *prettystr_omp_target_mode(int mode) {
  switch (mode) {
  case mode_none_target:
    return "<mode none>";
  case mode_target:
    return "<target>";
  case mode_target_teams:
    return "<target teams>";
  case mode_target_parallel_for_simd:
    return "<target parallel for simd>";
  case mode_target_parallel_for:
    return "<target parallel for>";
  case mode_target_teams_distribute_parallel_for:
    return "<target teams distribute parallel for>";
  case mode_target_teams_distribute_parallel_for_simd:
    return "<target teams distribute parallel for simd>";
  case mode_target_teams_distribute:
    return "<target teams distribute >";
  case mode_target_data_region:
    return "<target data>";
  case mode_target_data_enter_region:
    return "<target data enter>";
  case mode_target_data_exit_region:
    return "<target data exit>";
  }
  return "";
}

static void prettyprint_uplevel(const LLUplevel *uplevel) {
  /* TODO: how to get uplevel's sptr ? */
  printf("LLUplevel: %p symbols: ", uplevel);

  for (int i = 0; i < uplevel->vals_count; ++i) {
    if (!uplevel->vals[i])
      continue;

#ifdef PRETTYPRINT_FLANG1
    if (STYPEG(uplevel->vals[i]) == ST_ARRDSC)
#else
    if (DESCARRAYG(uplevel->vals[i]))
#endif
        continue;
    printf("%s%s ", get_termstr(termstr_green),
        prettystr_symtype(uplevel->vals[i]));
    printf("%s%s, ", get_termstr(termstr_clear),
        prettystr_sym(uplevel->vals[i]));
  }

  printf("\n");
}

static void prettyprint_uplevel_chain(const LLUplevel *uplevel) {
  while (uplevel) {
    prettyprint_uplevel(uplevel);
    if (!uplevel->parent)
      break;
    printf("child of :\n  ");
    uplevel = llmp_has_uplevel(uplevel->parent);
  }
}

static void prettyprint_uplevel_sptr(int uplevel_sptr) {
  LLUplevel *uplevel = llmp_get_uplevel(uplevel_sptr);

  printf("uplevel: %s ", prettystr_sym(uplevel_sptr));
  if (uplevel->parent)
    printf("parent: %s ", prettystr_sym(uplevel->parent));

  prettyprint_uplevel(uplevel);
}

static void prettyprint_scope_sptr(int scope_sptr) {
  printf("scope: %s ", prettystr_sym(scope_sptr));
  int uplevel_sptr = PARUPLEVELG(scope_sptr);

  if (PARSYMSG(uplevel_sptr)) {
    prettyprint_uplevel_sptr(uplevel_sptr);
  }
  printf("\n");
}

#ifdef PRETTYPRINT_FLANG1
void prettyprint_ast(int ast);
void prettyprint_ast_with_attr(int ast, struct prettyprint_attr *attr);

static struct prettyprint_attr default_arg_attr = {
  .newline = false,
  .lineno = false,
  .expand_obj = true
};

static char *prettystr_object(int ast) {
  return prettystr_sym(A_SPTRG(ast));
}

static char *prettystr_op(int optype) {
  char cmdline[4096];

  switch (optype) {
    case OP_NEG:
      return "neg";
    case OP_ADD:
      return "add";
    case OP_SUB:
      return "sub";
    case OP_MUL:
      return "mul";
    case OP_DIV:
      return "div";

    case OP_LEQV:
      return "leqv";
    case OP_LNEQV:
      return "lneqv";
    case OP_LOR:
      return "lor";
    case OP_LAND:
      return "land";
    case OP_LNOT:
      return "lnot";

    case OP_EQ:
      return "eq";
    case OP_GE:
      return "ge";
    case OP_GT:
      return "gt";
    case OP_LE:
      return "le";
    case OP_LT:
      return "lt";
    case OP_NE:
      return "ne";

    /* TODO: what the hell is this ? */
    case OP_VAL:
      return "op_val";

    default:
      printf(get_termstr(termstr_red));
      printf("op %d not handled!\n", optype);
      sprintf(cmdline,
          "grep '#define OP_.* %d$' "FLANG_BUILD_PATH"/tools/flang1/utils/ast/ast.h",
          optype);
      system(cmdline);
      printf(get_termstr(termstr_clear));
  }
  return "";
}

static void prettyprint_stblk(int ast) {
  int scope_sptr = A_SPTRG(ast);
  prettyprint_scope_sptr(scope_sptr);
}

static void prettyprint_asn(int ast, struct prettyprint_attr *attr) {
  int lhs = A_DESTG(ast), rhs = A_SRCG(ast);

  printf("asn, ");
  printf("lhs: "); prettyprint_ast_with_attr(lhs, &default_arg_attr);
  printf(", rhs: "); prettyprint_ast_with_attr(rhs, &default_arg_attr);

  if (attr->newline)
    printf("\n");
}

static void prettyprint_binop(int ast, struct prettyprint_attr *attr) {
  int lop = A_LOPG(ast), rop = A_ROPG(ast);

  printf("(binop-%s ", prettystr_op(A_OPTYPEG(ast)));

  printf("lop: "); prettyprint_ast_with_attr(lop, &default_arg_attr);
  printf(", rop: "); prettyprint_ast_with_attr(rop, &default_arg_attr);

  printf(")");
  if (attr->newline)
    printf("\n");
}

static void prettyprint_unop(int ast, struct prettyprint_attr *attr) {
  int lop = A_LOPG(ast);

  printf("(unop-%s ", prettystr_op(A_OPTYPEG(ast)));
  printf("lop: "); prettyprint_ast_with_attr(lop, &default_arg_attr);

  printf(")");
  if (attr->newline)
    printf("\n");
}

static void prettyprint_entry(int ast, struct prettyprint_attr *attr) {
  printf("%d %s", ast, prettystr_sym(A_SPTRG(ast)));
  if (attr->newline)
    printf("\n");
}

static void prettyprint_ifthen(int ast, struct prettyprint_attr *attr) {
  printf("ifthen ");
  prettyprint_ast_with_attr(A_IFEXPRG(ast), &default_arg_attr);
  if (attr->newline)
    printf("\n");
}

static void prettyprint_func(int ast, struct prettyprint_attr *attr) {
  int i, arg;
  int argt = A_ARGSG(ast);
  int argcnt = A_ARGCNTG(ast);
  bool first_ele = true;

  printf("%s(", prettystr_object(A_LOPG(ast)));
  for (i = 0; i < argcnt; i++) {
    arg = ARGT_ARG(argt, i);

    if (first_ele) { first_ele = false; }
    else { printf(", "); }

    prettyprint_ast_with_attr(arg, &default_arg_attr);
  }
  printf(")");

  if (DTY(A_DTYPEG(ast)))
    printf(" -> %s%s %s", get_termstr(termstr_green),
        prettystr_dt(A_DTYPEG(ast)), get_termstr(termstr_clear));
  if (attr->newline)
    printf("\n");
}

static void prettyprint_subscr(int ast, struct prettyprint_attr *attr) {
  int asd = A_ASDG(ast);
  int ndim = ASD_NDIM(asd);
  int i;
  bool first_ele = true;

  prettyprint_ast_with_attr(A_LOPG(ast), &default_arg_attr);

  printf("[");
  for (i = 0; i < ndim; ++i) {
    int ss;
    ss = ASD_SUBS(asd, i);

    if (first_ele) { first_ele = false; }
    else { printf(", "); }

    prettyprint_ast_with_attr(ss, &default_arg_attr);
  }
  printf("]");

  if (attr->newline)
    printf("\n");
}

static void prettyprint_conv(int ast, struct prettyprint_attr *attr) {
  printf("(conv-to-%s ", prettystr_dt(A_DTYPEG(ast)));
  printf("(");
  prettyprint_ast_with_attr(A_LOPG(ast), &default_arg_attr);
  printf(")");

  printf(")");
  if (attr->newline)
    printf("\n");
}

#include "gramtk.h"
static void prettyprint_alloc(int ast, struct prettyprint_attr *attr) {
  if (A_TKNG(ast) == TK_ALLOCATE) { printf("allocate"); }
  else { printf("deallocate"); }

  if (A_LOPG(ast)) {
    printf(", stat:");
    prettyprint_ast_with_attr(A_LOPG(ast), &default_arg_attr);
  }

  printf(" ");
  prettyprint_ast_with_attr(A_SRCG(ast), &default_arg_attr);
  if (attr->newline)
    printf("\n");
}

static void prettyprint_triple(int ast, struct prettyprint_attr *attr) {
  printf("triple lb: ");
  prettyprint_ast_with_attr(A_LBDG(ast), &default_arg_attr);
  printf(", ub: ");
  prettyprint_ast_with_attr(A_UPBDG(ast), &default_arg_attr);
  printf(", stride: ");
  prettyprint_ast_with_attr(A_STRIDEG(ast), &default_arg_attr);

  if (attr->newline)
    printf("\n");
}

static void prettyprint_forall(int ast, struct prettyprint_attr *attr) {
  printf("forall list: ");

  prettyprint_ast_with_attr(A_LISTG(ast), &default_arg_attr);
  if (attr->newline)
    printf("\n");
}

static void prettyprint_do(int ast, struct prettyprint_attr *attr) {
  if (A_TYPEG(ast) == A_MP_PDO) {
    printf("mp_pdo: ");
    printf(", dovar: ");
    prettyprint_ast_with_attr(A_DOVARG(ast), &default_arg_attr);
    printf(", lastval: ");
    prettyprint_ast_with_attr(A_LASTVALG(ast), &default_arg_attr);
  } else {
    printf("do: ");
  }

  printf(" init: ");
  prettyprint_ast_with_attr(A_M1G(ast), &default_arg_attr);
  printf(", limit: ");
  prettyprint_ast_with_attr(A_M2G(ast), &default_arg_attr);
  printf(", skip: ");
    prettyprint_ast_with_attr(A_M3G(ast), &default_arg_attr);

  if (attr->newline)
    printf("\n");
}

static void prettyprint_mp_target(int ast, struct prettyprint_attr *attr) {
  printf("%s, ", astb.atypes[A_TYPEG(ast)]);
  printf("mode: ");
  printf("%s", prettystr_omp_target_mode(A_COMBINEDTYPEG(ast)));
  if (A_IFPARG(ast)) {
    printf(", if-par: ");
    prettyprint_ast_with_attr(A_IFPARG(ast), &default_arg_attr);
  }

  if (attr->newline)
    printf("\n");
}

static void prettyprint_mp_teams(int ast, struct prettyprint_attr *attr) {
  printf("%s", astb.atypes[A_TYPEG(ast)]);
  if (A_NTEAMSG(ast)) {
    printf(", num_teams: ");
    prettyprint_ast_with_attr(A_NTEAMSG(ast), &default_arg_attr);
  }

  if (A_THRLIMITG(ast)) {
    printf(", thread_limit: ");
    prettyprint_ast_with_attr(A_THRLIMITG(ast), &default_arg_attr);
  }

  if (attr->newline)
    printf("\n");
}

static void prettyprint_mp_reductionitem(int ast, struct prettyprint_attr *attr) {
  printf("%s, ", astb.atypes[A_TYPEG(ast)]);

  printf("op: %s ", prettystr_op(A_REDOPRG(ast)));

  int sptr;
  if (sptr = A_PRVSYMG(ast)) {
    printf("private sym: %s, ", prettystr_sym(sptr));
  }

  if (sptr = A_SHSYMG(ast)) {
    printf("shared sym: %s, ", prettystr_sym(sptr));
  }

  if (attr->newline)
    printf("\n");

}

static void prettyprint_mp_atomic_kind(int ast, struct prettyprint_attr *attr) {
  printf("%s, ", astb.atypes[A_TYPEG(ast)]);

  if (A_TYPEG(ast) == A_MP_ATOMICUPDATE) {
    printf("op: %s, ", prettystr_op(A_OPTYPEG(ast)));
  }

  if (A_MEM_ORDERG(ast)) {
    printf("mem-order: ");
    prettyprint_ast_with_attr(A_MEM_ORDERG(ast), &default_arg_attr);
  }

  if (attr->newline)
    printf("\n");
}

static void prettyprint_mp_bmpscope(int ast, struct prettyprint_attr *attr) {
  printf("%s, ", astb.atypes[A_TYPEG(ast)]);
  prettyprint_stblk(A_STBLKG(ast));

  if (attr->newline)
    printf("\n");
}

static void prettyprint_where(int ast, struct prettyprint_attr *attr) {
  printf("%s, ", astb.atypes[A_TYPEG(ast)]);
  if (A_IFSTMTG(ast)) {
    printf("asn: ");
    prettyprint_ast_with_attr(A_IFSTMTG(ast), &default_arg_attr);
  }
  printf("mask: ");
  prettyprint_ast_with_attr(A_IFEXPRG(ast), &default_arg_attr);

  if (attr->newline)
    printf("\n");
}

void prettyprint_ast_with_attr(int ast, struct prettyprint_attr *attr) {
  char cmdline[4096] = {0};

  /* skip list */
  switch (A_TYPEG(ast)) {
  case A_MP_EMAP:
  case A_COMMENT:
    goto skip;
  case A_ID:
  case A_CNST:
  case A_MEM:
    if (!attr->expand_obj) {
      goto skip;
    }
  }

  if (STD_LINENO(A_STDG(ast)) && attr->lineno && attr->newline)
    printf("%d: ", STD_LINENO(A_STDG(ast)));
  switch (A_TYPEG(ast)) {
  case A_ID:
  case A_CNST:
  case A_MEM:
    if (attr->expand_obj) {
      printf("%s%s %s", get_termstr(termstr_green),
          prettystr_dt(A_DTYPEG(ast)), get_termstr(termstr_clear));
      if (A_TYPEG(ast) == A_MEM) {
        printf("%s.%s", prettystr_object(A_PARENTG(ast)),
            prettystr_object(A_MEMG(ast)));
      } else {
        printf("%s", prettystr_object(ast));
      }

      if (attr->newline)
        printf("\n");
    }
    break;

  case A_ALLOC:
    prettyprint_alloc(ast, attr);
    break;
  case A_ASN:
    prettyprint_asn(ast, attr);
    break;

  case A_BINOP:
    if (attr->expand_obj)
      prettyprint_binop(ast, attr);
    break;

  case A_CONV:
    if (attr->expand_obj)
      prettyprint_conv(ast, &default_arg_attr);
    break;

  case A_CALL:
    printf("call ");
    prettyprint_func(ast, attr);
    break;

  case A_DO:
    prettyprint_do(ast, attr);
    break;

  case A_ENTRY:
    prettyprint_entry(ast, attr);
    break;

  case A_ENDWHERE: case A_ELSEWHERE:
    printf("%s\n", astb.atypes[A_TYPEG(ast)]);
    break;

  case A_FORALL:
    prettyprint_forall(ast, attr);
    break;

  case A_FUNC: /* function reference */
    if (attr->expand_obj) {
      printf("func-ref ");
      prettyprint_func(ast, attr);
    }
    break;

  case A_IFTHEN:
    prettyprint_ifthen(ast, attr);
    break;

  case A_INTR:
    if (attr->expand_obj) {
      printf("intr ");
      prettyprint_func(ast, attr);
    }
    break;

  case A_ICALL:
    printf("icall ");
    prettyprint_func(ast, attr);
    break;

  case A_NULL:
    printf("%sNULL%s", get_termstr(termstr_blue), get_termstr(termstr_clear));
    if (attr->newline)
      printf("\n");
    break;

  case A_SUBSCR:
    if (attr->expand_obj)
      prettyprint_subscr(ast, attr);
    break;

  case A_TRIPLE:
    if (attr->expand_obj)
      prettyprint_triple(ast, attr);
    break;

  case A_UNOP:
    if (attr->expand_obj)
      prettyprint_unop(ast, attr);
    break;

  case A_WHERE:
    prettyprint_where(ast, attr);
    break;

  case A_MP_MAP:
    printf("%s ", astb.atypes[A_TYPEG(ast)]);
    prettyprint_ast_with_attr(A_LOPG(ast), &default_arg_attr);
    printf("\n");
    break;

  case A_MP_BMPSCOPE:
    prettyprint_mp_bmpscope(ast, attr);
    break;

  case A_MP_EMPSCOPE: case A_MP_ATOMIC: case A_MP_ENDATOMIC:
  case A_MP_ENDTARGETDATA: case A_MP_ENDTARGET: case A_MP_DISTRIBUTE:
  case A_MP_ENDPDO: case A_MP_ENDDISTRIBUTE: case A_MP_ENDTEAMS:
  case A_MP_BREDUCTION: case A_MP_EREDUCTION:
  case A_MP_CRITICAL: case A_MP_ENDCRITICAL:
  case A_CONTINUE: case A_END: case A_ENDDO: case A_ENDIF:
    printf("%s\n", astb.atypes[A_TYPEG(ast)]);
    break;

  case A_MP_TARGETDATA: case A_MP_TARGETENTERDATA: case A_MP_TARGETEXITDATA:
  case A_MP_TARGETUPDATE: case A_MP_PARALLEL: case A_MP_ENDPARALLEL:
    printf("%s", astb.atypes[A_TYPEG(ast)]);
    if (A_IFPARG(ast)) {
      printf(", if-par: ");
      prettyprint_ast_with_attr(A_IFPARG(ast), &default_arg_attr);
    }
    printf("\n");
    break;

  case A_MP_TEAMS:
    prettyprint_mp_teams(ast, attr);
    break;

  case A_MP_TARGET:
    prettyprint_mp_target(ast, attr);
    break;

  case A_MP_PDO:
    prettyprint_do(ast, attr);
    break;

  case A_MP_REDUCTIONITEM:
    prettyprint_mp_reductionitem(ast, attr);
    break;

  case A_MP_ATOMICUPDATE: case A_MP_ATOMICREAD: case A_MP_ATOMICWRITE:
    prettyprint_mp_atomic_kind(ast, attr);
    break;

  default:
    printf(get_termstr(termstr_red));
    printf("Unhandled ast-type %d, from ast.h:\n", A_TYPEG(ast));
    sprintf(cmdline,
        "grep '#define A_.* %d$' "FLANG_BUILD_PATH"/tools/flang1/utils/ast/ast.h",
        A_TYPEG(ast));
    system(cmdline);
    printf(get_termstr(termstr_clear));
    break;
  }

  /* else it must be an intermediate ast for the upcoming ast, so we'll "group"
   * it with it */
skip:
  return;
}

void prettyprint_ast(int ast) {
  struct prettyprint_attr attr = {
    .newline = true,
    .expand_obj = false,
    .lineno = true
  };

  prettyprint_ast_with_attr(ast, &attr);
}

void prettyprint_ast_currsub() {
  printf("ast dump of %s:\n", prettystr_func_signature(gbl.currsub));
  ast_visit(1, 1);
  for (int std = STD_NEXT(0); std > 0; std = STD_NEXT(std)) {
    int ast;
    int curr_lineno = STD_LINENO(std);

    ast = STD_AST(std);
    ast_traverse(ast, NULL, prettyprint_ast, &curr_lineno);
  }

  ast_unvisit();
  printf("\n\n");
}
#endif

#ifdef PRETTYPRINT_FLANG2

void prettyprint_ili_with_attr(int i, struct prettyprint_attr *attr);
void prettyprint_ili(int i);

static bool prettyutil_is_ili_con(int i) {
  ILI_OP opc = ILI_OPC(i);

  switch (opc) {
  case IL_ACON: case IL_ICON: case IL_KCON: case IL_FCON: case IL_DCON:
    return true;
  }

  return false;
}

static bool prettyutil_is_ilio_lnk(ILI_OP opc, int opri) {
  switch (IL_OPRFLAG(opc, opri)) {
  case ILIO_LNK: case ILIO_IRLNK: case ILIO_KRLNK: case ILIO_ARLNK:
#ifdef ILIO_PPLNK
  case ILIO_PPLNK:
#endif
  case ILIO_SPLNK: case ILIO_DPLNK: case ILIO_QPLNK: case ILIO_CSLNK: case ILIO_CDLNK:
    return true;
  }

  return false;
}

static bool prettyutil_is_ilio_reg(ILI_OP opc, int opri) {
  switch (IL_OPRFLAG(opc, opri)) {
  case ILIO_IR: case ILIO_KR: case ILIO_AR: case ILIO_SP:
  case ILIO_DP: case ILIO_CS: case ILIO_CD: case ILIO_XMM:
    return true;
  }

  return false;
}

static void prettyprint_ili_opns(int i) {
  ILI_OP opc = ILI_OPC(i);
  int noprs = ilis[opc].oprs;
  struct prettyprint_attr default_arg_attr;
  default_arg_attr.is_ili_opn = true;

  static char *cond[] = {"eq", "ne", "lt", "ge", "le", "gt",
    "noteq", "notne", "notlt", "notge", "notle", "notgt"};
  static char *msz;

  /* Yes, it <= noprs. ugghh this ruined an evening of mine */
  for (int opri = 1; opri <= noprs; opri++) {
    int opn = ILI_OPND(i, opri);

    /* If it's a lnk (ie. a reference) to another ILI (denoted by, eg. ^45)
     * then directly print what it is */
    if (prettyutil_is_ilio_lnk(opc, opri)) {
      prettyprint_ili_with_attr(opn, &default_arg_attr);
      if (opri != noprs)
        printf(", ");
      continue;
    }

    /* skip those register defs like sp(1), ir(1) etc. */
    if (prettyutil_is_ilio_reg(opc, opri))
      continue;

    switch (IL_OPRFLAG(opc, opri)) {

    /* The great thing if opn is a symbol is that opn itself is an "int sptr"!
     * Yayy */
    case ILIO_SYM:
      if(opc != IL_ACON) {
        printf("%s%s %s%s", get_termstr(termstr_green), prettystr_symtype(opn),
            get_termstr(termstr_clear), prettystr_sym(opn));

      } else {
        if (CONVAL1G(opn)) {
          printf("%s ", prettystr_sym(opn));
          printf("<");
          printf("%s%s %s%s", get_termstr(termstr_green),
              prettystr_dt(DTYPEG(CONVAL1G(opn))),
              get_termstr(termstr_clear), prettystr_sym(CONVAL1G(opn)));
        } else
          printf("<%d", CONVAL1G(opn));

        printf(",%" ISZ_PF "d>", ACONOFFG(opn));
      }
      if (opri != noprs)
        printf(", ");

      break;

    case ILIO_NME:
      __dumpname(stdout, opn);

      if (opri != noprs)
        printf(", ");

      break;

    /* STC means store condition is it ? */
    case ILIO_STC:
      switch (opc) {
      case IL_ICMP: case IL_FCMP: case IL_DCMP: case IL_ACMP:
      case IL_ICMPZ: case IL_FCMPZ: case IL_DCMPZ: case IL_ACMPZ:
      case IL_ICJMP: case IL_FCJMP: case IL_DCJMP: case IL_ACJMP:
      case IL_ICJMPZ: case IL_FCJMPZ: case IL_DCJMPZ: case IL_ACJMPZ:
#ifdef TM_LPCMP
      case IL_ICLOOP: case IL_FCLOOP: case IL_DCLOOP: case IL_ACLOOP:
      case IL_ICLOOPZ: case IL_FCLOOPZ: case IL_DCLOOPZ: case IL_ACLOOPZ:
#endif
      case IL_UICMP: case IL_UICMPZ: case IL_UICJMP: case IL_UICJMPZ:
      case IL_KCJMP: case IL_KCJMPZ: case IL_KCMP: case IL_KCMPZ:
      case IL_UKCJMP: case IL_UKCJMPZ: case IL_UKCMP: case IL_UKCMPZ:
      case IL_LCJMPZ:
#ifdef IL_X87CMP
      case IL_X87CMP:
#endif
#ifdef IL_DOUBLEDOUBLECMP
      case IL_DOUBLEDOUBLECMP:
#endif
        printf("stc-cond: %s ", cond[opn - 1]);
        break;

      case IL_LD: case IL_ST: case IL_LDKR: case IL_STKR:
        /* msz = dump_msz(ConvertMSZ(opn)); */
        msz = dump_msz((MSZ)opn);
        printf("stc-msz %s ", msz);
        break;
      }
      break;

    default:
      printf("[X ilio: %d] ", IL_OPRFLAG(opc, opri));
      break;
    }
  }
}

static bool is_jsr(int opc)
{
  switch (opc) {
  case IL_JSR: case IL_JSRA: case IL_QJSR: case IL_GJSR: case IL_GJSRA:
#ifdef LONG_DOUBLE_FLOAT128
  case IL_FLOAT128RESULT:
#endif
    return true;
  }
  return false;
}


void prettyprint_ili_with_attr(int i, struct prettyprint_attr *attr) {
  ILI_OP opc = ILI_OPC(i);

  /* Standalone CONs are skipped, but as operands
   * "<data-type> <value>"
   */
  if (prettyutil_is_ili_con(i)) {
    if (attr->is_ili_opn) {
      printf("(");
      prettyprint_ili_opns(i);
      printf(")");
    }
    return;
  }

  /* Standalone ARGs (GARG, ARGAR etc.) are skipped, but as operands
   * "<ARG-name> <operands>"
   */
  if (is_argili_opcode(opc)) {
    if (attr->is_ili_opn) {
      printf("(");
      printf("%s: ", ilis[opc].name);
      prettyprint_ili_opns(i);
      printf(")");
    }
    return;
  }

  /* Standalone DAs, ie "define function arg register" ILI like DAIR (for int
   * reg), DASP (for float reg) etc. are skipped, but as operands "<DA-name>
   * <operands>"
   */
  if (is_daili_opcode(opc)) {
    if (attr->is_ili_opn) {
      printf("(");
      printf("%s: ", ilis[opc].name);
      prettyprint_ili_opns(i);
      printf(")");
    }
    return;
  }

  /* Standalone DFRs ie. "define function result register" ILI like DFRIR (for
   * int reg), DFRSP (for float reg), DFRDP (for double reg) etc. are skipped,
   * but as operands: "<DFR-name> <operands>"
   */
  if (is_dfrili_opcode(opc)) {
    if (attr->is_ili_opn) {
      printf("(");
      printf("%s: ", ilis[opc].name);
      prettyprint_ili_opns(i);
      printf(")");
    }
    return;
  }

  /* All other ILIs participating as operands as referenced by their index in
   * the format: "^<idx>"
   */
  if (attr->is_ili_opn) {
    printf("(^%d)", i);
    return;
  }

  printf("%d %s ", i, ilis[opc].name);
  if (ILI_ALT(i))
    printf("<alt> ");

  prettyprint_ili_opns(i);
  if (!attr->is_ili_opn) {
    printf("\n");
  }
}

void prettyprint_ili(int i) {
  struct prettyprint_attr attr;
  attr.is_ili_opn = false;

  prettyprint_ili_with_attr(i, &attr);
}

void prettyprint_ilis(int from, int n) {
  for (int i = from; i < n; i++) {
    prettyprint_ili(i);
  }
}

void prettyprint_ili_currsub() {
  prettyprint_ilis(1, ilib.stg_avail);
  printf("\n\n");
}

void prettyprint_all_bihs() {
  int block = BIHNUMG(gbl.currsub);

  for (; block; block = BIH_NEXT(block)) {
    if (BIH_LAST(block))
      break;
    printf("%d %s\n", BIH_LABEL(block), prettystr_sym(BIH_LABEL(block)));
  }
}

#endif

#endif
