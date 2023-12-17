//

#include <stdio.h>

#include "atom.h"
#include "pred.h"
#include "pred_format.h"
#include "pred_parse.h"

// align struc
typedef struct name_t {
    double __dummy__;  // force alignment?
    pred_atom_t name[];
} name_t;

name_t name_x = { .name = "\1x" };
name_t name_y = { .name = "\1y" };
name_t name_z = { .name = "\1z" };
name_t name_map = { .name = "\3map" };
name_t name_pair = { .name = "\4pair" };
name_t name_foo = { .name = "\3foo" };
name_t name_bar = { .name = "\3bar" };
name_t name_pi = { .name = "\2pi" };
name_t name_f = { .name = "\1f" };
name_t name_P = { .name = "\1P" };

pred_atom_t* atm_x = name_x.name;
pred_atom_t* atm_y = name_y.name;
pred_atom_t* atm_z = name_z.name;
pred_atom_t* atm_map = name_map.name;
pred_atom_t* atm_pair = name_pair.name;
pred_atom_t* atm_foo = name_foo.name;
pred_atom_t* atm_bar = name_bar.name;
pred_atom_t* atm_pi = name_pi.name;
pred_atom_t* atm_f = name_f.name;
pred_atom_t* atm_P = name_P.name;



void test_case(pred_t* p, parg_t* args, char* expect)
{
    char buf[1024];
    
    pred_format(p, args, buf, sizeof(buf));
    if (strcmp(buf, expect) != 0) {
	fprintf(stderr, "unexpected result %s - expected %s\n",
		buf, expect);
	exit(1);
    }
}

void test_format()
{
    // "pi()"
    parg_t p_0[] = { parg_make_atm(atm_pi),
		     parg_make_ari(0),
                   };
    // "f(0)"
    parg_t p_1[] = { parg_make_atm(atm_f),
		     parg_make_ari(1),
		     parg_make_int(0) };
    // "f(x)"    
    parg_t p_2[] = { parg_make_atm(atm_f),
		     parg_make_ari(1),
		     parg_make_atm(atm_x) };
    // "pair(x,100)"
    parg_t p1[] = { parg_make_atm(atm_pair),
		    parg_make_ari(2),
		    parg_make_atm(atm_x),
		    parg_make_int(100) };
    // "pair(y,200)"    
    parg_t p2[] = { parg_make_atm(atm_pair),
		    parg_make_ari(2),
		    parg_make_atm(atm_y),
		    parg_make_int(200) };
    // "pair(z,-12)"
    parg_t p3[] = { parg_make_atm(atm_pair),
		    parg_make_ari(2),
		    parg_make_atm(atm_z),
		    parg_make_int(-12) };
    // "map(pair(x,100),pair(y,200),pair(z,-12))"
    parg_t map[] = { parg_make_atm(atm_map),
		     parg_make_ari(3),
		     parg_make_app(p1),
		     parg_make_app(p2),
		     parg_make_app(p3) };
    parg_t t_[]  = { parg_make_ari(0) };
    parg_t t_1[]  = { parg_make_ari(1), parg_make_int(1) };
    parg_t t_123[]  = { parg_make_ari(3),
			parg_make_int(1),
			parg_make_int(2),
			parg_make_int(3) };
    parg_t l_12[]  = { parg_make_int(1),
		       parg_make_int(2) };
    
    // integers
    parg_t p_arg_int_0[] = { parg_make_int(0) };
    parg_t p_arg_int_1[] = { parg_make_int(1) };
    parg_t p_arg_int_n1[] = { parg_make_int(-1) };
    parg_t p_arg_int_13[] = { parg_make_int(13) };
    parg_t p_arg_int_n999[] = { parg_make_int(-999) };

    // variables | atoms | names
    parg_t p_arg_atm_x[]    = { parg_make_atm(atm_x) };
    parg_t p_arg_atm_f[]    = { parg_make_atm(atm_f) };
    parg_t p_arg_atm_foo[]  = { parg_make_atm(atm_foo) };

    // function application
    parg_t p_arg_app_pi[]      = { parg_make_app(p_0) };
    parg_t p_arg_app_f0[]      = { parg_make_app(p_1) };
    parg_t p_arg_app_fx[]      = { parg_make_app(p_2) };
    parg_t p_arg_app_pair_x[]  = { parg_make_app(p1) };
    parg_t p_arg_app_map_xyz[] = { parg_make_app(map) };

    // tuple
    parg_t p_arg_tpl_empty[]   = { parg_make_tpl(t_) };
    parg_t p_arg_tpl_1[]       = { parg_make_tpl(t_1) };
    parg_t p_arg_tpl_123[]     = { parg_make_tpl(t_123) };

    // list
    parg_t p_arg_lst_12[]      = { parg_make_lst(l_12) };

    pred_t p = { .name = atm_P, .type = PRED_TYPE_BOOL, .arity = 1 };

    // INTEGER 
    test_case(&p, p_arg_int_0,  "P(0)");
    test_case(&p, p_arg_int_1,  "P(1)");
    test_case(&p, p_arg_int_n1, "P(-1)");
    test_case(&p, p_arg_int_13, "P(13)");
    test_case(&p, p_arg_int_n999, "P(-999)");

    // ATOMS | variable
    test_case(&p, p_arg_atm_x, "P(x)");
    test_case(&p, p_arg_atm_f, "P(f)");
    test_case(&p, p_arg_atm_foo, "P(foo)");

    // FUNCTION APPLICATION
    test_case(&p, p_arg_app_pi, "P(pi())");
    test_case(&p, p_arg_app_f0, "P(f(0))");
    test_case(&p, p_arg_app_fx, "P(f(x))");
    test_case(&p, p_arg_app_pair_x, "P(pair(x,100))");
    test_case(&p, p_arg_app_map_xyz, "P(map(pair(x,100),pair(y,200),pair(z,-12)))");

    // TUPLE
    test_case(&p, p_arg_tpl_empty, "P({})");
    test_case(&p, p_arg_tpl_1, "P({1})");
    test_case(&p, p_arg_tpl_123, "P({1,2,3})");

    // LIST CELS
    test_case(&p, p_arg_lst_12, "P([1|2])");
}

void test_parse_parg_x(char* str, char* expect)
{
    char buf[1024];    
    char* ptr;
    atom_table_t tab;
    parg_t arg;

    fprintf(stderr, "TEST_PARSE: %s\n", str);
    
    atom_init(&tab);
    if ((ptr = parg_parse(str, &tab, &arg)) == NULL) {
	fprintf(stderr, "syntax error: %s\n", str);
	exit(1);
    }
    parg_format(arg, buf, sizeof(buf));
    if (strcmp(buf, expect) != 0) {
	fprintf(stderr, "parse: unexpected result %s - expected %s\n",
		buf, expect);
	exit(1);
    }
}

void test_parse_parg(char* str)
{
    test_parse_parg_x(str, str);
}

void test_calc_num_elements(char* str, int expect)
{
    int r;
    if ((r = parg_parse_num_elements(str+1, str[0])) != expect) {
	fprintf(stderr, "num_elements: '%s' unexpected result %d - expected %d\n",
		str, r, expect);
	exit(1);
    }
}

void test_calc()
{
    test_calc_num_elements("{}", 0);
    test_calc_num_elements("{ 12 }", 1);
    test_calc_num_elements("{ [], {} }", 2);
    test_calc_num_elements("{ -12, foo(x) }", 2);
    test_calc_num_elements("{ [a,b], 12, {1,{2},3} }", 3);
    test_calc_num_elements("{ f(), [1], [2], 3, {4,5}, 6 }", 6);

    test_calc_num_elements("()", 0);
    test_calc_num_elements("(1)", 1);
    test_calc_num_elements("([1],f(1,2))", 2);    
    test_calc_num_elements("( x, y, f(x,y))", 3);

    test_calc_num_elements("[]", 0);
    test_calc_num_elements("[1]", 1);
    test_calc_num_elements("[[1],f(1,2)]", 2);    
    test_calc_num_elements("[ x, y, f(x,y)]", 3);
}

void test_parse()
{
    test_parse_parg("0");
    test_parse_parg("1");
    test_parse_parg("-1");
    test_parse_parg("13");
    test_parse_parg("-999");
    test_parse_parg("x");
    test_parse_parg("f");
    test_parse_parg("foo");
    test_parse_parg("pi()");
    test_parse_parg("f(0)");
    test_parse_parg("f(x)");
    test_parse_parg("pair(x,100)");
    test_parse_parg("map(pair(x,100),pair(y,200),pair(z,-12))");
    test_parse_parg("{}");
    test_parse_parg("{1}");
    test_parse_parg("{1,2,3}");
    test_parse_parg_x("[]", "nil");
    test_parse_parg("[1]");
    test_parse_parg("[1,2]");
    test_parse_parg("[1|2]");
    test_parse_parg("[1,2,3]");
    test_parse_parg("[{p(x),p(y)},[1,2],{2,3},x]");
}


int main(int argc, char** argv)
{
    (void) argc;
    (void) argv;
    
    test_format();
    test_calc();
    test_parse();
    exit(0);
}
