// test atom storage

#include <stdio.h>
#include <stdlib.h>

#include "atom.h"

char* atom[] = {
    "true",
    "false",
    "nil",
    "a",
    "b",
    "c",
    NULL
};

void test()
{
    atom_table_t tab;
    int i;
    
    atom_init(&tab);

    i = 0;
    while(atom[i] != NULL) {
	char* atm;	
	atom_insert(&tab, atom[i], NULL);
	if (atom_find(&tab, atom[i], &atm)) {
	    printf("insert: atom = %s (%p)\n", atm, atm);
	}	
	i++;
    }
    i = 0;
    while(atom[i] != NULL) {
	char* atm;
	if (atom_find(&tab, atom[i], &atm)) {
	    printf("atom = %s (%p)\n", atm, atm);
	}
	else {
	    printf("atom %s not found\n", atom[i]);
	}
	i++;
    }
    atom_reset(&tab);
}

int main(int argc, char** argv)
{
    (void) argc;
    (void) argv;
    test();
    exit(0);
}
