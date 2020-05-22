#include <stdio.h>      /* '#include' --> Include */
#define ZAP "SOMETEXT"    /* '#define' --> Macro */
#define N 100

/* NOTE NOTES */
/* URGENT */
// URGENT blabla
// URGENT
// DEPRECATED
// DEPRECATED A
// URGENT_
/* TODO */
#define NOTE 1
#define NOTE_1
#define URGENT
#define DEPRECATED
#define URGENT A
#define MYNOTE
#define TODO 
int URGENT = 1;

typedef int size; /* 'typedef' --> Structure */

/* Foo function */
int foo(void) {

	char c = 'A'   /* 'char' --> Type */
	const char* s = "My string";   /* 'const' --> StorageClass */
	float x = 5.67;
	float y = 58.67f;
	int a = 5 + 10;
	long b = 50000000L;

	for (y = 1 ; y < 100 ; ++y) {   /* 'for' --> Repeat */
		if (x == y)
			do_something();
		else
			do_something_else();
	}

	while (1) { /* 'while' --> Repeat */
		goto ici; /* 'goto' --> Statement */
	}
ici: /* Label */

	return 10;   /* 'return' --> Statement */
}

struct mystruct {   /* 'struct' --> Structure */
	int n;
	float y;
}
