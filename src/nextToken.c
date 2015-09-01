#include <stdio.h>

#define MAX_STRING_SIZE 100000
static char token[MAX_STRING_SIZE];

#define SEPARATOR ','

char *
nextToken(FILE *f, int *end)
{
    token[0] = '\0';
    int pos = 0;
    char c;

    while( (c =  getc(f) )) {
	if(c == SEPARATOR || c == '\n' || c == EOF) {
	    break;
	}

	token[pos++] = c;
    }
    if(c == EOF && end)
	*end = 1;
    
    if(pos) {
	token[pos] = '\0';
	return(token);
    }
    return(NULL);
}

#include <Rdefines.h>

SEXP
R_test(SEXP r_file, SEXP r_n)
{
    FILE *f = fopen(CHAR(STRING_ELT( r_file, 0)), "r");
    if(!f) {
	PROBLEM "cannot open %s", CHAR(STRING_ELT( r_file, 0))
	    ERROR;
    }
    SEXP ans;
    int i =  0, n, end = 0;
    n = INTEGER(r_n)[0];
    PROTECT(ans = NEW_CHARACTER(n));
    char *el;
    while(i < n) {
	el = nextToken(f,  &end);
	if(el)
	    SET_STRING_ELT(ans, i, mkChar(el));
	else
	    break;
	i++;
    }
    UNPROTECT(1);
    return(ans);
}
