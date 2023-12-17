//
//  Bignum library test
// 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BIGNUM_DEBUG  // bifnum_printf
#include "bignum.h"

// numbers used in some tests below

#define P1 "4205092373"
#define P2 "2648892151"
#define N  "11138836181069664323"  // N  = P1*P2 
#define L  "11138836174215679800"  // L = (P1-1)*(P2-1)
#define E  "65537"
#define D  "8048237016610999673"   // D = modinv(E,L)
//
// Message = <<"HELLO",0,0,0>> = 5207652434750472192
//
#define M    "5207652434750472192"
#define MDN  "1783597635534869275"  // M^D mod N (encrypted)

#define MDP1 "1797691939"           // M^D mod P1
#define MDP2 "882983471"            // M^D mod P2

#include <sys/time.h>
uint64_t time_tick(void)
{
    struct timeval t;
    gettimeofday(&t, 0);
    return t.tv_sec*(uint64_t)1000000 + t.tv_usec;
}

void test_1()
{
    AUTO_BEGIN {
	BIGNUM_AUTO(x, 3);
	BIGNUM_AUTO(y, 3);

	// fixme make bignum_from_string more precise with need
	bignum_from_string("12345678", &x);
	bignum_from_string("12345679", &y);
	if (!(bignum_comp(&x, &y) < 0)) { printf("ERROR\n"); return; }

	bignum_from_string("12345678", &x);
	bignum_from_string("12345678", &y);    
	if (!(bignum_comp(&x, &y) == 0)) { printf("ERROR\n"); return; }

	bignum_from_string("-12345678", &x);
	bignum_from_string("-12345678", &y);    
	if (!(bignum_comp(&x, &y) == 0)) { printf("ERROR\n"); return; }

	bignum_from_string("-12345678", &x);
	bignum_from_string("12345678", &y);    
	if (!(bignum_comp(&x, &y) < 0)) { printf("ERROR\n"); return; }
	
	printf("OK\n");
    } AUTO_END;	
}

// some bit stuff
void test_2()
{
    AUTO_BEGIN {    
	BIGNUM_AUTO(m, 9);

	bignum_from_string(M, &m);

	bignum_printf("m = %s\n", &m);
	printf("m #digits = %d\n", m.size);
	printf("m #bytes = %d\n", bignum_byte_size(&m));
	printf("m #bits  = %d\n", bignum_bit_size(&m));
	printf("m popcount = %d\n", bignum_popcount(&m));
	printf("n parity = %d\n", bignum_parity(&m));
    } AUTO_END;
}

void test_123456789()
{
    bignum_t a, b, c;

    bignum_halloc(&a, 8);
    bignum_halloc(&b, 8);
    bignum_halloc(&c, 8);

    bignum_from_string("3803", &a);
    bignum_from_string("3607", &b);
    bignum_multiply(&a, &b, &c);
    bignum_from_string("3", &a);
    bignum_multiply(&a, &c, &c);
    bignum_multiply(&a, &c, &c);
    bignum_printf("c = %s\n", &c);
}

void test_gcd()
{
    AUTO_BEGIN {        
	BIGNUM_AUTO(x, 8);
	BIGNUM_AUTO(y, 8);
	BIGNUM_DYNAMIC(gcd);

	bignum_from_string("123456789123456789", &x);
	bignum_from_string("10000001", &y);

	bignum_printf("x = %s\n", &x);
	bignum_printf("y = %s\n", &y);
	
	bignum_gcd(&x, &y, &gcd);
	bignum_printf("gcd = %s\n", &gcd);

	bignum_gcd(&y, &x, &gcd);
	bignum_printf("gcd = %s\n", &gcd);    
    } AUTO_END;
}

void test_gcd2()
{
    AUTO_BEGIN {    
	BIGNUM_AUTO(x, 8);
	BIGNUM_AUTO(y, 8);
	BIGNUM_DYNAMIC(q);
	BIGNUM_DYNAMIC(r);

	bignum_from_string("12345677677", &x);
	bignum_from_string("7779112", &y);
	
	bignum_printf("x = %s\n", &x);
	bignum_printf("y = %s\n", &y);

	bignum_divrem(&x, &y, &q, &r);
	
	bignum_printf("q = %s\n", &q);
	bignum_printf("r = %s\n", &r);
    } AUTO_END;
}

void test_egcd()
{
    AUTO_BEGIN {    
	BIGNUM_AUTO(x, 8);
	BIGNUM_AUTO(y, 8);
	BIGNUM_DYNAMIC(m1);
	BIGNUM_DYNAMIC(m2);
	BIGNUM_DYNAMIC(gcd);    

	bignum_from_string("123456789123456789", &x);
	bignum_from_string("10000001", &y);

	bignum_printf("x = %s\n", &x);
	bignum_printf("y = %s\n", &y);
	
	bignum_egcd(&x, &y, &gcd, &m1, &m2);
	bignum_printf("gcd = %s\n", &gcd);
	bignum_printf("m1 = %s\n", &m1);
	bignum_printf("m2 = %s\n", &m2);
    } AUTO_END;
}

//
// Calculate: powmod(M,D,N)
// M is message "HELLO\0\0\0"
//

void test_powmod()
{
    AUTO_BEGIN {
	BIGNUM_AUTO(d, 9);
	BIGNUM_AUTO(n, 9);
	BIGNUM_AUTO(m, 9);
	BIGNUM_AUTO(r, 9);
	char* ptr;
	char  rbuf[64];

	printf("test_powmod\n");
	bignum_from_string(D, &d);
	bignum_from_string(N, &n);
	bignum_from_string(M, &m);
    
	bignum_printf("m = %s\n", &m);
	bignum_printf("d = %s\n", &d);
	bignum_printf("n = %s\n", &n);
	
	bignum_powmod(&m, &d, &n, &r);

	ptr = bignum_to_string(&r, rbuf, sizeof(rbuf));
	if (strcmp(ptr, MDN) == 0)
	    printf("OK\n");
	else {
	    bignum_printf("r = %s\n", &r);
	    printf("ERROR\n");
	}
    } AUTO_END;
}

//
// Calculate: powmod(M,D,N) = powmod_two_prime(M,D,P1,P2)
// M is message "HELLO\0\0\0"
//

void test_powmod_two_prime()
{
    AUTO_BEGIN {
	BIGNUM_AUTO(d, 9);
	BIGNUM_AUTO(p1, 5);
	BIGNUM_AUTO(p2, 5);
	BIGNUM_AUTO(m, 9);
	BIGNUM_AUTO(r, 9);
	char* ptr;
	char  rbuf[64];

	printf("test_powmod_two_prime\n");    
	bignum_from_string(D, &d);
	bignum_from_string(P1, &p1);
	bignum_from_string(P2, &p2);
	bignum_from_string(M, &m);

	bignum_printf("m = %s\n", &m);
	bignum_printf("d = %s\n", &d);
	bignum_printf("p1 = %s\n", &p1);
	bignum_printf("p2 = %s\n", &p2);        
    
	bignum_powmod_two_prime(&m, &d, &p1, &p2, &r);

	ptr = bignum_to_string(&r, rbuf, sizeof(rbuf));
	if (strcmp(ptr, MDN) == 0)
	    printf("OK\n");
	else {
	    bignum_printf("r = %s\n", &r);
	    printf("ERROR\n");
	}
    } AUTO_END;
}

void test_powmod_prime()
{
    AUTO_BEGIN {
	BIGNUM_AUTO(d, 9);
	BIGNUM_AUTO(p1, 5);
	BIGNUM_AUTO(p2, 5);
	BIGNUM_AUTO(m, 9);
	BIGNUM_AUTO(r, 4);
	char* ptr;
	char  rbuf[64];

	printf("test_powmod_prime\n");    
	bignum_from_string(D, &d);
	bignum_from_string(P1, &p1);
	bignum_from_string(P2, &p2);
	bignum_from_string(M, &m);
	
	bignum_printf("m = %s\n", &m);
	bignum_printf("d = %s\n", &d);
	bignum_printf("p1 = %s\n", &p1);
	bignum_printf("p2 = %s\n", &p2);

	bignum_powmod_prime(&m, &d, &p1, &r);
	ptr = bignum_to_string(&r, rbuf, sizeof(rbuf));
	if (strcmp(ptr, MDP1) == 0)
	    printf("OK\n");
	else {
	    bignum_printf("m^d mod p1 = %s\n", &r);
	    printf("ERROR\n");
	}
	
	bignum_powmod_prime(&m, &d, &p2, &r);
	ptr = bignum_to_string(&r, rbuf, sizeof(rbuf));
	if (strcmp(ptr, MDP2) == 0)
	    printf("OK\n");
	else {
	    bignum_printf("m^d mod p2 = %s\n", &r);
	    printf("ERROR\n");
	}
    } AUTO_END;
}

#define K1024

#ifdef K1024
#include "key_1024.h"  // 0,12s, (-O = 0,03s )
#include "message_1024.h"
#endif

#ifdef K2048
#include "key_2048.h"  // 0,12s, (-O = 0,03s )
#include "message_2048.h"
#endif

#ifdef K4096
#include "key_4096.h"  // 0,12s, (-O = 0,03s )
#include "message_4096.h"
#endif

#define NUM_RSA_KEYS 1

// p1*p2 = n
typedef struct _rsakey_t {
    bignum_t d;     // private exponent
    bignum_t p1;    // prime1
    bignum_t p2;    // prime2
    bignum_t n;     // modulus
    bignum_t e;     // public exponent
} rsakey_t;

rsakey_t key[NUM_RSA_KEYS];

void key_init()
{
#ifdef  K1024
    bignum_const(&key[0].d, key_1024_d, sizeof(key_1024_d)/sizeof(digit_t));
    bignum_const(&key[0].n, key_1024_n, sizeof(key_1024_n)/sizeof(digit_t));
    bignum_const(&key[0].p1, key_1024_p1, sizeof(key_1024_p1)/sizeof(digit_t));
    bignum_const(&key[0].p2, key_1024_p2, sizeof(key_1024_p2)/sizeof(digit_t));
    bignum_const(&key[0].e, key_1024_e, sizeof(key_1024_e)/sizeof(digit_t));
#endif

#ifdef K2048
    bignum_const(&key[0].d, key_2048_d, sizeof(key_2048_d)/sizeof(digit_t));
    bignum_const(&key[0].n, key_2048_n, sizeof(key_2048_n)/sizeof(digit_t));
    bignum_const(&key[0].p1, key_2048_p1, sizeof(key_2048_p1)/sizeof(digit_t));
    bignum_const(&key[0].p2, key_2048_p2, sizeof(key_2048_p2)/sizeof(digit_t));
    bignum_const(&key[0].e, key_2048_e, sizeof(key_2048_e)/sizeof(digit_t));
#endif    

#ifdef K4096
    bignum_const(&key[0].d, key_4096_d, sizeof(key_4096_d)/sizeof(digit_t));
    bignum_const(&key[0].n, key_4096_n, sizeof(key_4096_n)/sizeof(digit_t));
    bignum_const(&key[0].p1, key_4096_p1, sizeof(key_4096_p1)/sizeof(digit_t));
    bignum_const(&key[0].p2, key_4096_p2, sizeof(key_4096_p2)/sizeof(digit_t));
    bignum_const(&key[0].e, key_4096_e, sizeof(key_4096_e)/sizeof(digit_t));
#endif    
}

int test_sign()
{
    uint64_t t0, t1;
    bignum_t msg;
    int rsz = key[0].n.size;

    bignum_const(&msg, m_ds, sizeof(m_ds)/sizeof(digit_t));
    printf("M[%lu] = ", msg.size*sizeof(digit_t)*8);
    bignum_xprintf("%s\n", &msg);
    
    AUTO_BEGIN {
	BIGNUM_AUTO(r, rsz);

	t0 = time_tick();
	bignum_powmod(&msg, &key[0].d, &key[0].n, &r);
	//bignum_powmod_two_prime(&msg, &key[0].d, &key[0].p1, &key[0].p2, &r);
	t1 = time_tick();
	printf("time = %lus, %luus\n",(t1-t0) / 1000000, (t1-t0) % 1000000);
	printf("S[%ld] = ", r.size*sizeof(digit_t)*8);
	bignum_xprintf("%s\n", &r);
    } AUTO_END;
    return 0;
}

int test_alloc()
{
    char* ptr;
    char  rbuf[64];
    
    AUTO_BEGIN {    
	BIGNUM_AUTO(x, 8);
	BIGNUM_AUTO(y, 8);
    
	bignum_from_string("3803", &x);
	bignum_from_string("4002", &y);
	AUTO_BEGIN {	
	    BIGNUM_DYNAMIC(z);
	    bignum_multiply(&x, &y, &z);

	    ptr = bignum_to_string(&z, rbuf, sizeof(rbuf));
	    if (strcmp(ptr, "15219606") == 0)
		printf("OK\n");
	    else {
		bignum_printf("z = %s\n", &z);
		printf("ERROR\n");
	    }	
	} AUTO_END;
    } AUTO_END;
    return 0;
}

int main()
{
    test_1();
    test_123456789();
    test_powmod();
    test_powmod_prime();
    test_powmod_two_prime();    

    test_gcd2();
    test_gcd();
    test_egcd();
    test_alloc(5);

    key_init(); test_sign();

    bignum_hinfo();
    exit(0);
}
