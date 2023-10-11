


An array: []
  any[]

any[any]


An associative array: #[]
  byte[byte]

A tuple: '[]

A char:
A number:

foo(ubyte b, ubyte a = 0)


A string:




recv fid {
  case #[a = 1]:




void start() {
    ackermann(2, 2);
}

num ackermann(num m, num n) {
    if (m == 0) {
        n + 1;
    } else if (n == 0 && m > 0) {
        ackermann(m - 1, 1);
    } else if (m > 0 && n > 0) {
        ackermann(m - 1, ackermann(m, n - 1));
    }
}

void ackermann(m, n) {
    match '{m, n} {
        case '(0, _):
            n + 1;
        case '(_, n) when m > 0:
            ackermann(m - 1, 1);
        case '(_, _) when m > 0 && n > 0:
            ackermann(m - 1, ackermann(m, n - 1));
    }
}

'[ubyte, ubyte] foo() {
  '[0, 1];
}

[ubyte] foo() {
  [0, 1, 2]
}

[0, :a , :c] foo() {
  [0, :a, :c]
}

ulong[ulong] bar() {

}

struct foo {
  static foo = 3;
  private ubyte bar;
