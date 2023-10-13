#include <cmath>
#include <iostream>
#include <fstream>
#include <map>
#include <regex>
#include <stack>
#include <string>
using namespace std;
typedef vector<int> vi;

enum type {
    RM = 0,
    RR = 1,
    RI = 2,
    J = 3
};

enum code {
    HALT = 0,
    SYSCALL = 1,
    ADD = 2,
    ADDI = 3,
    SUB = 4,
    SUBI = 5,
    MUL = 6,
    MULI = 7,
    DIV = 8,
    DIVI = 9,
    LC = 12,
    SHL = 13,
    SHLI = 14,
    SHR = 15,
    SHRI = 16,
    AND = 17,
    ANDI = 18,
    OR = 19,
    ORI = 20,
    XOR = 21,
    XORI = 22,
    NOT = 23,
    MOV = 24,
    ADDD = 32,
    SUBD = 33,
    MULD = 34,
    DIVD = 35,
    ITOD = 36,
    DTOI = 37,
    PUSH = 38,
    POP = 39,
    CALL = 40,
    CALLI = 41,
    RET = 42,
    CMP = 43,
    CMPI = 44,
    CMPD = 45,
    JMP = 46,
    JNE = 47,
    JEQ = 48,
    JLE = 49,
    JL = 50,
    JGE = 51,
    JG = 52,
    LOAD = 64,
    STORE = 65,
    LOAD2 = 66,
    STORE2 = 67,
    LOADR = 68,
    LOADR2 = 69,
    STORER = 70,
    STORER2 = 71,
    SKIP = -1
};

struct command {
    int code;
    int type;

    void (*func)(vi);
};

union un {
    double dbl;
    struct { int x, y; };
};

union long_un {
    long long ll;
    struct { int x, y; };
};

union word {
    int res;
    struct { char a, b, c, d; };
};

map<string, code> map_of_codes = {{"halt", HALT}, {"syscall", SYSCALL}, {"add", ADD}, {"addi", ADDI},
                                  {"sub", SUB}, {"subi", SUBI}, {"mul", MUL}, {"muli", MULI},
                                  {"div", DIV}, {"divi", DIVI}, {"lc", LC}, {"shl", SHL},
                                  {"shli", SHLI}, {"shr", SHR}, {"shri", SHRI}, {"and", AND},
                                  {"andi", ANDI}, {"or", OR}, {"ori", ORI}, {"xor", XOR},
                                  {"xori", XORI}, {"not", NOT}, {"mov", MOV}, {"addd", ADDD},
                                  {"subd", SUBD}, {"muld", MULD}, {"divd", DIVD}, {"itod", ITOD},
                                  {"dtoi", DTOI}, {"push", PUSH}, {"pop", POP}, {"call", CALL},
                                  {"calli", CALLI}, {"ret", RET}, {"cmp", CMP}, {"cmpi", CMPI},
                                  {"cmpd", CMPD}, {"jmp", JMP}, {"jne", JNE}, {"jeq", JEQ},
                                  {"jle", JLE}, {"jl", JL}, {"jge", JGE}, {"jg", JG},
                                  {"load", LOAD}, {"store", STORE}, {"load2", LOAD2}, {"store2", STORE2},
                                  {"loadr", LOADR}, {"loadr2", LOADR2}, {"storer", STORER}, {"storer2", STORER2}};

static int regs[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (1 << 20) - 1, 0};

int cmp_flag = 0;
bool exit0 = false;

static int mem[1 << 20];     // command and {vars}
static map<string, int> marks;                                  // stores the marks (marks -> address)
static map<string, double> consts;                              // stores constants

void halt (vi arr) {
    exit0 = true;
}

void syscall (vi arr) {
    switch (arr[1]) {
        case 0:
            exit0 = true;
            break;
        case 100: {
            int num; scanf("%d", &num);
            regs[arr[0]] = num;
            break;
        }
        case 101: {
            double dbl; scanf("%lf", &dbl);
            un u = {dbl};
            regs[arr[0]] = u.x;
            regs[arr[0] + 1] = u.y;
            break;
        }
        case 102:
            printf("%d", regs[arr[0]]);
            break;
        case 103: {
            un u{};
            u.x = regs[arr[0]];
            u.y = regs[arr[0] + 1];
            printf("%lg", u.dbl);
            break;
        }
        case 104:
            char ch;
            scanf("%c", &ch);
            regs[arr[0]] = static_cast<int>(ch);
            break;
        case 105:
            printf("%c", regs[arr[0]]);
            break;
    }
}

void add (vi arr) {
    regs[arr[0]] += regs[arr[1]] + arr[2];
}

void addi (vi arr) {
    regs[arr[0]] += arr[1];
}

void sub (vi arr) {
    regs[arr[0]] -= (regs[arr[1]] + arr[2]);
}

void subi (vi arr) {
    regs[arr[0]] -= arr[1];
}

void mul (vi arr) {
    long long res = static_cast<long long>(regs[arr[0]]) * (static_cast<long long>(regs[arr[1]]) + arr[2]);
    long_un u{res};
    regs[arr[0]] = u.x;
    regs[arr[0] + 1] = u.y;
}

void muli (vi arr) {
    long long res = static_cast<int64_t>(regs[arr[0]]) * static_cast<int64_t>(arr[1]);
    long_un u{res};
    regs[arr[0]] = u.x;
    regs[arr[0] + 1] = u.y;
}

void div (vi arr) {
    long_un u{};
    u.x = regs[arr[0]];
    u.y = regs[arr[0] + 1];
    regs[arr[0]] = static_cast<int32_t>(u.ll / (regs[arr[1]] + arr[2]));
    regs[arr[0] + 1] = static_cast<int32_t>(u.ll % (regs[arr[1]] + arr[2]));
}

void divi (vi arr) {
    long_un u{};
    u.x = regs[arr[0]];
    u.y = regs[arr[0] + 1];
    regs[arr[0]] = static_cast<int32_t>(u.ll / arr[1]);
    regs[arr[0] + 1] = static_cast<int32_t>(u.ll % arr[1]);
}

void lc (vi arr) {
    regs[arr[0]] = arr[1];
}

void shl (vi arr) {
    regs[arr[0]] <<= (regs[arr[1]] + arr[2]);
}

void shli (vi arr) {
    regs[arr[0]] <<= arr[1];
}

void shr (vi arr) {
    regs[arr[0]] >>= (regs[arr[1]] + arr[2]);
}

void shri (vi arr) {
    regs[arr[0]] >>= arr[1];
}

void and_ (vi arr) {
    regs[arr[0]] &= (regs[arr[1]] + arr[2]);
}

void andi (vi arr) {
    regs[arr[0]] &= arr[1];
}

void or_ (vi arr) {
    regs[arr[0]] |= (regs[arr[1]] + arr[2]);
}

void ori (vi arr) {
    regs[arr[0]] |= arr[1];
}

void xor_ (vi arr) {
    regs[arr[0]] ^= (regs[arr[1]] + arr[2]);
}

void xori (vi arr) {
    regs[arr[0]] ^= arr[1];
}

void not_ (vi arr) {
    regs[arr[0]] = ~regs[arr[0]];
}

void mov (vi arr) {
    regs[arr[0]] = static_cast<int32_t>(regs[arr[1]] + arr[2]);
}

void addd (vi arr) {
    un u1{}, u2{};
    u1.x = regs[arr[0]]; u1.y = regs[arr[0] + 1];
    u2.x = regs[arr[1]]; u2.y = regs[arr[1] + 1];

    u1.dbl += u2.dbl;
    regs[arr[0]] = u1.x; regs[arr[0] + 1] = u1.y;
}

void subd (vi arr) {
    un u1{}, u2{};
    u1.x = regs[arr[0]]; u1.y = regs[arr[0] + 1];
    u2.x = regs[arr[1]]; u2.y = regs[arr[1] + 1];

    u1.dbl -= u2.dbl;
    regs[arr[0]] = u1.x; regs[arr[0] + 1] = u1.y;
}

void muld (vi arr) {
    un u1{}, u2{};
    u1.x = regs[arr[0]]; u1.y = regs[arr[0] + 1];
    u2.x = regs[arr[1]]; u2.y = regs[arr[1] + 1];

    u1.dbl *= u2.dbl;
    regs[arr[0]] = u1.x; regs[arr[0] + 1] = u1.y;
}

void divd (vi arr) {
    un u1{}, u2{};
    u1.x = regs[arr[0]]; u1.y = regs[arr[0] + 1];
    u2.x = regs[arr[1]]; u2.y = regs[arr[1] + 1];

    u1.dbl /= u2.dbl;
    regs[arr[0]] = u1.x; regs[arr[0] + 1] = u1.y;
}

void itod (vi arr) {
    auto dbl = static_cast<double>(regs[arr[1]]);
    un u{dbl};
    regs[arr[0]] = u.x; regs[arr[0] + 1] = u.y;
}

void dtoi (vi arr) {
    un u{};
    u.x = regs[arr[1]]; u.y = regs[arr[1] + 1];
    regs[arr[0]] = static_cast<int32_t>(trunc(u.dbl));
}

void push (vi arr) {
    mem[--regs[14]] = regs[arr[0]] + arr[1];
}

void pop (vi arr) {
    regs[arr[0]] = mem[regs[14]++] + arr[1];
}

static stack<int> st;
void call (vi arr) {
    mem[--regs[14]] = regs[15] + 1;
    //st.push(regs[15] + 1);
    regs[15] = regs[arr[1]] + mem[5] + 127;
    regs[arr[0]] = regs[15];
}

void calli (vi arr) {
    mem[--regs[14]] = regs[15] + 1;
    //st.push(regs[15] + 1);
    regs[15] = arr[0] + mem[5] + 127;
}

void ret (vi arr) {
    regs[15] = mem[regs[14]] - 1;
    regs[14] += (arr[1] + 1);
    //regs[15] = st.top() - 1;
    //cout << "stack: " << st.top() << '\n';
    //st.pop();
}

void cmp (vi arr) {
    if (regs[arr[0]] > regs[arr[1]])
        cmp_flag = 1;
    else if (regs[arr[0]] == regs[arr[1]])
        cmp_flag = 0;
    else
        cmp_flag = -1;
}

void cmpi (vi arr) {
    if (regs[arr[0]] > arr[1])
        cmp_flag = 1;
    else if (regs[arr[0]] == arr[1])
        cmp_flag = 0;
    else
        cmp_flag = -1;
}

void cmpd (vi arr) {
    un u1{}, u2{};
    u1.x = regs[arr[0]]; u1.y = regs[arr[0] + 1];
    u2.x = regs[arr[1]]; u2.y = regs[arr[1] + 1];

    if (u1.dbl < u2.dbl)
        cmp_flag = -1;
    else if (u1.dbl == u2.dbl)
        cmp_flag = 0;
    else
        cmp_flag = 1;
}

void jmp (vi arr) {
    regs[15] = arr[0] - 1 + mem[5] + 128;
}

void jne (vi arr) {
    if (cmp_flag)
        regs[15] = arr[0] - 1 + mem[5] + 128;
}

void jeq (vi arr) {
    if (!cmp_flag)
        regs[15] = arr[0] - 1 + mem[5] + 128;
}

void jle (vi arr) {
    if (cmp_flag != 1)
        regs[15] = arr[0] - 1 + mem[5] + 128;
}

void jl (vi arr) {
    if (cmp_flag == -1)
        regs[15] = arr[0] - 1 + mem[5] + 128;
}

void jge (vi arr) {
    if (cmp_flag != -1)
        regs[15] = arr[0] - 1 + mem[5] + 128;
}

void jg (vi arr) {
    if (cmp_flag == 1)
        regs[15] = arr[0] - 1 + mem[5] + 128;
}

void load (vi arr) {
    regs[arr[0]] = mem[arr[1]];
}

void store (vi arr) {
    mem[arr[1]] = regs[arr[0]];
}

void load2 (vi arr) {
    regs[arr[0]] = mem[arr[1]];
    regs[arr[0] + 1] = mem[arr[1] + 1];
}

void store2 (vi arr) {
    mem[arr[1]] = regs[arr[0]];
    mem[arr[1] + 1] = regs[arr[0] + 1];
}

void loadr (vi arr) {
    regs[arr[0]] = mem[regs[arr[1]] + arr[2]];
}

void loadr2 (vi arr) {
    regs[arr[0]] = mem[regs[arr[1]] + arr[2]];
    regs[arr[0] + 1] = mem[regs[arr[1]] + arr[2] + 1];
}

void storer (vi arr) {
    mem[arr[1] + arr[2]] = regs[arr[0]];
}

void storer2 (vi arr) {
    mem[regs[arr[1]] + arr[2]] = regs[arr[0]];
    mem[regs[arr[1]] + arr[2] + 1] = regs[arr[0] + 1];
}

void skip (vi arr) {

}

vector<command> funcs {
        {HALT, RI, halt}, {SYSCALL, RI, syscall}, {ADD, RR, add},
        {ADDI, RI, addi}, {SUB, RR, sub}, {SUBI, RI, subi},
        {MUL, RR, mul}, {MULI, RI, muli}, {DIV, RR, div},
        {DIVI, RI, divi}, {SKIP, J, skip}, {SKIP, J, skip}, // 10 - 11
        {LC, RI, lc}, {SHL, RR, shl}, {SHLI, RI, shli},
        {SHR, RR, shr}, {SHRI, RI, shri}, {AND, RR, and_},
        {ANDI, RI, andi}, {OR, RR, or_}, {ORI, RI, ori},
        {XOR, RR, xor_}, {XORI, RI, xori}, {NOT, RI, not_},
        {MOV, RR, mov}, {SKIP, J, skip}, {SKIP, J, skip},   // 25 - 26
        {SKIP, J, skip}, {SKIP, J, skip}, {SKIP, J, skip},  // 27 - 29
        {SKIP, J, skip}, {SKIP, J, skip}, {ADDD, RR, addd}, // 30 - 32
        {SUBD, RR, subd}, {MULD, RR, muld}, {DIVD, RR, divd},
        {ITOD, RR, itod}, {DTOI, RR, dtoi}, {PUSH, RI, push},
        {POP, RI, pop}, {CALL, RR, call}, {CALLI, J, calli},
        {RET, RI, ret}, {CMP, RR, cmp}, {CMPI, RI, cmpi},
        {CMPD, RR, cmpd}, {JMP, J, jmp}, {JNE, J, jne},
        {JEQ, J, jeq}, {JLE, J, jle}, {JL, J, jl},
        {JGE, J, jge}, {JG, J, jg}, {SKIP, J, skip},        // 53
        {SKIP, J, skip}, {SKIP, J, skip}, {SKIP, J, skip},  // 54 - 56
        {SKIP, J, skip}, {SKIP, J, skip}, {SKIP, J, skip},  // 57 - 59
        {SKIP, J, skip}, {SKIP, J, skip}, {SKIP, J, skip},  // 60 - 62
        {SKIP, J, skip}, {LOAD, RM, load}, {STORE, RM, store}, // 63 - 65
        {LOAD2, RM, load2}, {STORE2, RM, store2}, {LOADR, RR, loadr},
        {STORER, RR, storer}, {LOADR2, RR, loadr2}, {STORER2, RR, storer2}
};

int main() {
    FILE* bf = fopen("test11.in", "rb");
    int sz = 0, code_sz = 0, data_sz = 0, consts_sz = 0, start_pos = 0;
    char* buf;

    if (bf) {
        fseek(bf, 0, SEEK_END);
        sz = ftell(bf);
        rewind(bf);

        buf = (char*) malloc(sz * sizeof(char));
        fread(buf, sizeof(char), sz, bf);
        memcpy(mem, buf, sz);

        start_pos = mem[7] + 128;
        code_sz = mem[4];
        consts_sz = mem[5];
        data_sz = mem[6];

        free(buf);
        fclose(bf);
    }

    for (regs[15] = start_pos; regs[15] < mem[5] + 128 + code_sz; regs[15]++) {
        int cur = mem[regs[15]];
        switch (funcs[cur >> 24].type) {
            case RR: {
                funcs[cur >> 24].func({(cur >> 20) & 0xF, (cur >> 16) & 0xF, cur & 0xFFFF});
                break;
            }
            case RI:
            case RM: {
                funcs[cur >> 24].func({(cur >> 20) & 0xF, cur & 0xFFFFF});
                break;
            }
            case J: {
                funcs[cur >> 24].func({cur & 0xFFFFF});
                break;
            }
        }

        if (exit0)
            break;
    }

    return 0;
}