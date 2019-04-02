/*
** $Id: lobject.c $
** Some generic functions over Lua objects
** See Copyright Notice in lua.h
*/

#define lobject_c
#define LUA_CORE

#include "lprefix.h"


#include <locale.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lua.h"

#include "lctype.h"
#include "ldebug.h"
#include "ldo.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "lvm.h"


/*
** converts an integer to a "floating point byte", represented as
** (eeeeexxx), where the real value is (1xxx) * 2^(eeeee - 1) if
** eeeee != 0 and (xxx) otherwise.
 * 使用浮点数的方法扩充整数范围(0 ~ 15 * 2^30) e代表指数
 * 采用了5尾指数，3 位尾数的方式 使得这个8位数的表示范围达到了 0 ~ 15 * 2^30
 * 如果传进来的x没有超出三尾数的范围那么就直接返回
*/
int luaO_int2fb (unsigned int x) {
  int e = 0;  /* exponent */
  if (x < 8) return x;
  while (x >= (8 << 4)) {  /* coarse steps */
    x = (x + 0xf) >> 4;  /* x = ceil(x / 16) */
    e += 4;
  }
  while (x >= (8 << 1)) {  /* fine steps */
    x = (x + 1) >> 1;  /* x = ceil(x / 2) */
    e++;
  }
  return ((e+1) << 3) | (cast_int(x) - 8);
}


/* converts back */
int luaO_fb2int (int x) {
  return (x < 8) ? x : ((x & 7) + 8) << ((x >> 3) - 1);
}


/*
** Computes ceil(log2(x))
 * 计算 ceil(log2(x)),也就是求出x的以2为底的对数，在对结果进行向上取整
 * 函数的大意就是，先把x转换成2^n的形式，然后求出n就可以了
 * 比如说传进来的是256，那么就是2^8 这里的log_2数组之所以这么写，就是为了按照这种思想求n
 * 如果传进来的数字在128和256之间  那么也用8来作为n的结果。目的就是实现ceil的向上取整
 * 如果x大于256，就让x除以256，然后把结果加上8
*/
int luaO_ceillog2 (unsigned int x) {
  static const lu_byte log_2[256] = {  /* log_2[i] = ceil(log2(i - 1)) */
    0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
    6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
  };
  int l = 0;
  x--;
  while (x >= 256) { l += 8; x >>= 8; }
  return l + log_2[x];
}

/* 两个整数运算 */
static lua_Integer intarith (lua_State *L, int op, lua_Integer v1,
                                                   lua_Integer v2) {
  switch (op) {
    case LUA_OPADD: return intop(+, v1, v2);
    case LUA_OPSUB:return intop(-, v1, v2);
    case LUA_OPMUL:return intop(*, v1, v2);
    case LUA_OPMOD: return luaV_mod(L, v1, v2);
    case LUA_OPIDIV: return luaV_idiv(L, v1, v2);
    case LUA_OPBAND: return intop(&, v1, v2);
    case LUA_OPBOR: return intop(|, v1, v2);
    case LUA_OPBXOR: return intop(^, v1, v2);
    case LUA_OPSHL: return luaV_shiftl(v1, v2);
    case LUA_OPSHR: return luaV_shiftl(v1, -v2);
    case LUA_OPUNM: return intop(-, 0, v1);
    case LUA_OPBNOT: return intop(^, ~l_castS2U(0), v1);
    default: lua_assert(0); return 0;
  }
}

/* 两个number运算 */
static lua_Number numarith (lua_State *L, int op, lua_Number v1,
                                                  lua_Number v2) {
  switch (op) {
    case LUA_OPADD: return luai_numadd(L, v1, v2);
    case LUA_OPSUB: return luai_numsub(L, v1, v2);
    case LUA_OPMUL: return luai_nummul(L, v1, v2);
    case LUA_OPDIV: return luai_numdiv(L, v1, v2);
    case LUA_OPPOW: return luai_numpow(L, v1, v2);
    case LUA_OPIDIV: return luai_numidiv(L, v1, v2);
    case LUA_OPUNM: return luai_numunm(L, v1);
    case LUA_OPMOD: return luaV_modf(L, v1, v2);
    default: lua_assert(0); return 0;
  }
}

/* 两个TValue的算术运算 */
int luaO_rawarith (lua_State *L, int op, const TValue *p1, const TValue *p2,
                   TValue *res) {
  switch (op) {
    case LUA_OPBAND: case LUA_OPBOR: case LUA_OPBXOR:
    case LUA_OPSHL: case LUA_OPSHR:
    case LUA_OPBNOT: {  /* operate only on integers 作用整型的操作符 */
      lua_Integer i1; lua_Integer i2;
      if (tointegerns(p1, &i1) && tointegerns(p2, &i2)) {
        setivalue(res, intarith(L, op, i1, i2));
        return 1;
      }
      else return 0;  /* fail */
    }
    case LUA_OPDIV: case LUA_OPPOW: {  /* operate only on floats 作用浮点数的操作符 */
      lua_Number n1; lua_Number n2;
      if (tonumberns(p1, n1) && tonumberns(p2, n2)) {
        setfltvalue(res, numarith(L, op, n1, n2));
        return 1;
      }
      else return 0;  /* fail */
    }
    default: {  /* other operations 其他 */
      lua_Number n1; lua_Number n2;
      if (ttisinteger(p1) && ttisinteger(p2)) {
        setivalue(res, intarith(L, op, ivalue(p1), ivalue(p2)));
        return 1;
      }
      else if (tonumberns(p1, n1) && tonumberns(p2, n2)) {
        setfltvalue(res, numarith(L, op, n1, n2));
        return 1;
      }
      else return 0;  /* fail */
    }
  }
}


void luaO_arith (lua_State *L, int op, const TValue *p1, const TValue *p2,
                 StkId res) {
  if (!luaO_rawarith(L, op, p1, p2, s2v(res))) {
    /* could not perform raw operation; try metamethod */
    luaT_trybinTM(L, p1, p2, res, cast(TMS, (op - LUA_OPADD) + TM_ADD));
  }
}

/* 把能转变成数字的字符变成数字并返回
 * lisdigit检查参数是否为十进制数字字符，当c为数字0~9时，返回非零值，否则返回零
 * 如果c是十进制字符的话，就返回具体的数字
 * 如果不是的话，ltolower就把字符转成小写字符之后，再减去'a'得到数字，加上10，就等于说16进制的字符变成了数字
 */
int luaO_hexavalue (int c) {
  if (lisdigit(c)) return c - '0';
  else return (ltolower(c) - 'a') + 10;
}

/*
 * 获取*s指向的字符是'+'还是'-'
 * 1代表负,0代表正,并且s向前移一位
 */
static int isneg (const char **s) {
  if (**s == '-') { (*s)++; return 1; }
  else if (**s == '+') (*s)++;
  return 0;
}



/*
** {==================================================================
** Lua's implementation for 'lua_strx2number'
 * lua_strx2number函数的实现
** ===================================================================
*/

#if !defined(lua_strx2number)

/* maximum number of significant digits to read (to avoid overflows
   even with single floats)
   读取的最大有限位(避免但单精度浮点数移除)
*/
#define MAXSIGDIG	30

/*
** convert a hexadecimal numeric string to a number, following
** C99 specification for 'strtod'
 * 将十六进制数字串转换成数字,'strtod'的C99规范
*/
static lua_Number lua_strx2number (const char *s, char **endptr) {
  int dot = lua_getlocaledecpoint(); // 得到本地的小数点号(ascii表示'.'为46)
  lua_Number r = 0.0;  /* result (accumulator) */
  //  sigdig代表有意义数字的数量,nosigdig代表没有意义数字的数量(没有意义的就是字符串开头的0,中间的不算)
  int sigdig = 0;  /* number of significant digits  */
  int nosigdig = 0;  /* number of non-significant digits */
  int e = 0;  /* exponent correction */
  int neg;  /* 1 if number is negative */
  int hasdot = 0;  /* true after seen a dot */
  *endptr = cast_charp(s);  /* nothing is valid yet */
  // 首先s跳过前面的空格,然后neg得到*s的符号,1代表负,0代表正
  while (lisspace(cast_uchar(*s))) s++; /* skip initial spaces */
  neg = isneg(&s);  /* check sign */
  // 然后检测开头的两位字符是不是"0x"或"0X",不然的话就不是16进制,直接返回0
  if (!(*s == '0' && (*(s + 1) == 'x' || *(s + 1) == 'X')))  /* check '0x' */
    return 0.0;  /* invalid format (no '0x') */
  // 在循环里面刚开始遇见第一个点就把hasdot赋值1,如果第二次再碰见点的话就跳出循环
  for (s += 2; ; s++) {  /* skip '0x' and read numeral */
    if (*s == dot) {
      if (hasdot) break;  /* second dot? stop loop */
      else hasdot = 1;
    }
    else if (lisxdigit(cast_uchar(*s))) { // lisxdigit检查参数是否为16进制数字,如果是的话返回非0值,不然返回0.参数类型为int,但是可以直接将char类型数据传入
      if (sigdig == 0 && *s == '0')  /* non-significant digit (zero)? */
        nosigdig++;
      else if (++sigdig <= MAXSIGDIG)  /* can read it without overflow? */
          // luaO_hexavalue参数转换成十进制数字
          // 把之前的结果乘以16,就代表16进制往左移了一位,然后再加上新的数
          r = (r * cast_num(16.0)) + luaO_hexavalue(*s);
      else e++; /* too many digits; ignore, but still count for exponent 然后超出MAXSIGDIG限制的字符会被计入e,然后小数点后面如果有字符的话会抵消一部分e */
      if (hasdot) e--;  /* decimal digit? correct exponent */
    }
    else break;  /* neither a dot nor a digit */
  }
  if (nosigdig + sigdig == 0)  /* no digits? */
    return 0.0;  /* invalid format */
  // endptr是有效字符的下一位
  *endptr = cast_charp(s);  /* valid up to here  */
  e *= 4;  /* each digit multiplies/divides value by 2^4 */
  if (*s == 'p' || *s == 'P') {  /* exponent part? */
    int exp1 = 0;  /* exponent value */
    int neg1;  /* exponent sign */
    s++;  /* skip 'p' */
    neg1 = isneg(&s);  /* sign */
    if (!lisdigit(cast_uchar(*s)))
      return 0.0;  /* invalid; must have at least one digit */
    while (lisdigit(cast_uchar(*s)))  /* read exponent */
      exp1 = exp1 * 10 + *(s++) - '0';
    if (neg1) exp1 = -exp1;
    e += exp1;
    *endptr = cast_charp(s);  /* valid up to here */
  }
  if (neg) r = -r;
  return l_mathop(ldexp)(r, e);
}

#endif
/* }====================================================== */


/* maximum length of a numeral 数字的最大长度 */
#if !defined (L_MAXLENNUM)
#define L_MAXLENNUM	200
#endif

static const char *l_str2dloc (const char *s, lua_Number *result, int mode) {
  char *endptr;
  // 转16进制数字或者10进制
  *result = (mode == 'x') ? lua_strx2number(s, &endptr)  /* try to convert */
                          : lua_str2number(s, &endptr);
  if (endptr == s) return NULL;  /* nothing recognized? */
  while (lisspace(cast_uchar(*endptr))) endptr++;  /* skip trailing spaces */
  return (*endptr == '\0') ? endptr : NULL;  /* OK if no trailing characters */
}


/*
** Convert string 's' to a Lua number (put in 'result'). Return NULL
** on fail or the address of the ending '\0' on success.
** 'pmode' points to (and 'mode' contains) special things in the string:
** - 'x'/'X' means a hexadecimal numeral
** - 'n'/'N' means 'inf' or 'nan' (which should be rejected)
** - '.' just optimizes the search for the common case (nothing special)
** This function accepts both the current locale or a dot as the radix
** mark. If the conversion fails, it may mean number has a dot but
** locale accepts something else. In that case, the code copies 's'
** to a buffer (because 's' is read-only), changes the dot to the
** current locale radix mark, and tries to convert again.
 * 把字符串s转换成number数据，在失败时返回NULL或在成功时返回字符串最后一位的地址(就是'\0'的地址)
 *  pmode指向字符串中特殊的字符（'.','x','X','n','N'）
 *  'x'/'X'意味着十六进制字符
 *  'n'/'N'意味着无穷大或未定义(它们被直接返回)
 *  该函数接受当前区域设置或点作为基数标记。如果转换失败,那可能意味着数字是一个点并且本地也接受了其他别的东西
 *  在这种情况下,代码拷贝了一个s来指向一段缓冲区(因为s是只读的),将点更改为当前区域设置的基数，并尝试再次转换
*/
static const char *l_str2d (const char *s, lua_Number *result) {
  const char *endptr;
  // 是在源字符串（s1）中找出最先含有搜索字符串（s2）中任一字符的位置并返回，若找不到则返回空指针。
  const char *pmode = strpbrk(s, ".xXnN");
  int mode = pmode ? ltolower(cast_uchar(*pmode)) : 0;
  if (mode == 'n')  /* reject 'inf' and 'nan' */
    return NULL;
  endptr = l_str2dloc(s, result, mode);  /* try to convert */
  if (endptr == NULL) {  /* failed? may be a different locale */
    char buff[L_MAXLENNUM + 1];
    const char *pdot = strchr(s, '.');
    if (strlen(s) > L_MAXLENNUM || pdot == NULL)
      return NULL;  /* string too long or no dot; fail */
    strcpy(buff, s);  /* copy string to buffer */
    buff[pdot - s] = lua_getlocaledecpoint();  /* correct decimal point */
    endptr = l_str2dloc(buff, result, mode);  /* try again */
    if (endptr != NULL)
      endptr = s + (endptr - buff);  /* make relative to 's' */
  }
  return endptr;
}


#define MAXBY10		cast(lua_Unsigned, LUA_MAXINTEGER / 10)
#define MAXLASTD	cast_int(LUA_MAXINTEGER % 10)

// 把字符串转换为int，结果保存在result中
// 通过lisspace跳过开头的空格字符
// 然后isneg获取正负号，1代表负,0代表正,并且s向前移一位
// 然后判断是不是十六进制，是的话跳过'0x'，然后lisxdigit检查参数是否为16进制数字,如果是的话返回非0值,不然返回0.
// 然后将结果*16并加上新得的数字，luaO_hexavalue的作用就是把能转变成数字的字符变成数字并返回
// 10进制的话，原理一样，但是多了一个溢出检测，机制就是判断a是不是大于LUA_MAXINTEGER / 10，因为下一步要进行a*10
static const char *l_str2int (const char *s, lua_Integer *result) {
  lua_Unsigned a = 0;
  int empty = 1;
  int neg;
  while (lisspace(cast_uchar(*s))) s++;  /* skip initial spaces */
  neg = isneg(&s);
  if (s[0] == '0' &&
      (s[1] == 'x' || s[1] == 'X')) {  /* hex? */
    s += 2;  /* skip '0x' */
    for (; lisxdigit(cast_uchar(*s)); s++) {
      a = a * 16 + luaO_hexavalue(*s);
      empty = 0;
    }
  }
  else {  /* decimal */
    for (; lisdigit(cast_uchar(*s)); s++) {
      int d = *s - '0';
      if (a >= MAXBY10 && (a > MAXBY10 || d > MAXLASTD + neg))  /* overflow? */
        return NULL;  /* do not accept it (as integer) */
      a = a * 10 + d;
      empty = 0;
    }
  }
  while (lisspace(cast_uchar(*s))) s++;  /* skip trailing spaces */
  if (empty || *s != '\0') return NULL;  /* something wrong in the numeral */
  else {
    *result = l_castU2S((neg) ? 0u - a : a);
    return s;
  }
}
/*
 * 把一个字符串转换成数字
 * 首先调用l_str2int看能不能转换成int，可以的话调用setivalue创建一个类型为LUA_TNUMINT的int
 * 不行的话调用setfltvalue看能不能转换成float类型的数据,可以的话调用setfltvalue创建一个类型为LUA_TNUMFLT的float
 * 还是不行的就返回0，代表转换失败
 * 成功的话返回字符串的长度
*/
size_t luaO_str2num (const char *s, TValue *o) {
  lua_Integer i; lua_Number n;
  const char *e;
  if ((e = l_str2int(s, &i)) != NULL) {  /* try as an integer */
    setivalue(o, i);
  }
  else if ((e = l_str2d(s, &n)) != NULL) {  /* else try as a float */
    setfltvalue(o, n);
  }
  else
    return 0;  /* conversion failed */
  return (e - s) + 1;  /* success; return string size */
}

/*
 * 返回x的utf8字节数量
 * x <= 0x10FFFF，这里x不能超过unicode的最大编码
 * x < 0x80属于ascii编码，buff的最后一位保存x，并且返回字节数1
 * 0x80等于十进制的128
 * 如果不属于ascii范围的话，就需要延长字节
 * 首先定义一个mfb，因为ascii的范围为0x00 - 0x7f
 * 大于ascii的范围之后就需要扩充字节了
 */
int luaO_utf8esc (char *buff, unsigned long x) {
  int n = 1;  /* number of bytes put in buffer (backwards) */
  lua_assert(x <= 0x10FFFF);
  if (x < 0x80)  /* ascii? */
    buff[UTF8BUFFSZ - 1] = cast_char(x);
  else {  /* need continuation bytes */
    unsigned int mfb = 0x3f;  /* maximum that fits in first byte */
    do {  /* add continuation bytes */
      buff[UTF8BUFFSZ - (n++)] = cast_char(0x80 | (x & 0x3f));
      x >>= 6;  /* remove added bits */
      mfb >>= 1;  /* now there is one less bit available in first byte */
    } while (x > mfb);  /* still needs continuation byte? */
    buff[UTF8BUFFSZ - n] = cast_char((~mfb << 1) | x);  /* add first byte */
  }
  return n;
}


/* maximum length of the conversion of a number to a string */
#define MAXNUMBER2STR	50


/*
** Convert a number object to a string
 * 数字对象转换为string
 *
*/
void luaO_tostring (lua_State *L, TValue *obj) {
  char buff[MAXNUMBER2STR];
  size_t len;
  lua_assert(ttisnumber(obj));
  if (ttisinteger(obj))
    len = lua_integer2str(buff, sizeof(buff), ivalue(obj));
  else {
    len = lua_number2str(buff, sizeof(buff), fltvalue(obj));
    // strspn的原型为：size_t strspn (const char *s,const char * accept)
    // strspn()从参数s 字符串的开头计算连续的字符，而这些字符都完全是accept 所指字符串中的字符。
    // 简单的说，若strspn()返回的数值为n，则代表字符串s 开头连续有n 个字符都是属于字符串accept内的字符。
    // 所以这里通过strspn判断是不是转换成了int值
    if (buff[strspn(buff, "-0123456789")] == '\0') {  /* looks like an int? 检查是字符串否符合整型 */
      // lua_getlocaledecpoint()返回当地的小数点
      buff[len++] = lua_getlocaledecpoint();
      buff[len++] = '0';  /* adds '.0' to result 字符串结尾添加\0*/
    }
  }
  // 调用setsvalue2s创建一个LUA_TSTRING类型的TValue压入栈中
  setsvalue(L, obj, luaS_newlstr(L, buff, len));
}

/*
 * 创建一个字符串，让L->top指向该字符串
 * luaS_newlstr创建一个字符串
 * 然后调用luaD_inctop，让栈顶指针自增1
 */
static void pushstr (lua_State *L, const char *str, size_t l) {
  setsvalue2s(L, L->top, luaS_newlstr(L, str, l));
  L->top++;
}


/*
** this function handles only '%d', '%c', '%f', '%p', and '%s'
   conventional formats, plus Lua-specific '%I' and '%U'
   luaO_pushvfstring的作用是把luaO_pushfstring传进来的可变参数中的值，一个一个的加入lua栈中
   这个函数仅支持'%d', '%c', '%f', '%p', and '%s'转换格式，外加lua的'%I' and '%U'
   首先，e的位置就是%的位置，然后调用pushstr向栈中=顶压入一个字符串，并栈顶自增1
   下面就是不同格式的参数压入规则了
   如果是's'的话，代表以'\0'结尾的字符串，通过va_arg返回可变的参数，va_arg的第二个参数是你要返回的参数的类型
   如果是'c'的话，代表以int作为的字符，然后lisprint判断是否为可打印字符，是的话直接入栈，不然就转成%d然后输出对应的ascii码
   如果是'd'的话，就代表是一个int，直接入栈，然后把这个int转换成字符串
   如果是'I'的话，就代表lua里面的lua_Integer，也是直接入栈，然后把这个lua_Integer转换成字符串
   如果是'f'的话，就代表是一个lua_Number，执行流程和lua_Integer一样
   如果是'p'的话，就代表是一个指针，把argp中的void *类型的参数按照%p的格式转换进buff中，然后返回装入buff中的size，然后入栈
   如果是'U'的话，就代表一个UTF-8序列，计算出序列的字节数，然后入栈
*/
const char *luaO_pushvfstring (lua_State *L, const char *fmt, va_list argp) {
  int n = 0;  /* number of strings in the stack to concatenate */
  const char *e;  /* points to next conversion specifier */
  while ((e = strchr(fmt, '%')) != NULL) { // strchr作用是找出fmt中首次出现'%'的位置，并返回其指针
    pushstr(L, fmt, e - fmt);  /* string up to conversion specifier */
    switch (*(e+1)) {
      case 's': {  /* zero-terminated string */
        const char *s = va_arg(argp, char *);
        if (s == NULL) s = "(null)";
        pushstr(L, s, strlen(s));
        break;
      }
      case 'c': {  /* an 'int' as a character */
        char buff = cast_char(va_arg(argp, int));
        if (lisprint(cast_uchar(buff)))
          pushstr(L, &buff, 1);
        else  /* non-printable character; print its code */
          luaO_pushfstring(L, "<\\%d>", cast_uchar(buff));
        break;
      }
      case 'd': {  /* an 'int' */
        setivalue(s2v(L->top), va_arg(argp, int));
        goto top2str;
      }
      case 'I': {  /* a 'lua_Integer' */
        setivalue(s2v(L->top), cast(lua_Integer, va_arg(argp, l_uacInt)));
        goto top2str;
      }
      case 'f': {  /* a 'lua_Number' */
        setfltvalue(s2v(L->top), cast_num(va_arg(argp, l_uacNumber)));
      top2str:  /* convert the top element to a string */
        L->top++;
        luaO_tostring(L, s2v(L->top - 1));
        break;
      }
      case 'p': {  /* a pointer */
        char buff[4*sizeof(void *) + 8]; /* should be enough space for a '%p' */
        void *p = va_arg(argp, void *);
        int l = lua_pointer2str(buff, sizeof(buff), p);
        pushstr(L, buff, l);
        break;
      }
      case 'U': {  /* an 'int' as a UTF-8 sequence */
        char buff[UTF8BUFFSZ];
        int l = luaO_utf8esc(buff, cast(long, va_arg(argp, long)));
        pushstr(L, buff + UTF8BUFFSZ - l, l);
        break;
      }
      case '%': {
        pushstr(L, "%", 1);
        break;
      }
      default: {
        luaG_runerror(L, "invalid option '%%%c' to 'lua_pushfstring'",
                         *(e + 1));
      }
    }
    n += 2;
    if (L->top + 2 > L->stack_last) {  /* no free stack space? */
      luaV_concat(L, n);
      n = 1;
    }
    fmt = e + 2;
  }
  pushstr(L, fmt, strlen(fmt));
  if (n > 0) luaV_concat(L, n + 1);
  return svalue(s2v(L->top - 1));
}

/*
  luaO_pushfstring的作用是把一个字符串按照固定格式压入L栈中，比如说：

  va_list,va_start,va_end 这些宏定义在stdarg.h中
  va_list定义了一个可变参数argp
  va_start初始化了argp，第一个参数是argp，第二个参数是fmt（可变参数的前一个参数）
*/
const char *luaO_pushfstring (lua_State *L, const char *fmt, ...) {
  const char *msg;
  va_list argp;
  va_start(argp, fmt);
  msg = luaO_pushvfstring(L, fmt, argp);
  va_end(argp);
  return msg;
}


/* number of chars of a literal string without the ending \0 */
#define LL(x)	(sizeof(x)/sizeof(char) - 1)

#define RETS	"..."
#define PRE	"[string \""
#define POS	"\"]"

#define addstr(a,b,l)	( memcpy(a,b,(l) * sizeof(char)), a += (l) )

void luaO_chunkid (char *out, const char *source, size_t bufflen) {
  size_t l = strlen(source);
  if (*source == '=') {  /* 'literal' source */
    if (l <= bufflen)  /* small enough? */
      memcpy(out, source + 1, l * sizeof(char));
    else {  /* truncate it */
      addstr(out, source + 1, bufflen - 1);
      *out = '\0';
    }
  }
  else if (*source == '@') {  /* file name */
    if (l <= bufflen)  /* small enough? */
      memcpy(out, source + 1, l * sizeof(char));
    else {  /* add '...' before rest of name */
      addstr(out, RETS, LL(RETS));
      bufflen -= LL(RETS);
      memcpy(out, source + 1 + l - bufflen, bufflen * sizeof(char));
    }
  }
  else {  /* string; format as [string "source"] */
    const char *nl = strchr(source, '\n');  /* find first new line (if any) */
    addstr(out, PRE, LL(PRE));  /* add prefix */
    bufflen -= LL(PRE RETS POS) + 1;  /* save space for prefix+suffix+'\0' */
    if (l < bufflen && nl == NULL) {  /* small one-line source? */
      addstr(out, source, l);  /* keep it */
    }
    else {
      if (nl != NULL) l = nl - source;  /* stop at first newline */
      if (l > bufflen) l = bufflen;
      addstr(out, source, l);
      addstr(out, RETS, LL(RETS));
    }
    memcpy(out, POS, (LL(POS) + 1) * sizeof(char));
  }
}

