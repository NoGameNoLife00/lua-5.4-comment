/*
** $Id: lobject.h $
** Type definitions for Lua objects
 * Lua对象类型定义
** See Copyright Notice in lua.h
*/


#ifndef lobject_h
#define lobject_h


#include <stdarg.h>


#include "llimits.h"
#include "lua.h"


/*
** Extra tags for non-values
*/
#define LUA_TUPVAL	LUA_NUMTAGS  /* upvalues */
#define LUA_TPROTO	(LUA_NUMTAGS+1)  /* function prototypes */

/*
** number of all possible tags (including LUA_TNONE)
*/
#define LUA_TOTALTAGS	(LUA_TPROTO + 2)


/*
** tags for Tagged Values have the following use of bits:
** bits 0-3: actual tag (a LUA_T* value)
** bits 4-5: variant bits
** bit 6: whether value is collectable
 * tt_是区分不同类型其中-> bit 0-3:基本类型 4-5:子类型 6: 是否能GC
*/



/*
** Union of all Lua values (Lua中所有类型联合体)
*/
typedef union Value {
  struct GCObject *gc;    /* collectable objects  */
  void *p;         /* light userdata */
  int b;           /* booleans */
  lua_CFunction f; /* light C functions */
  lua_Integer i;   /* integer numbers */
  lua_Number n;    /* float numbers */
} Value;


/*
** Tagged Values. This is the basic representation of values in Lua:
** an actual value plus a tag with its type.
*/

#define TValuefields	Value value_; lu_byte tt_

typedef struct TValue {
  TValuefields;
} TValue;


#define val_(o)		((o)->value_)
#define valraw(o)	(&val_(o))


/* raw type tag of a TValue */
#define rawtt(o)	((o)->tt_)

/* tag with no variants (bits 0-3)
 * 0-3位类型tag 不包括变量属性*/
#define novariant(t)	((t) & 0x0F)

/* type tag of a TValue (bits 0-3 for tags + variant bits 4-5)
 * tt_的0-3位标记和4-5变量组合表示TValue的类型
 * */
#define withvariant(t)	((t) & 0x3F)
#define ttypetag(o)	withvariant(rawtt(o))

/* type of a TValue 获取TValue的类型 */
#define ttype(o)	(novariant(rawtt(o)))


/* Macros to test type 检查类型宏 */
#define checktag(o,t)		(rawtt(o) == (t))
#define checktype(o,t)		(ttype(o) == (t))


/* Macros for internal tests 测试宏 */
#define righttt(obj)		(ttypetag(obj) == gcvalue(obj)->tt)

#define checkliveness(L,obj) \
	lua_longassert(!iscollectable(obj) || \
		(righttt(obj) && (L == NULL || !isdead(G(L),gcvalue(obj)))))


/* Macros to set values 设置对象类型 */
#define settt_(o,t)	((o)->tt_=(t))


#define setobj(L,obj1,obj2) \
	{ TValue *io1=(obj1); const TValue *io2=(obj2); \
          io1->value_ = io2->value_; io1->tt_ = io2->tt_; \
	  (void)L; checkliveness(L,io1); lua_assert(!isreallyempty(io1)); }

/*
** different types of assignments, according to destination
 * 不同类型转换赋值
*/

/* from stack to stack */
#define setobjs2s(L,o1,o2)	setobj(L,s2v(o1),s2v(o2))
/* to stack (not from same stack) */
#define setobj2s(L,o1,o2)	setobj(L,s2v(o1),o2)
/* from table to same table */
#define setobjt2t	setobj
/* to new object */
#define setobj2n	setobj
/* to table */
#define setobj2t	setobj



typedef union StackValue {
  TValue val;
} StackValue;


typedef StackValue *StkId;  /* index to stack elements */

/* convert a 'StackValue' to a 'TValue' */
#define s2v(o)	(&(o)->val)



/*
** {==================================================================
** Nil
** ===================================================================
*/

/* macro to test for (any kind of) nil
 * 检查tag任何种类的nil
 * */
#define ttisnil(v)		checktype((v), LUA_TNIL)

/* macro to test for a "pure" nil
 * 检查类型位是否为nil
 * */
#define ttisstrictnil(o)	checktag((o), LUA_TNIL)


#define setnilvalue(obj) settt_(obj, LUA_TNIL)


/*
** Variant tag, used only in tables to signal an empty slot
** (which might be different from a slot containing nil)
 * 变量标记,仅用于表中标记空槽
*/
#define LUA_TEMPTY	(LUA_TNIL | (1 << 4))

/*
** Variant used only in the value returned for a key not found in a
** table (absent key).
 * 标记用于表中没有找到key时返回
*/
#define LUA_TABSTKEY	(LUA_TNIL | (2 << 4))


#define isabstkey(v)		checktag((v), LUA_TABSTKEY)


/*
** macro to detect non-standard nils (used only in assertions)
 * 是否为不标准的nil
*/
#define isreallyempty(v)	(ttisnil(v) && !ttisstrictnil(v))


/*
** By default, entries with any kind of nil are considered empty.
** (In any definition, values associated with absent keys must also
** be accepted as empty.)
 * 默认上任何种类的nil都被定义为空
*/
#define isempty(v)		ttisnil(v)


/* macro defining a value corresponding to an absent key */
#define ABSTKEYCONSTANT		{NULL}, LUA_TABSTKEY


/* mark an entry as empty */
#define setempty(v)		settt_(v, LUA_TEMPTY)



/* }================================================================== */


/*
** {==================================================================
** Booleans
** ===================================================================
*/

#define ttisboolean(o)		checktag((o), LUA_TBOOLEAN)

#define bvalue(o)	check_exp(ttisboolean(o), val_(o).b)

#define bvalueraw(v)	((v).b)

#define l_isfalse(o)	(ttisnil(o) || (ttisboolean(o) && bvalue(o) == 0))

#define setbvalue(obj,x) \
  { TValue *io=(obj); val_(io).b=(x); settt_(io, LUA_TBOOLEAN); }

/* }================================================================== */


/*
** {==================================================================
** Threads
** ===================================================================
*/

#define ttisthread(o)		checktag((o), ctb(LUA_TTHREAD))

#define thvalue(o)	check_exp(ttisthread(o), gco2th(val_(o).gc))

#define setthvalue(L,obj,x) \
  { TValue *io = (obj); lua_State *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TTHREAD)); \
    checkliveness(L,io); }

#define setthvalue2s(L,o,t)	setthvalue(L,s2v(o),t)

/* }================================================================== */


/*
** {==================================================================
** Collectable Objects
** ===================================================================
*/

/*
** Common Header for all collectable objects (in macro form, to be
** included in other objects)
 * 所有GC对象的通用头部
*/
#define CommonHeader	struct GCObject *next; lu_byte tt; lu_byte marked


/* Common type for all collectable objects */
typedef struct GCObject {
  CommonHeader;
} GCObject;


/* Bit mark for collectable types */
#define BIT_ISCOLLECTABLE	(1 << 6)

#define iscollectable(o)	(rawtt(o) & BIT_ISCOLLECTABLE)

/* mark a tag as collectable */
#define ctb(t)			((t) | BIT_ISCOLLECTABLE)

#define gcvalue(o)	check_exp(iscollectable(o), val_(o).gc)

#define gcvalueraw(v)	((v).gc)

#define setgcovalue(L,obj,x) \
  { TValue *io = (obj); GCObject *i_g=(x); \
    val_(io).gc = i_g; settt_(io, ctb(i_g->tt)); }

/* }================================================================== */


/*
** {==================================================================
** Numbers
** ===================================================================
*/

/* Variant tags for numbers 数字类型子类型标记 */
#define LUA_TNUMFLT	(LUA_TNUMBER | (1 << 4))  /* float numbers */
#define LUA_TNUMINT	(LUA_TNUMBER | (2 << 4))  /* integer numbers */

#define ttisnumber(o)		checktype((o), LUA_TNUMBER)
#define ttisfloat(o)		checktag((o), LUA_TNUMFLT)
#define ttisinteger(o)		checktag((o), LUA_TNUMINT)

#define nvalue(o)	check_exp(ttisnumber(o), \
	(ttisinteger(o) ? cast_num(ivalue(o)) : fltvalue(o)))
#define fltvalue(o)	check_exp(ttisfloat(o), val_(o).n)
#define ivalue(o)	check_exp(ttisinteger(o), val_(o).i)

#define fltvalueraw(v)	((v).n)
#define ivalueraw(v)	((v).i)

#define setfltvalue(obj,x) \
  { TValue *io=(obj); val_(io).n=(x); settt_(io, LUA_TNUMFLT); }

#define chgfltvalue(obj,x) \
  { TValue *io=(obj); lua_assert(ttisfloat(io)); val_(io).n=(x); }

#define setivalue(obj,x) \
  { TValue *io=(obj); val_(io).i=(x); settt_(io, LUA_TNUMINT); }

#define chgivalue(obj,x) \
  { TValue *io=(obj); lua_assert(ttisinteger(io)); val_(io).i=(x); }

/* }================================================================== */


/*
** {==================================================================
** Strings
** ===================================================================
*/

/* Variant tags for strings 字符串类型子类型标记 */
#define LUA_TSHRSTR	(LUA_TSTRING | (1 << 4))  /* short strings */
#define LUA_TLNGSTR	(LUA_TSTRING | (2 << 4))  /* long strings */

#define ttisstring(o)		checktype((o), LUA_TSTRING)
#define ttisshrstring(o)	checktag((o), ctb(LUA_TSHRSTR))
#define ttislngstring(o)	checktag((o), ctb(LUA_TLNGSTR))

#define tsvalueraw(v)	(gco2ts((v).gc))

#define tsvalue(o)	check_exp(ttisstring(o), gco2ts(val_(o).gc))

#define setsvalue(L,obj,x) \
  { TValue *io = (obj); TString *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(x_->tt)); \
    checkliveness(L,io); }

/* set a string to the stack */
#define setsvalue2s(L,o,s)	setsvalue(L,s2v(o),s)

/* set a string to a new object */
#define setsvalue2n	setsvalue


/*
** Header for string value; string bytes follow the end of this structure
** (aligned according to 'UTString'; see next).
 * 字符串头部, 真正的字符串内存紧接着直接存在struct之后 字符串对象大小为:sizeof(TString)+shrlen+1
*/
typedef struct TString {
  CommonHeader;
    /* reserved words for short strings; "has hash" for longs
     * 在短字符串中>0则表示为一个系统保留的关键字,extra的值对应词法分析的一个token值
     * 用于加速词法分析速度和保证不被GC,对于长字符串,一般很少做索引或者比较,
     * 所以长字符串直接链接到allgc链表上做GC对象来处理.Lua不会对新创建的长字符串对象计算哈希值,
     * 也不保证长字符串对象的唯一性。当长字符串需要被用来当作索引时，
     * 会为其计算一次哈希值，并使用extra来记录是否已经为其计算了哈希
     */
  lu_byte extra;
  lu_byte shrlen;  /* length for short strings 字符串长度 */
  unsigned int hash;
  union {
    size_t lnglen;  /* length for long strings 长字符串的长度 */
    struct TString *hnext;  /* linked list for hash table 全局的TString,整个链表就是字符串池 */
  } u;
} TString;


/*
** Ensures that address after this type is always fully aligned.
 * 确保内存对齐
*/
typedef union UTString {
  LUAI_MAXALIGN;  /* ensures maximum alignment for strings 确保字符串最大对齐 */
  TString tsv;
} UTString;


/*
** Get the actual string (array of bytes) from a 'TString'.
** (Access to 'extra' ensures that value is really a 'TString'.)
 * 从TString中获取字符串内容(通过extra来确定变量确实是TString
*/
#define getstr(ts)  \
  check_exp(sizeof((ts)->extra), cast_charp((ts)) + sizeof(UTString))


/* get the actual string (array of bytes) from a Lua value
 * 从lua值中获取字符串内容
 */
#define svalue(o)       getstr(tsvalue(o))

/* get string length from 'TString *s'
 * 从String中获取字符串长度
 */
#define tsslen(s)	((s)->tt == LUA_TSHRSTR ? (s)->shrlen : (s)->u.lnglen)

/* get string length from 'TValue *o'
 * 从TValue中获取字符串长度
 */
#define vslen(o)	tsslen(tsvalue(o))

/* }================================================================== */


/*
** {==================================================================
** Userdata
** ===================================================================
*/

#define ttislightuserdata(o)	checktag((o), LUA_TLIGHTUSERDATA)
#define ttisfulluserdata(o)	checktype((o), LUA_TUSERDATA)

#define pvalue(o)	check_exp(ttislightuserdata(o), val_(o).p)
#define uvalue(o)	check_exp(ttisfulluserdata(o), gco2u(val_(o).gc))

#define pvalueraw(v)	((v).p)

#define setpvalue(obj,x) \
  { TValue *io=(obj); val_(io).p=(x); settt_(io, LUA_TLIGHTUSERDATA); }

#define setuvalue(L,obj,x) \
  { TValue *io = (obj); Udata *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TUSERDATA)); \
    checkliveness(L,io); }


/* Ensures that addresses after this type are always fully aligned.、
 * 确保这个类型后面内存完全对齐
 */
typedef union UValue {
  TValue uv;
  LUAI_MAXALIGN;  /* ensures maximum alignment for udata bytes */
} UValue;


/*
** Header for userdata with user values;
** memory area follows the end of this structure.
 * 用户数据userdata的头部, 内存紧接着这个结构之后
*/
typedef struct Udata {
  CommonHeader;
  unsigned short nuvalue;  /* number of user values */
  size_t len;  /* number of bytes 实际用户数据长度 */
  struct Table *metatable;
  GCObject *gclist;
  UValue uv[1];  /* user values */
} Udata;


/*
** Header for userdata with no user values. These userdata do not need
** to be gray during GC, and therefore do not need a 'gclist' field.
** To simplify, the code always use 'Udata' for both kinds of userdata,
** making sure it never accesses 'gclist' on userdata with no user values.
** This structure here is used only to compute the correct size for
** this representation. (The 'bindata' field in its end ensures correct
** alignment for binary data following this header.)
*/
typedef struct Udata0 {
  CommonHeader;
  unsigned short nuvalue;  /* number of user values */
  size_t len;  /* number of bytes */
  struct Table *metatable;
  union {LUAI_MAXALIGN;} bindata;
} Udata0;


/* compute the offset of the memory area of a userdata */
#define udatamemoffset(nuv) \
	((nuv) == 0 ? offsetof(Udata0, bindata)  \
                    : offsetof(Udata, uv) + (sizeof(UValue) * (nuv)))

/* get the address of the memory block inside 'Udata' */
#define getudatamem(u)	(cast_charp(u) + udatamemoffset((u)->nuvalue))

/* compute the size of a userdata */
#define sizeudata(nuv,nb)	(udatamemoffset(nuv) + (nb))

/* }================================================================== */


/*
** {==================================================================
** Prototypes
** ===================================================================
*/

/*
** Description of an upvalue for function prototypes
 * upvalue的描述信息
*/
typedef struct Upvaldesc {
  TString *name;  /* upvalue name (for debug information) */
  lu_byte instack;  /* whether it is in stack (register) 是否在栈上 */
  lu_byte idx;  /* index of upvalue (in stack or in outer function's list) 栈上的dix */
} Upvaldesc;


/*
** Description of a local variable for function prototypes
** (used for debug information)
 * 函数原型中的局部变量定义 debug用
*/
typedef struct LocVar {
  TString *varname;
  int startpc;  /* first point where variable is active */
  int endpc;    /* first point where variable is dead */
} LocVar;


/*
** Associates the absolute line source for a given instruction ('pc').
** The array 'lineinfo' gives, for each instruction, the difference in
** lines from the previous instruction. When that difference does not
** fit into a byte, Lua saves the absolute line for that instruction.
** (Lua also saves the absolute line periodically, to speed up the
** computation of a line number: we can use binary search in the
** absolute-line array, but we must traverse the 'lineinfo' array
** linearly to compute a line.)
*/
typedef struct AbsLineInfo {
  int pc;
  int line;
} AbsLineInfo;

/*
** Function Prototypes 函数原型
*/
typedef struct Proto {
  CommonHeader;
  lu_byte numparams;  /* number of fixed (named) parameters 函数参数个数 */
  lu_byte is_vararg; /* 是否有变长参数 */
  lu_byte maxstacksize;  /* number of registers needed by this function 最大的函数栈长度 */

  int sizeupvalues;  /* size of 'upvalues' 闭包变量长度 */
  Upvaldesc *upvalues;  /* upvalue information 闭包变量数组 */

  int sizek;  /* size of 'k' 常量数组长度 */
  TValue *k;  /* constants used by the function  常量数组*/

  int sizelocvars; /* 局部变量数组长度 */
  LocVar *locvars;  /* information about local variables (debug information) 局部变量数组 */

  int sizecode; /* 指令数组的长度 */
  Instruction *code;  /* opcodes 三地址指令数组 */

  int sizelineinfo; /* 行信息数组长度 */
  ls_byte *lineinfo;  /* information about source lines (debug information) 行信息数组 */
  AbsLineInfo *abslineinfo;  /* idem */

  int sizep;  /* size of 'p' 嵌套的Proto长度 */
  struct Proto **p;  /* functions defined inside the function 嵌套的Proto数组 */

  int sizeabslineinfo;  /* size of 'abslineinfo' */

  int linedefined;  /* debug information 函数的起始定义行号  */
  int lastlinedefined;  /* debug information 函数的末尾定义行号 */
  TString  *source;  /* used for debug information 源码字符串 */

  GCObject *gclist;
} Proto;

/* }================================================================== */


/*
** {==================================================================
** Closures 闭包
** ===================================================================
*/

/* Variant tags for functions 函数子类型标记位 */
#define LUA_TLCL	(LUA_TFUNCTION | (1 << 4))  /* Lua closure  Lua闭包*/
#define LUA_TLCF	(LUA_TFUNCTION | (2 << 4))  /* light C function 纯C函数 */
#define LUA_TCCL	(LUA_TFUNCTION | (3 << 4))  /* C closure C闭包*/

#define ttisfunction(o)		checktype(o, LUA_TFUNCTION)
#define ttisclosure(o)		((rawtt(o) & 0x1F) == LUA_TLCL)
#define ttisLclosure(o)		checktag((o), ctb(LUA_TLCL))
#define ttislcf(o)		checktag((o), LUA_TLCF)
#define ttisCclosure(o)		checktag((o), ctb(LUA_TCCL))

#define isLfunction(o)	ttisLclosure(o)

#define clvalue(o)	check_exp(ttisclosure(o), gco2cl(val_(o).gc))
#define clLvalue(o)	check_exp(ttisLclosure(o), gco2lcl(val_(o).gc))
#define fvalue(o)	check_exp(ttislcf(o), val_(o).f)
#define clCvalue(o)	check_exp(ttisCclosure(o), gco2ccl(val_(o).gc))

#define fvalueraw(v)	((v).f)

#define setclLvalue(L,obj,x) \
  { TValue *io = (obj); LClosure *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TLCL)); \
    checkliveness(L,io); }

#define setclLvalue2s(L,o,cl)	setclLvalue(L,s2v(o),cl)

#define setfvalue(obj,x) \
  { TValue *io=(obj); val_(io).f=(x); settt_(io, LUA_TLCF); }

#define setclCvalue(L,obj,x) \
  { TValue *io = (obj); CClosure *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TCCL)); \
    checkliveness(L,io); }


/*
** Upvalues for Lua closures
 * lua闭包变量
*/
typedef struct UpVal {
  CommonHeader;
  TValue *v;  /* points to stack or to its own value 变量真正值的指针 */
  union {
    struct {  /* (when open) */
      struct UpVal *next;  /* linked list */
      struct UpVal **previous;
    } open;
    TValue value;  /* the value (when closed) */
  } u;
} UpVal;


/* variant for "To Be Closed" upvalues
 * 闭包中外部局部变量
 */
#define LUA_TUPVALTBC	(LUA_TUPVAL | (1 << 4))


#define ClosureHeader \
	CommonHeader; lu_byte nupvalues; GCObject *gclist
/* C闭包 */
typedef struct CClosure {
  ClosureHeader;
  lua_CFunction f;
  TValue upvalue[1];  /* list of upvalues */
} CClosure;

/* Lua闭包 */
typedef struct LClosure {
  ClosureHeader;
  struct Proto *p; /* 分析阶段生成的字节码等信息 */
  UpVal *upvals[1];  /* list of upvalues */
} LClosure;


typedef union Closure {
  CClosure c;
  LClosure l;
} Closure;


#define getproto(o)	(clLvalue(o)->p)

/* }================================================================== */


/*
** {==================================================================
** Tables
** ===================================================================
*/

#define ttistable(o)		checktag((o), ctb(LUA_TTABLE))

#define hvalue(o)	check_exp(ttistable(o), gco2t(val_(o).gc))

#define sethvalue(L,obj,x) \
  { TValue *io = (obj); Table *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TTABLE)); \
    checkliveness(L,io); }

#define sethvalue2s(L,o,h)	sethvalue(L,s2v(o),h)


/*
** Nodes for Hash tables: A pack of two TValue's (key-value pairs)
** plus a 'next' field to link colliding entries. The distribution
** of the key's fields ('key_tt' and 'key_val') not forming a proper
** 'TValue' allows for a smaller size for 'Node' both in 4-byte
** and 8-byte alignments.
 * 哈希表节点:2个TValue一组(K-V)加上一个next来连接冲突项目
 * key的属性(key_tt和key_val)不构成正常的TValue,允许4字节或8字节对齐
*/
typedef union Node {
  struct NodeKey {
    TValuefields;  /* fields for value */
    lu_byte key_tt;  /* key type */
    int next;  /* for chaining */
    Value key_val;  /* key value */
  } u;
  TValue i_val;  /* direct access to node's value as a proper 'TValue' (val是普通的TValue) */
} Node;


/* copy a value into a key 复制值到key中 */
#define setnodekey(L,node,obj) \
	{ Node *n_=(node); const TValue *io_=(obj); \
	  n_->u.key_val = io_->value_; n_->u.key_tt = io_->tt_; \
	  (void)L; checkliveness(L,io_); }


/* copy a value from a key 从key中复制值 */
#define getnodekey(L,obj,node) \
	{ TValue *io_=(obj); const Node *n_=(node); \
	  io_->value_ = n_->u.key_val; io_->tt_ = n_->u.key_tt; \
	  (void)L; checkliveness(L,io_); }


/*
** About 'alimit': if 'isrealasize(t)' is true, then 'alimit' is the
** real size of 'array'. Otherwise, the real size of 'array' is the
** smallest power of two not smaller than 'alimit' (or zero iff 'alimit'
** is zero); 'alimit' is then used as a hint for #t.
*/

#define BITRAS		(1 << 7)
#define isrealasize(t)		(!((t)->marked & BITRAS))
#define setrealasize(t)		((t)->marked &= cast_byte(~BITRAS))
#define setnorealasize(t)	((t)->marked |= BITRAS)


typedef struct Table {
  CommonHeader;
  /* 1<<p means tagmethod(p) is not present
   * 用于表示在这个表中提供了哪些元方法,默认为0;
   * 当查找过至少一次以后，如果该表中存在某个元方法，那么就将该元方法对应的 flag 位置为1,
   * 这样下一次查找时只需要判断该位即可。所有的元方法映射的bit 在 ltm.h中定义
   */
  lu_byte flags;
  /* log2 of size of 'node' array
   * 该表Hash桶大小的log2值,Hash桶数组大小一定是2的次方,当扩展Hash桶的时候每次需要乘以2.
   */
  lu_byte lsizenode;
  unsigned int alimit;  /* "limit" of 'array' array 数组部分实际大小 */
  TValue *array;  /* array part 指向该表的数组部分的起始位置 */
  Node *node; /* 指向该表的Hash部分的起始位置 */
  /* any free position is before this position
   * 指向Lua表的Hash 部分的末尾位置
   */
  Node *lastfree;
  struct Table *metatable; /* 元表 */
  GCObject *gclist; /* gc链表 */
} Table;


/*
** Macros to manipulate keys inserted in nodes
*/
#define keytt(node)		((node)->u.key_tt)
#define keyval(node)		((node)->u.key_val)

#define keyisnil(node)		(keytt(node) == LUA_TNIL)
#define keyisinteger(node)	(keytt(node) == LUA_TNUMINT)
#define keyival(node)		(keyval(node).i)
#define keyisshrstr(node)	(keytt(node) == ctb(LUA_TSHRSTR))
#define keystrval(node)		(gco2ts(keyval(node).gc))

#define setnilkey(node)		(keytt(node) = LUA_TNIL)

#define keyiscollectable(n)	(keytt(n) & BIT_ISCOLLECTABLE)

#define gckey(n)	(keyval(n).gc)
#define gckeyN(n)	(keyiscollectable(n) ? gckey(n) : NULL)


/*
** Use a "nil table" to mark dead keys in a table. Those keys serve
** to keep space for removed entries, which may still be part of
** chains. Note that the 'keytt' does not have the BIT_ISCOLLECTABLE
** set, so these values are considered not collectable and are different
** from any valid value.
*/
#define setdeadkey(n)	(keytt(n) = LUA_TTABLE, gckey(n) = NULL)

/* }================================================================== */



/*
** 'module' operation for hashing (size is always a power of 2)
*/
#define lmod(s,size) \
	(check_exp((size&(size-1))==0, (cast_int((s) & ((size)-1)))))


#define twoto(x)	(1<<(x))
#define sizenode(t)	(twoto((t)->lsizenode)) // 是返回t的哈希部分的尺寸


/* size of buffer for 'luaO_utf8esc' function */
#define UTF8BUFFSZ	8

LUAI_FUNC int luaO_int2fb (unsigned int x);
LUAI_FUNC int luaO_fb2int (int x);
LUAI_FUNC int luaO_utf8esc (char *buff, unsigned long x);
LUAI_FUNC int luaO_ceillog2 (unsigned int x);
LUAI_FUNC int luaO_rawarith (lua_State *L, int op, const TValue *p1,
                             const TValue *p2, TValue *res);
LUAI_FUNC void luaO_arith (lua_State *L, int op, const TValue *p1,
                           const TValue *p2, StkId res);
LUAI_FUNC size_t luaO_str2num (const char *s, TValue *o);
LUAI_FUNC int luaO_hexavalue (int c);
LUAI_FUNC void luaO_tostring (lua_State *L, TValue *obj);
LUAI_FUNC const char *luaO_pushvfstring (lua_State *L, const char *fmt,
                                                       va_list argp);
LUAI_FUNC const char *luaO_pushfstring (lua_State *L, const char *fmt, ...);
LUAI_FUNC void luaO_chunkid (char *out, const char *source, size_t len);


#endif

