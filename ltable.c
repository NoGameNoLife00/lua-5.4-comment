/*
** $Id: ltable.c $
** Lua tables (hash)
** See Copyright Notice in lua.h
*/

#define ltable_c
#define LUA_CORE

#include "lprefix.h"


/*
** Implementation of tables (aka arrays, objects, or hash tables).
** Tables keep its elements in two parts: an array part and a hash part.
** Non-negative integer keys are all candidates to be kept in the array
** part. The actual size of the array is the largest 'n' such that
** more than half the slots between 1 and n are in use.
** Hash uses a mix of chained scatter table with Brent's variation.
** A main invariant of these tables is that, if an element is not
** in its main position (i.e. the 'original' position that its hash gives
** to it), then the colliding element is in its own main position.
** Hence even when the load factor reaches 100%, performance remains good.
 * 表的实现(数组,对象,哈希表)
 * 表中的成员分为两个部分, 数组和哈希. 非负正整数都保存在数组部分候选部分
*/

#include <math.h>
#include <limits.h>

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "lvm.h"


/*
** MAXABITS is the largest integer such that MAXASIZE fits in an
** unsigned int.
 * MAXABITS是数组size的以2为底的对数
*/
#define MAXABITS	cast_int(sizeof(int) * CHAR_BIT - 1)


/*cd
** MAXASIZE is the maximum size of the array part. It is the minimum
** between 2^MAXABITS and the maximum size such that, measured in bytes,
** it fits in a 'size_t'.
 * 数组部分的最大size是2^MAXABITS. MAXABITS是最大的整数，MAXASIZE是对于unsigned int而言的
 * MAXASIZE就是数组的最大size
*/
#define MAXASIZE	luaM_limitN(1u << MAXABITS, TValue)

/*
** MAXHBITS is the largest integer such that 2^MAXHBITS fits in a
** signed int.
 * 2^MAXHBITS是最大的有符号整型
*/
#define MAXHBITS	(MAXABITS - 1)


/*
** MAXHSIZE is the maximum size of the hash part. It is the minimum
** between 2^MAXHBITS and the maximum size such that, measured in bytes,
** it fits in a 'size_t'.
 * 哈希部分的最大size是2^MAXHBITS.
*/
#define MAXHSIZE	luaM_limitN(1u << MAXHBITS, Node)

/* 求hash值
 * gnode用来返回t的哈希部分的第i个节点，此处的i就是哈希部分0-size-1的整数值
 * 这里的sizenode(t)是返回t的哈希部分的尺寸
 * 然后再调用lmod求出node所在位置的i值（lmod函数是最重要的）
 * 再调用gnode来取得node节点
 */
#define hashpow2(t,n)		(gnode(t, lmod((n), sizenode(t))))

// 通过str结构体的hash字段求出哈希值,原理和int一样,这个hash就是字符串的hash值
#define hashstr(t,str)		hashpow2(t, (str)->hash)
#define hashboolean(t,p)	hashpow2(t, p)
#define hashint(t,i)		hashpow2(t, i)


/*
** for some types, it is better to avoid modulus by power of 2, as
** they tend to have many 2 factors.
 * 有些类型,最好避免用2的幂做模数, 他们可能有二义性
*/
#define hashmod(t,n)	(gnode(t, ((n) % ((sizenode(t)-1)|1))))


#define hashpointer(t,p)	hashmod(t, point2uint(p))


#define dummynode		(&dummynode_) // 虚拟节点, 用于空哈希部分的元素

static const Node dummynode_ = {
  {{NULL}, LUA_TEMPTY,  /* value's value and type */
   LUA_TNIL, 0, {NULL}}  /* key type, next, and key value */
};


static const TValue absentkey = {ABSTKEYCONSTANT};



/*
** Hash for floating-point numbers.
** The main computation should be just
**     n = frexp(n, &i); return (n * INT_MAX) + i
** but there are some numerical subtleties.
** In a two-complement representation, INT_MAX does not has an exact
** representation as a float, but INT_MIN does; because the absolute
** value of 'frexp' is smaller than 1 (unless 'n' is inf/NaN), the
** absolute value of the product 'frexp * -INT_MIN' is smaller or equal
** to INT_MAX. Next, the use of 'unsigned int' avoids overflows when
** adding 'i'; the use of '~u' (instead of '-u') avoids problems with
** INT_MIN.
 * 浮点型的哈希算法
 * 主要的计算公式为 n = frexp(n, &i); return (n * INT_MAX) + i
 * 但有些数字有些微妙之处:
 * 在一个两元组表示中，INT_MAX没有精确的浮点数表示,但是INT_MIN却有
 * 因为frexp的绝对值是一个比1小的数字(除非n是无穷大或无效数字)
 * 'frexp * -INT_MIN'的绝对值小于或等于INT_MAX.使用'unsigned int'类型避免+i的时候溢出
 * 用'~u'替代'-u'避免INT_MIN的问题
 * frexp是一个C语言函数，功能是把一个浮点数分解为尾数和指数
 * 第一个参数是要分解的浮点数据,第二个参数是存储指数的指针,返回的是尾数
 * 然后拿着返回的尾数来乘以-INT_MIN,结果用n保存
 * 下面判断n能不能转换为int,不能的话就报错
 * 可以的话就用unsigned int类型的u来保存ni加上之前的指数
 * 如果u超出了INT_MAX就返回~u,不然直接u
*/
#if !defined(l_hashfloat)
static int l_hashfloat (lua_Number n) {
  int i;
  lua_Integer ni;
  n = l_mathop(frexp)(n, &i) * -cast_num(INT_MIN);
  if (!lua_numbertointeger(n, &ni)) {  /* is 'n' inf/-inf/NaN? */
    lua_assert(luai_numisnan(n) || l_mathop(fabs)(n) == cast_num(HUGE_VAL));
    return 0;
  }
  else {  /* normal case */
    unsigned int u = cast_uint(i) + cast_uint(ni);
    return cast_int(u <= cast_uint(INT_MAX) ? u : ~u);
  }
}
#endif


/*
** returns the 'main' position of an element in a table (that is,
** the index of its hash value). The key comes broken (tag in 'ktt'
** and value in 'vkl') so that we can call it on keys inserted into
** nodes.
 * 返回表中key的主位置节点(哈希值序号)
*/
static Node *mainposition (const Table *t, int ktt, const Value *kvl) {
  switch (withvariant(ktt)) {
    case LUA_TNUMINT:
      return hashint(t, ivalueraw(*kvl));
    case LUA_TNUMFLT:
      return hashmod(t, l_hashfloat(fltvalueraw(*kvl)));
    case LUA_TSHRSTR:
      return hashstr(t, tsvalueraw(*kvl));
    case LUA_TLNGSTR:
      return hashpow2(t, luaS_hashlongstr(tsvalueraw(*kvl)));
    case LUA_TBOOLEAN:
      return hashboolean(t, bvalueraw(*kvl));
    case LUA_TLIGHTUSERDATA:
      return hashpointer(t, pvalueraw(*kvl));
    case LUA_TLCF:
      return hashpointer(t, fvalueraw(*kvl));
    default:
      return hashpointer(t, gcvalueraw(*kvl));
  }
}


static Node *mainpositionTV (const Table *t, const TValue *key) {
  return mainposition(t, rawtt(key), valraw(key));
}


/*
** Check whether key 'k1' is equal to the key in node 'n2'.
** This equality is raw, so there are no metamethods. Floats
** with integer values have been normalized, so integers cannot
** be equal to floats. It is assumed that 'eqshrstr' is simply
** pointer equality, so that short strings are handled in the
** default case.
 * 检查k1是否等于节点n2的key
 * 这里的比较是通过raw得到,所以没有元方法
 * 浮点和整型已经被标准化,所以整型不会等于浮点
 * 长字符串使用eqlngstr比较指针,而短字符串用默认方式处理
*/
static int equalkey (const TValue *k1, const Node *n2) {
  if (rawtt(k1) != keytt(n2))  /* not the same variants? */
   return 0;  /* cannot be same key */
  switch (ttypetag(k1)) {
    case LUA_TNIL:
      return 1;
    case LUA_TNUMINT:
      return (ivalue(k1) == keyival(n2));
    case LUA_TNUMFLT:
      return luai_numeq(fltvalue(k1), fltvalueraw(keyval(n2)));
    case LUA_TBOOLEAN:
      return bvalue(k1) == bvalueraw(keyval(n2));
    case LUA_TLIGHTUSERDATA:
      return pvalue(k1) == pvalueraw(keyval(n2));
    case LUA_TLCF:
      return fvalue(k1) == fvalueraw(keyval(n2));
    case LUA_TLNGSTR:
      return luaS_eqlngstr(tsvalue(k1), keystrval(n2));
    default:
      return gcvalue(k1) == gcvalueraw(keyval(n2));
  }
}


/*
** True if value of 'alimit' is equal to the real size of the array
** part of table 't'. (Otherwise, the array part must be larger than
** 'alimit'.)
*/
#define limitequalsasize(t)	(isrealasize(t) || ispow2((t)->alimit))


/*
** Returns the real size of the 'array' array
 * 返回数组部分的实际大小
*/
LUAI_FUNC unsigned int luaH_realasize (const Table *t) {
  if (limitequalsasize(t))
    return t->alimit;  /* this is the size */
  else {
    unsigned int size = t->alimit;
    /* compute the smallest power of 2 not smaller than 'n' */
    size |= (size >> 1);
    size |= (size >> 2);
    size |= (size >> 4);
    size |= (size >> 8);
    size |= (size >> 16);
#if (INT_MAX >> 30 >> 1) > 0
    size |= (size >> 32);  /* int has more than 32 bits */
#endif
    size++;
    lua_assert(ispow2(size) && size/2 < t->alimit && t->alimit < size);
    return size;
  }
}


/*
** Check whether real size of the array is a power of 2.
** (If it is not, 'alimit' cannot be changed to any other value
** without changing the real size.)
*/
static int ispow2realasize (const Table *t) {
  return (!isrealasize(t) || ispow2(t->alimit));
}


static unsigned int setlimittosize (Table *t) {
  t->alimit = luaH_realasize(t);
  setrealasize(t);
  return t->alimit;
}


#define limitasasize(t)	check_exp(isrealasize(t), t->alimit)



/*
** "Generic" get version. (Not that generic: not valid for integers,
** which may be in array part, nor for floats with integral values.)
 * 获得key对应的val值
*/
static const TValue *getgeneric (Table *t, const TValue *key) {
  Node *n = mainpositionTV(t, key);
  for (;;) {  /* check whether 'key' is somewhere in the chain */
    if (equalkey(key, n))
      return gval(n);  /* that's it */
    else {
      int nx = gnext(n);
      if (nx == 0)
        return &absentkey;  /* not found */
      n += nx;
    }
  }
}


/*
** returns the index for 'k' if 'k' is an appropriate key to live in
** the array part of a table, 0 otherwise.
 * 如果key是一个在数组部分的合适的key（就是要确保key的值是正整数才能在数组部分）那么就返回key的索引，否则返回0
*/
static unsigned int arrayindex (lua_Integer k) {
  if (0 < k && l_castS2U(k) <= MAXASIZE)
    return cast_uint(k);  /* 'key' is an appropriate array index */
  else
    return 0;
}


/*
** returns the index of a 'key' for table traversals. First goes all
** elements in the array part, then elements in the hash part. The
** beginning of a traversal is signaled by 0.
 * 返回表遍历的key的索引。首先从数组部分开始遍历，然后才是哈希部分。遍历是由0开始的
*/
static unsigned int findindex (lua_State *L, Table *t, TValue *key,
                               unsigned int asize) {
  unsigned int i; // 此处的i保存的就是key对应的值（对于正整数而言就是它本身的值，其他的话就是0）
  if (ttisnil(key)) return 0;  /* first iteration */
  i = ttisinteger(key) ? arrayindex(ivalue(key)) : 0;
  if (i != 0 && i <= asize)  /* is 'key' inside array part? */
    return i;  /* yes; that's the index 根据i的值来判断是否在数组部分,是的话就直接返回i */
  else {
    const TValue *n = getgeneric(t, key); // 如果是在哈希部分的话，就先找出和这个key对应的mp
    if (unlikely(isabstkey(n)))
      luaG_runerror(L, "invalid key to 'next'");  /* key not found */
    i = cast_int(nodefromval(n) - gnode(t, 0));  /* key index in hash table */
    /* hash elements are numbered after array ones */
    return (i + 1) + asize;
  }
}

/*
 * luaH_next实现table的递归。通过上一个键，来找到下一个键值对。
 * 首先通过findindex找到key对应的index（对于数组部分，就是对应的key值;哈希部分的话，就是next对应的整数值（偏差））
 * 找到这个i之后，把i+1，就是下一个元素index，然后会把&t->array[i]放入栈顶
 */
int luaH_next (lua_State *L, Table *t, StkId key) {
  unsigned int asize = luaH_realasize(t);
  unsigned int i = findindex(L, t, s2v(key), asize);  /* find original key */
  for (; i < asize; i++) {  /* try first array part */
    if (!isempty(&t->array[i])) {  /* a non-empty entry? */
      setivalue(s2v(key), i + 1);
      setobj2s(L, key + 1, &t->array[i]);
      return 1;
    }
  }
  for (i -= asize; cast_int(i) < sizenode(t); i++) {  /* hash part */
    if (!isempty(gval(gnode(t, i)))) {  /* a non-empty entry? */
      Node *n = gnode(t, i);
      getnodekey(L, s2v(key), n);
      setobj2s(L, key + 1, gval(n));
      return 1;
    }
  }
  return 0;  /* no more elements */
}

/*
 * 释放哈希部分内存
 */
static void freehash (lua_State *L, Table *t) {
  if (!isdummy(t))
    luaM_freearray(L, t->node, cast_sizet(sizenode(t)));
}


/*
** {=============================================================
** Rehash
** ==============================================================
*/

/*
** Compute the optimal size for the array part of table 't'. 'nums' is a
** "count array" where 'nums[i]' is the number of integers in the table
** between 2^(i - 1) + 1 and 2^i. 'pna' enters with the total number of
** integer keys in the table and leaves with the number of keys that
** will go to the array part; return the optimal size.  (The condition
** 'twotoi > 0' in the for loop stops the loop if 'twotoi' overflows.)
 * 计算表t中理想的数组部分大小,
 * nums数组中第i个元素存放的是table中key在2的i-1次幂和2的i次幂之间的元素数量,pna是table中正整数的数量
 * 遍历这个nums数组，获得其范围区间内所包含的整数数量大于50%的最大索引，
 * 作为重新哈希之后的数组大小，超过这个范围的正整数，就分配到哈希部分了
*/
static unsigned int computesizes (unsigned int nums[], unsigned int *pna) {
  int i;
  unsigned int twotoi;  /* 2^i (candidate for optimal size) */
  unsigned int a = 0;  /* number of elements smaller than 2^i */
  unsigned int na = 0;  /* number of elements to go to array part */
  unsigned int optimal = 0;  /* optimal size for array part */
  /* loop while keys can fill more than half of total size */
  for (i = 0, twotoi = 1;
       twotoi > 0 && *pna > twotoi / 2;
       i++, twotoi *= 2) {
    a += nums[i];
    if (a > twotoi/2) {  /* more than half elements present? */
      optimal = twotoi;  /* optimal size (till now) */
      na = a;  /* all elements up to 'optimal' will go to array part */
    }
  }
  lua_assert((optimal == 0 || optimal / 2 < na) && na <= optimal);
  *pna = na;
  return optimal;
}

/*
 * 对nums的某些位置进行计算
 */
static int countint (lua_Integer key, unsigned int *nums) {
  unsigned int k = arrayindex(key); // 返回key的索引，返回的值不等于0才算key在t的数组部分
  if (k != 0) {  /* is 'key' an appropriate array index? */
    nums[luaO_ceillog2(k)]++;  /* count as such 求出k的以2为底的对数，在对结果进行向上取整  */
    return 1;
  }
  else
    return 0;
}


/*
** Count keys in array part of table 't': Fill 'nums[i]' with
** number of keys that will go into corresponding slice and return
** total number of non-nil keys.
 * numusearray函数遍历表中的数组部分，计算其中的元素数量，并更新对应的nums数组中的元素数量
*/
static unsigned int numusearray (const Table *t, unsigned int *nums) {
  int lg;
  unsigned int ttlg;  /* 2^lg */
  unsigned int ause = 0;  /* summation of 'nums' */
  unsigned int i = 1;  /* count to traverse all array keys */
  unsigned int asize = limitasasize(t);  /* real array size */
  /* traverse each slice */
  for (lg = 0, ttlg = 1; lg <= MAXABITS; lg++, ttlg *= 2) {
    unsigned int lc = 0;  /* counter */
    unsigned int lim = ttlg;
    if (lim > asize) {
      lim = asize;  /* adjust upper limit */
      if (i > lim)
        break;  /* no more elements to count */
    }
    /* count elements in range (2^(lg - 1), 2^lg] */
    //找出数组部分的2^(lg - 1), 2^lg之间的元素数量，然后对nums的对应格子进行赋值
    for (; i <= lim; i++) {
      if (!isempty(&t->array[i-1]))
        lc++;
    }
    nums[lg] += lc;
    ause += lc;
  }
  //然后记录下来array的数量并且返回
  return ause;
}

/*
 * 计算t中哈希部分的元素数量
 */
static int numusehash (const Table *t, unsigned int *nums, unsigned int *pna) {
  int totaluse = 0;  /* total number of elements */
  // 因为其中也可能存放了正整数，需要根据这里的正整数数量更新对应的nums数组元素数量 b
  int ause = 0;  /* elements added to 'nums' (can go to array part) */
  int i = sizenode(t);
  while (i--) {
    Node *n = &t->node[i];
    if (!isempty(gval(n))) {
      if (keyisinteger(n))
        ause += countint(keyival(n), nums);
      totaluse++;
    }
  }
  *pna += ause;
  return totaluse;
}


/*
** Creates an array for the hash part of a table with the given
** size, or reuses the dummy node if size is zero.
** The computation for size overflow is in two steps: the first
** comparison ensures that the shift in the second one does not
** overflow.
 * 对表的哈希部分大小进行设置
*/
static void setnodevector (lua_State *L, Table *t, unsigned int size) {
  if (size == 0) {  /* no elements to hash part? */
    //如果传进来的size为0，那么t的node(指向散列表起始位置的指针)是指一个虚拟节点
    t->node = cast(Node *, dummynode);  /* use common 'dummynode' */
    t->lsizenode = 0; // lsizenode是该表中以2为底的散列表大小的对数值，此处设置为0
    t->lastfree = NULL;  /* signal that it is using dummy node lastfree指向散列表最后位置的指针，此处设置为NUL */
  }
  else {
    // 如果size不为0，根据传入的size来计算哈希部分的lsizenode值
    int i;
    int lsize = luaO_ceillog2(size);
    // 如果size大于MAXHBITS或者1左移lsize位后大于MAXSIZE 报错
    if (lsize > MAXHBITS || (1u << lsize) > MAXHSIZE)
      luaG_runerror(L, "table overflow");
    // 求出lsize的2^lsize的结果
    size = twoto(lsize);
    // 申请哈希部分的空间了，让node指向新空间的起始地址
    t->node = luaM_newvector(L, size, Node);
    for (i = 0; i < (int)size; i++) {
      Node *n = gnode(t, i); // 返回t的哈希部分的第i个节点
      gnext(n) = 0; // gnext返回此节点的下一个节点的地址（其实是一个偏移）
      setnilkey(n); //  让n的u.key_tt字段设为NULL
      setempty(gval(n)); // 让n的i_val字段设为NULL
    }
    // 对t的lsizenode和lastfree设置就完成了哈希部分的初始化
    t->lsizenode = cast_byte(lsize);
    t->lastfree = gnode(t, size);  /* all positions are free lastfree指向的是size节点，也就是哈希部分的下一个节点 */
  }
}


/*
** (Re)insert all elements from the hash part of 'ot' into table 't'.
*/
static void reinsert (lua_State *L, Table *ot, Table *t) {
  int j;
  int size = sizenode(ot);
  for (j = 0; j < size; j++) {
    Node *old = gnode(ot, j);
    if (!isempty(gval(old))) {
      /* doesn't need barrier/invalidate cache, as entry was
         already present in the table */
      TValue k;
      getnodekey(L, &k, old);
      setobjt2t(L, luaH_set(L, t, &k), gval(old));
    }
  }
}


/*
** Exchange the hash part of 't1' and 't2'.
 * 交换连个表的哈希部分
*/
static void exchangehashpart (Table *t1, Table *t2) {
  lu_byte lsizenode = t1->lsizenode;
  Node *node = t1->node;
  Node *lastfree = t1->lastfree;
  t1->lsizenode = t2->lsizenode;
  t1->node = t2->node;
  t1->lastfree = t2->lastfree;
  t2->lsizenode = lsizenode;
  t2->node = node;
  t2->lastfree = lastfree;
}


/*
** Resize table 't' for the new given sizes. Both allocations (for
** the hash part and for the array part) can fail, which creates some
** subtleties. If the first allocation, for the hash part, fails, an
** error is raised and that is it. Otherwise, it copies the elements from
** the shrinking part of the array (if it is shrinking) into the new
** hash. Then it reallocates the array part.  If that fails, the table
** is in its original state; the function frees the new hash part and then
** raises the allocation error. Otherwise, it sets the new hash part
** into the table, initializes the new part of the array (if any) with
** nils and reinserts the elements of the old hash back into the new
** parts of the table.
 * 对表t重新分配大小,设置数组和哈希部分是可能失败的
 * 创建时需要的一些细节：如果先分配的哈希部分失败就会有错误抛出,
 * 当数组部分在收缩的时候,数组部分会先进入新的哈希部分,然后再重新分配数组部分
 * 如果重分配失败,这个表会变成初始状态,这个函数会释放新的哈希部分然后抛出分配错误
 * 它会设置新的哈希部分到表里,为新的数组部分初始化为nil(如果有),然后再把旧的哈希部分插入新的哈希部分
 *
*/
void luaH_resize (lua_State *L, Table *t, unsigned int newasize,
                                          unsigned int nhsize) {
  unsigned int i;
  Table newt;  /* to keep the new hash part */
  unsigned int oldasize = setlimittosize(t);
  TValue *newarray;
  /* create new hash part with appropriate size into 'newt' */
  // 创建哈希部分到新表
  setnodevector(L, &newt, nhsize);
  if (newasize < oldasize) {  /* will array shrink? 数组缩小? */
    // 尝试设置新的大小和交换哈希部分
    t->alimit = newasize;  /* pretend array has new size... */
    exchangehashpart(t, &newt);  /* and new hash */
    /* re-insert into the new hash the elements from vanishing slice */
    // 将数组缩小的部分的node,重新插入到哈希部分
    for (i = newasize; i < oldasize; i++) {
      if (!isempty(&t->array[i]))
        luaH_setint(L, t, i + 1, &t->array[i]);
    }
    // 存下久的数组大小, 并交换回来
    t->alimit = oldasize;  /* restore current size...  */
    exchangehashpart(t, &newt);  /* and hash (in case of errors)  */
  }
  /* allocate new array 分配新的数组部分内存 */
  newarray = luaM_reallocvector(L, t->array, oldasize, newasize, TValue);
  if (unlikely(newarray == NULL && newasize > 0)) {  /* allocation failed? */
    // 分配内存失败, 释放哈希部分, 抛错
    freehash(L, &newt);  /* release new hash part */
    luaM_error(L);  /* raise error (with array unchanged) */
  }
  /* allocation ok; initialize new part of the array */
  // 真的交换哈希部分，和设置数组部分
  exchangehashpart(t, &newt);  /* 't' has the new hash ('newt' has the old) */
  t->array = newarray;  /* set new array part */
  t->alimit = newasize;
  for (i = oldasize; i < newasize; i++)  /* clear new slice of the array 清理新的数组部分 */
     setempty(&t->array[i]);
  /* re-insert elements from old hash part into new parts 将旧表的哈希部分插入新表 */
  reinsert(L, &newt, t);  /* 'newt' now has the old hash */
  freehash(L, &newt);  /* free old hash part 释放旧表 */
}

/*
 * 挑战数组部分的大小
 */
void luaH_resizearray (lua_State *L, Table *t, unsigned int nasize) {
  int nsize = allocsizenode(t);
  luaH_resize(L, t, nasize, nsize);
}

/*
** nums[i] = number of keys 'k' where 2^(i - 1) < k <= 2^i
 * 重新哈希操作
 * nums这个位图的意义在于：nums数组中第i个元素存放的是key在2的i-1次幂和2的i次幂之间的元素数量
*/
static void rehash (lua_State *L, Table *t, const TValue *ek) {
  unsigned int asize;  /* optimal size for array part */
  unsigned int na;  /* number of keys in the array part  */
  unsigned int nums[MAXABITS + 1];
  int i;
  int totaluse;
  for (i = 0; i <= MAXABITS; i++) nums[i] = 0;  /* reset counts */
  setlimittosize(t);
  na = numusearray(t, nums);  /* count keys in array part 数组部分的元素数量，并且对nums数组的对应元素进行赋值 */
  totaluse = na;  /* all those keys are integer keys */
  // 遍历t中的哈希部分，因为其中可能也存在了正整数，需要根据这里的正整数数量更新对应的nums数组元素数量
  totaluse += numusehash(t, nums, &na);  /* count keys in hash part */
  /* count extra key 计算额外新添加的key，并且更新nums数组和totaluse */
  if (ttisinteger(ek))
    na += countint(ivalue(ek), nums);
  totaluse++;
  /* compute new size for array part */
  // 此时，nums数组已经有了当前这个table中所有正整数的分配统计，调用computesizes计算哈希之后的数组部分的大小
  asize = computesizes(nums, &na);
  /* resize the table to new computed sizes */
  luaH_resize(L, t, asize, totaluse - na);
}



/*
** }=============================================================
*/

/*
 * 创建空表
 */
Table *luaH_new (lua_State *L) {
  GCObject *o = luaC_newobj(L, LUA_TTABLE, sizeof(Table));
  Table *t = gco2t(o);
  t->metatable = NULL;
  t->flags = cast_byte(~0);
  t->array = NULL;
  t->alimit = 0;
  setnodevector(L, t, 0);
  return t;
}

/*
 * 释放表的内存
 */
void luaH_free (lua_State *L, Table *t) {
  freehash(L, t);
  luaM_freearray(L, t->array, luaH_realasize(t));
  luaM_free(L, t);
}

/*
 * 从哈希部分的后面开始找可以用的空闲节点,没有找到的话就返回NULL重新哈希
 * 哈希填充是从后面开始的
 */
static Node *getfreepos (Table *t) {
  if (!isdummy(t)) {
    while (t->lastfree > t->node) {
      t->lastfree--;
      if (keyisnil(t->lastfree))
        return t->lastfree;
    }
  }
  return NULL;  /* could not find a free place */
}



/*
** inserts a new key into a hash table; first, check whether key's main
** position is free. If not, check whether colliding node is in its main
** position or not: if it is not, move colliding node to an empty place and
** put new key in its main position; otherwise (colliding node is in its main
** position), new key goes to an empty position.
 * 插入一个新的key到哈希表中
 * 首先检查key的主位置是否空的,如果不是就检查已经存的节点是不是在主位置里面,
 * 如果不是就将要已经存的节点移到空的地方并把这个新的key放进主位置
*/
TValue *luaH_newkey (lua_State *L, Table *t, const TValue *key) {
  Node *mp;
  TValue aux;
  if (unlikely(ttisnil(key)))
    // key为nil就报错
    luaG_runerror(L, "table index is nil");
  else if (ttisfloat(key)) {
    // key为浮点数
    lua_Number f = fltvalue(key);
    lua_Integer k;
    if (luaV_flttointeger(f, &k, 0)) {  /* does key fit in an integer? 是否可以转换为整数 */
      // 用构造出来的aux对key初始化
      setivalue(&aux, k);
      key = &aux;  /* insert it as an integer */
    }
    else if (unlikely(luai_numisnan(f)))
      luaG_runerror(L, "table index is NaN"); // 未定义数字 抛错
  }
  mp = mainpositionTV(t, key); // 获取key在table中的主位置节点
  if (!isempty(gval(mp)) || isdummy(t)) {  /* main position is taken? */
    Node *othern;
    Node *f = getfreepos(t);  /* get a free place */
    if (f == NULL) {  /* cannot find a free place? 没有空闲的位置? */
      rehash(L, t, key);  /* grow table 扩容 */
      /* whatever called 'newkey' takes care of TM cache */
      return luaH_set(L, t, key);  /* insert key into grown table 拓展之后再插入 */
    }
    lua_assert(!isdummy(t));
    othern = mainposition(t, keytt(mp), &keyval(mp)); // 获取mp节点key的主节点位置
    //为什么有othern != mp这种情况，表示mp这个node原本不属于这个位置的，只是占用而已。
    //因为mainposition取出来的node，有可能本来就不是存放在这个位置的。而是之前与某一个位置冲突，而放在lastfree里的。
    //所以othern != mp这种情况，表示的是原来不应存放在这个位置的node移到lastfree，而这个位置被新node占据
    if (othern != mp) {  /* is colliding node out of its main position?  */
      // thern开始开始向后遍历一直到mp的前一个位置,然后将mp的数据移动到空节点上
      /* yes; move colliding node into free position 根据othern ,逐个next，获得mp的上一个节点 */
      while (othern + gnext(othern) != mp)  /* find previous */
        othern += gnext(othern);
      gnext(othern) = cast_int(f - othern);  /* rechain to point to 'f' 让othern指向f节点 */
      *f = *mp;  /* copy colliding node into free pos. (mp->next also goes) 将mp值复制到前面找到的空闲节点f  */
      // 如果mp还有下个节点，需要将f的下个节点修正，将mp的next置为0.
      if (gnext(mp) != 0) {
        gnext(f) += cast_int(mp - f);  /* correct 'next' */
        gnext(mp) = 0;  /* now 'mp' is free */
      }
      setempty(gval(mp)); // 将mp的值置为nil（此时mp位置就可以放置新的键值对了）
    }
    else {  /* colliding node is in its own main position mp的位置原本就是属于这个位置的.也就是说与这个位置的哈希值是碰撞的 */
      /* new node will go into free position 新节点会放到新的空位置 */
      if (gnext(mp) != 0)
        gnext(f) = cast_int((mp + gnext(mp)) - f);  /* chain new position */
      else lua_assert(gnext(f) == 0);
      // 空节点插入到主位置节点之后
      gnext(mp) = cast_int(f - mp);
      mp = f;
    }
  }
  // 设置一下mp的k，并且返回mp的val，让用户操作
  setnodekey(L, mp, key);
  luaC_barrierback(L, obj2gco(t), key);
  lua_assert(isempty(gval(mp)));
  return gval(mp);
}


/*
** Search function for integers. If integer is inside 'alimit', get it
** directly from the array part. Otherwise, if 'alimit' is not equal to
** the real size of the array, key still can be in the array part. In
** this case, try to avoid a call to 'luaH_realasize' when key is just
** one more than the limit (so that it can be incremented without
** changing the real size of the array).
 * 获取整型key对应的值
 * 如果这个int值小于alimit,会在数组部分找,如果alimit不等于数组部分的真实大小,
 * key仍在数组部分,这种情况下当只有key一个超过了limit,尽量避免调用luaH_realasize
 * (这样的增量没有实际改变数组部分的真实大小)
*/
const TValue *luaH_getint (Table *t, lua_Integer key) {
  if (l_castS2U(key) - 1u < t->alimit)  /* (1 <= key && key <= t->alimit)? */
    return &t->array[key - 1];
  else if (!limitequalsasize(t) &&  /* key still may be in the array part? */
           (l_castS2U(key) == t->alimit + 1 ||
            l_castS2U(key) - 1u < luaH_realasize(t))) {
    t->alimit = cast_uint(key);  /* probably '#t' is here now */
    return &t->array[key - 1];
  }
  else {
    Node *n = hashint(t, key);
    for (;;) {  /* check whether 'key' is somewhere in the chain */
      if (keyisinteger(n) && keyival(n) == key)
        return gval(n);  /* that's it */
      else {
        int nx = gnext(n);
        if (nx == 0) break;
        n += nx;
      }
    }
    return &absentkey;
  }
}


/*
** search function for short strings
 * 获取短字符串的值
*/
const TValue *luaH_getshortstr (Table *t, TString *key) {
  Node *n = hashstr(t, key);
  lua_assert(key->tt == LUA_TSHRSTR);
  for (;;) {  /* check whether 'key' is somewhere in the chain */
    if (keyisshrstr(n) && eqshrstr(keystrval(n), key))
      return gval(n);  /* that's it */
    else {
      int nx = gnext(n);
      if (nx == 0)
        return &absentkey;  /* not found */
      n += nx;
    }
  }
}

/*
 * 获取字符串key的值
 */
const TValue *luaH_getstr (Table *t, TString *key) {
  if (key->tt == LUA_TSHRSTR)
    return luaH_getshortstr(t, key);
  else {  /* for long strings, use generic case */
    TValue ko;
    setsvalue(cast(lua_State *, NULL), &ko, key);
    return getgeneric(t, &ko);
  }
}


/*
** main search function
*/
const TValue *luaH_get (Table *t, const TValue *key) {
  switch (ttypetag(key)) {
    case LUA_TSHRSTR: return luaH_getshortstr(t, tsvalue(key));
    case LUA_TNUMINT: return luaH_getint(t, ivalue(key));
    case LUA_TNIL: return &absentkey;
    case LUA_TNUMFLT: {
      lua_Integer k;
      if (luaV_flttointeger(fltvalue(key), &k, 0)) /* index is an integral? */
        return luaH_getint(t, k);  /* use specialized version */
      /* else... */
    }  /* FALLTHROUGH */
    default:
      return getgeneric(t, key);
  }
}


/*
** beware: when using this function you probably need to check a GC
** barrier and invalidate the TM cache.
 * 查询key是否存在,不存在就创建并返回索引,存在就直接返回索引
*/
TValue *luaH_set (lua_State *L, Table *t, const TValue *key) {
  const TValue *p = luaH_get(t, key);
  if (!isabstkey(p))
    return cast(TValue *, p);
  else return luaH_newkey(L, t, key);
}

/*
 * 把t中key的value设置为传进来的value
 *
 */
void luaH_setint (lua_State *L, Table *t, lua_Integer key, TValue *value) {
  const TValue *p = luaH_getint(t, key);
  TValue *cell;
  if (!isabstkey(p))
    cell = cast(TValue *, p);
  else {
    TValue k;
    setivalue(&k, key);
    cell = luaH_newkey(L, t, &k);
  }
  setobj2t(L, cell, value);
}


/*
** Try to find a boundary in the hash part of table 't'. From the
** caller, we know that 'j' is zero or present and that 'j + 1' is
** present. We want to find a larger key that is absent from the
** table, so that we can do a binary search between the two keys to
** find a boundary. We keep doubling 'j' until we get an absent index.
** If the doubling would overflow, we try LUA_MAXINTEGER. If it is
** absent, we are ready for the binary search. ('j', being max integer,
** is larger or equal to 'i', but it cannot be equal because it is
** absent while 'i' is present; so 'j > i'.) Otherwise, 'j' is a
** boundary. ('j + 1' cannot be a present integer key because it is
** not a valid integer in Lua.)
*/
static lua_Unsigned hash_search (Table *t, lua_Unsigned j) {
  lua_Unsigned i;
  if (j == 0) j++;  /* the caller ensures 'j + 1' is present */
  do {
    i = j;  /* 'i' is a present index */
    if (j <= l_castS2U(LUA_MAXINTEGER) / 2)
      j *= 2;
    else {
      j = LUA_MAXINTEGER;
      if (isempty(luaH_getint(t, j)))  /* t[j] not present? */
        break;  /* 'j' now is an absent index */
      else  /* weird case */
        return j;  /* well, max integer is a boundary... */
    }
  } while (!isempty(luaH_getint(t, j)));  /* repeat until an absent t[j] */
  /* i < j  &&  t[i] present  &&  t[j] absent */
  while (j - i > 1u) {  /* do a binary search between them */
    lua_Unsigned m = (i + j) / 2;
    if (isempty(luaH_getint(t, m))) j = m;
    else i = m;
  }
  return i;
}

/*
 * 数组二分查找
 */
static unsigned int binsearch (const TValue *array, unsigned int i,
                                                    unsigned int j) {
  while (j - i > 1u) {  /* binary search */
    unsigned int m = (i + j) / 2;
    if (isempty(&array[m - 1])) j = m;
    else i = m;
  }
  return i;
}


/*
** Try to find a boundary in table 't'. (A 'boundary' is an integer index
** such that t[i] is present and t[i+1] is absent, or 0 if t[1] is absent
** and 'maxinteger' if t[maxinteger] is present.)
** (In the next explanation, we use Lua indices, that is, with base 1.
** The code itself uses base 0 when indexing the array part of the table.)
** The code starts with 'limit', a position in the array part that may
** be a boundary.
** (1) If 't[limit]' is empty, there must be a boundary before it.
** As a common case (e.g., after 't[#t]=nil'), check whether 'hint-1'
** is present. If so, it is a boundary. Otherwise, do a binary search
** between 0 and limit to find a boundary. In both cases, try to
** use this boundary as the new 'limit', as a hint for the next call.
** (2) If 't[limit]' is not empty and the array has more elements
** after 'limit', try to find a boundary there. Again, try first
** the special case (which should be quite frequent) where 'limit+1'
** is empty, so that 'limit' is a boundary. Otherwise, check the
** last element of the array part (set it as a new limit). If it is empty,
** there must be a boundary between the old limit (present) and the new
** limit (absent), which is found with a binary search. (This boundary
** always can be a new limit.)
** (3) The last case is when there are no elements in the array part
** (limit == 0) or its last element (the new limit) is present.
** In this case, must check the hash part. If there is no hash part,
** the boundary is 0. Otherwise, if 'limit+1' is absent, 'limit' is
** a boundary. Finally, if 'limit+1' is present, call 'hash_search'
** to find a boundary in the hash part of the table. (In those
** cases, the boundary is not inside the array part, and therefore
** cannot be used as a new limit.)
 * 获取表t的size
*/
lua_Unsigned luaH_getn (Table *t) {
  unsigned int limit = t->alimit;
  if (limit > 0 && isempty(&t->array[limit - 1])) {
    /* (1) there must be a boundary before 'limit' */
    if (limit >= 2 && !isempty(&t->array[limit - 2])) {
      /* 'limit - 1' is a boundary; can it be a new limit? */
      if (ispow2realasize(t) && !ispow2(limit - 1)) {
        t->alimit = limit - 1;
        setnorealasize(t);
      }
      return limit - 1;
    }
    else {  /* must search for a boundary in [0, limit] */
      unsigned int boundary = binsearch(t->array, 0, limit);
      /* can this boundary represent the real size of the array? */
      if (ispow2realasize(t) && boundary > luaH_realasize(t) / 2) {
        t->alimit = boundary;  /* use it as the new limit */
        setnorealasize(t);
      }
      return boundary;
    }
  }
  /* 'limit' is zero or present in table */
  if (!limitequalsasize(t)) {
    /* (2) 'limit' > 0 and array has more elements after 'limit' */
    if (isempty(&t->array[limit]))  /* 'limit + 1' is empty? */
      return limit;  /* this is the boundary */
    /* else, try last element in the array */
    limit = luaH_realasize(t);
    if (isempty(&t->array[limit - 1])) {  /* empty? */
      /* there must be a boundary in the array after old limit,
         and it must be a valid new limit */
      unsigned int boundary = binsearch(t->array, t->alimit, limit);
      t->alimit = boundary;
      return boundary;
    }
    /* else, new limit is present in the table; check the hash part */
  }
  /* (3) 'limit' is the last element and either is zero or present in table */
  lua_assert(limit == luaH_realasize(t) &&
             (limit == 0 || !isempty(&t->array[limit - 1])));
  if (isdummy(t) || isempty(luaH_getint(t, cast(lua_Integer, limit + 1))))
    return limit;  /* 'limit + 1' is absent... */
  else  /* 'limit + 1' is also present */
    return hash_search(t, limit);
}



#if defined(LUA_DEBUG)

Node *luaH_mainposition (const Table *t, const TValue *key) {
  return mainpositionTV(t, key);
}

int luaH_isdummy (const Table *t) { return isdummy(t); }

#endif
