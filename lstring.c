/*
** $Id: lstring.c $
** String table (keeps all strings handled by Lua)
** See Copyright Notice in lua.h
*/

#define lstring_c
#define LUA_CORE

#include "lprefix.h"


#include <string.h>

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"


/*
** Lua will use at most ~(2^LUAI_HASHLIMIT) bytes from a string to
** compute its hash
 * lua最多会从一个字符串中截取2^5字节来计算其哈希值(不足32字节的全部用上，超出的截取最后的32字节)
*/
#if !defined(LUAI_HASHLIMIT)
#define LUAI_HASHLIMIT		5
#endif



/*
** Maximum size for string table.
*/
#define MAXSTRTB	cast_int(luaM_limitN(MAX_INT, TString*))


/*
** equality for long strings
 * 比较长字符串是否相等
*/
int luaS_eqlngstr (TString *a, TString *b) {
  size_t len = a->u.lnglen;
  lua_assert(a->tt == LUA_TLNGSTR && b->tt == LUA_TLNGSTR);
  return (a == b) ||  /* same instance or... 是否同一实例对象 */
    ((len == b->u.lnglen) &&  /* equal length and ... 长度是否相等 */
     (memcmp(getstr(a), getstr(b), len) == 0));  /* equal contents 检查字符 */
}

/*
 * 计算字符串哈希值
 * 对于长度小于2^LUAI_HASHLIMIT的字符串，每一个字节都会参加hash计算,
 * 长度大于等于32的字符串,从末尾开始,每(1 >> LUAI_HASHLIMIT) + 1位参与hash计算
 * 此处的随机种子是在创建虚拟机的global_State(全局状态机)时构造并存储在global_State中的
 */
unsigned int luaS_hash (const char *str, size_t l, unsigned int seed) {
  unsigned int h = seed ^ cast_uint(l);
  size_t step = (l >> LUAI_HASHLIMIT) + 1;
  for (; l >= step; l -= step)
    h ^= ((h<<5) + (h>>2) + cast_byte(str[l - 1]));
  return h;
}

/*
 * 计算长字符串哈希值
 */
unsigned int luaS_hashlongstr (TString *ts) {
  lua_assert(ts->tt == LUA_TLNGSTR);
  if (ts->extra == 0) {  /* no hash? 判断是否哈希过?  */
    ts->hash = luaS_hash(getstr(ts), ts->u.lnglen, ts->hash);
    ts->extra = 1;  /* now it has its hash */
  }
  return ts->hash;
}


static void tablerehash (TString **vect, int osize, int nsize) {
  int i;
  for (i = osize; i < nsize; i++)  /* clear new elements 清理拓展的空间 */
    vect[i] = NULL;
  for (i = 0; i < osize; i++) {  /* rehash old part of the array */
    TString *p = vect[i];
    vect[i] = NULL;
    while (p) {  /* for each string in the list */
      TString *hnext = p->u.hnext;  /* save next */
      unsigned int h = lmod(p->hash, nsize);  /* new position */
      p->u.hnext = vect[h];  /* chain it into array */
      vect[h] = p;
      p = hnext;
    }
  }
}


/*
** Resize the string table. If allocation fails, keep the current size.
** (This can degrade performance, but any non-zero size should work
** correctly.)
 * 改变字符串散列桶大小. 分配失败就会保持当前大小
 * (任一非零大小都应该有效,虽然会降低性能)
*/
void luaS_resize (lua_State *L, int nsize) {
  stringtable *tb = &G(L)->strt;
  int osize = tb->size;
  TString **newvect;
  if (nsize < osize)  /* shrinking table? 缩容? */
    tablerehash(tb->hash, osize, nsize);  /* depopulate shrinking part */
  newvect = luaM_reallocvector(L, tb->hash, osize, nsize, TString*);
  if (unlikely(newvect == NULL)) {  /* reallocation failed? 分配内存失败? */
    if (nsize < osize)  /* was it shrinking table? */
      tablerehash(tb->hash, nsize, osize);  /* restore to original size 还原原始大小 */
    /* leave table as it was */
  }
  else {  /* allocation succeeded */
    tb->hash = newvect;
    tb->size = nsize;
    if (nsize > osize)
      tablerehash(newvect, osize, nsize);  /* rehash for new size 给拓若的内存重新哈希 */
  }
}


/*
** Clear API string cache. (Entries cannot be empty, so fill them with
** a non-collectable string.)
 * 清理全局表的字符串缓存
 *
*/
void luaS_clearcache (global_State *g) {
  int i, j;
  for (i = 0; i < STRCACHE_N; i++)
    for (j = 0; j < STRCACHE_M; j++) {
    if (iswhite(g->strcache[i][j]))  /* will entry be collected? 对象即将被回收? */
      g->strcache[i][j] = g->memerrmsg;  /* replace it with something fixed 用固定信息代替 */
    }
}


/*
** Initialize the string table and the string cache
 * 初始化字符串散列桶和字符串缓存
*/
void luaS_init (lua_State *L) {
  global_State *g = G(L);
  int i, j;
  stringtable *tb = &G(L)->strt;
  tb->hash = luaM_newvector(L, MINSTRTABSIZE, TString*);
  tablerehash(tb->hash, 0, MINSTRTABSIZE);  /* clear array */
  tb->size = MINSTRTABSIZE;
  /* pre-create memory-error message memerrmsg指向一个不可回收的错误信息字符串  */
  g->memerrmsg = luaS_newliteral(L, MEMERRMSG);
  luaC_fix(L, obj2gco(g->memerrmsg));  /* it should never be collected */
  for (i = 0; i < STRCACHE_N; i++)  /* fill cache with valid strings 所有缓存指向改错误信息字符串 */
    for (j = 0; j < STRCACHE_M; j++)
      g->strcache[i][j] = g->memerrmsg;
}



/*
** creates a new string object
 * 创建新的字符串对象
*/
static TString *createstrobj (lua_State *L, size_t l, int tag, unsigned int h) {
  TString *ts;
  GCObject *o;
  size_t totalsize;  /* total size of TString object */
  totalsize = sizelstring(l);
  o = luaC_newobj(L, tag, totalsize);
  ts = gco2ts(o);
  ts->hash = h;
  ts->extra = 0;
  getstr(ts)[l] = '\0';  /* ending 0 */
  return ts;
}

/*
 * 创建长字符串对象
 */
TString *luaS_createlngstrobj (lua_State *L, size_t l) {
  TString *ts = createstrobj(L, l, LUA_TLNGSTR, G(L)->seed);
  ts->u.lnglen = l;
  return ts;
}

/*
 * 从全局字符串哈希桶中移除指定字符串
 */
void luaS_remove (lua_State *L, TString *ts) {
  stringtable *tb = &G(L)->strt;
  TString **p = &tb->hash[lmod(ts->hash, tb->size)];
  while (*p != ts)  /* find previous element */
    p = &(*p)->u.hnext;
  *p = (*p)->u.hnext;  /* remove element from its list */
  tb->nuse--;
}

/*
 * 字符串哈希桶的拓容
 */
static void growstrtab (lua_State *L, stringtable *tb) {
  if (unlikely(tb->nuse == MAX_INT)) {  /* too many strings? 字符串太多,超过了int的最大值? */
    luaC_fullgc(L, 1);  /* try to free some... */
    if (tb->nuse == MAX_INT)  /* still too many? gc了字符串数量没有减少,将抛错 */
      luaM_error(L);  /* cannot even create a message... */
  }
  if (tb->size <= MAXSTRTB / 2)  /* can grow string table? 当前大小小于等于最大值的一半就可以拓容 */
    luaS_resize(L, tb->size * 2);
}


/*
** Checks whether short string exists and reuses it or creates a new one.
 * 检查短字符串是否存在并复用/创建
*/
static TString *internshrstr (lua_State *L, const char *str, size_t l) {
  TString *ts;
  global_State *g = G(L);
  stringtable *tb = &g->strt;
  unsigned int h = luaS_hash(str, l, g->seed);
  TString **list = &tb->hash[lmod(h, tb->size)];
  lua_assert(str != NULL);  /* otherwise 'memcmp'/'memcpy' are undefined */
  // 查找字符串是否已经存在
  for (ts = *list; ts != NULL; ts = ts->u.hnext) {
    if (l == ts->shrlen && (memcmp(str, getstr(ts), l * sizeof(char)) == 0)) {
      /* found! */
      if (isdead(g, ts))  /* dead (but not collected yet)? 字符串死亡状态但是还没有被回收? */
        changewhite(ts);  /* resurrect it 复用它 */
      return ts;
    }
  }
  /* else must create a new string 创建字符串 */
  if (tb->nuse >= tb->size) {  /* need to grow string table? 需要拓容? */
    growstrtab(L, tb);
    list = &tb->hash[lmod(h, tb->size)];  /* rehash with new size 用新大小来取哈希值 */
  }
  ts = createstrobj(L, l, LUA_TSHRSTR, h);
  memcpy(getstr(ts), str, l * sizeof(char));
  ts->shrlen = cast_byte(l);
  ts->u.hnext = *list;
  *list = ts;
  tb->nuse++;
  return ts;
}


/*
** new string (with explicit length) 新建指定长度字符串对象
*/
TString *luaS_newlstr (lua_State *L, const char *str, size_t l) {
  if (l <= LUAI_MAXSHORTLEN)  /* short string? */
    return internshrstr(L, str, l);
  else {
    TString *ts;
    if (unlikely(l >= (MAX_SIZE - sizeof(TString))/sizeof(char)))
      luaM_toobig(L);
    ts = luaS_createlngstrobj(L, l);
    memcpy(getstr(ts), str, l * sizeof(char));
    return ts;
  }
}


/*
** Create or reuse a zero-terminated string, first checking in the
** cache (using the string address as a key). The cache can contain
** only zero-terminated strings, so it is safe to use 'strcmp' to
** check hits.
 * 创建或者复用一个\0结尾的字符串, 首先先检查缓存(字符串地址作为key来哈希)
 * 缓存里可以只有\0结尾的字符串,所以使用strcmp是可以安全命中的
*/
TString *luaS_new (lua_State *L, const char *str) {
  unsigned int i = point2uint(str) % STRCACHE_N;  /* hash */
  int j;
  TString **p = G(L)->strcache[i];
  for (j = 0; j < STRCACHE_M; j++) {
    if (strcmp(str, getstr(p[j])) == 0)  /* hit? */
      return p[j];  /* that is it */
  }
  /* normal route */
  for (j = STRCACHE_M - 1; j > 0; j--)
    p[j] = p[j - 1];  /* move out last element 成员全部向后移一位*/
  /* new element is first in the list 新成员放到最前面 */
  p[0] = luaS_newlstr(L, str, strlen(str));
  return p[0];
}

/*
 * 创建一个userdata
 */
Udata *luaS_newudata (lua_State *L, size_t s, int nuvalue) {
  Udata *u;
  int i;
  GCObject *o;
  if (unlikely(s > MAX_SIZE - udatamemoffset(nuvalue)))
    luaM_toobig(L);
  o = luaC_newobj(L, LUA_TUSERDATA, sizeudata(nuvalue, s));
  u = gco2u(o);
  u->len = s;
  u->nuvalue = nuvalue;
  u->metatable = NULL;
  for (i = 0; i < nuvalue; i++)
    setnilvalue(&u->uv[i].uv);
  return u;
}

