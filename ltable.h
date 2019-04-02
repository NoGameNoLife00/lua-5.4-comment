/*
** $Id: ltable.h $
** Lua tables (hash)
** See Copyright Notice in lua.h
*/

#ifndef ltable_h
#define ltable_h

#include "lobject.h"

// t 为table结构体, n为table中哈希部分的一个节点
#define gnode(t,i)	(&(t)->node[i]) // 用来返回t的哈希部分的第i个节点，此处的i就是哈希部分0~(size-1)的整数值
#define gval(n)		(&(n)->i_val) // gval用来返回节点中的值（是一个TValue类型）
#define gnext(n)	((n)->u.next) // gnext用来返回此节点的下一个节点的地址（其实是一个偏移）


#define invalidateTMcache(t)	((t)->flags = 0) // 清空table的元表


/* true when 't' is using 'dummynode' as its hash part
 * 如果lastfree为NULL，说明这个table没有哈希部分
 */
#define isdummy(t)		((t)->lastfree == NULL)


/* allocated size for hash nodes
 * 获得哈希部分分配的大小
 */
#define allocsizenode(t)	(isdummy(t) ? 0 : sizenode(t))


/* returns the Node, given the value of a table entry
 * 转成talbe中的Node结构
 */
#define nodefromval(v) 	cast(Node *, (v))


LUAI_FUNC const TValue *luaH_getint (Table *t, lua_Integer key);
LUAI_FUNC void luaH_setint (lua_State *L, Table *t, lua_Integer key,
                                                    TValue *value);
LUAI_FUNC const TValue *luaH_getshortstr (Table *t, TString *key);
LUAI_FUNC const TValue *luaH_getstr (Table *t, TString *key);
LUAI_FUNC const TValue *luaH_get (Table *t, const TValue *key);
LUAI_FUNC TValue *luaH_newkey (lua_State *L, Table *t, const TValue *key);
LUAI_FUNC TValue *luaH_set (lua_State *L, Table *t, const TValue *key);
LUAI_FUNC Table *luaH_new (lua_State *L);
LUAI_FUNC void luaH_resize (lua_State *L, Table *t, unsigned int nasize,
                                                    unsigned int nhsize);
LUAI_FUNC void luaH_resizearray (lua_State *L, Table *t, unsigned int nasize);
LUAI_FUNC void luaH_free (lua_State *L, Table *t);
LUAI_FUNC int luaH_next (lua_State *L, Table *t, StkId key);
LUAI_FUNC lua_Unsigned luaH_getn (Table *t);
LUAI_FUNC unsigned int luaH_realasize (const Table *t);


#if defined(LUA_DEBUG)
LUAI_FUNC Node *luaH_mainposition (const Table *t, const TValue *key);
LUAI_FUNC int luaH_isdummy (const Table *t);
#endif


#endif
