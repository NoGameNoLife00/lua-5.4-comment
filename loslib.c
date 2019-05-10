/*
** $Id: loslib.c $
** Standard Operating System library
 * 标准系统操作库
** See Copyright Notice in lua.h
*/

#define loslib_c
#define LUA_LIB

#include "lprefix.h"


#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "lua.h"

#include "lauxlib.h"
#include "lualib.h"


/*
** {==================================================================
** List of valid conversion specifiers for the 'strftime' function;
** options are grouped by length; group of length 2 start with '||'.
** ===================================================================
*/
#if !defined(LUA_STRFTIMEOPTIONS)	/* { */

/* options for ANSI C 89 (only 1-char options) */
#define L_STRFTIMEC89		"aAbBcdHIjmMpSUwWxXyYZ%"

/* options for ISO C 99 and POSIX */
#define L_STRFTIMEC99 "aAbBcCdDeFgGhHIjmMnprRStTuUVwWxXyYzZ%" \
    "||" "EcECExEXEyEY" "OdOeOHOIOmOMOSOuOUOVOwOWOy"  /* two-char options */

/* options for Windows */
#define L_STRFTIMEWIN "aAbBcdHIjmMpSUwWxXyYzZ%" \
    "||" "#c#x#d#H#I#j#m#M#S#U#w#W#y#Y"  /* two-char options */

#if defined(LUA_USE_WINDOWS)
#define LUA_STRFTIMEOPTIONS	L_STRFTIMEWIN
#elif defined(LUA_USE_C89)
#define LUA_STRFTIMEOPTIONS	L_STRFTIMEC89
#else  /* C99 specification */
#define LUA_STRFTIMEOPTIONS	L_STRFTIMEC99
#endif

#endif					/* } */
/* }================================================================== */


/*
** {==================================================================
** Configuration for time-related stuff
** ===================================================================
*/

#if !defined(l_time_t)		/* { */
/*
** type to represent time_t in Lua
 * lua中的time_t表现
*/
#define l_timet			lua_Integer
#define l_pushtime(L,t)		lua_pushinteger(L,(lua_Integer)(t))

static time_t l_checktime (lua_State *L, int arg) {
  lua_Integer t = luaL_checkinteger(L, arg);
  luaL_argcheck(L, (time_t)t == t, arg, "time out-of-bounds");
  return (time_t)t;
}

#endif				/* } */


#if !defined(l_gmtime)		/* { */
/*
** By default, Lua uses gmtime/localtime, except when POSIX is available,
** where it uses gmtime_r/localtime_r
 * lua默认使用gmtime(格林威治GMT时间)和localtime(本地时间),当使用POSIX时，会使用gmtime_r和localtime_r
*/

#if defined(LUA_USE_POSIX)	/* { */

#define l_gmtime(t,r)		gmtime_r(t,r)
#define l_localtime(t,r)	localtime_r(t,r)

#else				/* }{ */

/* ISO C definitions */
#define l_gmtime(t,r)		((void)(r)->tm_sec, gmtime(t))
#define l_localtime(t,r)  	((void)(r)->tm_sec, localtime(t))

#endif				/* } */

#endif				/* } */

/* }================================================================== */


/*
** {==================================================================
** Configuration for 'tmpnam':
** By default, Lua uses tmpnam except when POSIX is available, where
** it uses mkstemp.
 * tmpnam函数的配置：默认使用tmpnam,当POSIX可用时，使用mkstemp
 * mkstemp会以此文件名创建一个文件以回避安全风险
** ===================================================================
*/
#if !defined(lua_tmpnam)	/* { */

#if defined(LUA_USE_POSIX)	/* { */

#include <unistd.h>

#define LUA_TMPNAMBUFSIZE	32

#if !defined(LUA_TMPNAMTEMPLATE)
#define LUA_TMPNAMTEMPLATE	"/tmp/lua_XXXXXX"
#endif

#define lua_tmpnam(b,e) { \
        strcpy(b, LUA_TMPNAMTEMPLATE); \
        e = mkstemp(b); \
        if (e != -1) close(e); \
        e = (e == -1); }

#else				/* }{ */

/* ISO C definitions */
#define LUA_TMPNAMBUFSIZE	L_tmpnam
#define lua_tmpnam(b,e)		{ e = (tmpnam(b) == NULL); }

#endif				/* } */

#endif				/* } */
/* }================================================================== */



/*
 * 调用系统解释器执行command
 * 命令成功运行完毕，第一个返回值就是 true， 否则是 nil otherwise
 * 在第一个返回值之后，函数返回一个字符串加一个数字
 */
static int os_execute (lua_State *L) {
  const char *cmd = luaL_optstring(L, 1, NULL);
  int stat = system(cmd); // 执行DOS命令
  if (cmd != NULL)
    return luaL_execresult(L, stat);
  else {
    lua_pushboolean(L, stat);  /* true if there is a shell 当不带参数调用时,shell存在的时候返回true */
    return 1;
  }
}

/*
 * 删除指定名字的文件(在 POSIX 系统上可以是一个空目录)如果函数失败，返回 nil 加一个错误描述串及出错码
 */
static int os_remove (lua_State *L) {
  const char *filename = luaL_checkstring(L, 1);
  return luaL_fileresult(L, remove(filename) == 0, filename);
}

/*
 * 将名字为fromname的文件或目录更名为toname.如果函数失败,返回nil加一个错误描述串及出错码。
 */
static int os_rename (lua_State *L) {
  const char *fromname = luaL_checkstring(L, 1);
  const char *toname = luaL_checkstring(L, 2);
  return luaL_fileresult(L, rename(fromname, toname) == 0, NULL);
}

/*
 * 返回一个可用于临时文件的文件名字符串.这个文件在使用前必须显式打开,不再使用时需要显式删除.
 */
static int os_tmpname (lua_State *L) {
  char buff[LUA_TMPNAMBUFSIZE];
  int err;
  lua_tmpnam(buff, err);
  if (err)
    return luaL_error(L, "unable to generate a unique filename");
  lua_pushstring(L, buff);
  return 1;
}

/*
 * 返回进程环境变量 varname 的值,如果该变量未定义,返回nil.
 */
static int os_getenv (lua_State *L) {
  lua_pushstring(L, getenv(luaL_checkstring(L, 1)));  /* if NULL push nil */
  return 1;
}

/*
 * 返回程序使用的按秒计CPU时间的近似值
 */
static int os_clock (lua_State *L) {
  lua_pushnumber(L, ((lua_Number)clock())/(lua_Number)CLOCKS_PER_SEC);
  return 1;
}


/*
** {======================================================
** Time/Date operations
 * Time/Date 操作
** { year=%Y, month=%m, day=%d, hour=%H, min=%M, sec=%S,
**   wday=%w+1, yday=%j, isdst=? }
** =======================================================
*/

static void setfield (lua_State *L, const char *key, int value) {
  lua_pushinteger(L, value);
  lua_setfield(L, -2, key);
}

static void setboolfield (lua_State *L, const char *key, int value) {
  if (value < 0)  /* undefined? */
    return;  /* does not set field */
  lua_pushboolean(L, value);
  lua_setfield(L, -2, key);
}


/*
** Set all fields from structure 'tm' in the table on top of the stack
 * 用tm结构里的值设置栈顶的table值
*/
static void setallfields (lua_State *L, struct tm *stm) {
  setfield(L, "sec", stm->tm_sec);
  setfield(L, "min", stm->tm_min);
  setfield(L, "hour", stm->tm_hour);
  setfield(L, "day", stm->tm_mday);
  setfield(L, "month", stm->tm_mon + 1);
  setfield(L, "year", stm->tm_year + 1900);
  setfield(L, "wday", stm->tm_wday + 1);
  setfield(L, "yday", stm->tm_yday + 1);
  setboolfield(L, "isdst", stm->tm_isdst); // 夏令时标志
}


static int getboolfield (lua_State *L, const char *key) {
  int res;
  res = (lua_getfield(L, -1, key) == LUA_TNIL) ? -1 : lua_toboolean(L, -1);
  lua_pop(L, 1);
  return res;
}


/* maximum value for date fields (to avoid arithmetic overflows with 'int')
 * 日期域的最大值
 */
#if !defined(L_MAXDATEFIELD)
#define L_MAXDATEFIELD	(INT_MAX / 2)
#endif

static int getfield (lua_State *L, const char *key, int d, int delta) {
  int isnum;
  int t = lua_getfield(L, -1, key);  /* get field and its type */
  lua_Integer res = lua_tointegerx(L, -1, &isnum);
  if (!isnum) {  /* field is not an integer? */
    if (t != LUA_TNIL)  /* some other value? */
      return luaL_error(L, "field '%s' is not an integer", key);
    else if (d < 0)  /* absent field; no default? */
      return luaL_error(L, "field '%s' missing in date table", key);
    res = d;
  }
  else {
    if (!(-L_MAXDATEFIELD <= res && res <= L_MAXDATEFIELD))
      return luaL_error(L, "field '%s' is out-of-bound", key);
    res -= delta;
  }
  lua_pop(L, 1);
  return (int)res;
}


static const char *checkoption (lua_State *L, const char *conv,
                                ptrdiff_t convlen, char *buff) {
  const char *option = LUA_STRFTIMEOPTIONS;
  int oplen = 1;  /* length of options being checked */
  for (; *option != '\0' && oplen <= convlen; option += oplen) {
    if (*option == '|')  /* next block? */
      oplen++;  /* will check options with next length (+1) */
    else if (memcmp(conv, option, oplen) == 0) {  /* match? */
      memcpy(buff, conv, oplen);  /* copy valid option to buffer */
      buff[oplen] = '\0';
      return conv + oplen;  /* return next item */
    }
  }
  luaL_argerror(L, 1,
    lua_pushfstring(L, "invalid conversion specifier '%%%s'", conv));
  return conv;  /* to avoid warnings */
}


/* maximum size for an individual 'strftime' item */
#define SIZETIMEFMT	250

/*
 * 返回一个包含日期及时刻的字符串或表.格式化方法取决于所给字符串format
 * 如果format以'!'打头,日期以协调世界时格式化.在这个可选字符项之后,如果format为字符串"*t",date返回有后续域的表:
 * year(四位数字),month(1–12),day(1–31),hour(0–23),min(0–59),sec(0–61),wday(星期几,星期天为1),yday(当年的第几天),
 * 以及isdst(夏令时标记,一个布尔量). 对于最后一个域,如果该信息不提供的话就不存在.
 * 如果format并非"*t",date以字符串形式返回,格式化方法遵循ISO C函数strftime的规则.
 * 如果不传参数调用,date返回一个合理的日期时间串,格式取决于宿主程序以及当前的区域设置(即,os.date()等价于os.date("%c")).
 * 在非POSIX系统上,由于这个函数依赖C函数gmtime和localtime,它可能并非线程安全的.
 */
static int os_date (lua_State *L) {
  size_t slen;
  const char *s = luaL_optlstring(L, 1, "%c", &slen);
  time_t t = luaL_opt(L, l_checktime, 2, time(NULL));
  const char *se = s + slen;  /* 's' end */
  struct tm tmr, *stm;
  if (*s == '!') {  /* UTC? */
    stm = l_gmtime(&t, &tmr);
    s++;  /* skip '!' */
  }
  else
    stm = l_localtime(&t, &tmr);
  if (stm == NULL)  /* invalid date? */
    return luaL_error(L,
                 "time result cannot be represented in this installation");
  if (strcmp(s, "*t") == 0) {
    lua_createtable(L, 0, 9);  /* 9 = number of fields */
    setallfields(L, stm);
  }
  else {
    char cc[4];  /* buffer for individual conversion specifiers */
    luaL_Buffer b;
    cc[0] = '%';
    luaL_buffinit(L, &b);
    while (s < se) {
      if (*s != '%')  /* not a conversion specifier? */
        luaL_addchar(&b, *s++);
      else {
        size_t reslen;
        char *buff = luaL_prepbuffsize(&b, SIZETIMEFMT);
        s++;  /* skip '%' */
        s = checkoption(L, s, se - s, cc + 1);  /* copy specifier to 'cc' */
        reslen = strftime(buff, SIZETIMEFMT, cc, stm);
        luaL_addsize(&b, reslen);
      }
    }
    luaL_pushresult(&b);
  }
  return 1;
}

/*
 * 当不传参数时,返回当前时刻.如果传入一张表,就返回由这张表表示的时刻.这张表必须包含域year,month,及day;
 * 可以包含有hour(默认为 12), min(默认为0),sec(默认为0),以及isdst(默认为 nil).
 */
static int os_time (lua_State *L) {
  time_t t;
  if (lua_isnoneornil(L, 1))  /* called without args? */
    t = time(NULL);  /* get current time */
  else {
    struct tm ts;
    luaL_checktype(L, 1, LUA_TTABLE);
    lua_settop(L, 1);  /* make sure table is at the top */
    ts.tm_sec = getfield(L, "sec", 0, 0);
    ts.tm_min = getfield(L, "min", 0, 0);
    ts.tm_hour = getfield(L, "hour", 12, 0);
    ts.tm_mday = getfield(L, "day", -1, 0);
    ts.tm_mon = getfield(L, "month", -1, 1);
    ts.tm_year = getfield(L, "year", -1, 1900);
    ts.tm_isdst = getboolfield(L, "isdst");
    t = mktime(&ts);
    setallfields(L, &ts);  /* update fields with normalized values */
  }
  if (t != (time_t)(l_timet)t || t == (time_t)(-1))
    return luaL_error(L,
                  "time result cannot be represented in this installation");
  l_pushtime(L, t);
  return 1;
}

/*
 * 返回以秒计算的时刻t1到t2的差值.(这里的时刻是由 os.time 返回的值).在POSIX,Windows,和其它一些系统中,这个值就等于t2-t1
 */
static int os_difftime (lua_State *L) {
  time_t t1 = l_checktime(L, 1);
  time_t t2 = l_checktime(L, 2);
  lua_pushnumber(L, (lua_Number)difftime(t1, t2));
  return 1;
}

/* }====================================================== */

/*
 * 设置当前区域
 */
static int os_setlocale (lua_State *L) {
  static const int cat[] = {LC_ALL, LC_COLLATE, LC_CTYPE, LC_MONETARY,
                      LC_NUMERIC, LC_TIME};
  static const char *const catnames[] = {"all", "collate", "ctype", "monetary",
     "numeric", "time", NULL};
  const char *l = luaL_optstring(L, 1, NULL);
  int op = luaL_checkoption(L, 2, "all", catnames);
  lua_pushstring(L, setlocale(cat[op], l));
  return 1;
}

/*
 * 调用ISO C函数的exit来终止主程序,
 * lua中第一个参数等于true,返回码为EXIT_SUCCESS, 等于false就为EXIT_FAILURE,
 * 如果是数字,返回码就等于数字值. 第二个参数默认为真,在退出前关闭虚拟机
 */
static int os_exit (lua_State *L) {
  int status;
  if (lua_isboolean(L, 1))
    status = (lua_toboolean(L, 1) ? EXIT_SUCCESS : EXIT_FAILURE);
  else
    status = (int)luaL_optinteger(L, 1, EXIT_SUCCESS);
  if (lua_toboolean(L, 2))
    lua_close(L);
  if (L) exit(status);  /* 'if' to avoid warnings for unreachable 'return' */
  return 0;
}


static const luaL_Reg syslib[] = {
  {"clock",     os_clock},
  {"date",      os_date},
  {"difftime",  os_difftime},
  {"execute",   os_execute},
  {"exit",      os_exit},
  {"getenv",    os_getenv},
  {"remove",    os_remove},
  {"rename",    os_rename},
  {"setlocale", os_setlocale},
  {"time",      os_time},
  {"tmpname",   os_tmpname},
  {NULL, NULL}
};

/* }====================================================== */



LUAMOD_API int luaopen_os (lua_State *L) {
  luaL_newlib(L, syslib);
  return 1;
}

