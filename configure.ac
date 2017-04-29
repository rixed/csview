AC_INIT(csview,0.1)
m4_include([m4/ocaml.m4])

AC_PROG_CC

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_CHECK_OCAML_PKG(batteries net_codecs parsercombinator owww)

if test -x "$(which xdg-open)" ; then
  DEFAULT_OPEN="xdg-open"
elif test -x "$(which open)" ; then
  DEFAULT_OPEN="open"
fi
AC_SUBST([DEFAULT_OPEN])

DEFAULT_WWW_ROOT="$datadir/csview"
AC_ARG_WITH(
  [www-root],
  AS_HELP_STRING([--with-www-root], [where to store static files]),
  [DEFAULT_WWW_ROOT="$withval"],
  [DEFAULT_WWW_ROOT="/usr/local/share/csview"])
AC_SUBST([DEFAULT_WWW_ROOT])

AC_CONFIG_FILES([Makefile make.common compilConfig.ml])
AC_OUTPUT
