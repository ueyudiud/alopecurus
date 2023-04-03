/**
 *@file akw.h
 */

#ifndef akw_h_
#define akw_h_

#define KEYWORD_LIST(_) \
    _(_ENV, "_ENV")     \
    _(AS, "as")         \
    _(BREAK, "break")   \
    _(CASE, "case")     \
    _(CONST, "const")   \
    _(CONTINUE, "continue") \
    _(DO, "do")         \
    _(ELSE, "else")     \
    _(EXPORT, "export") \
    _(FALSE, "false")   \
    _(FN, "fn")         \
    _(FOR, "for")       \
    _(IF, "if")         \
    _(IMPORT, "import") \
    _(IN, "in")         \
    _(IS, "is")         \
    _(LET, "let")       \
    _(LOOP, "loop")     \
    _(MATCH, "match")   \
    _(NIL, "nil")       \
    _(RETURN, "return") \
    _(TRUE, "true")     \
	_(WHILE, "while")   \
	_(UNDERSCORE, "_")

#define OPERATOR_LIST(_) \
    _(LBK, "(") \
    _(RBK, ")") \
    _(LSQ, "[") \
    _(RSQ, "]") \
    _(LBR, "{") \
    _(RBR, "}") \
    _(SHARP, "#") \
    _(AT, "@") \
    _(TILDE, "~") \
    _(COMMA, ",") \
    _(SEMI, ";") \
    _(DOT, ".") \
    _(BDOT, "..") \
    _(TDOT, "...") \
    _(COLON, ":") \
    _(BCOLON, "::") \
    _(PLUS, "+") \
    _(MINUS, "-") \
    _(STAR, "*") \
    _(LSLASH, "/") \
    _(PERCENT, "%") \
    _(ASSIGN, "=") \
    _(EQ, "==") \
    _(BANG, "!") \
    _(NE, "!=") \
    _(GT, ">") \
    _(GE, ">=") \
    _(SHL, "<<") \
    _(LT, "<") \
    _(LE, "<=") \
    _(SHR, ">>") \
    _(HAT, "^") \
    _(AMP, "&") \
    _(BAMP, "&&") \
    _(BAR, "|") \
    _(BBAR, "||") \
    _(QUESTION, "?") \
    _(QDOT, "?.") \
    _(ELVIS, "?:")

#endif /* akw_h_ */
