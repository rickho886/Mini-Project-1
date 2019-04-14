#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#define MAXLEN 256
#define TBLSIZE 65535
typedef enum {UNKNOWN, END, INT, ID, ADDSUB, MULDIV, ANDORXOR, ASSIGN,
LPAREN, RPAREN, ENDFILE} TokenSet;

typedef enum {MISPAREN, NOTNUMID, NOTFOUND, RUNOUT} ErrorType;

typedef struct {
    char name[MAXLEN];
    int val;
} Symbol;
extern Symbol table[TBLSIZE];


typedef struct _Node {
    char lexeme[MAXLEN];
    TokenSet data;
    int val;
    struct _Node *left, *right;
} BTNode;

extern int match (TokenSet token);
extern void advance(void);
extern char* getLexeme(void);
extern int getval(void);
extern int setval(char *str, int val);
extern BTNode* makeNode(TokenSet tok, const char *lexe);
extern void freeTree(BTNode *root);
extern BTNode* factor(void);
extern BTNode* term(void);
extern BTNode* expr(void);
extern void statement(void);
extern void error();
extern int evaluateTree(BTNode *root);
extern void printPrefix(BTNode *root);
extern void resetID();

Symbol table[TBLSIZE];

int sbcount = 0;

static TokenSet getToken(void);
static TokenSet lookahead = UNKNOWN;
static char lexeme[MAXLEN];

int regId = -1;
int varId = 3;
int start = 1;
int change = 0;
int check = 1;
Symbol input[TBLSIZE];

TokenSet getToken(void)
{
    int i;
    char c;

    while ( (c = fgetc(stdin)) == ' ' || c== '\t' );  // ©¿²¤ªÅ¥Õ¦r¤¸
    if (isdigit(c)) {
        lexeme[0] = c;
        c = fgetc(stdin);
        i = 1;
        while (isdigit(c) && i<MAXLEN) {
            lexeme[i] = c;
            ++i;
            c = fgetc(stdin);
        }
        ungetc(c, stdin);
        lexeme[i] = '\0';
        return INT;
    } else if (c == '+' || c == '-' || c == '&' || c == '|' || c == '^') {
        lexeme[0] = c;
        lexeme[1] = '\0';
        return ADDSUB;
    } else if (c == '*' || c == '/') {
        lexeme[0] = c;
        lexeme[1] = '\0';
        return MULDIV;
    } else if (c == '\n') {
        lexeme[0] = '\0';
        return END;
    } else if (c == '=') {
        strcpy(lexeme, "=");
        return ASSIGN;
    } else if (c == '(') {
        strcpy(lexeme, "(");
        return LPAREN;
    } else if (c == ')') {
        strcpy(lexeme, ")");
        return RPAREN;
    } else if (isalpha(c) || c == '_') {
        lexeme[0] = c;
        c = fgetc(stdin);
        i = 1;
        while (isalpha(c) || isdigit(c) || c == '_') {
            lexeme[i] = c;
            ++i;
            c = fgetc(stdin);
        }
        ungetc(c, stdin);
        lexeme[i] = '\0';
        return ID;
    } else if (c == EOF) {
        return ENDFILE;
    } else {
        error();
    }
}

void advance(void)
{
    lookahead = getToken();
}

int match(TokenSet token)
{
    if (lookahead == UNKNOWN) advance();
    return token == lookahead;
}

char* getLexeme(void)
{
    return lexeme;
}

int getval(void)
{
    int i, retval, found;

    if (match(INT)) {
        retval = atoi(getLexeme());
    } else if (match(ID)) {
        i = 0; found = 0; retval = 0;
        while (i<sbcount && !found) {
            if (strcmp(getLexeme(), table[i].name)==0) {
                retval = table[i].val;
                found = 1;
                break;
            } else {
                i++;
            }
        }
        if (!found) {
            if (sbcount < TBLSIZE) {
                strcpy(table[sbcount].name, getLexeme());
                table[sbcount].val = 0;
                sbcount++;
            } else {
                error();
            }
        }
    }
    return retval;
}
int setval(char *str, int val)
{
    int i, retval;
    i = 0;
    while (i<sbcount) {
        if (strcmp(str, table[i].name)==0) {
            table[i].val = val;
            retval = val;
            break;
        } else {
            i++;
        }
    }
    return retval;
}
/* create a node without any child.*/
BTNode* makeNode(TokenSet tok, const char *lexe){
    BTNode *node = (BTNode*) malloc(sizeof(BTNode));
    strcpy(node->lexeme, lexe);
    node->data = tok;
    node->val = 0;
    node->left = NULL;
    node->right = NULL;
    return node;
}
/* clean a tree.*/
void freeTree(BTNode *root){
    if (root!=NULL) {
        freeTree(root->left);
        freeTree(root->right);
        free(root);
    }
}
/*factor := INT | ADDSUB INT | ADDSUB ID | ID ASSIGN expr | ID | LPAREN expr RPAREN*/
BTNode* factor(void)
{
    BTNode* retp = NULL;
    char tmpstr[MAXLEN];

    if (match(INT)) {
        retp =  makeNode(INT, getLexeme());
        retp->val = getval();
        advance();
    } else if (match(ID)) {
        BTNode* left = makeNode(ID, getLexeme());
        left->val = getval();
        strcpy(tmpstr, getLexeme());
        advance();
        if (match(ASSIGN)) {
            retp = makeNode(ASSIGN, getLexeme());
            advance();
            retp->right = expr();
            retp->left = left;
        } else {
            retp = left;
        }
    } else if (match(ADDSUB)) {
        strcpy(tmpstr, getLexeme());
        advance();
        if (match(ID) || match(INT)) {
            retp = makeNode(ADDSUB, tmpstr);
            if (match(ID))
                retp->right = makeNode(ID, getLexeme());
            else
                retp->right = makeNode(INT, getLexeme());
            retp->right->val = getval();
            retp->left = makeNode(INT, "0");
            retp->left->val = 0;
            advance();
        } else {
            error();
        }
    } else if (match(LPAREN)) {
        advance();
        retp = expr();
        if (match(RPAREN)) {
            advance();
        } else {
            error();
        }
    } else {
        error();
    }
    return retp;
}
/*  term        := factor term_tail
    term_tail := MULDIV factor term_tail | NIL*/
BTNode* term(void)
{
    BTNode *retp, *left;
    retp = left = factor();

    while (match(MULDIV)) {
        retp = makeNode(MULDIV, getLexeme());
        advance();
        retp->right = factor();
        retp->left = left;
        left = retp;
    }
    return retp;
}
/*  expr        := term expr_tail
  expr_tail   := ADDSUB term expr_tail | NIL*/
BTNode* expr(void)
{
    BTNode *retp, *left;
    //int retval;
    retp = left = term();
    while (match(ADDSUB)) {
        retp = makeNode(ADDSUB, getLexeme());
        advance();
        retp->right = term();
        retp->left = left;
        left = retp;
    }
    return retp;
}
/*statement   := ENDFILE | END | expr END*/
void statement(void)
{
    BTNode* retp;
    if (match(ENDFILE)) {
        printf("MOV r0 [0]\n");
        printf("MOV r1 [4]\n");
        printf("MOV r2 [8]\n");
        printf("EXIT 0\n");
        exit(0);
    } else if (match(END)) {
        advance();
    } else {
        retp = expr();
        if (match(END)) {
            evaluateTree(retp);
            freeTree(retp);
            resetID();
            advance();
        }
    }
}
void error()
{
    printf("EXIT 1");
    exit(0);
}

void resetID()
{
    regId = -1;
    start = 1;
    change = 0;
    check = 1;
}

int evaluateTree(BTNode *root)
{
    int retval = 0, lv, rv;
    if (root != NULL)
    {
        switch (root->data)
        {
        case ID:
            if(root->lexeme[1] == '\0' && (root->lexeme[0] == 'x' || root->lexeme[0] == 'y' || root->lexeme[0] == 'z')) {
                if(change == 0) printf("MOV r%d [%d]\n", ++regId, (root->lexeme[0]-'x') * 4);
                else if(change == 1) {
                    printf("MOV [%d] r%d\n", (root->lexeme[0]-'x') * 4, regId);
                    change ^= 1;
                }
            }
            else {
                int idx = 3;
                int create = 1;
                for(; idx < varId; idx++) {
                    if(strcmp(input[idx].name, root->lexeme) == 0) {
                        create = 0;
                        break;
                    }
                }
                if(create == 1 && check == 0) {
                    strcpy(input[varId].name, root->lexeme);
                    input[varId].val = varId;
                    idx = varId;
                    varId++;
                }
                if(input[idx].val == 0) {
                    error();
                }
                if(change == 0) {
                    printf("MOV r%d [%d]\n", ++regId, input[idx].val*4);
                }
                else if(change == 1) {
                    printf("MOV [%d] r%d\n", input[idx].val*4, regId);
                    change ^= 1;
                }
            }
            break;
        case INT:
            retval = root->val;
            printf("MOV r%d %d\n", ++regId, retval);
            break;
        case ASSIGN:
        case ADDSUB:
        case MULDIV:
            if(start == 1) {
                start = 0;
                if(root->lexeme[0] != '=')
                    error();
                rv = evaluateTree(root->right);
                change = 1;
                check = 0;
                lv = evaluateTree(root->left);
            } else {
                lv = evaluateTree(root->left);
                rv = evaluateTree(root->right);
            }
            if (strcmp(root->lexeme, "+") == 0) {
                printf("ADD r%d r%d\n", regId-1, regId);
                regId--;
            }
            else if (strcmp(root->lexeme, "-") == 0) {
                printf("SUB r%d r%d\n", regId-1, regId);
                regId--;
            }
            else if (strcmp(root->lexeme, "*") == 0) {
                printf("MUL r%d r%d\n", regId-1, regId);
                regId--;
            }
            else if (strcmp(root->lexeme, "/") == 0) {
                printf("DIV r%d r%d\n", regId-1, regId);
                regId--;
            }
            else if (strcmp(root->lexeme, "&") == 0) {
                printf("AND r%d r%d\n", regId-1, regId);
                regId--;
            }
            else if (strcmp(root->lexeme, "|") == 0) {
                printf("OR r%d r%d\n", regId-1, regId);
                regId--;
            }
            else if (strcmp(root->lexeme, "^") == 0) {
                printf("XOR r%d r%d\n", regId-1, regId);
                regId--;
            }
            else if (strcmp(root->lexeme, "=") == 0)
                retval = setval(root->left->lexeme, rv);
            break;
        default:
            retval = 0;
        }
    }
    return retval;
}


/* print a tree by pre-order. */
void printPrefix(BTNode *root)
{
    if (root != NULL)
    {
        printf("%s ", root->lexeme);
        printPrefix(root->left);
        printPrefix(root->right);
    }
}

int main()
{
    while (1) {
        statement();
    }
    return 0;
}
