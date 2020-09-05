#include <cctype>
#include <cstdlib>
#include <deque>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <sstream>
#include <streambuf>
#include <vector>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/EHPersonalities.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/CodeGen/CommandFlags.inc"
#include "llvm/CodeGen/GCMetadata.h"
#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/CodeGen/LinkAllCodegenComponents.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/InitializePasses.h"
#include "llvm/PassRegistry.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

typedef enum TokenType {
    TOKEN_INT = 256,
    TOKEN_PLUS,
    TOKEN_HYPHEN,
    TOKEN_ASTERISK,
    TOKEN_SLASH,
    TOKEN_EQUAL,
    TOKEN_EQUAL_2,
    TOKEN_PLUS_EQUAL,
    TOKEN_HYPHEN_EQUAL,
    TOKEN_ASTERISK_EQUAL,
    TOKEN_SLASH_EQUAL,
    TOKEN_PLUS_PLUS,
    TOKEN_HYPHEN_HYPHEN,
    TOKEN_NOT,
    TOKEN_NOT_EQUAL,
    TOKEN_GT,
    TOKEN_LT,
    TOKEN_GT_EQUAL,
    TOKEN_LT_EQUAL,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_AND_2,
    TOKEN_OR_2,
    TOKEN_XOR,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_SEMICOLON,
    TOKEN_COLON,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_AT,
    TOKEN_STR_LITERAL,
    TOKEN_CHAR_LITERAL,
    TOKEN_IDENT,
    TOKEN_VAR,
    TOKEN_RETURN,
    TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_FUN,
    TOKEN_FOR,
    TOKEN_WHILE,
    TOKEN_AS,
    TOKEN_STRUCT,
    TOKEN_EOF,
} TokenType;

class Token {
  private:
    std::string token_str;
    TokenType type;

  public:
    Token(std::string token_str, TokenType type)
        : token_str(token_str), type(type){};
    std::string as_str();
    TokenType get_token_type();
};

std::string Token::as_str() { return token_str; }

TokenType Token::get_token_type() { return type; }

class Lexer {
  private:
    std::string source;
    int cur_pos;
    std::deque<Token> peeked;

    bool eof();
    char read_char();
    char cur_char();
    char escape(char meta);
    TokenType get_ident_type(std::string str);
    Token tokenize_number();
    Token tokenize_ident();
    Token tokenize_symbol();
    Token next();

  public:
    Lexer(std::string source) : source(source), cur_pos(0){};
    std::string type_to_str(TokenType type);
    Token peek(unsigned int offset);
    Token read();
    Token curr();
};

bool Lexer::eof() { return cur_pos >= source.size(); }

char Lexer::read_char() { return source[cur_pos++]; }

char Lexer::cur_char() { return source[cur_pos]; }

char Lexer::escape(char meta) {
    switch (meta) {
    case 'n':
        return '\n';
    case 'r':
        return '\r';
    case 't':
        return '\t';
    case 'b':
        return '\b';
    case 'f':
        return '\f';
    case 'v':
        return '\f';
    case '0':
        return '\0';
    case '\'':
        return '\'';
    case '"':
        return '"';
    case '\\':
        return '\\';
    default:
        throw "Error: unknown escape character";
    }
}

TokenType Lexer::get_ident_type(std::string str) {
    if (str == "var") {
        return TOKEN_VAR;
    } else if (str == "return") {
        return TOKEN_RETURN;
    } else if (str == "if") {
        return TOKEN_IF;
    } else if (str == "else") {
        return TOKEN_ELSE;
    } else if (str == "fun") {
        return TOKEN_FUN;
    } else if (str == "for") {
        return TOKEN_FOR;
    } else if (str == "while") {
        return TOKEN_WHILE;
    } else if (str == "as") {
        return TOKEN_AS;
    } else if (str == "struct") {
        return TOKEN_STRUCT;
    } else {
        return TOKEN_IDENT;
    }
}

Token Lexer::tokenize_number() {
    int begin = cur_pos;
    while (!eof() && std::isdigit(cur_char())) {
        read_char();
    }
    int end = cur_pos;

    std::string result_str = source.substr(begin, end - begin);
    return Token(result_str, TOKEN_INT);
}

Token Lexer::tokenize_ident() {
    int begin = cur_pos;
    while (!eof() && (std::isalnum(cur_char()) || cur_char() == '_')) {
        read_char();
    }
    int end = cur_pos;
    std::string result_str = source.substr(begin, end - begin);
    TokenType type = get_ident_type(result_str);
    return Token(result_str, type);
}

Token Lexer::tokenize_symbol() {
    char sym = read_char();
    if (sym == '+') {
        if (cur_char() == '=') {
            read_char();
            return Token("+=", TOKEN_PLUS_EQUAL);
        } else if (cur_char() == '+') {
            read_char();
            return Token("++", TOKEN_PLUS_PLUS);
        }
        return Token("+", TOKEN_PLUS);
    } else if (sym == '-') {
        if (cur_char() == '=') {
            read_char();
            return Token("-=", TOKEN_HYPHEN_EQUAL);
        } else if (cur_char() == '-') {
            read_char();
            return Token("--", TOKEN_HYPHEN_HYPHEN);
        }
        return Token("-", TOKEN_HYPHEN);
    } else if (sym == '*') {
        if (cur_char() == '=') {
            read_char();
            return Token("*=", TOKEN_ASTERISK_EQUAL);
        }
        return Token("*", TOKEN_ASTERISK);
    } else if (sym == '/') {
        if (cur_char() == '=') {
            read_char();
            return Token("/=", TOKEN_SLASH_EQUAL);
        }
        return Token("/", TOKEN_SLASH);
    } else if (sym == '(') {
        return Token("(", TOKEN_LPAREN);
    } else if (sym == ')') {
        return Token(")", TOKEN_RPAREN);
    } else if (sym == '{') {
        return Token("{", TOKEN_LBRACE);
    } else if (sym == '}') {
        return Token("}", TOKEN_RBRACE);
    } else if (sym == '=') {
        if (cur_char() == '=') {
            read_char();
            return Token("==", TOKEN_EQUAL_2);
        } else {
            return Token("=", TOKEN_EQUAL);
        }
    } else if (sym == '!') {
        if (cur_char() == '=') {
            read_char();
            return Token("!=", TOKEN_NOT_EQUAL);
        } else {
            return Token("!", TOKEN_NOT);
        }
    } else if (sym == '>') {
        if (cur_char() == '=') {
            read_char();
            return Token(">=", TOKEN_GT_EQUAL);
        } else {
            return Token(">", TOKEN_GT);
        }
    } else if (sym == '<') {
        if (cur_char() == '=') {
            read_char();
            return Token("<=", TOKEN_LT_EQUAL);
        } else {
            return Token("<", TOKEN_LT);
        }
    } else if (sym == '&') {
        if (cur_char() == '&') {
            read_char();
            return Token("&&", TOKEN_AND_2);
        } else {
            return Token("&", TOKEN_AND);
        }
    } else if (sym == '|') {
        if (cur_char() == '|') {
            read_char();
            return Token("||", TOKEN_OR_2);
        } else {
            return Token("|", TOKEN_OR);
        }
    } else if (sym == '^') {
        return Token("^", TOKEN_XOR);
    } else if (sym == ';') {
        return Token(";", TOKEN_SEMICOLON);
    } else if (sym == ':') {
        return Token(":", TOKEN_COLON);
    } else if (sym == ',') {
        return Token(",", TOKEN_COMMA);
    } else if (sym == '.') {
        return Token(".", TOKEN_DOT);
    } else if (sym == '@') {
        return Token("@", TOKEN_AT);
    } else if (sym == '"') {
        std::string buf;
        while (!eof() && cur_char() != '"') {
            char ch = read_char();
            if (ch == '\\') {
                buf.push_back(escape(read_char()));
            } else {
                buf.push_back(ch);
            }
        }
        if (cur_char() != '"') {
            throw "Error: string literal must be enclosed with double quote";
        }
        read_char();

        return Token(buf, TOKEN_STR_LITERAL);
    } else if (sym == '\'') {
        std::string buf;
        if (cur_char() == '\\') {
            read_char();
            buf.push_back(escape(read_char()));
        } else {
            buf.push_back(read_char());
        }
        if (cur_char() != '\'') {
            throw "Error: char literal must be enclosed with single quote";
        }
        read_char();

        return Token(buf, TOKEN_CHAR_LITERAL);
    } else {
        std::stringstream stream;
        stream << "Error: unknown symbol: " << sym << std::endl;
        throw stream.str();
    }
}

Token Lexer::next() {
    if (std::isspace(cur_char()) || cur_char() == '\n') {
        while (!eof() && (std::isspace(cur_char()) || cur_char() == '\n'))
            read_char();
    }
    if (cur_pos >= source.size() || cur_char() == '\0') {
        return Token("\0", TOKEN_EOF);
    }
    if (std::isdigit(cur_char())) {
        return tokenize_number();
    } else if (std::isalpha(cur_char()) || cur_char() == '_') {
        return tokenize_ident();
    } else if (std::ispunct(cur_char())) {
        return tokenize_symbol();
    } else {
        std::stringstream stream;
        stream << "Error: unknown character: " << (int)cur_char() << std::endl;
        throw stream.str();
    }
}

std::string Lexer::type_to_str(TokenType type) {
    switch (type) {
    case TOKEN_INT:
        return "int";
    case TOKEN_PLUS:
        return "+";
    case TOKEN_HYPHEN:
        return "-";
    case TOKEN_ASTERISK:
        return "*";
    case TOKEN_SLASH:
        return "/";
    case TOKEN_EQUAL:
        return "=";
    case TOKEN_EQUAL_2:
        return "==";
    case TOKEN_PLUS_EQUAL:
        return "+=";
    case TOKEN_HYPHEN_EQUAL:
        return "-=";
    case TOKEN_ASTERISK_EQUAL:
        return "*=";
    case TOKEN_SLASH_EQUAL:
        return "/=";
    case TOKEN_PLUS_PLUS:
        return "++";
    case TOKEN_HYPHEN_HYPHEN:
        return "--";
    case TOKEN_NOT:
        return "!";
    case TOKEN_NOT_EQUAL:
        return "!=";
    case TOKEN_GT:
        return ">";
    case TOKEN_LT:
        return "<";
    case TOKEN_GT_EQUAL:
        return ">=";
    case TOKEN_LT_EQUAL:
        return "<=";
    case TOKEN_AND:
        return "&";
    case TOKEN_AND_2:
        return "&&";
    case TOKEN_OR:
        return "|";
    case TOKEN_OR_2:
        return "||";
    case TOKEN_XOR:
        return "^";
    case TOKEN_LPAREN:
        return "(";
    case TOKEN_RPAREN:
        return ")";
    case TOKEN_LBRACE:
        return "{";
    case TOKEN_RBRACE:
        return "}";
    case TOKEN_SEMICOLON:
        return ";";
    case TOKEN_COLON:
        return ":";
    case TOKEN_COMMA:
        return ",";
    case TOKEN_IDENT:
        return "ident";
    case TOKEN_VAR:
        return "var";
    case TOKEN_RETURN:
        return "return";
    case TOKEN_IF:
        return "if";
    case TOKEN_ELSE:
        return "else";
    case TOKEN_FUN:
        return "fun";
    case TOKEN_FOR:
        return "for";
    case TOKEN_WHILE:
        return "while";
    case TOKEN_AS:
        return "as";
    case TOKEN_STRUCT:
        return "struct";
    case TOKEN_EOF:
        return "eof";
    default:
        return "`Unknown`";
    }
}

Token Lexer::peek(unsigned int offset) {
    if (offset < peeked.size())
        return peeked[offset];

    for (int i = 0; i <= offset; i++) {
        peeked.push_back(next());
    }

    Token token = peeked.back();
    return token;
}

Token Lexer::read() {
    if (peeked.size() != 0) {
        Token token = peeked.front();
        peeked.pop_front();
        return token;
    }

    return next();
}

Token Lexer::curr() { return peek(0); }

class Compiler {
  public:
    LLVMContext context;
    IRBuilder<> builder;
    std::unique_ptr<Module> module;

    Compiler()
        : builder(context), module{std::make_unique<Module>("code", context)} {}
    void declare_function(std::string name, std::vector<Type *> arg_types,
                          Type *ret_type);
    Type *get_primitive_type(std::string type_name);
    void generate_obj_file(std::string name);
};

void Compiler::declare_function(std::string name, std::vector<Type *> arg_types,
                                Type *ret_type) {
    FunctionType *ft = FunctionType::get(ret_type, arg_types, false);
    Function *f =
        Function::Create(ft, Function::ExternalLinkage, name, module.get());
}

Type *Compiler::get_primitive_type(std::string name) {
    if (name == "i64") {
        return Type::getInt64Ty(context);
    } else if (name == "i32") {
        return Type::getInt32Ty(context);
    } else if (name == "i16") {
        return Type::getInt16Ty(context);
    } else if (name == "i8") {
        return Type::getInt8Ty(context);
    } else if (name == "bool") {
        return Type::getInt1Ty(context);
    } else if (name == "string") {
        return Type::getInt8PtrTy(context);
    } else {
        throw "Error: unknown type name";
    }
}

void Compiler::generate_obj_file(std::string name) {
    auto target_triple = sys::getDefaultTargetTriple();
    std::string error_str;
    auto target = TargetRegistry::lookupTarget(target_triple, error_str);
    if (!target) {
        errs() << error_str;
        exit(1);
    }

    auto cpu = "generic";
    auto features = "";

    auto rm = Optional<Reloc::Model>();
    TargetOptions opt; //  = InitTargetOptionsFromCodeGenFlags();
    TargetMachine *target_machine =
        target->createTargetMachine(target_triple, cpu, features, opt, rm);
    module->setDataLayout(target_machine->createDataLayout());

    std::error_code ec;
    raw_fd_ostream dest(name, ec, sys::fs::OF_None);

    if (ec) {
        errs() << "Could not open file: " << ec.message();
        exit(1);
    }

    legacy::PassManager pass;

    auto file_type = TargetMachine::CGFT_ObjectFile;
    if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type)) {
        errs() << "target_machine can't emit a file of this type";
        exit(1);
    }

    pass.run(*module);
    dest.flush();
}

template <class P, class Q> class Env {
  private:
    std::optional<Env<P, Q> *> parent;
    std::map<P, Q> env;

  public:
    Env(Env<P, Q> &parent) : parent{std::make_optional(new Env)} {
        *(this->parent.value()) = parent;
    }
    Env() : parent(std::nullopt) {}
    ~Env() = default;
    std::optional<Q> get(P p) {
        if (env.find(p) != env.end()) {
            return std::make_optional<Q>(env[p]);
        } else {
            if (parent) {
                return parent.value()->get(p);
            } else {
                return std::nullopt;
            }
        }
    }

    void set(P p, Q q) { env[p] = q; }
};

class LangType;

class LangType {
  public:
    virtual ~LangType() = default;
    virtual Type *llvm_type(Compiler &c) = 0;
    virtual std::string to_str() = 0;
};

class IntLangType : public LangType {
  private:
    int bits;

  public:
    IntLangType(int bits) : bits(bits) {}
    Type *llvm_type(Compiler &c) override;
    std::string to_str() override;
};

Type *IntLangType::llvm_type(Compiler &c) {
    return Type::getIntNTy(c.context, bits);
}

std::string IntLangType::to_str() {
    std::stringstream stream;
    stream << "i" << bits;
    return stream.str();
}

class StrLangType : public LangType {
  public:
    StrLangType() {}
    Type *llvm_type(Compiler &c) override;
    std::string to_str() override;
};

Type *StrLangType::llvm_type(Compiler &c) {
    return Type::getInt8PtrTy(c.context);
}

std::string StrLangType::to_str() { return "str"; }

class PtrLangType : public LangType {
  private:
    std::unique_ptr<LangType> ptr_to;

  public:
    PtrLangType(std::unique_ptr<LangType> ptr_to) : ptr_to(std::move(ptr_to)) {}
    Type *llvm_type(Compiler &c) override;
    std::string to_str() override;
};

Type *PtrLangType::llvm_type(Compiler &c) {
    return PointerType::get(ptr_to->llvm_type(c), 0);
}

std::string PtrLangType::to_str() {
    std::stringstream stream;
    stream << ptr_to->to_str() << "*";
    return stream.str();
}

class VoidLangType : public LangType {
  public:
    VoidLangType() {}
    Type *llvm_type(Compiler &c) override;
    std::string to_str() override;
};

Type *VoidLangType::llvm_type(Compiler &c) {
    return Type::getVoidTy(c.context);
}

std::string VoidLangType::to_str() {
    return "void";
}

class UserDefinedLangType : public LangType {
  private:
    std::string name;

  public:
    UserDefinedLangType(std::string name) : name(name) {}
    Type *llvm_type(Compiler &c) override;
    std::string to_str() override;
};

Type *UserDefinedLangType::llvm_type(Compiler &c) {
    return c.module->getTypeByName(name);
}

std::string UserDefinedLangType::to_str() { return name; }

using TypeEnv = Env<std::string, std::vector<std::string>>;

using Scope = Env<std::string, Value *>;

class Expr {
  public:
    virtual ~Expr() = default;

    virtual Value *codegen(Compiler &c, Scope &s, TypeEnv &type_env) = 0;
    virtual Value *lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) = 0;
    virtual std::string to_str() = 0;
};

class IntegerExpr : public Expr {
  private:
    int number;

  public:
    IntegerExpr(int number) : number(number){};
    Value *codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    Value *lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

Value *IntegerExpr::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    return ConstantInt::get(c.context, APInt(32, number, true));
}

Value *IntegerExpr::lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    return codegen(c, s, type_env);
}

std::string IntegerExpr::to_str() {
    std::stringstream stream;
    stream << number;
    return stream.str();
}

class StringExpr : public Expr {
  private:
    std::string str_val;

  public:
    StringExpr(std::string str_val) : str_val{str_val} {}
    Value *codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    Value *lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

Value *StringExpr::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    return c.builder.CreateGlobalString(str_val);
}

Value *StringExpr::lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    return codegen(c, s, type_env);
}

std::string StringExpr::to_str() {
    std::stringstream stream;
    stream << "\"" << str_val << "\"";
    return stream.str();
}

class CharExpr : public Expr {
  private:
    char ch;
  public:
    CharExpr(char ch) : ch(ch) {}
    Value *codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    Value *lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

Value *CharExpr::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    return ConstantInt::get(c.context, APInt(8, (int)ch, true));
}

Value *CharExpr::lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    return codegen(c, s, type_env);
}

std::string CharExpr::to_str() {
    std::stringstream stream;
    stream << "'" << ch << "'";
    return stream.str();
}

class VarExpr : public Expr {
  private:
    std::string name;
    std::unique_ptr<LangType> ty;

  public:
    VarExpr(std::string name) : name(name) {}
    Value *codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    Value *lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

Value *VarExpr::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    auto val_opt = s.get(name);
    Value *val;
    if (val_opt) {
        val = val_opt.value();
    } else {
        throw "Error: undeclared variable";
    }

    return c.builder.CreateLoad(val);
}

Value *VarExpr::lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    auto val_opt = s.get(name);
    Value *val;
    if (val_opt) {
        val = val_opt.value();
    } else {
        throw "Error: undeclared variable";
    }

    return val;
}

std::string VarExpr::to_str() { return name; }

class BinaryExpr : public Expr {
  private:
    std::unique_ptr<Expr> lhs;
    std::unique_ptr<Expr> rhs;
    std::string op;

  public:
    BinaryExpr(std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs,
               std::string op)
        : lhs{std::move(lhs)}, rhs{std::move(rhs)}, op(op){};
    Value *codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    Value *lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

Value *BinaryExpr::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    Value *lval;
    bool is_assign_op = false;
    if (op == "=" || op == "+=" || op == "-=" || op == "*=" || op == "/=") {
        lval = lhs->lvalue_codegen(c, s, type_env);
        is_assign_op = true;
    } else {
        lval = lhs->codegen(c, s, type_env);
    }
    Value *rval = rhs->codegen(c, s, type_env);

    if (!is_assign_op && (lval->getType()->isPointerTy() || rval->getType()->isPointerTy())) {
        if (lval->getType()->isPointerTy() && rval->getType()->isPointerTy()) {
            std::stringstream stream;
            stream << "Error: invalid operation: `Pointer` " << op << " `Pointer` is not allowed";
            throw stream.str();
        }

        if (rval->getType()->isPointerTy()) {
            std::swap(lval, rval);
        }

        if (op == "+") {
            return c.builder.CreateGEP(lval->getType()->getPointerElementType(), lval, rval);
        } else if (op == "-") {
            Value *neg_rval = c.builder.CreateNeg(rval);
            return c.builder.CreateGEP(lval->getType()->getPointerElementType(), lval, neg_rval);
        } else {
            std::stringstream stream;
            stream << "Error: invalid operation: `" << op << "` operator is not allowed in pointer operation";
            throw stream.str();
        }
    } else if (op == "+") {
        return c.builder.CreateAdd(lval, rval, "addtmp");
    } else if (op == "-") {
        return c.builder.CreateSub(lval, rval, "subtmp");
    } else if (op == "*") {
        return c.builder.CreateMul(lval, rval, "multmp");
    } else if (op == "/") {
        return c.builder.CreateSDiv(lval, rval, "divtmp");
    } else if (op == "==") {
        Value *val = c.builder.CreateICmpEQ(lval, rval, "eqtmp");
        return c.builder.CreateZExt(val, Type::getInt32Ty(c.context));
    } else if (op == "!=") {
        Value *val = c.builder.CreateICmpNE(lval, rval, "noteqtmp");
        return c.builder.CreateZExt(val, Type::getInt32Ty(c.context));
    } else if (op == ">") {
        Value *val = c.builder.CreateICmpSGT(lval, rval);
        return c.builder.CreateZExt(val, Type::getInt32Ty(c.context), "gttmp");
    } else if (op == ">=") {
        Value *val = c.builder.CreateICmpSGE(lval, rval);
        return c.builder.CreateZExt(val, Type::getInt32Ty(c.context),
                                    "gteqtmp");
    } else if (op == "<") {
        Value *val = c.builder.CreateICmpSLT(lval, rval);
        return c.builder.CreateZExt(val, Type::getInt32Ty(c.context), "lttmp");
    } else if (op == "<=") {
        Value *val = c.builder.CreateICmpSLE(lval, rval);
        return c.builder.CreateZExt(val, Type::getInt32Ty(c.context),
                                    "lteqtmp");
    } else if (op == "&&" || op == "&") {
        return c.builder.CreateAnd(lval, rval, "andtmp");
    } else if (op == "||" || op == "|") {
        return c.builder.CreateOr(lval, rval, "ortmp");
    } else if (op == "^") {
        return c.builder.CreateXor(lval, rval, "xortmp");
    } else if (op == "=") {
        c.builder.CreateStore(rval, lval, "assigntmp");
        return c.builder.CreateLoad(lval);
    } else if (op == "+=") {
        Value *tmp =
            c.builder.CreateAdd(c.builder.CreateLoad(lval), rval, "addtmp");
        c.builder.CreateStore(tmp, lval);
        return c.builder.CreateLoad(lval);
    } else if (op == "-=") {
        Value *tmp =
            c.builder.CreateSub(c.builder.CreateLoad(lval), rval, "subtmp");
        c.builder.CreateStore(tmp, lval);
        return c.builder.CreateLoad(lval);
    } else if (op == "*=") {
        Value *tmp =
            c.builder.CreateMul(c.builder.CreateLoad(lval), rval, "multmp");
        c.builder.CreateStore(tmp, lval);
        return c.builder.CreateLoad(lval);
    } else if (op == "/=") {
        Value *tmp =
            c.builder.CreateSDiv(c.builder.CreateLoad(lval), rval, "divtmp");
        c.builder.CreateStore(tmp, lval);
        return c.builder.CreateLoad(lval);
    } else {
        std::stringstream stream;
        stream << "Error: unknown operator: " << op << std::endl;
        throw stream.str();
    }
}

Value *BinaryExpr::lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    return codegen(c, s, type_env);
}

std::string BinaryExpr::to_str() {
    std::stringstream stream;
    stream << "(" << lhs->to_str() << op << rhs->to_str() << ")";
    return stream.str();
}

class PrefixExpr : public Expr {
  private:
    std::unique_ptr<Expr> expr;
    std::string op;

  public:
    PrefixExpr(std::unique_ptr<Expr> expr, std::string op)
        : expr{std::move(expr)}, op(op) {}
    Value *codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    Value *lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

Value *PrefixExpr::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    if (op == "++") {
        Value *val = expr->lvalue_codegen(c, s, type_env);
        Value *tmp = c.builder.CreateAdd(
            c.builder.CreateLoad(val),
            ConstantInt::get(c.context, APInt(32, 1, true)));
        c.builder.CreateStore(tmp, val);
        return c.builder.CreateLoad(val);
    } else if (op == "--") {
        Value *val = expr->lvalue_codegen(c, s, type_env);
        Value *tmp = c.builder.CreateSub(
            c.builder.CreateLoad(val),
            ConstantInt::get(c.context, APInt(32, 1, true)));
        c.builder.CreateStore(tmp, val);
        return c.builder.CreateLoad(val);
    } else if (op == "-") {
        Value *val = expr->codegen(c, s, type_env);
        return c.builder.CreateNeg(val);
    } else {
        std::stringstream stream;
        stream << "Error: unknown operator: " << op << std::endl;
        throw stream.str();
    }
}

Value *PrefixExpr::lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    return codegen(c, s, type_env);
}

std::string PrefixExpr::to_str() {
    std::stringstream stream;
    stream << op << expr->to_str();
    return stream.str();
}

class PostfixExpr : public Expr {
  private:
    std::unique_ptr<Expr> expr;
    std::string op;

  public:
    PostfixExpr(std::unique_ptr<Expr> expr, std::string op)
        : expr{std::move(expr)}, op(op) {}
    Value *codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    Value *lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

Value *PostfixExpr::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    if (op == "++") {
        Value *val = expr->lvalue_codegen(c, s, type_env);
        Value *tmp = c.builder.CreateLoad(val);
        Value *result = c.builder.CreateAdd(
            tmp, ConstantInt::get(c.context, APInt(32, 1, true)));
        c.builder.CreateStore(result, val);
        return tmp;
    } else if (op == "--") {
        Value *val = expr->lvalue_codegen(c, s, type_env);
        Value *tmp = c.builder.CreateLoad(val);
        Value *result = c.builder.CreateSub(
            tmp, ConstantInt::get(c.context, APInt(32, 1, true)));
        c.builder.CreateStore(result, val);
        return tmp;
    } else if (op == ".*") {
        Value *val = expr->codegen(c, s, type_env);
        if (!val->getType()->isPointerTy()) {
            throw "Error: can't dereference except pointers.";
        }

        return c.builder.CreateLoad(val);
    } else {
        std::stringstream stream;
        stream << "Error: unknown operator: " << op << std::endl;
        throw stream.str();
    }
}

Value *PostfixExpr::lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    if (op == ".*") {
        Value *val = expr->codegen(c, s, type_env);
        return val;
    }
    return codegen(c, s, type_env);
}

std::string PostfixExpr::to_str() {
    std::stringstream stream;
    stream << expr->to_str() << op;
    return stream.str();
}

class MemberAccessExpr : public Expr {
  private:
    std::unique_ptr<Expr> source;
    std::string name;

  public:
    MemberAccessExpr(std::unique_ptr<Expr> source, std::string name)
        : source{std::move(source)}, name(name) {}
    Value *codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    Value *lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

Value *MemberAccessExpr::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    Value *val = source->lvalue_codegen(c, s, type_env);
    Type *source_type = val->getType();
    if (!source_type->isPointerTy()) {
        std::stringstream stream;
        stream << "Error: access to raw struct is unimplemented";
        throw stream.str();
    }

    std::string source_type_name =
        source_type->getPointerElementType()->getStructName();
    std::optional<std::vector<std::string>> member_names_opt =
        type_env.get(source_type_name);
    if (!member_names_opt.has_value()) {
        std::stringstream stream;
        stream << "Error: undefined user-defined type: " << source_type_name;
        throw stream.str();
    }

    std::vector<std::string> member_names = member_names_opt.value();
    int idx = -1;
    for (int i = 0; i < member_names.size(); i++) {
        if (name == member_names[i]) {
            idx = i;
            break;
        }
    }

    if (idx == -1) {
        std::stringstream stream;
        stream << "Error: undefined member name: " << name;
        throw stream.str();
    }

    std::vector<Value *> idxes;
    idxes.push_back(ConstantInt::get(c.context, APInt(32, 0, true)));
    idxes.push_back(ConstantInt::get(c.context, APInt(32, idx, true)));

    Value *ptr_to_member = c.builder.CreateInBoundsGEP(
        source_type->getPointerElementType(), val, idxes);
    Value *ptr =
        c.builder.CreateCast(Instruction::CastOps::BitCast, ptr_to_member,
                             source_type->getPointerElementType()
                                 ->getStructElementType(idx)
                                 ->getPointerTo());
    return c.builder.CreateLoad(
        source_type->getPointerElementType()->getStructElementType(idx), ptr);
}

Value *MemberAccessExpr::lvalue_codegen(Compiler &c, Scope &s,
                                        TypeEnv &type_env) {
    Value *val = source->lvalue_codegen(c, s, type_env);
    Type *source_type = val->getType();
    if (!source_type->isPointerTy()) {
        std::stringstream stream;
        stream << "Error: access to raw struct is unimplemented";
        throw stream.str();
    }

    std::string source_type_name =
        source_type->getPointerElementType()->getStructName();
    std::optional<std::vector<std::string>> member_names_opt =
        type_env.get(source_type_name);
    if (!member_names_opt.has_value()) {
        std::stringstream stream;
        stream << "Error: undefined user-defined type: " << source_type_name;
        throw stream.str();
    }

    std::vector<std::string> member_names = member_names_opt.value();
    int idx = -1;
    for (int i = 0; i < member_names.size(); i++) {
        if (name == member_names[i]) {
            idx = i;
            break;
        }
    }

    if (idx == -1) {
        std::stringstream stream;
        stream << "Error: undefined member name: " << name;
        throw stream.str();
    }

    std::vector<Value *> idxes;
    idxes.push_back(ConstantInt::get(c.context, APInt(32, 0, true)));
    idxes.push_back(ConstantInt::get(c.context, APInt(32, idx, true)));

    Value *ptr_to_member = c.builder.CreateInBoundsGEP(
        source_type->getPointerElementType(), val, idxes);
    Value *ptr =
        c.builder.CreateCast(Instruction::CastOps::BitCast, ptr_to_member,
                             source_type->getPointerElementType()
                                 ->getStructElementType(idx)
                                 ->getPointerTo());
    return ptr;
}

std::string MemberAccessExpr::to_str() {
    std::stringstream stream;
    stream << "(" << source->to_str() << "." << name << ")";
    return stream.str();
}

class CastExpr : public Expr {
  private:
    std::unique_ptr<Expr> expr;
    std::unique_ptr<LangType> ty;

  public:
    CastExpr(std::unique_ptr<Expr> expr, std::unique_ptr<LangType> ty)
        : expr{std::move(expr)}, ty{std::move(ty)} {}
    Value *codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    Value *lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

Value *CastExpr::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    Value *val = expr->codegen(c, s, type_env);
    Type *cast_to = ty->llvm_type(c);
    if (val->getType()->isIntegerTy() && cast_to->isIntegerTy()) {
        return c.builder.CreateCast(Instruction::CastOps::SExt, val, cast_to);
    } else if (val->getType()->isPointerTy() && cast_to->isPointerTy()) {
        return c.builder.CreateCast(Instruction::CastOps::BitCast, val,
                                    cast_to);
    } else {
        throw "unimplemented!";
    }
}

Value *CastExpr::lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    return codegen(c, s, type_env);
}

std::string CastExpr::to_str() {
    std::stringstream stream;
    stream << expr->to_str() << " as " << ty->to_str();
    return stream.str();
}

class CallExpr : public Expr {
  private:
    std::string name;
    std::vector<std::unique_ptr<Expr>> args;

  public:
    CallExpr(std::string name, std::vector<std::unique_ptr<Expr>> args)
        : name(name), args{std::move(args)} {}
    Value *codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    Value *lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

Value *CallExpr::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    Function *callee = c.module->getFunction(name);
    if (callee == nullptr) {
        std::stringstream stream;
        stream << "Error: undeclared function calling: " << name;
        throw stream.str();
    }
    std::vector<Value *> arg_values;
    for (int i = 0; i < args.size(); i++) {
        arg_values.push_back(args[i]->codegen(c, s, type_env));
    }
    return c.builder.CreateCall(callee, arg_values, "calltmp");
}

Value *CallExpr::lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    return codegen(c, s, type_env);
}

std::string CallExpr::to_str() {
    std::stringstream stream;
    stream << "(" << name << " (";
    for (int i = 0; i < args.size(); i++) {
        stream << args[i]->to_str();
        if (i != args.size() - 1) {
            stream << ", ";
        }
    }
    stream << ")";

    return stream.str();
}

class BuiltinCallExpr : public Expr {
  public:
    virtual Value *codegen(Compiler &c, Scope &s, TypeEnv &type_env) = 0;
    virtual Value *lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) = 0;
    virtual std::string to_str() = 0;
};

class BuiltinSizeof : public BuiltinCallExpr {
  private:
    std::unique_ptr<LangType> type;
  public:
    BuiltinSizeof(std::unique_ptr<LangType> type) : type{std::move(type)} {}
    Value *codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    Value *lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

Value *BuiltinSizeof::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    Type *lltype = type->llvm_type(c);
    Value *const_one = ConstantInt::get(c.context, APInt(32, 1, true));
    Value *size_ptr = c.builder.CreateGEP(lltype, ConstantPointerNull::get(lltype->getPointerTo()), const_one);
    Value *size = c.builder.CreateCast(Instruction::CastOps::PtrToInt, size_ptr, Type::getInt32Ty(c.context));
    return size;
}

Value *BuiltinSizeof::lvalue_codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    return codegen(c, s, type_env);
}

std::string BuiltinSizeof::to_str() {
    std::stringstream stream;
    stream << "@sizeof(" << type->to_str() << ")";
    throw stream.str();
}

class Stmt {
  public:
    virtual ~Stmt() = default;

    virtual void codegen(Compiler &c, Scope &s, TypeEnv &type_env) = 0;
    virtual std::string to_str() = 0;
};

class VarStmt : public Stmt {
  private:
    std::string name;
    std::unique_ptr<LangType> type;
    std::optional<std::unique_ptr<Expr>> expr;

  public:
    VarStmt(std::string name, std::unique_ptr<LangType> type,
            std::unique_ptr<Expr> expr)
        : name(name), type{std::move(type)}, expr{std::make_optional(
                                                 std::move(expr))} {}
    VarStmt(std::string name, std::unique_ptr<LangType> type)
        : name(name), type{std::move(type)}, expr{std::nullopt} {}
    void codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

void VarStmt::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    Value *ptr;
    ptr = c.builder.CreateAlloca(type->llvm_type(c));
    s.set(name, ptr);
    if (expr) {
        Value *val = expr.value()->codegen(c, s, type_env);
        c.builder.CreateStore(val, ptr);
    }
}

std::string VarStmt::to_str() {
    std::stringstream stream;
    stream << "(var " << name;
    if (expr) {
        stream << " = " << expr.value()->to_str() << ")";
    } else {
        stream << ")";
    }

    return stream.str();
}

class ExprStmt : public Stmt {
  private:
    std::unique_ptr<Expr> expr;

  public:
    ExprStmt(std::unique_ptr<Expr> expr) : expr{std::move(expr)} {}
    void codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

void ExprStmt::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    expr->codegen(c, s, type_env);
}

std::string ExprStmt::to_str() { return expr->to_str(); }

class ReturnStmt : public Stmt {
  private:
      std::optional<std::unique_ptr<Expr>> result;

  public:
    ReturnStmt() : result{std::nullopt} {}
    ReturnStmt(std::unique_ptr<Expr> result) : result{std::move(result)} {}
    void codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

void ReturnStmt::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    if (result) {
        c.builder.CreateRet(result.value()->codegen(c, s, type_env));
    } else {
        c.builder.CreateRetVoid();
    }
}

std::string ReturnStmt::to_str() {
    std::stringstream stream;
    if (result) {
        stream << "(return " << result.value()->to_str() << ")";
    } else {
        stream << "(return_void)";
    }
    return stream.str();
}

class IfStmt : public Stmt {
  private:
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Stmt> then_body;
    std::optional<std::unique_ptr<Stmt>> else_body;

  public:
    IfStmt(std::unique_ptr<Expr> condition, std::unique_ptr<Stmt> then_body,
           std::optional<std::unique_ptr<Stmt>> else_body)
        : condition{std::move(condition)}, then_body{std::move(then_body)},
          else_body{std::move(else_body)} {}
    void codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

void IfStmt::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    Value *condition_val = condition->codegen(c, s, type_env);
    condition_val = c.builder.CreateICmpNE(
        condition_val, ConstantInt::get(c.context, APInt(32, 0)), "ifcond");
    Function *func = c.builder.GetInsertBlock()->getParent();
    BasicBlock *then_bb = BasicBlock::Create(c.context, "then", func);
    std::optional<BasicBlock *> else_bb = std::nullopt;
    if (else_body) {
        else_bb = std::make_optional(BasicBlock::Create(c.context, "else"));
    }
    BasicBlock *merge_bb = BasicBlock::Create(c.context, "ifcont");
    if (else_bb) {
        c.builder.CreateCondBr(condition_val, then_bb, else_bb.value());
    } else {
        c.builder.CreateCondBr(condition_val, then_bb, merge_bb);
    }

    // then
    c.builder.SetInsertPoint(then_bb);
    Scope then_s(s);
    then_body->codegen(c, then_s, type_env);
    c.builder.CreateBr(merge_bb);
    then_bb = c.builder.GetInsertBlock();

    // else
    if (else_bb) {
        func->getBasicBlockList().push_back(else_bb.value());
        c.builder.SetInsertPoint(else_bb.value());
        Scope else_s(s);
        else_body.value()->codegen(c, else_s, type_env);
        c.builder.CreateBr(merge_bb);
        else_bb = c.builder.GetInsertBlock();
    }

    func->getBasicBlockList().push_back(merge_bb);
    c.builder.SetInsertPoint(merge_bb);
}

std::string IfStmt::to_str() {
    std::stringstream stream;
    stream << "(if " << condition->to_str() << " then " << then_body->to_str();
    if (else_body) {
        stream << " else " << else_body.value()->to_str();
    }

    stream << ")";
    return stream.str();
}

class CompoundStmt : public Stmt {
  private:
    std::vector<std::unique_ptr<Stmt>> stmts;

  public:
    CompoundStmt(std::vector<std::unique_ptr<Stmt>> stmts)
        : stmts{std::move(stmts)} {}
    void codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

void CompoundStmt::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    Scope compound_scope(s);
    for (int i = 0; i < stmts.size(); i++) {
        stmts[i]->codegen(c, compound_scope, type_env);
    }
}

std::string CompoundStmt::to_str() {
    std::stringstream stream;
    bool empty = stmts.size() == 0;
    stream << "(";
    for (int i = 0; i < stmts.size(); i++) {
        stream << "(";
        stream << stmts[i]->to_str();
        stream << ")";

        if (i != stmts.size() - 1) {
            stream << ", ";
        }
    }
    stream << ")";
    return stream.str();
}

class FunctionStmt : public Stmt {
  private:
    std::string name;
    std::vector<std::pair<std::string, std::unique_ptr<LangType>>> args;
    std::unique_ptr<LangType> ret_type;
    std::optional<std::unique_ptr<Stmt>> body;

  public:
    FunctionStmt(
        std::string name,
        std::vector<std::pair<std::string, std::unique_ptr<LangType>>> args,
        std::unique_ptr<LangType> ret_type, std::unique_ptr<Stmt> body)
        : name(name), args{std::move(args)}, ret_type{std::move(ret_type)},
          body{std::make_optional(std::move(body))} {}
    FunctionStmt(
        std::string name,
        std::vector<std::pair<std::string, std::unique_ptr<LangType>>> args,
        std::unique_ptr<LangType> ret_type)
        : name(name), args{std::move(args)}, ret_type{std::move(ret_type)},
          body{std::nullopt} {}
    void codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

void FunctionStmt::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    std::vector<Type *> arg_types;
    for (int i = 0; i < args.size(); i++) {
        arg_types.push_back(args[i].second->llvm_type(c));
    }

    c.declare_function(name, arg_types, ret_type->llvm_type(c));
    if (body) {
        Function *f = c.module->getFunction(name);
        int idx = 0;
        BasicBlock *bb = BasicBlock::Create(c.context, "entry", f);
        c.builder.SetInsertPoint(bb);
        Scope function_scope(s);
        for (auto &arg : f->args()) {
            Value *arg_ptr = c.builder.CreateAlloca(arg.getType());
            c.builder.CreateStore(&arg, arg_ptr);
            function_scope.set(args[idx++].first, arg_ptr);
        }
        body.value()->codegen(c, function_scope, type_env);
        verifyFunction(*f);
    }
}

std::string FunctionStmt::to_str() {
    std::stringstream stream;
    stream << "(fun " << name << " (";
    for (int i = 0; i < args.size(); i++) {
        stream << args[i].first;
        if (i != args.size() - 1) {
            stream << ", ";
        }
    }

    stream << ")";
    if (body) {
        stream << " " << body.value()->to_str();
    }
    stream << ")";
    return stream.str();
}

class StructStmt : public Stmt {
  private:
    std::string name;
    std::vector<std::unique_ptr<LangType>> elements;
    std::vector<std::string> member_names;

  public:
    StructStmt(std::string name,
               std::vector<std::unique_ptr<LangType>> elements,
               std::vector<std::string> member_names)
        : name(name), elements{std::move(elements)}, member_names{
                                                         member_names} {}
    void codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

void StructStmt::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    std::vector<Type *> types;
    for (int i = 0; i < elements.size(); i++) {
        types.push_back(elements[i]->llvm_type(c));
    }

    StructType::create(c.context, types, name);
    type_env.set(name, member_names);
}

std::string StructStmt::to_str() {
    std::stringstream stream;
    stream << "(struct " << name << " (";
    for (int i = 0; i < elements.size(); i++) {
        stream << elements.at(i)->to_str();
        if (i != elements.size() - 1) {
            stream << " ";
        }
    }

    stream << "))";
    return stream.str();
}

class ForStmt : public Stmt {
  private:
    std::unique_ptr<Stmt> init;
    std::unique_ptr<Expr> cond;
    std::unique_ptr<Expr> loop;
    std::unique_ptr<Stmt> body;

  public:
    ForStmt(std::unique_ptr<Stmt> init, std::unique_ptr<Expr> cond,
            std::unique_ptr<Expr> loop, std::unique_ptr<Stmt> body)
        : init{std::move(init)}, cond{std::move(cond)}, loop{std::move(loop)},
          body{std::move(body)} {}
    void codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

void ForStmt::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    Scope for_scope(s);
    init->codegen(c, for_scope, type_env);
    Function *f = c.builder.GetInsertBlock()->getParent();

    BasicBlock *cond_bb = BasicBlock::Create(c.context, "for_cond", f);
    BasicBlock *body_bb = BasicBlock::Create(c.context, "for_body", f);
    BasicBlock *loop_bb = BasicBlock::Create(c.context, "for_loop", f);
    BasicBlock *merge_bb = BasicBlock::Create(c.context, "for_end", f);

    c.builder.CreateBr(cond_bb);

    c.builder.SetInsertPoint(cond_bb);
    Value *val = cond->codegen(c, for_scope, type_env);
    c.builder.CreateCondBr(val, body_bb, merge_bb);

    c.builder.SetInsertPoint(body_bb);
    body->codegen(c, for_scope, type_env);
    c.builder.CreateBr(loop_bb);

    c.builder.SetInsertPoint(loop_bb);
    loop->codegen(c, for_scope, type_env);
    c.builder.CreateBr(cond_bb);

    c.builder.SetInsertPoint(merge_bb);
}

std::string ForStmt::to_str() {
    std::stringstream stream;
    stream << "(for ";
    stream << init->to_str() << ", ";
    stream << cond->to_str() << ", ";
    stream << loop->to_str() << " ";

    stream << body->to_str();
    stream << ")";
    return stream.str();
}

class WhileStmt : public Stmt {
  private:
    std::unique_ptr<Expr> cond;
    std::unique_ptr<Stmt> body;

  public:
    WhileStmt(std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> body)
        : cond{std::move(cond)}, body{std::move(body)} {}
    void codegen(Compiler &c, Scope &s, TypeEnv &type_env) override;
    std::string to_str() override;
};

void WhileStmt::codegen(Compiler &c, Scope &s, TypeEnv &type_env) {
    Scope while_scope(s);
    Function *f = c.builder.GetInsertBlock()->getParent();
    BasicBlock *cond_bb = BasicBlock::Create(c.context, "while_cond", f);
    BasicBlock *body_bb = BasicBlock::Create(c.context, "while_body", f);
    BasicBlock *merge_bb = BasicBlock::Create(c.context, "while_end", f);

    c.builder.CreateBr(cond_bb);

    c.builder.SetInsertPoint(cond_bb);
    Value *val = cond->codegen(c, while_scope, type_env);
    c.builder.CreateCondBr(val, body_bb, merge_bb);

    c.builder.SetInsertPoint(body_bb);
    body->codegen(c, while_scope, type_env);
    c.builder.CreateBr(cond_bb);

    c.builder.SetInsertPoint(merge_bb);
}

std::string WhileStmt::to_str() {
    std::stringstream stream;
    stream << "(while " << cond->to_str() << " " << body->to_str() << ")";
    return stream.str();
}

class Parser {
  private:
    Lexer l;
    TypeEnv type_env;

    bool match(TokenType type);
    Token expect(TokenType type);

  public:
    Parser(Lexer l) : l(l) {}

    std::unique_ptr<LangType> get_type_from_string(std::string ty_name);
    std::unique_ptr<LangType> type_annotation();

    std::unique_ptr<Expr> builtin_call();
    std::unique_ptr<Expr> primary_expr();
    std::unique_ptr<Expr> postfix_expr();
    std::unique_ptr<Expr> prefix_expr();
    std::unique_ptr<Expr> cast_expr();
    std::unique_ptr<Expr> mul_expr();
    std::unique_ptr<Expr> add_expr();
    std::unique_ptr<Expr> rel_expr();
    std::unique_ptr<Expr> equal_expr();
    std::unique_ptr<Expr> bitwise_and_expr();
    std::unique_ptr<Expr> bitwise_xor_expr();
    std::unique_ptr<Expr> bitwise_or_expr();
    std::unique_ptr<Expr> logical_and_expr();
    std::unique_ptr<Expr> logical_or_expr();
    std::unique_ptr<Expr> assign_expr();
    std::unique_ptr<Expr> main_expr();

    std::unique_ptr<Stmt> var_stmt();
    std::unique_ptr<Stmt> expr_stmt();
    std::unique_ptr<Stmt> return_stmt();
    std::unique_ptr<Stmt> if_stmt();
    std::unique_ptr<Stmt> for_stmt();
    std::unique_ptr<Stmt> while_stmt();
    std::unique_ptr<Stmt> compound_stmt();
    std::unique_ptr<Stmt> function_stmt();
    std::unique_ptr<Stmt> struct_stmt();
    std::unique_ptr<Stmt> stmt();
    std::unique_ptr<Stmt> toplevel_stmt();
    std::vector<std::unique_ptr<Stmt>> toplevel_stmt_list();

    std::vector<std::pair<std::string, std::unique_ptr<LangType>>> arguments();
};

bool Parser::match(TokenType type) { return l.curr().get_token_type() == type; }

Token Parser::expect(TokenType type) {
    if (l.curr().get_token_type() != type) {
        std::stringstream stream;
        stream << "Error: expected " << l.type_to_str(type) << " but got "
               << l.type_to_str(l.curr().get_token_type()) << std::endl;
        throw stream.str();
    }

    return l.read();
}

std::unique_ptr<LangType> Parser::get_type_from_string(std::string ty_name) {
    if (ty_name == "i8") {
        return std::make_unique<IntLangType>(8);
    } else if (ty_name == "i16") {
        return std::make_unique<IntLangType>(16);
    } else if (ty_name == "i32") {
        return std::make_unique<IntLangType>(32);
    } else if (ty_name == "i64") {
        return std::make_unique<IntLangType>(64);
    } else if (ty_name == "str") {
        return std::make_unique<StrLangType>();
    } else if (ty_name == "char") {
        return std::make_unique<IntLangType>(8);
    } else if (ty_name == "void") {
        return std::make_unique<VoidLangType>();
    } else {
        return std::make_unique<UserDefinedLangType>(ty_name);
    }
}

std::unique_ptr<LangType> Parser::type_annotation() {
    std::string type_a = expect(TOKEN_IDENT).as_str();

    std::unique_ptr<LangType> ty = get_type_from_string(type_a);

    while (match(TOKEN_ASTERISK)) {
        l.read();
        PtrLangType ptr(std::move(ty));
        std::unique_ptr<LangType> uni_ptr =
            std::make_unique<PtrLangType>(std::move(ptr));
        ty = std::move(uni_ptr);
    }

    return ty;
}

std::unique_ptr<Expr> Parser::builtin_call() {
    expect(TOKEN_AT);
    std::string name = expect(TOKEN_IDENT).as_str();
    if (name == "sizeof") {
        expect(TOKEN_LPAREN);
        std::unique_ptr<LangType> type = type_annotation();
        expect(TOKEN_RPAREN);
        return std::make_unique<BuiltinSizeof>(std::move(type));
    } else {
        std::stringstream stream;
        stream << "Error: invalid builtin function name: " << name;
        throw stream.str();
    }
}

std::unique_ptr<Expr> Parser::primary_expr() {
    if (match(TOKEN_INT)) {
        return std::make_unique<IntegerExpr>(
            IntegerExpr(std::stoi(l.read().as_str())));
    } else if (match(TOKEN_LPAREN)) {
        l.read();
        std::unique_ptr<Expr> expr = main_expr();
        expect(TOKEN_RPAREN);
        return expr;
    } else if (match(TOKEN_IDENT)) {
        Token name = l.read();
        if (match(TOKEN_LPAREN)) {
            l.read();
            std::vector<std::unique_ptr<Expr>> args;
            while (!match(TOKEN_RPAREN)) {
                args.push_back(main_expr());
                if (match(TOKEN_COMMA)) {
                    l.read();
                } else if (match(TOKEN_RPAREN)) {
                    break;
                } else {
                    std::stringstream stream;
                    stream << "Error: expected `,` or `)` but got "
                           << l.curr().as_str();
                    throw stream.str();
                }
            }

            expect(TOKEN_RPAREN);

            return std::make_unique<CallExpr>(name.as_str(), std::move(args));
        } else {
            return std::make_unique<VarExpr>(name.as_str());
        }
    } else if (match(TOKEN_STR_LITERAL)) {
        return std::make_unique<StringExpr>(l.read().as_str());
    } else if (match(TOKEN_CHAR_LITERAL)) {
        return std::make_unique<CharExpr>(l.read().as_str().at(0));
    } else if (match(TOKEN_AT)) {
        return builtin_call();
    } else {
        std::stringstream stream;
        stream << "Error: can't parse primary expression: " << l.curr().as_str()
               << std::endl;
        throw stream.str();
    }
}

std::unique_ptr<Expr> Parser::postfix_expr() {
    auto expr = primary_expr();
    for (;;) {
        if (match(TOKEN_PLUS_PLUS) || match(TOKEN_HYPHEN_HYPHEN)) {
            expr = std::make_unique<PostfixExpr>(std::move(expr),
                                                 l.read().as_str());
        } else if (match(TOKEN_DOT)) {
            l.read();
            if (match(TOKEN_IDENT)) {
                std::string name = expect(TOKEN_IDENT).as_str();
                expr =
                    std::make_unique<MemberAccessExpr>(std::move(expr), name);
            } else if (match(TOKEN_ASTERISK)) {
                l.read();
                expr = std::make_unique<PostfixExpr>(std::move(expr), ".*");
            } else {
                std::stringstream stream;
                stream << "Error: expected * or identifier but got "
                       << l.curr().as_str();
                throw stream.str();
            }
        } else {
            break;
        }
    }

    return expr;
}

std::unique_ptr<Expr> Parser::prefix_expr() {
    if (match(TOKEN_PLUS_PLUS) || match(TOKEN_HYPHEN_HYPHEN) ||
        match(TOKEN_HYPHEN)) {
        std::string op = l.read().as_str();
        auto expr = prefix_expr();
        return std::make_unique<PrefixExpr>(std::move(expr), op);
    } else {
        return postfix_expr();
    }
}

std::unique_ptr<Expr> Parser::cast_expr() {
    auto expr = prefix_expr();
    for (;;) {
        if (match(TOKEN_AS)) {
            l.read();
            std::unique_ptr<LangType> ty = type_annotation();
            expr = std::make_unique<CastExpr>(std::move(expr), std::move(ty));
        } else {
            return expr;
        }
    }
}

std::unique_ptr<Expr> Parser::mul_expr() {
    auto lhs = cast_expr();
    for (; match(TOKEN_ASTERISK) || match(TOKEN_SLASH);) {
        Token op = l.read();
        auto rhs = cast_expr();
        lhs = std::make_unique<BinaryExpr>(std::move(lhs), std::move(rhs),
                                           op.as_str());
    }

    return lhs;
}

std::unique_ptr<Expr> Parser::add_expr() {
    auto lhs = mul_expr();
    for (; match(TOKEN_PLUS) || match(TOKEN_HYPHEN);) {
        Token op = l.read();
        auto rhs = mul_expr();
        lhs = std::make_unique<BinaryExpr>(std::move(lhs), std::move(rhs),
                                           op.as_str());
    }

    return lhs;
}

std::unique_ptr<Expr> Parser::rel_expr() {
    auto lhs = add_expr();
    for (; match(TOKEN_GT) || match(TOKEN_GT_EQUAL) || match(TOKEN_LT) ||
           match(TOKEN_LT_EQUAL);) {
        Token op = l.read();
        auto rhs = add_expr();
        lhs = std::make_unique<BinaryExpr>(std::move(lhs), std::move(rhs),
                                           op.as_str());
    }

    return lhs;
}

std::unique_ptr<Expr> Parser::equal_expr() {
    auto lhs = rel_expr();
    for (; match(TOKEN_EQUAL_2) || match(TOKEN_NOT_EQUAL);) {
        Token op = l.read();
        auto rhs = rel_expr();
        lhs = std::make_unique<BinaryExpr>(std::move(lhs), std::move(rhs),
                                           op.as_str());
    }

    return lhs;
}

std::unique_ptr<Expr> Parser::bitwise_and_expr() {
    auto lhs = equal_expr();
    for (; match(TOKEN_AND);) {
        Token op = l.read();
        auto rhs = equal_expr();
        lhs = std::make_unique<BinaryExpr>(std::move(lhs), std::move(rhs),
                                           op.as_str());
    }

    return lhs;
}

std::unique_ptr<Expr> Parser::bitwise_xor_expr() {
    auto lhs = bitwise_and_expr();
    for (; match(TOKEN_OR);) {
        Token op = l.read();
        auto rhs = bitwise_and_expr();
        lhs = std::make_unique<BinaryExpr>(std::move(lhs), std::move(rhs),
                                           op.as_str());
    }

    return lhs;
}

std::unique_ptr<Expr> Parser::bitwise_or_expr() {
    auto lhs = bitwise_xor_expr();
    for (; match(TOKEN_OR);) {
        Token op = l.read();
        auto rhs = bitwise_xor_expr();
        lhs = std::make_unique<BinaryExpr>(std::move(lhs), std::move(rhs),
                                           op.as_str());
    }

    return lhs;
}

std::unique_ptr<Expr> Parser::logical_and_expr() {
    auto lhs = bitwise_or_expr();
    for (; match(TOKEN_AND_2);) {
        Token op = l.read();
        auto rhs = bitwise_or_expr();
        lhs = std::make_unique<BinaryExpr>(std::move(lhs), std::move(rhs),
                                           op.as_str());
    }

    return lhs;
}

std::unique_ptr<Expr> Parser::logical_or_expr() {
    auto lhs = logical_and_expr();
    for (; match(TOKEN_OR_2);) {
        Token op = l.read();
        auto rhs = logical_and_expr();
        lhs = std::make_unique<BinaryExpr>(std::move(lhs), std::move(rhs),
                                           op.as_str());
    }

    return lhs;
}

std::unique_ptr<Expr> Parser::assign_expr() {
    auto lhs = logical_or_expr();
    for (; match(TOKEN_EQUAL) || match(TOKEN_PLUS_EQUAL) ||
           match(TOKEN_HYPHEN_EQUAL) || match(TOKEN_ASTERISK_EQUAL) ||
           match(TOKEN_SLASH_EQUAL);) {
        Token op = l.read();
        auto rhs = logical_or_expr();
        lhs = std::make_unique<BinaryExpr>(std::move(lhs), std::move(rhs),
                                           op.as_str());
    }

    return lhs;
}

std::unique_ptr<Expr> Parser::main_expr() { return assign_expr(); }

std::unique_ptr<Stmt> Parser::var_stmt() {
    expect(TOKEN_VAR);
    Token name = expect(TOKEN_IDENT);
    expect(TOKEN_COLON);
    std::unique_ptr<LangType> type = type_annotation();
    if (match(TOKEN_SEMICOLON)) {
        return std::make_unique<VarStmt>(name.as_str(), std::move(type));
    }
    expect(TOKEN_EQUAL);
    auto expr = main_expr();
    return std::make_unique<VarStmt>(name.as_str(), std::move(type),
                                     std::move(expr));
}

std::unique_ptr<Stmt> Parser::expr_stmt() {
    auto expr = main_expr();
    return std::make_unique<ExprStmt>(std::move(expr));
}

std::unique_ptr<Stmt> Parser::return_stmt() {
    expect(TOKEN_RETURN);
    if (match(TOKEN_SEMICOLON)) {
        return std::make_unique<ReturnStmt>();
    }

    auto expr = main_expr();
    return std::make_unique<ReturnStmt>(std::move(expr));
}

std::unique_ptr<Stmt> Parser::if_stmt() {
    expect(TOKEN_IF);
    auto condition = main_expr();
    auto then_body = stmt();
    if (match(TOKEN_ELSE)) {
        l.read();
        auto else_body = std::make_optional(stmt());

        return std::make_unique<IfStmt>(IfStmt(
            std::move(condition), std::move(then_body), std::move(else_body)));
    } else {
        return std::make_unique<IfStmt>(
            IfStmt(std::move(condition), std::move(then_body), std::nullopt));
    }
}

std::unique_ptr<Stmt> Parser::for_stmt() {
    expect(TOKEN_FOR);
    std::unique_ptr<Stmt> init = var_stmt();
    expect(TOKEN_COMMA);
    std::unique_ptr<Expr> cond = main_expr();
    expect(TOKEN_COMMA);
    std::unique_ptr<Expr> loop = main_expr();

    std::unique_ptr<Stmt> body = compound_stmt();

    return std::make_unique<ForStmt>(std::move(init), std::move(cond),
                                     std::move(loop), std::move(body));
}

std::unique_ptr<Stmt> Parser::while_stmt() {
    expect(TOKEN_WHILE);
    auto cond = main_expr();
    auto body = compound_stmt();

    return std::make_unique<WhileStmt>(std::move(cond), std::move(body));
}

std::unique_ptr<Stmt> Parser::compound_stmt() {
    expect(TOKEN_LBRACE);
    std::vector<std::unique_ptr<Stmt>> stmts;
    while (!match(TOKEN_RBRACE)) {
        auto s = stmt();
        stmts.push_back(std::move(s));
    }
    expect(TOKEN_RBRACE);

    return std::make_unique<CompoundStmt>(std::move(stmts));
}

std::unique_ptr<Stmt> Parser::function_stmt() {
    expect(TOKEN_FUN);
    Token name = expect(TOKEN_IDENT);
    std::vector<std::pair<std::string, std::unique_ptr<LangType>>> args =
        arguments();
    expect(TOKEN_COLON);
    std::unique_ptr<LangType> ret_type = type_annotation();
    if (match(TOKEN_SEMICOLON)) {
        l.read();
        return std::make_unique<FunctionStmt>(name.as_str(), std::move(args),
                                              std::move(ret_type));
    } else {
        auto body = stmt();
        return std::make_unique<FunctionStmt>(name.as_str(), std::move(args),
                                              std::move(ret_type),
                                              std::move(body));
    }
}

std::unique_ptr<Stmt> Parser::struct_stmt() {
    expect(TOKEN_STRUCT);
    std::string name = expect(TOKEN_IDENT).as_str();
    expect(TOKEN_LBRACE);
    std::vector<std::string> names;
    std::vector<std::unique_ptr<LangType>> member_types;

    while (!match(TOKEN_RBRACE)) {
        std::string element_name = expect(TOKEN_IDENT).as_str();
        expect(TOKEN_COLON);
        std::unique_ptr<LangType> type = type_annotation();
        names.push_back(element_name);
        member_types.push_back(std::move(type));

        if (!match(TOKEN_COMMA)) {
            break;
        }
        expect(TOKEN_COMMA);
    }

    expect(TOKEN_RBRACE);

    std::unique_ptr<UserDefinedLangType> struct_type =
        std::make_unique<UserDefinedLangType>(name);
    return std::make_unique<StructStmt>(name, std::move(member_types), names);
}

std::unique_ptr<Stmt> Parser::stmt() {
    std::unique_ptr<Stmt> s;
    if (match(TOKEN_VAR)) {
        s = var_stmt();
        expect(TOKEN_SEMICOLON);
    } else if (match(TOKEN_RETURN)) {
        s = return_stmt();
        expect(TOKEN_SEMICOLON);
    } else if (match(TOKEN_IF)) {
        s = if_stmt();
    } else if (match(TOKEN_LBRACE)) {
        s = compound_stmt();
    } else if (match(TOKEN_FOR)) {
        s = for_stmt();
    } else if (match(TOKEN_WHILE)) {
        s = while_stmt();
    } else {
        s = expr_stmt();
        expect(TOKEN_SEMICOLON);
    }

    return s;
}

std::unique_ptr<Stmt> Parser::toplevel_stmt() {
    if (match(TOKEN_FUN)) {
        return function_stmt();
    } else if (match(TOKEN_STRUCT)) {
        return struct_stmt();
    } else {
        std::stringstream stream;
        stream << "Error: invalid toplevel stmt";
        throw stream.str();
    }
}

std::vector<std::unique_ptr<Stmt>> Parser::toplevel_stmt_list() {
    std::vector<std::unique_ptr<Stmt>> stmts;
    while (!match(TOKEN_EOF)) {
        auto s = toplevel_stmt();
        stmts.push_back(std::move(s));
    }

    return stmts;
}

std::vector<std::pair<std::string, std::unique_ptr<LangType>>>
Parser::arguments() {
    std::vector<std::pair<std::string, std::unique_ptr<LangType>>> args;
    expect(TOKEN_LPAREN);
    while (!match(TOKEN_RPAREN)) {
        Token name = expect(TOKEN_IDENT);
        expect(TOKEN_COLON);
        std::unique_ptr<LangType> type = type_annotation();
        args.emplace_back(name.as_str(), std::move(type));

        if (match(TOKEN_COMMA)) {
            l.read();
        } else if (TOKEN_RPAREN) {
            break;
        } else {
            std::stringstream stream;
            stream << "Error: expected `,` or `)` but got `"
                   << l.curr().as_str() << "`";
            throw stream.str();
        }
    }
    expect(TOKEN_RPAREN);

    return args;
}

class Option {
  private:
    int argc;
    char **argv;

  public:
    std::string input_file;
    std::string output_file;

    Option(int argc, char **argv) : argc(argc), argv(argv) {}
    void parse();
};

void Option::parse() {
    int i = 1;
    while (i < argc) {
        std::string cur_arg(argv[i]);
        if (cur_arg == "-i" || cur_arg == "--input") {
            input_file = std::string(argv[++i]);
        } else if (cur_arg == "-o" || cur_arg == "--output") {
            output_file = std::string(argv[++i]);
        } else {
            std::stringstream stream;
            stream << "Error: unknown option: " << cur_arg;
            throw stream.str();
        }
        i++;
    }
}

class Utility {
  public:
    Utility() {}
    std::string read_all(std::string filename);
};

std::string Utility::read_all(std::string filename) {
    std::ifstream t(filename);
    std::string str((std::istreambuf_iterator<char>(t)),
                    std::istreambuf_iterator<char>());
    return str;
}

int main(int argc, char **argv) {
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    try {
        Option option(argc, argv);
        option.parse();
        Utility util;
        std::string source = util.read_all(option.input_file);
        Lexer l(source);
        Parser p(l);
        Compiler c;
        auto stmts = p.toplevel_stmt_list();
        Scope s;
        TypeEnv type_env;
        for (int i = 0; i < stmts.size(); i++) {
            // std::cout << stmts[i]->to_str() << std::endl;
            stmts[i]->codegen(c, s, type_env);
        }
        c.module->print(errs(), nullptr);
        c.generate_obj_file(option.output_file);
    } catch (std::string msg) {
        std::cout << msg << std::endl;
        std::exit(1);
    } catch (char const *msg) {
        std::cout << msg << std::endl;
        std::exit(1);
    }

    return 0;
}
